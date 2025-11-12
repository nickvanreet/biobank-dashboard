# R/modules/mod_05_mic_qpcr.R
# -----------------------------------------------------------------------------
# Module 05 — MIC qPCR: BioMérieux MIC Analysis with Full QC
# -----------------------------------------------------------------------------
# This module ingests BioMérieux MIC Excel exports, links them to the biobank
# and extraction datasets, and provides a rich QC & interpretation experience.
#
# Key features:
# * Incremental file parsing with caching (data/MIC/)
# * User-adjustable thresholds, late positive windows, ignore cycles and
#   autofluorescence overrides
# * Control alias configuration (Positive/Negative controls)
# * Run metadata dashboard + Levey–Jennings monitoring of positive controls
# * ΔCq metrics and QC scatter plots
# * Flagging pipeline with exports (runs, samples, ΔCq, flags, L-J stats)
# * Global filter integration (province, structure, cohort, …)
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(readxl)
  library(jsonlite)
  library(digest)
  library(plotly)
  library(DT)
  library(glue)
  library(lubridate)
})

# =============================================================================
# SETTINGS & HELPERS
# =============================================================================

mic_default_settings <- function() {
  list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40),
      `18S2` = list(positive = 35, negative = 40),
      RNAseP_DNA = list(positive = 32, negative = 45),
      RNAseP_RNA = list(positive = 30, negative = 45)
    ),
    late_window = c(38, 40),
    ignore_cycles = 0L,
    autofluorescence = NA_real_,
    delta_rp_limit = 8,
    allow_review_controls = FALSE,
    pc_aliases = c("PC", "POS", "POSITIVE CONTROL"),
    nc_aliases = c("NC", "NEG", "NTC", "NEGATIVE CONTROL")
  )
}

parse_aliases <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character())
  x %>% str_split(",") %>% unlist() %>% str_trim() %>% discard(~.x == "") %>% toupper()
}

normalize_id <- function(x) {
  if (is.null(x)) return(NA_character_)
  x %>% as.character() %>% str_trim() %>% toupper()
}

vec_or_na <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) {
    df[[col]]
  } else {
    rep(default, nrow(df))
  }
}

apply_global_filters <- function(df, filters) {
  if (is.null(filters) || !is.list(filters) || !nrow(df)) return(df)
  f <- filters

  if (!is.null(f$date_range) && length(f$date_range) == 2 && "SampleDate" %in% names(df)) {
    df <- df %>% filter(is.na(SampleDate) | (SampleDate >= f$date_range[1] & SampleDate <= f$date_range[2]))
  }

  if (!is.null(f$province) && !identical(f$province, "all") && "Province" %in% names(df)) {
    df <- df %>% filter(is.na(Province) | Province %in% f$province)
  }

  if (!is.null(f$structure) && !identical(f$structure, "all") && "Structure" %in% names(df)) {
    df <- df %>% filter(is.na(Structure) | Structure %in% f$structure)
  }

  if (!is.null(f$cohort) && !identical(f$cohort, "all") && "Cohort" %in% names(df)) {
    df <- df %>% filter(is.na(Cohort) | Cohort %in% f$cohort)
  }

  df
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

calc_target_call <- function(cq, target, thresholds, late_window) {
  thr <- thresholds[[target]]
  if (is.null(thr) || is.na(cq)) return("Undetermined")
  if (!is.na(thr$positive) && cq <= thr$positive) return("Positive")
  if (!is.na(thr$negative) && cq > thr$negative) return("Negative")
  if (!is.null(late_window) && length(late_window) == 2) {
    if (!is.na(late_window[1]) && !is.na(late_window[2]) && cq > late_window[1] && cq <= late_window[2]) {
      return("LatePositive")
    }
  }
  "Negative"
}

calc_delta <- function(dna, rna) {
  if (is.na(dna) || is.na(rna)) return(NA_real_)
  rna - dna
}

scan_mic_files <- function(path) {
  if (!dir.exists(path)) {
    return(tibble(file_path = character(), file_name = character(),
                  size = numeric(), mtime = as.POSIXct(character()), hash = character()))
  }
  files <- list.files(path, pattern = "\\.(xls|xlsx)$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) {
    return(tibble(file_path = character(), file_name = character(),
                  size = numeric(), mtime = as.POSIXct(character()), hash = character()))
  }
  tibble(
    file_path = files,
    file_name = basename(files),
    size = purrr::map_dbl(files, ~ file.info(.x)$size),
    mtime = purrr::map_dbl(files, ~ as.numeric(file.info(.x)$mtime)) %>% as.POSIXct(origin = "1970-01-01", tz = Sys.timezone()),
    hash = purrr::map_chr(files, ~ digest(paste(.x, file.info(.x)$size, file.info(.x)$mtime), algo = "md5"))
  )
}

parse_run_datetime <- function(file_name) {
  # Attempt to parse formats like "2024-04-02 11.22.00 Run.xlsx" or variants
  base <- tools::file_path_sans_ext(file_name)
  parsed <- suppressWarnings(lubridate::parse_date_time(base,
                                                        orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM"),
                                                        truncated = 2))
  if (is.na(parsed)) {
    return(as.POSIXct(NA))
  }
  parsed
}

list_to_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

sample_control_type <- function(name, reported_type, pc_aliases, nc_aliases) {
  nm <- normalize_id(name)
  if (nm %in% pc_aliases) return("PC")
  if (nm %in% nc_aliases) return("NC")
  rpt <- normalize_id(reported_type)
  if (str_detect(rpt, "POS")) return("PC")
  if (str_detect(rpt, "NEG") || str_detect(rpt, "NTC")) return("NC")
  "Sample"
}

summarise_targets <- function(replicates_long, thresholds, late_window) {
  if (!nrow(replicates_long)) return(tibble())
  replicates_long %>%
    group_by(RunID, SampleID, SampleName, ControlType, Target) %>%
    summarise(
      n = sum(!is.na(Cq)),
      Cq_median = safe_median(Cq),
      Cq_mean = ifelse(all(is.na(Cq)), NA_real_, mean(Cq, na.rm = TRUE)),
      Calls = list(unique(AmpCall[!is.na(AmpCall)])),
      .groups = "drop"
    ) %>%
    mutate(TargetCall = map2_chr(Target, Cq_median,
                                 ~ calc_target_call(.y, .x, thresholds, late_window)))
}

ensure_columns <- function(df, cols, default = NA) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- default
  }
  df
}

finalise_samples <- function(target_summary, settings, delta_limit) {
  if (!nrow(target_summary)) return(tibble())
  # pivot to wide
  wide <- target_summary %>%
    select(RunID, SampleID, SampleName, ControlType, Target, Cq_median, TargetCall) %>%
    pivot_wider(names_from = Target,
                values_from = c(Cq_median, TargetCall),
                names_sep = "_")

  thresholds <- settings$thresholds

  wide <- ensure_columns(wide, paste0("Cq_median_", names(thresholds)), NA_real_)
  wide <- ensure_columns(wide, paste0("TargetCall_", names(thresholds)), NA_character_)

  wide %>%
    rowwise() %>%
    mutate(
      TargetsPresent = {
        med_cols <- paste0("Cq_median_", names(thresholds))
        vals <- c_across(all_of(med_cols))
        present <- med_cols[!is.na(vals)] %>% str_remove("^Cq_median_")
        paste(present, collapse = "; ")
      },
      Call_177T = TargetCall_177T,
      Call_18S2 = TargetCall_18S2,
      Call_RNAseP_DNA = TargetCall_RNAseP_DNA,
      Call_RNAseP_RNA = TargetCall_RNAseP_RNA,
      Delta_18S2_177T = calc_delta(Cq_median_177T, Cq_median_18S2),
      Delta_RP = calc_delta(Cq_median_RNAseP_DNA, Cq_median_RNAseP_RNA),
      Flag_SampleDecay = !is.na(Delta_RP) & Delta_RP > delta_limit,
      PositiveTryp = (Call_177T == "Positive" | Call_18S2 == "Positive"),
      LateTryp = !PositiveTryp & ((Call_177T == "LatePositive") | (Call_18S2 == "LatePositive")),
      HostOK = (Call_RNAseP_DNA %in% c("Positive", "LatePositive")),
      HostRNAOK = (Call_RNAseP_RNA %in% c("Positive", "LatePositive")),
      FinalCall = case_when(
        ControlType %in% c("PC", "NC") ~ "Control",
        PositiveTryp ~ "Positive",
        LateTryp ~ "LatePositive",
        !HostOK ~ "Invalid_NoDNA",
        TRUE ~ "Negative"
      ),
      FlagsList = {
        flags <- character()
        if (Flag_SampleDecay) flags <- c(flags, "Flag_SampleDecay")
        if (!HostRNAOK && HostOK && ControlType == "Sample") flags <- c(flags, "Flag_RNA_missing")
        if (is.na(Delta_18S2_177T) && PositiveTryp) flags <- c(flags, "Flag_IncompleteTargets")
        if (length(flags)) paste(flags, collapse = ";") else NA_character_
      },
      Flags = FlagsList,
      AnyFlag = !is.na(Flags)
    ) %>%
    ungroup() %>%
    select(-FlagsList)
}

control_qc_status <- function(samples_df, thresholds) {
  if (!nrow(samples_df)) return(tibble())
  samples_df %>%
    filter(ControlType %in% c("PC", "NC")) %>%
    mutate(
      ControlPass = case_when(
        ControlType == "PC" ~ (
          (is.na(Cq_median_177T) || Cq_median_177T <= thresholds$`177T`$positive) &
            (is.na(Cq_median_18S2) || Cq_median_18S2 <= thresholds$`18S2`$positive)
        ),
        ControlType == "NC" ~ (
          (is.na(Cq_median_177T) || Cq_median_177T > thresholds$`177T`$negative) &
            (is.na(Cq_median_18S2) || Cq_median_18S2 > thresholds$`18S2`$negative)
        ),
        TRUE ~ TRUE
      ),
      ControlFlag = if_else(ControlPass, NA_character_,
                            if_else(ControlType == "PC", "FailedPositiveControl", "FailedNegativeControl"))
    )
}

runs_from_samples <- function(samples_df, file_info, settings) {
  if (!nrow(samples_df)) return(tibble())
  control_status <- control_qc_status(samples_df, settings$thresholds)
  samples_df %>%
    distinct(RunID) %>%
    left_join(file_info %>% select(RunID, FilePath, FileName, RunDateTime, FileMTime, WellCount), by = "RunID") %>%
    left_join(
      samples_df %>% group_by(RunID) %>% summarise(
        TotalSamples = sum(ControlType == "Sample"),
        TotalControls = sum(ControlType %in% c("PC", "NC")),
        Positives = sum(FinalCall == "Positive" & ControlType == "Sample", na.rm = TRUE),
        Negatives = sum(FinalCall == "Negative" & ControlType == "Sample", na.rm = TRUE),
        LatePositives = sum(FinalCall == "LatePositive" & ControlType == "Sample", na.rm = TRUE),
        Flagged = sum(AnyFlag & ControlType == "Sample", na.rm = TRUE),
        .groups = "drop"
      ), by = "RunID"
    ) %>%
    left_join(
      control_status %>%
        group_by(RunID) %>%
        summarise(
          ControlsPassing = all(ControlPass),
          FailedControls = paste(na.omit(ControlFlag), collapse = ";"),
          .groups = "drop"
        ),
      by = "RunID"
    ) %>%
    mutate(
      RunValid = if_else(is.na(ControlsPassing), TRUE, ControlsPassing),
      ThresholdsJSON = list_to_json(settings$thresholds),
      IgnoreCycles = settings$ignore_cycles,
      AutoFluorescence = settings$autofluorescence
    )
}

link_biobank <- function(samples_df, biobank_df) {
  if (!nrow(samples_df) || is.null(biobank_df) || !nrow(biobank_df)) {
    return(samples_df %>% mutate(BiobankMatched = FALSE, BiobankNumero = NA_character_,
                                 Province = NA_character_, Structure = NA_character_, Cohort = NA_character_,
                                 SampleDate = as.Date(NA)))
  }
  bb <- biobank_df
  sample_vec <- normalize_id(coalesce(vec_or_na(bb, "Sample"), vec_or_na(bb, "numero"), vec_or_na(bb, "SampleID")))
  province_vec <- coalesce(vec_or_na(bb, "Province"), vec_or_na(bb, "province"))
  structure_vec <- coalesce(vec_or_na(bb, "structure_sanitaire"), vec_or_na(bb, "Structure"), vec_or_na(bb, "centre_de_sante"))
  cohort_vec <- coalesce(vec_or_na(bb, "Cohort"), vec_or_na(bb, "etude"))
  date_vec <- coalesce(vec_or_na(bb, "date_de_prelevement"), vec_or_na(bb, "DatePrelevement"), vec_or_na(bb, "sample_date"))
  numero_vec <- vec_or_na(bb, "numero")
  barcode_vec <- coalesce(vec_or_na(bb, "code_barres_kps"), vec_or_na(bb, "barcode"))

  bb_clean <- tibble(
    Sample_norm = sample_vec,
    Province = province_vec,
    Structure = structure_vec,
    Cohort = cohort_vec,
    SampleDate = suppressWarnings(as.Date(date_vec)),
    numero = numero_vec,
    code_barres_kps = barcode_vec
  ) %>%
    mutate(Sample_norm = normalize_id(Sample_norm)) %>%
    distinct(Sample_norm, .keep_all = TRUE)

  samples_df %>%
    mutate(SampleID = normalize_id(SampleID)) %>%
    left_join(bb_clean, by = c("SampleID" = "Sample_norm")) %>%
    mutate(BiobankMatched = !is.na(numero),
           BiobankNumero = numero,
           BiobankBarcode = code_barres_kps)
}

link_extractions <- function(samples_df, extractions_df) {
  if (!nrow(samples_df) || is.null(extractions_df) || !nrow(extractions_df)) {
    return(samples_df %>% mutate(ExtractionMatched = FALSE))
  }
  ex <- extractions_df
  sample_vec <- normalize_id(coalesce(vec_or_na(ex, "SampleID"), vec_or_na(ex, "Sample"), vec_or_na(ex, "numero")))
  barcode_vec <- normalize_id(coalesce(vec_or_na(ex, "code_barres_kps"), vec_or_na(ex, "barcode")))

  ex_clean <- tibble(
    SampleID = sample_vec,
    SampleBarcode = barcode_vec
  ) %>% distinct()

  samples_df %>%
    mutate(SampleID_norm = normalize_id(SampleID),
           BiobankBarcode_norm = normalize_id(BiobankBarcode)) %>%
    mutate(ExtractionMatched = SampleID_norm %in% ex_clean$SampleID |
             (!is.na(BiobankBarcode_norm) & BiobankBarcode_norm %in% ex_clean$SampleBarcode)) %>%
    select(-SampleID_norm, -BiobankBarcode_norm)
}

compute_lj <- function(replicates_long, target, control_type = "PC") {
  df <- replicates_long %>%
    filter(ControlType == control_type, Target == target, !is.na(Cq))
  if (!nrow(df)) {
    return(list(data = tibble(), summary = tibble(Target = target, Mean = NA_real_, SD = NA_real_, N = 0)))
  }
  stats <- df %>% group_by(RunID) %>% summarise(Cq_mean = mean(Cq, na.rm = TRUE), .groups = "drop")
  mu <- mean(df$Cq, na.rm = TRUE)
  sdv <- sd(df$Cq, na.rm = TRUE)
  plot_data <- stats %>% mutate(
    Mean = mu,
    SD = sdv,
    plus1 = mu + sdv,
    minus1 = mu - sdv,
    plus2 = mu + 2 * sdv,
    minus2 = mu - 2 * sdv,
    plus3 = mu + 3 * sdv,
    minus3 = mu - 3 * sdv
  )
  list(
    data = plot_data,
    summary = tibble(Target = target, Mean = mu, SD = sdv, N = nrow(df))
  )
}

prepare_exports <- function(samples_df, runs_df, replicates_long, lj_stats) {
  list(
    runs = runs_df,
    samples = samples_df,
    deltas = samples_df %>% select(RunID, SampleID, SampleName, Delta_18S2_177T, Delta_RP, Flag_SampleDecay),
    flags = samples_df %>% filter(ControlType == "Sample", AnyFlag | FinalCall == "Invalid_NoDNA") %>%
      select(RunID, SampleID, SampleName, FinalCall, Flags, Delta_RP),
    lj = map_dfr(lj_stats, "summary"),
    replicates = replicates_long
  )
}

# =============================================================================
# FILE PARSING
# =============================================================================

parse_mic_file <- function(file_row, settings) {
  res <- tryCatch({
    analyze_qpcr(
      micrun_file = file_row$file_path,
      rnasep_rna_cutoff = settings$thresholds$RNAseP_RNA$positive,
      cutoffs = settings$thresholds,
      verbose = FALSE
    )
  }, error = function(e) {
    warning(glue("Failed to parse {file_row$file_name}: {e$message}"))
    return(NULL)
  })
  if (is.null(res)) return(NULL)

  run_id <- tools::file_path_sans_ext(file_row$file_name)
  run_dt <- parse_run_datetime(file_row$file_name)
  replicates_wide <- res$replicate_data %>% mutate(RunID = run_id)

  replicate_long <- replicates_wide %>%
    mutate(SampleName = Name,
           SampleID = normalize_id(Name)) %>%
    select(RunID, SampleID, SampleName, Replicate, Type, is_control,
           starts_with("marker_"), starts_with("Cq_"), RNA_Preservation_Delta) %>%
    pivot_longer(cols = starts_with("Cq_"), names_to = "Target", values_to = "Cq") %>%
    mutate(Target = str_remove(Target, "^Cq_")) %>%
    left_join(
      replicates_wide %>% select(Name, Replicate, starts_with("marker_")) %>%
        pivot_longer(cols = starts_with("marker_"), names_to = "TargetCall", values_to = "MarkerCall") %>%
        mutate(TargetCall = str_remove(TargetCall, "^marker_")),
      by = c("SampleName" = "Name", "Replicate", "Target" = "TargetCall")
    ) %>%
    mutate(
      ControlType = sample_control_type(SampleName, Type, settings$pc_aliases, settings$nc_aliases),
      AmpCall = MarkerCall,
      AmpStatus = MarkerCall,
      CurveOK = NA,
      ResultRaw = MarkerCall
    ) %>%
    select(RunID, SampleID, SampleName, Replicate, ControlType, Target, Cq, AmpStatus, CurveOK, ResultRaw)

  target_summary <- summarise_targets(replicate_long, settings$thresholds, settings$late_window)
  samples_df <- finalise_samples(target_summary, settings, settings$delta_rp_limit) %>%
    mutate(RunID = run_id)

  file_info <- tibble(
    RunID = run_id,
    FilePath = file_row$file_path,
    FileName = file_row$file_name,
    FileMTime = file_row$mtime,
    RunDateTime = run_dt,
    WellCount = nrow(res$cq_data)
  )

  list(
    run = file_info,
    replicates = replicate_long,
    samples = samples_df
  )
}

parse_mic_directory <- function(path, settings, cache_state) {
  files <- scan_mic_files(path)
  if (!nrow(files)) {
    return(list(
      runs = tibble(),
      replicates = tibble(),
      samples = tibble(),
      cache = list(),
      files = files
    ))
  }

  cache <- cache_state
  all_runs <- list()
  all_samples <- list()
  all_replicates <- list()
  new_cache <- list()

  for (i in seq_len(nrow(files))) {
    row <- files[i, ]
    if (!is.null(cache[[row$hash]])) {
      parsed <- cache[[row$hash]]
    } else {
      parsed <- parse_mic_file(row, settings)
    }
    if (is.null(parsed)) next
    new_cache[[row$hash]] <- parsed
    all_runs[[length(all_runs) + 1]] <- parsed$run
    all_samples[[length(all_samples) + 1]] <- parsed$samples
    all_replicates[[length(all_replicates) + 1]] <- parsed$replicates
  }

  list(
    runs = bind_rows(all_runs),
    replicates = bind_rows(all_replicates),
    samples = bind_rows(all_samples),
    cache = new_cache,
    files = files
  )
}

# =============================================================================
# UI
# =============================================================================

mod_mic_qpcr_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "MIC qPCR",
    icon = icon("dna"),
    fill = TRUE,
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            div(class = "h4 mb-0", icon("dna"), " MIC qPCR"),
            div(
              actionButton(ns("refresh"), "Refresh", icon = icon("sync"), class = "btn-primary me-2"),
              actionButton(ns("force_export"), "Export QC", icon = icon("file-excel"), class = "btn-success")
            )
        )
      ),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4), gap = "16px",
          textInput(ns("mic_dir"), "MIC directory", value = "data/MIC", placeholder = "data/MIC"),
          numericInput(ns("ignore_cycles"), "Ignore cycles", value = mic_default_settings()$ignore_cycles, min = 0, max = 20, step = 1),
          numericInput(ns("autofluorescence"), "Autofluorescence (override)", value = mic_default_settings()$autofluorescence, min = 0, step = 0.1)
        ),
        layout_columns(
          col_widths = c(3,3,3,3), gap = "12px",
          numericInput(ns("th_177t_pos"), "177T positive ≤", value = mic_default_settings()$thresholds$`177T`$positive, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_177t_neg"), "177T negative >", value = mic_default_settings()$thresholds$`177T`$negative, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_18s2_pos"), "18S2 positive ≤", value = mic_default_settings()$thresholds$`18S2`$positive, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_18s2_neg"), "18S2 negative >", value = mic_default_settings()$thresholds$`18S2`$negative, min = 0, max = 50, step = 0.1)
        ),
        layout_columns(
          col_widths = c(3,3,3,3), gap = "12px",
          numericInput(ns("th_rnp_dna_pos"), "RNAseP DNA positive ≤", value = mic_default_settings()$thresholds$RNAseP_DNA$positive, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_rnp_dna_neg"), "RNAseP DNA negative >", value = mic_default_settings()$thresholds$RNAseP_DNA$negative, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_rnp_rna_pos"), "RNAseP RNA positive ≤", value = mic_default_settings()$thresholds$RNAseP_RNA$positive, min = 0, max = 50, step = 0.1),
          numericInput(ns("th_rnp_rna_neg"), "RNAseP RNA negative >", value = mic_default_settings()$thresholds$RNAseP_RNA$negative, min = 0, max = 50, step = 0.1)
        ),
        layout_columns(
          col_widths = c(3,3,3,3), gap = "12px",
          numericInput(ns("late_min"), "Late positive min", value = mic_default_settings()$late_window[1], min = 0, max = 50, step = 0.1),
          numericInput(ns("late_max"), "Late positive max", value = mic_default_settings()$late_window[2], min = 0, max = 50, step = 0.1),
          numericInput(ns("delta_rp"), "ΔCq RNA limit", value = mic_default_settings()$delta_rp_limit, min = 0, max = 20, step = 0.5),
          checkboxInput(ns("allow_review"), "Allow review despite control failures", value = mic_default_settings()$allow_review_controls)
        ),
        layout_columns(
          col_widths = c(6,6), gap = "16px",
          textInput(ns("pc_aliases"), "Positive control aliases", value = paste(mic_default_settings()$pc_aliases, collapse = ", ")),
          textInput(ns("nc_aliases"), "Negative control aliases", value = paste(mic_default_settings()$nc_aliases, collapse = ", "))
        )
      )
    ),
    layout_column_wrap(
      width = 1/6, gap = "12px", heights_equal = "row",
      value_box(title = "Runs", value = textOutput(ns("kpi_runs")), showcase = icon("folder-open")),
      value_box(title = "Samples", value = textOutput(ns("kpi_samples")), showcase = icon("vial")),
      value_box(title = "Biobank linked", value = textOutput(ns("kpi_biobank")), showcase = icon("link")),
      value_box(title = "Extractions linked", value = textOutput(ns("kpi_extractions")), showcase = icon("flask")),
      value_box(title = "Positives", value = textOutput(ns("kpi_positive")), showcase = icon("plus-circle")),
      value_box(title = "Flagged", value = textOutput(ns("kpi_flagged")), showcase = icon("flag"))
    ),
    navset_card_tab(
      nav_panel("Runs", icon = icon("clipboard-list"), DTOutput(ns("tbl_runs"))),
      nav_panel("Samples", icon = icon("vials"),
                DTOutput(ns("tbl_samples"))),
      nav_panel("Controls & L-J", icon = icon("flask"),
                layout_columns(
                  col_widths = c(12), gap = "16px",
                  card(card_header("Control status"), card_body(DTOutput(ns("tbl_controls")))),
                  card(card_header("177T Positive Control"), card_body_fill(plotlyOutput(ns("lj_177t"), height = "350px"))),
                  card(card_header("18S2 Positive Control"), card_body_fill(plotlyOutput(ns("lj_18s2"), height = "350px"))),
                  card(card_header("RNAseP DNA Positive Control"), card_body_fill(plotlyOutput(ns("lj_rnp_dna"), height = "350px"))),
                  card(card_header("RNAseP RNA Positive Control"), card_body_fill(plotlyOutput(ns("lj_rnp_rna"), height = "350px")))
                )),
      nav_panel("QC scatter", icon = icon("chart-scatter"),
                layout_columns(
                  col_widths = c(6,6), gap = "16px",
                  card(card_header("18S2 vs 177T"), card_body_fill(plotlyOutput(ns("scatter_tna"), height = "400px"))),
                  card(card_header("RNAseP RNA vs DNA"), card_body_fill(plotlyOutput(ns("scatter_rnp"), height = "400px")))
                )),
      nav_panel("Flags", icon = icon("flag"), DTOutput(ns("tbl_flags"))),
      nav_panel("Exports", icon = icon("download"),
                layout_columns(col_widths = c(4,4,4), gap = "16px",
                               card(card_header("Download"), card_body(
                                 downloadButton(ns("download_runs"), "Run metadata", class = "btn-primary w-100 mb-2"),
                                 downloadButton(ns("download_samples"), "Sample calls", class = "btn-secondary w-100 mb-2"),
                                 downloadButton(ns("download_delta"), "ΔCq summary", class = "btn-secondary w-100 mb-2"),
                                 downloadButton(ns("download_flags"), "Flagged list", class = "btn-warning w-100 mb-2"),
                                 downloadButton(ns("download_lj"), "L-J statistics", class = "btn-info w-100")
                               )))
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

mod_mic_qpcr_server <- function(id, biobank_df, extractions_df, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    settings <- reactive({
      list(
        thresholds = list(
          `177T` = list(positive = input$th_177t_pos, negative = input$th_177t_neg),
          `18S2` = list(positive = input$th_18s2_pos, negative = input$th_18s2_neg),
          RNAseP_DNA = list(positive = input$th_rnp_dna_pos, negative = input$th_rnp_dna_neg),
          RNAseP_RNA = list(positive = input$th_rnp_rna_pos, negative = input$th_rnp_rna_neg)
        ),
        late_window = c(input$late_min, input$late_max),
        ignore_cycles = input$ignore_cycles,
        autofluorescence = input$autofluorescence,
        delta_rp_limit = input$delta_rp,
        allow_review_controls = isTRUE(input$allow_review),
        pc_aliases = parse_aliases(input$pc_aliases),
        nc_aliases = parse_aliases(input$nc_aliases)
      )
    })

    cache_state <- reactiveVal(list())

    raw_data <- reactive({
      req(input$mic_dir)
      withProgress(message = "Reading MIC exports", value = 0.2, {
        parsed <- parse_mic_directory(input$mic_dir, settings(), isolate(cache_state()))
        cache_state(parsed$cache)
        parsed
      })
    })

    observeEvent(input$refresh, {
      cache_state(list())
      raw_data()
    })

    processed_data <- reactive({
      rd <- raw_data()
      if (!nrow(rd$samples)) {
        return(list(
          runs = tibble(),
          samples = tibble(),
          replicates = tibble(),
          lj = list(),
          exports = list()
        ))
      }
      runs_df <- runs_from_samples(rd$samples, rd$runs, settings())
      samples_df <- rd$samples %>%
        link_biobank(if (is.null(biobank_df)) NULL else biobank_df()) %>%
        link_extractions(if (is.null(extractions_df)) NULL else extractions_df())

      ctrl_details <- control_qc_status(samples_df, settings()$thresholds) %>%
        select(RunID, SampleID, ControlFlag)
      samples_df <- samples_df %>%
        left_join(ctrl_details, by = c("RunID", "SampleID")) %>%
        mutate(
          Flags = ifelse(!is.na(ControlFlag),
                         ifelse(is.na(Flags), ControlFlag, str_trim(paste(Flags, ControlFlag, sep = ";"))), Flags),
          AnyFlag = (ifelse(is.na(AnyFlag), FALSE, AnyFlag) | !is.na(ControlFlag))
        )

      if (!settings()$allow_review_controls) {
        invalid_runs <- runs_df %>% filter(!RunValid) %>% pull(RunID)
        samples_df <- samples_df %>%
          mutate(
            FinalCallOriginal = FinalCall,
            Flags = ifelse(RunID %in% invalid_runs,
                           str_trim(paste(Flags, "RunInvalid", sep = ";")), Flags),
            FinalCall = ifelse(RunID %in% invalid_runs, "RunInvalid", FinalCall),
            AnyFlag = ifelse(RunID %in% invalid_runs, TRUE, AnyFlag)
          )
      } else {
        invalid_runs <- runs_df %>% filter(!RunValid) %>% pull(RunID)
        samples_df <- samples_df %>%
          mutate(
            Flags = ifelse(RunID %in% invalid_runs,
                           str_trim(paste(Flags, "RunInvalid", sep = ";")), Flags),
            AnyFlag = ifelse(RunID %in% invalid_runs, TRUE, AnyFlag)
          )
      }

      replicates_long <- rd$replicates
      lj_list <- list(
        `177T` = compute_lj(replicates_long, "177T"),
        `18S2` = compute_lj(replicates_long, "18S2"),
        RNAseP_DNA = compute_lj(replicates_long, "RNAseP_DNA"),
        RNAseP_RNA = compute_lj(replicates_long, "RNAseP_RNA")
      )
      exports <- prepare_exports(samples_df, runs_df, replicates_long, lj_list)
      list(
        runs = runs_df,
        samples = samples_df,
        replicates = replicates_long,
        lj = lj_list,
        exports = exports
      )
    })

    filtered_samples <- reactive({
      pd <- processed_data()
      df <- pd$samples
      apply_global_filters(df, if (is.null(filters)) NULL else filters())
    })

    # KPIs -----------------------------------------------------------------
    output$kpi_runs <- renderText({
      rd <- processed_data()$runs
      sprintf("%s runs", scales::comma(nrow(rd)))
    })
    output$kpi_samples <- renderText({
      df <- filtered_samples()
      sprintf("%s", scales::comma(sum(df$ControlType == "Sample")))
    })
    output$kpi_biobank <- renderText({
      df <- filtered_samples()
      if (!nrow(df)) return("0")
      n <- sum(df$BiobankMatched & df$ControlType == "Sample", na.rm = TRUE)
      sprintf("%s (%.0f%%)", scales::comma(n), 100 * n / max(1, sum(df$ControlType == "Sample")))
    })
    output$kpi_extractions <- renderText({
      df <- filtered_samples()
      if (!nrow(df)) return("0")
      n <- sum(df$ExtractionMatched & df$ControlType == "Sample", na.rm = TRUE)
      sprintf("%s (%.0f%%)", scales::comma(n), 100 * n / max(1, sum(df$ControlType == "Sample")))
    })
    output$kpi_positive <- renderText({
      df <- filtered_samples()
      sum(df$FinalCall == "Positive" & df$ControlType == "Sample", na.rm = TRUE) %>% scales::comma()
    })
    output$kpi_flagged <- renderText({
      df <- filtered_samples()
      sum(df$AnyFlag & df$ControlType == "Sample", na.rm = TRUE) %>% scales::comma()
    })

    # Tables ---------------------------------------------------------------
    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs
      if (!nrow(runs)) {
        return(datatable(tibble(Message = "No MIC runs detected"), options = list(dom = 't'), rownames = FALSE))
      }
      datatable(
        runs %>% mutate(RunDateTime = as.character(RunDateTime), FileMTime = as.character(FileMTime)),
        rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv')),
        class = "compact stripe"
      )
    })

    output$tbl_samples <- renderDT({
      df <- filtered_samples() %>% filter(ControlType == "Sample")
      if (!nrow(df)) {
        return(datatable(tibble(Message = "No samples"), options = list(dom = 't'), rownames = FALSE))
      }
      datatable(
        df %>%
          mutate(across(starts_with("Cq_median"), ~ round(.x, 2)),
                 Delta_18S2_177T = round(Delta_18S2_177T, 2),
                 Delta_RP = round(Delta_RP, 2)),
        selection = "single",
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv')),
        class = "compact stripe"
      )
    })

    observeEvent(input$tbl_samples_rows_selected, {
      sel <- input$tbl_samples_rows_selected
      if (length(sel) != 1) return()
      df <- filtered_samples() %>% filter(ControlType == "Sample")
      row <- df[sel, ]
      replicates <- processed_data()$replicates %>% filter(SampleID == row$SampleID, RunID == row$RunID)
      output$sample_detail <- renderTable({
        replicates %>% select(Replicate, Target, Cq, AmpStatus) %>% arrange(Replicate, Target)
      }, striped = TRUE, bordered = TRUE)
      showModal(modalDialog(
        title = glue("Sample detail — {row$SampleName}"),
        tagList(
          tags$strong("Run:"), row$RunID, tags$br(),
          tags$strong("Final call:"), row$FinalCall, tags$br(),
          tags$strong("Flags:"), ifelse(is.na(row$Flags), "None", row$Flags), tags$br(),
          tableOutput(ns("sample_detail"))
        ),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    })

    output$tbl_controls <- renderDT({
      controls <- processed_data()$runs %>%
        select(RunID, ControlsPassing, FailedControls)
      if (!nrow(controls)) {
        return(datatable(tibble(Message = "No controls"), options = list(dom = 't'), rownames = FALSE))
      }
      datatable(controls, rownames = FALSE, options = list(dom = 't', paging = FALSE))
    })

    output$tbl_flags <- renderDT({
      df <- filtered_samples() %>% filter(ControlType == "Sample", AnyFlag | FinalCall %in% c("Invalid_NoDNA", "RunInvalid"))
      if (!nrow(df)) {
        return(datatable(tibble(Message = "No flagged samples"), options = list(dom = 't'), rownames = FALSE))
      }
      datatable(df %>% select(RunID, SampleName, FinalCall, Flags, Delta_RP, BiobankMatched, ExtractionMatched),
                rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip', buttons = c('copy', 'csv')))
    })

    # Plots ----------------------------------------------------------------
    render_lj_plot <- function(target_name, output_id) {
      output[[output_id]] <- renderPlotly({
        pd <- processed_data()
        lj <- pd$lj[[target_name]]
        if (is.null(lj) || !nrow(lj$data)) {
          return(plotly_empty(type = "scatter", mode = "lines") %>% layout(title = "No data"))
        }
        plot_ly(lj$data, x = ~RunID, y = ~Cq_mean, type = 'scatter', mode = 'markers+lines', name = 'Run mean') %>%
          add_lines(y = ~Mean, name = 'Mean', line = list(color = '#2c3e50')) %>%
          add_lines(y = ~plus1, name = '+1SD', line = list(color = '#3498db', dash = 'dot')) %>%
          add_lines(y = ~minus1, name = '-1SD', line = list(color = '#3498db', dash = 'dot')) %>%
          add_lines(y = ~plus2, name = '+2SD', line = list(color = '#e67e22', dash = 'dash')) %>%
          add_lines(y = ~minus2, name = '-2SD', line = list(color = '#e67e22', dash = 'dash')) %>%
          add_lines(y = ~plus3, name = '+3SD', line = list(color = '#e74c3c', dash = 'dashdot')) %>%
          add_lines(y = ~minus3, name = '-3SD', line = list(color = '#e74c3c', dash = 'dashdot')) %>%
          layout(title = glue("Levey–Jennings: {target_name}"), xaxis = list(title = "Run"), yaxis = list(title = "Cq"), legend = list(orientation = 'h'))
      })
    }

    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")

    output$scatter_tna <- renderPlotly({
      df <- filtered_samples() %>% filter(ControlType == "Sample", !is.na(Cq_median_177T), !is.na(Cq_median_18S2))
      if (!nrow(df)) return(plotly_empty(type = "scatter") %>% layout(title = "No data"))
      plot_ly(df, x = ~Cq_median_177T, y = ~Cq_median_18S2, color = ~FinalCall, type = 'scatter', mode = 'markers', text = ~SampleName) %>%
        layout(xaxis = list(title = "177T Cq"), yaxis = list(title = "18S2 Cq"), title = "18S2 vs 177T")
    })

    output$scatter_rnp <- renderPlotly({
      df <- filtered_samples() %>% filter(ControlType == "Sample", !is.na(Cq_median_RNAseP_DNA), !is.na(Cq_median_RNAseP_RNA))
      if (!nrow(df)) return(plotly_empty(type = "scatter") %>% layout(title = "No data"))
      plot_ly(df, x = ~Cq_median_RNAseP_DNA, y = ~Cq_median_RNAseP_RNA, color = ~Flag_SampleDecay, type = 'scatter', mode = 'markers', text = ~SampleName) %>%
        layout(xaxis = list(title = "RNAseP DNA Cq"), yaxis = list(title = "RNAseP RNA Cq"), title = "RNAseP RNA vs DNA")
    })

    # Downloads ------------------------------------------------------------
    output$download_runs <- downloadHandler(
      filename = function() sprintf("mic_runs_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        write_csv(processed_data()$runs, file)
      }
    )
    output$download_samples <- downloadHandler(
      filename = function() sprintf("mic_samples_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        write_csv(filtered_samples(), file)
      }
    )
    output$download_delta <- downloadHandler(
      filename = function() sprintf("mic_delta_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        write_csv(processed_data()$exports$deltas, file)
      }
    )
    output$download_flags <- downloadHandler(
      filename = function() sprintf("mic_flags_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        write_csv(processed_data()$exports$flags, file)
      }
    )
    output$download_lj <- downloadHandler(
      filename = function() sprintf("mic_lj_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        write_csv(processed_data()$exports$lj, file)
      }
    )

    observeEvent(input$force_export, {
      pd <- processed_data()
      if (!nrow(pd$runs)) {
        showNotification("No data to export", type = "warning")
        return()
      }
      dir.create("outputs", showWarnings = FALSE)
      path <- file.path("outputs", sprintf("MIC_QC_%s.csv", format(Sys.time(), "%Y%m%d%H%M")))
      write_csv(pd$exports$samples, path)
      showNotification(glue("QC export saved to {path}"), type = "message", duration = 4)
    })
  })
}

# =============================================================================
# HOW TO MOUNT THE MODULE
# =============================================================================
# In the main UI:
#   nav_panel("MIC qPCR", mod_mic_qpcr_ui("mic"))
# In the server:
#   mod_mic_qpcr_server("mic", biobank_df = biobank_data, extractions_df = extraction_data, filters = filters)
