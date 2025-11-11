# ==============================================================================
# qPCR Analysis for BioMérieux MIC (ROBUST VERSION)
# ------------------------------------------------------------------------------
# Provides the analyze_qpcr() pipeline and helper utilities used by Module 05.
# This script is sourced automatically from global.R when the application starts
# (all files under R/core/ are loaded before modules), so the functions are
# available for .mic_read_dir() and downstream MIC features.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
})

# ==============================================================================
# CONFIGURATION
# ==============================================================================

DEFAULT_CUTOFFS <- list(
  "177T"       = list(positive = 35, negative = 40),  # DNA target
  "18S2"       = list(positive = 35, negative = 40),  # RNA target
  "RNAseP_DNA" = list(positive = 32, negative = 999),
  "RNAseP_RNA" = list(positive = 30, negative = 999)
)

# thresholds for RNA preservation ΔCq
PRESERVATION_DELTA_GOOD <- 5   # ≤5 => Good
PRESERVATION_DELTA_WARN <- 8   # >5 & ≤8 => Moderate ; >8 => Poor

# ==============================================================================
# HELPERS (header detection + column finders)
# ==============================================================================

find_column <- function(df, patterns) {
  for (pattern in patterns) {
    m <- grep(pattern, names(df), ignore.case = TRUE, value = TRUE)
    if (length(m) > 0) return(m[1])
  }
  NULL
}

guess_header_row <- function(mat) {
  # row contains (Well|Name|Sample|ID) AND (Cq|Ct|Cp)
  candidates <- which(apply(mat, 1, function(r) {
    any(grepl("\\b(Well|Name|Sample|ID|Identifier|Position)\\b", r, ignore.case = TRUE)) &&
      any(grepl("\\b(Cq|Ct|Cp|Quant.*Cq|Cross.*Point)\\b", r, ignore.case = TRUE))
  }))
  if (length(candidates)) candidates[1] else NA_integer_
}

read_any_result_table <- function(path, sheet) {
  raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
  if (nrow(raw) == 0) return(tibble::tibble())

  mat <- apply(raw, 2, as.character)
  hdr <- guess_header_row(mat)
  if (is.na(hdr)) return(tibble::tibble())  # no recognizable table

  headers <- as.character(unlist(raw[hdr, ]))
  headers <- make.names(trimws(headers), unique = TRUE)
  dat <- raw[(hdr + 1):nrow(raw), , drop = FALSE]
  names(dat) <- headers

  dat <- tibble::as_tibble(dat)
  dat <- dat[, colSums(!is.na(dat)) > 0, drop = FALSE]
  if (nrow(dat) == 0) return(tibble::tibble())

  dat
}

pick_cq_col <- function(df) {
  find_column(df, c("^Cq$","^Ct$","^Cp$",
                    "Cq\\s*Mean","Ct\\s*Mean","Cp\\s*Mean",
                    "Cq\\s*Avg","Ct\\s*Avg","Cp\\s*Avg",
                    "Cq.*Value","Ct.*Value","Cp.*Value",
                    "Quant.*Cq","Cross.*Point"))
}
pick_name_col <- function(df) {
  if ("Name" %in% names(df)) return("Name")
  find_column(df, c("^Name$","Sample.*Name","^Sample$","Identifier","ID",
                    "^Well$","Well.*Position","Position"))
}

# ==============================================================================
# EXTRACTION: read Samples + per-target Result sheets, map Well->Name
# ==============================================================================

extract_cq_values <- function(micrun_file) {
  stopifnot(file.exists(micrun_file))
  sheets <- readxl::excel_sheets(micrun_file)

  # --- Samples (Well -> Name) ---
  samples_raw <- suppressMessages(readxl::read_excel(micrun_file, sheet = "Samples"))
  well_col <- if ("Well" %in% names(samples_raw)) "Well" else find_column(samples_raw, c("^Well$","Well.*Pos"))
  name_col <- if ("Name" %in% names(samples_raw)) "Name" else find_column(samples_raw, c("^Name$","Sample.*Name"))
  type_col <- find_column(samples_raw, c("^Type$","Sample.*Type","Control.*Type"))

  samples <- tibble::tibble(
    Well = if (!is.null(well_col)) as.character(samples_raw[[well_col]]) else NA_character_,
    Name = if (!is.null(name_col)) as.character(samples_raw[[name_col]]) else NA_character_,
    Type = if (!is.null(type_col)) as.character(samples_raw[[type_col]]) else "Unknown"
  ) |>
    dplyr::mutate(
      Name = dplyr::coalesce(Name, Well),
      is_control = Type %in% c("NTC","Standard","Positive Control","Negative Control","Positive","Negative") |
        grepl("Control|NTC|CP|CN", Name, ignore.case = TRUE),
      Type = dplyr::case_when(
        Type == "NTC" ~ "Negative",
        Type == "Negative Control" ~ "Negative",
        Type == "Positive Control" ~ "Positive",
        grepl("^CP$|Pos.*Control", Name, ignore.case=TRUE) ~ "Positive",
        grepl("^CN$|Neg.*Control|NTC", Name, ignore.case=TRUE) ~ "Negative",
        TRUE ~ Type
      )
    )

  # --- Targets -> RESULT sheets ---
  target_defs <- list(
    "177T"        = "Cycling 177T Result",
    "18S2"        = "Cycling 18S2 Result",
    "RNAseP_DNA"  = "Cycling RNAseP-DNA Result",
    "RNAseP_RNA"  = "Cycling RNAseP-RNA Result"
  )

  cq_data <- tibble::tibble()
  thresholds <- list()

  for (tgt in names(target_defs)) {
    result_sheet <- target_defs[[tgt]]
    if (!(result_sheet %in% sheets)) next

    df <- read_any_result_table(micrun_file, result_sheet)
    if (!nrow(df)) next

    cq_col <- pick_cq_col(df)
    nm_col <- pick_name_col(df)
    if (is.null(cq_col) || is.null(nm_col)) next

    tmp <- df |>
      dplyr::transmute(Name_raw = .data[[nm_col]],
                       Cq = suppressWarnings(as.numeric(.data[[cq_col]]))) |>
      dplyr::mutate(target = tgt)

    # If Name_raw are wells, map via Samples
    if (identical(nm_col, "Well") || all(grepl("^[A-H]\\d{1,2}$", tmp$Name_raw, perl=TRUE), na.rm=TRUE)) {
      tmp <- tmp |>
        dplyr::rename(Well = Name_raw) |>
        dplyr::left_join(samples |> dplyr::select(Well, Name), by = "Well") |>
        dplyr::mutate(Name = dplyr::coalesce(Name, Well)) |>
        dplyr::select(Name, Cq, target)
    } else {
      tmp <- tmp |>
        dplyr::rename(Name = Name_raw) |>
        dplyr::select(Name, Cq, target)
    }

    cq_data <- dplyr::bind_rows(cq_data, tmp)

    # optional: threshold from same sheet
    res_raw <- suppressMessages(readxl::read_excel(micrun_file, sheet = result_sheet, col_names = FALSE))
    res_mat <- apply(res_raw, 2, as.character)
    has_thr <- which(apply(res_mat, 1, function(r) any(grepl("Threshold|Cutoff", r, TRUE))))
    if (length(has_thr)) {
      row_i <- has_thr[1]
      nums <- suppressWarnings(as.numeric(res_mat[row_i, ]))
      thr  <- nums[which(!is.na(nums))[1]]
      if (!is.na(thr)) thresholds[[tgt]] <- thr
    }
  }

  if (!nrow(cq_data)) {
    stop("No per-sample Cq rows found in RESULT sheets after header detection. ",
         "Your *Data* tabs are raw curves; per-sample calls live in *Result* tabs.")
  }

  # Avoid many-to-many on Name -> use a distinct lookup
  samples_simple <- samples %>%
    dplyr::select(Name, Type, is_control) %>%
    dplyr::distinct()

  out <- cq_data %>%
    dplyr::left_join(samples_simple, by = "Name") %>%
    dplyr::group_by(Name, target) %>%
    dplyr::mutate(Replicate = paste0("Rep", dplyr::row_number())) %>%
    dplyr::ungroup()

  list(
    cq_data = out,
    samples = samples,
    run_settings = list(),  # fill if you need "Run Information" parsing
    thresholds = thresholds
  )
}

# ==============================================================================
# INTERPRETATION: per-target Positive/Indeterminate/Negative via cutoffs
# (robust to -1 / NA / "Undetermined")
# ==============================================================================

apply_interpretation <- function(cq_data, cutoffs = DEFAULT_CUTOFFS) {
  # optional robustification if some Cq appear as strings
  cq_data %>%
    mutate(
      # handle -1 / NA / Inf as no amplification (Negative)
      interpretation = pmap_chr(
        list(target, Cq),
        function(tgt, cq) {
          # treat invalids as Negative (no amplification)
          if (is.na(cq) || cq < 0 || is.infinite(cq)) return("Negative")
          if (!tgt %in% names(cutoffs)) return("Unknown")
          tgt_cutoffs <- cutoffs[[tgt]]
          if (cq <= tgt_cutoffs$positive) {
            "Positive"
          } else if (cq >= tgt_cutoffs$negative) {
            "Negative"
          } else {
            "Indeterminate"
          }
        }
      )
    )
}

# ==============================================================================
# DECISION: unified Trypanozoon calls + control checks + RNA preservation judge
# ==============================================================================

compute_nuc_acid_quality <- function(df, rnasep_rna_cutoff = 30) {
  df %>%
    mutate(
      rnasep_dna = case_when(
        is.na(RNAseP_DNA) | RNAseP_DNA == "Indeterminate" ~ "Poor DNA",
        RNAseP_DNA == "Negative" ~ "Poor DNA",
        RNAseP_DNA == "Positive" ~ "Good",
        TRUE ~ "Poor DNA"
      ),
      rnasep_rna = case_when(
        is.na(RNAseP_RNA) | RNAseP_RNA == "Indeterminate" ~ "No RNA",
        RNAseP_RNA == "Negative" ~ "No RNA",
        RNAseP_RNA == "Positive" & RNAseP_RNA_Cq <= rnasep_rna_cutoff ~ "Good",
        RNAseP_RNA == "Positive" & RNAseP_RNA_Cq > rnasep_rna_cutoff ~ "Poor RNA",
        TRUE ~ "No RNA"
      )
    )
}

# ΔCq judge for RNA preservation: Δ = RNaseP_RNA_Cq - RNaseP_DNA_Cq
evaluate_rna_preservation <- function(df, delta_good = PRESERVATION_DELTA_GOOD, delta_warn = PRESERVATION_DELTA_WARN) {
  coerce_num <- function(x) suppressWarnings(as.numeric(ifelse(x %in% c("Undetermined","No Ct","No Cq"), NA, x)))
  rna_cq  <- coerce_num(df$RNAseP_RNA_Cq)
  dna_cq  <- coerce_num(df$RNAseP_DNA_Cq)

  rna_nd  <- is.na(rna_cq) | (rna_cq < 0)
  dna_nd  <- is.na(dna_cq) | (dna_cq < 0)

  delta <- rna_cq - dna_cq

  preservation <- dplyr::case_when(
    dna_nd                    ~ "Unknown (no RNAseP-DNA)",
    rna_nd                    ~ "Severe RNA loss (no RNAseP-RNA)",
    is.infinite(delta)        ~ "Unknown",
    is.na(delta)              ~ "Unknown",
    delta <= delta_good       ~ "Good RNA preservation",
    delta <= delta_warn       ~ "Moderate RNA loss",
    delta >  delta_warn       ~ "Poor RNA preservation",
    TRUE                      ~ "Unknown"
  )

  dplyr::mutate(df,
                RNA_Preservation_Status = preservation,
                RNA_Preservation_Delta  = delta
  )
}

apply_trypanozoon_decision <- function(replicate_summary,
                                       rnasep_rna_cutoff = 30,
                                       delta_good = PRESERVATION_DELTA_GOOD,
                                       delta_warn = PRESERVATION_DELTA_WARN) {
  x <- compute_nuc_acid_quality(replicate_summary, rnasep_rna_cutoff)
  x <- evaluate_rna_preservation(x, delta_good = delta_good, delta_warn = delta_warn)

  x %>%
    mutate(
      dna_pos = (marker_177T == "Positive"),
      rna_pos = (marker_18S2 == "Positive"),

      # Base decision for unknown samples
      decision_base = case_when(
        dna_pos & rna_pos ~ "Trypanozoon detected (TNA-based)",
        dna_pos         ~ "Trypanozoon DNA detected",
        rna_pos         ~ "Trypanozoon RNA detected",
        rnasep_dna == "Poor DNA" ~ "Failed DNA extraction",
        rnasep_rna == "Poor RNA" ~ "Negative [RNA extraction failed]",
        TRUE ~ "Negative"
      ),
      category_base = case_when(
        dna_pos | rna_pos ~ "positive",
        grepl("^Failed DNA extraction$", decision_base) ~ "failed",
        grepl("RNA extraction failed", decision_base) ~ "negative_rna_failed",
        TRUE ~ "negative"
      ),

      # Control overrides (✓/⚠)
      decision = case_when(
        Type %in% c("Negative","NTC") & (dna_pos | rna_pos) ~ "⚠ Contaminated Negative Control",
        Type %in% c("Negative","NTC") & !(dna_pos | rna_pos) ~ "✓ Negative control OK",
        Type %in% c("Positive","Standard","CP") & (dna_pos & rna_pos) ~ "✓ Positive control OK (TNA)",
        Type %in% c("Positive","Standard","CP") & dna_pos ~ "✓ Positive control OK (DNA)",
        Type %in% c("Positive","Standard","CP") & rna_pos ~ "✓ Positive control OK (RNA)",
        Type %in% c("Positive","Standard","CP") & !(dna_pos | rna_pos) ~ "⚠ Failed Positive Control",
        TRUE ~ decision_base
      ),
      category = case_when(
        grepl("control OK", decision) ~ "control_ok",
        grepl("Contaminated Negative Control|Failed Positive Control", decision) ~ "control_fail",
        TRUE ~ category_base
      )
    ) %>%
    select(-dna_pos, -rna_pos, -decision_base, -category_base)
}

# ==============================================================================
# SUMMARIES
# ==============================================================================

summarize_by_replicate <- function(cq_data) {
  replicate_summary <- cq_data %>%
    select(Name, Replicate, target, interpretation, Cq, is_control, Type) %>%
    pivot_wider(
      names_from = target,
      values_from = c(interpretation, Cq),
      names_sep = "_"
    )

  names(replicate_summary) <- gsub("interpretation_", "marker_", names(replicate_summary))

  required_markers <- c("marker_177T", "marker_18S2", "marker_RNAseP_DNA", "marker_RNAseP_RNA")
  required_cqs     <- c("Cq_177T", "Cq_18S2", "Cq_RNAseP_DNA", "Cq_RNAseP_RNA")

  for (col in required_markers) if (!col %in% names(replicate_summary)) replicate_summary[[col]] <- NA_character_
  for (col in required_cqs)     if (!col %in% names(replicate_summary)) replicate_summary[[col]] <- NA_real_

  replicate_summary %>%
    rename(
      RNAseP_DNA    = marker_RNAseP_DNA,
      RNAseP_RNA    = marker_RNAseP_RNA,
      RNAseP_DNA_Cq = Cq_RNAseP_DNA,
      RNAseP_RNA_Cq = Cq_RNAseP_RNA
    )
}

summarize_by_sample <- function(replicate_decisions) {
  replicate_decisions %>%
    group_by(Name, is_control, Type) %>%
    summarize(
      n_replicates   = n(),
      n_positive     = sum(category == "positive", na.rm = TRUE),
      n_negative     = sum(category == "negative", na.rm = TRUE),
      n_failed       = sum(category == "failed",   na.rm = TRUE),
      n_inconclusive = sum(category == "inconclusive", na.rm = TRUE),
      n_control_ok   = sum(category == "control_ok",   na.rm = TRUE),
      n_control_fail = sum(category == "control_fail", na.rm = TRUE),

      final_decision = {
        d <- decision[!is.na(decision)]
        if (length(d) == 0) "No valid replicates" else names(sort(table(d), decreasing = TRUE))[1]
      },

      avg_177T_Cq = mean(Cq_177T[marker_177T == "Positive"], na.rm = TRUE),
      avg_18S2_Cq = mean(Cq_18S2[marker_18S2 == "Positive"], na.rm = TRUE),

      rna_quality = {
        s <- rnasep_rna[!is.na(rnasep_rna)]
        if (!length(s)) "Unknown" else if (all(s == "Good")) "Good"
        else if (all(s == "No RNA")) "No RNA" else if (any(s == "Poor RNA")) "Poor RNA" else "Mixed"
      },
      dna_quality = {
        s <- rnasep_dna[!is.na(rnasep_dna)]
        if (!length(s)) "Unknown" else if (all(s == "Good")) "Good" else "Poor DNA"
      },

      # RNA preservation consensus & average ΔCq
      rna_preservation = {
        vals <- RNA_Preservation_Status[!is.na(RNA_Preservation_Status)]
        if (!length(vals)) "Unknown" else names(sort(table(vals), decreasing = TRUE))[1]
      },
      avg_preservation_delta = mean(RNA_Preservation_Delta, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      final_category = case_when(
        n_control_fail > 0 ~ "control_fail",
        n_control_ok   > 0 & is_control ~ "control_ok",
        n_positive >= 2 ~ "positive",
        n_negative >= 2 ~ "negative",
        n_failed   >= 2 ~ "failed",
        TRUE ~ "inconclusive"
      ),
      quality_flag = case_when(
        final_category == "control_fail"                     ~ "⚠ Control out of spec",
        rna_preservation == "Severe RNA loss (no RNAseP-RNA)"~ "⚠ Severe RNA loss",
        rna_preservation == "Poor RNA preservation"          ~ "⚠ Poor RNA preservation",
        dna_quality == "Poor DNA"                            ~ "⚠ Poor DNA extraction",
        rna_quality == "Poor RNA"                            ~ "⚠ Poor RNA extraction",
        n_replicates < 3                                     ~ "⚠ Insufficient replicates",
        final_category == "inconclusive"                     ~ "⚠ Inconclusive results",
        TRUE ~ "✓ Good"
      )
    )
}

# ==============================================================================
# MAIN PIPELINE
# ==============================================================================

analyze_qpcr <- function(micrun_file,
                         rnasep_rna_cutoff = DEFAULT_CUTOFFS$RNAseP_RNA$positive,
                         cutoffs = DEFAULT_CUTOFFS,
                         verbose = TRUE) {

  if (verbose) cat("📂 Loading data from:", basename(micrun_file), "\n")

  extracted <- extract_cq_values(micrun_file)
  if (nrow(extracted$cq_data) == 0) stop("No Cq data extracted; check sheet names and column headings.")

  if (verbose) {
    cat("✓ Found", nrow(extracted$samples), "samples\n")
    cat("✓ Targets:", paste(unique(extracted$cq_data$target), collapse = ", "), "\n")
  }

  if (verbose) cat("🔍 Interpreting Cq values...\n")
  interpreted <- apply_interpretation(extracted$cq_data, cutoffs)

  if (verbose) cat("📊 Summarizing by replicate...\n")
  replicate_summary <- summarize_by_replicate(interpreted)

  if (verbose) cat("🎯 Applying decision logic...\n")
  replicate_decisions <- apply_trypanozoon_decision(
    replicate_summary,
    rnasep_rna_cutoff = rnasep_rna_cutoff
  )

  if (verbose) cat("📋 Creating sample summary...\n")
  sample_summary <- summarize_by_sample(replicate_decisions)

  if (verbose) {
    cat("\n✅ Analysis complete!\n")
    cat("   Samples:", nrow(sample_summary), "\n")
    cat("   Positive:", sum(sample_summary$final_category == "positive"), "\n")
    cat("   Negative:", sum(sample_summary$final_category == "negative"), "\n")
    cat("   Failed/Inconclusive:",
        sum(sample_summary$final_category %in% c("failed", "inconclusive")), "\n\n")
  }

  list(
    sample_summary = sample_summary,
    replicate_data = replicate_decisions,
    cq_data        = interpreted,
    run_settings   = extracted$run_settings,
    thresholds     = extracted$thresholds,
    cutoffs        = cutoffs
  )
}

# ==============================================================================
# EXPORT / REPORT
# ==============================================================================

export_to_excel <- function(results, output_file) {
  require(writexl)
  sheets <- list(
    "Sample Summary"    = results$sample_summary,
    "Replicate Details" = results$replicate_data,
    "All Cq Values"     = results$cq_data
  )
  writexl::write_xlsx(sheets, output_file)
  cat("✓ Results exported to:", output_file, "\n")
}

generate_report <- function(results) {
  report <- c(
    strrep("=", 70),
    "qPCR ANALYSIS REPORT",
    strrep("=", 70),
    "",
    "SUMMARY",
    strrep("-", 70),
    sprintf("Total samples: %d", nrow(results$sample_summary))
  )

  summary_counts <- results$sample_summary %>%
    count(final_category, .drop = FALSE)

  for (i in seq_len(nrow(summary_counts))) {
    report <- c(report,
                sprintf("  %s: %d",
                        summary_counts$final_category[i],
                        summary_counts$n[i]))
  }

  report <- c(report, "", "CONTROL ISSUES", strrep("-", 70))
  ctrl <- results$sample_summary %>%
    filter(final_category %in% c("control_ok","control_fail")) %>%
    arrange(Name)
  if (nrow(ctrl) > 0) {
    for (i in seq_len(nrow(ctrl))) {
      report <- c(report,
                  sprintf("%-15s  %s",
                          ctrl$Name[i],
                          ctrl$quality_flag[i]))
    }
  } else {
    report <- c(report, "  None")
  }

  report <- c(report, "", "RNA PRESERVATION ISSUES", strrep("-", 70))
  pres <- results$sample_summary %>%
    dplyr::filter(rna_preservation %in% c("Severe RNA loss (no RNAseP-RNA)",
                                          "Poor RNA preservation",
                                          "Moderate RNA loss")) %>%
    dplyr::arrange(desc(rna_preservation))
  if (nrow(pres) > 0) {
    for (i in seq_len(nrow(pres))) {
      report <- c(report,
                  sprintf("%-15s  %s  (ΔCq=%.2f)",
                          pres$Name[i], pres$rna_preservation[i], pres$avg_preservation_delta[i]))
    }
  } else {
    report <- c(report, "  None")
  }

  report <- c(report, "", strrep("=", 70))
  report
}

# ==============================================================================
# END
# ==============================================================================
