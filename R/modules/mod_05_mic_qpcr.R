# R/modules/mod_05_mic_qpcr.R
# =============================================================================
# Module 05 — MIC qPCR: BioMérieux MIC Analysis with Full QC (REBUILT)
# =============================================================================
# Clean, modular architecture for MIC file analysis with:
# - Incremental file parsing with caching
# - User-adjustable thresholds and QC parameters
# - Levey-Jennings monitoring
# - ΔCq metrics and flagging
# - Comprehensive QC scatter plots
# - Export capabilities
# =============================================================================

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
# CONFIGURATION & DEFAULTS
# =============================================================================

default_mic_settings <- function() {
  list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40),
      `18S2` = list(positive = 35, negative = 40),
      RNAseP_DNA = list(positive = 28, negative = 40),
      RNAseP_RNA = list(positive = 35, negative = 40)
    ),
    late_window = c(35, 40),
    delta_rp_limit = 8,
    allow_review_controls = FALSE,
    pc_aliases = c("PC", "POS", "POSITIVE", "CP"),
    nc_aliases = c("NC", "NEG", "NTC", "CN")
  )
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

normalize_id <- function(x) {
  if (is.null(x)) return(NA_character_)
  x %>% as.character() %>% str_trim() %>% toupper()
}

normalize_field_name <- function(x) {
  if (is.null(x)) return(NA_character_)

  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "")
}

match_column_name <- function(data_names, candidates) {
  if (is.null(data_names) || !length(data_names)) {
    return(NULL)
  }

  normalized_cols <- tibble(
    original = data_names,
    normalized = normalize_field_name(data_names)
  ) %>%
    mutate(normalized = replace_na(normalized, ""))

  for (candidate in candidates) {
    cand_norm <- normalize_field_name(candidate)

    if (is.na(cand_norm) || !nzchar(cand_norm)) {
      next
    }

    direct_match <- which(normalized_cols$normalized == cand_norm)
    if (length(direct_match)) {
      return(normalized_cols$original[direct_match[1]])
    }

    partial_match <- which(vapply(normalized_cols$normalized, function(col_norm) {
      if (!nzchar(col_norm)) {
        return(FALSE)
      }

      str_detect(col_norm, fixed(cand_norm)) || str_detect(cand_norm, fixed(col_norm))
    }, logical(1)))

    if (length(partial_match)) {
      return(normalized_cols$original[partial_match[1]])
    }
  }

  NULL
}

safe_get_column <- function(df, possible_names, default = NA_character_) {
  # Try each possible column name and return the first one found
  for (name in possible_names) {
    if (!is.null(df) && name %in% names(df)) {
      return(df[[name]])
    }
  }
  rep(default, nrow(df))
}

parse_aliases <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character())
  text %>% 
    str_split(",") %>% 
    unlist() %>% 
    str_trim() %>% 
    discard(~.x == "") %>% 
    toupper()
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (length(na.omit(x)) <= 1) return(NA_real_)
  sd(x, na.rm = TRUE)
}

# =============================================================================
# FILE DISCOVERY & CACHING
# =============================================================================

scan_mic_directory <- function(path) {
  if (!dir.exists(path)) {
    return(tibble(
      file_path = character(),
      file_name = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      hash = character()
    ))
  }
  
  files <- list.files(path, pattern = "\\.(xls|xlsx)$", 
                      full.names = TRUE, ignore.case = TRUE)
  
  if (!length(files)) {
    return(tibble(
      file_path = character(),
      file_name = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      hash = character()
    ))
  }
  
  # Get file info for all files at once (vectorized - faster than map)
  info_df <- file.info(files)

  tibble(
    file_path = files,
    file_name = basename(files),
    size = info_df$size,
    mtime = info_df$mtime,
    hash = map2_chr(files, seq_along(files), ~digest(paste(.x, info_df$size[.y], info_df$mtime[.y]), algo = "md5"))
  )
}

parse_run_datetime <- function(filename) {
  base <- tools::file_path_sans_ext(filename)
  parsed <- suppressWarnings(
    parse_date_time(base, orders = c("Ymd HMS", "Ymd HM", "Ymd"), truncated = 2)
  )
  if (is.na(parsed)) return(as.POSIXct(NA))
  parsed
}

# =============================================================================
# FILE PARSING (leveraging qpcr_analysis.R)
# =============================================================================

parse_single_mic_file <- function(file_info, settings) {
  result <- tryCatch({
    
    # Use the analyze_qpcr function from qpcr_analysis.R
    analysis <- analyze_qpcr(
      micrun_file = file_info$file_path,
      rnasep_rna_cutoff = settings$thresholds$RNAseP_RNA$positive,
      cutoffs = settings$thresholds,
      verbose = FALSE
    )
    
    run_id <- tools::file_path_sans_ext(file_info$file_name)
    run_datetime <- parse_run_datetime(file_info$file_name)
    
    # Get replicate data and ensure it has required columns
    rep_data <- analysis$replicate_data
    
    if (!nrow(rep_data)) {
      warning(glue("No replicate data in {file_info$file_name}"))
      return(list(success = FALSE, error = "No replicate data"))
    }
    
    # Create replicates_long with proper structure
    replicates_long <- tibble()

    # Process each target
    target_mappings <- list(
      "177T" = list(cq = "Cq_177T", marker = "marker_177T"),
      "18S2" = list(cq = "Cq_18S2", marker = "marker_18S2"),
      "RNAseP_DNA" = list(cq = "RNAseP_DNA_Cq", marker = "RNAseP_DNA"),  # DIFFERENT!
      "RNAseP_RNA" = list(cq = "RNAseP_RNA_Cq", marker = "RNAseP_RNA")   # DIFFERENT!
    )
    
    for (target in names(target_mappings)) {
      cq_col <- target_mappings[[target]]$cq
      marker_col <- target_mappings[[target]]$marker

      if (cq_col %in% names(rep_data) && marker_col %in% names(rep_data)) {
        target_data <- rep_data %>%
          mutate(
            RunID = run_id,
            SampleID = normalize_id(Name),
            SampleName = Name,
            Target = target,
            Cq = .data[[cq_col]],
            Call = .data[[marker_col]],
            ControlType = case_when(
              Type %in% c("Positive", "Standard", "CP") ~ "PC",
              Type %in% c("Negative", "NTC", "CN") ~ "NC",
              TRUE ~ "Sample"
            )
          ) %>%
          select(RunID, SampleID, SampleName, Replicate, ControlType, Target, Cq, Call)
        
        replicates_long <- bind_rows(replicates_long, target_data)
      }
    }
    
    if (!nrow(replicates_long)) {
      warning(glue("No valid target data in {file_info$file_name}"))
      return(list(success = FALSE, error = "No valid target data"))
    }
    
    # Aggregate samples using pipeline-derived sample summary where available
    sample_summary <- analysis$sample_summary
    if (is.null(sample_summary)) {
      sample_summary <- tibble()
    }

    samples <- aggregate_samples_from_replicates(
      replicates_long,
      sample_summary,
      settings,
      run_id
    )
    
    if (!nrow(samples)) {
      warning(glue("Failed to aggregate samples in {file_info$file_name}"))
      return(list(success = FALSE, error = "Failed to aggregate samples"))
    }
    
    # Run metadata - FIXED: Convert JSON to character
    run_meta <- tibble(
      RunID = run_id,
      FilePath = file_info$file_path,
      FileName = file_info$file_name,
      FileMTime = file_info$mtime,
      RunDateTime = run_datetime,
      WellCount = n_distinct(rep_data$Name),
      ThresholdsJSON = as.character(toJSON(settings$thresholds, auto_unbox = TRUE))
    )
    
    list(
      run = run_meta,
      replicates = replicates_long,
      samples = samples,
      success = TRUE
    )
    
  }, error = function(e) {
    warning(glue("Failed to parse {file_info$file_name}: {e$message}"))
    list(success = FALSE, error = as.character(e$message))
  })
  
  result
}

# =============================================================================
# SAMPLE AGGREGATION & INTERPRETATION
# =============================================================================

aggregate_samples_from_replicates <- function(replicates_long, sample_summary, settings, run_id) {

  if (!nrow(replicates_long)) {
    return(tibble(
      RunID = character(),
      SampleID = character(),
      SampleName = character(),
      ControlType = character(),
      Cq_median_177T = numeric(),
      Cq_median_18S2 = numeric(),
      Cq_median_RNAseP_DNA = numeric(),
      Cq_median_RNAseP_RNA = numeric(),
      Cq_mean_177T = numeric(),
      Cq_mean_18S2 = numeric(),
      Cq_mean_RNAseP_DNA = numeric(),
      Cq_mean_RNAseP_RNA = numeric(),
      Cq_sd_177T = numeric(),
      Cq_sd_18S2 = numeric(),
      Cq_sd_RNAseP_DNA = numeric(),
      Cq_sd_RNAseP_RNA = numeric(),
      Call_177T = character(),
      Call_18S2 = character(),
      Call_RNAseP_DNA = character(),
      Call_RNAseP_RNA = character(),
      Delta_18S2_177T = numeric(),
      Delta_RP = numeric(),
      Flags = character(),
      AnyFlag = logical(),
      FinalCall = character(),
      DecisionStep = character(),
      DecisionReason = character(),
      ConfidenceScore = character(),
      WellSummary = character(),
      WellAggregateConflict = logical(),
      ConflictDetails = character(),
      PipelineCategory = character(),
      PipelineDecision = character(),
      PipelineQualityFlag = character(),
      ReplicatesTotal = numeric(),
      Replicates_Positive = numeric(),
      Replicates_Negative = numeric(),
      Replicates_Failed = numeric(),
      Replicates_Inconclusive = numeric(),
      Replicates_ControlOK = numeric(),
      Replicates_ControlFail = numeric(),
      RNA_Quality = character(),
      DNA_Quality = character(),
      RNA_Preservation_Status = character(),
      RNA_Preservation_Delta = numeric(),
      Avg_177T_Positive_Cq = numeric(),
      Avg_18S2_Positive_Cq = numeric(),
      Sample_DNA_Positive = logical(),
      Sample_DNA_Suspect = logical(),
      Sample_RNA_Positive = logical(),
      Sample_RNA_Suspect = logical(),
      Sample_TNA_Positive = logical(),
      Sample_TNA_Suspect = logical(),
      Wells_DNA_Positive = numeric(),
      Wells_DNA_Suspect = numeric(),
      Wells_RNA_Positive = numeric(),
      Wells_RNA_Suspect = numeric(),
      Wells_TNA_Positive = numeric(),
      Wells_TNA_Suspect = numeric()
    ))
  }

  if (is.null(sample_summary)) {
    sample_summary <- tibble()
  }

  # Calculate median Cq per sample per target
  target_summary <- replicates_long %>%
    group_by(RunID, SampleID, SampleName, ControlType, Target) %>%
    summarise(
      Cq_median = safe_median(Cq),
      Cq_mean = safe_mean(Cq),
      Cq_sd = safe_sd(Cq),
      n_reps = sum(!is.na(Cq)),
      .groups = "drop"
    )

  # Pivot wide
  samples_wide <- target_summary %>%
    select(RunID, SampleID, SampleName, ControlType, Target,
           Cq_median, Cq_mean, Cq_sd) %>%
    pivot_wider(
      names_from = Target,
      values_from = c(Cq_median, Cq_mean, Cq_sd),
      names_glue = "{.value}_{Target}"
    )

  # Ensure all target columns exist
  targets <- c("177T", "18S2", "RNAseP_DNA", "RNAseP_RNA")
  stats_cols <- c("Cq_median", "Cq_mean", "Cq_sd")
  for (stat in stats_cols) {
    for (target in targets) {
      col <- paste0(stat, "_", target)
      if (!col %in% names(samples_wide)) {
        samples_wide[[col]] <- NA_real_
      }
    }
  }

  # Replicate-level co-detection (well scoring)
  well_flags <- replicates_long %>%
    filter(Target %in% c("177T", "18S2")) %>%
    mutate(
      TargetCall = map2_chr(Cq, Target, ~classify_target_vectorized(.x, .y, settings)),
      TargetDetected = TargetCall %in% c("Positive", "LatePositive"),
      TargetPositive = TargetCall == "Positive",
      TargetLate = TargetCall == "LatePositive"
    ) %>%
    group_by(RunID, SampleID, SampleName, Replicate) %>%
    summarise(
      Well_DNA_Positive = any(Target == "177T" & TargetPositive, na.rm = TRUE),
      Well_DNA_Suspect = any(Target == "177T" & TargetLate, na.rm = TRUE),
      Well_RNA_Positive = any(Target == "18S2" & TargetPositive, na.rm = TRUE),
      Well_RNA_Suspect = any(Target == "18S2" & TargetLate, na.rm = TRUE),
      Well_TNA_Positive = any(Target == "177T" & TargetPositive, na.rm = TRUE) &
        any(Target == "18S2" & TargetPositive, na.rm = TRUE),
      Well_TNA_Detected = any(Target == "177T" & TargetDetected, na.rm = TRUE) &
        any(Target == "18S2" & TargetDetected, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Well_TNA_Suspect = Well_TNA_Detected & !Well_TNA_Positive
    )

  well_counts <- if (nrow(well_flags)) {
    well_flags %>%
      group_by(RunID, SampleID, SampleName) %>%
      summarise(
        Wells_DNA_Positive = sum(Well_DNA_Positive, na.rm = TRUE),
        Wells_DNA_Suspect = sum(!Well_DNA_Positive & Well_DNA_Suspect, na.rm = TRUE),
        Wells_RNA_Positive = sum(Well_RNA_Positive, na.rm = TRUE),
        Wells_RNA_Suspect = sum(!Well_RNA_Positive & Well_RNA_Suspect, na.rm = TRUE),
        Wells_TNA_Positive = sum(Well_TNA_Positive, na.rm = TRUE),
        Wells_TNA_Suspect = sum(Well_TNA_Suspect, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    tibble(
      RunID = character(),
      SampleID = character(),
      SampleName = character(),
      Wells_DNA_Positive = numeric(),
      Wells_DNA_Suspect = numeric(),
      Wells_RNA_Positive = numeric(),
      Wells_RNA_Suspect = numeric(),
      Wells_TNA_Positive = numeric(),
      Wells_TNA_Suspect = numeric()
    )
  }

  samples_wide <- samples_wide %>%
    left_join(well_counts, by = c("RunID", "SampleID", "SampleName")) %>%
    mutate(
      Call_177T = classify_target_vectorized(coalesce(Cq_mean_177T, Cq_median_177T), "177T", settings),
      Call_18S2 = classify_target_vectorized(coalesce(Cq_mean_18S2, Cq_median_18S2), "18S2", settings),
      Call_RNAseP_DNA = classify_target_vectorized(coalesce(Cq_mean_RNAseP_DNA, Cq_median_RNAseP_DNA), "RNAseP_DNA", settings),
      Call_RNAseP_RNA = classify_target_vectorized(coalesce(Cq_mean_RNAseP_RNA, Cq_median_RNAseP_RNA), "RNAseP_RNA", settings)
    )

  # Calculate deltas
  samples_wide <- samples_wide %>%
    mutate(
      Delta_18S2_177T = coalesce(Cq_mean_18S2, Cq_median_18S2) - coalesce(Cq_mean_177T, Cq_median_177T),
      Delta_RP = coalesce(Cq_mean_RNAseP_RNA, Cq_median_RNAseP_RNA) - coalesce(Cq_mean_RNAseP_DNA, Cq_median_RNAseP_DNA)
    )

  summary_template <- tibble(
    RunID = character(),
    SampleID = character(),
    SampleName = character(),
    PipelineCategory = character(),
    PipelineDecision = character(),
    PipelineQualityFlag = character(),
    ReplicatesTotal = numeric(),
    Replicates_Positive = numeric(),
    Replicates_Negative = numeric(),
    Replicates_Failed = numeric(),
    Replicates_Inconclusive = numeric(),
    Replicates_ControlOK = numeric(),
    Replicates_ControlFail = numeric(),
    Avg_177T_Positive_Cq = numeric(),
    Avg_18S2_Positive_Cq = numeric(),
    RNA_Quality = character(),
    DNA_Quality = character(),
    RNA_Preservation_Status = character(),
    RNA_Preservation_Delta = numeric()
  )

  summary_clean <- summary_template

  if (nrow(sample_summary)) {
    summary_clean <- sample_summary %>%
      mutate(
        SampleName = as.character(Name),
        SampleID = normalize_id(SampleName),
        RunID = run_id
      ) %>%
      transmute(
        RunID,
        SampleID,
        SampleName,
        PipelineCategory = as.character(final_category),
        PipelineDecision = as.character(final_decision),
        PipelineQualityFlag = as.character(quality_flag),
        ReplicatesTotal = as.numeric(n_replicates),
        Replicates_Positive = as.numeric(n_positive),
        Replicates_Negative = as.numeric(n_negative),
        Replicates_Failed = as.numeric(n_failed),
        Replicates_Inconclusive = as.numeric(n_inconclusive),
        Replicates_ControlOK = as.numeric(n_control_ok),
        Replicates_ControlFail = as.numeric(n_control_fail),
        Avg_177T_Positive_Cq = as.numeric(avg_177T_Cq),
        Avg_18S2_Positive_Cq = as.numeric(avg_18S2_Cq),
        RNA_Quality = as.character(rna_quality),
        DNA_Quality = as.character(dna_quality),
        RNA_Preservation_Status = as.character(rna_preservation),
        RNA_Preservation_Delta = as.numeric(avg_preservation_delta)
      )
  }

  samples_wide <- samples_wide %>%
    left_join(summary_clean, by = c("RunID", "SampleID", "SampleName"))

  # Determine final call and flags - use rowwise for complex logic
  # Extract min_positive_reps setting (default 2/4)
  min_tna_reps <- if (!is.null(settings$min_positive_reps)) {
    as.numeric(settings$min_positive_reps)
  } else {
    2  # Default: require 2/4 replicates for positive call
  }

  samples_wide <- samples_wide %>%
    rowwise() %>%
    mutate(
      PipelineCategoryLower = tolower(coalesce(PipelineCategory, "")),

      # Calculate QC pass count (based on RNAseP-DNA detection)
      QC_Pass_Count = coalesce(ReplicatesTotal, 4) - coalesce(Replicates_Failed, 0),
      NegativeReplicates = coalesce(Replicates_Negative, 0),

      # Per-target positivity based on well counts
      PositiveTryp = (Call_177T == "Positive" | Call_18S2 == "Positive"),
      LateTryp = !PositiveTryp & (Call_177T == "LatePositive" | Call_18S2 == "LatePositive"),
      HostOK = (Call_RNAseP_DNA %in% c("Positive", "LatePositive")),
      HostFailed = (ControlType == "Sample" & !HostOK),
      Flag_SampleDecay = !is.na(Delta_RP) & Delta_RP > settings$delta_rp_limit,
      PipelineQualityFlagClean = if_else(
        !is.na(PipelineQualityFlag) & grepl("^⚠", PipelineQualityFlag),
        trimws(sub("^⚠\\s*", "", PipelineQualityFlag)),
        NA_character_
      ),
      PipelineDecisionFlag = if_else(
        ControlType %in% c("PC", "NC") & !is.na(PipelineDecision) & grepl("^⚠", PipelineDecision),
        trimws(sub("^⚠\\s*", "", PipelineDecision)),
        NA_character_
      ),
      Sample_DNA_Positive = Call_177T == "Positive",
      Sample_DNA_Suspect = Call_177T == "LatePositive",
      Sample_RNA_Positive = Call_18S2 == "Positive",
      Sample_RNA_Suspect = Call_18S2 == "LatePositive",
      Sample_TNA_Positive = (Call_177T == "Positive" & Call_18S2 == "Positive"),
      Sample_TNA_Suspect = (
        Call_177T %in% c("Positive", "LatePositive") &
          Call_18S2 %in% c("Positive", "LatePositive") &
          !(Call_177T == "Positive" & Call_18S2 == "Positive")
      ),

      # Enhanced final call using clear decision tree with detailed reasoning
      FinalCall = case_when(
        # Step 0: Handle controls first
        ControlType %in% c("PC", "NC") & PipelineCategoryLower == "control_fail" ~ "Control_Fail",
        ControlType %in% c("PC", "NC") ~ "Control",

        # Step 1: Check QC validity (all replicates failed QC)
        QC_Pass_Count == 0 ~ "Invalid",

        # Step 2: TNA-based calling (highest priority) - use well counts
        Wells_TNA_Positive >= min_tna_reps ~ "Positive",

        # Step 3: Single replicate TNA positive
        Wells_TNA_Positive == 1 ~ "Indeterminate",

        # Step 4: DNA or RNA only patterns (using well counts)
        Wells_DNA_Positive >= min_tna_reps & Wells_RNA_Positive == 0 ~ "Positive_DNA",
        Wells_RNA_Positive >= min_tna_reps & Wells_DNA_Positive == 0 ~ "Positive_RNA",

        # Step 5: Late positives (TNA suspect with sufficient replicates)
        Wells_TNA_Suspect >= min_tna_reps ~ "LatePositive",

        # Step 6a: Single weak signal but strong negative evidence
        Wells_DNA_Positive == 1 & Wells_RNA_Positive == 0 & NegativeReplicates >= 2 ~ "Negative",
        Wells_RNA_Positive == 1 & Wells_DNA_Positive == 0 & NegativeReplicates >= 2 ~ "Negative",

        # Step 6b: Mixed weak signals (1 replicate with DNA or RNA)
        Wells_DNA_Positive == 1 | Wells_RNA_Positive == 1 ~ "Indeterminate",
        Wells_TNA_Suspect == 1 ~ "Indeterminate",

        # Step 7: True negative (good QC, no detection)
        NegativeReplicates >= 3 ~ "Negative",

        # Step 8: Insufficient valid replicates
        TRUE ~ "Indeterminate"
      ),

      # Capture which step made the decision
      DecisionStep = case_when(
        ControlType %in% c("PC", "NC") & PipelineCategoryLower == "control_fail" ~ "Step0",
        ControlType %in% c("PC", "NC") ~ "Step0",
        QC_Pass_Count == 0 ~ "Step1",
        Wells_TNA_Positive >= min_tna_reps ~ "Step2",
        Wells_TNA_Positive == 1 ~ "Step3",
        Wells_DNA_Positive >= min_tna_reps & Wells_RNA_Positive == 0 ~ "Step4a",
        Wells_RNA_Positive >= min_tna_reps & Wells_DNA_Positive == 0 ~ "Step4b",
        Wells_TNA_Suspect >= min_tna_reps ~ "Step5",
        Wells_DNA_Positive == 1 & Wells_RNA_Positive == 0 & NegativeReplicates >= 2 ~ "Step6a",
        Wells_RNA_Positive == 1 & Wells_DNA_Positive == 0 & NegativeReplicates >= 2 ~ "Step6a",
        Wells_DNA_Positive == 1 | Wells_RNA_Positive == 1 ~ "Step6b",
        Wells_TNA_Suspect == 1 ~ "Step6b",
        NegativeReplicates >= 3 ~ "Step7",
        TRUE ~ "Step8"
      ),

      # Detailed explanation of the decision
      DecisionReason = case_when(
        ControlType %in% c("PC", "NC") & PipelineCategoryLower == "control_fail" ~
          "Control sample failed quality checks",
        ControlType %in% c("PC", "NC") ~
          "Control sample passed quality checks",
        QC_Pass_Count == 0 ~
          "All replicates failed QC (no RNAseP-DNA detected)",
        Wells_TNA_Positive >= min_tna_reps ~
          sprintf("Strong TNA positive: %d/%d wells with both DNA+RNA positive", Wells_TNA_Positive, coalesce(ReplicatesTotal, 4)),
        Wells_TNA_Positive == 1 ~
          sprintf("Weak TNA signal: only 1/%d well with both DNA+RNA positive", coalesce(ReplicatesTotal, 4)),
        Wells_DNA_Positive >= min_tna_reps & Wells_RNA_Positive == 0 ~
          sprintf("DNA-only positive: %d/%d wells DNA+, no RNA detected", Wells_DNA_Positive, coalesce(ReplicatesTotal, 4)),
        Wells_RNA_Positive >= min_tna_reps & Wells_DNA_Positive == 0 ~
          sprintf("RNA-only positive: %d/%d wells RNA+, no DNA detected", Wells_RNA_Positive, coalesce(ReplicatesTotal, 4)),
        Wells_TNA_Suspect >= min_tna_reps ~
          sprintf("Late/weak TNA signal: %d/%d wells with late detection", Wells_TNA_Suspect, coalesce(ReplicatesTotal, 4)),
        Wells_DNA_Positive == 1 & Wells_RNA_Positive == 0 & NegativeReplicates >= 2 ~
          sprintf("Single DNA outlier with %d negative replicates", NegativeReplicates),
        Wells_RNA_Positive == 1 & Wells_DNA_Positive == 0 & NegativeReplicates >= 2 ~
          sprintf("Single RNA outlier with %d negative replicates", NegativeReplicates),
        Wells_DNA_Positive == 1 | Wells_RNA_Positive == 1 ~
          sprintf("Single weak signal: DNA=%d, RNA=%d", Wells_DNA_Positive, Wells_RNA_Positive),
        Wells_TNA_Suspect == 1 ~
          "Single well with late/suspect TNA detection",
        NegativeReplicates >= 3 ~
          sprintf("Clear negative: %d/%d replicates negative", NegativeReplicates, coalesce(ReplicatesTotal, 4)),
        TRUE ~
          sprintf("Insufficient valid replicates (QC passed: %d)", QC_Pass_Count)
      ),

      # Create well-level summary text
      WellSummary = if_else(
        ControlType %in% c("PC", "NC"),
        "Control sample",
        sprintf("DNA:%d+/%ds, RNA:%d+/%ds, TNA:%d+/%ds",
                Wells_DNA_Positive, Wells_DNA_Suspect,
                Wells_RNA_Positive, Wells_RNA_Suspect,
                Wells_TNA_Positive, Wells_TNA_Suspect)
      ),

      # Calculate confidence score based on replicate agreement
      ConfidenceScore = case_when(
        ControlType %in% c("PC", "NC") ~ "Control",
        QC_Pass_Count == 0 ~ "Invalid",

        # High confidence: strong agreement
        Wells_TNA_Positive >= 3 ~ "High",
        NegativeReplicates >= 3 ~ "High",
        Wells_DNA_Positive >= 3 & Wells_RNA_Positive == 0 ~ "High",
        Wells_RNA_Positive >= 3 & Wells_DNA_Positive == 0 ~ "High",

        # Medium confidence: minimum consensus
        Wells_TNA_Positive == 2 ~ "Medium",
        Wells_TNA_Suspect >= 2 ~ "Medium",
        Wells_DNA_Positive == 2 | Wells_RNA_Positive == 2 ~ "Medium",

        # Low confidence: weak or conflicting signals
        Wells_TNA_Positive == 1 ~ "Low",
        Wells_DNA_Positive == 1 | Wells_RNA_Positive == 1 ~ "Low",
        Wells_TNA_Suspect == 1 ~ "Low",
        QC_Pass_Count <= 2 ~ "Low",

        TRUE ~ "Low"
      ),

      # Detect conflicts between well-level and aggregate-level calls
      WellAggregateConflict = case_when(
        ControlType %in% c("PC", "NC") ~ FALSE,

        # Well-level says TNA positive but aggregate doesn't
        Wells_TNA_Positive >= min_tna_reps & !Sample_TNA_Positive ~ TRUE,

        # Well-level says DNA/RNA positive but aggregate doesn't
        Wells_DNA_Positive >= min_tna_reps & !Sample_DNA_Positive ~ TRUE,
        Wells_RNA_Positive >= min_tna_reps & !Sample_RNA_Positive ~ TRUE,

        # Aggregate says positive but well-level doesn't
        Sample_TNA_Positive & Wells_TNA_Positive < min_tna_reps ~ TRUE,
        Sample_DNA_Positive & Wells_DNA_Positive < min_tna_reps ~ TRUE,
        Sample_RNA_Positive & Wells_RNA_Positive < min_tna_reps ~ TRUE,

        TRUE ~ FALSE
      ),

      # Detailed conflict explanation
      ConflictDetails = case_when(
        !WellAggregateConflict ~ NA_character_,
        Wells_TNA_Positive >= min_tna_reps & !Sample_TNA_Positive ~
          "Well-level TNA+ but aggregate Cqs don't confirm TNA+",
        Wells_DNA_Positive >= min_tna_reps & !Sample_DNA_Positive ~
          "Well-level DNA+ but aggregate Cq suggests negative/late",
        Wells_RNA_Positive >= min_tna_reps & !Sample_RNA_Positive ~
          "Well-level RNA+ but aggregate Cq suggests negative/late",
        Sample_TNA_Positive & Wells_TNA_Positive < min_tna_reps ~
          "Aggregate TNA+ but well-level doesn't meet threshold",
        Sample_DNA_Positive & Wells_DNA_Positive < min_tna_reps ~
          "Aggregate DNA+ but well-level doesn't meet threshold",
        Sample_RNA_Positive & Wells_RNA_Positive < min_tna_reps ~
          "Aggregate RNA+ but well-level doesn't meet threshold",
        TRUE ~ NA_character_
      ),
      Flags = {
        flags <- character()
        if (Flag_SampleDecay) flags <- c(flags, "SampleDecay")
        if (HostFailed) flags <- c(flags, "NoDNA")
        if (is.na(Delta_18S2_177T) && PositiveTryp) flags <- c(flags, "IncompleteTargets")
        if (WellAggregateConflict) flags <- c(flags, "WellAggregateConflict")
        if (ConfidenceScore == "Low" && !ControlType %in% c("PC", "NC")) flags <- c(flags, "LowConfidence")
        if (!is.na(PipelineQualityFlagClean)) flags <- c(flags, PipelineQualityFlagClean)
        if (!is.na(PipelineDecisionFlag)) flags <- c(flags, PipelineDecisionFlag)
        flags <- unique(flags)
        if (length(flags)) paste(flags, collapse = ";") else NA_character_
      },
      AnyFlag = !is.na(Flags)
    ) %>%
    ungroup() %>%
    mutate(
      across(starts_with("Sample_"), ~replace_na(.x, FALSE)),
      across(starts_with("Wells_"), ~replace_na(.x, 0))
    ) %>%
    select(
      -PositiveTryp,
      -LateTryp,
      -HostOK,
      -HostFailed,
      -Flag_SampleDecay,
      -PipelineCategoryLower,
      -PipelineQualityFlagClean,
      -PipelineDecisionFlag,
      -QC_Pass_Count
    )

  samples_wide <- samples_wide %>%
    relocate(
      # Decision tree outputs - most important columns first
      FinalCall, DecisionStep, DecisionReason,
      ConfidenceScore, WellSummary,
      WellAggregateConflict, ConflictDetails,
      .after = AnyFlag
    ) %>%
    relocate(
      # Pipeline data
      PipelineCategory, PipelineDecision, PipelineQualityFlag,
      ReplicatesTotal, Replicates_Positive, Replicates_Negative,
      Replicates_Failed, Replicates_Inconclusive,
      Replicates_ControlOK, Replicates_ControlFail,
      RNA_Quality, DNA_Quality, RNA_Preservation_Status,
      RNA_Preservation_Delta, Avg_177T_Positive_Cq,
      Avg_18S2_Positive_Cq,
      .after = ConflictDetails
    ) %>%
    relocate(
      # Well-level and Sample-level detection details
      Sample_DNA_Positive, Sample_DNA_Suspect,
      Sample_RNA_Positive, Sample_RNA_Suspect,
      Sample_TNA_Positive, Sample_TNA_Suspect,
      Wells_DNA_Positive, Wells_DNA_Suspect,
      Wells_RNA_Positive, Wells_RNA_Suspect,
      Wells_TNA_Positive, Wells_TNA_Suspect,
      .after = Avg_18S2_Positive_Cq
    )

  samples_wide
}

# =============================================================================
# DECISION TREE VISUALIZATION
# =============================================================================

#' Visualize the decision tree path for a sample
#'
#' @param sample_data A single row from the aggregated samples data
#' @param settings The qPCR settings used for classification
#' @return A character string with the decision tree visualization
#' @export
visualize_decision_path <- function(sample_data, settings = NULL) {
  if (nrow(sample_data) == 0) {
    return("No sample data provided")
  }

  # Extract key values
  sample_name <- sample_data$SampleName
  final_call <- sample_data$FinalCall
  decision_step <- sample_data$DecisionStep
  decision_reason <- sample_data$DecisionReason
  confidence <- sample_data$ConfidenceScore
  well_summary <- sample_data$WellSummary
  conflict <- sample_data$WellAggregateConflict
  conflict_details <- sample_data$ConflictDetails

  # Build the decision path visualization
  output <- c(
    "═══════════════════════════════════════════════════════════════",
    sprintf("SAMPLE: %s", sample_name),
    "═══════════════════════════════════════════════════════════════",
    "",
    "DECISION TREE PATH:",
    "───────────────────────────────────────────────────────────────"
  )

  # Add step-by-step evaluation
  steps <- list(
    "Step0" = "Controls",
    "Step1" = "QC Validity Check",
    "Step2" = "TNA Positive (>=2 wells)",
    "Step3" = "Single TNA Well",
    "Step4a" = "DNA-only Pattern",
    "Step4b" = "RNA-only Pattern",
    "Step5" = "Late Positive TNA",
    "Step6a" = "Single Outlier + Negatives",
    "Step6b" = "Weak Mixed Signals",
    "Step7" = "Clear Negative (>=3 reps)",
    "Step8" = "Insufficient Data"
  )

  for (step_id in names(steps)) {
    step_name <- steps[[step_id]]
    if (step_id == decision_step) {
      output <- c(output, sprintf("  ✓ %s: %s → MATCHED", step_id, step_name))
    } else {
      output <- c(output, sprintf("  ○ %s: %s", step_id, step_name))
    }
  }

  output <- c(
    output,
    "",
    "RESULT:",
    "───────────────────────────────────────────────────────────────",
    sprintf("  Final Call:   %s", final_call),
    sprintf("  Confidence:   %s", confidence),
    sprintf("  Reason:       %s", decision_reason),
    "",
    "WELL-LEVEL SUMMARY:",
    "───────────────────────────────────────────────────────────────",
    sprintf("  %s", well_summary)
  )

  # Add sample-level calls
  if (!is.null(sample_data$Sample_TNA_Positive)) {
    output <- c(
      output,
      "",
      "AGGREGATE VS WELL-LEVEL:",
      "───────────────────────────────────────────────────────────────",
      sprintf("  Sample TNA+:  %s  |  Wells TNA+:  %d",
              ifelse(sample_data$Sample_TNA_Positive, "YES", "NO"),
              sample_data$Wells_TNA_Positive),
      sprintf("  Sample DNA+:  %s  |  Wells DNA+:  %d",
              ifelse(sample_data$Sample_DNA_Positive, "YES", "NO"),
              sample_data$Wells_DNA_Positive),
      sprintf("  Sample RNA+:  %s  |  Wells RNA+:  %d",
              ifelse(sample_data$Sample_RNA_Positive, "YES", "NO"),
              sample_data$Wells_RNA_Positive)
    )
  }

  # Add conflict warning if present
  if (!is.na(conflict) && conflict) {
    output <- c(
      output,
      "",
      "⚠ CONFLICT WARNING:",
      "───────────────────────────────────────────────────────────────",
      sprintf("  %s", conflict_details)
    )
  }

  # Add Cq values if available
  if (!is.null(sample_data$Cq_mean_177T)) {
    output <- c(
      output,
      "",
      "Cq VALUES (Mean):",
      "───────────────────────────────────────────────────────────────",
      sprintf("  177T (DNA):      %.2f  →  %s",
              ifelse(is.na(sample_data$Cq_mean_177T), NA, sample_data$Cq_mean_177T),
              sample_data$Call_177T),
      sprintf("  18S2 (RNA):      %.2f  →  %s",
              ifelse(is.na(sample_data$Cq_mean_18S2), NA, sample_data$Cq_mean_18S2),
              sample_data$Call_18S2),
      sprintf("  RNAseP-DNA:      %.2f  →  %s",
              ifelse(is.na(sample_data$Cq_mean_RNAseP_DNA), NA, sample_data$Cq_mean_RNAseP_DNA),
              sample_data$Call_RNAseP_DNA),
      sprintf("  Delta (RNA-DNA): %.2f",
              ifelse(is.na(sample_data$Delta_18S2_177T), NA, sample_data$Delta_18S2_177T))
    )
  }

  output <- c(output, "═══════════════════════════════════════════════════════════════")

  # Return as single string with newlines
  paste(output, collapse = "\n")
}

#' Generate a summary report for multiple samples
#'
#' @param samples_data Aggregated samples data
#' @param filter_by Optional filter (e.g., "Indeterminate", "LowConfidence")
#' @return A character string with the summary report
#' @export
summarize_decision_quality <- function(samples_data, filter_by = NULL) {
  if (nrow(samples_data) == 0) {
    return("No samples to summarize")
  }

  # Apply filter if specified
  if (!is.null(filter_by)) {
    if (filter_by == "Conflicts") {
      samples_data <- samples_data %>% filter(WellAggregateConflict == TRUE)
    } else if (filter_by == "LowConfidence") {
      samples_data <- samples_data %>% filter(ConfidenceScore == "Low")
    } else if (filter_by == "Indeterminate") {
      samples_data <- samples_data %>% filter(FinalCall == "Indeterminate")
    } else {
      samples_data <- samples_data %>% filter(FinalCall == filter_by)
    }
  }

  if (nrow(samples_data) == 0) {
    return(sprintf("No samples found matching filter: %s", filter_by))
  }

  # Calculate summary statistics
  total <- nrow(samples_data)

  call_counts <- samples_data %>%
    group_by(FinalCall) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(desc(n))

  confidence_counts <- samples_data %>%
    filter(!ControlType %in% c("PC", "NC")) %>%
    group_by(ConfidenceScore) %>%
    summarise(n = n(), .groups = "drop")

  conflict_count <- sum(samples_data$WellAggregateConflict == TRUE, na.rm = TRUE)

  step_counts <- samples_data %>%
    filter(!ControlType %in% c("PC", "NC")) %>%
    group_by(DecisionStep) %>%
    summarise(n = n(), .groups = "drop") %>%
    arrange(DecisionStep)

  # Build report
  output <- c(
    "═══════════════════════════════════════════════════════════════",
    "DECISION TREE QUALITY REPORT",
    "═══════════════════════════════════════════════════════════════",
    sprintf("Total Samples: %d", total),
    ""
  )

  if (!is.null(filter_by)) {
    output <- c(output, sprintf("Filter Applied: %s", filter_by), "")
  }

  output <- c(
    output,
    "FINAL CALL DISTRIBUTION:",
    "───────────────────────────────────────────────────────────────"
  )

  for (i in 1:nrow(call_counts)) {
    pct <- round(100 * call_counts$n[i] / total, 1)
    output <- c(output, sprintf("  %-20s: %4d  (%5.1f%%)",
                                call_counts$FinalCall[i],
                                call_counts$n[i],
                                pct))
  }

  output <- c(
    output,
    "",
    "CONFIDENCE DISTRIBUTION:",
    "───────────────────────────────────────────────────────────────"
  )

  for (i in 1:nrow(confidence_counts)) {
    output <- c(output, sprintf("  %-20s: %4d",
                                confidence_counts$ConfidenceScore[i],
                                confidence_counts$n[i]))
  }

  output <- c(
    output,
    "",
    "DECISION STEP USAGE:",
    "───────────────────────────────────────────────────────────────"
  )

  for (i in 1:nrow(step_counts)) {
    output <- c(output, sprintf("  %-10s: %4d",
                                step_counts$DecisionStep[i],
                                step_counts$n[i]))
  }

  output <- c(
    output,
    "",
    sprintf("Well/Aggregate Conflicts: %d", conflict_count),
    "═══════════════════════════════════════════════════════════════"
  )

  paste(output, collapse = "\n")
}

classify_target <- function(cq, target_name, settings) {
  # Vectorized version that handles NA values properly
  if (length(cq) == 0) return(character(0))
  
  # Get thresholds for this target
  thresholds <- settings$thresholds[[target_name]]
  if (is.null(thresholds)) {
    return(rep("Undetermined", length(cq)))
  }
  
  pos_cutoff <- as.numeric(thresholds$positive)
  neg_cutoff <- as.numeric(thresholds$negative)
  late_window <- settings$late_window
  
  # Vectorized classification
  result <- rep("Undetermined", length(cq))
  
  # Handle NA values
  valid <- !is.na(cq)
  
  # Classify valid values
  if (any(valid)) {
    result[valid & cq <= pos_cutoff] <- "Positive"
    result[valid & cq > neg_cutoff] <- "Negative"
    
    # Late positive window
    if (!is.null(late_window) && length(late_window) == 2) {
      late_pos <- valid & cq > late_window[1] & cq <= late_window[2]
      result[late_pos] <- "LatePositive"
    }
    
    # Everything else (between positive and negative) is Negative
    middle <- valid & cq > pos_cutoff & cq <= neg_cutoff
    result[middle] <- "Negative"
  }
  
  result
}

# Simpler classify_target using map
classify_target_vectorized <- function(cq_vector, target_name, settings) {
  map_chr(cq_vector, function(cq) {
    if (is.na(cq)) return("Undetermined")
    
    thresholds <- settings$thresholds[[target_name]]
    if (is.null(thresholds)) return("Undetermined")
    
    pos_cutoff <- as.numeric(thresholds$positive)
    neg_cutoff <- as.numeric(thresholds$negative)
    late_window <- settings$late_window
    
    if (cq <= pos_cutoff) return("Positive")
    if (cq > neg_cutoff) return("Negative")
    if (!is.null(late_window) && length(late_window) == 2 && 
        cq > late_window[1] && cq <= late_window[2]) {
      return("LatePositive")
    }
    "Negative"
  })
}

# =============================================================================
# DIRECTORY PARSING WITH CACHING
# =============================================================================

parse_mic_directory <- function(path, settings, cache_state) {
  files <- scan_mic_directory(path)

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

  # Separate files into cached and non-cached
  files_with_cache_status <- files %>%
    mutate(is_cached = map_lgl(hash, ~!is.null(cache[[.x]])))

  cached_files <- files_with_cache_status %>% filter(is_cached)
  uncached_files <- files_with_cache_status %>% filter(!is_cached)

  # Get cached results
  cached_results <- map(cached_files$hash, ~cache[[.x]])

  # Parse uncached files (use parallel only for larger batches to avoid overhead)
  uncached_results <- list()
  if (nrow(uncached_files) > 0) {
    # Only use parallel processing for 5+ files to avoid fork overhead
    if (nrow(uncached_files) >= 5 && .Platform$OS.type == "unix") {
      # Use mclapply on Unix systems for parallel processing
      # Determine number of cores to use (max 4 or half of available cores)
      n_cores <- min(4, max(1, parallel::detectCores() %/% 2))

      uncached_results <- parallel::mclapply(
        seq_len(nrow(uncached_files)),
        function(i) {
          file_row <- uncached_files[i, ]
          parse_single_mic_file(file_row, settings)
        },
        mc.cores = n_cores
      )
    } else {
      # Sequential processing for small batches, Windows, or single file
      uncached_results <- lapply(
        seq_len(nrow(uncached_files)),
        function(i) {
          file_row <- uncached_files[i, ]
          parse_single_mic_file(file_row, settings)
        }
      )
    }
  }

  # Combine all results
  all_results <- c(cached_results, uncached_results)
  all_hashes <- c(cached_files$hash, uncached_files$hash)

  # Filter successful results and build output lists
  all_runs <- list()
  all_samples <- list()
  all_replicates <- list()
  new_cache <- list()

  for (i in seq_along(all_results)) {
    parsed <- all_results[[i]]
    hash <- all_hashes[i]

    if (!parsed$success) next

    new_cache[[hash]] <- parsed
    all_runs[[length(all_runs) + 1]] <- parsed$run
    all_samples[[length(all_samples) + 1]] <- parsed$samples
    all_replicates[[length(all_replicates) + 1]] <- parsed$replicates
  }

  runs_df <- if (length(all_runs)) bind_rows(all_runs) else tibble()
  samples_df <- if (length(all_samples)) bind_rows(all_samples) else tibble()
  replicates_df <- if (length(all_replicates)) bind_rows(all_replicates) else tibble()

  list(
    runs = runs_df,
    replicates = replicates_df,
    samples = samples_df,
    cache = new_cache,
    files = files
  )
}

# =============================================================================
# CONTROL QC & RUN VALIDATION
# =============================================================================

validate_controls <- function(samples_df, settings) {
  if (!nrow(samples_df)) return(tibble())
  
  controls <- samples_df %>%
    filter(ControlType %in% c("PC", "NC"))
  
  if (!nrow(controls)) return(tibble())
  
  thresholds <- settings$thresholds
  
  # Extract threshold values safely
  th_177t_pos <- as.numeric(thresholds$`177T`$positive)
  th_177t_neg <- as.numeric(thresholds$`177T`$negative)
  th_18s2_pos <- as.numeric(thresholds$`18S2`$positive)
  th_18s2_neg <- as.numeric(thresholds$`18S2`$negative)
  
  controls %>%
    mutate(
      # For PC: both targets should be positive (Cq <= pos cutoff) OR NA
      PC_177T_ok = if_else(is.na(Cq_median_177T), TRUE, Cq_median_177T <= th_177t_pos),
      PC_18S2_ok = if_else(is.na(Cq_median_18S2), TRUE, Cq_median_18S2 <= th_18s2_pos),
      
      # For NC: both targets should be negative (Cq > neg cutoff) OR NA
      NC_177T_ok = if_else(is.na(Cq_median_177T), TRUE, Cq_median_177T > th_177t_neg),
      NC_18S2_ok = if_else(is.na(Cq_median_18S2), TRUE, Cq_median_18S2 > th_18s2_neg),
      
      # Determine pass/fail based on control type
      ControlPass = case_when(
        ControlType == "PC" ~ (PC_177T_ok & PC_18S2_ok),
        ControlType == "NC" ~ (NC_177T_ok & NC_18S2_ok),
        TRUE ~ TRUE
      ),
      
      ControlFlag = case_when(
        !ControlPass & ControlType == "PC" ~ "FailedPositiveControl",
        !ControlPass & ControlType == "NC" ~ "FailedNegativeControl",
        TRUE ~ NA_character_
      )
    ) %>%
    select(RunID, SampleID, SampleName, ControlType, ControlPass, ControlFlag)
}

create_run_summary <- function(samples_df, runs_df, control_status) {
  if (!nrow(samples_df)) return(tibble())
  
  sample_counts <- samples_df %>%
    group_by(RunID) %>%
    summarise(
      TotalSamples = sum(ControlType == "Sample"),
      TotalControls = sum(ControlType %in% c("PC", "NC")),
      Positives = sum(FinalCall == "Positive" & ControlType == "Sample", na.rm = TRUE),
      Negatives = sum(FinalCall == "Negative" & ControlType == "Sample", na.rm = TRUE),
      LatePositives = sum(FinalCall == "LatePositive" & ControlType == "Sample", na.rm = TRUE),
      InvalidNoDNA = sum(FinalCall == "Invalid_NoDNA" & ControlType == "Sample", na.rm = TRUE),
      Indeterminate = sum(FinalCall == "Indeterminate" & ControlType == "Sample", na.rm = TRUE),
      Flagged = sum(AnyFlag & ControlType == "Sample", na.rm = TRUE),
      .groups = "drop"
    )
  
  control_summary <- control_status %>%
    group_by(RunID) %>%
    summarise(
      ControlRecords = n(),
      ControlsPassing = all(ControlPass, na.rm = TRUE),
      ControlsPassed = sum(ControlPass, na.rm = TRUE),
      ControlsFailed = sum(!ControlPass, na.rm = TRUE),
      ControlPassRate = if_else(ControlRecords > 0,
                                ControlsPassed / ControlRecords,
                                NA_real_),
      FailedControls = paste(unique(na.omit(ControlFlag)), collapse = "; "),
      .groups = "drop"
    )

  runs_df %>%
    left_join(sample_counts, by = "RunID") %>%
    left_join(control_summary, by = "RunID") %>%
    mutate(
      RunValid = if_else(is.na(ControlsPassing), TRUE, ControlsPassing),
      TotalSamples = replace_na(TotalSamples, 0),
      Positives = replace_na(Positives, 0),
      Negatives = replace_na(Negatives, 0),
      LatePositives = replace_na(LatePositives, 0),
      InvalidNoDNA = replace_na(InvalidNoDNA, 0),
      Indeterminate = replace_na(Indeterminate, 0),
      Flagged = replace_na(Flagged, 0),
      ControlsPassed = replace_na(ControlsPassed, 0),
      ControlsFailed = replace_na(ControlsFailed, 0),
      ControlPassRate = if_else(is.nan(ControlPassRate), NA_real_, ControlPassRate),
      FailedControls = na_if(FailedControls, ""),
      ControlRecords = replace_na(ControlRecords, 0)
    )
}

# =============================================================================
# BIOBANK & EXTRACTION LINKING
# =============================================================================

# Replace link_to_biobank with this corrected version:

link_to_biobank <- function(samples_df, biobank_df) {
  if (!nrow(samples_df) || is.null(biobank_df) || !nrow(biobank_df)) {
    message("⚠️ No biobank data available for linking")
    return(samples_df %>%
             mutate(
               BiobankMatched = FALSE,
               Province = NA_character_,
               HealthZone = NA_character_,
               Structure = NA_character_,
               Cohort = NA_character_,
               SampleDate = as.Date(NA)
             ))
  }

  # DEBUG: Print what columns we actually have
  message("📊 Biobank data columns: ", paste(names(biobank_df), collapse = ", "))
  message("📊 Biobank data has ", nrow(biobank_df), " rows")

  fetch_column <- function(data, candidates, transform = identity, default = NA_character_) {
    matched <- match_column_name(names(data), candidates)

    if (!is.null(matched)) {
      return(transform(data[[matched]]))
    }

    rep(default, nrow(data))
  }

  # Find the ID column (use lab_id or barcode from your actual data)
  id_candidates <- c(
    "lab_id", "barcode", "numero", "Numero", "Sample", "sample_id",
    "code_barres_kps", "code-barres kps", "codebarres_kps"
  )
  id_col <- match_column_name(names(biobank_df), id_candidates)

  if (!is.null(id_col)) {
    message("✓ Found ID column: ", id_col)
  }

  if (is.null(id_col)) {
    warning("❌ No ID column found in biobank data")
    return(samples_df %>%
             mutate(
               BiobankMatched = FALSE,
               Province = NA_character_,
               HealthZone = NA_character_,
               Structure = NA_character_,
               Cohort = NA_character_,
               SampleDate = as.Date(NA)
             ))
  }

  # Build lookup using your actual column names
  bb_lookup <- tibble(
    id_norm = normalize_id(biobank_df[[id_col]]),
    Province = fetch_column(biobank_df, c("province", "Province", "biobank_province")),
    HealthZone = fetch_column(biobank_df, c("health_zone", "zone_de_sante", "Zone de sante", "biobank_health_zone")),
    Structure = fetch_column(
      biobank_df,
      c(
        "health_facility", "health_structure", "structure_sanitaire",
        "structure sanitaire", "biobank_health_facility", "biobank_structure_sanitaire"
      )
    ),
    Cohort = fetch_column(biobank_df, c("study", "Study", "cohort", "biobank_study", "etude")),
    SampleDate = fetch_column(
      biobank_df,
      c(
        "date_sample", "sample_date", "Date", "collection_date",
        "date_de_prelevement", "date_prelevement"
      ),
      transform = function(x) suppressWarnings(as.Date(x)),
      default = as.Date(NA)
    )
  ) %>%
    filter(!is.na(id_norm), id_norm != "") %>%
    distinct(id_norm, .keep_all = TRUE)

  message("📊 Created biobank lookup with ", nrow(bb_lookup), " unique samples")
  if (nrow(bb_lookup) > 0) {
    message("   Example biobank IDs: ", paste(head(bb_lookup$id_norm, 5), collapse = ", "))
  }
  
  # Get MIC sample IDs for comparison
  mic_ids <- normalize_id(samples_df$SampleID) %>% unique() %>% .[!is.na(.) & . != ""]
  message("📊 MIC has ", length(mic_ids), " unique sample IDs")
  if (length(mic_ids) > 0) {
    message("   Example MIC IDs: ", paste(head(mic_ids, 5), collapse = ", "))
  }
  
  # Join MIC SampleID to biobank ID
  result <- samples_df %>%
    mutate(SampleID_norm = normalize_id(SampleID)) %>%
    left_join(bb_lookup, by = c("SampleID_norm" = "id_norm")) %>%
    mutate(
      BiobankMatched = dplyr::if_else(
        dplyr::coalesce(!is.na(Province), FALSE) |
          dplyr::coalesce(!is.na(Structure), FALSE) |
          dplyr::coalesce(!is.na(Cohort), FALSE) |
          dplyr::coalesce(!is.na(HealthZone), FALSE),
        TRUE,
        FALSE,
        missing = FALSE
      )
    ) %>%
    select(-SampleID_norm)

  n_matched <- sum(result$BiobankMatched, na.rm = TRUE)
  message("✓ Matched ", n_matched, " MIC samples to biobank (",
          round(100 * n_matched / nrow(samples_df)), "%)")
  
  result
}

link_to_extractions <- function(samples_df, extractions_df) {
  if (!nrow(samples_df) || is.null(extractions_df) || !nrow(extractions_df)) {
    message("⚠️ No extractions data available for linking")
    return(samples_df %>% mutate(ExtractionMatched = FALSE))
  }
  
  # DEBUG: Print what columns we actually have
  message("📊 Extractions data columns: ", paste(names(extractions_df), collapse = ", "))
  message("📊 Extractions data has ", nrow(extractions_df), " rows")
  
  # Try to find ANY column that might contain sample identifiers
  possible_id_cols <- c(
    "SampleID", "Sample", "numero", "sample_id", "Numero", 
    "sample", "SAMPLEID", "ID", "id", "Name", "name",
    "code_barres_kps", "barcode"  # Also try barcode matching
  )
  
  extraction_col <- NULL
  for (col in possible_id_cols) {
    if (col %in% names(extractions_df)) {
      extraction_col <- col
      message("✓ Found extraction ID column: ", col)
      break
    }
  }
  
  if (is.null(extraction_col)) {
    warning("❌ No matching ID column found in extractions data. Available columns: ", 
            paste(names(extractions_df), collapse = ", "))
    return(samples_df %>% mutate(ExtractionMatched = FALSE))
  }
  
  # Get extraction IDs and normalize
  ex_ids <- extractions_df[[extraction_col]] %>%
    as.character() %>%
    normalize_id() %>%
    unique() %>%
    .[!is.na(.) & . != ""]
  
  message("📊 Found ", length(ex_ids), " unique extraction IDs")
  if (length(ex_ids) > 0) {
    message("   Examples: ", paste(head(ex_ids, 3), collapse = ", "))
  }
  
  # Match MIC SampleID to extraction IDs
  samples_df %>%
    mutate(
      SampleID_norm = normalize_id(SampleID),
      ExtractionMatched = SampleID_norm %in% ex_ids
    ) %>%
    select(-SampleID_norm)
}

# =============================================================================
# LEVEY-JENNINGS CALCULATION
# =============================================================================

compute_levey_jennings <- function(replicates_long, target, control_type = "PC") {
  if (!nrow(replicates_long)) {
    return(list(
      data = tibble(),
      summary = tibble(
        Target = target,
        ControlType = control_type,
        Mean = NA_real_,
        SD = NA_real_,
        N = 0,
        FallbackUsed = FALSE,
        Within2SD_pct = NA_real_,
        LatestRunID = NA_character_,
        LatestCq = NA_real_
      ),
      fallback = FALSE,
      control_type_used = control_type
    ))
  }

  df <- replicates_long %>%
    filter(Target == target, !is.na(Cq))

  if (!"ControlType" %in% names(df)) {
    df <- df %>% mutate(ControlType = NA_character_)
  }

  has_controls <- function(x) {
    any(x %in% c("PC", "NC"), na.rm = TRUE)
  }

  if (!has_controls(df$ControlType) && "Type" %in% names(df)) {
    df <- df %>%
      mutate(
        ControlType = case_when(
          ControlType %in% c("PC", "NC") ~ ControlType,
          Type %in% c("Positive", "Positive Control", "Standard", "CP", "PC", "POS") ~ "PC",
          Type %in% c("Negative", "Negative Control", "NC", "NTC", "NEG") ~ "NC",
          TRUE ~ ControlType
        )
      )
  }

  if (!has_controls(df$ControlType)) {
    df <- df %>%
      mutate(
        .name_upper = toupper(ifelse(is.na(SampleName), "", as.character(SampleName))),
        .id_upper = toupper(ifelse(is.na(SampleID), "", as.character(SampleID))),
        ControlType = case_when(
          ControlType %in% c("PC", "NC") ~ ControlType,
          stringr::str_detect(.name_upper, "(?<![A-Z0-9])(PC|POS|POSITIVE)(?![A-Z0-9])") ~ "PC",
          stringr::str_detect(.id_upper, "(?<![A-Z0-9])(PC|POS|POSITIVE)(?![A-Z0-9])") ~ "PC",
          stringr::str_detect(.name_upper, "(?<![A-Z0-9])(NC|NEG|NTC|NEGATIVE)(?![A-Z0-9])") ~ "NC",
          stringr::str_detect(.id_upper, "(?<![A-Z0-9])(NC|NEG|NTC|NEGATIVE)(?![A-Z0-9])") ~ "NC",
          TRUE ~ ControlType
        )
      ) %>%
      select(-any_of(c(".name_upper", ".id_upper")))
  }

  df <- df %>% mutate(ControlType = if_else(ControlType == "", NA_character_, ControlType))

  ctrl_df <- df %>% filter(ControlType == control_type)
  fallback <- FALSE
  control_type_used <- control_type

  if (!nrow(ctrl_df)) {
    ctrl_df <- df
    fallback <- TRUE
    control_type_used <- "All"
  }

  if (!nrow(ctrl_df)) {
    return(list(
      data = tibble(),
      summary = tibble(
        Target = target,
        ControlType = control_type_used,
        Mean = NA_real_,
        SD = NA_real_,
        N = 0,
        FallbackUsed = fallback,
        Within2SD_pct = NA_real_,
        LatestRunID = NA_character_,
        LatestCq = NA_real_
      ),
      fallback = fallback,
      control_type_used = control_type_used
    ))
  }

  run_stats <- ctrl_df %>%
    group_by(RunID) %>%
    summarise(Cq_mean = mean(Cq, na.rm = TRUE), .groups = "drop") %>%
    arrange(RunID)

  mu <- mean(ctrl_df$Cq, na.rm = TRUE)
  sdv <- sd(ctrl_df$Cq, na.rm = TRUE)

  plot_data <- run_stats %>%
    mutate(
      Mean = mu,
      SD = sdv,
      plus1 = mu + sdv,
      minus1 = mu - sdv,
      plus2 = mu + 2 * sdv,
      minus2 = mu - 2 * sdv,
      plus3 = mu + 3 * sdv,
      minus3 = mu - 3 * sdv
    )

  within_limits <- if (nrow(plot_data)) {
    valid <- !is.na(plot_data$Cq_mean)
    total <- sum(valid)
    if (!total) NA_real_ else round(100 * sum(plot_data$Cq_mean[valid] >= plot_data$minus2[valid] &
                                              plot_data$Cq_mean[valid] <= plot_data$plus2[valid]) / total, 1)
  } else {
    NA_real_
  }

  latest_cq <- if (nrow(plot_data) && any(!is.na(plot_data$Cq_mean))) {
    tail(na.omit(plot_data$Cq_mean), 1)
  } else {
    NA_real_
  }

  latest_run <- if (nrow(plot_data) && any(!is.na(plot_data$Cq_mean))) {
    tail(plot_data$RunID[!is.na(plot_data$Cq_mean)], 1)
  } else {
    NA_character_
  }

  list(
    data = plot_data,
    summary = tibble(
      Target = target,
      ControlType = control_type_used,
      Mean = mu,
      SD = sdv,
      N = nrow(ctrl_df),
      FallbackUsed = fallback,
      Within2SD_pct = within_limits,
      LatestRunID = latest_run,
      LatestCq = latest_cq
    ),
    fallback = fallback,
    control_type_used = control_type_used
  )
}

# =============================================================================
# GLOBAL FILTER APPLICATION
# =============================================================================

apply_global_filters <- function(df, filters) {
  if (is.null(filters) || !is.list(filters) || !nrow(df)) return(df)
  
  # Date range filter
  if (!is.null(filters$date_range) && length(filters$date_range) == 2 && "SampleDate" %in% names(df)) {
    date_range <- tryCatch(
      suppressWarnings(as.Date(filters$date_range)),
      error = function(e) rep(NA, 2)
    )
    if (!any(is.na(date_range))) {
      df <- df %>%
        mutate(SampleDate = suppressWarnings(as.Date(SampleDate))) %>%
        filter(is.na(SampleDate) | (SampleDate >= date_range[1] & SampleDate <= date_range[2]))
    }
  }
  
  # Province filter
  if (!is.null(filters$province) && !identical(filters$province, "all") && "Province" %in% names(df)) {
    df <- df %>% filter(is.na(Province) | Province %in% filters$province)
  }

  # Health zone filter
  if (!is.null(filters$zone) && !identical(filters$zone, "all") && "HealthZone" %in% names(df)) {
    df <- df %>% filter(is.na(HealthZone) | HealthZone %in% filters$zone)
  }

  # Structure filter (normalized to match sidebar dropdown keys)
  if (!is.null(filters$structure) && !identical(filters$structure, "all") && "Structure" %in% names(df)) {
    target <- normalize_structure_value(filters$structure)

    if (!all(is.na(target))) {
      df <- df %>%
        mutate(`__Structure_norm` = normalize_structure_value(Structure)) %>%
        filter(is.na(`__Structure_norm`) | `__Structure_norm` %in% target) %>%
        select(-`__Structure_norm`)
    }
  }

  # Cohort filter
  if (!is.null(filters$cohort) && !identical(filters$cohort, "all") && "Cohort" %in% names(df)) {
    df <- df %>% filter(is.na(Cohort) | Cohort %in% filters$cohort)
  }
  
  df
}

# =============================================================================
# REDESIGNED UI - RETURNS LIST OF 5 SEPARATE NAV_PANELS
# =============================================================================

mod_mic_qpcr_ui <- function(id) {
  ns <- NS(id)

  # ===========================================================================
  # SHARED ACTION BAR (appears on all panels)
  # ===========================================================================
  action_bar <- card(
    class = "mb-3",
    card_body(
      class = "py-2",
      layout_columns(
        col_widths = c(8, 4),
        div(
          textInput(
            ns("mic_dir"),
            NULL,
            value = "data/MIC",
            placeholder = "Path to MIC Excel files",
            width = "100%"
          )
        ),
        div(
          class = "d-flex gap-2 align-items-end",
          actionButton(
            ns("refresh"),
            "Refresh Data",
            icon = icon("sync"),
            class = "btn-primary",
            style = "width: 140px;"
          ),
          actionButton(
            ns("settings"),
            "Settings",
            icon = icon("sliders"),
            class = "btn-outline-secondary",
            style = "width: 120px;"
          )
        )
      )
    )
  )

  # ===========================================================================
  # RETURN LIST OF NAV_PANELS (NOT A SINGLE NAV_PANEL!)
  # ===========================================================================
  list(

    # =========================================================================
    # PANEL 1: MIC OVERVIEW (KPIs Only)
    # =========================================================================
    nav_panel(
      title = "MIC - Run",
      icon = icon("dna"),

      action_bar,

      # Row 1: Overview metrics
      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",

        value_box(
          title = "Total Runs",
          value = textOutput(ns("kpi_runs")),
          showcase = icon("folder-open"),
          theme = "primary"
        ),

        value_box(
          title = "Total Samples",
          value = textOutput(ns("kpi_samples")),
          showcase = icon("vial"),
          theme = "info"
        ),

        value_box(
          title = "Positives",
          value = textOutput(ns("kpi_positives")),
          showcase = icon("check-circle"),
          theme = "success"
        ),

        value_box(
          title = "Late Positives",
          value = textOutput(ns("kpi_late_positives")),
          showcase = icon("clock"),
          theme = "warning"
        ),

        value_box(
          title = "Indeterminate",
          value = textOutput(ns("kpi_incomplete")),
          showcase = icon("exclamation-triangle"),
          theme = "danger"
        )
      ),

      # Row 2: Data quality metrics
      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",

        value_box(
          title = "Prevalence",
          value = textOutput(ns("kpi_prevalence")),
          showcase = icon("percent"),
          theme = "success"
        ),

        value_box(
          title = "QC Issues",
          value = textOutput(ns("kpi_flagged")),
          showcase = icon("flag"),
          theme = "warning"
        ),

        value_box(
          title = "Biobank Linked",
          value = textOutput(ns("kpi_biobank")),
          showcase = icon("link"),
          theme = "secondary"
        ),

        value_box(
          title = "Extractions Linked",
          value = textOutput(ns("kpi_extractions")),
          showcase = icon("flask"),
          theme = "secondary"
        ),

        value_box(
          title = "Valid Runs",
          value = textOutput(ns("kpi_valid_runs")),
          showcase = icon("check-square"),
          theme = "success"
        )
      ),

      # Row 3: RNA/DNA Quality (separate row as requested)
      layout_column_wrap(
        width = 1/2,
        heights_equal = "row",

        value_box(
          title = "RNA Quality (indicator - needs fix)",
          value = textOutput(ns("kpi_rna_good")),
          showcase = icon("star"),
          theme = "info"
        ),

        value_box(
          title = "DNA Quality (indicator - needs fix)",
          value = textOutput(ns("kpi_dna_good")),
          showcase = icon("star"),
          theme = "info"
        )
      ),

      # Levey-Jennings quality trends (moved from QC panel)
      div(
        class = "mt-4",
        layout_columns(
          col_widths = c(12),
          gap = "16px",
          card(
            card_header("177T Positive Control", class = "bg-light"),
            plotlyOutput(ns("lj_177t"), height = "800px", width = "100%")
          ),
          card(
            card_header("18S2 Positive Control", class = "bg-light"),
            plotlyOutput(ns("lj_18s2"), height = "800px", width = "100%")
          ),
          card(
            card_header("RNAseP-DNA Positive Control", class = "bg-light"),
            plotlyOutput(ns("lj_rnp_dna"), height = "800px", width = "100%")
          ),
          card(
            card_header("RNAseP-RNA Positive Control", class = "bg-light"),
            plotlyOutput(ns("lj_rnp_rna"), height = "800px", width = "100%")
          )
        )
      ),

      # Control status table
      card(
        class = "mt-3",
        card_header("Control Status by Run"),
        card_body(
          DTOutput(ns("tbl_controls")),
          class = "p-3"
        )
      )
    ),

    # =========================================================================
    # PANEL 2: MIC - SAMPLES
    # =========================================================================
    nav_panel(
      title = "MIC - Samples",
      icon = icon("vials"),

      # Note: Filters from sidebar apply automatically, no filters table here

      card(
        full_screen = TRUE,
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Sample Results"),
          downloadButton(ns("dl_samples_filtered"), "Download", class = "btn-sm btn-outline-primary")
        ),
        card_body(
          DTOutput(ns("tbl_samples")),
          class = "p-3"
        )
      )
    ),

    # =========================================================================
    # PANEL 3: MIC - ANALYSIS
    # =========================================================================
    nav_panel(
      title = "MIC - Analysis",
      icon = icon("chart-scatter"),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Trypanozoon: 18S2 vs 177T"),
          card_body(
            plotlyOutput(ns("scatter_tryp"), height = "550px"),
            class = "p-3"
          )
        ),

        card(
          card_header("Sample Quality: RNAseP RNA vs DNA (ΔCq)"),
          card_body(
            plotlyOutput(ns("scatter_rnp"), height = "550px"),
            class = "p-3"
          )
        )
      )
    ),

    # =========================================================================
    # PANEL 4: MIC - EXPORT
    # =========================================================================
    nav_panel(
      title = "MIC - Export",
      icon = icon("download"),

      layout_columns(
        col_widths = c(6, 6),

        card(
          card_header("Core Data Exports"),
          card_body(
            class = "p-4",
            h5("Sample-Level Data", class = "mb-3"),
            downloadButton(ns("dl_samples"), "All Sample Calls", class = "btn-primary w-100 mb-3"),
            downloadButton(ns("dl_positives"), "Positive Samples Only", class = "btn-success w-100 mb-3"),

            tags$hr(),

            h5("Run-Level Data", class = "mb-3"),
            downloadButton(ns("dl_runs"), "Run Metadata", class = "btn-info w-100 mb-3"),
            downloadButton(ns("dl_controls"), "Control Performance", class = "btn-info w-100 mb-3")
          )
        ),

        card(
          card_header("Analysis Exports"),
          card_body(
            class = "p-4",
            h5("Quality Metrics", class = "mb-3"),
            downloadButton(ns("dl_deltas"), "ΔCq Summary", class = "btn-secondary w-100 mb-3"),
            downloadButton(ns("dl_lj"), "Levey-Jennings Stats", class = "btn-secondary w-100 mb-3"),

            tags$hr(),

            h5("Complete Dataset", class = "mb-3"),
            downloadButton(ns("dl_complete"), "Full Export (All Data)", class = "btn-dark w-100 mb-3")
          )
        )
      )
    )
  )
}

# =============================================================================
# NEW SERVER IMPLEMENTATION - PASTE INTO mod_05_mic_qpcr.R starting at line 1135
# =============================================================================

mod_mic_qpcr_server <- function(id, biobank_df, extractions_df, filters) {
  moduleServer(id, function(input, output, session) {

    # =========================================================================
    # REACTIVE VALUES & SETTINGS
    # =========================================================================

    cache_state <- reactiveVal(list())

    # Settings inputs - initialize with defaults in case modal hasn't been opened yet
    observeEvent(session$userData, once = TRUE, {
      # Initialize threshold values if they don't exist
      defaults <- list(
        th_177t_pos = 35, th_177t_neg = 40,
        th_18s2_pos = 35, th_18s2_neg = 40,
        th_rnp_dna_pos = 28, th_rnp_dna_neg = 40,
        th_rnp_rna_pos = 35, th_rnp_rna_neg = 40,
        late_min = 35, late_max = 40,
        delta_rp_limit = 8,
        allow_review = FALSE
      )

      for (name in names(defaults)) {
        if (is.null(input[[name]])) {
          updateNumericInput(session, name, value = defaults[[name]])
        }
      }
    })

    settings <- reactive({
      list(
        thresholds = list(
          `177T` = list(positive = input$th_177t_pos %||% 35, negative = input$th_177t_neg %||% 40),
          `18S2` = list(positive = input$th_18s2_pos %||% 35, negative = input$th_18s2_neg %||% 40),
          RNAseP_DNA = list(positive = input$th_rnp_dna_pos %||% 28, negative = input$th_rnp_dna_neg %||% 40),
          RNAseP_RNA = list(positive = input$th_rnp_rna_pos %||% 35, negative = input$th_rnp_rna_neg %||% 40)
        ),
        late_window = c(input$late_min %||% 35, input$late_max %||% 40),
        delta_rp_limit = input$delta_rp_limit %||% 8,
        allow_review_controls = isTRUE(input$allow_review),
        pc_aliases = c("PC", "POS", "POSITIVE", "CP"),
        nc_aliases = c("NC", "NEG", "NTC", "CN")
      )
    })

    # =========================================================================
    # SETTINGS MODAL
    # =========================================================================

    observeEvent(input$settings, {
      showModal(modalDialog(
        title = "qPCR Settings & Thresholds",
        size = "xl",
        easyClose = TRUE,

        layout_columns(
          col_widths = c(6, 6),

          # Left column: Trypanozoon targets
          card(
            card_header("Trypanozoon Targets (DNA & RNA)", class = "bg-primary text-white"),
            card_body(
              class = "p-3",

              h6("177T (DNA Target)", class = "mt-2 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_177t_pos"),
                  "Positive ≤",
                  value = isolate(input$th_177t_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_177t_neg"),
                  "Negative >",
                  value = isolate(input$th_177t_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              ),

              h6("18S2 (RNA Target)", class = "mt-3 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_18s2_pos"),
                  "Positive ≤",
                  value = isolate(input$th_18s2_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_18s2_neg"),
                  "Negative >",
                  value = isolate(input$th_18s2_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              )
            )
          ),

          # Right column: RNAseP targets
          card(
            card_header("RNAseP Targets (Quality Control)", class = "bg-info text-white"),
            card_body(
              class = "p-3",

              h6("RNAseP-DNA", class = "mt-2 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_rnp_dna_pos"),
                  "Positive ≤",
                  value = isolate(input$th_rnp_dna_pos) %||% 28,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_rnp_dna_neg"),
                  "Negative >",
                  value = isolate(input$th_rnp_dna_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              ),

              h6("RNAseP-RNA", class = "mt-3 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_rnp_rna_pos"),
                  "Positive ≤",
                  value = isolate(input$th_rnp_rna_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_rnp_rna_neg"),
                  "Negative >",
                  value = isolate(input$th_rnp_rna_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              )
            )
          )
        ),

        # QC parameters at bottom
        card(
          card_header("Quality Control Parameters", class = "bg-warning"),
          card_body(
            class = "p-3",
            layout_columns(
              col_widths = c(4, 4, 4),

              div(
                h6("Late Positive Window"),
                p(class = "small text-muted mb-2", "Cq values within this range are flagged as LatePositive instead of Positive/Negative."),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput(
                    session$ns("late_min"),
                    "Min",
                    value = isolate(input$late_min) %||% 35,
                    min = 0,
                    max = 50,
                    step = 0.5,
                    width = "100%"
                  ),
                  numericInput(
                    session$ns("late_max"),
                    "Max",
                    value = isolate(input$late_max) %||% 40,
                    min = 0,
                    max = 50,
                    step = 0.5,
                    width = "100%"
                  )
                )
              ),

              div(
                h6("RNA Preservation"),
                numericInput(
                  session$ns("delta_rp_limit"),
                  "ΔCq Limit",
                  value = isolate(input$delta_rp_limit) %||% 8,
                  min = 0,
                  max = 20,
                  step = 0.5,
                  width = "100%"
                ),
                p(class = "small text-muted mt-1", "Max acceptable difference between RNA and DNA")
              ),

              div(
                h6("Control Handling"),
                checkboxInput(
                  session$ns("allow_review"),
                  "Allow review despite control failures",
                  value = isolate(input$allow_review) %||% FALSE
                ),
                p(class = "small text-muted mt-1", "Process samples even when controls fail")
              )
            )
          )
        ),

        footer = div(
          actionButton(session$ns("apply_settings"), "Apply Settings", class = "btn-primary"),
          modalButton("Cancel", class = "btn-secondary")
        )
      ))
    })

    observeEvent(input$apply_settings, {
      # Clear cached parses so new thresholds are applied immediately
      cache_state(list())
      removeModal()

      # Reprocess MIC files with the updated settings
      raw_data()
      showNotification(
        "Settings applied. MIC data reprocessed with new thresholds.",
        type = "message",
        duration = 5
      )
    })

    # =========================================================================
    # DATA LOADING
    # =========================================================================

    raw_data <- reactive({
      req(input$mic_dir)

      withProgress(message = "Loading MIC files...", value = 0.3, {
        parsed <- parse_mic_directory(input$mic_dir, settings(), cache_state())
        cache_state(parsed$cache)
        parsed
      })
    })

    # Force refresh
    observeEvent(input$refresh, {
      cache_state(list())
      raw_data()
    })

    # =========================================================================
    # PROCESSED DATA
    # =========================================================================

    processed_data <- reactive({
      rd <- raw_data()

      if (!nrow(rd$samples)) {
        return(list(
          runs = tibble(),
          samples = tibble(),
          replicates = tibble(),
          control_status = tibble(),
          lj_stats = list()
        ))
      }

      # Validate controls
      control_status <- validate_controls(rd$samples, settings())

      # Create run summary
      runs_summary <- create_run_summary(rd$samples, rd$runs, control_status)

      # Link to biobank and extractions
      samples_linked <- rd$samples %>%
        link_to_biobank(if (is.null(biobank_df)) NULL else biobank_df()) %>%
        link_to_extractions(if (is.null(extractions_df)) NULL else extractions_df())

      # Apply control flags
      samples_linked <- samples_linked %>%
        left_join(
          control_status %>% select(RunID, SampleID, ControlFlag),
          by = c("RunID", "SampleID")
        ) %>%
        mutate(
          Flags = if_else(
            !is.na(ControlFlag),
            if_else(is.na(Flags), ControlFlag, paste(Flags, ControlFlag, sep = ";")),
            Flags
          ),
          AnyFlag = AnyFlag | !is.na(ControlFlag)
        )

      # Mark invalid runs
      if (!settings()$allow_review_controls) {
        invalid_runs <- runs_summary %>%
          filter(!RunValid) %>%
          pull(RunID)

        if (length(invalid_runs) > 0) {
          samples_linked <- samples_linked %>%
            mutate(
              FinalCall = if_else(RunID %in% invalid_runs, "RunInvalid", FinalCall),
              Flags = if_else(RunID %in% invalid_runs,
                              paste(Flags, "RunInvalid", sep = ";"),
                              Flags),
              AnyFlag = if_else(RunID %in% invalid_runs, TRUE, AnyFlag)
            )
        }
      }

      # Compute Levey-Jennings stats
      lj_stats <- list(
        `177T` = compute_levey_jennings(rd$replicates, "177T"),
        `18S2` = compute_levey_jennings(rd$replicates, "18S2"),
        RNAseP_DNA = compute_levey_jennings(rd$replicates, "RNAseP_DNA"),
        RNAseP_RNA = compute_levey_jennings(rd$replicates, "RNAseP_RNA")
      )

      list(
        runs = runs_summary,
        samples = samples_linked,
        replicates = rd$replicates,
        control_status = control_status,
        lj_stats = lj_stats
      )
    })

    # =========================================================================
    # FILTERED SAMPLES (with UI filters)
    # =========================================================================

    filtered_samples <- reactive({
      pd <- processed_data()
      df <- pd$samples

      if (!nrow(df)) return(df)

      # Apply global sidebar filters first
      df <- apply_global_filters(df, if (is.null(filters)) NULL else filters())

      # Apply module-specific UI filters
      if (!is.null(input$filter_call) && input$filter_call != "all") {
        df <- df %>% filter(FinalCall == input$filter_call)
      }

      if (isTRUE(input$filter_flagged_only)) {
        df <- df %>% filter(AnyFlag == TRUE)
      }

      df
    })

    # =========================================================================
    # KPIs - ALL 10 METRICS
    # =========================================================================

    output$kpi_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0")
      nrow(runs) %>% scales::comma()
    })

    output$kpi_samples <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")

      df %>%
        filter(ControlType == "Sample") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_positives <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", FinalCall == "Positive") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_late_positives <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", FinalCall == "LatePositive") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_incomplete <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", FinalCall == "Indeterminate") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_prevalence <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0%")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")

      n_pos <- sum(df$FinalCall == "Positive", na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_pos / total, 1) else 0

      paste0(pct, "%")
    })

    output$kpi_flagged <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", AnyFlag == TRUE) %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_biobank <- renderText({
      df <- filtered_samples()
      required_cols <- c("ControlType", "BiobankMatched")
      if (!nrow(df) || !all(required_cols %in% names(df))) {
        return("0")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0")

      has_id <- "SampleID" %in% names(df)
      has_name <- "SampleName" %in% names(df)

      summary <- df %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_id) as.character(SampleID) else NA_character_,
            if (has_name) as.character(SampleName) else NA_character_,
            paste0("row_", dplyr::row_number())
          )
        ) %>%
        group_by(SampleKey) %>%
        summarise(
          Linked = any(as.logical(BiobankMatched), na.rm = TRUE),
          .groups = "drop"
        )

      total_unique <- nrow(summary)
      if (!total_unique) return("0")

      n_linked <- sum(summary$Linked, na.rm = TRUE)
      percent_text <- scales::percent(n_linked / total_unique, accuracy = 0.1)

      glue::glue("{scales::comma(n_linked)} ({percent_text})")
    })

    output$kpi_extractions <- renderText({
      df <- filtered_samples()
      required_cols <- c("ControlType", "ExtractionMatched")
      if (!nrow(df) || !all(required_cols %in% names(df))) {
        return("0")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0")

      has_id <- "SampleID" %in% names(df)
      has_name <- "SampleName" %in% names(df)

      summary <- df %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_id) as.character(SampleID) else NA_character_,
            if (has_name) as.character(SampleName) else NA_character_,
            paste0("row_", dplyr::row_number())
          )
        ) %>%
        group_by(SampleKey) %>%
        summarise(
          Linked = any(as.logical(ExtractionMatched), na.rm = TRUE),
          .groups = "drop"
        )

      total_unique <- nrow(summary)
      if (!total_unique) return("0")

      n_linked <- sum(summary$Linked, na.rm = TRUE)
      percent_text <- scales::percent(n_linked / total_unique, accuracy = 0.1)

      glue::glue("{scales::comma(n_linked)} ({percent_text})")
    })

    output$kpi_dna_good <- renderText({
      df <- processed_data()$samples
      if (!nrow(df) || !"Call_RNAseP_DNA" %in% names(df)) return("N/A")

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      good <- sum(df$Call_RNAseP_DNA %in% c("Positive", "LatePositive"), na.rm = TRUE)
      total <- sum(!is.na(df$Call_RNAseP_DNA))

      if (total == 0) return("N/A")
      pct <- round(100 * good / total)
      paste0(pct, "%")
    })

    output$kpi_rna_good <- renderText({
      df <- processed_data()$samples
      if (!nrow(df) || !"Call_RNAseP_RNA" %in% names(df) || !"Delta_RP" %in% names(df)) return("N/A")

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      # Good RNA = detected AND ΔRP < limit
      limit <- settings()$delta_rp_limit
      good <- sum(
        df$Call_RNAseP_RNA %in% c("Positive", "LatePositive") &
          !is.na(df$Delta_RP) &
          df$Delta_RP <= limit,
        na.rm = TRUE
      )
      total <- sum(!is.na(df$Call_RNAseP_RNA))

      if (total == 0) return("N/A")
      pct <- round(100 * good / total)
      paste0(pct, "%")
    })

    output$kpi_valid_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"RunValid" %in% names(runs)) return("0/0")

      valid <- sum(runs$RunValid, na.rm = TRUE)
      total <- nrow(runs)

      glue("{valid}/{total}")
    })

    # =========================================================================
    # TABLES - WITH IMPROVED STYLING
    # =========================================================================

    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs

      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No MIC runs found. Check that files exist in the MIC directory."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      display_runs <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗")
        )

      available_cols <- intersect(
        c("RunID", "FileName", "RunDateTime", "WellCount", "TotalSamples",
          "Positives", "Negatives", "LatePositives", "Flagged", "RunValid"),
        names(display_runs)
      )

      datatable(
        display_runs %>% select(all_of(available_cols)),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 15, 25, 50, -1), c('10', '15', '25', '50', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        formatStyle('RunValid',
                    color = styleEqual(c('✓', '✗'), c('green', 'red')))
    })

    output$tbl_samples <- renderDT({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found. Check MIC files and filters."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      if (!"ControlType" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: ControlType column missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- df %>% filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found (only controls detected)."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Round numeric columns
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA",
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(df)
      )

      if (length(numeric_cols) > 0) {
        df <- df %>%
          mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }

      # Rename SampleID to Barcode for clarity
      if ("SampleID" %in% names(df)) {
        df <- df %>% rename(Barcode = SampleID)
      }

      # Select columns - INCLUDE BARCODE and replicate counts
      available_cols <- intersect(
        c("RunID", "Barcode", "SampleName", "FinalCall",
          "Wells_TNA_Positive", "Wells_DNA_Positive", "Wells_RNA_Positive",
          "ReplicatesTotal", "Replicates_Positive", "Replicates_Negative", "Replicates_Failed",
          "Cq_median_177T", "Cq_median_18S2",
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "Province", "Structure",
          "BiobankMatched", "ExtractionMatched", "Flags"),
        names(df)
      )

      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = which(available_cols %in% c('FinalCall', 'BiobankMatched', 'ExtractionMatched')) - 1)
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        filter = 'top'
      ) %>%
        formatStyle('FinalCall',
                    backgroundColor = styleEqual(
                      c('Positive', 'Positive_DNA', 'Positive_RNA', 'LatePositive', 'Negative', 'Indeterminate', 'Invalid_NoDNA', 'Control', 'Control_Fail'),
                      c('#d4edda', '#b3e0f2', '#d4b3f2', '#ffe8a1', '#f8f9fa', '#fff3cd', '#f8d7da', '#dbe9ff', '#f5c6cb')
                    ))
    })

    output$tbl_controls <- renderDT({
      ctrl <- processed_data()$control_status

      if (!nrow(ctrl)) {
        return(datatable(
          tibble(Message = "No control data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      available_cols <- intersect(
        c("RunID", "SampleName", "ControlType", "ControlPass", "ControlFlag"),
        names(ctrl)
      )

      ctrl <- ctrl %>%
        mutate(ControlPass = if_else(ControlPass, "✓ Pass", "✗ Fail"))

      datatable(
        ctrl %>% select(all_of(available_cols)),
        options = list(
          dom = 'tp',
          paging = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe"
      ) %>%
        formatStyle('ControlPass',
                    color = styleEqual(c('✓ Pass', '✗ Fail'), c('green', 'red')))
    })

    output$tbl_flags <- renderDT({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      if (!"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: required columns missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          AnyFlag == TRUE | (if ("FinalCall" %in% names(df)) FinalCall == "Invalid_NoDNA" else FALSE)
        )

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "✓ No flagged samples - all QC passed!"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", "Flags", "Delta_RP",
          "BiobankMatched", "ExtractionMatched"),
        names(df)
      )

      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })

    # =========================================================================
    # LEVEY-JENNINGS PLOTS (with improved styling)
    # =========================================================================

    render_lj_plot <- function(target_name, output_name) {
      output[[output_name]] <- renderPlotly({
        lj <- processed_data()$lj_stats[[target_name]]

        if (is.null(lj) || !is.list(lj)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        if (is.null(lj$data) || !nrow(lj$data)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        plot_ly(lj$data, x = ~RunID, y = ~Cq_mean,
                type = 'scatter', mode = 'markers+lines',
                name = 'Run Mean',
                marker = list(size = 12, color = '#2c3e50'),
                line = list(width = 3, color = '#2c3e50')) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~Mean, name = 'Mean',
                    line = list(color = 'black', width = 2),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus1, name = '+1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus1, name = '-1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus2, name = '+2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus2, name = '-2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus3, name = '+3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus3, name = '-3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    inherit = FALSE) %>%
          layout(
            xaxis = list(title = "Run ID", tickangle = -45, automargin = TRUE),
            yaxis = list(title = "Cq Value", automargin = TRUE),
            legend = list(orientation = 'h', y = -0.25, x = 0),
            margin = list(t = 40, r = 40, b = 120, l = 60)
          )
      })
    }

    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")

    # =========================================================================
    # QC SCATTER PLOTS (with improved styling)
    # =========================================================================

    output$scatter_tryp <- renderPlotly({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      required_cols <- c("ControlType", "Cq_median_177T", "Cq_median_18S2", "FinalCall", "SampleName")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        return(plotly_empty() %>%
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_177T),
          !is.na(Cq_median_18S2)
        )

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with both 177T and 18S2 data"))
      }

      plot_ly(df, x = ~Cq_median_177T, y = ~Cq_median_18S2,
              color = ~FinalCall,
              colors = c(
                "Positive" = "#27ae60",
                "Positive_DNA" = "#3498db",
                "Positive_RNA" = "#9b59b6",
                "LatePositive" = "#f39c12",
                "Negative" = "#95a5a6",
                "Indeterminate" = "#f1c40f",
                "Invalid_NoDNA" = "#e74c3c"
              ),
              type = 'scatter', mode = 'markers',
              text = ~SampleName,
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "177T Cq: %{x:.2f}<br>",
                "18S2 Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          title = list(text = "Trypanozoon Detection: 18S2 vs 177T", font = list(size = 16)),
          xaxis = list(title = "177T Cq (DNA)"),
          yaxis = list(title = "18S2 Cq (RNA)"),
          legend = list(title = list(text = "Call")),
          hovermode = 'closest'
        )
    })

    output$scatter_rnp <- renderPlotly({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      required_cols <- c("ControlType", "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA", "SampleName", "Delta_RP")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        return(plotly_empty() %>%
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_RNAseP_DNA),
          !is.na(Cq_median_RNAseP_RNA)
        ) %>%
        mutate(
          Quality = case_when(
            is.na(Delta_RP) ~ "Unknown",
            Delta_RP <= 5 ~ "Good",
            Delta_RP <= 8 ~ "Moderate",
            TRUE ~ "Poor"
          )
        )

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with RNAseP data"))
      }

      plot_ly(df, x = ~Cq_median_RNAseP_DNA, y = ~Cq_median_RNAseP_RNA,
              color = ~Quality,
              colors = c("Good" = "#27ae60", "Moderate" = "#f39c12",
                         "Poor" = "#e74c3c", "Unknown" = "#95a5a6"),
              type = 'scatter', mode = 'markers',
              text = ~paste0(SampleName, "<br>ΔCq: ", round(Delta_RP, 2)),
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "DNA Cq: %{x:.2f}<br>",
                "RNA Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          title = list(text = "RNA Preservation Quality (ΔCq Analysis)", font = list(size = 16)),
          xaxis = list(title = "RNAseP-DNA Cq"),
          yaxis = list(title = "RNAseP-RNA Cq"),
          legend = list(title = list(text = "RNA Quality")),
          hovermode = 'closest'
        )
    })

    # =========================================================================
    # DOWNLOADS - COMPREHENSIVE
    # =========================================================================

    output$dl_runs <- downloadHandler(
      filename = function() sprintf("mic_runs_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$runs, file)
    )

    output$dl_samples <- downloadHandler(
      filename = function() sprintf("mic_samples_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )

    output$dl_samples_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )

    output$dl_positives <- downloadHandler(
      filename = function() sprintf("mic_positives_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          filter(ControlType == "Sample", FinalCall == "Positive")
        write_csv(df, file)
      }
    )

    output$dl_deltas <- downloadHandler(
      filename = function() sprintf("mic_deltas_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          select(RunID, SampleID, SampleName, Delta_18S2_177T, Delta_RP, any_of("Flags"))
        write_csv(df, file)
      }
    )

    output$dl_flags <- downloadHandler(
      filename = function() sprintf("mic_flags_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          filter(ControlType == "Sample", AnyFlag | FinalCall == "Invalid_NoDNA")
        write_csv(df, file)
      }
    )

    output$dl_lj <- downloadHandler(
      filename = function() sprintf("mic_lj_stats_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        lj <- processed_data()$lj_stats
        stats <- map_dfr(lj, "summary")
        write_csv(stats, file)
      }
    )

    output$dl_controls <- downloadHandler(
      filename = function() sprintf("mic_controls_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$control_status, file)
    )

    output$dl_complete <- downloadHandler(
      filename = function() sprintf("mic_complete_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          showNotification("writexl package required for Excel export", type = "error")
          return()
        }

        pd <- processed_data()
        sheets <- list(
          "Samples" = filtered_samples(),
          "Runs" = pd$runs,
          "Controls" = pd$control_status,
          "LJ_Stats" = map_dfr(pd$lj_stats, "summary"),
          "Replicates" = pd$replicates
        )
        writexl::write_xlsx(sheets, file)
      }
    )

    # Quick export button
    observeEvent(input$export_qc, {
      pd <- processed_data()
      if (!nrow(pd$samples)) {
        showNotification("No data to export", type = "warning")
        return()
      }

      dir.create("outputs", showWarnings = FALSE)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      path <- file.path("outputs", glue("MIC_QC_{timestamp}.csv"))
      write_csv(pd$samples, path)

      showNotification(
        glue("QC export saved: {path}"),
        type = "message",
        duration = 5
      )
    })

    # =========================================================================
    # RETURN qPCR data for other modules (e.g., Module 06)
    # =========================================================================
    return(list(
      qpcr_samples = reactive({
        pd <- processed_data()
        if (!nrow(pd$samples)) return(tibble())
        pd$samples
      })
    ))

  })
}

# Define %||% operator if not already defined (NULL coalescing)
`%||%` <- function(x, y) if (is.null(x)) y else x

# =============================================================================
# MOUNTING INSTRUCTIONS
# =============================================================================
# In your main UI:
#   mod_mic_qpcr_ui("mic")
#
# In your main server:
#   mod_mic_qpcr_server("mic", 
#                       biobank_df = reactive(biobank_data),
#                       extractions_df = reactive(extractions_data),
#                       filters = global_filters)
#
# =============================================================================
# FEATURES OF THIS REDESIGNED MODULE:
# =============================================================================
# 
# NEW UI FEATURES:
# - Cleaner action bar with Settings modal button
# - 10 KPIs across 2 rows (was 6)
# - Filters on Samples tab (Call, Province, Structure, Flagged only)
# - Better organized Export tab with categorized downloads
# - Full-screen capable tables
# - Improved visual hierarchy
#
# NEW METRICS:
# - Prevalence percentage
# - DNA Quality (% with good RNAseP-DNA detection)
# - RNA Quality (% with good RNAseP-RNA and ΔCq < limit)
# - Valid Runs (fraction)
#
# NEW FUNCTIONALITY:
# - Settings modal (click Settings button to adjust thresholds)
# - Dynamic Province/Structure filter dropdowns
# - Multiple new download options (positives only, controls, complete Excel)
# - Better table styling with color coding
# - Improved Levey-Jennings and scatter plot styling
#
# PRESERVED CORE FUNCTIONALITY:
# - All file parsing logic (unchanged)
# - All data aggregation logic (unchanged)
# - All QC validation logic (unchanged)
# - All biobank/extraction linking logic (unchanged)
# - Caching system (unchanged)
# =============================================================================

# =============================================================================
# WRAPPER FUNCTIONS - DELEGATES TO MODULAR COORDINATOR
# =============================================================================
# NOTE: The functions above are deprecated. The modular architecture uses
# the coordinator and submodules (mod_05a through mod_05g).
# The reassignment is now done in global.R after all modules are loaded
# to ensure mod_mic_qpcr_coordinator_ui and mod_mic_qpcr_coordinator_server exist.
# =============================================================================

