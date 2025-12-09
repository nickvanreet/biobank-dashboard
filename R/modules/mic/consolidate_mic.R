# ==============================================================================
# MIC qPCR - RETEST CONSOLIDATION MODULE
# ==============================================================================
# Consolidates retested MIC samples using conservative resolution logic:
# - Any valid positive = final positive
# - Tracks discordance for review
# - Adds confidence scoring based on replicate agreement and Cq values
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# Source the generic consolidation utility
# (In production, this would be handled by package loading)
# source("R/utils_consolidate.R")

# ==============================================================================
# MIC-SPECIFIC STATUS EXTRACTION
# ==============================================================================

#' Extract MIC status from FinalCall column
#'
#' MIC has specific FinalCall values that need to be mapped to standard statuses:
#' - "Positive", "Detected", "Trypanozoon*" -> Positive
#' - "Negative" -> Negative
#' - "Indeterminate", "Inconclusive", "Review" -> Borderline
#' - "Invalid", "Failed", "RunInvalid" -> Invalid
#'
#' @param final_call Character vector of FinalCall values
#' @return Character vector of normalized statuses
#' @export
normalize_mic_status <- function(final_call) {
  call_lower <- tolower(trimws(as.character(final_call)))

  dplyr::case_when(
    grepl("^positive|detected|trypanozoon", call_lower) ~ "Positive",
    grepl("^negative$|not detected", call_lower) ~ "Negative",
    grepl("indeterminate|inconclusive|review|retest", call_lower) ~ "Borderline",
    grepl("invalid|failed|runinvalid", call_lower) ~ "Invalid",
    is.na(final_call) ~ NA_character_,
    TRUE ~ "Unknown"
  )
}

# ==============================================================================
# MIC CONFIDENCE SCORING
# ==============================================================================

#' Calculate MIC confidence score for a sample
#'
#' Confidence is based on:
#' 1. Number of agreeing replicates
#' 2. Cq values (lower = more confident)
#' 3. RNA/DNA quality markers
#'
#' @param n_positive Number of positive replicates
#' @param n_negative Number of negative replicates
#' @param n_total Total replicates
#' @param avg_cq_177T Average Cq for 177T (if positive)
#' @param dna_quality DNA quality status
#' @param rna_quality RNA quality status
#' @return Character: "High", "Medium", or "Low"
#' @export
calculate_mic_confidence <- function(n_positive,
                                      n_negative,
                                      n_total,
                                      avg_cq_177T = NA,
                                      dna_quality = "Good",
                                      rna_quality = "Good") {

  # Default to low if no data
  if (is.na(n_total) || n_total == 0) return("Low")

  # Calculate agreement ratio
  max_agreement <- max(n_positive, n_negative, na.rm = TRUE)
  agreement_ratio <- max_agreement / n_total

  # High confidence criteria:
  # - 3+ out of 4 replicates agree
  # - If positive: Cq < 32
  # - Good DNA quality
  if (agreement_ratio >= 0.75 && dna_quality == "Good") {
    if (n_positive > n_negative) {
      # Positive result - check Cq
      if (!is.na(avg_cq_177T) && avg_cq_177T < 32) {
        return("High")
      } else if (!is.na(avg_cq_177T) && avg_cq_177T < 38) {
        return("Medium")
      }
    } else {
      # Negative result with good agreement
      return("High")
    }
  }

  # Medium confidence:
  # - 2+ out of 4 replicates agree
  # - Reasonable DNA quality
  if (agreement_ratio >= 0.5 && dna_quality %in% c("Good", "Acceptable")) {
    return("Medium")
  }

  # Low confidence for everything else
  return("Low")
}

# ==============================================================================
# MAIN MIC CONSOLIDATION FUNCTION
# ==============================================================================

#' Consolidate retested MIC samples
#'
#' Takes MIC sample data (potentially with multiple runs per sample) and:
#' 1. Identifies retested samples
#' 2. Applies conservative resolution (any positive = positive)
#' 3. Flags discordant results
#' 4. Calculates confidence scores
#'
#' @param mic_df Data frame with MIC sample results. Expected columns:
#'   - SampleID or SampleName: sample identifier
#'   - FinalCall: result call (Positive, Negative, etc.)
#'   - RunID or RunDate: to identify different test runs
#'   - Optional: Cq values, quality columns
#' @param sample_id_col Column name for sample identifier (default: auto-detect)
#' @param status_col Column name for status (default: "FinalCall")
#' @param include_all_runs If TRUE, keep all runs in output. If FALSE, return one row per sample.
#' @return Data frame with consolidation columns added:
#'   - mic_status_final: Consolidated status
#'   - mic_status_normalized: Normalized version of status
#'   - mic_n_tests: Number of test runs
#'   - mic_is_discordant: TRUE if results disagreed
#'   - mic_is_retest: TRUE if tested more than once
#'   - mic_confidence: Confidence score
#'   - mic_resolution_detail: Human-readable summary
#' @export
consolidate_mic_results <- function(mic_df,
                                     sample_id_col = NULL,
                                     status_col = "FinalCall",
                                     include_all_runs = TRUE) {

  if (!nrow(mic_df)) {
    return(mic_df)
  }

  # Auto-detect sample ID column
  if (is.null(sample_id_col)) {
    possible_id_cols <- c("SampleID", "SampleName", "Name", "LinkedBarcode", "Barcode")
    for (col in possible_id_cols) {
      if (col %in% names(mic_df)) {
        sample_id_col <- col
        break
      }
    }
    if (is.null(sample_id_col)) {
      warning("Could not auto-detect sample ID column. Using row numbers.")
      mic_df$.__sample_id <- seq_len(nrow(mic_df))
      sample_id_col <- ".__sample_id"
    }
  }

  # Validate status column
  if (!status_col %in% names(mic_df)) {
    # Try alternative columns
    alt_status_cols <- c("FinalCall", "final_category", "Call", "Status", "Result")
    for (col in alt_status_cols) {
      if (col %in% names(mic_df)) {
        status_col <- col
        break
      }
    }
    if (!status_col %in% names(mic_df)) {
      warning("Status column not found: ", status_col)
      return(mic_df)
    }
  }

  # Normalize the status column first
  mic_df <- mic_df %>%
    mutate(
      mic_status_normalized = normalize_mic_status(!!sym(status_col))
    )

  # Use the generic consolidation function
  mic_df <- consolidate_sample_results(
    df = mic_df,
    sample_id_col = sample_id_col,
    status_col = "mic_status_normalized",
    test_type = "mic",
    prefix = "mic"
  )

  # Add is_retest flag
  mic_df <- mic_df %>%
    mutate(
      mic_is_retest = mic_n_tests > 1
    )

  # Calculate confidence scores if we have the necessary columns
  has_replicate_info <- all(c("n_positive", "n_negative", "n_replicates") %in% names(mic_df)) ||
                        all(c("Pos177T", "Neg177T") %in% names(mic_df))

  if (has_replicate_info) {
    # Use replicate counts if available
    if ("n_positive" %in% names(mic_df)) {
      mic_df <- mic_df %>%
        mutate(
          mic_confidence = pmap_chr(
            list(
              n_positive = n_positive,
              n_negative = n_negative,
              n_total = n_replicates,
              avg_cq = if ("avg_177T_Cq" %in% names(.)) avg_177T_Cq else NA_real_,
              dna_qual = if ("dna_quality" %in% names(.)) dna_quality else "Good"
            ),
            function(n_positive, n_negative, n_total, avg_cq, dna_qual) {
              calculate_mic_confidence(n_positive, n_negative, n_total, avg_cq, dna_qual)
            }
          )
        )
    }
  } else {
    # Default confidence based on agreement only
    mic_df <- mic_df %>%
      mutate(
        mic_confidence = dplyr::case_when(
          mic_is_discordant ~ "Low",
          mic_n_tests >= 2 & !mic_is_discordant ~ "High",
          mic_n_tests == 1 ~ "Medium",
          TRUE ~ "Unknown"
        )
      )
  }

  # If only one row per sample requested
  if (!include_all_runs) {
    mic_df <- get_mic_sample_summary(mic_df, sample_id_col)
  }

  return(mic_df)
}

#' Get one row per sample summary for MIC
#'
#' @param mic_df Consolidated MIC data frame
#' @param sample_id_col Sample ID column name
#' @return Data frame with one row per sample
#' @export
get_mic_sample_summary <- function(mic_df, sample_id_col = "SampleID") {

  # Define columns to keep (take first value)
  keep_cols <- c(
    "SampleName", "LinkedBarcode", "LinkedNumero",
    "HealthZone", "HealthArea", "Village"
  )
  keep_cols <- keep_cols[keep_cols %in% names(mic_df)]

  # MIC-specific columns
  mic_cols <- c(
    "mic_status_final", "mic_status_normalized", "mic_n_tests", "mic_n_valid",
    "mic_is_discordant", "mic_is_retest", "mic_confidence",
    "mic_resolution_method", "mic_resolution_detail"
  )
  mic_cols <- mic_cols[mic_cols %in% names(mic_df)]

  mic_df %>%
    group_by(across(all_of(sample_id_col))) %>%
    summarise(
      across(all_of(mic_cols), first),
      across(all_of(keep_cols), first),
      # Aggregate test info
      test_dates = paste(unique(na.omit(RunDate)), collapse = ", "),
      test_run_ids = paste(unique(na.omit(RunID)), collapse = ", "),
      .groups = "drop"
    )
}

# ==============================================================================
# DISCORDANCE ANALYSIS
# ==============================================================================

#' Get detailed discordance report for MIC samples
#'
#' @param mic_df Consolidated MIC data frame
#' @param sample_id_col Sample ID column
#' @return Data frame with discordant samples and their details
#' @export
get_mic_discordance_report <- function(mic_df, sample_id_col = "SampleID") {

  if (!"mic_is_discordant" %in% names(mic_df)) {
    warning("mic_is_discordant column not found. Run consolidate_mic_results first.")
    return(tibble())
  }

  # Filter to discordant samples
  discordant_df <- mic_df %>%
    filter(mic_is_discordant == TRUE)

  if (!nrow(discordant_df)) {
    message("No discordant MIC samples found")
    return(tibble())
  }

  # Get details for each discordant sample
  discordant_summary <- discordant_df %>%
    group_by(across(all_of(sample_id_col))) %>%
    summarise(
      n_tests = first(mic_n_tests),
      final_status = first(mic_status_final),
      confidence = first(mic_confidence),
      resolution_detail = first(mic_resolution_detail),
      # List all individual results
      all_results = paste(mic_status_normalized, collapse = " | "),
      all_dates = paste(unique(na.omit(RunDate)), collapse = " | "),
      .groups = "drop"
    ) %>%
    arrange(desc(n_tests))

  return(discordant_summary)
}

# ==============================================================================
# INTEGRATION HELPER
# ==============================================================================

#' Add consolidation columns to existing MIC pipeline output
#'
#' This function can be called at the end of the MIC pipeline to add
#' consolidation without changing existing logic.
#'
#' @param processed_mic List output from MIC pipeline (with $samples element)
#' @return Same list with consolidation columns added to $samples
#' @export
add_mic_consolidation <- function(processed_mic) {

  if (is.null(processed_mic$samples) || !nrow(processed_mic$samples)) {
    return(processed_mic)
  }

  # Apply consolidation
  processed_mic$samples <- consolidate_mic_results(
    processed_mic$samples,
    include_all_runs = TRUE
  )

  # Add discordance summary
  processed_mic$discordance_summary <- get_mic_discordance_report(
    processed_mic$samples
  )

  # Add sample summary (one row per sample)
  processed_mic$sample_summary <- get_mic_sample_summary(
    processed_mic$samples
  )

  return(processed_mic)
}
