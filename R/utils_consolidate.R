# ==============================================================================
# RETEST CONSOLIDATION UTILITIES
# ==============================================================================
# Generic functions for resolving retested samples across all test types
# (MIC, ELISA-PE, ELISA-VSG, iELISA)
#
# Design principles:
# 1. Generic - same logic works for all test types
# 2. Conservative - any positive = positive (clinical safety)
# 3. Transparent - always track discordance and resolution method
# 4. Extensible - easy to add new test types or rules
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ==============================================================================
# STATUS HIERARCHY
# ==============================================================================
# Define the status hierarchy for each test type
# Higher number = takes precedence in conservative resolution

#' Get status hierarchy for a test type
#' @param test_type One of: "mic", "elisa_pe", "elisa_vsg", "ielisa"
#' @return Named numeric vector with status levels
get_status_hierarchy <- function(test_type = "generic") {
  # Conservative hierarchy: Positive > Borderline > Negative > Invalid
  # Invalid is excluded from resolution (it's not a real result)
  c(
    "Positive" = 100,
    "positive" = 100,      # MIC uses lowercase
    "Borderline" = 50,
    "borderline" = 50,
    "inconclusive" = 50,   # MIC-specific
    "Negative" = 10,
    "negative" = 10,       # MIC uses lowercase
    "Invalid" = 0,
    "invalid" = 0,
    "failed" = 0,          # MIC-specific
    "RunInvalid" = 0       # MIC-specific
  )
}

#' Normalize status to standard format
#' @param status Character vector of status values
#' @param test_type Test type for context-specific normalization
#' @return Character vector with normalized status
normalize_status <- function(status, test_type = "generic") {
  status_lower <- tolower(trimws(as.character(status)))

  dplyr::case_when(
    status_lower %in% c("positive", "detected", "trypanozoon detected") ~ "Positive",
    status_lower %in% c("borderline", "inconclusive", "indeterminate", "review", "retest") ~ "Borderline",
    status_lower %in% c("negative", "not detected") ~ "Negative",
    status_lower %in% c("invalid", "failed", "runinvalid", "invalid_nodna") ~ "Invalid",
    is.na(status) ~ NA_character_,
    TRUE ~ status  # Keep original if no match

  )
}

# ==============================================================================
# CORE RESOLUTION FUNCTION
# ==============================================================================

#' Resolve multiple test results to a single consolidated status
#'
#' This is the CORE function that applies conservative resolution logic:
#' - Invalid results are excluded from consideration
#' - If all valid results agree -> use that result
#' - If discordant -> use highest priority (Positive > Borderline > Negative)
#' - Always flag discordance for review
#'
#' @param statuses Character vector of status values from multiple tests
#' @param test_type Test type (for normalization context)
#' @param dates Optional: dates of tests (for tracking)
#' @param run_ids Optional: run IDs (for tracking)
#' @return List with:
#'   - status_final: Resolved status
#'   - n_tests: Total number of tests
#'   - n_valid: Number of valid (non-Invalid) tests
#'   - is_discordant: TRUE if valid tests disagree
#'   - resolution_method: How the status was resolved
#'   - detail: Human-readable summary
#'   - all_statuses: Original statuses for audit
#' @export
resolve_retest_results <- function(statuses,
                                    test_type = "generic",
                                    dates = NULL,
                                    run_ids = NULL) {

  # Handle empty input
  if (length(statuses) == 0 || all(is.na(statuses))) {
    return(list(
      status_final = NA_character_,
      n_tests = 0L,
      n_valid = 0L,
      is_discordant = FALSE,
      resolution_method = "no_tests",
      detail = "No tests available",
      all_statuses = character()
    ))
  }

  # Normalize all statuses
  statuses_norm <- normalize_status(statuses, test_type)

  # Get hierarchy
  hierarchy <- get_status_hierarchy(test_type)

  # Separate valid from invalid
  valid_mask <- !is.na(statuses_norm) &
                !statuses_norm %in% c("Invalid", "invalid", "failed", "RunInvalid")

  valid_statuses <- statuses_norm[valid_mask]
  n_tests <- length(statuses)
  n_valid <- length(valid_statuses)

  # Handle no valid results

if (n_valid == 0) {
    return(list(
      status_final = "Invalid",
      n_tests = n_tests,
      n_valid = 0L,
      is_discordant = FALSE,
      resolution_method = "all_invalid",
      detail = sprintf("All %d tests invalid", n_tests),
      all_statuses = statuses_norm
    ))
  }

  # Count each valid status type
  status_counts <- table(valid_statuses)
  unique_valid <- names(status_counts)

  # Check for discordance (multiple different valid statuses)
  is_discordant <- length(unique_valid) > 1

  # Resolve status
  if (!is_discordant) {
    # All valid tests agree
    status_final <- unique_valid[1]
    resolution_method <- ifelse(n_valid == 1, "single_test", "concordant")
    detail <- sprintf("%d %s (%s)", n_valid,
                      ifelse(n_valid == 1, "test", "concordant tests"),
                      status_final)
  } else {
    # Discordant - use conservative (highest priority) resolution
    # Get priorities for each unique status
    priorities <- hierarchy[unique_valid]
    priorities[is.na(priorities)] <- -1  # Unknown statuses get lowest priority

    # Take highest priority status
    status_final <- unique_valid[which.max(priorities)]
    resolution_method <- "conservative"

    # Build detail string
    count_parts <- paste(
      sprintf("%d %s", as.integer(status_counts), names(status_counts)),
      collapse = ", "
    )
    detail <- sprintf("DISCORDANT: %s -> %s (conservative)",
                      count_parts, status_final)
  }

  # Normalize final status
  status_final <- normalize_status(status_final, test_type)

  return(list(
    status_final = status_final,
    n_tests = n_tests,
    n_valid = n_valid,
    is_discordant = is_discordant,
    resolution_method = resolution_method,
    detail = detail,
    all_statuses = statuses_norm
  ))
}

# ==============================================================================
# DATA FRAME CONSOLIDATION
# ==============================================================================

#' Consolidate retested samples in a data frame
#'
#' Groups samples by identifier and applies resolution logic.
#' Keeps all original rows but adds consolidated columns.
#'
#' @param df Data frame with test results
#' @param sample_id_col Column name(s) for sample identifier (can be vector for composite keys)
#' @param status_col Column name containing status values
#' @param date_col Optional: column name for test dates
#' @param run_id_col Optional: column name for run identifiers
#' @param test_type Test type for normalization
#' @param prefix Prefix for new columns (default: based on test_type)
#' @return Data frame with added consolidation columns:
#'   - {prefix}_status_final: Consolidated status
#'   - {prefix}_n_tests: Number of tests for this sample
#'   - {prefix}_is_discordant: TRUE if results disagreed
#'   - {prefix}_resolution_detail: Human-readable resolution summary
#' @export
consolidate_sample_results <- function(df,
                                        sample_id_col,
                                        status_col,
                                        date_col = NULL,
                                        run_id_col = NULL,
                                        test_type = "generic",
                                        prefix = NULL) {

  if (!nrow(df)) return(df)

  # Validate columns exist
  required_cols <- c(sample_id_col, status_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Missing columns: ", paste(missing_cols, collapse = ", "))
    return(df)
  }

  # Set prefix
  if (is.null(prefix)) {
    prefix <- test_type
  }

  # Create sample key for grouping (handle composite keys)
  if (length(sample_id_col) == 1) {
    df <- df %>% mutate(.sample_key = as.character(!!sym(sample_id_col)))
  } else {
    # Composite key
    df <- df %>%
      mutate(.sample_key = paste(!!!syms(sample_id_col), sep = "_"))
  }

  # Get statuses for each sample
  consolidation_summary <- df %>%
    group_by(.sample_key) %>%
    summarise(
      .resolution = list(resolve_retest_results(
        statuses = !!sym(status_col),
        test_type = test_type
      )),
      .groups = "drop"
    ) %>%
    mutate(
      !!paste0(prefix, "_status_final") := map_chr(.resolution, ~ .x$status_final),
      !!paste0(prefix, "_n_tests") := map_int(.resolution, ~ as.integer(.x$n_tests)),
      !!paste0(prefix, "_n_valid") := map_int(.resolution, ~ as.integer(.x$n_valid)),
      !!paste0(prefix, "_is_discordant") := map_lgl(.resolution, ~ .x$is_discordant),
      !!paste0(prefix, "_resolution_method") := map_chr(.resolution, ~ .x$resolution_method),
      !!paste0(prefix, "_resolution_detail") := map_chr(.resolution, ~ .x$detail)
    ) %>%
    select(-.resolution)

  # Join back to original data
  result <- df %>%
    left_join(consolidation_summary, by = ".sample_key") %>%
    select(-.sample_key)

  return(result)
}

# ==============================================================================
# UNIQUE SAMPLE SUMMARY
# ==============================================================================

#' Get one row per unique sample with consolidated results
#'
#' Returns a summary table with one row per sample, containing:
#' - Sample identifiers
#' - Consolidated status
#' - Discordance flags
#' - Test counts
#'
#' @param df Data frame with consolidation columns (output of consolidate_sample_results)
#' @param sample_id_col Column name(s) for sample identifier
#' @param prefix Prefix used in consolidation
#' @param keep_cols Additional columns to keep (will take first value per sample)
#' @return Data frame with one row per unique sample
#' @export
get_sample_summary <- function(df,
                                sample_id_col,
                                prefix,
                                keep_cols = NULL) {

  if (!nrow(df)) return(tibble())

  # Define consolidation columns to include
  consol_cols <- c(
    paste0(prefix, "_status_final"),
    paste0(prefix, "_n_tests"),
    paste0(prefix, "_n_valid"),
    paste0(prefix, "_is_discordant"),
    paste0(prefix, "_resolution_method"),
    paste0(prefix, "_resolution_detail")
  )

  # Filter to columns that exist
  consol_cols <- consol_cols[consol_cols %in% names(df)]

  # Group and summarize
  summary_df <- df %>%
    group_by(across(all_of(sample_id_col))) %>%
    summarise(
      across(all_of(consol_cols), first),
      across(all_of(keep_cols[keep_cols %in% names(df)]), first),
      .groups = "drop"
    )

  return(summary_df)
}

# ==============================================================================
# CLASSIFICATION HELPERS
# ==============================================================================

#' Classify suspect based on all test results
#'
#' Applies the classification logic:
#' - Molecular positive: MIC positive
#' - Serological positive: ELISA-VSG OR iELISA positive
#'   - High confidence: Both positive
#'   - Low confidence: Only ELISA-VSG positive
#'   - Unusual: Only iELISA positive
#' - Both: Molecular AND Serological positive
#'
#' @param mic_status MIC consolidated status (or NA if not tested)
#' @param elisa_vsg_status ELISA-VSG consolidated status (or NA)
#' @param elisa_pe_status ELISA-PE consolidated status (for comparison, or NA)
#' @param ielisa_status iELISA consolidated status (or NA)
#' @return List with classification results
#' @export
classify_suspect <- function(mic_status = NA_character_,
                              elisa_vsg_status = NA_character_,
                              elisa_pe_status = NA_character_,
                              ielisa_status = NA_character_) {

  # Helper to check if status is positive
  is_positive <- function(status) {
    !is.na(status) && tolower(status) %in% c("positive", "detected")
  }

  # Helper to check if status is borderline
  is_borderline <- function(status) {
    !is.na(status) && tolower(status) %in% c("borderline", "inconclusive")
  }

  # Molecular status (MIC)
  molecular_positive <- is_positive(mic_status)
  molecular_borderline <- is_borderline(mic_status)

  # Serological status
  elisa_vsg_positive <- is_positive(elisa_vsg_status)
  ielisa_positive <- is_positive(ielisa_status)
  elisa_pe_positive <- is_positive(elisa_pe_status)  # For comparison

  # Any serological positive
  serological_positive <- elisa_vsg_positive || ielisa_positive

  # Serological confidence
  serological_confidence <- dplyr::case_when(
    elisa_vsg_positive && ielisa_positive ~ "High",
    elisa_vsg_positive && !ielisa_positive ~ "Low",
    !elisa_vsg_positive && ielisa_positive ~ "Unusual",
    TRUE ~ NA_character_
  )

  # PE vs VSG comparison (for development validation)
  pe_vsg_concordant <- dplyr::case_when(
    is.na(elisa_pe_status) || is.na(elisa_vsg_status) ~ NA,
    is_positive(elisa_pe_status) == is_positive(elisa_vsg_status) ~ TRUE,
    TRUE ~ FALSE
  )

  # Overall classification
  overall_classification <- dplyr::case_when(
    molecular_positive && serological_positive ~
      paste0("Both (", serological_confidence, " serology)"),
    molecular_positive ~ "Molecular only",
    serological_positive ~
      paste0("Serological only (", serological_confidence, ")"),
    molecular_borderline || is_borderline(elisa_vsg_status) || is_borderline(ielisa_status) ~
      "Borderline - Review",
    TRUE ~ "Negative"
  )

  return(list(
    molecular_positive = molecular_positive,
    molecular_borderline = molecular_borderline,
    serological_positive = serological_positive,
    serological_confidence = serological_confidence,
    elisa_vsg_positive = elisa_vsg_positive,
    ielisa_positive = ielisa_positive,
    elisa_pe_positive = elisa_pe_positive,
    pe_vsg_concordant = pe_vsg_concordant,
    classification = overall_classification
  ))
}

#' Vectorized version of classify_suspect for data frames
#' @export
classify_suspects_df <- function(df,
                                  mic_col = "mic_status_final",
                                  elisa_vsg_col = "elisa_vsg_status_final",
                                  elisa_pe_col = "elisa_pe_status_final",
                                  ielisa_col = "ielisa_status_final") {

  # Get values (use NA if column doesn't exist)
  get_col <- function(col_name) {
    if (col_name %in% names(df)) df[[col_name]] else NA_character_
  }

  mic <- get_col(mic_col)
  vsg <- get_col(elisa_vsg_col)
  pe <- get_col(elisa_pe_col)
  ielisa <- get_col(ielisa_col)

  # Apply classification row by row
  classifications <- purrr::pmap(
    list(mic, vsg, pe, ielisa),
    function(m, v, p, i) classify_suspect(m, v, p, i)
  )

  # Extract results into columns
  df %>%
    mutate(
      molecular_positive = map_lgl(classifications, ~ .x$molecular_positive),
      serological_positive = map_lgl(classifications, ~ .x$serological_positive),
      serological_confidence = map_chr(classifications, ~ .x$serological_confidence %||% NA_character_),
      pe_vsg_concordant = map_lgl(classifications, ~ .x$pe_vsg_concordant %||% NA),
      suspect_classification = map_chr(classifications, ~ .x$classification)
    )
}

# ==============================================================================
# DISCORDANCE SUMMARY
# ==============================================================================

#' Get summary of all discordant samples
#' @param df Data frame with consolidation columns
#' @param prefix Prefix used in consolidation
#' @return Data frame with only discordant samples
#' @export
get_discordant_samples <- function(df, prefix) {
  discordant_col <- paste0(prefix, "_is_discordant")

  if (!discordant_col %in% names(df)) {
    warning("Discordance column not found: ", discordant_col)
    return(tibble())
  }

  df %>%
    filter(!!sym(discordant_col) == TRUE)
}
