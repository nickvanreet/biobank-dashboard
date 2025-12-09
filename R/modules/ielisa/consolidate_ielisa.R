# ==============================================================================
# iELISA - RETEST CONSOLIDATION MODULE
# ==============================================================================
# Consolidates retested iELISA samples using conservative resolution
# Handles per-antigen (LiTat 1.3 and 1.5) and overall sample status
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ==============================================================================
# iELISA-SPECIFIC STATUS HANDLING
# ==============================================================================

#' Normalize iELISA status values
#' @param status Character vector of status values
#' @return Normalized status vector
#' @export
normalize_ielisa_status <- function(status) {
  status_lower <- tolower(trimws(as.character(status)))

  dplyr::case_when(
    status_lower %in% c("positive", "pos", "+", "true") ~ "Positive",
    status_lower %in% c("borderline", "equivocal") ~ "Borderline",
    status_lower %in% c("negative", "neg", "-", "false") ~ "Negative",
    status_lower %in% c("invalid", "failed", "na") ~ "Invalid",
    is.na(status) ~ NA_character_,
    TRUE ~ status
  )
}

#' Determine per-antigen status from inhibition percentage
#' @param pct_inhibition Percent inhibition value
#' @param positive_threshold Threshold for positive (default: 30)
#' @param borderline_threshold Lower threshold for borderline zone (default: 25)
#' @return Status string: "Positive", "Borderline", "Negative", or "Invalid"
#' @export
classify_ielisa_inhibition <- function(pct_inhibition,
                                        positive_threshold = 30,
                                        borderline_threshold = 25) {
  dplyr::case_when(
    is.na(pct_inhibition) ~ "Invalid",
    pct_inhibition >= positive_threshold ~ "Positive",
    pct_inhibition >= borderline_threshold ~ "Borderline",
    TRUE ~ "Negative"
  )
}

# ==============================================================================
# MAIN iELISA CONSOLIDATION FUNCTION
# ==============================================================================

#' Consolidate retested iELISA samples
#'
#' Handles consolidation at two levels:
#' 1. Per-antigen (LiTat 1.3 and LiTat 1.5 separately)
#' 2. Overall sample (positive if either antigen positive)
#'
#' @param ielisa_df Data frame with iELISA results
#' @param sample_id_cols Column names for sample identifier
#' @param status_col Column name for overall status (default: "status_final")
#' @param include_all_runs If TRUE, keep all runs
#' @param positive_threshold Inhibition threshold for positive
#' @param borderline_threshold Inhibition threshold for borderline zone
#' @return Data frame with consolidation columns
#' @export
consolidate_ielisa_results <- function(ielisa_df,
                                        sample_id_cols = c("numero_labo", "code_barres_kps"),
                                        status_col = "status_final",
                                        include_all_runs = TRUE,
                                        positive_threshold = 30,
                                        borderline_threshold = 25) {

  if (!nrow(ielisa_df)) return(ielisa_df)

  prefix <- "ielisa"

  # Validate sample ID columns
  sample_id_cols <- sample_id_cols[sample_id_cols %in% names(ielisa_df)]
  if (length(sample_id_cols) == 0) {
    # Try alternative columns
    alt_cols <- c("SampleIndex", "sample_id", "barcode")
    for (col in alt_cols) {
      if (col %in% names(ielisa_df)) {
        sample_id_cols <- col
        break
      }
    }
    if (length(sample_id_cols) == 0) {
      warning("No valid sample ID columns found for iELISA consolidation")
      return(ielisa_df)
    }
  }

  # Add per-antigen status if we have inhibition values
  if ("pct_inh_f1_13" %in% names(ielisa_df)) {
    ielisa_df <- ielisa_df %>%
      mutate(
        status_L13 = classify_ielisa_inhibition(pct_inh_f1_13, positive_threshold, borderline_threshold),
        status_L15 = classify_ielisa_inhibition(pct_inh_f1_15, positive_threshold, borderline_threshold)
      )
  }

  # Normalize overall status
  if (status_col %in% names(ielisa_df)) {
    ielisa_df <- ielisa_df %>%
      mutate(
        ielisa_status_normalized = normalize_ielisa_status(!!sym(status_col))
      )
  } else {
    # Derive from antigen statuses
    ielisa_df <- ielisa_df %>%
      mutate(
        ielisa_status_normalized = dplyr::case_when(
          status_L13 == "Positive" | status_L15 == "Positive" ~ "Positive",
          status_L13 == "Borderline" | status_L15 == "Borderline" ~ "Borderline",
          status_L13 == "Invalid" & status_L15 == "Invalid" ~ "Invalid",
          TRUE ~ "Negative"
        )
      )
  }

  # Apply generic consolidation for overall status
  ielisa_df <- consolidate_sample_results(
    df = ielisa_df,
    sample_id_col = sample_id_cols,
    status_col = "ielisa_status_normalized",
    test_type = "ielisa",
    prefix = prefix
  )

  # Also consolidate per-antigen if available
  if ("status_L13" %in% names(ielisa_df)) {
    # LiTat 1.3 consolidation
    l13_consol <- ielisa_df %>%
      consolidate_sample_results(
        sample_id_col = sample_id_cols,
        status_col = "status_L13",
        test_type = "ielisa",
        prefix = "ielisa_L13"
      ) %>%
      select(all_of(sample_id_cols),
             starts_with("ielisa_L13_"))

    # LiTat 1.5 consolidation
    l15_consol <- ielisa_df %>%
      consolidate_sample_results(
        sample_id_col = sample_id_cols,
        status_col = "status_L15",
        test_type = "ielisa",
        prefix = "ielisa_L15"
      ) %>%
      select(all_of(sample_id_cols),
             starts_with("ielisa_L15_"))

    # Join back (avoiding duplicates)
    ielisa_df <- ielisa_df %>%
      left_join(
        l13_consol %>% distinct(across(all_of(sample_id_cols)), .keep_all = TRUE),
        by = sample_id_cols
      ) %>%
      left_join(
        l15_consol %>% distinct(across(all_of(sample_id_cols)), .keep_all = TRUE),
        by = sample_id_cols
      )
  }

  # Add retest flag
  ielisa_df <- ielisa_df %>%
    mutate(
      ielisa_is_retest = ielisa_n_tests > 1
    )

  # Add confidence score
  ielisa_df <- ielisa_df %>%
    mutate(
      ielisa_confidence = dplyr::case_when(
        ielisa_is_discordant ~ "Low",
        ielisa_n_tests >= 2 & !ielisa_is_discordant ~ "High",
        ielisa_n_tests == 1 ~ "Medium",
        TRUE ~ "Unknown"
      )
    )

  # Add antigen detail column
  if (all(c("ielisa_L13_status_final", "ielisa_L15_status_final") %in% names(ielisa_df))) {
    ielisa_df <- ielisa_df %>%
      mutate(
        ielisa_antigen_detail = dplyr::case_when(
          ielisa_L13_status_final == "Positive" & ielisa_L15_status_final == "Positive" ~ "Both antigens",
          ielisa_L13_status_final == "Positive" ~ "LiTat 1.3 only",
          ielisa_L15_status_final == "Positive" ~ "LiTat 1.5 only",
          TRUE ~ "Neither"
        )
      )
  }

  if (!include_all_runs) {
    ielisa_df <- get_ielisa_sample_summary(ielisa_df, sample_id_cols)
  }

  return(ielisa_df)
}

#' Get one row per sample for iELISA
#' @export
get_ielisa_sample_summary <- function(ielisa_df, sample_id_cols) {

  consol_cols <- names(ielisa_df)[grepl("^ielisa_", names(ielisa_df))]

  ielisa_df %>%
    group_by(across(all_of(sample_id_cols))) %>%
    summarise(
      across(all_of(consol_cols), first),
      mean_pct_inh_L13 = mean(pct_inh_f1_13, na.rm = TRUE),
      mean_pct_inh_L15 = mean(pct_inh_f1_15, na.rm = TRUE),
      test_dates = paste(unique(na.omit(plate_date)), collapse = ", "),
      test_plates = paste(unique(na.omit(plate_id)), collapse = ", "),
      .groups = "drop"
    )
}

#' Get discordance report for iELISA
#' @export
get_ielisa_discordance_report <- function(ielisa_df, sample_id_cols) {

  if (!"ielisa_is_discordant" %in% names(ielisa_df)) {
    warning("Run consolidate_ielisa_results first")
    return(tibble())
  }

  ielisa_df %>%
    filter(ielisa_is_discordant == TRUE) %>%
    group_by(across(all_of(sample_id_cols))) %>%
    summarise(
      n_tests = first(ielisa_n_tests),
      final_status = first(ielisa_status_final),
      all_results = paste(ielisa_status_normalized, collapse = " | "),
      all_L13 = paste(round(pct_inh_f1_13, 1), collapse = " | "),
      all_L15 = paste(round(pct_inh_f1_15, 1), collapse = " | "),
      all_dates = paste(unique(na.omit(plate_date)), collapse = " | "),
      .groups = "drop"
    )
}

#' Add consolidation to iELISA pipeline output
#' @export
add_ielisa_consolidation <- function(ielisa_result) {

  if (is.null(ielisa_result$samples) || !nrow(ielisa_result$samples)) {
    return(ielisa_result)
  }

  ielisa_result$samples <- consolidate_ielisa_results(
    ielisa_result$samples,
    include_all_runs = TRUE
  )

  ielisa_result$discordance_summary <- get_ielisa_discordance_report(
    ielisa_result$samples,
    sample_id_cols = c("numero_labo", "code_barres_kps")
  )

  ielisa_result$sample_summary <- get_ielisa_sample_summary(
    ielisa_result$samples,
    sample_id_cols = c("numero_labo", "code_barres_kps")
  )

  return(ielisa_result)
}
