# ==============================================================================
# ELISA - RETEST CONSOLIDATION MODULE
# ==============================================================================
# Consolidates retested ELISA samples (PE and VSG) using conservative resolution
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ==============================================================================
# ELISA-SPECIFIC STATUS NORMALIZATION
# ==============================================================================

#' Normalize ELISA status values
#' @param status Character vector of status values
#' @return Normalized status vector
#' @export
normalize_elisa_status <- function(status) {
  status_lower <- tolower(trimws(as.character(status)))

  dplyr::case_when(
    status_lower %in% c("positive", "pos", "+") ~ "Positive",
    status_lower %in% c("borderline", "equivocal", "indeterminate") ~ "Borderline",
    status_lower %in% c("negative", "neg", "-") ~ "Negative",
    status_lower %in% c("invalid", "failed", "qc_fail") ~ "Invalid",
    is.na(status) ~ NA_character_,
    TRUE ~ status
  )
}

# ==============================================================================
# MAIN ELISA CONSOLIDATION FUNCTION
# ==============================================================================

#' Consolidate retested ELISA samples
#'
#' @param elisa_df Data frame with ELISA results
#' @param elisa_type "PE" or "VSG" (for prefix naming)
#' @param sample_id_cols Column names for sample identifier (default: c("numero_labo", "code_barres_kps"))
#' @param status_col Column name for status (default: "status_raw")
#' @param include_all_runs If TRUE, keep all runs
#' @return Data frame with consolidation columns
#' @export
consolidate_elisa_results <- function(elisa_df,
                                       elisa_type = "PE",
                                       sample_id_cols = c("numero_labo", "code_barres_kps"),
                                       status_col = "status_raw",
                                       include_all_runs = TRUE) {

  if (!nrow(elisa_df)) return(elisa_df)

  # Set prefix based on type
  prefix <- paste0("elisa_", tolower(elisa_type))

  # Filter to samples only (exclude controls)
  sample_mask <- if ("sample_type" %in% names(elisa_df)) {
    elisa_df$sample_type == "sample"
  } else {
    rep(TRUE, nrow(elisa_df))
  }

  samples_df <- elisa_df[sample_mask, ]
  controls_df <- elisa_df[!sample_mask, ]

  if (!nrow(samples_df)) {
    return(elisa_df)
  }

  # Validate sample ID columns exist
  sample_id_cols <- sample_id_cols[sample_id_cols %in% names(samples_df)]
  if (length(sample_id_cols) == 0) {
    warning("No valid sample ID columns found")
    return(elisa_df)
  }

  # Normalize status
  samples_df <- samples_df %>%
    mutate(
      !!paste0(prefix, "_status_normalized") := normalize_elisa_status(!!sym(status_col))
    )

  # Apply generic consolidation
  samples_df <- consolidate_sample_results(
    df = samples_df,
    sample_id_col = sample_id_cols,
    status_col = paste0(prefix, "_status_normalized"),
    test_type = "elisa",
    prefix = prefix
  )

  # Add retest flag
  samples_df <- samples_df %>%
    mutate(
      !!paste0(prefix, "_is_retest") := !!sym(paste0(prefix, "_n_tests")) > 1
    )

  # Add quality-based confidence
  if ("qc_overall" %in% names(samples_df) && "cv_Ag_plus" %in% names(samples_df)) {
    samples_df <- samples_df %>%
      mutate(
        !!paste0(prefix, "_confidence") := dplyr::case_when(
          !!sym(paste0(prefix, "_is_discordant")) ~ "Low",
          qc_overall == TRUE & cv_Ag_plus < 15 ~ "High",
          qc_overall == TRUE ~ "Medium",
          TRUE ~ "Low"
        )
      )
  } else {
    samples_df <- samples_df %>%
      mutate(
        !!paste0(prefix, "_confidence") := dplyr::case_when(
          !!sym(paste0(prefix, "_is_discordant")) ~ "Low",
          !!sym(paste0(prefix, "_n_tests")) >= 2 ~ "High",
          TRUE ~ "Medium"
        )
      )
  }

  # Recombine with controls
  if (nrow(controls_df) > 0) {
    # Add NA consolidation columns to controls
    consol_cols <- setdiff(names(samples_df), names(controls_df))
    for (col in consol_cols) {
      controls_df[[col]] <- NA
    }
    result_df <- bind_rows(samples_df, controls_df)
  } else {
    result_df <- samples_df
  }

  if (!include_all_runs) {
    result_df <- get_elisa_sample_summary(result_df, sample_id_cols, prefix)
  }

  return(result_df)
}

#' Get one row per sample for ELISA
#' @export
get_elisa_sample_summary <- function(elisa_df, sample_id_cols, prefix) {

  consol_cols <- c(
    paste0(prefix, "_status_final"),
    paste0(prefix, "_status_normalized"),
    paste0(prefix, "_n_tests"),
    paste0(prefix, "_is_discordant"),
    paste0(prefix, "_is_retest"),
    paste0(prefix, "_confidence"),
    paste0(prefix, "_resolution_detail")
  )
  consol_cols <- consol_cols[consol_cols %in% names(elisa_df)]

  keep_cols <- c("elisa_type", "HealthZone", "HealthArea", "Village")
  keep_cols <- keep_cols[keep_cols %in% names(elisa_df)]

  elisa_df %>%
    filter(sample_type == "sample" | is.na(sample_type)) %>%
    group_by(across(all_of(sample_id_cols))) %>%
    summarise(
      across(all_of(consol_cols), first),
      across(all_of(keep_cols), first),
      mean_PP_percent = mean(PP_percent, na.rm = TRUE),
      mean_DOD = mean(DOD, na.rm = TRUE),
      test_dates = paste(unique(na.omit(plate_date)), collapse = ", "),
      .groups = "drop"
    )
}

#' Get discordance report for ELISA
#' @export
get_elisa_discordance_report <- function(elisa_df, sample_id_cols, prefix) {

  discordant_col <- paste0(prefix, "_is_discordant")

  if (!discordant_col %in% names(elisa_df)) {
    warning("Run consolidate_elisa_results first")
    return(tibble())
  }

  elisa_df %>%
    filter(!!sym(discordant_col) == TRUE) %>%
    group_by(across(all_of(sample_id_cols))) %>%
    summarise(
      n_tests = first(!!sym(paste0(prefix, "_n_tests"))),
      final_status = first(!!sym(paste0(prefix, "_status_final"))),
      all_results = paste(!!sym(paste0(prefix, "_status_normalized")), collapse = " | "),
      all_PP = paste(round(PP_percent, 1), collapse = " | "),
      all_dates = paste(unique(na.omit(plate_date)), collapse = " | "),
      .groups = "drop"
    )
}

#' Add consolidation to ELISA pipeline output
#' @export
add_elisa_consolidation <- function(elisa_result, elisa_type = "PE") {

  if (is.null(elisa_result$interpreted_data)) {
    return(elisa_result)
  }

  prefix <- paste0("elisa_", tolower(elisa_type))

  elisa_result$interpreted_data <- consolidate_elisa_results(
    elisa_result$interpreted_data,
    elisa_type = elisa_type,
    include_all_runs = TRUE
  )

  elisa_result$discordance_summary <- get_elisa_discordance_report(
    elisa_result$interpreted_data,
    sample_id_cols = c("numero_labo", "code_barres_kps"),
    prefix = prefix
  )

  return(elisa_result)
}
