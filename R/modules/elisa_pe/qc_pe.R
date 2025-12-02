# =============================================================================
# ELISA-PE QC Module (STEP 2)
# Validates controls and calculates QC metrics
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Note: Shared utilities (utils_elisa.R) should be sourced by the caller

# =============================================================================
# STEP 2: QC VALIDATION
# =============================================================================

#' Summarize OD replicates for each sample/control
#' @param wells_long Long format data from ingestion
#' @param qc_settings QC settings list
#' @return Tibble with summarized OD values and CV metrics
summarize_od_replicates <- function(wells_long, qc_settings) {
  summarize_entry <- function(df_group, group_keys) {
    plus <- df_group$od[df_group$ag_state == "Ag_plus"]
    zero <- df_group$od[df_group$ag_state == "Ag0"]

    if (length(plus) < 2) plus <- c(plus, rep(NA_real_, 2 - length(plus)))
    if (length(zero) < 2) zero <- c(zero, rep(NA_real_, 2 - length(zero)))

    Ag_plus_1 <- plus[1]
    Ag_plus_2 <- plus[2]
    Ag0_1 <- zero[1]
    Ag0_2 <- zero[2]

    mean_Ag_plus <- mean(plus, na.rm = TRUE)
    mean_Ag0 <- mean(zero, na.rm = TRUE)
    DOD <- calculate_dod(mean_Ag_plus, mean_Ag0)

    cv_Ag_plus <- calculate_cv(Ag_plus_1, Ag_plus_2)
    cv_Ag0 <- calculate_cv(Ag0_1, Ag0_2)

    qc_Ag_plus <- if(is.na(cv_Ag_plus)) NA else cv_Ag_plus <= qc_settings$cv_max_ag_plus
    qc_Ag0 <- if(is.na(cv_Ag0)) NA else cv_Ag0 <= qc_settings$cv_max_ag0
    qc_overall <- if(is.na(qc_Ag_plus) || is.na(qc_Ag0)) NA else (qc_Ag_plus & qc_Ag0)

    tibble(
      Ag_plus_1 = Ag_plus_1,
      Ag_plus_2 = Ag_plus_2,
      Ag0_1 = Ag0_1,
      Ag0_2 = Ag0_2,
      mean_Ag_plus = mean_Ag_plus,
      mean_Ag0 = mean_Ag0,
      DOD = DOD,
      cv_Ag_plus = cv_Ag_plus,
      cv_Ag0 = cv_Ag0,
      qc_Ag_plus = qc_Ag_plus,
      qc_Ag0 = qc_Ag0,
      qc_overall = qc_overall
    )
  }

  wells_long %>%
    group_by(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, plate_date, elisa_type) %>%
    group_modify(~ summarize_entry(.x, .y)) %>%
    ungroup()
}

#' Validate plate-level controls
#' @param df_summary Summarized data with controls
#' @param qc_settings QC settings list
#' @return Data frame with plate validation flags
validate_plate_controls <- function(df_summary, qc_settings) {
  df_summary %>%
    group_by(plate_id, plate_num) %>%
    mutate(
      # Identify control types
      control_type = identify_control_type(sample, sample_code),

      # Calculate control means per plate
      PC_DOD = mean(DOD[control_type == "positive_control"], na.rm = TRUE),
      NC_DOD = mean(DOD[control_type == "negative_control"], na.rm = TRUE),

      positive_control_od = mean(mean_Ag_plus[control_type == "positive_control"], na.rm = TRUE),
      negative_control_od = mean(mean_Ag_plus[control_type == "negative_control"], na.rm = TRUE),

      # Handle NaN (no controls found)
      positive_control_od = ifelse(is.nan(positive_control_od), NA_real_, positive_control_od),
      negative_control_od = ifelse(is.nan(negative_control_od), NA_real_, negative_control_od),

      # Validate against thresholds
      plate_positive_control_valid = ifelse(
        is.na(positive_control_od),
        NA,
        positive_control_od >= qc_settings$pos_control_min_od &
          positive_control_od <= qc_settings$pos_control_max_od
      ),

      plate_negative_control_valid = ifelse(
        is.na(negative_control_od),
        NA,
        negative_control_od < qc_settings$neg_control_max_od
      ),

      # Overall plate validity
      plate_valid = plate_positive_control_valid & plate_negative_control_valid
    ) %>%
    ungroup()
}

#' Main ELISA-PE QC function
#' @param ingestion_result Result from ingest_elisa_pe()
#' @param qc_settings QC settings list (default: elisa_default_qc_settings())
#' @return List with:
#'   - qc_summary: Data frame with QC metrics and validation flags
#'   - run_validity_table: Summary of plate validity
#'   - valid_runs: List of valid plate IDs
#'   - invalid_runs: List of invalid plate IDs
#' @export
qc_elisa_pe <- function(ingestion_result, qc_settings = elisa_default_qc_settings()) {
  cat(sprintf("🔍 QC VALIDATION (PE): %s\n", ingestion_result$metadata$plate_id))

  # Summarize OD replicates
  df_summary <- summarize_od_replicates(
    ingestion_result$wells_long,
    qc_settings
  )

  cat(sprintf("  ✓ Summarized %d entries (samples + controls)\n", nrow(df_summary)))

  # Validate plate controls
  qc_summary <- validate_plate_controls(df_summary, qc_settings)

  # Create run validity table
  run_validity_table <- qc_summary %>%
    distinct(plate_id, plate_num, plate_valid,
             positive_control_od, negative_control_od,
             plate_positive_control_valid, plate_negative_control_valid) %>%
    mutate(
      qc_reason = case_when(
        is.na(plate_valid) ~ "No controls found",
        !plate_positive_control_valid ~ "Positive control OD out of range",
        !plate_negative_control_valid ~ "Negative control OD out of range",
        plate_valid ~ "Pass",
        TRUE ~ "Unknown"
      )
    )

  # Identify valid and invalid runs
  valid_runs <- run_validity_table %>%
    filter(plate_valid == TRUE) %>%
    pull(plate_id)

  invalid_runs <- run_validity_table %>%
    filter(plate_valid == FALSE | is.na(plate_valid)) %>%
    pull(plate_id)

  cat(sprintf("  ✓ QC complete: %d valid plates, %d invalid plates\n",
              length(valid_runs), length(invalid_runs)))

  if (length(invalid_runs) > 0) {
    cat(sprintf("  ⚠ Invalid plates: %s\n", paste(invalid_runs, collapse = ", ")))
  }

  return(list(
    qc_summary = qc_summary,
    qc_settings = qc_settings,
    run_validity_table = run_validity_table,
    valid_runs = valid_runs,
    invalid_runs = invalid_runs
  ))
}
