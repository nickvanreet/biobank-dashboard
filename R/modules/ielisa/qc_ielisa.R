# =============================================================================
# iELISA QC Module (STEP 2)
# Validates control performance and plate quality
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# QC SETTINGS
# =============================================================================

#' Get default iELISA QC settings
#' @return List with QC thresholds
#' @export
ielisa_default_qc_settings <- function() {
  list(
    # Negative control OD range
    neg_od_min = 1.0,
    neg_od_max = 3.1,

    # Positive control OD range (optional, not in spec but useful)
    pos_od_min = 0.3,
    pos_od_max = 0.7,

    # Positive control inhibition (using Formula 1)
    ctrl_inh_min = 30,  # Must be ≥ 30% per spec
    ctrl_inh_max = 100,  # Upper bound (not in spec, but logical)

    # Control CV
    ctrl_cv_max = 20  # Maximum acceptable CV% for controls
  )
}

# =============================================================================
# STEP 2: QC VALIDATION
# =============================================================================

#' Calculate mean and CV for control replicates
#' @param val1 First replicate OD
#' @param val2 Second replicate OD
#' @return List with mean and CV
calc_control_stats <- function(val1, val2) {
  if (is.na(val1) || is.na(val2)) {
    return(list(mean = NA_real_, cv = NA_real_))
  }

  vals <- c(val1, val2)
  m <- mean(vals, na.rm = TRUE)
  s <- sd(vals, na.rm = TRUE)
  cv <- ifelse(m == 0, NA_real_, 100 * s / m)

  list(mean = m, cv = cv)
}

#' Calculate control statistics (means and CVs)
#' @param controls Control OD tibble from ingestion
#' @return Tibble with mean OD and CV for each control
calculate_control_stats <- function(controls) {
  controls %>%
    mutate(
      # LiTat 1.3 stats
      L13_neg_stats = purrr::map2(OD_L13_neg_1, OD_L13_neg_2, calc_control_stats),
      OD_L13_neg_mean = purrr::map_dbl(L13_neg_stats, "mean"),
      OD_L13_neg_cv = purrr::map_dbl(L13_neg_stats, "cv"),

      L13_pos_stats = purrr::map2(OD_L13_pos_1, OD_L13_pos_2, calc_control_stats),
      OD_L13_pos_mean = purrr::map_dbl(L13_pos_stats, "mean"),
      OD_L13_pos_cv = purrr::map_dbl(L13_pos_stats, "cv"),

      # LiTat 1.5 stats
      L15_neg_stats = purrr::map2(OD_L15_neg_1, OD_L15_neg_2, calc_control_stats),
      OD_L15_neg_mean = purrr::map_dbl(L15_neg_stats, "mean"),
      OD_L15_neg_cv = purrr::map_dbl(L15_neg_stats, "cv"),

      L15_pos_stats = purrr::map2(OD_L15_pos_1, OD_L15_pos_2, calc_control_stats),
      OD_L15_pos_mean = purrr::map_dbl(L15_pos_stats, "mean"),
      OD_L15_pos_cv = purrr::map_dbl(L15_pos_stats, "cv")
    ) %>%
    select(-ends_with("_stats"))  # Remove intermediate columns
}

#' Calculate positive control inhibition using Formula 1 (NEG-based)
#' Formula 1: % inhibition = 100 × (1 - OD_POS / OD_NEG)
#'
#' @param od_pos Mean positive control OD
#' @param od_neg Mean negative control OD
#' @return Percent inhibition
calc_pos_control_inhibition <- function(od_pos, od_neg) {
  if (is.na(od_pos) || is.na(od_neg) || od_neg == 0) {
    return(NA_real_)
  }
  100 * (1 - od_pos / od_neg)
}

#' Validate controls against QC thresholds
#' Per spec (section 6.1):
#' 1. NEG control OD: 1.000 < OD < 3.100
#' 2. POS control inhibition: ≥ 30%
#' 3. CV: < 20% (implicit from best practices)
#'
#' @param controls_with_stats Controls with calculated means and CVs
#' @param qc_settings QC threshold settings
#' @return Controls with QC flags added
validate_ielisa_controls <- function(controls_with_stats, qc_settings) {
  controls_with_stats %>%
    mutate(
      # Calculate POS control inhibition (using Formula 1)
      pct_inh_pos_13 = calc_pos_control_inhibition(OD_L13_pos_mean, OD_L13_neg_mean),
      pct_inh_pos_15 = calc_pos_control_inhibition(OD_L15_pos_mean, OD_L15_neg_mean),

      # Validate LiTat 1.3 controls
      neg_ok_13 = OD_L13_neg_mean >= qc_settings$neg_od_min &
                  OD_L13_neg_mean <= qc_settings$neg_od_max,
      pos_inh_ok_13 = pct_inh_pos_13 >= qc_settings$ctrl_inh_min,
      cv_ok_13 = OD_L13_neg_cv <= qc_settings$ctrl_cv_max &
                 OD_L13_pos_cv <= qc_settings$ctrl_cv_max,

      # Overall plate validity for LiTat 1.3
      plate_valid_L13 = neg_ok_13 & pos_inh_ok_13 & cv_ok_13,

      # Validate LiTat 1.5 controls
      neg_ok_15 = OD_L15_neg_mean >= qc_settings$neg_od_min &
                  OD_L15_neg_mean <= qc_settings$neg_od_max,
      pos_inh_ok_15 = pct_inh_pos_15 >= qc_settings$ctrl_inh_min,
      cv_ok_15 = OD_L15_neg_cv <= qc_settings$ctrl_cv_max &
                 OD_L15_pos_cv <= qc_settings$ctrl_cv_max,

      # Overall plate validity for LiTat 1.5
      plate_valid_L15 = neg_ok_15 & pos_inh_ok_15 & cv_ok_15
    )
}

#' Perform iELISA QC validation
#' @param ingestion_data Output from ingest_ielisa()
#' @param qc_settings QC settings (optional, uses defaults if NULL)
#' @return List with updated controls and samples (unchanged)
#' @export
qc_ielisa <- function(ingestion_data, qc_settings = NULL) {
  cat(sprintf("🔍 QC VALIDATION (iELISA): %s\n", ingestion_data$metadata$plate_id))

  if (is.null(qc_settings)) {
    qc_settings <- ielisa_default_qc_settings()
  }

  # Calculate control statistics
  controls_with_stats <- calculate_control_stats(ingestion_data$controls)

  # Validate controls
  controls_validated <- validate_ielisa_controls(controls_with_stats, qc_settings)

  # Report QC results
  cat(sprintf("  ✓ LiTat 1.3 - NEG OD: %.3f (CV: %.1f%%), POS inhibition: %.1f%%\n",
              controls_validated$OD_L13_neg_mean,
              controls_validated$OD_L13_neg_cv,
              controls_validated$pct_inh_pos_13))
  cat(sprintf("  ✓ LiTat 1.5 - NEG OD: %.3f (CV: %.1f%%), POS inhibition: %.1f%%\n",
              controls_validated$OD_L15_neg_mean,
              controls_validated$OD_L15_neg_cv,
              controls_validated$pct_inh_pos_15))
  cat(sprintf("  ✓ Plate validity: L13=%s, L15=%s\n",
              ifelse(controls_validated$plate_valid_L13, "PASS", "FAIL"),
              ifelse(controls_validated$plate_valid_L15, "PASS", "FAIL")))

  return(list(
    metadata = ingestion_data$metadata,
    controls = controls_validated,
    samples = ingestion_data$samples
  ))
}
