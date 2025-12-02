# =============================================================================
# iELISA Interpretation Module (STEP 3)
# Calculates % inhibition using two formulas and classifies samples
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# INHIBITION CALCULATION
# =============================================================================

#' Calculate % inhibition using Formula 1 (NEG-based, per spec)
#' Formula 1: % inhibition = 100 × (1 - OD_sample / OD_NEG)
#'
#' This is the OFFICIAL formula from the iELISA spec (section 6.2)
#'
#' @param od_sample Sample OD value
#' @param od_neg Negative control mean OD
#' @return Percent inhibition
calc_inhibition_f1 <- function(od_sample, od_neg) {
  if (is.na(od_sample) || is.na(od_neg) || od_neg == 0) {
    return(NA_real_)
  }
  100 * (1 - od_sample / od_neg)
}

#' Calculate % inhibition using Formula 2 (NEG-POS normalized)
#' Formula 2: % inhibition = 100 × (OD_NEG - OD_sample) / (OD_NEG - OD_POS)
#'
#' This formula normalizes between NEG (0% inhibition) and POS (100% inhibition)
#' Useful for comparison and validation
#'
#' @param od_sample Sample OD value
#' @param od_neg Negative control mean OD
#' @param od_pos Positive control mean OD
#' @return Percent inhibition
calc_inhibition_f2 <- function(od_sample, od_neg, od_pos) {
  if (is.na(od_sample) || is.na(od_neg) || is.na(od_pos)) {
    return(NA_real_)
  }
  if (od_neg == od_pos) {
    return(NA_real_)  # Avoid division by zero
  }
  100 * (od_neg - od_sample) / (od_neg - od_pos)
}

# =============================================================================
# STEP 3: INTERPRETATION
# =============================================================================

#' Calculate % inhibition for all samples using both formulas
#' @param samples Sample data with OD values
#' @param controls Control data with validated means
#' @return Samples with inhibition percentages added
calculate_sample_inhibition <- function(samples, controls) {
  samples %>%
    mutate(
      # Formula 1 (NEG-based - OFFICIAL)
      pct_inh_f1_13 = purrr::map2_dbl(
        OD_L13,
        controls$OD_L13_neg_mean,
        calc_inhibition_f1
      ),
      pct_inh_f1_15 = purrr::map2_dbl(
        OD_L15,
        controls$OD_L15_neg_mean,
        calc_inhibition_f1
      ),

      # Formula 2 (NEG-POS normalized)
      pct_inh_f2_13 = purrr::pmap_dbl(
        list(OD_L13, controls$OD_L13_neg_mean, controls$OD_L13_pos_mean),
        calc_inhibition_f2
      ),
      pct_inh_f2_15 = purrr::pmap_dbl(
        list(OD_L15, controls$OD_L15_neg_mean, controls$OD_L15_pos_mean),
        calc_inhibition_f2
      ),

      # Calculate formula difference (QC metric)
      diff_f1_f2_13 = abs(pct_inh_f1_13 - pct_inh_f2_13),
      diff_f1_f2_15 = abs(pct_inh_f1_15 - pct_inh_f2_15),

      # Flag large discrepancies between formulas (> 25% difference)
      qc_formula_agree_13 = diff_f1_f2_13 < 25 | is.na(diff_f1_f2_13),
      qc_formula_agree_15 = diff_f1_f2_15 < 25 | is.na(diff_f1_f2_15)
    )
}

#' Classify samples as positive/negative
#' Per spec (section 6.3):
#' - Positive: if % inhibition of LiTat 1.3 AND/OR LiTat 1.5 is ≥ 30%
#' - Negative: if % inhibition of BOTH LiTat 1.3 AND LiTat 1.5 is < 30%
#'
#' We use Formula 1 (NEG-based) by default as it's the official spec formula
#'
#' @param samples_with_inhibition Samples with inhibition calculated
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Which formula to use for classification ("f1" or "f2", default: "f1")
#' @return Samples with positivity classification
classify_sample_positivity <- function(samples_with_inhibition, threshold = 30, formula = "f1") {
  # Select inhibition columns based on formula choice
  if (formula == "f1") {
    samples_with_inhibition <- samples_with_inhibition %>%
      mutate(
        positive_L13 = pct_inh_f1_13 >= threshold,
        positive_L15 = pct_inh_f1_15 >= threshold
      )
  } else {  # f2
    samples_with_inhibition <- samples_with_inhibition %>%
      mutate(
        positive_L13 = pct_inh_f2_13 >= threshold,
        positive_L15 = pct_inh_f2_15 >= threshold
      )
  }

  # Overall positivity: positive if EITHER antigen is positive
  samples_with_inhibition %>%
    mutate(
      sample_positive = positive_L13 | positive_L15,
      # Classification string
      status_final = case_when(
        is.na(positive_L13) & is.na(positive_L15) ~ "Invalid",
        sample_positive ~ "Positive",
        TRUE ~ "Negative"
      )
    )
}

#' Perform iELISA interpretation
#' @param qc_data Output from qc_ielisa()
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Formula to use for classification ("f1" or "f2", default: "f1")
#' @return List with metadata, controls, and interpreted samples
#' @export
interpret_ielisa <- function(qc_data, threshold = 30, formula = "f1") {
  cat(sprintf("🔬 INTERPRETATION (iELISA): %s\n", qc_data$metadata$plate_id))

  # Calculate inhibition using both formulas
  samples_with_inhibition <- calculate_sample_inhibition(
    qc_data$samples,
    qc_data$controls
  )

  # Classify positivity
  samples_classified <- classify_sample_positivity(
    samples_with_inhibition,
    threshold = threshold,
    formula = formula
  )

  # Report summary
  n_positive <- sum(samples_classified$sample_positive, na.rm = TRUE)
  n_negative <- sum(!samples_classified$sample_positive, na.rm = TRUE)
  n_invalid <- sum(is.na(samples_classified$sample_positive))

  cat(sprintf("  ✓ Formula used: %s (threshold: %.0f%%)\n",
              ifelse(formula == "f1", "F1 (NEG-based)", "F2 (NEG-POS normalized)"),
              threshold))
  cat(sprintf("  ✓ Results: %d positive, %d negative, %d invalid\n",
              n_positive, n_negative, n_invalid))

  # Check for formula disagreements
  n_disagree_13 <- sum(!samples_classified$qc_formula_agree_13, na.rm = TRUE)
  n_disagree_15 <- sum(!samples_classified$qc_formula_agree_15, na.rm = TRUE)
  if (n_disagree_13 > 0 || n_disagree_15 > 0) {
    cat(sprintf("  ⚠ Formula disagreements: L13=%d, L15=%d samples\n",
                n_disagree_13, n_disagree_15))
  }

  return(list(
    metadata = qc_data$metadata,
    controls = qc_data$controls,
    samples = samples_classified
  ))
}
