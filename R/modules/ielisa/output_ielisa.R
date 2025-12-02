# =============================================================================
# iELISA Output Module (STEP 4)
# Formats data into standardized output structure
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# STEP 4: OUTPUT FORMATTING
# =============================================================================

#' Format iELISA results into standardized output structure
#' Creates a clean, analysis-ready format compatible with the dashboard
#'
#' @param interpretation_data Output from interpret_ielisa()
#' @return Tibble with standardized columns
#' @export
format_ielisa_output <- function(interpretation_data) {
  metadata <- interpretation_data$metadata
  controls <- interpretation_data$controls
  samples <- interpretation_data$samples

  # Create output combining plate info, controls, and sample results
  output <- samples %>%
    mutate(
      # Plate identifiers
      file = metadata$file_list,
      plate_id = metadata$plate_id,
      plate_date = metadata$plate_date,
      PlateDate = metadata$plate_date,  # Alias for compatibility
      test_type = "iELISA",

      # Add control data (broadcast to all rows for easy access)
      # LiTat 1.3 controls
      OD_L13_neg_1 = controls$OD_L13_neg_1,
      OD_L13_neg_2 = controls$OD_L13_neg_2,
      OD_L13_pos_1 = controls$OD_L13_pos_1,
      OD_L13_pos_2 = controls$OD_L13_pos_2,
      OD_L13_neg_mean = controls$OD_L13_neg_mean,
      OD_L13_pos_mean = controls$OD_L13_pos_mean,
      OD_L13_neg_cv = controls$OD_L13_neg_cv,
      OD_L13_pos_cv = controls$OD_L13_pos_cv,
      pct_inh_pos_13 = controls$pct_inh_pos_13,
      plate_valid_L13 = controls$plate_valid_L13,

      # LiTat 1.5 controls
      OD_L15_neg_1 = controls$OD_L15_neg_1,
      OD_L15_neg_2 = controls$OD_L15_neg_2,
      OD_L15_pos_1 = controls$OD_L15_pos_1,
      OD_L15_pos_2 = controls$OD_L15_pos_2,
      OD_L15_neg_mean = controls$OD_L15_neg_mean,
      OD_L15_pos_mean = controls$OD_L15_pos_mean,
      OD_L15_neg_cv = controls$OD_L15_neg_cv,
      OD_L15_pos_cv = controls$OD_L15_pos_cv,
      pct_inh_pos_15 = controls$pct_inh_pos_15,
      plate_valid_L15 = controls$plate_valid_L15,

      # Aliases for consistency with other modules
      LabID = numero_labo,
      Barcode = code_barres_kps
    )

  # Reorder columns for logical grouping
  output <- output %>%
    select(
      # File & plate info
      file, plate_id, plate_date, PlateDate, test_type,

      # Sample identifiers
      SampleIndex, numero_labo, code_barres_kps, LabID, Barcode,

      # Sample OD values
      OD_L13, OD_L15, well_L13, well_L15, ExcelRow, Col13, Col15,

      # Sample inhibition (both formulas)
      pct_inh_f1_13, pct_inh_f1_15,
      pct_inh_f2_13, pct_inh_f2_15,
      diff_f1_f2_13, diff_f1_f2_15,
      qc_formula_agree_13, qc_formula_agree_15,

      # Sample classification
      positive_L13, positive_L15, sample_positive, status_final,

      # Control OD values (LiTat 1.3)
      OD_L13_neg_1, OD_L13_neg_2, OD_L13_pos_1, OD_L13_pos_2,
      OD_L13_neg_mean, OD_L13_pos_mean,
      OD_L13_neg_cv, OD_L13_pos_cv,
      pct_inh_pos_13, plate_valid_L13,

      # Control OD values (LiTat 1.5)
      OD_L15_neg_1, OD_L15_neg_2, OD_L15_pos_1, OD_L15_pos_2,
      OD_L15_neg_mean, OD_L15_pos_mean,
      OD_L15_neg_cv, OD_L15_pos_cv,
      pct_inh_pos_15, plate_valid_L15
    )

  return(output)
}

#' Process complete iELISA file through 4-step pipeline
#' Wrapper function that runs all 4 steps in sequence
#'
#' @param file_path Path to iELISA Excel file
#' @param qc_settings QC settings (optional, uses defaults if NULL)
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Formula to use for classification ("f1" or "f2", default: "f1")
#' @return Standardized output tibble
#' @export
process_ielisa_file <- function(file_path,
                                 qc_settings = NULL,
                                 threshold = 30,
                                 formula = "f1") {
  # Source required modules (if not already loaded)
  if (!exists("ingest_ielisa")) {
    source(file.path("R", "modules", "ielisa", "ingest_ielisa.R"), local = TRUE)
  }
  if (!exists("qc_ielisa")) {
    source(file.path("R", "modules", "ielisa", "qc_ielisa.R"), local = TRUE)
  }
  if (!exists("interpret_ielisa")) {
    source(file.path("R", "modules", "ielisa", "interpret_ielisa.R"), local = TRUE)
  }

  # Run 4-step pipeline
  cat(sprintf("\n═══════════════════════════════════════════════════════════\n"))
  cat(sprintf("Processing: %s\n", basename(file_path)))
  cat(sprintf("═══════════════════════════════════════════════════════════\n"))

  step1 <- ingest_ielisa(file_path)
  step2 <- qc_ielisa(step1, qc_settings)
  step3 <- interpret_ielisa(step2, threshold, formula)
  step4 <- format_ielisa_output(step3)

  cat(sprintf("✅ COMPLETE: %d samples processed\n", nrow(step4)))
  cat(sprintf("═══════════════════════════════════════════════════════════\n\n"))

  return(list(
    results_clean = step4,
    metadata = step3$metadata,
    controls = step3$controls
  ))
}
