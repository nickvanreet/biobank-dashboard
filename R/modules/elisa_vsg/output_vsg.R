# =============================================================================
# ELISA-VSG Output Module (STEP 4)
# Creates standardized tidy output format for dashboard consumption
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# =============================================================================
# STEP 4: CLEAN OUTPUT
# =============================================================================

#' Create standardized ELISA-VSG output
#' Follows the unified format specification:
#'   sample_id, test_type, run_id, run_valid, sample_valid, status_final,
#'   n_runs, metric_1, metric_2, qc_flags, file_origin, test_date
#'
#' @param interpretation_result Result from interpret_elisa_vsg()
#' @return Tibble in standardized format
#' @export
output_elisa_vsg <- function(interpretation_result) {
  cat(sprintf("📤 OUTPUT (VSG): Creating standardized format\n"))

  interpreted_data <- interpretation_result$interpreted_data

  # Create standardized output
  results_clean <- interpreted_data %>%
    mutate(
      # Unified sample identifier (prefer barcode, fallback to numero_labo)
      sample_id = coalesce(code_barres_kps, numero_labo, sample),

      # Test type
      test_type = "ELISA-VSG",

      # Run identifier (plate_id + plate_num)
      run_id = paste0(plate_id, "_P", plate_num),

      # Validity flags
      run_valid = coalesce(plate_valid, FALSE),
      sample_valid = coalesce(qc_overall, FALSE),

      # Metrics
      metric_1 = PP_percent,        # Primary metric: PP%
      metric_2 = DOD,               # Secondary metric: DOD
      metric_1_name = "PP%",
      metric_2_name = "DOD",

      # File origin
      file_origin = plate_id,

      # Test date
      test_date = plate_date
    ) %>%
    select(
      # Core identifiers
      sample_id, sample_type, test_type, run_id,

      # Validity
      run_valid, sample_valid,

      # Status
      status_final, status_raw,

      # Retest tracking
      n_runs,

      # Metrics
      metric_1, metric_1_name, metric_2, metric_2_name,
      PP_percent, DOD,                      # Keep original names for compatibility

      # QC flags
      qc_flags,

      # OD values (for detailed analysis)
      mean_Ag_plus, mean_Ag0,
      Ag_plus_1, Ag_plus_2, Ag0_1, Ag0_2,
      cv_Ag_plus, cv_Ag0,

      # Control values (for QC plots)
      positive_control_od, negative_control_od,
      PC_DOD, NC_DOD,

      # Metadata
      file_origin, test_date, plate_id, plate_num,
      numero_labo, code_barres_kps, sample, sample_code,
      elisa_type
    )

  cat(sprintf("  ✓ Created standardized output: %d rows\n", nrow(results_clean)))

  # Summary statistics
  if (nrow(results_clean) > 0) {
    samples_only <- results_clean %>% filter(sample_type == "sample")
    if (nrow(samples_only) > 0) {
      n_positive <- sum(samples_only$status_final == "Positive", na.rm = TRUE)
      n_negative <- sum(samples_only$status_final == "Negative", na.rm = TRUE)
      n_borderline <- sum(samples_only$status_final == "Borderline", na.rm = TRUE)
      n_invalid <- sum(samples_only$status_final == "Invalid", na.rm = TRUE)

      cat(sprintf("  ✓ Summary: %d Positive, %d Negative, %d Borderline, %d Invalid\n",
                  n_positive, n_negative, n_borderline, n_invalid))
    }
  }

  return(results_clean)
}

#' Process ELISA-VSG file end-to-end
#' Convenience function that runs all 4 steps
#'
#' @param file_path Path to ELISA-VSG Excel file
#' @param qc_settings QC settings (default: elisa_default_qc_settings())
#' @return List with:
#'   - results_clean: Standardized output
#'   - metadata: Processing metadata
#'   - qc_summary: QC details
#'   - sample_summary: Per-sample summary
#' @export
process_elisa_vsg_file <- function(file_path,
                                     qc_settings = elisa_default_qc_settings()) {
  cat(sprintf("\n═══════════════════════════════════════════════\n"))
  cat(sprintf("Processing ELISA-VSG: %s\n", basename(file_path)))
  cat(sprintf("═══════════════════════════════════════════════\n"))

  # STEP 1: Ingestion
  ingestion_result <- ingest_elisa_vsg(file_path)

  # STEP 2: QC
  qc_result <- qc_elisa_vsg(ingestion_result, qc_settings)

  # STEP 3: Interpretation
  interpretation_result <- interpret_elisa_vsg(qc_result)

  # STEP 4: Output
  results_clean <- output_elisa_vsg(interpretation_result)

  cat(sprintf("═══════════════════════════════════════════════\n"))
  cat(sprintf("✅ ELISA-VSG processing complete\n"))
  cat(sprintf("═══════════════════════════════════════════════\n\n"))

  return(list(
    results_clean = results_clean,
    metadata = ingestion_result$metadata,
    qc_summary = qc_result$run_validity_table,
    sample_summary = interpretation_result$sample_summary
  ))
}
