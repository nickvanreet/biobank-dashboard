# ==============================================================================
# MIC qPCR - STEP 4: OUTPUT FORMATTING
# ==============================================================================
# Formats results into standardized structure for dashboard compatibility
# ==============================================================================

#' Format MIC Output
#'
#' @param interpretation_data Output from interpret_mic()
#' @param qc_data Output from qc_mic() (for control flags)
#' @param ingestion_data Output from ingest_mic() (for metadata)
#' @return List with standardized output structure:
#'   - sample_summary: per-sample final calls
#'   - replicate_data: per-replicate decisions
#'   - cq_data: all Cq values with interpretations
#'   - qc_flags: control validation results
#'   - run_settings: metadata
#'   - thresholds: cutoffs used
#'   - cutoffs: complete QC settings
#' @export
format_mic_output <- function(interpretation_data, qc_data, ingestion_data) {
  list(
    sample_summary = interpretation_data$sample_summary,
    replicate_data = interpretation_data$replicate_decisions,
    cq_data        = qc_data$cq_data_qc,
    qc_flags       = qc_data$qc_flags,
    run_settings   = ingestion_data$run_settings,
    thresholds     = ingestion_data$thresholds,
    cutoffs        = qc_data$qc_settings
  )
}

#' Process Single MIC File (Wrapper)
#'
#' @param file_path Path to BioMérieux Excel file
#' @param qc_settings QC settings (uses defaults if NULL)
#' @param verbose Logical, print diagnostic messages
#' @return Formatted output from format_mic_output()
#' @export
process_mic_file <- function(file_path, qc_settings = NULL, verbose = FALSE) {
  if (verbose) {
    cat(strrep("=", 70), "\n")
    cat("MIC qPCR MODULAR PIPELINE\n")
    cat(strrep("=", 70), "\n")
  }

  # Step 1: Ingest
  ingested <- ingest_mic(file_path, verbose = verbose)

  # Step 2: QC
  qc_result <- qc_mic(ingested, qc_settings = qc_settings, verbose = verbose)

  # Step 3: Interpret
  interpreted <- interpret_mic(qc_result, verbose = verbose)

  # Step 4: Format output
  output <- format_mic_output(interpreted, qc_result, ingested)

  if (verbose) {
    cat(strrep("=", 70), "\n")
    cat("PIPELINE COMPLETE\n")
    cat(strrep("=", 70), "\n\n")
  }

  output
}

#' Export MIC Results to Excel
#'
#' @param results Output from process_mic_file()
#' @param output_file Path for Excel output
#' @export
export_mic_to_excel <- function(results, output_file) {
  require(writexl)
  sheets <- list(
    "Sample Summary"    = results$sample_summary,
    "Replicate Details" = results$replicate_data,
    "All Cq Values"     = results$cq_data,
    "QC Flags"          = results$qc_flags
  )
  writexl::write_xlsx(sheets, output_file)
  cat("✓ Results exported to:", output_file, "\n")
}

#' Generate MIC Report Summary
#'
#' @param results Output from process_mic_file()
#' @return Character vector with report lines
#' @export
generate_mic_report <- function(results) {
  report <- c(
    strrep("=", 70),
    "MIC qPCR ANALYSIS REPORT",
    strrep("=", 70),
    "",
    "SUMMARY",
    strrep("-", 70),
    sprintf("Total samples: %d", nrow(results$sample_summary))
  )

  summary_counts <- results$sample_summary %>%
    dplyr::count(final_category, .drop = FALSE)

  for (i in seq_len(nrow(summary_counts))) {
    report <- c(report,
                sprintf("  %s: %d",
                        summary_counts$final_category[i],
                        summary_counts$n[i]))
  }

  report <- c(report, "", "CONTROL ISSUES", strrep("-", 70))
  ctrl <- results$sample_summary %>%
    dplyr::filter(final_category %in% c("control_ok", "control_fail")) %>%
    dplyr::arrange(Name)

  if (nrow(ctrl) > 0) {
    for (i in seq_len(nrow(ctrl))) {
      report <- c(report,
                  sprintf("%-15s  %s",
                          ctrl$Name[i],
                          ctrl$quality_flag[i]))
    }
  } else {
    report <- c(report, "  None")
  }

  report <- c(report, "", "RNA PRESERVATION ISSUES", strrep("-", 70))
  pres <- results$sample_summary %>%
    dplyr::filter(rna_preservation %in% c("Severe RNA loss (no RNAseP-RNA)",
                                          "Poor RNA preservation",
                                          "Moderate RNA loss")) %>%
    dplyr::arrange(dplyr::desc(rna_preservation))

  if (nrow(pres) > 0) {
    for (i in seq_len(nrow(pres))) {
      report <- c(report,
                  sprintf("%-15s  %s  (ΔCq=%.2f)",
                          pres$Name[i], pres$rna_preservation[i], pres$avg_preservation_delta[i]))
    }
  } else {
    report <- c(report, "  None")
  }

  report <- c(report, "", strrep("=", 70))
  report
}
