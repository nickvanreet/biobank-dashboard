# ==============================================================================
# qPCR Analysis for BioMérieux MIC (MODULAR ARCHITECTURE)
# ------------------------------------------------------------------------------
# Uses modular 4-step pipeline from R/modules/mic/:
#   Step 1: ingest_mic.R  - Extract Cq values from BioMérieux Excel
#   Step 2: qc_mic.R      - Apply cutoffs and validate controls
#   Step 3: interpret_mic.R - Decision tree and RNA preservation
#   Step 4: output_mic.R  - Format standardized output
#
# This script provides the analyze_qpcr() wrapper function for backward
# compatibility. It sources and uses the modular pipeline functions.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
})

# ==============================================================================
# SOURCE MODULAR PIPELINE COMPONENTS
# ==============================================================================
# These files define the core pipeline functions and their dependencies

# Source order matters - qc_mic.R defines coerce_cutoff_numeric/sanitize_cutoffs
# which are used by other modules
source("R/modules/mic/qc_mic.R")
source("R/modules/mic/ingest_mic.R")
source("R/modules/mic/interpret_mic.R")
source("R/modules/mic/output_mic.R")

# ==============================================================================
# CONFIGURATION & CONSTANTS
# ==============================================================================

DEFAULT_CUTOFFS <- list(
  "177T"       = list(positive = 35, negative = 40),  # DNA target
  "18S2"       = list(positive = 35, negative = 40),  # RNA target
  "RNAseP_DNA" = list(positive = 32, negative = 999),
  "RNAseP_RNA" = list(positive = 30, negative = 999)
)

# Thresholds for RNA preservation ΔCq
PRESERVATION_DELTA_GOOD <- 5   # ≤5 => Good
PRESERVATION_DELTA_WARN <- 8   # >5 & ≤8 => Moderate ; >8 => Poor

mic_log <- function(..., .sep = "", .appendLF = TRUE) {
  if (isTRUE(getOption("mic.verbose", FALSE))) {
    base::message(..., sep = .sep, appendLF = .appendLF)
  }
  invisible(NULL)
}

# ==============================================================================
# MAIN PIPELINE WRAPPER (Backward Compatibility)
# ==============================================================================

analyze_qpcr <- function(micrun_file,
                         rnasep_rna_cutoff = DEFAULT_CUTOFFS$RNAseP_RNA$positive,
                         cutoffs = DEFAULT_CUTOFFS,
                         verbose = TRUE) {

  # Sanitize inputs
  cutoffs <- sanitize_cutoffs(cutoffs)
  rnasep_rna_cutoff <- coerce_cutoff_numeric(rnasep_rna_cutoff)

  # Prepare QC settings for modular pipeline
  qc_settings <- list(
    thresholds = cutoffs,
    late_window = c(38, 40),
    delta_rp_limit = 8,
    delta_good = PRESERVATION_DELTA_GOOD,
    delta_warn = PRESERVATION_DELTA_WARN,
    min_positive_reps = 2
  )

  # Call modular pipeline (defined in R/modules/mic/output_mic.R)
  result <- process_mic_file(micrun_file, qc_settings = qc_settings, verbose = verbose)

  # Return in expected format for backward compatibility
  list(
    sample_summary = result$sample_summary,
    replicate_data = result$replicate_data,
    cq_data        = result$cq_data,
    run_settings   = result$run_settings,
    thresholds     = result$thresholds,
    cutoffs        = result$cutoffs
  )
}

# ==============================================================================
# BACKWARD COMPATIBILITY FUNCTIONS
# ==============================================================================
# These functions provide the old interface that other modules may still use

# Alias for the modular ingest function
extract_cq_values <- function(micrun_file) {
  ingest_mic(micrun_file, verbose = FALSE)
}

# Alias for QC interpretation
apply_interpretation <- function(cq_data, cutoffs = DEFAULT_CUTOFFS) {
  # This mimics the old behavior - apply cutoffs and return interpreted data
  cutoffs <- sanitize_cutoffs(cutoffs)

  if (!nrow(cq_data)) {
    return(dplyr::mutate(cq_data, interpretation = character()))
  }

  cq_data %>%
    mutate(
      interpretation = purrr::map2_chr(
        target, Cq,
        function(tgt, cq) {
          cq_num <- suppressWarnings(as.numeric(cq))
          if (is.na(cq_num) || cq_num < 0 || is.infinite(cq_num)) return("Negative")
          if (!tgt %in% names(cutoffs)) return("Unknown")

          tgt_cutoffs <- cutoffs[[tgt]]
          pos_cutoff <- coerce_cutoff_numeric(tgt_cutoffs$positive)
          neg_cutoff <- coerce_cutoff_numeric(tgt_cutoffs$negative)

          if (is.null(pos_cutoff) || is.na(pos_cutoff)) pos_cutoff <- -Inf
          if (is.null(neg_cutoff) || is.na(neg_cutoff)) neg_cutoff <- Inf

          if (cq_num <= pos_cutoff) {
            "Positive"
          } else if (cq_num >= neg_cutoff) {
            "Negative"
          } else {
            "Indeterminate"
          }
        }
      )
    )
}

# These functions are now defined in modular files but may be called directly
# by other parts of the codebase. Re-export them here for compatibility.
# (They're already loaded from the modular files, so this just ensures
# they're explicitly available in this namespace)

# From qc_mic.R:
# - coerce_cutoff_numeric (already loaded)
# - sanitize_cutoffs (already loaded)

# From ingest_mic.R:
# - find_column (already loaded)
# - pick_cq_col (already loaded)
# - pick_name_col (already loaded)

# From interpret_mic.R:
# - summarize_by_replicate (already loaded)
# - summarize_by_sample (already loaded)
# - apply_trypanozoon_decision (already loaded)
# - compute_nuc_acid_quality (already loaded)
# - evaluate_rna_preservation (already loaded)

# ==============================================================================
# EXPORT / REPORT FUNCTIONS
# ==============================================================================

export_to_excel <- function(results, output_file) {
  # Use the modular version if available, otherwise fall back
  if (exists("export_mic_to_excel")) {
    export_mic_to_excel(results, output_file)
  } else {
    require(writexl)
    sheets <- list(
      "Sample Summary"    = results$sample_summary,
      "Replicate Details" = results$replicate_data,
      "All Cq Values"     = results$cq_data
    )
    writexl::write_xlsx(sheets, output_file)
    cat("✓ Results exported to:", output_file, "\n")
  }
}

generate_report <- function(results) {
  # Use the modular version if available, otherwise fall back
  if (exists("generate_mic_report")) {
    generate_mic_report(results)
  } else {
    report <- c(
      strrep("=", 70),
      "qPCR ANALYSIS REPORT",
      strrep("=", 70),
      "",
      "SUMMARY",
      strrep("-", 70),
      sprintf("Total samples: %d", nrow(results$sample_summary))
    )

    summary_counts <- results$sample_summary %>%
      count(final_category, .drop = FALSE)

    for (i in seq_len(nrow(summary_counts))) {
      report <- c(report,
                  sprintf("  %s: %d",
                          summary_counts$final_category[i],
                          summary_counts$n[i]))
    }

    report <- c(report, "", strrep("=", 70))
    report
  }
}

# ==============================================================================
# END
# ==============================================================================
