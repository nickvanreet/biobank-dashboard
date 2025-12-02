# =============================================================================
# Test Script for Modular ELISA Processing
# Verifies that the new 4-step pipeline works correctly
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
})

# Source the modular processing functions
source("R/modules/elisa_shared/process_elisa_modular.R")

cat("\n═══════════════════════════════════════════════\n")
cat("Testing Modular ELISA Processing Pipeline\n")
cat("═══════════════════════════════════════════════\n\n")

# =============================================================================
# TEST 1: ELISA-PE Processing
# =============================================================================

cat("TEST 1: ELISA-PE Processing\n")
cat("───────────────────────────────────────────────\n")

pe_file <- "data/elisa_pe/250507 ELISA indirect pre-coated plaque (1 plaque).xlsx"

if (file.exists(pe_file)) {
  tryCatch({
    pe_result <- process_elisa_file_modular(pe_file)

    cat("\n✅ PE Processing SUCCESS\n")
    cat("   Rows returned: ", nrow(pe_result), "\n")
    cat("   Test type: ", unique(pe_result$test_type), "\n")
    cat("   Samples: ", sum(pe_result$sample_type == "sample", na.rm = TRUE), "\n")
    cat("   Controls: ", sum(pe_result$sample_type == "control", na.rm = TRUE), "\n")

    if (nrow(pe_result) > 0 && "status_final" %in% names(pe_result)) {
      samples <- pe_result %>% filter(sample_type == "sample")
      if (nrow(samples) > 0) {
        status_counts <- table(samples$status_final)
        cat("   Status distribution: ", paste(names(status_counts), "=", status_counts, collapse = ", "), "\n")
      }
    }
  }, error = function(e) {
    cat("\n❌ PE Processing FAILED\n")
    cat("   Error: ", e$message, "\n")
  })
} else {
  cat("⚠ PE test file not found: ", pe_file, "\n")
}

# =============================================================================
# TEST 2: ELISA-VSG Processing
# =============================================================================

cat("\nTEST 2: ELISA-VSG Processing\n")
cat("───────────────────────────────────────────────\n")

vsg_file <- "data/elisa_vsg/250709 Résultats indirect ELISA vF.6.xlsx"

if (file.exists(vsg_file)) {
  tryCatch({
    vsg_result <- process_elisa_file_modular(vsg_file)

    cat("\n✅ VSG Processing SUCCESS\n")
    cat("   Rows returned: ", nrow(vsg_result), "\n")
    cat("   Test type: ", unique(vsg_result$test_type), "\n")
    cat("   Unique plates: ", n_distinct(vsg_result$plate_num), "\n")
    cat("   Samples: ", sum(vsg_result$sample_type == "sample", na.rm = TRUE), "\n")
    cat("   Controls: ", sum(vsg_result$sample_type == "control", na.rm = TRUE), "\n")

    if (nrow(vsg_result) > 0 && "status_final" %in% names(vsg_result)) {
      samples <- vsg_result %>% filter(sample_type == "sample")
      if (nrow(samples) > 0) {
        status_counts <- table(samples$status_final)
        cat("   Status distribution: ", paste(names(status_counts), "=", status_counts, collapse = ", "), "\n")
      }
    }
  }, error = function(e) {
    cat("\n❌ VSG Processing FAILED\n")
    cat("   Error: ", e$message, "\n")
  })
} else {
  cat("⚠ VSG test file not found: ", vsg_file, "\n")
}

# =============================================================================
# TEST 3: Legacy Format Conversion
# =============================================================================

cat("\nTEST 3: Legacy Format Conversion\n")
cat("───────────────────────────────────────────────\n")

if (exists("pe_result") && !is.null(pe_result)) {
  tryCatch({
    legacy_format <- convert_to_legacy_format(pe_result)

    cat("\n✅ Legacy Conversion SUCCESS\n")
    cat("   Has legacy columns: ")
    legacy_cols <- c("plate_number", "sample_positive", "plate_valid", "PP_percent", "DOD")
    has_cols <- legacy_cols %in% names(legacy_format)
    cat(paste(legacy_cols, "=", has_cols, collapse = ", "), "\n")

    if (all(has_cols)) {
      cat("   ✓ All expected legacy columns present\n")
    } else {
      cat("   ⚠ Some legacy columns missing\n")
    }
  }, error = function(e) {
    cat("\n❌ Legacy Conversion FAILED\n")
    cat("   Error: ", e$message, "\n")
  })
}

# =============================================================================
# TEST 4: Standardized Output Validation
# =============================================================================

cat("\nTEST 4: Standardized Output Validation\n")
cat("───────────────────────────────────────────────\n")

expected_cols <- c(
  "sample_id", "test_type", "run_id", "run_valid", "sample_valid",
  "status_final", "status_raw", "n_runs", "metric_1", "metric_2",
  "PP_percent", "DOD", "qc_flags", "file_origin", "test_date"
)

if (exists("pe_result") && !is.null(pe_result)) {
  has_expected <- expected_cols %in% names(pe_result)

  cat("\n✅ Checking standardized columns:\n")
  for (i in seq_along(expected_cols)) {
    status <- if (has_expected[i]) "✓" else "✗"
    cat("   ", status, " ", expected_cols[i], "\n", sep = "")
  }

  if (all(has_expected)) {
    cat("\n   ✓ All standardized columns present\n")
  } else {
    missing <- expected_cols[!has_expected]
    cat("\n   ⚠ Missing columns: ", paste(missing, collapse = ", "), "\n")
  }
}

cat("\n═══════════════════════════════════════════════\n")
cat("Testing Complete\n")
cat("═══════════════════════════════════════════════\n\n")
