#!/usr/bin/env Rscript
# =============================================================================
# Validation Script for Modular ELISA Implementation
# Checks if all modules can be sourced without syntax errors
# =============================================================================

cat("═══════════════════════════════════════════════\n")
cat("Validating Modular ELISA Implementation\n")
cat("═══════════════════════════════════════════════\n\n")

# Track results
results <- list()

# Function to test sourcing a file
test_source <- function(file_path, description) {
  cat(sprintf("Testing: %s\n", description))
  cat(sprintf("  File: %s\n", file_path))

  if (!file.exists(file_path)) {
    cat(sprintf("  ❌ FAIL: File not found\n\n"))
    return(FALSE)
  }

  result <- tryCatch({
    # Try to parse the file (checks syntax without executing)
    parse(file_path)
    cat(sprintf("  ✅ PASS: No syntax errors\n\n"))
    TRUE
  }, error = function(e) {
    cat(sprintf("  ❌ FAIL: Syntax error\n"))
    cat(sprintf("  Error: %s\n\n", e$message))
    FALSE
  })

  return(result)
}

# Test all module files
cat("Testing ELISA-PE modules:\n")
cat("─────────────────────────────────────────────\n")
results$pe_ingest <- test_source(
  "R/modules/elisa_pe/ingest_pe.R",
  "ELISA-PE Ingestion Module"
)
results$pe_qc <- test_source(
  "R/modules/elisa_pe/qc_pe.R",
  "ELISA-PE QC Module"
)
results$pe_interpret <- test_source(
  "R/modules/elisa_pe/interpret_pe.R",
  "ELISA-PE Interpretation Module"
)
results$pe_output <- test_source(
  "R/modules/elisa_pe/output_pe.R",
  "ELISA-PE Output Module"
)

cat("Testing ELISA-VSG modules:\n")
cat("─────────────────────────────────────────────\n")
results$vsg_ingest <- test_source(
  "R/modules/elisa_vsg/ingest_vsg.R",
  "ELISA-VSG Ingestion Module"
)
results$vsg_qc <- test_source(
  "R/modules/elisa_vsg/qc_vsg.R",
  "ELISA-VSG QC Module"
)
results$vsg_interpret <- test_source(
  "R/modules/elisa_vsg/interpret_vsg.R",
  "ELISA-VSG Interpretation Module"
)
results$vsg_output <- test_source(
  "R/modules/elisa_vsg/output_vsg.R",
  "ELISA-VSG Output Module"
)

cat("Testing shared modules:\n")
cat("─────────────────────────────────────────────\n")
results$shared_utils <- test_source(
  "R/modules/elisa_shared/utils_elisa.R",
  "Shared ELISA Utilities"
)
results$shared_process <- test_source(
  "R/modules/elisa_shared/process_elisa_modular.R",
  "Modular Processing Integration"
)

cat("Testing modified infrastructure:\n")
cat("─────────────────────────────────────────────\n")
results$utils_elisa <- test_source(
  "R/utils_elisa.R",
  "ELISA Utilities (Modified)"
)

# Summary
cat("═══════════════════════════════════════════════\n")
cat("Validation Summary\n")
cat("═══════════════════════════════════════════════\n\n")

total <- length(results)
passed <- sum(unlist(results))
failed <- total - passed

cat(sprintf("Total modules: %d\n", total))
cat(sprintf("✅ Passed: %d\n", passed))
cat(sprintf("❌ Failed: %d\n\n", failed))

if (failed == 0) {
  cat("🎉 ALL MODULES VALIDATED SUCCESSFULLY!\n")
  cat("The modular ELISA implementation is ready for testing.\n\n")
  quit(status = 0)
} else {
  cat("⚠️  VALIDATION FAILED\n")
  cat("Please fix the errors above before testing.\n\n")
  quit(status = 1)
}
