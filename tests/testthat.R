# ==============================================================================
# Test Runner for MIC Module Unit Tests
# ==============================================================================
# This file runs all tests in the tests/testthat directory.
# It ensures the MIC module functions are available for testing.
# ==============================================================================

library(testthat)

# Set up test environment
message("Setting up test environment for MIC modules...")

# Source required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(glue)
  library(lubridate)
})

# Source the MIC utility functions
tryCatch({
  source(file.path("..", "R", "modules", "mod_05_mic_qpcr.R"), local = FALSE)
  message("✓ Loaded mod_05_mic_qpcr.R")
}, error = function(e) {
  warning("Could not load mod_05_mic_qpcr.R: ", e$message)
})

# Run all tests with progress reporter
message("\nRunning MIC module unit tests...\n")
test_results <- test_dir(
  "testthat",
  reporter = c("progress", "summary"),
  stop_on_failure = FALSE
)

# Print summary
cat("\n")
message("════════════════════════════════════════════════════════════════")
message("Test suite completed!")
message("════════════════════════════════════════════════════════════════")

