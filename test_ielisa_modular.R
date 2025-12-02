# Test script for iELISA modular pipeline
# Verifies that the new 4-step pipeline works with existing iELISA files

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

# Source the updated utils
source("R/utils_ielisa.R")

# Test with a single file
test_file <- "data/ielisa/250528 Résultats iELISA v6.xlsx"

cat("═══════════════════════════════════════════════════════════\n")
cat("Testing iELISA Modular Pipeline\n")
cat("═══════════════════════════════════════════════════════════\n\n")

# Test 1: Parse single file
cat("TEST 1: Parse single file\n")
cat("─────────────────────────────────────────────────────────\n")
result <- tryCatch({
  parse_ielisa_file(test_file)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(result)) {
  cat("\n✅ SUCCESS: File parsed successfully\n")
  cat("   Samples:", nrow(result), "\n")
  cat("   Columns:", ncol(result), "\n")
  cat("   Positive samples (L13):", sum(result$positive_L13, na.rm = TRUE), "\n")
  cat("   Positive samples (L15):", sum(result$positive_L15, na.rm = TRUE), "\n")
  cat("   Overall positive:", sum(result$sample_positive, na.rm = TRUE), "\n")
  cat("\n   Sample data preview:\n")
  print(result %>%
    select(numero_labo, code_barres_kps, OD_L13, OD_L15,
           pct_inh_f1_13, pct_inh_f1_15,
           positive_L13, positive_L15, sample_positive) %>%
    head(5))
} else {
  cat("\n❌ FAILED: Could not parse file\n")
}

cat("\n═══════════════════════════════════════════════════════════\n\n")

# Test 2: Load all files from folder
cat("TEST 2: Load all files from folder (with caching)\n")
cat("─────────────────────────────────────────────────────────\n")
all_data <- tryCatch({
  load_ielisa_data()
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  NULL
})

if (!is.null(all_data) && nrow(all_data) > 0) {
  cat("\n✅ SUCCESS: All files loaded\n")
  cat("   Total samples:", nrow(all_data), "\n")
  cat("   Total files:", n_distinct(all_data$file), "\n")
  cat("   Date range:", min(all_data$plate_date, na.rm = TRUE), "to",
      max(all_data$plate_date, na.rm = TRUE), "\n")
  cat("   Overall positive:", sum(all_data$sample_positive, na.rm = TRUE), "\n")
  cat("   Plate validity (L13):", sum(all_data$plate_valid_L13, na.rm = TRUE), "of",
      n_distinct(all_data$plate_id), "plates\n")
  cat("   Plate validity (L15):", sum(all_data$plate_valid_L15, na.rm = TRUE), "of",
      n_distinct(all_data$plate_id), "plates\n")
} else {
  cat("\n❌ FAILED: Could not load files\n")
}

cat("\n═══════════════════════════════════════════════════════════\n")
cat("Testing complete!\n")
cat("═══════════════════════════════════════════════════════════\n")
