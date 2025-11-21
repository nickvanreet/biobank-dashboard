# Test script to debug concordance data loading
suppressPackageStartupMessages({
  library(tidyverse)
})

# Load utilities
source("R/utils_elisa.R")
source("R/utils_elisa_concordance.R")

# Test 1: Load ELISA data
cat("=== TEST 1: Loading ELISA data ===\n")
all_elisa <- load_elisa_data(
  dirs = c("data/elisa_pe", "data/elisa_vsg"),
  biobank_df = NULL
)

cat("Total ELISA records loaded:", nrow(all_elisa), "\n")
cat("Columns:", paste(names(all_elisa), collapse = ", "), "\n\n")

# Test 2: Check elisa_type column
cat("=== TEST 2: Check elisa_type column ===\n")
if ("elisa_type" %in% names(all_elisa)) {
  cat("elisa_type column EXISTS\n")
  cat("Unique elisa_type values:", paste(unique(all_elisa$elisa_type), collapse = ", "), "\n")
  cat("Count by elisa_type:\n")
  print(table(all_elisa$elisa_type, useNA = "always"))
} else {
  cat("ERROR: elisa_type column MISSING!\n")
}
cat("\n")

# Test 3: Filter by elisa_type
cat("=== TEST 3: Filter by elisa_type ===\n")
pe_data <- all_elisa %>% filter(elisa_type == "ELISA_pe")
vsg_data <- all_elisa %>% filter(elisa_type == "ELISA_vsg")

cat("PE records:", nrow(pe_data), "\n")
cat("VSG records:", nrow(vsg_data), "\n\n")

# Test 4: Check sample_type distribution
cat("=== TEST 4: Check sample_type distribution ===\n")
if ("sample_type" %in% names(all_elisa)) {
  cat("PE sample_type distribution:\n")
  print(table(pe_data$sample_type, useNA = "always"))
  cat("\nVSG sample_type distribution:\n")
  print(table(vsg_data$sample_type, useNA = "always"))
} else {
  cat("ERROR: sample_type column MISSING!\n")
}
cat("\n")

# Test 5: Filter samples only
cat("=== TEST 5: Filter samples only ===\n")
pe_samples <- pe_data %>% filter(sample_type == "sample")
vsg_samples <- vsg_data %>% filter(sample_type == "sample")

cat("PE samples (excluding controls):", nrow(pe_samples), "\n")
cat("VSG samples (excluding controls):", nrow(vsg_samples), "\n\n")

# Test 6: Match samples
cat("=== TEST 6: Match samples ===\n")
matched_data <- match_elisa_samples(pe_samples, vsg_samples)
cat("Matched samples:", nrow(matched_data), "\n")
if (nrow(matched_data) > 0) {
  cat("Match methods:\n")
  print(table(matched_data$match_method))
  cat("\nSample of matched data:\n")
  print(head(matched_data %>% select(pe_sample, vsg_sample, pe_barcode, vsg_barcode, match_method)))
} else {
  cat("WARNING: No samples matched!\n")
}
