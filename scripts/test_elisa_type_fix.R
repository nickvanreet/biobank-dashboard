#!/usr/bin/env Rscript
# Quick test to verify elisa_type classification is working

suppressPackageStartupMessages({
  library(tidyverse)
})

# Load the ELISA utilities
source("R/utils_elisa.R")

# Clear cache to force re-parsing
clear_elisa_cache()

# Load a minimal biobank dataset for linking
biobank_df <- tibble(
  code_barres_kps = "TEST",
  numero_labo = "TEST",
  Province = "Test",
  HealthZone = "Test",
  Structure = "Test",
  Sex = "M",
  Age = 30,
  AgeGroup = "30-39",
  SampleDate = Sys.Date(),
  Cohort = "Test"
)

# Load ELISA data (will trigger parsing with new cache version)
cat("\n=== Loading ELISA data ===\n")
elisa_data <- load_elisa_data(biobank_df = biobank_df)

# Check elisa_type column
cat("\n=== Checking elisa_type column ===\n")
if (!"elisa_type" %in% names(elisa_data)) {
  cat("ERROR: elisa_type column not found!\n")
  cat("Available columns:", paste(names(elisa_data), collapse = ", "), "\n")
} else {
  type_counts <- table(elisa_data$elisa_type, useNA = "ifany")
  cat("elisa_type distribution:\n")
  print(type_counts)

  # Check for NAs
  na_count <- sum(is.na(elisa_data$elisa_type))
  total_count <- nrow(elisa_data)

  if (na_count > 0) {
    cat("\nWARNING:", na_count, "of", total_count, "records have NA for elisa_type\n")
    cat("This indicates the fix did not work!\n")
  } else {
    cat("\n✓ SUCCESS: All records have a valid elisa_type value\n")

    # Show breakdown
    pe_count <- sum(elisa_data$elisa_type == "ELISA_pe", na.rm = TRUE)
    vsg_count <- sum(elisa_data$elisa_type == "ELISA_vsg", na.rm = TRUE)
    cat("  - ELISA_pe:", pe_count, "records\n")
    cat("  - ELISA_vsg:", vsg_count, "records\n")
  }
}

cat("\n=== Test complete ===\n")
