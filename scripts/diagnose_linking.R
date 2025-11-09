# Diagnostic script to understand linking failure
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
})

source("R/core/data_loader_utils.R")
source("R/core/extraction_data_utils.R")
source("R/core/data_linking_utils.R")
source("R/data/data_cleaner_improved.R")

cat("=== DIAGNOSTIC: Data Linking Issue ===\n\n")

# Load extraction data
cat("Loading extraction data...\n")
extraction_df <- load_extraction_dataset("data/extractions")

# Show sample of extraction data
cat("\n--- Sample Extraction Data ---\n")
extraction_sample <- extraction_df %>%
  select(sample_id, record_number, extraction_date, health_structure) %>%
  head(10)
print(extraction_sample)

# Check what the original column names were
cat("\n--- Checking raw extraction file column names ---\n")
extraction_files <- list.files("data/extractions", pattern = "\\.(xlsx|xls)$", full.names = TRUE)
if (length(extraction_files) > 0) {
  raw_extraction <- read_excel(extraction_files[1], n_max = 5)
  cat("Original columns:", paste(names(raw_extraction), collapse = ", "), "\n")
  cat("\nFirst few rows:\n")
  print(raw_extraction[1:3, 1:min(10, ncol(raw_extraction))])
}

# Load biobank data
cat("\n\nLoading biobank data...\n")
biobank_files <- list_biobank_files("data/biobank")
if (length(biobank_files) > 0) {
  raw_biobank <- load_biobank_file(biobank_files[1])
  quality_report <- analyze_data_quality(raw_biobank)
  biobank_df <- quality_report$clean_data

  cat("\n--- Sample Biobank Data ---\n")
  biobank_sample <- biobank_df %>%
    select(any_of(c("numero", "code_barres_kps", "structure_sanitaire", "date_prelevement", "etude"))) %>%
    head(10)
  print(biobank_sample)

  cat("\n--- Raw biobank column names ---\n")
  cat("Original columns:", paste(names(raw_biobank), collapse = ", "), "\n")
}

# Test normalization on actual data
cat("\n\n--- Testing Barcode Normalization ---\n")
if (exists("extraction_df") && exists("biobank_df")) {

  # Sample barcodes from extraction
  ext_barcodes <- extraction_df$sample_id[1:5]
  cat("Extraction sample_id (first 5):\n")
  print(ext_barcodes)
  cat("Normalized:\n")
  print(normalize_barcode(ext_barcodes))

  # Sample barcodes from biobank
  if ("code_barres_kps" %in% names(biobank_df)) {
    bb_barcodes <- biobank_df$code_barres_kps[1:5]
    cat("\nBiobank code_barres_kps (first 5):\n")
    print(bb_barcodes)
    cat("Normalized:\n")
    print(normalize_barcode(bb_barcodes))
  }

  # Check numero fields
  if ("numero" %in% names(biobank_df)) {
    bb_numeros <- biobank_df$numero[1:5]
    cat("\nBiobank numero (first 5):\n")
    print(bb_numeros)
  }

  # Try to find any matches manually
  cat("\n--- Manual Match Test ---\n")
  ext_norm <- normalize_barcode(extraction_df$sample_id)

  if ("code_barres_kps" %in% names(biobank_df)) {
    bb_norm <- normalize_barcode(biobank_df$code_barres_kps)
    matches <- sum(ext_norm %in% bb_norm, na.rm = TRUE)
    cat(sprintf("Matches on barcode: %d/%d\n", matches, length(ext_norm)))
  }

  if ("numero" %in% names(biobank_df)) {
    bb_num_norm <- normalize_barcode(as.character(biobank_df$numero))
    matches_num <- sum(ext_norm %in% bb_num_norm, na.rm = TRUE)
    cat(sprintf("Matches on numero: %d/%d\n", matches_num, length(ext_norm)))
  }
}

cat("\n=== END DIAGNOSTIC ===\n")
