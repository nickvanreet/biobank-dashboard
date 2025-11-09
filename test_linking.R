# Test script for data linking functionality
# ============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(janitor)
})

# Source required files
source("R/core/data_loader_utils.R")
source("R/core/extraction_data_utils.R")
source("R/core/data_linking_utils.R")
source("R/data/data_cleaner_improved.R")

cat("=== Testing Data Linking Functionality ===\n\n")

# Test 1: Barcode Normalization
cat("Test 1: Barcode Normalization\n")
test_barcodes <- c("KPS-12345", "kps0012345", "  12345  ", "KPS 12345", NA)
normalized <- normalize_barcode(test_barcodes)
cat("Original:  ", paste(test_barcodes, collapse = ", "), "\n")
cat("Normalized:", paste(normalized, collapse = ", "), "\n\n")

# Test 2: Load Extraction Data
cat("Test 2: Loading Extraction Data\n")
tryCatch({
  extraction_df <- load_extraction_dataset("data/extractions")
  cat(sprintf("✓ Loaded %d extraction records\n", nrow(extraction_df)))
  cat("  Columns:", paste(names(extraction_df), collapse = ", "), "\n\n")
}, error = function(e) {
  cat("✗ Error loading extraction data:", e$message, "\n\n")
})

# Test 3: Load Biobank Data
cat("Test 3: Loading Biobank Data\n")
tryCatch({
  biobank_files <- list_biobank_files("data/biobank")
  if (length(biobank_files) > 0) {
    cat(sprintf("  Found %d biobank file(s)\n", length(biobank_files)))
    raw_data <- load_biobank_file(biobank_files[1])
    quality_report <- analyze_data_quality(raw_data)
    biobank_df <- quality_report$clean_data
    cat(sprintf("✓ Loaded %d biobank records\n", nrow(biobank_df)))
    cat("  Columns:", paste(head(names(biobank_df), 10), collapse = ", "), "...\n\n")
  } else {
    cat("✗ No biobank files found\n\n")
    biobank_df <- NULL
  }
}, error = function(e) {
  cat("✗ Error loading biobank data:", e$message, "\n\n")
  biobank_df <- NULL
})

# Test 4: Link Datasets
cat("Test 4: Linking Extraction to Biobank\n")
tryCatch({
  if (!is.null(extraction_df) && !is.null(biobank_df)) {
    linked_df <- link_extraction_to_biobank(extraction_df, biobank_df)
    cat(sprintf("✓ Linked dataset created with %d records\n", nrow(linked_df)))

    # Show linkage statistics
    n_matched <- sum(linked_df$biobank_matched, na.rm = TRUE)
    n_total <- nrow(linked_df)
    pct_matched <- 100 * n_matched / n_total

    cat(sprintf("  Matched to biobank: %d/%d (%.1f%%)\n", n_matched, n_total, pct_matched))

    if ("health_structure_match" %in% names(linked_df)) {
      n_structure_match <- sum(linked_df$health_structure_match == TRUE, na.rm = TRUE)
      n_structure_mismatch <- sum(linked_df$health_structure_match == FALSE, na.rm = TRUE)
      cat(sprintf("  Health structure matches: %d\n", n_structure_match))
      cat(sprintf("  Health structure mismatches: %d\n", n_structure_mismatch))
    }
    cat("\n")
  } else {
    cat("✗ Cannot link: missing extraction or biobank data\n\n")
    linked_df <- extraction_df
  }
}, error = function(e) {
  cat("✗ Error linking datasets:", e$message, "\n\n")
  linked_df <- extraction_df
})

# Test 5: Summary Metrics
cat("Test 5: Computing Linkage Metrics\n")
tryCatch({
  if (!is.null(linked_df)) {
    metrics <- summarise_linkage_metrics(linked_df)
    cat("  Total extractions:", metrics$total_extractions, "\n")
    cat("  Matched to biobank:", metrics$matched_to_biobank, "\n")
    cat("  Unmatched:", metrics$unmatched_to_biobank, "\n")
    cat("  Linkage rate:", sprintf("%.1f%%", 100 * metrics$pct_matched), "\n")
    cat("  Structure matches:", metrics$health_structure_matches, "\n")
    cat("  Structure mismatches:", metrics$health_structure_mismatches, "\n\n")
  }
}, error = function(e) {
  cat("✗ Error computing metrics:", e$message, "\n\n")
})

# Test 6: Time Series Analysis
cat("Test 6: Health Structure Volume Over Time\n")
tryCatch({
  if (!is.null(linked_df)) {
    time_series <- summarise_health_structure_volumes_over_time(linked_df)
    cat(sprintf("✓ Generated %d time series records\n", nrow(time_series)))

    if (nrow(time_series) > 0) {
      cat("  Health structures:", paste(unique(time_series$health_structure), collapse = ", "), "\n")
      cat("  Date range:", format(min(time_series$month), "%Y-%m"), "to",
          format(max(time_series$month), "%Y-%m"), "\n\n")

      # Show sample data
      cat("  Sample (latest month):\n")
      latest <- time_series %>%
        group_by(health_structure) %>%
        filter(month == max(month)) %>%
        ungroup() %>%
        select(health_structure, month, n_extractions, total_volume, pct_ready)
      print(latest, n = 5)
    }
  }
}, error = function(e) {
  cat("✗ Error in time series:", e$message, "\n\n")
})

# Test 7: Volume Targets
cat("\nTest 7: Volume Target Calculation\n")
tryCatch({
  if (!is.null(linked_df)) {
    targets <- calculate_volume_targets(linked_df, expected_monthly_volume = 50)
    cat(sprintf("✓ Generated %d target records\n", nrow(targets)))

    if (nrow(targets) > 0) {
      latest_targets <- targets %>%
        group_by(health_structure) %>%
        filter(month == max(month)) %>%
        ungroup() %>%
        select(health_structure, month, actual_volume, expected_volume,
               pct_of_target, meets_target)

      cat("  Latest month targets:\n")
      print(latest_targets, n = 5)
    }
  }
}, error = function(e) {
  cat("✗ Error calculating targets:", e$message, "\n\n")
})

cat("\n=== All Tests Complete ===\n")
