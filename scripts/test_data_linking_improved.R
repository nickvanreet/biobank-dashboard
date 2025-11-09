# Test script for improved data linking functionality
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

cat("=== Testing Improved Data Linking Functionality ===\n\n")

# Test 1: Barcode Normalization
cat("Test 1: Barcode Normalization\n")
test_barcodes <- c("KPS-12345", "kps0012345", "  12345  ", "KPS 12345", NA, "001234")
normalized <- normalize_barcode(test_barcodes)
cat("Original:  ", paste(test_barcodes, collapse = ", "), "\n")
cat("Normalized:", paste(normalized, collapse = ", "), "\n\n")

# Test 2: Load Extraction Data
cat("Test 2: Loading Extraction Data\n")
tryCatch({
  extraction_df <- load_extraction_dataset("data/extractions")
  cat(sprintf("✓ Loaded %d extraction records\n", nrow(extraction_df)))
  cat("  Columns:", paste(names(extraction_df), collapse = ", "), "\n")

  # Verify removed columns
  removed_cols <- c("filter_type", "project", "batch")
  still_present <- removed_cols[removed_cols %in% names(extraction_df)]
  if (length(still_present) > 0) {
    cat("  ⚠ WARNING: These columns should have been removed:", paste(still_present, collapse = ", "), "\n")
  } else {
    cat("  ✓ Unnecessary columns removed (filter_type, project, batch)\n")
  }

  # Verify required columns for freezer module
  freezer_cols <- c("rack", "rack_row", "rack_column")
  missing_freezer <- freezer_cols[!freezer_cols %in% names(extraction_df)]
  if (length(missing_freezer) > 0) {
    cat("  ⚠ WARNING: Missing freezer columns:", paste(missing_freezer, collapse = ", "), "\n")
  } else {
    cat("  ✓ Freezer module columns present (Rack, Rangée, Position)\n")
  }

  # Verify RSC columns
  rsc_cols <- c("rsc_run", "rsc_position")
  missing_rsc <- rsc_cols[!rsc_cols %in% names(extraction_df)]
  if (length(missing_rsc) > 0) {
    cat("  ⚠ WARNING: Missing RSC columns:", paste(missing_rsc, collapse = ", "), "\n")
  } else {
    cat("  ✓ RSC columns present (RSC Run, RSC Position)\n")
  }

  cat("\n")
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

    # Check for key matching fields
    if ("numero" %in% names(biobank_df) || "lab_id" %in% names(biobank_df)) {
      cat("  ✓ Numero/lab_id field present\n")
    }
    if ("code_barres_kps" %in% names(biobank_df) || "barcode" %in% names(biobank_df)) {
      cat("  ✓ Barcode field present\n")
    }
    cat("\n")
  } else {
    cat("✗ No biobank files found\n\n")
    biobank_df <- NULL
  }
}, error = function(e) {
  cat("✗ Error loading biobank data:", e$message, "\n\n")
  biobank_df <- NULL
})

# Test 4: Link Datasets with Both Barcode and Numero
cat("Test 4: Linking Extraction to Biobank (Barcode + Numero)\n")
tryCatch({
  if (!is.null(extraction_df) && !is.null(biobank_df)) {
    linked_df <- link_extraction_to_biobank(extraction_df, biobank_df)
    cat(sprintf("✓ Linked dataset created with %d records\n", nrow(linked_df)))

    # Show linkage statistics
    n_matched <- sum(linked_df$biobank_matched, na.rm = TRUE)
    n_total <- nrow(linked_df)
    pct_matched <- 100 * n_matched / n_total

    cat(sprintf("  Matched to biobank: %d/%d (%.1f%%)\n", n_matched, n_total, pct_matched))

    # Show match type distribution
    if ("biobank_match_type" %in% names(linked_df) && n_matched > 0) {
      match_types <- linked_df %>%
        filter(biobank_matched) %>%
        count(biobank_match_type)
      cat("  Match type breakdown:\n")
      for (i in 1:nrow(match_types)) {
        cat(sprintf("    - %s: %d\n", match_types$biobank_match_type[i], match_types$n[i]))
      }
    }

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
  cat("✗ Error linking datasets:", e$message, "\n")
  cat("  Full error:\n")
  print(e)
  cat("\n")
  linked_df <- extraction_df
})

# Test 5: Extract Quality Mapping
cat("Test 5: Extract Quality Mapping Verification\n")
tryCatch({
  if (!is.null(extraction_df) && "extract_quality" %in% names(extraction_df)) {
    quality_counts <- extraction_df %>%
      count(extract_quality, sort = TRUE)
    cat("  Extract quality distribution:\n")
    print(quality_counts)

    # Verify expected values
    expected <- c("Clear", "Foncé", "Échec", "Unknown")
    actual <- unique(extraction_df$extract_quality[!is.na(extraction_df$extract_quality)])
    unexpected <- setdiff(actual, expected)
    if (length(unexpected) > 0) {
      cat("  ⚠ WARNING: Unexpected quality values:", paste(unexpected, collapse = ", "), "\n")
    } else {
      cat("  ✓ All quality values correctly mapped (C=Clear, F=Foncé/Dark, E=Échec/Failure)\n")
    }
    cat("\n")
  }
}, error = function(e) {
  cat("✗ Error checking extract quality:", e$message, "\n\n")
})

# Test 6: Summary Metrics
cat("Test 6: Computing Linkage Metrics\n")
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

# Test 7: Sample of Matched Records
cat("Test 7: Sample of Matched Records\n")
tryCatch({
  if (!is.null(linked_df) && any(linked_df$biobank_matched, na.rm = TRUE)) {
    matched_sample <- linked_df %>%
      filter(biobank_matched) %>%
      select(sample_id, extraction_date, biobank_match_type, biobank_barcode,
             biobank_lab_id, health_structure, biobank_health_facility,
             rsc_run, rsc_position, rack, rack_row, rack_column) %>%
      head(5)

    cat("  First 5 matched records:\n")
    print(matched_sample, n = 5)
    cat("\n")
  } else {
    cat("  No matched records to display\n\n")
  }
}, error = function(e) {
  cat("✗ Error displaying matched records:", e$message, "\n\n")
})

# Test 8: Freezer Module Fields Check
cat("Test 8: Freezer Module Fields Verification\n")
tryCatch({
  if (!is.null(linked_df)) {
    freezer_sample <- linked_df %>%
      filter(!is.na(rack) | !is.na(rack_row) | !is.na(rack_column)) %>%
      select(sample_id, rack, rack_row, rack_column, rsc_run, rsc_position) %>%
      head(5)

    if (nrow(freezer_sample) > 0) {
      cat("  Sample records with freezer location data:\n")
      print(freezer_sample)
      cat("\n")
    } else {
      cat("  ⚠ No records with freezer location data found\n\n")
    }
  }
}, error = function(e) {
  cat("✗ Error checking freezer fields:", e$message, "\n\n")
})

cat("=== All Tests Complete ===\n")
