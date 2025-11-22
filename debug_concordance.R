# Debug script for concordance analysis
# This script checks what data is available and why matches might be failing

library(tidyverse)
library(readxl)

# Source necessary utilities
source("R/utils_elisa_concordance.R")
source("R/utils_elisa.R")
source("R/utils_ielisa.R")
source("R/core/mic_qpcr_pipeline.R")

# ==============================================================================
# Load MIC Data
# ==============================================================================
cat("\n=== MIC/qPCR Data ===\n")
mic_files <- list.files("data/pcr", pattern = "\\.xlsx$", full.names = TRUE)
cat("MIC files found:", length(mic_files), "\n")

if (length(mic_files) > 0) {
  # Load first file as example
  mic_example <- tryCatch({
    read_mic_qpcr_file(mic_files[1])
  }, error = function(e) {
    cat("Error loading MIC file:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(mic_example)) {
    cat("MIC columns:", paste(names(mic_example), collapse = ", "), "\n")
    cat("MIC rows:", nrow(mic_example), "\n")
    if (nrow(mic_example) > 0) {
      cat("Sample Barcode examples:\n")
      print(head(mic_example$Barcode, 10))
      cat("Sample TestNumber examples:\n")
      print(head(mic_example$TestNumber, 10))
    }
  }
}

# ==============================================================================
# Load ELISA-PE Data
# ==============================================================================
cat("\n=== ELISA-PE Data ===\n")
pe_files <- list.files("data/elisa_pe", pattern = "\\.xlsx$", full.names = TRUE)
cat("ELISA-PE files found:", length(pe_files), "\n")

if (length(pe_files) > 0) {
  # Load first file as example
  pe_example <- tryCatch({
    load_elisa_file(pe_files[1])
  }, error = function(e) {
    cat("Error loading ELISA-PE file:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(pe_example)) {
    cat("ELISA-PE columns:", paste(names(pe_example), collapse = ", "), "\n")
    cat("ELISA-PE total rows:", nrow(pe_example), "\n")
    pe_samples <- pe_example %>% filter(sample_type == "sample")
    cat("ELISA-PE sample rows:", nrow(pe_samples), "\n")
    if (nrow(pe_samples) > 0) {
      cat("Sample barcode (code_barres_kps) examples:\n")
      print(head(pe_samples$code_barres_kps, 10))
      cat("Sample numero_labo examples:\n")
      print(head(pe_samples$numero_labo, 10))
    }
  }
}

# ==============================================================================
# Load ELISA-VSG Data
# ==============================================================================
cat("\n=== ELISA-VSG Data ===\n")
vsg_files <- list.files("data/elisa_vsg", pattern = "\\.xlsx$", full.names = TRUE)
cat("ELISA-VSG files found:", length(vsg_files), "\n")

if (length(vsg_files) > 0) {
  # Load first file as example
  vsg_example <- tryCatch({
    load_elisa_file(vsg_files[1])
  }, error = function(e) {
    cat("Error loading ELISA-VSG file:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(vsg_example)) {
    cat("ELISA-VSG columns:", paste(names(vsg_example), collapse = ", "), "\n")
    cat("ELISA-VSG total rows:", nrow(vsg_example), "\n")
    vsg_samples <- vsg_example %>% filter(sample_type == "sample")
    cat("ELISA-VSG sample rows:", nrow(vsg_samples), "\n")
    if (nrow(vsg_samples) > 0) {
      cat("Sample barcode (code_barres_kps) examples:\n")
      print(head(vsg_samples$code_barres_kps, 10))
    }
  }
}

# ==============================================================================
# Load iELISA Data
# ==============================================================================
cat("\n=== iELISA Data ===\n")
ielisa_files <- list.files("data/lsd/ielisa", pattern = "\\.xlsx$", full.names = TRUE)
cat("iELISA files found:", length(ielisa_files), "\n")

if (length(ielisa_files) > 0) {
  # Load first file as example
  ielisa_example <- tryCatch({
    load_ielisa_file(ielisa_files[1])
  }, error = function(e) {
    cat("Error loading iELISA file:", conditionMessage(e), "\n")
    NULL
  })

  if (!is.null(ielisa_example)) {
    cat("iELISA columns:", paste(names(ielisa_example), collapse = ", "), "\n")
    cat("iELISA rows:", nrow(ielisa_example), "\n")
    if (nrow(ielisa_example) > 0) {
      # Check what identifier columns are available
      cat("Sample identifier columns:\n")
      if ("Barcode" %in% names(ielisa_example)) {
        cat("Barcode examples:\n")
        print(head(ielisa_example$Barcode, 10))
      }
      if ("code_barres_kps" %in% names(ielisa_example)) {
        cat("code_barres_kps examples:\n")
        print(head(ielisa_example$code_barres_kps, 10))
      }
      if ("LabID" %in% names(ielisa_example)) {
        cat("LabID examples:\n")
        print(head(ielisa_example$LabID, 10))
      }
      if ("numero_labo" %in% names(ielisa_example)) {
        cat("numero_labo examples:\n")
        print(head(ielisa_example$numero_labo, 10))
      }
    }
  }
}

# ==============================================================================
# Test matching logic
# ==============================================================================
cat("\n=== Testing Matching Logic ===\n")

# Try to match MIC with ELISA-PE
if (!is.null(mic_example) && !is.null(pe_example)) {
  cat("\nTrying to match MIC with ELISA-PE...\n")

  # Normalize identifiers
  mic_barcodes <- unique(normalize_elisa_id(mic_example$Barcode))
  mic_numbers <- unique(normalize_elisa_id(mic_example$TestNumber))
  pe_barcodes <- unique(normalize_elisa_id(pe_example$code_barres_kps))
  pe_numbers <- unique(normalize_elisa_id(pe_example$numero_labo))

  # Check for overlaps
  barcode_matches <- sum(mic_barcodes %in% pe_barcodes, na.rm = TRUE)
  number_matches <- sum(mic_numbers %in% pe_numbers, na.rm = TRUE)

  cat("Normalized MIC barcodes (non-NA):", sum(!is.na(mic_barcodes)), "\n")
  cat("Normalized PE barcodes (non-NA):", sum(!is.na(pe_barcodes)), "\n")
  cat("Barcode matches:", barcode_matches, "\n")

  cat("Normalized MIC test numbers (non-NA):", sum(!is.na(mic_numbers)), "\n")
  cat("Normalized PE lab numbers (non-NA):", sum(!is.na(pe_numbers)), "\n")
  cat("Number matches:", number_matches, "\n")

  # Show examples of what would match
  if (barcode_matches > 0) {
    cat("\nExample matching barcodes:\n")
    matches <- intersect(mic_barcodes, pe_barcodes)
    print(head(matches[!is.na(matches)], 5))
  }
}

# Try to match iELISA with ELISA-PE
if (!is.null(ielisa_example) && !is.null(pe_example)) {
  cat("\nTrying to match iELISA with ELISA-PE...\n")

  # Normalize identifiers - handle different column names
  ielisa_barcodes <- unique(normalize_elisa_id(coalesce(
    if ("Barcode" %in% names(ielisa_example)) ielisa_example$Barcode else NA_character_,
    if ("code_barres_kps" %in% names(ielisa_example)) ielisa_example$code_barres_kps else NA_character_
  )))

  ielisa_numbers <- unique(normalize_elisa_id(coalesce(
    if ("LabID" %in% names(ielisa_example)) ielisa_example$LabID else NA_character_,
    if ("numero_labo" %in% names(ielisa_example)) ielisa_example$numero_labo else NA_character_
  )))

  pe_barcodes <- unique(normalize_elisa_id(pe_example$code_barres_kps))
  pe_numbers <- unique(normalize_elisa_id(pe_example$numero_labo))

  # Check for overlaps
  barcode_matches <- sum(ielisa_barcodes %in% pe_barcodes, na.rm = TRUE)
  number_matches <- sum(ielisa_numbers %in% pe_numbers, na.rm = TRUE)

  cat("Normalized iELISA barcodes (non-NA):", sum(!is.na(ielisa_barcodes)), "\n")
  cat("Normalized PE barcodes (non-NA):", sum(!is.na(pe_barcodes)), "\n")
  cat("Barcode matches:", barcode_matches, "\n")

  cat("Normalized iELISA lab IDs (non-NA):", sum(!is.na(ielisa_numbers)), "\n")
  cat("Normalized PE lab numbers (non-NA):", sum(!is.na(pe_numbers)), "\n")
  cat("Number matches:", number_matches, "\n")

  # Show examples of what would match
  if (barcode_matches > 0) {
    cat("\nExample matching barcodes:\n")
    matches <- intersect(ielisa_barcodes, pe_barcodes)
    print(head(matches[!is.na(matches)], 5))
  }
}

cat("\n=== Diagnostic Complete ===\n")
