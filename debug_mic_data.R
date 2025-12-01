# Debug script to identify why MIC data is being lost
# Run this in R console after sourcing global.R

library(tidyverse)

# Source global to get all functions
source("global.R")

cat(strrep("=", 80), "\n")
cat("MIC DATA DEBUGGING\n")
cat(strrep("=", 80), "\n\n")

# Get site paths
site_paths <- get_site_paths("lsd")

cat("1. Checking MIC directory:\n")
cat("   Path:", site_paths$mic_dir, "\n")
cat("   Exists:", dir.exists(site_paths$mic_dir), "\n\n")

# List MIC files
mic_files <- list.files(site_paths$mic_dir, pattern = "\\.xlsx$", full.names = TRUE)
cat("2. MIC files found:", length(mic_files), "\n")
if (length(mic_files) > 0) {
  cat("   First 5 files:\n")
  cat("  ", head(basename(mic_files), 5), sep = "\n   ")
  cat("\n\n")
}

# Parse MIC data
cat("3. Parsing MIC data...\n")
mic_settings <- list(
  thresholds = list(
    `177T` = list(positive = 35, negative = 40),
    `18S2` = list(positive = 35, negative = 40),
    RNAseP_DNA = list(positive = 32, negative = 45),
    RNAseP_RNA = list(positive = 30, negative = 45)
  ),
  late_window = c(38, 40),
  delta_rp_limit = 8,
  allow_review_controls = FALSE,
  min_positive_reps = 2
)

mic_data <- parse_mic_directory(site_paths$mic_dir, mic_settings, list())

cat("   Runs parsed:", nrow(mic_data$runs), "\n")
cat("   Replicates parsed:", nrow(mic_data$replicates), "\n")
cat("   Samples aggregated:", nrow(mic_data$samples), "\n\n")

if (nrow(mic_data$samples) > 0) {
  cat("4. MIC samples structure:\n")
  cat("   Columns:", paste(names(mic_data$samples), collapse = ", "), "\n\n")

  cat("5. Sample ID columns:\n")
  if ("SampleID" %in% names(mic_data$samples)) {
    sample_ids <- mic_data$samples$SampleID
    cat("   SampleID column exists\n")
    cat("   Total values:", length(sample_ids), "\n")
    cat("   NA values:", sum(is.na(sample_ids)), "\n")
    cat("   Empty values:", sum(sample_ids == "", na.rm = TRUE), "\n")
    cat("   First 10 SampleIDs:\n")
    cat("  ", head(sample_ids, 10), sep = "\n   ")
    cat("\n\n")
  }

  if ("SampleName" %in% names(mic_data$samples)) {
    sample_names <- mic_data$samples$SampleName
    cat("   SampleName column exists\n")
    cat("   Total values:", length(sample_names), "\n")
    cat("   NA values:", sum(is.na(sample_names)), "\n")
    cat("   Empty values:", sum(sample_names == "", na.rm = TRUE), "\n")
    cat("   First 10 SampleNames:\n")
    cat("  ", head(sample_names, 10), sep = "\n   ")
    cat("\n\n")
  }

  cat("6. Testing normalize_sample_id:\n")
  id_columns <- c("code_barres_kps", "barcode", "SampleID", "sample_id", "Sample_ID", "Name")

  # Test coalesce_any_column
  barcode_values <- coalesce_any_column(mic_data$samples, id_columns)
  cat("   Barcode values found:", sum(!is.na(barcode_values)), "out of", length(barcode_values), "\n")
  cat("   First 10 barcode values:\n")
  cat("  ", head(barcode_values, 10), sep = "\n   ")
  cat("\n\n")

  # Test normalization
  normalized_ids <- mapply(
    normalize_sample_id,
    barcode = barcode_values,
    lab_id = NA
  )
  cat("   After normalization:\n")
  cat("   Total normalized:", length(normalized_ids), "\n")
  cat("   NA after normalization:", sum(is.na(normalized_ids)), "\n")
  cat("   Empty after normalization:", sum(normalized_ids == "", na.rm = TRUE), "\n")
  cat("   Unique normalized IDs:", length(unique(normalized_ids[!is.na(normalized_ids)])), "\n")
  cat("   First 10 normalized:\n")
  cat("  ", head(normalized_ids, 10), sep = "\n   ")
  cat("\n\n")

  cat("7. PipelineCategory status:\n")
  if ("PipelineCategory" %in% names(mic_data$samples)) {
    cat("   PipelineCategory exists\n")
    cat("   Values:\n")
    print(table(mic_data$samples$PipelineCategory, useNA = "ifany"))
  } else {
    cat("   ERROR: PipelineCategory column missing!\n")
  }

} else {
  cat("ERROR: No samples found in MIC data!\n")
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("DIAGNOSIS COMPLETE\n")
cat(strrep("=", 80), "\n")
