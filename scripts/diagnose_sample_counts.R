#!/usr/bin/env Rscript
# Diagnostic Script: Sample Count Analysis
# Run this to understand why sample counts are high

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("SAMPLE COUNT DIAGNOSTIC REPORT\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

source("R/core/extraction_data_utils.R")

# Part 1: Raw File Analysis
cat("PART 1: RAW FILE ANALYSIS\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

extraction_dir <- "data/extractions"
files <- list.files(extraction_dir, pattern = "\\.xlsx$", full.names = TRUE)

cat(sprintf("Found %d Excel files in %s\n\n", length(files), extraction_dir))

# Analyze each file individually
file_stats <- lapply(seq_along(files), function(i) {
  file <- files[i]
  filename <- basename(file)

  tryCatch({
    # Read raw Excel data
    raw_df <- read_excel(file)
    total_rows <- nrow(raw_df)

    # Count non-empty rows (at least one non-NA value)
    non_empty <- raw_df %>%
      filter(if_any(everything(), ~ !is.na(.) & . != "")) %>%
      nrow()

    # Look for barcode column
    barcode_col <- grep("barcode|code.*barre", names(raw_df), ignore.case = TRUE, value = TRUE)
    barcode_count <- 0
    if (length(barcode_col) > 0) {
      barcode_count <- sum(!is.na(raw_df[[barcode_col[1]]]) & raw_df[[barcode_col[1]]] != "")
    }

    list(
      file = filename,
      total_rows = total_rows,
      non_empty_rows = non_empty,
      barcodes = barcode_count,
      status = "OK"
    )
  }, error = function(e) {
    list(
      file = filename,
      total_rows = NA,
      non_empty_rows = NA,
      barcodes = NA,
      status = paste("ERROR:", e$message)
    )
  })
})

file_df <- bind_rows(file_stats)

cat("Individual file statistics:\n")
print(file_df %>% arrange(desc(non_empty_rows)), n = Inf)

cat("\n")
cat(sprintf("Total raw rows across all files: %s\n",
            scales::comma(sum(file_df$total_rows, na.rm = TRUE))))
cat(sprintf("Total non-empty rows: %s\n",
            scales::comma(sum(file_df$non_empty_rows, na.rm = TRUE))))
cat(sprintf("Total barcodes: %s\n\n",
            scales::comma(sum(file_df$barcodes, na.rm = TRUE))))

# Part 2: Processed Data Analysis
cat("\n")
cat("PART 2: PROCESSED DATA ANALYSIS (after cleaning)\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

cat("Loading and processing data (watch for log messages)...\n\n")

# Capture the loading process with messages
df <- load_all_extractions(extraction_dir)

cat("\n")
cat(sprintf("After processing: %s total samples\n\n", scales::comma(nrow(df))))

# Check samples per source file
if ("source_file" %in% names(df)) {
  cat("Samples per file (after deduplication and cleaning):\n")
  samples_per_file <- df %>%
    group_by(source_file) %>%
    summarise(samples = n()) %>%
    arrange(desc(samples))

  print(samples_per_file, n = Inf)

  cat("\n")
  cat(sprintf("Mean samples per file: %.1f\n", mean(samples_per_file$samples)))
  cat(sprintf("Median samples per file: %.1f\n", median(samples_per_file$samples)))
  cat(sprintf("Min samples per file: %d\n", min(samples_per_file$samples)))
  cat(sprintf("Max samples per file: %d\n", max(samples_per_file$samples)))
}

# Part 3: Duplicate Analysis
cat("\n\n")
cat("PART 3: DUPLICATE ANALYSIS\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

# Check for duplicates by barcode
if ("barcode" %in% names(df)) {
  dup_barcodes <- df %>%
    filter(!is.na(barcode) & barcode != "") %>%
    group_by(barcode) %>%
    summarise(
      count = n(),
      files = paste(unique(source_file), collapse = ", ")
    ) %>%
    filter(count > 1) %>%
    arrange(desc(count))

  if (nrow(dup_barcodes) > 0) {
    cat(sprintf("⚠️  Found %d barcodes appearing multiple times:\n\n", nrow(dup_barcodes)))
    print(dup_barcodes, n = min(20, nrow(dup_barcodes)))

    if (nrow(dup_barcodes) > 20) {
      cat(sprintf("\n... and %d more\n", nrow(dup_barcodes) - 20))
    }
  } else {
    cat("✅ No duplicate barcodes found (all barcodes are unique)\n")
  }
} else {
  cat("⚠️  No barcode column found in processed data\n")
}

# Part 4: Summary Metrics
cat("\n\n")
cat("PART 4: SUMMARY METRICS\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

metrics <- summarise_extraction_metrics(df)

cat(sprintf("Files with barcodes: %s\n", scales::comma(metrics$files_with_barcodes)))
cat(sprintf("Total samples: %s\n", scales::comma(metrics$total)))

if (!is.na(metrics$linked_total)) {
  cat(sprintf("Linked to biobank: %s\n", scales::comma(metrics$linked_total)))
}

# Part 5: Recommendations
cat("\n\n")
cat("PART 5: INTERPRETATION\n")
cat("-" %>% rep(80) %>% paste(collapse = ""), "\n\n")

avg_per_file <- nrow(df) / length(files)

if (avg_per_file < 10) {
  cat("✅ NORMAL: Low sample count (~%.1f per file) - typical for small batches\n", avg_per_file)
} else if (avg_per_file <= 50) {
  cat(sprintf("✅ NORMAL: Moderate sample count (~%.1f per file) - typical for daily extractions\n", avg_per_file))
} else if (avg_per_file <= 100) {
  cat(sprintf("⚠️  HIGH: ~%.1f samples per file - verify this matches your lab workflow\n", avg_per_file))
} else {
  cat(sprintf("🚨 VERY HIGH: ~%.1f samples per file - INVESTIGATE for potential issues:\n", avg_per_file))
  cat("   - Duplicate file exports?\n")
  cat("   - Multiple tabs/sheets being combined?\n")
  cat("   - Test data mixed with production?\n")
}

cat("\n")
cat("Next steps:\n")
cat("1. Manually open 2-3 files and verify row counts match this report\n")
cat("2. Check if the total (", scales::comma(nrow(df)), ") matches your lab records\n")
cat("3. If counts seem wrong, investigate files with unusually high counts\n")

cat("\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n")
cat("END OF DIAGNOSTIC REPORT\n")
cat("=" %>% rep(80) %>% paste(collapse = ""), "\n\n")
