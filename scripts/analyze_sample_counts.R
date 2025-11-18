#!/usr/bin/env Rscript

# Analyze sample counts in extraction files
library(dplyr)
library(purrr)

source("R/core/extraction_data_utils.R")

# Get all extraction files
extraction_dir <- "data/extractions"
files <- list.files(extraction_dir, pattern = "\\.xlsx$", full.names = TRUE)

cat(sprintf("Found %d extraction files\n\n", length(files)))

# Analyze each file
file_analysis <- map_dfr(files, function(file) {
  tryCatch({
    df <- readxl::read_excel(file)

    # Count total rows
    total_rows <- nrow(df)

    # Count non-empty rows (rows with at least some data)
    non_empty_rows <- sum(apply(df, 1, function(row) any(!is.na(row) & row != "")))

    # Try to find barcode column
    barcode_cols <- grep("barcode|code.?barre", names(df), ignore.case = TRUE, value = TRUE)
    has_barcodes <- length(barcode_cols) > 0

    barcode_count <- 0
    if (has_barcodes) {
      barcode_col <- barcode_cols[1]
      barcodes <- df[[barcode_col]]
      barcode_count <- sum(!is.na(barcodes) & barcodes != "")
    }

    data.frame(
      file = basename(file),
      total_rows = total_rows,
      non_empty_rows = non_empty_rows,
      has_barcodes = has_barcodes,
      barcode_count = barcode_count
    )
  }, error = function(e) {
    data.frame(
      file = basename(file),
      total_rows = NA,
      non_empty_rows = NA,
      has_barcodes = FALSE,
      barcode_count = 0,
      error = as.character(e$message)
    )
  })
})

# Print summary
cat("File-by-file analysis:\n")
cat("======================\n\n")
print(file_analysis %>% arrange(desc(total_rows)))

cat("\n\nSummary Statistics:\n")
cat("===================\n")
cat(sprintf("Total files: %d\n", nrow(file_analysis)))
cat(sprintf("Files with barcodes: %d\n", sum(file_analysis$has_barcodes)))
cat(sprintf("Total rows across all files: %d\n", sum(file_analysis$total_rows, na.rm = TRUE)))
cat(sprintf("Total non-empty rows: %d\n", sum(file_analysis$non_empty_rows, na.rm = TRUE)))
cat(sprintf("Total barcodes found: %d\n", sum(file_analysis$barcode_count, na.rm = TRUE)))
cat(sprintf("Average rows per file: %.1f\n", mean(file_analysis$total_rows, na.rm = TRUE)))
cat(sprintf("Median rows per file: %.1f\n", median(file_analysis$total_rows, na.rm = TRUE)))

# Now load using the actual app logic and compare
cat("\n\nLoading data using app logic:\n")
cat("==============================\n")
df_app <- load_all_extractions(extraction_dir)
metrics <- summarise_extraction_metrics(df_app)

cat(sprintf("Total samples (after deduplication): %d\n", metrics$total))
cat(sprintf("Files with barcodes: %d\n", metrics$files_with_barcodes))

# Check for duplicates
if ("source_file" %in% names(df_app)) {
  cat("\n\nSamples per source file:\n")
  cat("========================\n")
  samples_per_file <- df_app %>%
    group_by(source_file) %>%
    summarise(count = n()) %>%
    arrange(desc(count))
  print(samples_per_file)
}
