#!/usr/bin/env Rscript
# Quick check: How many rows does each Excel file actually have?

library(readxl)

files <- list.files("data/extractions", pattern = "\\.xlsx$", full.names = TRUE)

cat("\nQuick File Row Count\n")
cat("====================\n\n")

for (file in files) {
  df <- read_excel(file, col_types = "text")
  non_empty <- sum(apply(df, 1, function(row) any(!is.na(row) & row != "")))
  cat(sprintf("%-50s %4d rows\n", basename(file), non_empty))
}

cat("\nTotal files:", length(files), "\n")
cat("Total non-empty rows:", sum(sapply(files, function(f) {
  df <- read_excel(f, col_types = "text")
  sum(apply(df, 1, function(row) any(!is.na(row) & row != "")))
})), "\n\n")
