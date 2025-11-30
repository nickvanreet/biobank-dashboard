#!/usr/bin/env Rscript
# Create a tidy assay table for dashboard QA and offline validation

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(readr)
})

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

source(file.path("R", "data", "dashboard_data_utils.R"))
source("global.R")

option_list <- list(
  make_option(c("-s", "--site"), type = "character", default = config$current_site,
              help = "Site identifier from config.yml [default %default]"),
  make_option(c("-o", "--output"), type = "character", default = "data/dashboard_tidy_assays.csv",
              help = "Where to write the tidy assay table (csv/rds supported by extension)"),
  make_option(c("-f", "--format"), type = "character", default = NULL,
              help = "Force output format (csv or rds). Defaults to inferred from extension."),
  make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
              help = "Print summary stats to the console")
)

opt <- parse_args(OptionParser(option_list = option_list))
site_paths <- get_site_paths(opt$site)

# Load core datasets using existing utilities
biobank_file <- get_latest_biobank_file(site_paths$biobank_dir)
biobank_df <- NULL
if (!is.null(biobank_file)) {
  biobank_df <- load_biobank_file_cached(biobank_file, cache_dir = site_paths$cache_dir) %>%
    clean_biobank_data_improved()
}

ielisa_df <- tryCatch(
  load_ielisa_data(site_paths$ielisa_dir, cache_dir = site_paths$cache_dir),
  error = function(e) NULL
)

elisa_df <- tryCatch(
  load_elisa_data(dirs = c(site_paths$elisa_pe_dir, site_paths$elisa_vsg_dir), biobank_df = biobank_df),
  error = function(e) NULL
)

mic_obj <- tryCatch({
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
  parse_mic_directory(site_paths$mic_dir, mic_settings, cache_state = list())
}, error = function(e) NULL)

prepared <- prepare_assay_dashboard_data(
  biobank_df = biobank_df,
  elisa_df = elisa_df,
  ielisa_df = ielisa_df,
  mic_data = mic_obj,
  filters = list(date_range = NULL)
)

tidy <- prepared$tidy_assays

# Derive concordance categories for quick QA
sample_summary <- tidy %>%
  mutate(is_positive = status == "Positive") %>%
  group_by(sample_id) %>%
  summarise(
    assays_tested = n_distinct(assay),
    positives = sum(is_positive, na.rm = TRUE),
    unique_positive = positives == 1,
    all_positive = positives == assays_tested,
    none_positive = positives == 0,
    last_assay_date = max(assay_date, na.rm = TRUE),
    .groups = "drop"
  )

output_format <- opt$format %||% tools::file_ext(opt$output)
if (tolower(output_format) %in% c("rds")) {
  saveRDS(list(tidy_assays = tidy, sample_summary = sample_summary), opt$output)
} else {
  readr::write_csv(tidy, opt$output)
}

if (isTRUE(opt$verbose)) {
  cat("Tidy assay rows:", nrow(tidy), "\n")
  cat("Samples with assays:", nrow(sample_summary), "\n")
  cat("Unique positives (single-assay positives):", sum(sample_summary$unique_positive, na.rm = TRUE), "\n")
  cat("Assay coverage by type:\n")
  print(tidy %>% count(assay, status, name = "n"))
}

# Documented assumptions for transparency
cat("Assumptions: ELISA positivity at PP% >= 20 or DOD >= 0.3; borderline ranges 15-20% PP% or 0.2-0.3 DOD.\n")
cat("iELISA positivity at >= 30% inhibition; borderline 25-30%. MIC positive calls matched on final call strings.\n")
cat("Missing metrics are kept as 'Missing' to surface gaps rather than silently dropping samples.\n")
