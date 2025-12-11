# R/utils_ielisa.R
# Utilities for loading and parsing iELISA (inhibition ELISA) data
# Refactored to use the new modular 4-step pipeline
# iELISA tests for LiTat 1.3 and LiTat 1.5 antigens

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(stringr)
  library(purrr)
  library(digest)
})

# Source modular pipeline components
source(file.path("R", "modules", "ielisa", "ingest_ielisa.R"), local = TRUE)
source(file.path("R", "modules", "ielisa", "qc_ielisa.R"), local = TRUE)
source(file.path("R", "modules", "ielisa", "interpret_ielisa.R"), local = TRUE)
source(file.path("R", "modules", "ielisa", "output_ielisa.R"), local = TRUE)

# ============================================================================
# MAIN PARSING FUNCTIONS (using modular pipeline)
# ============================================================================

#' Parse a single iELISA file using modular pipeline
#'
#' Runs the full 4-step modular pipeline:
#' 1. Ingest: Read OD values and sample metadata
#' 2. QC: Validate controls
#' 3. Interpret: Calculate % inhibition and classify samples
#' 4. Output: Format results
#'
#' @param path Path to Excel file
#' @param neg_od_min Minimum acceptable negative control OD (default: 1)
#' @param neg_od_max Maximum acceptable negative control OD (default: 3.1)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 30)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 100)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Formula to use ("f1" for NEG-based, "f2" for NEG-POS normalized, default: "f1")
#' @return Tibble with parsed control and sample data
parse_ielisa_file <- function(path,
                               neg_od_min = 1,
                               neg_od_max = 3.1,
                               pos_od_min = 0.3,
                               pos_od_max = 0.7,
                               ctrl_inh_min = 30,
                               ctrl_inh_max = 100,
                               ctrl_cv_max = 20,
                               threshold = 30,
                               formula = "f1") {

  # Build QC settings
  qc_settings <- list(
    neg_od_min = neg_od_min,
    neg_od_max = neg_od_max,
    pos_od_min = pos_od_min,
    pos_od_max = pos_od_max,
    ctrl_inh_min = ctrl_inh_min,
    ctrl_inh_max = ctrl_inh_max,
    ctrl_cv_max = ctrl_cv_max
  )

  # Run modular pipeline
  result <- process_ielisa_file(path, qc_settings, threshold, formula)

  # Return clean results (backward compatible format)
  return(result$results_clean)
}

#' Parse all iELISA files in a folder using modular pipeline
#'
#' @param folder Path to folder containing iELISA Excel files
#' @param neg_od_min Minimum acceptable negative control OD (default: 1)
#' @param neg_od_max Maximum acceptable negative control OD (default: 3.1)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 30)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 100)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Formula to use ("f1" or "f2", default: "f1")
#' @return Combined tibble with all parsed data
parse_ielisa_folder <- function(folder,
                                 neg_od_min = 1,
                                 neg_od_max = 3.1,
                                 pos_od_min = 0.3,
                                 pos_od_max = 0.7,
                                 ctrl_inh_min = 30,
                                 ctrl_inh_max = 100,
                                 ctrl_cv_max = 20,
                                 threshold = 30,
                                 formula = "f1") {
  files <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

  if (length(files) == 0) {
    warning("No Excel files found in: ", folder)
    return(tibble())
  }

  message("Found ", length(files), " iELISA files")

  res <- map(files, ~ {
    message("Processing: ", basename(.x))
    tryCatch(
      parse_ielisa_file(.x,
                        neg_od_min = neg_od_min,
                        neg_od_max = neg_od_max,
                        pos_od_min = pos_od_min,
                        pos_od_max = pos_od_max,
                        ctrl_inh_min = ctrl_inh_min,
                        ctrl_inh_max = ctrl_inh_max,
                        ctrl_cv_max = ctrl_cv_max,
                        threshold = threshold,
                        formula = formula),
      error = function(e) {
        warning("Error in ", basename(.x), ": ", e$message)
        NULL
      }
    )
  })

  res <- res[!map_lgl(res, is.null)]

  if (length(res) == 0) {
    warning("No files successfully parsed.")
    return(tibble())
  }

  bind_rows(res)
}

# ============================================================================
# CROSS-RUN DUPLICATE DETECTION
# ============================================================================

#' Add cross-run duplicate detection to iELISA data
#'
#' Identifies samples tested multiple times across different plates
#'
#' @param ielisa_data Combined iELISA data from multiple files
#' @return Data with duplicate flags added
add_duplicate_detection <- function(ielisa_data) {
  if (nrow(ielisa_data) == 0) {
    return(ielisa_data)
  }

  # Detect samples tested multiple times across all files
  dup_summary <- ielisa_data %>%
    group_by(numero_labo, code_barres_kps) %>%
    summarise(
      n_runs = n_distinct(file),
      run_files = paste(unique(file), collapse = "; "),
      .groups = "drop"
    ) %>%
    filter(n_runs > 1)  # only samples tested ≥2 times

  # Join back to mark duplicates
  result <- ielisa_data %>%
    left_join(dup_summary %>% select(numero_labo, code_barres_kps, n_runs, run_files),
              by = c("numero_labo", "code_barres_kps")) %>%
    mutate(
      duplicate_across_files = !is.na(n_runs) & n_runs > 1
    )

  # Flag mismatches where numero_labo matches but code_barres_kps differs
  barcode_mismatch <- ielisa_data %>%
    group_by(numero_labo) %>%
    summarise(n_barcodes = n_distinct(code_barres_kps, na.rm = TRUE), .groups = "drop") %>%
    filter(n_barcodes > 1)

  result <- result %>%
    mutate(
      barcode_conflict = numero_labo %in% barcode_mismatch$numero_labo
    )

  # Flag mismatches where code_barres_kps matches but numero_labo differs
  labid_mismatch <- ielisa_data %>%
    group_by(code_barres_kps) %>%
    summarise(n_labids = n_distinct(numero_labo, na.rm = TRUE), .groups = "drop") %>%
    filter(n_labids > 1)

  result <- result %>%
    mutate(
      labid_conflict = code_barres_kps %in% labid_mismatch$code_barres_kps
    )

  result
}

# ============================================================================
# RECALCULATE SAMPLE QC WITH CUSTOM SETTINGS
# ============================================================================

#' Recalculate sample positivity with custom threshold and formula
#'
#' Applies user-selected threshold and formula to determine sample positivity
#' with support for borderline classification.
#'
#' @param ielisa_data iELISA data frame with both formulas calculated
#' @param threshold Positivity threshold (% inhibition, default 30)
#' @param borderline_threshold Borderline threshold (% inhibition, default 25)
#' @param formula Formula to use ("f1" or "f2", default "f1")
#' @return Data frame with updated positive_L13, positive_L15, and status columns
apply_custom_qc <- function(ielisa_data, threshold = 30, borderline_threshold = 25, formula = "f1") {
  if (nrow(ielisa_data) == 0) {
    return(ielisa_data)
  }

  # Select the appropriate columns based on formula choice
  if (formula == "f1") {
    ielisa_data <- ielisa_data %>%
      mutate(
        positive_L13 = pct_inh_f1_13 >= threshold,
        positive_L15 = pct_inh_f1_15 >= threshold,
        borderline_L13 = pct_inh_f1_13 >= borderline_threshold & pct_inh_f1_13 < threshold,
        borderline_L15 = pct_inh_f1_15 >= borderline_threshold & pct_inh_f1_15 < threshold,
        sample_positive = positive_L13 | positive_L15,
        sample_borderline = !sample_positive & (borderline_L13 | borderline_L15),
        status_final = case_when(
          is.na(positive_L13) & is.na(positive_L15) ~ "Invalid",
          sample_positive ~ "Positive",
          sample_borderline ~ "Borderline",
          TRUE ~ "Negative"
        )
      )
  } else {  # f2
    ielisa_data <- ielisa_data %>%
      mutate(
        positive_L13 = pct_inh_f2_13 >= threshold,
        positive_L15 = pct_inh_f2_15 >= threshold,
        borderline_L13 = pct_inh_f2_13 >= borderline_threshold & pct_inh_f2_13 < threshold,
        borderline_L15 = pct_inh_f2_15 >= borderline_threshold & pct_inh_f2_15 < threshold,
        sample_positive = positive_L13 | positive_L15,
        sample_borderline = !sample_positive & (borderline_L13 | borderline_L15),
        status_final = case_when(
          is.na(positive_L13) & is.na(positive_L15) ~ "Invalid",
          sample_positive ~ "Positive",
          sample_borderline ~ "Borderline",
          TRUE ~ "Negative"
        )
      )
  }

  ielisa_data
}

# ============================================================================
# CACHED DATA LOADING
# ============================================================================

#' Load iELISA data with caching (site-aware)
#'
#' Loads all iELISA files from the specified directory with MD5 hash-based caching
#' for performance optimization
#'
#' @param ielisa_dir Path to iELISA data directory (default: uses site paths)
#' @param cache_dir Path to cache directory (default: uses site paths)
#' @param neg_od_min Minimum acceptable negative control OD (default: 1)
#' @param neg_od_max Maximum acceptable negative control OD (default: 3.1)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 30)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 100)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @param threshold Positivity threshold (default: 30%)
#' @param formula Formula to use ("f1" or "f2", default: "f1")
#' @return Tibble with all iELISA data
load_ielisa_data <- function(ielisa_dir = NULL,
                              cache_dir = NULL,
                              neg_od_min = 1,
                              neg_od_max = 3.1,
                              pos_od_min = 0.3,
                              pos_od_max = 0.7,
                              ctrl_inh_min = 30,
                              ctrl_inh_max = 100,
                              ctrl_cv_max = 20,
                              threshold = 30,
                              formula = "f1") {

  # Use site-aware paths if not specified
  if (is.null(ielisa_dir) && exists("config") && !is.null(config$site_paths)) {
    ielisa_dir <- config$site_paths$ielisa_dir
    cache_dir <- config$site_paths$cache_dir
  } else {
    # Fallback to legacy paths
    if (is.null(ielisa_dir)) ielisa_dir <- "data/ielisa"
    if (is.null(cache_dir)) cache_dir <- "data/ielisa_cache"
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  # Get all Excel files
  files <- list.files(ielisa_dir, pattern = "\\.xlsx$", full.names = TRUE)

  if (length(files) == 0) {
    message("No iELISA files found in: ", ielisa_dir)
    return(tibble())
  }

  # Compute hash of all files + QC parameters
  all_hashes <- sapply(files, digest, file = TRUE, algo = "md5")
  qc_params <- paste(neg_od_min, neg_od_max, pos_od_min, pos_od_max,
                     ctrl_inh_min, ctrl_inh_max, ctrl_cv_max, threshold, formula, sep = "_")
  combined_hash <- digest(paste(paste(all_hashes, collapse = ""), qc_params, sep = "_"), algo = "md5")
  cache_file <- file.path(cache_dir, paste0("ielisa_modular_", combined_hash, ".rds"))

  # Try to load from cache
  if (file.exists(cache_file)) {
    message("Loading iELISA data from cache...")
    return(readRDS(cache_file))
  }

  # Parse all files using modular pipeline
  message("Parsing ", length(files), " iELISA files using modular pipeline...")
  data <- parse_ielisa_folder(ielisa_dir,
                               neg_od_min = neg_od_min,
                               neg_od_max = neg_od_max,
                               pos_od_min = pos_od_min,
                               pos_od_max = pos_od_max,
                               ctrl_inh_min = ctrl_inh_min,
                               ctrl_inh_max = ctrl_inh_max,
                               ctrl_cv_max = ctrl_cv_max,
                               threshold = threshold,
                               formula = formula)

  # Add duplicate detection
  if (nrow(data) > 0) {
    data <- add_duplicate_detection(data)
  }

  # Save to cache
  saveRDS(data, cache_file)
  message("Cached iELISA data to: ", cache_file)

  data
}
