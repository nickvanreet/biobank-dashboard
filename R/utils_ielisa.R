# R/utils_ielisa.R
# Utilities for loading and parsing iELISA (inhibition ELISA) data
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

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Safely convert to numeric
#' @param x Value to convert
#' @return Numeric value or NA
safe_num <- function(x){
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", ".", gsub("%", "", as.character(x)))))
}

#' Convert column letter to index
#' @param letter Column letter (A-Z)
#' @return Numeric column index
col_to_idx <- function(letter) match(letter, LETTERS)

# ============================================================================
# CONTROL WELL EXTRACTION
# ============================================================================

#' Extract control wells from row 13 of OD plate
#' Control wells are at fixed positions:
#' - LiTat 1.3 NEG: B13, C13
#' - LiTat 1.3 POS: D13, E13
#' - LiTat 1.5 NEG: H13, I13
#' - LiTat 1.5 POS: J13, K13
#'
#' @param od OD data frame (96-well plate layout)
#' @return Tibble with control OD values, means, and CVs
extract_controls <- function(od){
  rw <- 13

  L13_neg <- c(safe_num(od[rw, 2]), safe_num(od[rw, 3]))   # B,C
  L13_pos <- c(safe_num(od[rw, 4]), safe_num(od[rw, 5]))   # D,E

  L15_neg <- c(safe_num(od[rw, 8]), safe_num(od[rw, 9]))   # H,I
  L15_pos <- c(safe_num(od[rw,10]), safe_num(od[rw,11]))   # J,K

  mm <- function(x){
    m <- mean(x, na.rm=TRUE)
    sd <- sd(x, na.rm=TRUE)
    cv <- ifelse(m == 0, NA, 100*sd/m)
    list(mean=m, cv=cv)
  }

  tibble(
    OD_L13_neg_1 = L13_neg[1],
    OD_L13_neg_2 = L13_neg[2],
    OD_L13_pos_1 = L13_pos[1],
    OD_L13_pos_2 = L13_pos[2],
    OD_L15_neg_1 = L15_neg[1],
    OD_L15_neg_2 = L15_neg[2],
    OD_L15_pos_1 = L15_pos[1],
    OD_L15_pos_2 = L15_pos[2],

    OD_L13_neg_mean = mm(L13_neg)$mean,
    OD_L13_pos_mean = mm(L13_pos)$mean,
    OD_L15_neg_mean = mm(L15_neg)$mean,
    OD_L15_pos_mean = mm(L15_pos)$mean,

    OD_L13_neg_cv = mm(L13_neg)$cv,
    OD_L13_pos_cv = mm(L13_pos)$cv,
    OD_L15_neg_cv = mm(L15_neg)$cv,
    OD_L15_pos_cv = mm(L15_pos)$cv
  )
}

# ============================================================================
# SAMPLE WELL MAPPING
# ============================================================================

#' Calculate Excel row for a given sample index
#' @param n Sample index (1-44)
#' @return Excel row number
row_for_sample <- function(n){
  if (n <= 2) 13 else 14 + floor((n - 3) / 6)
}

# Column mappings for the two antigens
col13_letters <- c("F","G","B","C","D","E")  # LiTat 1.3 wells
col15_letters <- c("L","M","H","I","J","K")  # LiTat 1.5 wells

# ============================================================================
# MAIN PARSER
# ============================================================================

#' Parse a single iELISA file (version 10)
#'
#' Parses an iELISA Excel file with the following structure:
#' - Sheet "450-600 nm": Contains OD values in 96-well layout
#' - Sheet "ECHANTILLONS": Contains sample metadata (LabID, Barcode)
#'
#' @param path Path to Excel file
#' @param neg_od_min Minimum acceptable negative control OD (default: 1)
#' @param neg_od_max Maximum acceptable negative control OD (default: 3)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 50)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 80)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @return Tibble with parsed control and sample data
parse_ielisa_file <- function(path,
                               neg_od_min = 1,
                               neg_od_max = 3,
                               pos_od_min = 0.3,
                               pos_od_max = 0.7,
                               ctrl_inh_min = 50,
                               ctrl_inh_max = 80,
                               ctrl_cv_max = 20){

  # --- Load sheets ----------------------------------------------------------------
  od <- read_excel(path, sheet="450-600 nm", col_names=FALSE)
  ech <- read_excel(path, sheet="ECHANTILLONS", col_names=FALSE)

  names(ech) <- paste0("X", seq_len(ncol(ech)))

  # --- Read flexible sample list ---------------------------------------------------
  samples <- ech %>%
    slice(15:n()) %>%            # skip headers
    filter(!is.na(X2)) %>%       # LabID present
    transmute(
      SampleIndex = row_number(),
      LabID = X2,
      Barcode = X3,
      # Add aliases for consistency with other modules
      numero_labo = X2,
      code_barres_kps = X3
    )

  nS <- nrow(samples)

  if (nS == 0){
    warning("No valid samples found in ECHANTILLONS for: ", basename(path))
    return(NULL)
  }

  # --- Extract control wells --------------------------------------------------------
  ctrl <- extract_controls(od)

  # --- Mapping wells ---------------------------------------------------------------
  samples <- samples %>%
    rowwise() %>%
    mutate(
      ExcelRow = row_for_sample(SampleIndex),

      Col13 = col13_letters[(SampleIndex-1) %% 6 + 1],
      Col15 = col15_letters[(SampleIndex-1) %% 6 + 1],

      idx13 = col_to_idx(Col13),
      idx15 = col_to_idx(Col15),

      OD_L13 = safe_num(od[ExcelRow, idx13]),
      OD_L15 = safe_num(od[ExcelRow, idx15])
    ) %>% ungroup()

  # ---------------------------------------------------------
  # % inhibition formula 1 (simple NEG-based)
  # ---------------------------------------------------------
  samples <- samples %>%
    mutate(
      pct_inh_f1_13 = 100 * (1 - OD_L13 / ctrl$OD_L13_neg_mean),
      pct_inh_f1_15 = 100 * (1 - OD_L15 / ctrl$OD_L15_neg_mean)
    )

  # ---------------------------------------------------------
  # % inhibition formula 2 (linear between NEG and POS)
  # ---------------------------------------------------------
  samples <- samples %>%
    mutate(
      pct_inh_f2_13 =
        100 * (ctrl$OD_L13_neg_mean - OD_L13) /
        (ctrl$OD_L13_neg_mean - ctrl$OD_L13_pos_mean),

      pct_inh_f2_15 =
        100 * (ctrl$OD_L15_neg_mean - OD_L15) /
        (ctrl$OD_L15_neg_mean - ctrl$OD_L15_pos_mean)
    )

  # ---------------------------------------------------------
  # FORMULA CONSISTENCY
  # ---------------------------------------------------------
  samples <- samples %>%
    mutate(
      diff_f1_f2_13 = abs(pct_inh_f1_13 - pct_inh_f2_13),
      diff_f1_f2_15 = abs(pct_inh_f1_15 - pct_inh_f2_15),
      qc_formula_agree_13 = diff_f1_f2_13 < 25,
      qc_formula_agree_15 = diff_f1_f2_15 < 25
    )

  # ---------------------------------------------------------
  # PLATE QC
  # ---------------------------------------------------------
  pct_inh_pos_13_f1 <- 100*(1 - ctrl$OD_L13_pos_mean / ctrl$OD_L13_neg_mean)
  pct_inh_pos_15_f1 <- 100*(1 - ctrl$OD_L15_pos_mean / ctrl$OD_L15_neg_mean)

  # Use configurable thresholds for control QC
  neg_ok_13 <- ctrl$OD_L13_neg_mean > neg_od_min & ctrl$OD_L13_neg_mean < neg_od_max
  pos_ok_13 <- ctrl$OD_L13_pos_mean > pos_od_min & ctrl$OD_L13_pos_mean < pos_od_max
  pos_inh_ok_13 <- pct_inh_pos_13_f1 > ctrl_inh_min & pct_inh_pos_13_f1 < ctrl_inh_max
  cv_ok_13 <- ctrl$OD_L13_neg_cv < ctrl_cv_max & ctrl$OD_L13_pos_cv < ctrl_cv_max

  neg_ok_15 <- ctrl$OD_L15_neg_mean > neg_od_min & ctrl$OD_L15_neg_mean < neg_od_max
  pos_ok_15 <- ctrl$OD_L15_pos_mean > pos_od_min & ctrl$OD_L15_pos_mean < pos_od_max
  pos_inh_ok_15 <- pct_inh_pos_15_f1 > ctrl_inh_min & pct_inh_pos_15_f1 < ctrl_inh_max
  cv_ok_15 <- ctrl$OD_L15_neg_cv < ctrl_cv_max & ctrl$OD_L15_pos_cv < ctrl_cv_max

  plate_qc <- tibble(
    plate_valid_L13 = neg_ok_13 & pos_ok_13 & pos_inh_ok_13 & cv_ok_13,
    plate_valid_L15 = neg_ok_15 & pos_ok_15 & pos_inh_ok_15 & cv_ok_15,
    pct_inh_pos_13 = pct_inh_pos_13_f1,
    pct_inh_pos_15 = pct_inh_pos_15_f1
  )

  # ---------------------------------------------------------
  # SAMPLE POSITIVITY (30% inhibition threshold)
  # ---------------------------------------------------------
  samples <- samples %>%
    mutate(
      positive_L13 = pct_inh_f2_13 >= 30,
      positive_L15 = pct_inh_f2_15 >= 30
    )

  # ---------------------------------------------------------
  # DUPLICATE DETECTION (within file)
  # ---------------------------------------------------------
  samples <- samples %>%
    group_by(LabID) %>%
    mutate(
      dup_LabID = n() > 1,
      mismatch_barcode = ifelse(dup_LabID & n_distinct(Barcode) > 1, TRUE, FALSE)
    ) %>%
    ungroup()

  # ---------------------------------------------------------
  # EXTRACT PLATE DATE FROM FILENAME (YYMMDD format)
  # ---------------------------------------------------------
  filename <- basename(path)
  date_str <- str_extract(filename, "^\\d{6}")
  plate_date <- if (!is.na(date_str)) {
    as.Date(date_str, format = "%y%m%d")
  } else {
    as.Date(NA)
  }

  # ---------------------------------------------------------
  # BUILD OUTPUT
  # ---------------------------------------------------------
  bind_cols(
    tibble(
      file = filename,
      plate_date = plate_date
    ),
    ctrl,
    plate_qc,
    samples
  )
}

# ============================================================================
# FOLDER PARSER
# ============================================================================

#' Parse all iELISA files in a folder
#'
#' @param folder Path to folder containing iELISA Excel files
#' @param neg_od_min Minimum acceptable negative control OD (default: 1)
#' @param neg_od_max Maximum acceptable negative control OD (default: 3)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 50)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 80)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @return Combined tibble with all parsed data
parse_ielisa_folder <- function(folder,
                                 neg_od_min = 1,
                                 neg_od_max = 3,
                                 pos_od_min = 0.3,
                                 pos_od_max = 0.7,
                                 ctrl_inh_min = 50,
                                 ctrl_inh_max = 80,
                                 ctrl_cv_max = 20){
  files <- list.files(folder, pattern="\\.xlsx$", full.names=TRUE)

  if (length(files) == 0) {
    warning("No Excel files found in: ", folder)
    return(tibble())
  }

  message("Found ", length(files), " iELISA files:")
  message(paste(basename(files), collapse="\n"))

  res <- map(files, ~{
    message("Parsing: ", basename(.x))
    tryCatch(
      parse_ielisa_file(.x,
                        neg_od_min = neg_od_min,
                        neg_od_max = neg_od_max,
                        pos_od_min = pos_od_min,
                        pos_od_max = pos_od_max,
                        ctrl_inh_min = ctrl_inh_min,
                        ctrl_inh_max = ctrl_inh_max,
                        ctrl_cv_max = ctrl_cv_max),
      error=function(e){
        warning("Error in ", basename(.x), ": ", e$message)
        NULL
      }
    )
  })

  res <- res[!map_lgl(res, is.null)]

  if (length(res)==0) {
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
    group_by(LabID, Barcode) %>%
    summarise(
      n_runs = n_distinct(file),
      run_files = paste(unique(file), collapse = "; "),
      .groups = "drop"
    ) %>%
    filter(n_runs > 1)  # only samples tested ≥2 times

  # Join back to mark duplicates
  result <- ielisa_data %>%
    left_join(dup_summary %>% select(LabID, Barcode, n_runs, run_files),
              by = c("LabID", "Barcode")) %>%
    mutate(
      duplicate_across_files = !is.na(n_runs) & n_runs > 1
    )

  # Flag mismatches where LabID matches but Barcode differs
  barcode_mismatch <- ielisa_data %>%
    group_by(LabID) %>%
    summarise(n_barcodes = n_distinct(Barcode, na.rm = TRUE), .groups = "drop") %>%
    filter(n_barcodes > 1)

  result <- result %>%
    mutate(
      barcode_conflict = LabID %in% barcode_mismatch$LabID
    )

  # Flag mismatches where Barcode matches but LabID differs
  labid_mismatch <- ielisa_data %>%
    group_by(Barcode) %>%
    summarise(n_labids = n_distinct(LabID, na.rm = TRUE), .groups = "drop") %>%
    filter(n_labids > 1)

  result <- result %>%
    mutate(
      labid_conflict = Barcode %in% labid_mismatch$Barcode
    )

  result
}

# ============================================================================
# RECALCULATE SAMPLE QC WITH CUSTOM SETTINGS
# ============================================================================

#' Recalculate sample positivity with custom threshold and formula
#'
#' Applies user-selected threshold and formula to determine sample positivity
#'
#' @param ielisa_data iELISA data frame with both formulas calculated
#' @param threshold Positivity threshold (% inhibition, default 30)
#' @param formula Formula to use ("f1" or "f2", default "f2")
#' @return Data frame with updated positive_L13 and positive_L15 columns
apply_custom_qc <- function(ielisa_data, threshold = 30, formula = "f2") {
  if (nrow(ielisa_data) == 0) {
    return(ielisa_data)
  }

  # Select the appropriate columns based on formula choice
  if (formula == "f1") {
    ielisa_data <- ielisa_data %>%
      mutate(
        positive_L13 = pct_inh_f1_13 >= threshold,
        positive_L15 = pct_inh_f1_15 >= threshold
      )
  } else {  # f2
    ielisa_data <- ielisa_data %>%
      mutate(
        positive_L13 = pct_inh_f2_13 >= threshold,
        positive_L15 = pct_inh_f2_15 >= threshold
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
#' @param neg_od_max Maximum acceptable negative control OD (default: 3)
#' @param pos_od_min Minimum acceptable positive control OD (default: 0.3)
#' @param pos_od_max Maximum acceptable positive control OD (default: 0.7)
#' @param ctrl_inh_min Minimum acceptable control inhibition % (default: 50)
#' @param ctrl_inh_max Maximum acceptable control inhibition % (default: 80)
#' @param ctrl_cv_max Maximum acceptable control CV % (default: 20)
#' @return Tibble with all iELISA data
load_ielisa_data <- function(ielisa_dir = NULL,
                              cache_dir = NULL,
                              neg_od_min = 1,
                              neg_od_max = 3,
                              pos_od_min = 0.3,
                              pos_od_max = 0.7,
                              ctrl_inh_min = 50,
                              ctrl_inh_max = 80,
                              ctrl_cv_max = 20) {

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
                     ctrl_inh_min, ctrl_inh_max, ctrl_cv_max, sep = "_")
  combined_hash <- digest(paste(paste(all_hashes, collapse = ""), qc_params, sep = "_"), algo = "md5")
  cache_file <- file.path(cache_dir, paste0("ielisa_", combined_hash, ".rds"))

  # Try to load from cache
  if (file.exists(cache_file)) {
    message("Loading iELISA data from cache...")
    return(readRDS(cache_file))
  }

  # Parse all files
  message("Parsing ", length(files), " iELISA files...")
  data <- parse_ielisa_folder(ielisa_dir,
                               neg_od_min = neg_od_min,
                               neg_od_max = neg_od_max,
                               pos_od_min = pos_od_min,
                               pos_od_max = pos_od_max,
                               ctrl_inh_min = ctrl_inh_min,
                               ctrl_inh_max = ctrl_inh_max,
                               ctrl_cv_max = ctrl_cv_max)

  # Add duplicate detection
  if (nrow(data) > 0) {
    data <- add_duplicate_detection(data)
  }

  # Save to cache
  saveRDS(data, cache_file)
  message("Cached iELISA data to: ", cache_file)

  data
}
