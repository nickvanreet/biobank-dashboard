# =============================================================================
# Shared ELISA Utilities
# Common functions for ELISA-PE and ELISA-VSG processing
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
})

# =============================================================================
# QC SETTINGS
# =============================================================================

#' Create default ELISA QC settings
#' @return List of QC threshold parameters
#' @export
elisa_default_qc_settings <- function() {
  list(
    # Control validation thresholds
    pos_control_min_od = 0.5,
    pos_control_max_od = 1.5,
    neg_control_max_od = 0.5,
    cv_max_ag_plus = 20,      # Maximum CV% for Ag+ replicates
    cv_max_ag0 = 20,          # Maximum CV% for Ag0 replicates

    # Sample classification thresholds
    sample_pp_cutoff = 20,     # PP% threshold for positivity
    sample_dod_cutoff = 0.3,   # DOD threshold for positivity
    borderline_pp_min = 15,    # Borderline range: 15-20%
    borderline_dod_min = 0.2   # Borderline range: 0.2-0.3
  )
}

# =============================================================================
# PARSING HELPERS
# =============================================================================

#' Parse deepwell pattern from string
#' Handles formats like "A1/A2" or "A1 ou A2" or "A1;A2"
#' @param x String to parse
#' @return Character vector of well positions
#' @export
parse_deepwell_pattern <- function(x) {
  if (is.na(x) || x == "") return(character(0))

  x %>%
    as.character() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("ou", "/") %>%
    str_replace_all(";", "/") %>%
    str_replace_all(",", "/") %>%
    str_split("/") %>%
    purrr::pluck(1)
}

#' Ensure expected columns are present in data frame
#' @param df Data frame
#' @param cols Character vector of column names
#' @param default Default value for missing columns
#' @return Data frame with all expected columns
#' @export
ensure_columns <- function(df, cols, default = NA_character_) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- default
    }
  }
  df
}

#' Parse plate date from filename
#' Handles formats: YYMMDD or YYYYMMDD at start of filename
#' @param filename Filename string
#' @return Date object or NA
#' @export
parse_plate_date_from_filename <- function(filename) {
  stem   <- basename(filename)
  digits <- str_extract(stem, "^\\d+")
  if (is.na(digits)) return(as.Date(NA))

  if (nchar(digits) == 6) {
    yy <- substr(digits, 1, 2)
    mm <- substr(digits, 3, 4)
    dd <- substr(digits, 5, 6)
    full <- paste0("20", yy, "-", mm, "-", dd)
    return(as.Date(full))
  }

  if (nchar(digits) == 8) {
    yyyy <- substr(digits, 1, 4)
    mm   <- substr(digits, 5, 6)
    dd   <- substr(digits, 7, 8)
    full <- paste0(yyyy, "-", mm, "-", dd)
    return(as.Date(full))
  }

  as.Date(NA)
}

# =============================================================================
# STATISTICAL HELPERS
# =============================================================================

#' Calculate coefficient of variation (CV%)
#' @param val1 First replicate value
#' @param val2 Second replicate value
#' @return CV% or NA if insufficient data
#' @export
calculate_cv <- function(val1, val2) {
  if (is.na(val1) || is.na(val2)) return(NA_real_)
  vals <- c(val1, val2)
  cv <- (sd(vals) / mean(vals)) * 100
  return(cv)
}

#' Calculate DOD (Delta OD) from Ag+ and Ag0 means
#' @param mean_ag_plus Mean OD for Ag+
#' @param mean_ag0 Mean OD for Ag0
#' @return DOD value
#' @export
calculate_dod <- function(mean_ag_plus, mean_ag0) {
  mean_ag_plus - mean_ag0
}

#' Calculate PP% (Percent Positivity)
#' @param dod Sample DOD
#' @param pc_dod Positive control DOD
#' @param nc_dod Negative control DOD
#' @return PP% value
#' @export
calculate_pp_percent <- function(dod, pc_dod, nc_dod) {
  pp_fraction <- (dod - nc_dod) / (pc_dod - nc_dod)
  pp_percent <- pp_fraction * 100
  return(pp_percent)
}

# =============================================================================
# CLASSIFICATION HELPERS
# =============================================================================

#' Classify sample status based on PP% and DOD
#' @param pp_percent Percent positivity
#' @param dod Delta OD
#' @param qc_settings QC settings list
#' @return Character vector: "Positive", "Borderline", "Negative", or "Invalid"
#' @export
classify_elisa_sample <- function(pp_percent, dod, qc_settings = elisa_default_qc_settings()) {
  case_when(
    is.na(pp_percent) | is.na(dod) ~ "Invalid",

    # Positive: PP% >= 20 OR DOD >= 0.3
    pp_percent >= qc_settings$sample_pp_cutoff |
      dod >= qc_settings$sample_dod_cutoff ~ "Positive",

    # Borderline: PP% 15-20 OR DOD 0.2-0.3
    (pp_percent >= qc_settings$borderline_pp_min &
       pp_percent < qc_settings$sample_pp_cutoff) |
      (dod >= qc_settings$borderline_dod_min &
         dod < qc_settings$sample_dod_cutoff) ~ "Borderline",

    # Negative: Below borderline thresholds
    TRUE ~ "Negative"
  )
}

# =============================================================================
# METADATA EXTRACTION
# =============================================================================

#' Extract plate metadata from filename and path
#' @param file_path Full path to ELISA file
#' @return List with plate_id, plate_date, elisa_type
#' @export
extract_plate_metadata <- function(file_path) {
  plate_id <- tools::file_path_sans_ext(basename(file_path))
  plate_date <- parse_plate_date_from_filename(basename(file_path))

  # Infer ELISA type from path
  elisa_type <- if (grepl("elisa_vsg|/vsg/", file_path, ignore.case = TRUE)) {
    "ELISA_vsg"
  } else if (grepl("elisa_pe|/pe/", file_path, ignore.case = TRUE)) {
    "ELISA_pe"
  } else {
    "ELISA_pe"  # Default to PE
  }

  list(
    plate_id = plate_id,
    plate_date = plate_date,
    elisa_type = elisa_type,
    source_path = file_path
  )
}

# =============================================================================
# CONTROL IDENTIFICATION
# =============================================================================

#' Identify control type from sample name or code
#' @param sample Sample position (e.g., "S1", "PC1", "NC1")
#' @param sample_code Sample code (e.g., "CP", "CN")
#' @return "positive_control", "negative_control", or NA
#' @export
identify_control_type <- function(sample, sample_code) {
  case_when(
    grepl("^PC", sample, ignore.case = TRUE) | sample_code == "CP" ~ "positive_control",
    grepl("^NC", sample, ignore.case = TRUE) | sample_code == "CN" ~ "negative_control",
    TRUE ~ NA_character_
  )
}

# =============================================================================
# DATA VALIDATION
# =============================================================================

#' Validate OD value is within reasonable range
#' @param od OD value
#' @param min_od Minimum valid OD (default 0)
#' @param max_od Maximum valid OD (default 4)
#' @return Logical
#' @export
validate_od_range <- function(od, min_od = 0, max_od = 4) {
  !is.na(od) & od >= min_od & od <= max_od
}

#' Create QC flags summary (vectorized)
#' @param qc_ag_plus QC pass for Ag+ (logical vector)
#' @param qc_ag0 QC pass for Ag0 (logical vector)
#' @return Character vector of QC flags
#' @export
create_qc_flags <- function(qc_ag_plus, qc_ag0) {
  purrr::map2_chr(qc_ag_plus, qc_ag0, function(plus, zero) {
    flags <- character()

    if (!is.na(plus) && !plus) {
      flags <- c(flags, "High CV Ag+")
    }

    if (!is.na(zero) && !zero) {
      flags <- c(flags, "High CV Ag0")
    }

    if (length(flags) == 0) {
      return(NA_character_)
    }

    paste(flags, collapse = "; ")
  })
}
