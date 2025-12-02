# =============================================================================
# iELISA Ingestion Module (STEP 1)
# Reads iELISA-specific format: OD values from 450-600 nm sheet,
# sample metadata from ECHANTILLONS sheet, fixed control positions
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Safely convert to numeric
#' @param x Value to convert
#' @return Numeric value or NA
safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", ".", gsub("%", "", as.character(x)))))
}

#' Convert column letter to index
#' @param letter Column letter (A-Z)
#' @return Numeric column index
col_to_idx <- function(letter) match(letter, LETTERS)

#' Calculate Excel row for a given sample index
#' Samples are arranged starting from row 13 (first 2 samples), then row 14+ (6 per row)
#' @param n Sample index (1-44)
#' @return Excel row number
row_for_sample <- function(n) {
  if (n <= 2) 13 else 14 + floor((n - 3) / 6)
}

# Column mappings for the two antigens
col13_letters <- c("F", "G", "B", "C", "D", "E")  # LiTat 1.3 wells (cycle through)
col15_letters <- c("L", "M", "H", "I", "J", "K")  # LiTat 1.5 wells (cycle through)

#' Parse date from filename (YYMMDD format)
#' @param filename File name string
#' @return Date object
parse_plate_date_from_filename <- function(filename) {
  date_str <- str_extract(filename, "^\\d{6}")
  if (!is.na(date_str)) {
    as.Date(date_str, format = "%y%m%d")
  } else {
    as.Date(NA)
  }
}

# =============================================================================
# STEP 1: INGESTION
# =============================================================================

#' Read iELISA OD grid from 450-600 nm sheet
#' @param path Path to Excel file
#' @return Raw OD data frame (entire plate)
ingest_ielisa_od_grid <- function(path) {
  od <- read_excel(path, sheet = "450-600 nm", col_names = FALSE, .name_repair = "minimal")

  # Return raw OD grid (96-well layout)
  # Rows are labeled, columns are numbered
  return(od)
}

#' Read iELISA sample metadata from ECHANTILLONS sheet
#' @param path Path to Excel file
#' @param plate_id Plate identifier
#' @return Tibble with sample metadata (LabID, Barcode)
ingest_ielisa_sample_metadata <- function(path, plate_id) {
  ech <- read_excel(path, sheet = "ECHANTILLONS", col_names = FALSE, .name_repair = "minimal")

  # Assign generic column names
  names(ech) <- paste0("X", seq_len(ncol(ech)))

  # Read sample list (skip first 14 rows - headers)
  # Sample data starts at row 15
  samples <- ech %>%
    dplyr::slice(-(1:14)) %>%
    dplyr::filter(!is.na(X2)) %>%  # LabID present
    dplyr::transmute(
      plate_id = plate_id,
      SampleIndex = dplyr::row_number(),
      numero_labo = as.character(X2),  # Lab ID
      code_barres_kps = as.character(X3)  # Barcode
    )

  return(samples)
}

#' Extract control wells from row 13 of OD plate
#' Control wells are at fixed positions:
#' - LiTat 1.3 NEG: B13, C13 (cols 2,3)
#' - LiTat 1.3 POS: D13, E13 (cols 4,5)
#' - LiTat 1.5 NEG: H13, I13 (cols 8,9)
#' - LiTat 1.5 POS: J13, K13 (cols 10,11)
#'
#' @param od OD data frame (96-well plate layout)
#' @return Tibble with control OD values
ingest_ielisa_controls <- function(od) {
  rw <- 13  # Control row

  tibble(
    # LiTat 1.3 controls
    OD_L13_neg_1 = safe_num(od[rw, 2]),  # B13
    OD_L13_neg_2 = safe_num(od[rw, 3]),  # C13
    OD_L13_pos_1 = safe_num(od[rw, 4]),  # D13
    OD_L13_pos_2 = safe_num(od[rw, 5]),  # E13

    # LiTat 1.5 controls
    OD_L15_neg_1 = safe_num(od[rw, 8]),  # H13
    OD_L15_neg_2 = safe_num(od[rw, 9]),  # I13
    OD_L15_pos_1 = safe_num(od[rw, 10]), # J13
    OD_L15_pos_2 = safe_num(od[rw, 11])  # K13
  )
}

#' Map sample wells to OD values
#' Uses complex well mapping algorithm specific to iELISA layout
#'
#' @param samples Sample metadata with SampleIndex
#' @param od OD grid
#' @return Samples with OD values mapped for both antigens
map_sample_wells <- function(samples, od) {
  samples %>%
    rowwise() %>%
    mutate(
      # Calculate Excel row for this sample
      ExcelRow = row_for_sample(SampleIndex),

      # Determine column letters (cycle through 6 columns)
      Col13 = col13_letters[(SampleIndex - 1) %% 6 + 1],  # LiTat 1.3
      Col15 = col15_letters[(SampleIndex - 1) %% 6 + 1],  # LiTat 1.5

      # Convert to column indices
      idx13 = col_to_idx(Col13),
      idx15 = col_to_idx(Col15),

      # Extract OD values
      OD_L13 = safe_num(od[ExcelRow, idx13]),
      OD_L15 = safe_num(od[ExcelRow, idx15]),

      # Create well IDs for reference
      well_L13 = paste0(Col13, ExcelRow),
      well_L15 = paste0(Col15, ExcelRow)
    ) %>%
    ungroup()
}

#' Main iELISA ingestion function
#' @param file_path Path to iELISA Excel file
#' @return List with:
#'   - metadata: plate_id, plate_date, test_type, n_samples_raw
#'   - controls: Control OD values for both antigens
#'   - samples: Sample metadata with OD values mapped
#' @export
ingest_ielisa <- function(file_path) {
  cat(sprintf("📥 INGESTION (iELISA): %s\n", basename(file_path)))

  # Extract metadata
  plate_id <- tools::file_path_sans_ext(basename(file_path))
  plate_date <- parse_plate_date_from_filename(basename(file_path))

  # Read OD grid
  od_grid <- ingest_ielisa_od_grid(file_path)
  cat(sprintf("  ✓ Read OD grid: 96-well plate\n"))

  # Read sample metadata
  sample_metadata <- ingest_ielisa_sample_metadata(file_path, plate_id)
  n_samples <- nrow(sample_metadata)
  cat(sprintf("  ✓ Read sample metadata: %d samples\n", n_samples))

  # Extract controls
  controls <- ingest_ielisa_controls(od_grid)
  cat(sprintf("  ✓ Extracted control wells (LiTat 1.3 & 1.5)\n"))

  # Map sample wells to OD values
  samples_with_od <- map_sample_wells(sample_metadata, od_grid)
  cat(sprintf("  ✓ Mapped sample wells to OD values\n"))

  # Build metadata summary
  metadata <- list(
    plate_id = plate_id,
    plate_date = plate_date,
    test_type = "iELISA",
    n_samples_raw = n_samples,
    n_tests = 1,  # Single plate per file
    file_list = basename(file_path)
  )

  cat(sprintf("  ✓ Ingestion complete: %d samples\n", n_samples))

  return(list(
    metadata = metadata,
    controls = controls,
    samples = samples_with_od
  ))
}
