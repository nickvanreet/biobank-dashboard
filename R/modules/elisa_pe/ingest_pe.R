# =============================================================================
# ELISA-PE Ingestion Module (STEP 1)
# Reads single-plate format, extracts OD values, maps samples to wells
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(janitor)
})

# Note: Shared utilities (utils_elisa.R) should be sourced by the caller

# =============================================================================
# STEP 1: INGESTION
# =============================================================================

#' Read ELISA-PE OD grid (supports both 1-plate and 4-plate formats)
#' @param path Path to Excel file
#' @param sheet Sheet name containing OD data
#' @return Tibble with columns: plate_num, well_id, row, col, ag_state, od
ingest_pe_od_grid <- function(path, sheet = "450 nm - 600 nm") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)

  # Check for "Plaque" markers (4-plate format indicator)
  plate_markers <- which(grepl("Plaque \\d+:", raw[[1]], ignore.case = TRUE))

  # ========== 4-PLATE FORMAT ==========
  if (length(plate_markers) > 0) {
    cat("  Detected 4-plate PE format\n")

    all_plates_od <- list()

    for (i in seq_along(plate_markers)) {
      plate_num <- i
      start_row <- plate_markers[i]

      # Skip 3 rows (marker, empty, header)
      data_start <- start_row + 3

      # Read 8 rows (A-H)
      plate_data <- raw[data_start:(data_start + 7), 1:13]
      names(plate_data) <- c("row_label", as.character(1:12))

      od_long <- plate_data %>%
        filter(row_label %in% LETTERS[1:8]) %>%
        mutate(across(`1`:`12`, as.numeric)) %>%
        pivot_longer(
          cols = `1`:`12`,
          names_to = "col",
          values_to = "od"
        ) %>%
        mutate(
          col = as.integer(col),
          plate_num = plate_num
        ) %>%
        select(plate_num, row_label, col, od)

      all_plates_od[[paste0("plate_", plate_num)]] <- od_long
    }

    # Combine all plates
    od_all <- bind_rows(all_plates_od)

    # Map to deepwell positions
    # Deepwell cols 1-3 → ELISA plate 1, cols 4-6 → plate 2, etc.
    od_all <- od_all %>%
      mutate(
        # Get position within set of 3
        pos_in_set = ((col - 1) %% 3) + 1,

        # Determine Ag+/Ag0 based on column
        ag_state = if_else(col <= 6, "Ag_plus", "Ag0"),

        # Map to deepwell column
        deepwell_col = case_when(
          plate_num == 1 ~ pos_in_set,
          plate_num == 2 ~ pos_in_set + 3,
          plate_num == 3 ~ pos_in_set + 6,
          plate_num == 4 ~ pos_in_set + 9
        ),

        # Create well_id using deepwell coordinates
        well_id = paste0(row_label, deepwell_col)
      ) %>%
      rename(row = row_label) %>%
      select(plate_num, well_id, row, col, ag_state, od)

    return(od_all)
  }

  # ========== 1-PLATE FORMAT (ORIGINAL LOGIC) ==========
  cat("  Detected 1-plate PE format\n")

  names(raw)[1] <- "row_label"

  raw_plate <- raw %>%
    mutate(row_label = as.character(row_label)) %>%
    filter(row_label %in% LETTERS[1:8])

  n_plate_cols <- ncol(raw_plate) - 1
  if (n_plate_cols <= 0) {
    stop("No OD columns detected in ", sheet)
  }

  plate_cols <- as.character(seq_len(n_plate_cols))
  names(raw_plate)[2:ncol(raw_plate)] <- plate_cols

  od_long <- raw_plate %>%
    mutate(across(all_of(plate_cols), ~ suppressWarnings(as.numeric(.)))) %>%
    rename(row = row_label) %>%
    pivot_longer(
      cols = all_of(plate_cols),
      names_to = "col",
      values_to = "od"
    ) %>%
    mutate(
      col = as.integer(col),
      well_id = paste0(row, col),
      ag_state = if_else(col <= 6, "Ag_plus", "Ag0"),
      plate_num = 1  # Single plate = plate 1
    ) %>%
    select(plate_num, well_id, row, col, ag_state, od)

  return(od_long)
}

#' Read ELISA-PE sample layout (supports both 1-plate and 4-plate formats)
#' @param path Path to Excel file
#' @param plate_id Plate identifier
#' @return Tibble with sample→well mapping
ingest_pe_sample_layout <- function(path, plate_id) {
  df_samples <- read_excel(path, sheet = "Results") %>%
    clean_names() %>%
    ensure_columns(c("sample", "numero_labo", "code_barres_kps", "sample_code"))

  dw_col <- names(df_samples)[str_detect(names(df_samples), "^deepwell")]
  if (length(dw_col) == 0) {
    stop("No 'Deepwell' column found in 'Results' sheet")
  }
  dw_col <- dw_col[1]

  # Check if ELISA column exists (indicates 4-plate format)
  has_elisa_col <- "elisa" %in% names(df_samples)

  if (has_elisa_col) {
    # ========== 4-PLATE FORMAT ==========
    # Direct mapping: use well_id from deepwell column and plate_num from elisa column
    samples_layout <- df_samples %>%
      filter(!is.na(.data[[dw_col]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = as.integer(elisa),
        sample_type = "sample",
        sample_code = NA_character_,
        well_id = .data[[dw_col]]  # Direct mapping in 4-plate
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, well_id, everything())
  } else {
    # ========== 1-PLATE FORMAT (ORIGINAL LOGIC) ==========
    # Pattern parsing: expand deepwell patterns like "A1/A2" or "A1 ou A2"
    samples_layout <- df_samples %>%
      filter(!is.na(.data[[dw_col]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = 1,
        sample_type = "sample",
        sample_code = NA_character_,  # Will be filled for controls only
        wells_raw = .data[[dw_col]],
        wells = map(wells_raw, parse_deepwell_pattern)
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, wells, everything()) %>%
      unnest_longer(wells, values_to = "well_id")
  }

  return(samples_layout)
}

#' Read ELISA-PE control layout (supports both 1-plate and 4-plate formats)
#' @param path Path to Excel file
#' @param plate_id Plate identifier
#' @return Tibble with control→well mapping
ingest_pe_control_layout <- function(path, plate_id) {
  df_controls <- read_excel(path, sheet = "Controls") %>%
    clean_names() %>%
    ensure_columns(c("sample", "sample_code"))

  dw_col <- names(df_controls)[str_detect(names(df_controls), "^deepwell")]
  if (length(dw_col) == 0) {
    stop("No 'Deepwell' column found in 'Controls' sheet")
  }
  dw_col <- dw_col[1]

  # Check if ELISA column exists (indicates 4-plate format)
  has_elisa_col <- "elisa" %in% names(df_controls)

  if (has_elisa_col) {
    # ========== 4-PLATE FORMAT ==========
    # Direct mapping: use well_id from deepwell column and plate_num from elisa column
    controls_layout <- df_controls %>%
      filter(!is.na(.data[[dw_col]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = as.integer(elisa),
        sample_type = "control",
        numero_labo = NA_character_,
        code_barres_kps = NA_character_,
        well_id = .data[[dw_col]]  # Direct mapping in 4-plate
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, well_id, everything())
  } else {
    # ========== 1-PLATE FORMAT (ORIGINAL LOGIC) ==========
    # Pattern parsing: expand deepwell patterns like "A1/A2" or "A1 ou A2"
    controls_layout <- df_controls %>%
      filter(!is.na(.data[[dw_col]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = 1,
        sample_type = "control",
        numero_labo = NA_character_,
        code_barres_kps = NA_character_,
        wells_raw = .data[[dw_col]],
        wells = map(wells_raw, parse_deepwell_pattern)
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, wells, everything()) %>%
      unnest_longer(wells, values_to = "well_id")
  }

  return(controls_layout)
}

#' Main ELISA-PE ingestion function
#' @param file_path Path to ELISA-PE Excel file
#' @return List with:
#'   - metadata: plate_id, plate_date, elisa_type, n_samples_raw, n_tests
#'   - od_data: OD values for all wells
#'   - sample_layout: Sample→well mapping
#'   - control_layout: Control→well mapping
#'   - wells_long: Combined OD + layout data
#' @export
ingest_elisa_pe <- function(file_path) {
  cat(sprintf("📥 INGESTION (PE): %s\n", basename(file_path)))

  # Extract metadata
  metadata <- extract_plate_metadata(file_path)
  plate_id <- metadata$plate_id
  plate_date <- metadata$plate_date

  # Read OD grid
  od_data <- ingest_pe_od_grid(file_path)
  cat(sprintf("  ✓ Read OD grid: %d wells\n", nrow(od_data)))

  # Read layouts
  sample_layout <- ingest_pe_sample_layout(file_path, plate_id)
  control_layout <- ingest_pe_control_layout(file_path, plate_id)
  cat(sprintf("  ✓ Mapped %d samples, %d controls\n",
              n_distinct(sample_layout$sample),
              n_distinct(control_layout$sample)))

  # Harmonize column types
  sample_layout <- sample_layout %>%
    mutate(
      numero_labo = as.character(numero_labo),
      code_barres_kps = as.character(code_barres_kps),
      sample_code = as.character(sample_code)
    )

  control_layout <- control_layout %>%
    mutate(
      numero_labo = as.character(numero_labo),
      code_barres_kps = as.character(code_barres_kps),
      sample_code = as.character(sample_code)
    )

  # Join OD data with layouts
  sample_wells <- sample_layout %>%
    left_join(od_data, by = c("plate_num", "well_id"), relationship = "many-to-many")

  control_wells <- control_layout %>%
    left_join(od_data, by = c("plate_num", "well_id"), relationship = "many-to-many")

  wells_long <- bind_rows(sample_wells, control_wells) %>%
    mutate(
      plate_date = plate_date,
      elisa_type = metadata$elisa_type,
      source_path = file_path
    )

  # Ensure all expected columns exist
  wells_long <- ensure_columns(
    wells_long,
    c("plate_id", "plate_num", "sample_type", "sample", "sample_code",
      "numero_labo", "code_barres_kps", "ag_state", "od")
  )

  # Calculate ingestion metrics
  n_samples_raw <- n_distinct(sample_layout$numero_labo, sample_layout$code_barres_kps)
  n_tests <- n_distinct(wells_long$plate_num)

  metadata$n_samples_raw <- n_samples_raw
  metadata$n_tests <- n_tests
  metadata$n_files_read <- 1
  metadata$file_list <- basename(file_path)

  cat(sprintf("  ✓ Ingestion complete: %d samples, %d plate(s)\n", n_samples_raw, n_tests))

  return(list(
    metadata = metadata,
    od_data = od_data,
    sample_layout = sample_layout,
    control_layout = control_layout,
    wells_long = wells_long
  ))
}
