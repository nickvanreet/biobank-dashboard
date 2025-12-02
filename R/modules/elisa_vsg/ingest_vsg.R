# =============================================================================
# ELISA-VSG Ingestion Module (STEP 1)
# Reads 4-plate format, extracts OD values, maps samples to wells
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(janitor)
})

# Source shared utilities
source(file.path("R", "modules", "elisa_shared", "utils_elisa.R"), local = TRUE)

# =============================================================================
# STEP 1: INGESTION (4-PLATE FORMAT)
# =============================================================================

#' Read ELISA-VSG OD grid (4-plate format)
#' @param path Path to Excel file
#' @param sheet Sheet name containing OD data
#' @return Tibble with columns: plate_num, well_id, row_label, col, ag_state, od
ingest_vsg_od_grid <- function(path, sheet = "450 nm - 600 nm") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)

  # Check for "Plaque" markers (4-plate format indicator)
  plate_markers <- which(grepl("Plaque \\d+:", raw[[1]], ignore.case = TRUE))

  if (length(plate_markers) == 0) {
    stop("No 4-plate format markers found. This may not be a VSG file.")
  }

  cat("  Detected 4-plate format\n")

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
    select(plate_num, well_id, row_label, col, ag_state, od)

  return(od_all)
}

#' Read ELISA-VSG sample layout (from Results sheet)
#' @param path Path to Excel file
#' @param plate_id Plate identifier
#' @return Tibble with sample→well mapping
ingest_vsg_sample_layout <- function(path, plate_id) {
  df_samples <- read_excel(path, sheet = "Results") %>%
    clean_names() %>%
    ensure_columns(c("sample", "numero_labo", "code_barres_kps", "sample_code", "elisa"))

  dw_col <- names(df_samples)[str_detect(names(df_samples), "^deepwell")]
  if (length(dw_col) == 0) {
    stop("No 'Deepwell' column found in 'Results' sheet")
  }
  dw_col <- dw_col[1]

  # Check if ELISA column exists (indicates 4-plate format)
  has_elisa_col <- "elisa" %in% names(df_samples)

  if (!has_elisa_col) {
    stop("ELISA column not found. This may not be a 4-plate VSG file.")
  }

  # 4-plate format: direct mapping
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

  return(samples_layout)
}

#' Read ELISA-VSG control layout (from Controls sheet)
#' @param path Path to Excel file
#' @param plate_id Plate identifier
#' @return Tibble with control→well mapping
ingest_vsg_control_layout <- function(path, plate_id) {
  df_controls <- read_excel(path, sheet = "Controls") %>%
    clean_names() %>%
    ensure_columns(c("sample", "sample_code", "elisa"))

  dw_col <- names(df_controls)[str_detect(names(df_controls), "^deepwell")]
  if (length(dw_col) == 0) {
    stop("No 'Deepwell' column found in 'Controls' sheet")
  }
  dw_col <- dw_col[1]

  has_elisa_col <- "elisa" %in% names(df_controls)

  if (!has_elisa_col) {
    stop("ELISA column not found in Controls sheet.")
  }

  # 4-plate format
  controls_layout <- df_controls %>%
    filter(!is.na(.data[[dw_col]])) %>%
    mutate(
      plate_id = plate_id,
      plate_num = as.integer(elisa),
      sample_type = "control",
      numero_labo = NA_character_,
      code_barres_kps = NA_character_,
      well_id = .data[[dw_col]]
    ) %>%
    select(plate_id, plate_num, sample_type, sample, sample_code,
           numero_labo, code_barres_kps, well_id, everything())

  return(controls_layout)
}

#' Main ELISA-VSG ingestion function
#' @param file_path Path to ELISA-VSG Excel file
#' @return List with:
#'   - metadata: plate_id, plate_date, elisa_type, n_samples_raw, n_tests
#'   - od_data: OD values for all wells
#'   - sample_layout: Sample→well mapping
#'   - control_layout: Control→well mapping
#'   - wells_long: Combined OD + layout data
#' @export
ingest_elisa_vsg <- function(file_path) {
  cat(sprintf("📥 INGESTION (VSG): %s\n", basename(file_path)))

  # Extract metadata
  metadata <- extract_plate_metadata(file_path)
  metadata$elisa_type <- "ELISA_vsg"  # Force VSG type
  plate_id <- metadata$plate_id
  plate_date <- metadata$plate_date

  # Read OD grid (4-plate format)
  od_data <- ingest_vsg_od_grid(file_path)
  cat(sprintf("  ✓ Read OD grid: %d plates, %d wells\n",
              n_distinct(od_data$plate_num), nrow(od_data)))

  # Read layouts
  sample_layout <- ingest_vsg_sample_layout(file_path, plate_id)
  control_layout <- ingest_vsg_control_layout(file_path, plate_id)
  cat(sprintf("  ✓ Mapped %d samples, %d controls across %d plates\n",
              n_distinct(sample_layout$sample),
              n_distinct(control_layout$sample),
              n_distinct(sample_layout$plate_num)))

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
