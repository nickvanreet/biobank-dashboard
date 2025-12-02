# =============================================================================
# ELISA Modular Processing Integration
# Wrapper that connects new 4-step pipeline with existing infrastructure
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Source all module files
source(file.path("R", "modules", "elisa_shared", "utils_elisa.R"), local = TRUE)
source(file.path("R", "modules", "elisa_pe", "ingest_pe.R"), local = TRUE)
source(file.path("R", "modules", "elisa_pe", "qc_pe.R"), local = TRUE)
source(file.path("R", "modules", "elisa_pe", "interpret_pe.R"), local = TRUE)
source(file.path("R", "modules", "elisa_pe", "output_pe.R"), local = TRUE)
source(file.path("R", "modules", "elisa_vsg", "ingest_vsg.R"), local = TRUE)
source(file.path("R", "modules", "elisa_vsg", "qc_vsg.R"), local = TRUE)
source(file.path("R", "modules", "elisa_vsg", "interpret_vsg.R"), local = TRUE)
source(file.path("R", "modules", "elisa_vsg", "output_vsg.R"), local = TRUE)

# =============================================================================
# FILE TYPE DETECTION
# =============================================================================

#' Detect ELISA type from file path and content
#' @param file_path Path to ELISA file
#' @return "PE" or "VSG"
detect_elisa_type <- function(file_path) {
  # First try path-based detection (most reliable)
  if (grepl("elisa_vsg|/vsg/", file_path, ignore.case = TRUE)) {
    return("VSG")
  }

  if (grepl("elisa_pe|/pe/", file_path, ignore.case = TRUE)) {
    return("PE")
  }

  # Fall back to checking for 4-plate markers in file
  tryCatch({
    raw <- readxl::read_excel(file_path, sheet = "450 nm - 600 nm", col_names = FALSE, n_max = 50)
    has_plaque_markers <- any(grepl("Plaque \\d+:", raw[[1]], ignore.case = TRUE))

    if (has_plaque_markers) {
      return("VSG")
    } else {
      return("PE")
    }
  }, error = function(e) {
    # Default to PE if detection fails
    warning("Could not detect ELISA type for ", basename(file_path), ", defaulting to PE")
    return("PE")
  })
}

# =============================================================================
# UNIFIED PROCESSING FUNCTION
# =============================================================================

#' Process ELISA file using modular pipeline (auto-detects PE vs VSG)
#' @param file_path Path to ELISA file
#' @param qc_settings QC settings (optional, uses defaults if NULL)
#' @return Standardized output tibble
#' @export
process_elisa_file_modular <- function(file_path, qc_settings = NULL) {
  if (is.null(qc_settings)) {
    qc_settings <- elisa_default_qc_settings()
  }

  # Detect file type
  elisa_type <- detect_elisa_type(file_path)

  # Route to appropriate pipeline
  if (elisa_type == "VSG") {
    result <- process_elisa_vsg_file(file_path, qc_settings)
  } else {
    result <- process_elisa_pe_file(file_path, qc_settings)
  }

  return(result$results_clean)
}

# =============================================================================
# BATCH PROCESSING (replaces parse_indirect_elisa_folder)
# =============================================================================

#' Process folder of ELISA files using modular pipeline
#' Backward-compatible with parse_indirect_elisa_folder()
#'
#' @param dir Character vector of directories to scan
#' @param pattern File pattern (default: "\\.xlsx$")
#' @param exclude_pattern Pattern to exclude files
#' @param recursive Scan recursively
#' @param cv_max_ag_plus Maximum CV% for Ag+ (default: 20)
#' @param cv_max_ag0 Maximum CV% for Ag0 (default: 20)
#' @return Combined tibble with all processed files
#' @export
parse_elisa_folder_modular <- function(dir,
                                        pattern = "\\.xlsx$",
                                        exclude_pattern = NULL,
                                        recursive = FALSE,
                                        cv_max_ag_plus = 20,
                                        cv_max_ag0 = 20) {
  # Build QC settings from parameters
  qc_settings <- elisa_default_qc_settings()
  qc_settings$cv_max_ag_plus <- cv_max_ag_plus
  qc_settings$cv_max_ag0 <- cv_max_ag0

  # Get file list
  dir <- unlist(dir)

  files <- unlist(lapply(dir, function(path_dir) {
    if (!dir.exists(path_dir)) {
      warning("Directory does not exist: ", path_dir)
      return(character(0))
    }
    list.files(
      path = path_dir,
      pattern = pattern,
      full.names = TRUE,
      recursive = recursive
    )
  }))

  if (!is.null(exclude_pattern)) {
    files <- files[!grepl(exclude_pattern, basename(files))]
  }

  if (length(files) == 0) {
    stop("No Excel files found in: ", paste(dir, collapse = ", "))
  }

  message("Processing ", length(files), " file(s) using modular pipeline.")

  # Process each file
  res_list <- lapply(files, function(f) {
    tryCatch(
      process_elisa_file_modular(f, qc_settings),
      error = function(e) {
        warning("Failed on file: ", f, " — ", conditionMessage(e))
        NULL
      }
    )
  })

  # Combine results
  df_all <- bind_rows(Filter(Negate(is.null), res_list))

  if (!nrow(df_all)) {
    return(tibble(
      sample_id = character(),
      test_type = character(),
      run_id = character(),
      run_valid = logical(),
      sample_valid = logical(),
      status_final = character(),
      status_raw = character(),
      n_runs = integer(),
      metric_1 = double(),
      metric_2 = double(),
      PP_percent = double(),
      DOD = double()
    ))
  }

  # Report summary
  type_counts <- table(df_all$test_type)
  message("✓ Processed: ", paste(names(type_counts), "=", type_counts, collapse = ", "))

  # Sort by date and plate
  df_all <- df_all %>%
    arrange(test_date, plate_id, plate_num, sample_type, sample)

  return(df_all)
}

# =============================================================================
# CONVERSION TO LEGACY FORMAT
# =============================================================================

#' Convert modular output to legacy format for backward compatibility
#' This allows the new modules to work with existing coordinator code
#'
#' @param modular_output Output from process_elisa_file_modular()
#' @return Tibble in legacy format (matches old parse_indirect_elisa output)
#' @export
convert_to_legacy_format <- function(modular_output) {
  # Create base legacy columns
  legacy_data <- modular_output %>%
    mutate(
      # Map back to legacy column names
      plate_number = as.integer(factor(paste(plate_id, plate_num, sep = "_"))),
      sample_positive = (status_final == "Positive"),
      qc_Ag_plus = sample_valid,
      qc_Ag0 = sample_valid,
      qc_overall = sample_valid,
      plate_date = test_date,
      plate_positive_control_valid = run_valid,
      plate_negative_control_valid = run_valid
    )

  # Select core columns (including biobank columns if present)
  core_cols <- c(
    "plate_id", "plate_num", "plate_number", "plate_date",
    "elisa_type",
    "sample_type", "sample", "sample_code",
    "numero_labo", "code_barres_kps",
    "PP_percent", "DOD", "sample_positive",
    "Ag_plus_1", "Ag_plus_2", "Ag0_1", "Ag0_2",
    "mean_Ag_plus", "mean_Ag0",
    "cv_Ag_plus", "cv_Ag0",
    "qc_Ag_plus", "qc_Ag0", "qc_overall",
    "positive_control_od", "negative_control_od",
    "plate_positive_control_valid", "plate_negative_control_valid",
    "plate_valid",
    "PC_DOD", "NC_DOD"
  )

  # Biobank columns to preserve if present
  biobank_cols <- c(
    "Province", "HealthZone", "Structure",
    "Sex", "Age", "AgeGroup",
    "SampleDate", "Cohort",
    "BiobankMatched",
    "biobank_barcode", "biobank_lab_id"  # Also keep lookup columns
  )

  # Keep all columns that exist (core + biobank)
  all_cols <- c(core_cols, biobank_cols)
  existing_cols <- intersect(all_cols, names(legacy_data))

  legacy_data %>% select(all_of(existing_cols))
}
