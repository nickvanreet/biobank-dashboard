# R/core/extraction_data_utils.R
# Utility functions for loading and shaping extraction quality data
# ============================================================================

#' List extraction data files
#' @param directory Path to extraction files directory
#' @return Character vector of file paths
#' @export
list_extraction_files <- function(directory) {
  if (is.null(directory) || !dir.exists(directory)) {
    warning(sprintf("Extraction directory not found: %s", directory))
    return(character(0))
  }

  files <- list.files(
    directory,
    pattern = "\\.(xlsx|xls|csv|tsv)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Filter out temporary Excel files
  files <- files[!grepl("~\\$", basename(files))]
  files
}


#' Determine whether an extraction row is empty
#' @param df Tibble of extraction data
.is_empty_extraction_row <- function(df) {
  if (!nrow(df)) return(logical())

  # Helper to convert blank strings to NA
  blank_to_na <- function(x) {
    if (is.null(x)) return(rep(NA, nrow(df)))
    out <- trimws(as.character(x))
    out[out == ""] <- NA_character_
    out
  }

  # Helper to safely get column or return NA vector
  safe_col <- function(df, col_name) {
    if (col_name %in% names(df)) {
      return(df[[col_name]])
    } else {
      return(rep(NA, nrow(df)))
    }
  }

  # Define core columns that must have at least one non-NA value for a row to be valid
  # Focus on identifier columns and key data columns
  core_cols <- dplyr::tibble(
    barcode = blank_to_na(safe_col(df, "barcode")),
    numero = blank_to_na(safe_col(df, "numero")),
    sample_id = blank_to_na(safe_col(df, "sample_id")),
    record_number = blank_to_na(safe_col(df, "record_number")),
    extraction_date = safe_col(df, "extraction_date"),
    drs_volume_ml = safe_col(df, "drs_volume_ml")
  )

  # A row is empty if ALL core columns are NA/blank
  # We explicitly check core columns only, ignoring metadata like source_file
  core_empty <- apply(core_cols, 1, function(row) all(is.na(row)))

  # Additional check: if ALL columns (excluding metadata) are NA/blank
  # Exclude source_file and other metadata columns from this check
  metadata_cols <- c("source_file", "has_cn", "is_duplicate", "dup_group", "most_recent",
                     "valid_sample_id", "duplicate_sample_id", "barcode_suspicious",
                     "validation_status", "flag_issue", "ready_for_freezer")
  data_cols <- setdiff(names(df), metadata_cols)

  if (length(data_cols) > 0) {
    df_data_only <- df[, data_cols, drop = FALSE]
    all_data_na <- apply(as.data.frame(df_data_only), 1, function(row) {
      all(is.na(row) | (is.character(row) & trimws(row) == ""))
    })
  } else {
    all_data_na <- rep(FALSE, nrow(df))
  }

  # Return TRUE if core fields are empty OR all data columns are empty
  core_empty | all_data_na
}

#' Parse extraction datetime with time support
#' @param x Vector to parse
.parse_extraction_datetime <- function(x) {
  if (inherits(x, c("POSIXct", "POSIXt"))) return(lubridate::as_datetime(x))
  suppressWarnings({
    dt <- suppressWarnings(lubridate::ymd_hms(x))
    bad <- is.na(dt)
    if (any(bad)) {
      dt[bad] <- suppressWarnings(lubridate::dmy_hms(as.character(x[bad])))
    }
    if (any(is.na(dt))) {
      dt[is.na(dt)] <- suppressWarnings(lubridate::as_datetime(as.character(x[is.na(dt)])))
    }
    dt
  })
}

# ---- Internal helpers ------------------------------------------------------

.parse_extraction_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  suppressWarnings({
    d <- as.Date(x)
    bad <- is.na(d)
    if (any(bad)) {
      xc <- as.character(x)
      d[bad] <- suppressWarnings(lubridate::ymd(xc[bad]))
      bad2 <- is.na(d)
      if (any(bad2)) {
        d[bad2] <- suppressWarnings(lubridate::dmy(xc[bad2]))
      }
    }
    d
  })
}

.infer_date_from_filename <- function(filename) {
  digits <- stringr::str_extract(filename, "\\d{6,8}")
  if (is.na(digits)) {
    return(NA)
  }

  if (nchar(digits) == 8) {
    return(suppressWarnings(lubridate::ymd(digits)))
  }

  if (nchar(digits) == 6) {
    yy <- as.integer(substr(digits, 1, 2))
    year_prefix <- ifelse(is.na(yy), NA_integer_, ifelse(yy >= 70, 1900, 2000))
    if (is.na(year_prefix)) {
      return(NA)
    }
    digits <- sprintf("%04d%s", year_prefix + yy, substr(digits, 3, 6))
    return(suppressWarnings(lubridate::ymd(digits)))
  }

  NA
}

.parse_numeric <- function(x) {
  if (is.null(x)) {
    return(numeric(0))
  }

  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- trimws(as.character(x))
  if (!length(x_chr)) {
    return(numeric(0))
  }

  suppressWarnings({
    parsed <- readr::parse_number(x_chr, locale = readr::locale(decimal_mark = ","))
    bad <- is.na(parsed) & !is.na(x_chr) & nzchar(x_chr)
    if (any(bad)) {
      parsed[bad] <- readr::parse_number(x_chr[bad], locale = readr::locale(decimal_mark = "."))
    }
    parsed
  })
}

#' Normalise extraction categorical values
#' @param x Character vector
#' @param map Named vector mapping lower-case tokens to desired values
#' @param other_value Value to use when no match
.normalize_categories <- function(x, map, other_value = NA_character_) {
  if (is.null(x)) return(rep(other_value, length.out = 0))
  key <- tolower(trimws(as.character(x)))
  key[key %in% c("", "na", "n/a", "none")] <- NA_character_
  out <- map[key]
  out[is.na(out)] <- other_value
  out
}

#' Read a single extraction data file and normalise columns
#' @param filepath Path to the file
#' @return Tibble with standardised columns
#' @export
load_extraction_file <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("Extraction file not found: %s", filepath))
  }

  ext <- tools::file_ext(filepath)
  df <- switch(tolower(ext),
    xlsx = suppressMessages(readxl::read_excel(filepath, guess_max = 5000)),
    xls  = suppressMessages(readxl::read_excel(filepath, guess_max = 5000)),
    csv  = readr::read_csv(filepath, show_col_types = FALSE),
    tsv  = readr::read_tsv(filepath, show_col_types = FALSE),
    stop(sprintf("Unsupported extraction file extension: %s", ext))
  )

  if (!nrow(df)) {
    return(tibble::tibble())
  }

  df <- janitor::clean_names(df)
  original_names <- names(df)

  col_map <- list(
    sample_id = c(
      "sample_id", "extraction_id", "numero", "lab_id", "code", "barcode",
      "code_barres", "code_barres_kps", "code_barre"
    ),
    barcode = c("barcode", "code_barres", "code_barres_kps", "code_barre"),
    numero = c("numero", "num", "numéro"),
    record_number = c("numero", "record", "sample_number"),
    extraction_date = c("extraction_date", "date_extraction", "date", "extractiondate"),
    health_structure = c(
      "health_structure", "structure_sanitaire", "structure", "facility",
      "health_facility", "centre_de_sante", "cs"
    ),
    drs_state = c(
      "drs_state", "etat_drs", "state", "etat_tube", "drs_condition",
      "etat_echantillon", "etat_echantillon_sang_drs", "etat_echantillon_sang_drs_liquide_visqueux_coagule"
    ),
    drs_volume_ml = c(
      "drs_volume_ml", "drs_volume", "volume_drs", "volume_recu", "volume_received",
      "volume_total_echantillon_sang_drs_ml", "volume_total_echantillon_sang_drs"
    ),
    drs_volume_ul = c("drs_volume_ul", "volume_ul"),
    filter_type = c("filter", "filtre", "filter_type", "filtration"),
    extract_quality = c(
      "extract_appearance", "extract_quality", "aspect_extrait", "visual_quality", "appearance",
      "evaluation", "evaluation_de_l_echantillon_extrait_clair_fonce_echec"
    ),
    technician = c("technician", "operateur", "operator", "technicien", "executeur"),
    project = c("project", "programme", "study", "projet"),
    batch = c("batch", "lot", "extraction_batch"),
    rsc_run = c("rsc_run"),
    rsc_position = c("rsc_position"),
    freezer_position = c("freezer_position", "position_congelateur", "position_freezer"),
    rack = c("rack"),
    rack_row = c("rangee", "row"),
    rack_column = c("position", "col"),
    remarks = c("remarks", "commentaires", "comments", "notes")
  )

  for (target in names(col_map)) {
    hits <- intersect(col_map[[target]], original_names)
    if (length(hits) > 0) {
      df[[target]] <- df[[hits[1]]]
    } else if (!target %in% names(df)) {
      df[[target]] <- rep(NA, nrow(df))
    }
  }

  extraction_datetime_raw <- df$extraction_date

  file_date <- .infer_date_from_filename(basename(filepath))

  df <- df %>%
    dplyr::transmute(
      source_file = basename(filepath),
      sample_id = as.character(sample_id),
      barcode = as.character(barcode),
      numero = as.character(numero),
      record_number = as.character(record_number),
      extraction_datetime = .parse_extraction_datetime(extraction_datetime_raw),
      extraction_date = dplyr::coalesce(
        .parse_extraction_date(extraction_date),
        as.Date(extraction_datetime)
      ),
      health_structure = stringr::str_to_title(trimws(as.character(health_structure))),
      drs_state = .normalize_categories(
        drs_state,
        c(
          "liquid" = "Liquid",
          "liquide" = "Liquid",
          "liq" = "Liquid",
          "l" = "Liquid",
          "viscous" = "Viscous",
          "visqueux" = "Viscous",
          "visc" = "Viscous",
          "v" = "Viscous",
          "coagulated" = "Coagulated",
          "coagule" = "Coagulated",
          "coagulatede" = "Coagulated",
          "c" = "Coagulated"
        ),
        other_value = "Unknown"
      ),
      drs_volume_ml = dplyr::coalesce(.parse_numeric(drs_volume_ml), .parse_numeric(drs_volume_ul) / 1000),
      extract_quality = .normalize_categories(
        extract_quality,
        c(
          "clear" = "Clear",
          "clair" = "Clear",
          "c" = "Clear",
          "fonce" = "Foncé",
          "foncé" = "Foncé",
          "dark" = "Foncé",
          "f" = "Foncé",
          "echec" = "Échec",
          "échec" = "Échec",
          "e" = "Échec"
        ),
        other_value = "Unknown"
      ),
      technician = stringr::str_to_title(trimws(as.character(technician))),
      # RSC fields for linking to biobank
      rsc_run = as.character(rsc_run),
      rsc_position = as.character(rsc_position),
      # Freezer storage location fields
      freezer_position = as.character(freezer_position),
      rack = as.character(rack),
      rack_row = as.character(rack_row),
      rack_column = as.character(rack_column),
      remarks = as.character(remarks)
    ) %>%
    dplyr::mutate(
      sample_id = dplyr::coalesce(sample_id, barcode, numero, record_number),
      extraction_date = dplyr::coalesce(extraction_date, file_date),
      extraction_datetime = dplyr::coalesce(extraction_datetime, as.POSIXct(extraction_date)),
      health_structure = dplyr::if_else(is.na(health_structure) | health_structure == "", "Unspecified", health_structure),
      drs_state = dplyr::if_else(is.na(drs_state) | drs_state == "", "Unknown", drs_state),
      extract_quality = dplyr::if_else(is.na(extract_quality) | extract_quality == "", "Unknown", extract_quality),
      drs_state_code = dplyr::case_when(
        drs_state == "Liquid" ~ "L",
        drs_state == "Viscous" ~ "V",
        drs_state == "Coagulated" ~ "C",
        TRUE ~ NA_character_
      ),
      extract_quality_code = dplyr::case_when(
        extract_quality == "Clear" ~ "C",
        extract_quality == "Foncé" ~ "F",
        extract_quality == "Échec" ~ "E",
        TRUE ~ NA_character_
      ),
      flag_issue = dplyr::case_when(
        is.na(drs_volume_ml) ~ TRUE,
        drs_volume_ml < 1 ~ TRUE,
        drs_state %in% c("Viscous", "Coagulated") ~ TRUE,
        extract_quality == "Foncé" ~ TRUE,
        extract_quality == "Échec" ~ TRUE,
        TRUE ~ FALSE
      ),
      ready_for_freezer = !flag_issue
    )

  df
}

#' Validate sample IDs and barcodes
#' @param df Extraction dataset
#' @return Dataset with validation flags added
#' @export
validate_sample_ids <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  df %>%
    dplyr::mutate(
      # Check for valid sample ID format (not empty, not too short)
      valid_sample_id = !is.na(sample_id) & nchar(sample_id) >= 3,

      # Check for duplicate sample IDs within same extraction date
      duplicate_sample_id = duplicated(paste(sample_id, extraction_date)) |
                           duplicated(paste(sample_id, extraction_date), fromLast = TRUE),

      # Check for suspicious barcode patterns
      barcode_suspicious = dplyr::case_when(
        is.na(sample_id) ~ TRUE,
        grepl("^[0]+$", sample_id) ~ TRUE,  # All zeros
        grepl("test|demo|example", sample_id, ignore.case = TRUE) ~ TRUE,
        TRUE ~ FALSE
      ),

      # Overall validation status
      validation_status = dplyr::case_when(
        !valid_sample_id ~ "Invalid ID",
        barcode_suspicious ~ "Suspicious Barcode",
        duplicate_sample_id ~ "Duplicate",
        TRUE ~ "Valid"
      )
    )
}

#' Remove duplicate extraction records
#' @param df Extraction dataset
#' @param keep_first If TRUE, keep first occurrence; if FALSE, keep last
#' @return Dataset with duplicates removed
#' @export
remove_duplicates <- function(df, keep_first = FALSE) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  original_count <- nrow(df)

  # Remove exact duplicates (all columns identical)
  df_dedup <- df %>%
    dplyr::distinct()

  exact_dups_removed <- original_count - nrow(df_dedup)

  # Remove duplicates based on sample_id and extraction_date
  # Keep the record with more complete data (fewer NAs)
  df_dedup <- df_dedup %>%
    dplyr::group_by(sample_id, extraction_date) %>%
    dplyr::mutate(
      na_count = rowSums(is.na(dplyr::pick(dplyr::everything()))),
      row_num = dplyr::row_number()
    ) %>%
    dplyr::filter(
      if (keep_first) {
        row_num == 1
      } else {
        na_count == min(na_count) & row_num == 1
      }
    ) %>%
    dplyr::select(-na_count, -row_num) %>%
    dplyr::ungroup()

  logical_dups_removed <- nrow(df) - exact_dups_removed - nrow(df_dedup)

  if (exact_dups_removed > 0 || logical_dups_removed > 0) {
    message(sprintf(
      "Removed %d exact duplicates and %d logical duplicates (%d records remain)",
      exact_dups_removed, logical_dups_removed, nrow(df_dedup)
    ))
  }

  df_dedup
}

#' Load and combine extraction spreadsheets from a directory
#' @param directory Path to extraction data directory
#' @param validate Should validation flags be added?
#' @return Tibble with combined extraction records
#' @export
load_all_extractions <- function(directory = config$paths$extractions_dir,
                                 validate = TRUE) {
  files <- list_extraction_files(directory)
  files <- files[grepl("\\.(xlsx|xls)$", files, ignore.case = TRUE)]
  if (!length(files)) {
    message("No extraction files found; returning empty dataset")
    return(tibble::tibble(
      source_file = character(),
      barcode = character(),
      numero = character(),
      sample_id = character(),
      record_number = character(),
      extraction_datetime = as.POSIXct(character()),
      extraction_date = as.Date(character()),
      health_structure = character(),
      drs_state = character(),
      drs_volume_ml = numeric(),
      extract_quality = character(),
      technician = character(),
      rsc_run = character(),
      rsc_position = character(),
      freezer_position = character(),
      rack = character(),
      rack_row = character(),
      rack_column = character(),
      remarks = character(),
      has_cn = logical(),
      is_duplicate = logical(),
      dup_group = integer(),
      most_recent = logical(),
      flag_issue = logical(),
      ready_for_freezer = logical(),
      valid_sample_id = logical(),
      duplicate_sample_id = logical(),
      barcode_suspicious = logical(),
      validation_status = character()
    ))
  }

  message(sprintf("Loading %d extraction files...", length(files)))

  safe_loader <- purrr::safely(
    load_extraction_file,
    otherwise = tibble::tibble(),
    quiet = TRUE
  )

  named_files <- stats::setNames(files, basename(files))
  loaded_files <- purrr::map(named_files, safe_loader)

  purrr::walk2(files, loaded_files, function(path, result) {
    if (!is.null(result$error)) {
      message(sprintf(
        "Failed to load extraction file %s: %s",
        basename(path),
        result$error$message
      ))
    }
  })

  df <- purrr::map_dfr(loaded_files, function(res) {
    out <- res$result
    out$source_file <- NULL
    out
  }, .id = "source_file") %>%
    dplyr::mutate(source_file = basename(.data$source_file))

  if (!nrow(df)) {
    return(df)
  }

  # Track row counts for logging
  rows_before_cleaning <- nrow(df)

  df <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.character), ~stringr::str_squish(.x))
    )

  # First pass: Filter empty rows BEFORE duplicate detection
  df <- df %>%
    dplyr::filter(!.is_empty_extraction_row(.))

  rows_after_empty_filter <- nrow(df)
  empty_rows_removed <- rows_before_cleaning - rows_after_empty_filter

  if (empty_rows_removed > 0) {
    message(sprintf(
      "Removed %d empty rows (%d records remain)",
      empty_rows_removed,
      rows_after_empty_filter
    ))
  }

  # Duplicate detection: identify rows with the same identifier AND freezer position
  # Normalize identifiers first (trim, lowercase for comparison)
  df <- df %>%
    dplyr::mutate(
      # Normalize each identifier field
      barcode_norm = stringr::str_trim(tolower(as.character(barcode))),
      barcode_norm = dplyr::na_if(barcode_norm, ""),
      numero_norm = stringr::str_trim(tolower(as.character(numero))),
      numero_norm = dplyr::na_if(numero_norm, ""),
      sample_id_norm = stringr::str_trim(tolower(as.character(sample_id))),
      sample_id_norm = dplyr::na_if(sample_id_norm, ""),
      record_number_norm = stringr::str_trim(tolower(as.character(record_number))),
      record_number_norm = dplyr::na_if(record_number_norm, ""),

      # Normalize freezer position fields
      freezer_position_norm = stringr::str_trim(tolower(as.character(freezer_position))),
      freezer_position_norm = dplyr::na_if(freezer_position_norm, ""),
      rack_norm = stringr::str_trim(tolower(as.character(rack))),
      rack_norm = dplyr::na_if(rack_norm, ""),
      rack_row_norm = stringr::str_trim(tolower(as.character(rack_row))),
      rack_row_norm = dplyr::na_if(rack_row_norm, ""),
      rack_column_norm = stringr::str_trim(tolower(as.character(rack_column))),
      rack_column_norm = dplyr::na_if(rack_column_norm, ""),

      # Create composite sample identifier
      sample_identifier = dplyr::coalesce(
        barcode_norm,
        numero_norm,
        sample_id_norm,
        record_number_norm
      ),

      # Create composite position identifier from available fields
      # This allows different freezer positions to be treated as separate samples (replicates)
      position_identifier = paste(
        dplyr::coalesce(freezer_position_norm, ""),
        dplyr::coalesce(rack_norm, ""),
        dplyr::coalesce(rack_row_norm, ""),
        dplyr::coalesce(rack_column_norm, ""),
        sep = "_"
      ),
      position_identifier = dplyr::na_if(position_identifier, "___"),
      position_identifier = dplyr::na_if(position_identifier, ""),

      # Final identifier for duplicate detection: sample + position
      # Same sample at different positions = replicates (kept)
      # Same sample at same position = duplicates (removed)
      identifier_for_dup = paste(
        dplyr::coalesce(sample_identifier, "NA"),
        dplyr::coalesce(position_identifier, "NA"),
        sep = "|"
      )
    ) %>%
    dplyr::select(-barcode_norm, -numero_norm, -sample_id_norm, -record_number_norm,
                  -freezer_position_norm, -rack_norm, -rack_row_norm, -rack_column_norm,
                  -sample_identifier, -position_identifier)

  # Group by identifier and detect duplicates
  # Important: only group rows that have a valid identifier
  df <- df %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::group_by(identifier_for_dup) %>%
    dplyr::mutate(
      # Only assign dup_group if identifier is not NA and there are multiple rows
      group_size = dplyr::n(),
      dup_group = dplyr::if_else(
        !is.na(identifier_for_dup) & group_size > 1,
        dplyr::cur_group_id(),
        NA_integer_
      ),
      # Mark as duplicate if there are multiple rows with same valid identifier
      is_duplicate = !is.na(identifier_for_dup) & group_size > 1,

      # For duplicates, identify the most recent one based on extraction_date
      latest_date = dplyr::if_else(
        is_duplicate,
        suppressWarnings(max(extraction_date, na.rm = TRUE)),
        as.Date(NA)
      ),
      latest_date = dplyr::if_else(is.infinite(latest_date), as.Date(NA), latest_date),
      most_recent = dplyr::if_else(
        is_duplicate & !is.na(extraction_date) & !is.na(latest_date),
        extraction_date == latest_date,
        FALSE
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-identifier_for_dup, -latest_date, -group_size, -row_id)

  # REMOVE duplicates: keep only the most recent record for each duplicate group
  rows_before_dedup <- nrow(df)
  df <- df %>%
    dplyr::filter(!is_duplicate | most_recent)

  rows_after_dedup <- nrow(df)
  duplicates_removed <- rows_before_dedup - rows_after_dedup

  if (duplicates_removed > 0) {
    message(sprintf(
      "Removed %d duplicate rows (same sample + position, kept most recent; %d records remain)",
      duplicates_removed,
      rows_after_dedup
    ))
  }

  text_fields <- df %>% dplyr::select(where(is.character))
  if (!ncol(text_fields)) {
    df$has_cn <- rep(FALSE, nrow(df))
  } else {
    df$has_cn <- apply(
      text_fields,
      1,
      function(row) {
        matches <- stringr::str_detect(row, stringr::regex("\bcn\b", ignore_case = TRUE))
        any(matches, na.rm = TRUE)
      }
    )
  }

  if (validate && nrow(df) > 0) {
    df <- validate_sample_ids(df)
  } else if (nrow(df) > 0) {
    df <- df %>%
      dplyr::mutate(
        valid_sample_id = TRUE,
        duplicate_sample_id = FALSE,
        barcode_suspicious = FALSE,
        validation_status = "Not Validated"
      )
  }

  # Second pass: Filter empty rows AFTER metadata additions
  # This catches any rows that might have only metadata but no actual data
  rows_before_final_filter <- nrow(df)
  df <- df %>%
    dplyr::filter(!.is_empty_extraction_row(.))

  rows_after_final_filter <- nrow(df)
  final_empty_rows_removed <- rows_before_final_filter - rows_after_final_filter

  if (final_empty_rows_removed > 0) {
    message(sprintf(
      "Removed %d additional empty rows after processing (%d records remain)",
      final_empty_rows_removed,
      rows_after_final_filter
    ))
  }

  # Final summary statistics
  n_duplicates <- sum(df$is_duplicate, na.rm = TRUE)
  n_unique_dup_groups <- length(unique(df$dup_group[!is.na(df$dup_group)]))
  n_cn <- sum(df$has_cn, na.rm = TRUE)

  message(sprintf(
    "Loaded %d extraction records (%d remaining duplicates, %d with CN)",
    nrow(df),
    n_duplicates,
    n_cn
  ))

  df
}

#' Load extraction dataset (backwards compatible wrapper)
#' @inheritParams load_all_extractions
#' @export
load_extraction_dataset <- function(directory = config$paths$extractions_dir,
                                   remove_dups = FALSE,
                                   validate = TRUE) {
  load_all_extractions(directory = directory, validate = validate)
}

#' Summarise extraction metrics for KPI displays
#' @param df Extraction dataset (already filtered)
#' @return Named list with KPI values
#' @export
summarise_extraction_metrics <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(list(
      total = 0,
      files_with_barcodes = 0,
      ready = 0,
      median_volume = NA_real_,
      pct_liquid = NA_real_,
      pct_clear = NA_real_,
      flagged = 0,
      valid_ids = 0,
      duplicates = 0,
      suspicious_barcodes = 0,
      validation_rate = NA_real_,
      mean_volume = NA_real_,
      volume_min = NA_real_,
      volume_max = NA_real_,
      rsc_run_count = 0,
      cn_total = 0,
      cn_pct = NA_real_,
      linked_total = 0
    ))
  }

  safe_pct <- function(x) {
    if (is.null(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    mean(x)
  }

  safe_mean <- function(x) {
    if (is.null(x)) return(NA_real_)
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    mean(x)
  }

  has_validation <- all(c("valid_sample_id", "duplicate_sample_id", "barcode_suspicious") %in% names(df))

  has_column <- function(column) {
    column %in% names(df)
  }

  has_source <- "source_file" %in% names(df)
  barcode_candidates <- NULL
  if (has_column("barcode_normalized")) {
    barcode_candidates <- df$barcode_normalized
  } else if (has_column("sample_id")) {
    barcode_candidates <- df$sample_id
  }
  if (!is.null(barcode_candidates)) {
    barcode_candidates <- stringr::str_trim(as.character(barcode_candidates))
    barcode_candidates[barcode_candidates %in% c("", "NA", "na")] <- NA_character_
  }

  files_with_barcodes <- if (has_source && !is.null(barcode_candidates)) {
    unique_files <- unique(df$source_file[!is.na(barcode_candidates) & barcode_candidates != ""])
    length(unique_files)
  } else if (has_source) {
    0L
  } else {
    NA_integer_
  }

  volume_values <- if (has_column("drs_volume_ml")) {
    df$drs_volume_ml[!is.na(df$drs_volume_ml)]
  } else {
    numeric(0)
  }

  volume_min <- if (length(volume_values)) min(volume_values) else NA_real_
  volume_max <- if (length(volume_values)) max(volume_values) else NA_real_

  rsc_runs <- if (has_column("rsc_run")) {
    runs <- df$rsc_run
    runs <- runs[!is.na(runs) & runs != ""]
    length(unique(runs))
  } else {
    NA_integer_
  }

  cn_total <- if (has_column("has_cn")) sum(df$has_cn, na.rm = TRUE) else 0

  list(
    total = nrow(df),
    files_with_barcodes = files_with_barcodes,
    ready = if (has_column("ready_for_freezer")) {
      sum(df$ready_for_freezer, na.rm = TRUE)
    } else {
      NA_integer_
    },
    median_volume = if (has_column("drs_volume_ml")) {
      stats::median(df$drs_volume_ml, na.rm = TRUE)
    } else {
      NA_real_
    },
    mean_volume = if (has_column("drs_volume_ml")) {
      safe_mean(df$drs_volume_ml)
    } else {
      NA_real_
    },
    pct_liquid = if (has_column("drs_state")) {
      safe_pct(df$drs_state == "Liquid")
    } else {
      NA_real_
    },
    pct_clear = if (has_column("extract_quality")) {
      safe_pct(df$extract_quality == "Clear")
    } else {
      NA_real_
    },
    flagged = if (has_column("flag_issue")) sum(df$flag_issue, na.rm = TRUE) else NA_integer_,
    valid_ids = if (has_validation) sum(df$valid_sample_id, na.rm = TRUE) else NA_integer_,
    duplicates = if (has_validation) sum(df$duplicate_sample_id, na.rm = TRUE) else NA_integer_,
    suspicious_barcodes = if (has_validation) sum(df$barcode_suspicious, na.rm = TRUE) else NA_integer_,
    validation_rate = if (has_validation) mean(df$valid_sample_id, na.rm = TRUE) else NA_real_,
    volume_min = volume_min,
    volume_max = volume_max,
    rsc_run_count = rsc_runs,
    cn_total = cn_total,
    cn_pct = if (nrow(df) > 0) cn_total / nrow(df) else NA_real_,
    linked_total = if (has_column("biobank_matched")) {
      sum(df$biobank_matched, na.rm = TRUE)
    } else {
      NA_integer_
    }
  )
}

#' Summarise extraction volumes by health structure
#' @param df Extraction dataset (already filtered)
#' @return Tibble with health structure summary
#' @export
summarise_by_health_structure <- function(df) {
  summarise_health_structure_volume_overview(df) %>%
    dplyr::select(
      health_structure,
      n_extractions,
      median_volume,
      mean_volume,
      total_volume,
      pct_ready,
      pct_flagged,
      pct_liquid
    )
}

# -----------------------------------------------------------------------------
# Internal helpers -----------------------------------------------------------
# -----------------------------------------------------------------------------

ensure_health_structure_column <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  if ("health_structure" %in% names(df)) {
    return(df)
  }

  fallback_cols <- c(
    "structure_sanitaire",
    "health_facility",
    "structure",
    "facility"
  )

  available <- fallback_cols[fallback_cols %in% names(df)]

  if (length(available) == 0) {
    return(df)
  }

  df$health_structure <- df[[available[[1]]]]
  df
}

#' Summarise extraction volumes by health structure (overview)
#' @param df Extraction dataset (already filtered)
#' @return Tibble with health structure summary including volume metrics
#' @export
summarise_health_structure_volume_overview <- function(df) {
  df <- ensure_health_structure_column(df)

  if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
    return(tibble::tibble(
      health_structure = character(),
      n_extractions = integer(),
      total_volume = numeric(),
      median_volume = numeric(),
      mean_volume = numeric(),
      pct_ready = numeric(),
      pct_flagged = numeric(),
      pct_liquid = numeric()
    ))
  }

  df <- df %>%
    {
      if (!"drs_volume_ml" %in% names(.)) {
        .$drs_volume_ml <- rep(NA_real_, nrow(.))
      }
      if (!"ready_for_freezer" %in% names(.)) {
        .$ready_for_freezer <- rep(NA, nrow(.))
      }
      if (!"flag_issue" %in% names(.)) {
        .$flag_issue <- rep(NA, nrow(.))
      }
      if (!"drs_state" %in% names(.)) {
        .$drs_state <- rep(NA_character_, nrow(.))
      }
      .
    }

  df %>%
    dplyr::filter(
      !is.na(health_structure),
      health_structure != "",
      health_structure != "Unspecified"
    ) %>%
    dplyr::group_by(health_structure) %>%
    dplyr::summarise(
      n_extractions = dplyr::n(),
      total_volume = if (all(is.na(drs_volume_ml))) NA_real_ else sum(drs_volume_ml, na.rm = TRUE),
      median_volume = stats::median(drs_volume_ml, na.rm = TRUE),
      mean_volume = if (all(is.na(drs_volume_ml))) NA_real_ else mean(drs_volume_ml, na.rm = TRUE),
      pct_ready = if (all(is.na(ready_for_freezer))) NA_real_ else mean(ready_for_freezer, na.rm = TRUE),
      pct_flagged = if (all(is.na(flag_issue))) NA_real_ else mean(flag_issue, na.rm = TRUE),
      pct_liquid = if (all(is.na(drs_state))) NA_real_ else mean(drs_state == "Liquid", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_extractions))
}

#' Summarise extraction volumes by health structure over time
#' @param df Extraction dataset (already filtered)
#' @param time_unit Time aggregation for the series ("month" or "week")
#' @return Tibble with volume trend metrics per health structure
#' @export
summarise_health_structure_volume_trends <- function(df, time_unit = c("month", "week")) {
  df <- ensure_health_structure_column(df)

  time_unit <- match.arg(time_unit)

  if (is.null(df) || !nrow(df) ||
      !"health_structure" %in% names(df) ||
      !"extraction_date" %in% names(df)) {
    return(tibble::tibble(
      health_structure = character(),
      period = as.Date(character()),
      n_extractions = integer(),
      total_volume = numeric(),
      median_volume = numeric(),
      mean_volume = numeric(),
      pct_ready = numeric(),
      pct_matched_biobank = numeric()
    ))
  }

  df <- df %>%
    {
      if (!"drs_volume_ml" %in% names(.)) {
        .$drs_volume_ml <- rep(NA_real_, nrow(.))
      }
      if (!"ready_for_freezer" %in% names(.)) {
        .$ready_for_freezer <- rep(NA, nrow(.))
      }
      if (!"biobank_matched" %in% names(.)) {
        .$biobank_matched <- rep(NA, nrow(.))
      }
      .
    }

  period_fn <- switch(
    time_unit,
    month = function(x) lubridate::floor_date(x, "month"),
    week = function(x) lubridate::floor_date(x, "week")
  )

  df %>%
    dplyr::filter(
      !is.na(health_structure),
      health_structure != "",
      health_structure != "Unspecified"
    ) %>%
    dplyr::filter(!is.na(extraction_date)) %>%
    dplyr::mutate(period = period_fn(extraction_date)) %>%
    dplyr::filter(!is.na(period)) %>%
    dplyr::group_by(health_structure, period) %>%
    dplyr::summarise(
      n_extractions = dplyr::n(),
      total_volume = if (all(is.na(drs_volume_ml))) NA_real_ else sum(drs_volume_ml, na.rm = TRUE),
      median_volume = stats::median(drs_volume_ml, na.rm = TRUE),
      mean_volume = if (all(is.na(drs_volume_ml))) NA_real_ else mean(drs_volume_ml, na.rm = TRUE),
      pct_ready = if (all(is.na(ready_for_freezer))) NA_real_ else mean(ready_for_freezer, na.rm = TRUE),
      pct_matched_biobank = if (all(is.na(biobank_matched))) NA_real_ else mean(biobank_matched, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(health_structure, period)
}
