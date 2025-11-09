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
    record_number = c("numero", "record", "sample_number"),
    extraction_date = c("extraction_date", "date_extraction", "date", "extractiondate"),
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

  file_date <- .infer_date_from_filename(basename(filepath))

  df <- df %>%
    dplyr::transmute(
      source_file = basename(filepath),
      sample_id = as.character(sample_id),
      record_number = as.character(record_number),
      extraction_date = .parse_extraction_date(extraction_date),
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
      filter_type = stringr::str_to_title(trimws(as.character(filter_type))),
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
      project = stringr::str_to_upper(trimws(as.character(project))),
      batch = as.character(batch),
      rsc_run = as.character(rsc_run),
      rsc_position = as.character(rsc_position),
      rack = as.character(rack),
      rack_row = as.character(rack_row),
      rack_column = as.character(rack_column),
      remarks = as.character(remarks)
    ) %>%
    dplyr::mutate(
      sample_id = dplyr::coalesce(sample_id, record_number),
      extraction_date = dplyr::coalesce(extraction_date, file_date),
      filter_type = dplyr::if_else(is.na(filter_type) | filter_type == "", "Unspecified", filter_type),
      project = dplyr::if_else(is.na(project) | project == "", "UNSPECIFIED", project),
      drs_state = dplyr::if_else(is.na(drs_state) | drs_state == "", "Unknown", drs_state),
      extract_quality = dplyr::if_else(is.na(extract_quality) | extract_quality == "", "Unknown", extract_quality),
      flag_issue = dplyr::case_when(
        is.na(drs_volume_ml) ~ TRUE,
        drs_volume_ml < 1 ~ TRUE,
        drs_state %in% c("Viscous", "Coagulated") ~ TRUE,
        extract_quality == "Foncé" ~ TRUE,
        TRUE ~ FALSE
      ),
      ready_for_freezer = !flag_issue
    )

  df
}

#' Load and combine all extraction data sets
#' @param directory Path to extraction data directory
#' @return Tibble with combined extraction records
#' @export
load_extraction_dataset <- function(directory = config$paths$extractions_dir) {
  files <- list_extraction_files(directory)
  if (!length(files)) {
    message("No extraction files found; returning empty dataset")
    return(tibble::tibble(
      source_file = character(),
      sample_id = character(),
      record_number = character(),
      extraction_date = as.Date(character()),
      drs_state = character(),
      drs_volume_ml = numeric(),
      filter_type = character(),
      extract_quality = character(),
      technician = character(),
      project = character(),
      batch = character(),
      rsc_run = character(),
      rsc_position = character(),
      rack = character(),
      rack_row = character(),
      rack_column = character(),
      remarks = character(),
      flag_issue = logical(),
      ready_for_freezer = logical()
    ))
  }

  purrr::map_dfr(files, load_extraction_file)
}

#' Summarise extraction metrics for KPI displays
#' @param df Extraction dataset (already filtered)
#' @return Named list with KPI values
#' @export
summarise_extraction_metrics <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(list(
      total = 0,
      ready = 0,
      median_volume = NA_real_,
      pct_liquid = NA_real_,
      pct_clear = NA_real_,
      flagged = 0
    ))
  }

  safe_pct <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_real_)
    mean(x)
  }

  list(
    total = nrow(df),
    ready = sum(df$ready_for_freezer, na.rm = TRUE),
    median_volume = stats::median(df$drs_volume_ml, na.rm = TRUE),
    pct_liquid = safe_pct(df$drs_state == "Liquid"),
    pct_clear = safe_pct(df$extract_quality == "Clear"),
    flagged = sum(df$flag_issue, na.rm = TRUE)
  )
}
