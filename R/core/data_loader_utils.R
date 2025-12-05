# R/core/data_loader_utils.R
# Core data loading utilities
# ============================================================================

# ---- Local helpers (no export) ----------------------------------------------

.norm_txt <- function(x) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII; NFC")
  }
  x <- trimws(tolower(x))
  x[x %in% c("", "na", "n/a", "null")] <- NA
  x
}

.norm_key <- function(x, kind = c("barcode","labid")) {
  kind <- match.arg(kind)
  x <- .norm_txt(x)
  # Only remove spaces and special punctuation, but keep dashes, underscores, dots
  # This prevents "001-A" and "001-B" from collapsing to the same ID
  x <- gsub("[^a-z0-9._-]", "", x)
  if (kind == "barcode") {
    x <- sub("^kps[-_]?", "", x)     # drop leading 'kps' with optional separator
  }
  x
}

.detect_col <- function(nms, patterns) {
  for (p in patterns) {
    hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NULL
}

.parse_date_safe <- function(x) {
  # Try Date -> dmy -> ymd (handles "JJ/MM/AAAA" style too)
  suppressWarnings({
    d <- as.Date(x)
    bad <- is.na(d)
    if (any(bad) && requireNamespace("lubridate", quietly = TRUE)) {
      xc <- as.character(x)
      d[bad]  <- suppressWarnings(lubridate::dmy(xc[bad]))
      bad2 <- is.na(d)
      if (any(bad2)) d[bad2] <- suppressWarnings(lubridate::ymd(xc[bad2]))
    }
    d
  })
}

# -----------------------------------------------------------------------------
#' Load biobank file from Excel
#' @param filepath Path to Excel file
#' @return Raw data frame
#' @export
load_biobank_file <- function(filepath) {
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  tryCatch({
    # Read Excel file
    df <- readxl::read_excel(filepath, sheet = 1)

    # Basic validation
    if (nrow(df) == 0) stop("File contains no data")

    # Clean column names immediately (retain a copy of original names if needed)
    df <- janitor::clean_names(df)

    message(sprintf("Loaded %d rows from %s", nrow(df), basename(filepath)))
    df

  }, error = function(e) {
    stop(sprintf("Failed to load file: %s", e$message))
  })
}

# -----------------------------------------------------------------------------
#' Get cache path for biobank file
#' @param filepath Path to Excel file
#' @param cache_dir Cache directory (default: NULL, will use site-aware path)
#' @return Path to RDS cache file
#' @export
get_biobank_cache_path <- function(filepath, cache_dir = NULL) {
  # Use site-aware cache directory if not specified
  if (is.null(cache_dir) && exists("config") && !is.null(config$site_paths)) {
    cache_dir <- config$site_paths$cache_dir
  } else if (is.null(cache_dir)) {
    cache_dir <- "data/biobank_cache"
  }

  # Ensure cache directory exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create cache filename based on original filename
  base_name <- tools::file_path_sans_ext(basename(filepath))
  cache_file <- file.path(cache_dir, paste0(base_name, ".rds"))

  cache_file
}

# -----------------------------------------------------------------------------
#' Check if biobank cache is fresh
#' @param excel_path Path to Excel file
#' @param cache_path Path to RDS cache file
#' @return TRUE if cache exists and is newer than Excel file
#' @export
biobank_cache_is_fresh <- function(excel_path, cache_path) {
  if (!file.exists(cache_path)) return(FALSE)
  if (!file.exists(excel_path)) return(FALSE)

  # Compare modification times
  excel_mtime <- file.info(excel_path)$mtime
  cache_mtime <- file.info(cache_path)$mtime

  cache_mtime >= excel_mtime
}

# -----------------------------------------------------------------------------
#' Load biobank file with RDS caching
#' @param filepath Path to Excel file
#' @param cache_dir Cache directory (default: NULL, will use site-aware path)
#' @param force_refresh Force refresh from Excel even if cache exists (default: FALSE)
#' @return Raw data frame
#' @export
load_biobank_file_cached <- function(filepath, cache_dir = NULL, force_refresh = FALSE) {
  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  cache_path <- get_biobank_cache_path(filepath, cache_dir)

  # Check if cache is fresh
  if (!force_refresh && biobank_cache_is_fresh(filepath, cache_path)) {
    message(sprintf("Loading from cache: %s", basename(cache_path)))
    tryCatch({
      df <- readRDS(cache_path)
      message(sprintf("Loaded %d rows from cache (fast!)", nrow(df)))
      return(df)
    }, error = function(e) {
      warning(sprintf("Failed to load cache, reading Excel: %s", e$message))
      # Fall through to Excel loading
    })
  }

  # Load from Excel
  message(sprintf("Reading Excel file: %s", basename(filepath)))
  df <- load_biobank_file(filepath)

  # Save to cache
  tryCatch({
    saveRDS(df, cache_path)
    message(sprintf("Cached to: %s", basename(cache_path)))
  }, error = function(e) {
    warning(sprintf("Failed to save cache: %s", e$message))
  })

  df
}

# -----------------------------------------------------------------------------
#' List available biobank files in directory
#' @param directory Path to directory containing Excel files
#' @return Character vector of file paths
#' @export
list_biobank_files <- function(directory) {
  if (!dir.exists(directory)) {
    warning(sprintf("Directory not found: %s", directory))
    return(character(0))
  }

  files <- list.files(
    directory,
    pattern = "\\.(xlsx|xls)$",
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Filter out temporary Excel files
  files <- files[!grepl("^~\\$", basename(files))]

  files
}

# -----------------------------------------------------------------------------
#' Get the most recent biobank file in directory
#' @param directory Path to directory containing Excel files
#' @return Path to the most recent file, or NULL if no files found
#' @export
get_latest_biobank_file <- function(directory) {
  files <- list_biobank_files(directory)

  if (length(files) == 0) {
    return(NULL)
  }

  # Get file modification times
  file_info <- file.info(files)
  file_info$path <- files

  # Sort by modification time (most recent first)
  file_info <- file_info[order(file_info$mtime, decreasing = TRUE), ]

  # Return the most recent file
  file_info$path[1]
}

# -----------------------------------------------------------------------------
#' Analyze data quality and generate comprehensive report
#'
#' Returns a list you can pass straight into `quality_report()` in the module:
#' - summary: rows_raw, rows_clean, columns, detected column names
#' - missing_barcode / missing_labid (among non-ghost rows)
#' - duplicates (on normalized key), barcode_conflicts (barcode_norm -> multiple labid_norm)
#' - completeness (percent per column)
#' - row_flags (per-row: date_sample + quality_flag = "OK"/"Invalid")
#' - row_flags_detailed (per-row: first failing reason for invalid rows)
#' - invalid_reasons (reason counts)
#' - clean_data (rows deemed OK with normalized/parsed fields available)
#'
#' @param df_raw Raw data frame (already read & clean_names())
#' @return Named list with quality metrics, flags, and clean_data
#' @export
analyze_data_quality <- function(df_raw) {
  
  if (is.null(df_raw) || !nrow(df_raw)) {
    stop("Empty data passed to analyze_data_quality()")
  }
  
  df <- df_raw
  
  nms <- names(df)
  
  # --- Detect key columns (FR headers included) ------------------------------
  barcode_col <- if ("barcode" %in% nms) "barcode" else
    .detect_col(nms, c("code.*barres.*kps", "code.*barre", "\\bbarcode\\b", "kps"))
  labid_col   <- if ("lab_id" %in% nms) "lab_id" else
    .detect_col(nms, c("^num[eé]ro$", "^numero$", "^num$", "lab.?id"))
  date_col    <- if ("date_sample" %in% nms) "date_sample" else
    .detect_col(nms, c("date.*pr[eé]l[eè]v", "date.*sample", "jj.?/?mm.?/?aaaa", "date.*prlv"))
  
  # --- Create normalized helper columns -------------------------------------
  if (!is.null(barcode_col) && barcode_col %in% nms) {
    df$.__barcode_norm <- .norm_key(df[[barcode_col]], "barcode")
  } else {
    df$.__barcode_norm <- NA_character_
  }
  
  if (!is.null(labid_col) && labid_col %in% nms) {
    df$.__labid_norm <- .norm_key(df[[labid_col]], "labid")
  } else {
    df$.__labid_norm <- NA_character_
  }
  
  # ghost rows = no identifier info at all
  keep <- !(is.na(df$.__barcode_norm) & is.na(df$.__labid_norm))
  df_kept <- df[keep, , drop = FALSE]
  # Track original row indices for filtering later
  df_kept$.__original_row_idx <- which(keep)
  
  # Parse sampling date
  if (!is.null(date_col) && date_col %in% names(df_kept)) {
    df_kept$.__date_sample <- .parse_date_safe(df_kept[[date_col]])
  } else {
    df_kept$.__date_sample <- as.Date(NA)
  }
  
  # --- Missing counts among non-ghost rows -----------------------------------
  missing_barcode <- sum(is.na(df_kept$.__barcode_norm), na.rm = TRUE)
  missing_labid   <- sum(is.na(df_kept$.__labid_norm),   na.rm = TRUE)
  
  # --- Key and conflicts/duplicates ------------------------------------------
  # Stable row key preferring barcode_norm, falling back to labid_norm
  key <- df_kept$.__barcode_norm
  key[is.na(key)] <- df_kept$.__labid_norm[is.na(key)]
  df_kept$.__key <- key
  
  # Duplicates: repeated keys (after normalization)
  dup_idx <- duplicated(df_kept$.__key) | duplicated(df_kept$.__key, fromLast = TRUE)
  duplicates <- if (any(dup_idx, na.rm = TRUE)) {
    df_kept[dup_idx & !is.na(df_kept$.__key), , drop = FALSE]
  } else {
    df_kept[0, , drop = FALSE]
  }
  
  # Conflicts: same barcode_norm mapping to multiple labid_norm values
  barcode_conflicts <- {
    tmp <- df_kept[!is.na(df_kept$.__barcode_norm) & !is.na(df_kept$.__labid_norm), , drop = FALSE]
    if (!nrow(tmp)) {
      tmp[0, , drop = FALSE]
    } else {
      # Get original column names
      labid_orig_col <- if (!is.null(labid_col) && labid_col %in% names(tmp)) labid_col else NULL
      barcode_orig_col <- if (!is.null(barcode_col) && barcode_col %in% names(tmp)) barcode_col else NULL

      agg <- tmp |>
        dplyr::group_by(.__barcode_norm) |>
        dplyr::summarise(
          n_lab_ids = dplyr::n_distinct(.__labid_norm),
          labids_norm = paste(sort(unique(.__labid_norm)), collapse = ", "),
          labids_original = if (!is.null(labid_orig_col)) {
            paste(sort(unique(as.character(.data[[labid_orig_col]]))), collapse = ", ")
          } else {
            paste(sort(unique(.__labid_norm)), collapse = ", ")
          },
          barcode_original = if (!is.null(barcode_orig_col)) {
            paste(sort(unique(as.character(.data[[barcode_orig_col]]))), collapse = ", ")
          } else {
            NA_character_
          },
          .groups   = "drop"
        ) |>
        dplyr::filter(n_lab_ids > 1) |>
        dplyr::rename(barcode_norm = .__barcode_norm)
      as.data.frame(agg)
    }
  }

  # Conflicts: same labid_norm mapping to multiple barcode_norm values
  labid_conflicts <- {
    tmp <- df_kept[!is.na(df_kept$.__labid_norm) & !is.na(df_kept$.__barcode_norm), , drop = FALSE]
    if (!nrow(tmp)) {
      tmp[0, , drop = FALSE]
    } else {
      # Get original column names
      labid_orig_col <- if (!is.null(labid_col) && labid_col %in% names(tmp)) labid_col else NULL
      barcode_orig_col <- if (!is.null(barcode_col) && barcode_col %in% names(tmp)) barcode_col else NULL

      agg <- tmp |>
        dplyr::group_by(.__labid_norm) |>
        dplyr::summarise(
          n_barcodes = dplyr::n_distinct(.__barcode_norm),
          barcodes_norm = paste(sort(unique(.__barcode_norm)), collapse = ", "),
          barcodes_original = if (!is.null(barcode_orig_col)) {
            paste(sort(unique(as.character(.data[[barcode_orig_col]]))), collapse = ", ")
          } else {
            paste(sort(unique(.__barcode_norm)), collapse = ", ")
          },
          labid_original = if (!is.null(labid_orig_col)) {
            paste(sort(unique(as.character(.data[[labid_orig_col]]))), collapse = ", ")
          } else {
            NA_character_
          },
          .groups    = "drop"
        ) |>
        dplyr::filter(n_barcodes > 1) |>
        dplyr::rename(labid_norm = .__labid_norm)
      as.data.frame(agg)
    }
  }
  
  # --- Completeness (simple %) on kept rows ----------------------------------
  completeness <- df_kept |>
    dplyr::summarise(dplyr::across(everything(), ~ sum(!is.na(.)) / dplyr::n() * 100)) |>
    tidyr::pivot_longer(everything(), names_to = "column", values_to = "percent_complete") |>
    dplyr::arrange(dplyr::desc(percent_complete))
  
  # --- Row-level validation & flags ------------------------------------------
  # Rules to define invalidity and the first failing reason
  compute_reason <- function(i) {
    if (is.na(df_kept$.__barcode_norm[i])) return("Missing barcode")
    if (is.na(df_kept$.__labid_norm[i]))   return("Missing lab ID")
    # barcode conflict?
    if (nrow(barcode_conflicts)) {
      if (df_kept$.__barcode_norm[i] %in% barcode_conflicts$barcode_norm) return("Barcode conflict")
    }
    # duplicate key?
    if (nrow(labid_conflicts)) {
      if (df_kept$.__labid_norm[i] %in% labid_conflicts$labid_norm) return("Lab ID conflict")
    }
    if (!is.na(df_kept$.__key[i]) && sum(df_kept$.__key == df_kept$.__key[i], na.rm = TRUE) > 1) {
      return("Duplicate key")
    }
    # missing date (optional rule)
    if (is.na(df_kept$.__date_sample[i])) return("Missing sampling date")
    # otherwise OK
    return(NA_character_)
  }
  
  invalid_reason <- vapply(seq_len(nrow(df_kept)), compute_reason, FUN.VALUE = character(1))
  quality_flag_simple <- ifelse(is.na(invalid_reason), "OK", "Invalid")
  
  row_flags <- data.frame(
    date_sample = df_kept$.__date_sample,
    quality_flag = quality_flag_simple,
    stringsAsFactors = FALSE
  )
  
  row_flags_detailed <- data.frame(
    date_sample = df_kept$.__date_sample,
    reason = ifelse(is.na(invalid_reason), "OK", invalid_reason),
    stringsAsFactors = FALSE
  )
  
  # --- Invalid reasons aggregation -------------------------------------------
  invalid_reasons <- row_flags_detailed |>
    dplyr::filter(reason != "OK") |>
    dplyr::count(reason, name = "count") |>
    dplyr::arrange(dplyr::desc(count))
  if (!nrow(invalid_reasons)) {
    invalid_reasons <- data.frame(reason = character(0), count = integer(0))
  }
  
  # --- Clean data (rows that passed validation) -------------------------------
  clean_idx <- which(quality_flag_simple == "OK")
  invalid_idx <- which(quality_flag_simple == "Invalid")

  # Get original row indices for invalid rows (in df_raw)
  invalid_original_idx <- if (length(invalid_idx)) {
    df_kept$.__original_row_idx[invalid_idx]
  } else {
    integer(0)
  }

  clean_data <- if (length(clean_idx)) df_kept[clean_idx, , drop = FALSE] else df_kept[0, , drop = FALSE]
  # expose parsed date as "date_sample" for downstream code
  clean_data$date_sample <- df_kept$.__date_sample[clean_idx]
  
  # --- Quality flags summary (overall) ----------------------------------------
  quality_flags <- data.frame(
    quality_flag = c("OK", "Invalid"),
    n = c(sum(quality_flag_simple == "OK"), sum(quality_flag_simple == "Invalid")),
    stringsAsFactors = FALSE
  )
  
  # --- Final report -----------------------------------------------------------
  list(
    summary = list(
      rows_raw    = nrow(df_raw),
      rows_kept   = nrow(df_kept),
      rows_clean  = nrow(clean_data),
      columns     = ncol(df_raw),
      barcode_col = barcode_col,
      labid_col   = labid_col,
      date_col    = date_col
    ),
    missing_barcode   = missing_barcode,
    missing_labid     = missing_labid,
    duplicates        = duplicates,
    barcode_conflicts = barcode_conflicts,
    labid_conflicts   = labid_conflicts,
    completeness      = completeness,
    quality_flags     = quality_flags,
    row_flags         = row_flags,          # for timeline (OK vs Invalid per date)
    row_flags_detailed= row_flags_detailed, # for future detailed charts
    invalid_reasons   = invalid_reasons,
    invalid_row_indices = invalid_original_idx,  # original row indices in df_raw for filtering
    clean_data        = clean_data          # optional convenience for the app
  )
}

# -----------------------------------------------------------------------------
#' Apply filters to cleaned data
#' @param df Cleaned data frame
#' @param date_range Date range (vector of length 2)
#' @param study Study filter ("all" or specific study)
#' @param province Province filter ("all" or specific province)
#' @param zone Zone filter ("all" or specific zone)
#' @return Filtered data frame
#' @export
apply_filters <- function(df, date_range = NULL, study = "all",
                          province = "all", zone = "all",
                          structure = "all") {

  if (is.null(df) || nrow(df) == 0) return(df)
  
  # Date filter
  if (!is.null(date_range) && "date_sample" %in% names(df)) {
    df <- df %>%
      dplyr::filter(
        .data$date_sample >= date_range[1],
        .data$date_sample <= date_range[2]
      )
  }
  
  # Study filter
  if (study != "all" && "study" %in% names(df)) {
    df <- df %>% dplyr::filter(.data$study == !!study)
  }
  
  # Province filter
  if (province != "all" && "province" %in% names(df)) {
    df <- df %>% dplyr::filter(.data$province == !!province)
  }
  
  # Zone filter
  if (zone != "all" && "health_zone" %in% names(df)) {
    df <- df %>% dplyr::filter(.data$health_zone == !!zone)
  }

  # Structure filter
  if (!is.null(structure) && structure != "all") {
    target <- normalize_structure_value(structure)

    if (!is.na(target)) {
      candidate_cols <- intersect(
        c(
          "health_structure",
          "health_facility",
          "structure_sanitaire",
          "biobank_health_facility",
          "biobank_structure_sanitaire"
        ),
        names(df)
      )

      if (length(candidate_cols)) {
        df <- df %>%
          dplyr::filter(
            dplyr::if_any(
              dplyr::all_of(candidate_cols),
              ~ normalize_structure_value(.x) == target
            )
          )
      }
    }
  }

  df
}

# -----------------------------------------------------------------------------
#' Normalize structure names to a canonical key
#' @param x Character vector of structure names
#' @return Uppercase character vector suitable for equality checks
#' @export
normalize_structure_value <- function(x) {
  if (is.null(x)) {
    return(rep(NA_character_, length.out = 0))
  }

  values <- stringr::str_trim(as.character(x))
  values <- stringr::str_replace_all(values, "\\s+", " ")
  values <- stringi::stri_trans_general(values, "Latin-ASCII")
  values <- stringr::str_to_upper(values)
  values[values %in% c("", "NA", "N/A", "NONE", "NULL")] <- NA_character_
  values
}

# -----------------------------------------------------------------------------
#' Format raw structure labels for display
#' @param x Character vector of structure names
#' @return Trimmed character vector with empty values set to NA
format_structure_label <- function(x) {
  if (is.null(x)) {
    return(rep(NA_character_, length.out = 0))
  }

  labels <- stringr::str_squish(as.character(x))
  labels[labels == ""] <- NA_character_
  labels
}

# -----------------------------------------------------------------------------
#' Build unique structure choices from available columns
#' @param df Data frame containing structure columns
#' @return Tibble with key/label pairs for the filter
build_structure_choices <- function(df) {
  candidate_cols <- intersect(
    c(
      "health_structure",
      "health_facility",
      "structure_sanitaire",
      "biobank_health_facility",
      "biobank_structure_sanitaire"
    ),
    names(df)
  )

  if (!length(candidate_cols)) {
    return(tibble::tibble())
  }

  values <- unlist(lapply(candidate_cols, function(col) df[[col]]), use.names = FALSE)

  tibble::tibble(
    key = normalize_structure_value(values),
    label = format_structure_label(values)
  ) %>%
    dplyr::filter(!is.na(.data$key)) %>%
    dplyr::group_by(.data$key) %>%
    dplyr::summarise(
      label = {
        valid <- label[!is.na(label)]
        if (!length(valid)) {
          NA_character_
        } else {
          valid[which.max(nchar(valid))]
        }
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(label = dplyr::coalesce(.data$label, .data$key)) %>%
    dplyr::arrange(.data$label)
}

# -----------------------------------------------------------------------------
#' Update filter choices based on data
#' @param session Shiny session
#' @param df Cleaned data frame
#' @export
update_filter_choices <- function(session, df) {
  if (is.null(df) || nrow(df) == 0) return()
  
  ns <- session$ns
  
  # Update study choices
  if ("study" %in% names(df)) {
    studies <- sort(unique(df$study[!is.na(df$study)]))
    updateSelectInput(
      session, "filter_study",
      choices = c("All" = "all", stats::setNames(studies, studies))
    )
  }
  
  # Update province choices
  if ("province" %in% names(df)) {
    provinces <- sort(unique(df$province[!is.na(df$province)]))
    updateSelectInput(
      session, "filter_province",
      choices = c("All" = "all", stats::setNames(provinces, provinces))
    )
  }
  
  # Update zone choices
  if ("health_zone" %in% names(df)) {
    zones <- sort(unique(df$health_zone[!is.na(df$health_zone)]))
    updateSelectInput(
      session, "filter_zone",
      choices = c("All" = "all", stats::setNames(zones, zones))
    )
  }

  # Update structure choices
  structure_choices <- build_structure_choices(df)

  if (nrow(structure_choices)) {
    current_value <- session$input[["filter_structure"]]
    selected_value <- "all"

    if (!is.null(current_value) && current_value != "all") {
      normalized <- normalize_structure_value(current_value)
      if (!is.na(normalized) && normalized %in% structure_choices$key) {
        selected_value <- normalized
      }
    } else if (identical(current_value, "all")) {
      selected_value <- "all"
    }

    updateSelectInput(
      session, "filter_structure",
      choices = c(
        "All" = "all",
        stats::setNames(structure_choices$label, structure_choices$key)
      ),
      selected = selected_value
    )
  }
}
