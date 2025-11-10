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
  # remove punctuation / spaces
  x <- gsub("[^a-z0-9]", "", x)
  if (kind == "barcode") {
    x <- sub("^kps", "", x)     # drop leading 'kps'
    x <- sub("^0+", "", x)      # drop leading zeros
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
      agg <- tmp |>
        dplyr::group_by(.__barcode_norm) |>
        dplyr::summarise(
          n_lab_ids = dplyr::n_distinct(.__labid_norm),
          lab_ids   = paste(sort(unique(.__labid_norm)), collapse = ", "),
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
      agg <- tmp |>
        dplyr::group_by(.__labid_norm) |>
        dplyr::summarise(
          n_barcodes = dplyr::n_distinct(.__barcode_norm),
          barcodes   = paste(sort(unique(.__barcode_norm)), collapse = ", "),
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
  if (structure != "all" && "health_structure" %in% names(df)) {
    df <- df %>% dplyr::filter(.data$health_structure == !!structure)
  }

  df
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
  if ("health_structure" %in% names(df)) {
    structures <- df$health_structure[!is.na(df$health_structure) & df$health_structure != ""]
    structures <- sort(unique(structures))
    updateSelectInput(
      session, "filter_structure",
      choices = c("All" = "all", stats::setNames(structures, structures))
    )
  }
}
