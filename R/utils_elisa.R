# R/utils_elisa.R
# Utilities for loading and caching ELISA indirect assay data
# Modeled after MIC qPCR data loading architecture

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
})

# ============================================================================
# NORMALIZATION HELPERS (mimic MIC patterns)
# ============================================================================

#' Normalize ID for matching (uppercase, trimmed)
#' @param x Character vector
#' @return Normalized character vector
normalize_id <- function(x) {
  x %>% as.character() %>% str_trim() %>% toupper()
}

#' Normalize key for barcode or lab ID matching
#' Preserves dashes, underscores, and dots to prevent sample ID collisions
#' @param x Character vector
#' @param kind Type of key ("barcode" or "labid")
#' @return Normalized key
.norm_key <- function(x, kind = c("barcode", "labid")) {
  kind <- match.arg(kind)
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "n/a", "null")] <- NA_character_
  # Only remove spaces and special punctuation, but keep dashes, underscores, dots
  # This prevents "001-A" and "001-B" from collapsing to the same ID
  x <- gsub("[^a-z0-9._-]", "", x)
  if (kind == "barcode") {
    x <- sub("^kps[-_]?", "", x)
  }
  x
}

# ============================================================================
# SCREENING NUMBER CALCULATION
# ============================================================================

#' Add screening numbers to ELISA data
#' Ranks tests by date for each unique sample (identified by barcode or numero_labo)
#' @param elisa_data Data frame with ELISA results
#' @return Data frame with screening_num and is_first_screening columns added
add_screening_numbers <- function(elisa_data) {
  if (nrow(elisa_data) == 0) {
    return(elisa_data %>%
      mutate(
        screening_num = integer(),
        is_first_screening = logical()
      ))
  }

  start_time <- Sys.time()

  # Only process samples (not controls)
  samples <- elisa_data %>%
    filter(sample_type == "sample")

  controls <- elisa_data %>%
    filter(sample_type != "sample")

  message("  → Split samples/controls: ", round(difftime(Sys.time(), start_time, units = "secs"), 2), "s")

  # For samples, add screening numbers
  if (nrow(samples) > 0) {
    norm_start <- Sys.time()

    # Check if normalized columns already exist (from link_elisa_to_biobank)
    # If not, compute them (fallback for standalone use)
    if (!"barcode_norm" %in% names(samples) || !"numero_norm" %in% names(samples)) {
      message("  → WARNING: Normalized columns missing, computing them (", nrow(samples), " rows)...")
      samples <- samples %>%
        mutate(
          barcode_norm = .norm_key(code_barres_kps, "barcode"),
          numero_norm = .norm_key(numero_labo, "labid")
        )
      remove_norm_cols <- TRUE
      message("  → Normalization: ", round(difftime(Sys.time(), norm_start, units = "secs"), 2), "s")
    } else {
      message("  → Using pre-computed normalized columns ✓")
      remove_norm_cols <- FALSE
    }

    group_start <- Sys.time()
    samples <- samples %>%
      mutate(
        # Reuse pre-computed normalized IDs (much faster!)
        sample_id = coalesce(barcode_norm, numero_norm)
      ) %>%
      group_by(sample_id, elisa_type) %>%
      arrange(plate_date, .by_group = TRUE) %>%
      mutate(
        screening_num = dense_rank(plate_date),
        is_first_screening = (screening_num == 1)
      ) %>%
      ungroup() %>%
      select(-sample_id)

    message("  → Group/arrange/rank: ", round(difftime(Sys.time(), group_start, units = "secs"), 2), "s")

    # Clean up temporary norm columns if we created them
    if (remove_norm_cols) {
      samples <- samples %>% select(-barcode_norm, -numero_norm)
    }
  }

  # For controls, set NA
  if (nrow(controls) > 0) {
    controls <- controls %>%
      mutate(
        screening_num = NA_integer_,
        is_first_screening = NA
      )
  }

  # Recombine
  combine_start <- Sys.time()
  result <- bind_rows(samples, controls) %>%
    arrange(plate_date, plate_id, sample)
  message("  → Combine & final arrange: ", round(difftime(Sys.time(), combine_start, units = "secs"), 2), "s")
  message("  → TOTAL TIME: ", round(difftime(Sys.time(), start_time, units = "secs"), 2), "s")

  result
}

# ============================================================================
# FLEXIBLE COLUMN MATCHING (mimic MIC pattern)
# ============================================================================

#' Match column name flexibly
#' @param names Character vector of column names
#' @param candidates Character vector of possible names (in priority order)
#' @return First matching column name, or NULL if none found
match_column_name <- function(names, candidates) {
  for (pattern in candidates) {
    # Try exact match first (case-insensitive)
    exact <- names[tolower(names) == tolower(pattern)]
    if (length(exact)) return(exact[1])

    # Try pattern match
    matches <- grep(pattern, names, ignore.case = TRUE, value = TRUE)
    if (length(matches)) return(matches[1])
  }
  NULL
}

#' Safely extract column with fallback
#' @param df Data frame
#' @param candidates Character vector of possible column names
#' @param default Default value if column not found
#' @return Column vector or default
safe_column <- function(df, candidates, default = NA_character_) {
  col_name <- match_column_name(names(df), candidates)
  if (!is.null(col_name)) {
    return(df[[col_name]])
  }
  rep(default, nrow(df))
}

# ============================================================================
# BIOBANK LINKAGE (improved with flexible matching)
# ============================================================================

#' Prepare biobank lookup table with flexible column matching
#' @param biobank_df Biobank data frame
#' @return Prepared lookup table or NULL
prepare_biobank_lookup <- function(biobank_df) {
  if (is.null(biobank_df) || !nrow(biobank_df)) return(NULL)

  # Find ID columns using flexible matching
  barcode_col <- match_column_name(
    names(biobank_df),
    c("code_barres_kps", "code-barres kps", "barcode", "sample_barcode", "kps_barcode")
  )

  numero_col <- match_column_name(
    names(biobank_df),
    c("lab_id", "numero", "numero_labo", "sample_id", "id_labo")
  )

  # Find demographic columns using flexible matching
  province_col <- match_column_name(
    names(biobank_df),
    c("province", "Province")
  )

  health_zone_col <- match_column_name(
    names(biobank_df),
    c("health_zone", "zone_de_sante", "zone", "health zone", "healthzone")
  )

  structure_col <- match_column_name(
    names(biobank_df),
    c("health_structure", "structure_sanitaire", "structure", "health_facility",
      "facility", "health facility")
  )

  sex_col <- match_column_name(
    names(biobank_df),
    c("sex", "sexe", "gender", "genre")
  )

  age_col <- match_column_name(
    names(biobank_df),
    c("age", "age_years")
  )

  age_group_col <- match_column_name(
    names(biobank_df),
    c("age_group", "age_groupe", "age group", "agegroup")
  )

  date_sample_col <- match_column_name(
    names(biobank_df),
    c("date_sample", "date_prelevement", "collection_date", "sample_date",
      "date prelevement", "date de prelevement")
  )

  cohort_col <- match_column_name(
    names(biobank_df),
    c("study", "etude", "cohort", "cohorte")
  )

  # Build lookup table with found columns
  lookup <- biobank_df %>%
    mutate(
      biobank_barcode = if (!is.null(barcode_col)) .data[[barcode_col]] else NA_character_,
      biobank_lab_id = if (!is.null(numero_col)) as.character(.data[[numero_col]]) else NA_character_,
      barcode_norm = .norm_key(biobank_barcode, "barcode"),
      numero_norm = .norm_key(biobank_lab_id, "labid"),
      Province = if (!is.null(province_col)) .data[[province_col]] else NA_character_,
      HealthZone = if (!is.null(health_zone_col)) .data[[health_zone_col]] else NA_character_,
      Structure = if (!is.null(structure_col)) .data[[structure_col]] else NA_character_,
      Sex = if (!is.null(sex_col)) .data[[sex_col]] else NA_character_,
      Age = if (!is.null(age_col)) as.numeric(.data[[age_col]]) else NA_real_,
      AgeGroup = if (!is.null(age_group_col)) .data[[age_group_col]] else NA_character_,
      SampleDate = if (!is.null(date_sample_col)) as.Date(.data[[date_sample_col]]) else as.Date(NA),
      Cohort = if (!is.null(cohort_col)) .data[[cohort_col]] else NA_character_
    ) %>%
    filter(!is.na(barcode_norm) | !is.na(numero_norm)) %>%
    select(barcode_norm, numero_norm, biobank_barcode, biobank_lab_id,
           Province, HealthZone, Structure, Sex, Age, AgeGroup, SampleDate, Cohort) %>%
    distinct(barcode_norm, numero_norm, .keep_all = TRUE)

  if (!nrow(lookup)) return(NULL)
  lookup
}

#' Link ELISA data to biobank with flexible column matching
#' @param elisa_df ELISA data frame
#' @param biobank_df Biobank data frame
#' @return ELISA data with biobank columns joined
link_elisa_to_biobank <- function(elisa_df, biobank_df) {
  if (is.null(elisa_df) || !nrow(elisa_df)) return(tibble())

  # Ensure ELISA has necessary ID columns (add if missing with proper initialization)
  required_cols <- c("sample_type", "sample", "sample_code", "numero_labo", "code_barres_kps", "plate_id")
  for (col in required_cols) {
    if (!col %in% names(elisa_df)) {
      elisa_df[[col]] <- NA_character_
    }
  }

  # Ensure numeric columns exist with proper type
  if (!"plate_num" %in% names(elisa_df)) {
    elisa_df$plate_num <- NA_integer_
  } else if (is.character(elisa_df$plate_num)) {
    elisa_df$plate_num <- as.integer(elisa_df$plate_num)
  }

  # Normalize ELISA IDs - use direct column access instead of .data pronoun
  elisa_prepped <- elisa_df %>%
    mutate(
      barcode_norm = .norm_key(code_barres_kps, "barcode"),
      numero_norm = .norm_key(coalesce(numero_labo, sample_code, sample), "labid"),
      SampleID_norm = normalize_id(coalesce(code_barres_kps, numero_labo, sample_code, sample))
    )

  # Prepare biobank lookup
  lookup <- prepare_biobank_lookup(biobank_df)

  if (is.null(lookup)) {
    message("No valid biobank lookup data available")
    return(elisa_prepped %>% mutate(BiobankMatched = FALSE))
  }

  # Join on barcode first - ensure unique barcode_norm
  lookup_barcode <- lookup %>%
    filter(!is.na(barcode_norm)) %>%
    distinct(barcode_norm, .keep_all = TRUE) %>%
    select(barcode_norm, biobank_barcode, biobank_lab_id, Province, HealthZone,
           Structure, Sex, Age, AgeGroup, SampleDate, Cohort)

  # Separate lookup for numero - ensure unique numero_norm
  lookup_numero <- lookup %>%
    filter(!is.na(numero_norm)) %>%
    distinct(numero_norm, .keep_all = TRUE) %>%
    select(numero_norm, biobank_barcode, biobank_lab_id, Province, HealthZone,
           Structure, Sex, Age, AgeGroup, SampleDate, Cohort)

  # Left join on barcode
  joined <- elisa_prepped %>%
    left_join(lookup_barcode, by = "barcode_norm", relationship = "many-to-one")

  # Ensure biobank columns exist after join
  if (!"biobank_barcode" %in% names(joined)) joined$biobank_barcode <- NA_character_
  if (!"biobank_lab_id" %in% names(joined)) joined$biobank_lab_id <- NA_character_
  if (!"Province" %in% names(joined)) joined$Province <- NA_character_
  if (!"HealthZone" %in% names(joined)) joined$HealthZone <- NA_character_
  if (!"Structure" %in% names(joined)) joined$Structure <- NA_character_
  if (!"Sex" %in% names(joined)) joined$Sex <- NA_character_
  if (!"Age" %in% names(joined)) joined$Age <- NA_real_
  if (!"AgeGroup" %in% names(joined)) joined$AgeGroup <- NA_character_
  if (!"SampleDate" %in% names(joined)) joined$SampleDate <- as.Date(NA)
  if (!"Cohort" %in% names(joined)) joined$Cohort <- NA_character_

  # For unmatched records, try joining on numero
  still_unmatched <- joined %>%
    filter(is.na(biobank_barcode) & is.na(biobank_lab_id))

  if (nrow(still_unmatched) > 0) {
    # Join unmatched on numero
    unmatched_joined <- still_unmatched %>%
      select(-biobank_barcode, -biobank_lab_id, -Province, -HealthZone, -Structure,
             -Sex, -Age, -AgeGroup, -SampleDate, -Cohort) %>%
      left_join(lookup_numero, by = "numero_norm", relationship = "many-to-one")

    # Combine matched and newly matched
    matched <- joined %>% filter(!is.na(biobank_barcode) | !is.na(biobank_lab_id))
    joined <- bind_rows(matched, unmatched_joined)
  }

  # Add match indicator
  joined <- joined %>%
    mutate(BiobankMatched = !is.na(biobank_barcode) | !is.na(biobank_lab_id))

  joined
}

#' Ensure core ELISA columns exist
#'
#' When ELISA data is pulled from cache or partially parsed files, some
#' expected columns may be missing. This helper adds them with appropriate
#' default values to avoid dplyr "Unknown or uninitialised column" warnings
#' downstream.
ensure_elisa_columns <- function(df) {
  required_cols <- list(
    plate_id = NA_character_,
    plate_num = NA_integer_,
    # NOTE: plate_number is NOT included here - it's calculated later in load_elisa_data()
    # to avoid column conflicts during left_join
    plate_date = as.Date(NA),
    elisa_type = NA_character_,  # CRITICAL: Required for filtering by PE/VSG
    sample_type = NA_character_,
    sample = NA_character_,      # NOTE: This is PLATE POSITION, not sample ID
    sample_code = NA_character_,
    numero_labo = NA_character_, # Actual sample ID (lab numero)
    code_barres_kps = NA_character_, # Actual sample ID (barcode)
    status_raw = NA_character_,  # CRITICAL: Required for consolidation
    status_final = NA_character_ # CRITICAL: Required for reporting
  )

  for (col_name in names(required_cols)) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- required_cols[[col_name]]
    }
  }

  df
}

# ============================================================================
# PERSISTENT RDS CACHING (per-file, like MIC)
# ============================================================================

#' Get ELISA cache directory
#' @return Path to cache directory
get_elisa_cache_dir <- function() {
  # Use site-aware cache directory
  cache_dir <- if (!is.null(config$site_paths)) {
    config$site_paths$cache_dir
  } else {
    file.path("data", "ELISA_cache")
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

# =============================================================================
# ELISA CONSOLIDATED SNAPSHOT CACHE
# =============================================================================
# Same incremental snapshot approach as MIC: one file read on startup,
# rebuild only when new Excel files are detected.
# =============================================================================

.elisa_snapshot_version <- 1L

load_elisa_snapshot <- function(file_list, cache_dir) {
  snapshot_path <- file.path(cache_dir, "elisa_snapshot.rds")
  snapshot <- if (file.exists(snapshot_path)) {
    tryCatch(readRDS(snapshot_path), error = function(e) NULL)
  } else NULL

  # Build per-file hashes based on path + mtime
  info <- file.info(file_list)
  file_hashes <- setNames(
    mapply(function(p, m) digest::digest(paste(p, m)), file_list, info$mtime),
    file_list
  )

  # If snapshot exists and is the right version, find unchanged files
  reused_data <- list()
  if (!is.null(snapshot) &&
      identical(snapshot$version, .elisa_snapshot_version) &&
      is.list(snapshot$parsed_files) &&
      is.data.frame(snapshot$manifest)) {

    snap_hashes <- setNames(snapshot$manifest$file_hash, snapshot$manifest$file_path)

    for (fp in file_list) {
      h <- file_hashes[[fp]]
      if (!is.null(snap_hashes[[fp]]) && identical(snap_hashes[[fp]], h)) {
        reused_data[[fp]] <- snapshot$parsed_files[[fp]]
      }
    }
    n_reused <- sum(sapply(reused_data, Negate(is.null)))
    message(sprintf("[ELISA snapshot] Reusing %d / %d files from snapshot",
                    n_reused, length(file_list)))
  }

  list(reused = reused_data, file_hashes = file_hashes)
}

save_elisa_snapshot <- function(parsed_files, file_list, file_hashes, cache_dir) {
  snapshot_path <- file.path(cache_dir, "elisa_snapshot.rds")
  info <- file.info(file_list)
  snapshot <- list(
    version = .elisa_snapshot_version,
    manifest = tibble(
      file_path  = file_list,
      file_hash  = unname(file_hashes[file_list]),
      mtime      = info$mtime
    ),
    parsed_files = parsed_files,
    saved_at = Sys.time()
  )
  tryCatch(
    saveRDS(snapshot, snapshot_path),
    error = function(e) message("[ELISA snapshot] Could not save: ", e$message)
  )
  invisible(NULL)
}

#' Get cache path for an ELISA file
#' @param elisa_file Path to ELISA file
#' @return Path to RDS cache file
get_elisa_cache_path <- function(elisa_file) {
  file.path(
    get_elisa_cache_dir(),
    paste0(tools::file_path_sans_ext(basename(elisa_file)), ".rds")
  )
}

#' Check if cache is fresh
#' @param elisa_file Path to ELISA file
#' @param cache_path Path to cache file
#' @return TRUE if cache is fresh, FALSE otherwise
elisa_cache_is_fresh <- function(elisa_file, cache_path) {
  if (!file.exists(cache_path)) return(FALSE)

  elisa_info <- file.info(elisa_file)
  cache_info <- file.info(cache_path)

  if (is.na(elisa_info$mtime) || is.na(cache_info$mtime)) return(FALSE)

  # Cache valid if newer or equal to source file
  cache_info$mtime >= elisa_info$mtime
}

# ============================================================================
# DATA LOADING WITH CACHING
# ============================================================================

# Cache environment (for session-level hash-based caching)
.elisa_cache_env <- new.env(parent = emptyenv())

# Cache version - increment this when data structure changes to invalidate old caches
.elisa_cache_version <- "v14_status_raw_fix"

#' Ensure ELISA modular pipeline is loaded
#' Sources the modular parser script if the function isn't in memory yet.
#' Cache invalidation is handled per-file via embedded parser_version fields —
#' no bulk deletion is ever performed here.
ensure_elisa_parser <- function() {
  modular_path <- file.path("R", "modules", "elisa_shared", "process_elisa_modular.R")
  if (!file.exists(modular_path)) {
    stop("ELISA modular processing script not found: ", modular_path)
  }
  if (!exists("process_elisa_file_modular", mode = "function")) {
    source(modular_path, local = .GlobalEnv)
    message("✓ ELISA modular pipeline loaded")
  }
}

#' Parse a single ELISA file with RDS caching
#' @param file_path Path to ELISA file
#' @param cv_max_ag_plus Maximum CV threshold for Ag+ control
#' @param cv_max_ag0 Maximum CV threshold for Ag0 control
#' @return Tibble with parsed ELISA data, or NULL on error
parse_single_elisa_file <- function(file_path, cv_max_ag_plus = 20, cv_max_ag0 = 20) {
  cache_path <- get_elisa_cache_path(file_path)

  # Try loading from cache if fresh.
  # The cache carries a parser_version field; if it doesn't match the current
  # version this single file is silently re-parsed and the cache overwritten.
  # No bulk deletion is ever needed.
  if (elisa_cache_is_fresh(file_path, cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached)) {
      # Handle both old format (plain data frame) and new format (list with $data + $parser_version)
      if (is.data.frame(cached) && nrow(cached) > 0) {
        # Old format — no version → treat as stale, re-parse once to upgrade
        cached <- NULL
      } else if (is.list(cached) &&
                 identical(cached$parser_version, .elisa_cache_version) &&
                 is.data.frame(cached$data) && nrow(cached$data) > 0) {
        return(cached$data)
      } else {
        cached <- NULL  # stale version or corrupt
      }
    }
  }

  # Parse file using modular pipeline
  result <- tryCatch(
    {
      # Build QC settings
      qc_settings <- elisa_default_qc_settings()
      qc_settings$cv_max_ag_plus <- cv_max_ag_plus
      qc_settings$cv_max_ag0 <- cv_max_ag0

      # Process using modular pipeline
      modular_output <- process_elisa_file_modular(file_path, qc_settings)

      if (is.null(modular_output) || !nrow(modular_output)) {
        warning("No data parsed from ", basename(file_path))
        return(NULL)
      }

      # Convert to legacy format for backward compatibility
      parsed <- convert_to_legacy_format(modular_output)

      # Save to cache — wrap in a list that carries the parser version
      tryCatch(
        saveRDS(list(data = parsed, parser_version = .elisa_cache_version), cache_path),
        error = function(e) warning("Failed to save cache for ", basename(file_path), ": ", e$message)
      )

      parsed
    },
    error = function(e) {
      warning("Failed to parse ", basename(file_path), ": ", e$message)
      NULL
    }
  )

  result
}

#' Load ELISA data with caching and biobank linkage
#' @param dirs Character vector of directories to scan
#' @param exclude_pattern Pattern to exclude files
#' @param recursive Scan directories recursively
#' @param cv_max_ag_plus Maximum CV threshold for Ag+ control
#' @param cv_max_ag0 Maximum CV threshold for Ag0 control
#' @param biobank_df Biobank data frame for linking
#' @return Tibble with ELISA results linked to biobank
#' @export
load_elisa_data <- function(
  dirs = NULL,
  exclude_pattern = "^251021 Résultats indirect ELISA vF\\.5",
  recursive = TRUE,
  cv_max_ag_plus = 20,
  cv_max_ag0 = 20,
  biobank_df = NULL
) {
  # Use site-aware paths if not specified
  if (is.null(dirs)) {
    dirs <- if (!is.null(config$site_paths)) {
      c(config$site_paths$elisa_pe_dir, config$site_paths$elisa_vsg_dir)
    } else {
      c("data/elisa_pe", "data/elisa_vsg")
    }
  }

  ensure_elisa_parser()

  # Build file list
  file_list <- unlist(lapply(dirs, function(d) {
    if (!dir.exists(d)) {
      message("Directory not found: ", d)
      return(character(0))
    }
    list.files(d, pattern = "\\.xlsx$", recursive = recursive, full.names = TRUE)
  }))

  if (!is.null(exclude_pattern) && length(file_list) > 0) {
    file_list <- file_list[!grepl(exclude_pattern, basename(file_list))]
  }

  # Build a session-level key so in-memory cache is still invalidated if biobank changes
  session_hash <- digest(list(.elisa_cache_version, sort(file_list)))

  # 1. Check in-memory session cache (fastest — avoids all disk I/O)
  cached <- .elisa_cache_env$data
  if (!is.null(cached) &&
      identical(.elisa_cache_env$hash, session_hash) &&
      identical(.elisa_cache_env$version, .elisa_cache_version)) {
    message("✓ ELISA in-memory cache hit (", nrow(cached$data), " rows)")
    result <- cached$data
    if (!is.null(biobank_df)) result <- link_elisa_to_biobank(result, biobank_df)
    return(ensure_elisa_columns(result))
  }

  # 2. Incremental snapshot cache — only parse new/changed files
  elisa_cache_dir <- get_elisa_cache_dir()
  snap <- load_elisa_snapshot(file_list, elisa_cache_dir)

  # Parse ELISA files — reuse snapshot for unchanged files, only parse new ones
  empty_elisa <- tibble(
    plate_id = character(), plate_num = integer(),
    plate_date = as.Date(character()), elisa_type = character(),
    sample_type = character(), sample = character(),
    sample_code = character(), numero_labo = character(),
    code_barres_kps = character()
  )

  if (!length(file_list)) {
    message("No ELISA files found in specified directories")
    parsed <- empty_elisa
  } else {
    # Determine which files need (re-)parsing
    files_to_parse <- file_list[!file_list %in% names(snap$reused)]
    n_reused       <- length(snap$reused)
    n_new          <- length(files_to_parse)

    message(sprintf("[ELISA] %d file(s) reused from snapshot, %d to parse",
                    n_reused, n_new))

    # Parse only new/changed files in parallel where possible
    newly_parsed <- list()
    if (n_new > 0) {
      use_parallel <- n_new >= 4 &&
        requireNamespace("future.apply", quietly = TRUE) &&
        !inherits(future::plan(), "sequential")

      if (use_parallel) {
        cv_plus <- cv_max_ag_plus; cv_zero <- cv_max_ag0
        newly_parsed <- future.apply::future_lapply(files_to_parse, function(f)
          parse_single_elisa_file(f, cv_max_ag_plus = cv_plus, cv_max_ag0 = cv_zero),
          future.seed = TRUE)
      } else {
        newly_parsed <- lapply(files_to_parse, function(f)
          parse_single_elisa_file(f, cv_max_ag_plus = cv_max_ag_plus, cv_max_ag0 = cv_max_ag0))
      }
      names(newly_parsed) <- files_to_parse
    }

    # Merge reused + newly parsed into a named list keyed by file path
    all_parsed_files <- c(snap$reused, newly_parsed)
    all_parsed_files <- Filter(Negate(is.null), all_parsed_files)

    # Save updated snapshot (includes newly parsed data)
    save_elisa_snapshot(all_parsed_files, file_list, snap$file_hashes, elisa_cache_dir)

    parsed_list <- unname(all_parsed_files)
    if (length(parsed_list) == 0) {
      parsed <- empty_elisa
    } else {
      parsed <- bind_rows(parsed_list)
      message("✓ Loaded ", nrow(parsed), " ELISA records from ", length(parsed_list), " file(s)")
    }
  }

  parsed <- ensure_elisa_columns(parsed)

  # Ensure elisa_type column exists and has valid values (parser should have set this correctly)
  if (!"elisa_type" %in% names(parsed)) {
    warning("elisa_type column missing from parsed data, adding default")
    parsed <- parsed %>% mutate(elisa_type = "ELISA_pe")
  }

  # Check if all elisa_type values are NA (indicates a parsing problem)
  if (nrow(parsed) > 0 && all(is.na(parsed$elisa_type))) {
    warning("All elisa_type values are NA after parsing! This should not happen.")
    message("DEBUG: Columns in parsed data: ", paste(names(parsed), collapse = ", "))
  }

  # CRITICAL: Assign unique plate_number across all files
  # This creates a sequential ID for each unique plate (by plate_id, plate_num, date, type)
  if (nrow(parsed) > 0) {
    plate_index <- parsed %>%
      distinct(plate_id, plate_num, plate_date, elisa_type) %>%
      arrange(plate_date, plate_id, plate_num) %>%
      mutate(plate_number = row_number())

    parsed <- parsed %>%
      left_join(plate_index, by = c("plate_id", "plate_num", "plate_date", "elisa_type"), relationship = "many-to-one") %>%
      arrange(plate_number, sample_type)

    message("✓ Assigned plate numbers to ", n_distinct(parsed$plate_number), " unique plates")
  } else {
    # Ensure plate_number exists even for empty data
    parsed <- parsed %>% mutate(plate_number = NA_integer_)
  }

  # Link to biobank
  if (!is.null(biobank_df)) {
    message("Linking ELISA data to biobank...")
    parsed <- link_elisa_to_biobank(parsed, biobank_df)
    n_matched <- sum(parsed$BiobankMatched, na.rm = TRUE)
    message("✓ Matched ", n_matched, "/", nrow(parsed), " records to biobank (",
            round(100 * n_matched / nrow(parsed), 1), "%)")
  } else {
    parsed <- parsed %>% mutate(BiobankMatched = FALSE)
  }

  # Ensure all expected biobank columns exist (only add if missing)
  biobank_cols <- list(
    Province = NA_character_,
    HealthZone = NA_character_,
    Structure = NA_character_,
    Sex = NA_character_,
    Age = NA_real_,
    AgeGroup = NA_character_,
    SampleDate = as.Date(NA),
    Cohort = NA_character_
  )

  for (col_name in names(biobank_cols)) {
    if (!col_name %in% names(parsed)) {
      parsed[[col_name]] <- biobank_cols[[col_name]]
    }
  }

  # Add screening numbers for each sample (rank by plate_date)
  message("Calculating screening numbers...")
  parsed <- add_screening_numbers(parsed)

  # Update in-memory session cache (instant on subsequent calls this session)
  .elisa_cache_env$data    <- list(data = parsed)
  .elisa_cache_env$hash    <- session_hash
  .elisa_cache_env$version <- .elisa_cache_version

  parsed
}

#' Get ELISA data (convenience wrapper)
#' @param biobank_df Biobank data frame for linking
#' @return Tibble with ELISA results
#' @export
get_elisa_data <- function(biobank_df = NULL) {
  load_elisa_data(biobank_df = biobank_df)
}

#' Clear ELISA cache (force reload)
#' @param clear_rds If TRUE, also remove RDS cache files from disk
#' @export
clear_elisa_cache <- function(clear_rds = FALSE) {
  # Clear session cache
  .elisa_cache_env$data <- NULL
  .elisa_cache_env$hash <- NULL
  .elisa_cache_env$version <- NULL
  message("✓ ELISA session cache cleared")

  # Optionally clear RDS files
  if (clear_rds) {
    cache_dir <- get_elisa_cache_dir()
    if (dir.exists(cache_dir)) {
      cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
      if (length(cache_files) > 0) {
        file.remove(cache_files)
        message("✓ Removed ", length(cache_files), " RDS cache file(s)")
      } else {
        message("  No RDS cache files to remove")
      }
    }
  }
}
