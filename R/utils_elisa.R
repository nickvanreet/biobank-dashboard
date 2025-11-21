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
#' @param x Character vector
#' @param kind Type of key ("barcode" or "labid")
#' @return Normalized key
.norm_key <- function(x, kind = c("barcode", "labid")) {
  kind <- match.arg(kind)
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na", "n/a", "null")] <- NA_character_
  x <- gsub("[^a-z0-9]", "", x)
  if (kind == "barcode") {
    x <- sub("^kps", "", x)
    x <- sub("^0+", "", x)
  }
  x
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
    plate_number = NA_integer_,
    plate_date = as.Date(NA),
    elisa_type = NA_character_,  # CRITICAL: Required for filtering by PE/VSG
    sample_type = NA_character_,
    sample = NA_character_,
    sample_code = NA_character_,
    numero_labo = NA_character_,
    code_barres_kps = NA_character_
  )

  for (col_name in names(required_cols)) {
    if (!col_name %in% names(df)) {
      df[[col_name]] <- required_cols[[col_name]]
    }
  }

  df
}

# ============================================================================
# DATA LOADING WITH CACHING
# ============================================================================

# Cache environment
.elisa_cache_env <- new.env(parent = emptyenv())

# Cache version - increment this when data structure changes to invalidate old caches
.elisa_cache_version <- "v9_force_parser_reload_and_fix_elisa_type"

#' Ensure ELISA parser is loaded
ensure_elisa_parser <- function() {
  parser_path <- file.path("scripts", "parse_indirect_elisa.R")

  if (!file.exists(parser_path)) {
    stop("ELISA parser script not found: ", parser_path)
  }

  parser_digest <- digest(file = parser_path)
  cached_digest <- .elisa_cache_env$parser_digest

  # Always (re)load when the function is missing or when the parser file changed
  if (!exists("parse_indirect_elisa_folder", mode = "function") ||
      is.null(cached_digest) || !identical(cached_digest, parser_digest)) {
    source(parser_path, local = .GlobalEnv)
    .elisa_cache_env$parser_digest <- parser_digest

    # Invalidate ELISA cache when the parser changes to avoid stale structures
    .elisa_cache_env$data <- NULL
    .elisa_cache_env$hash <- NULL
    .elisa_cache_env$version <- NULL

    message("✓ Loaded ELISA parser from ", parser_path, " (digest: ", parser_digest, ")")
  }
}

#' Load ELISA data with caching and biobank linkage
#' @param dirs Character vector of directories to scan
#' @param exclude_pattern Pattern to exclude files
#' @param recursive Scan directories recursively
#' @param cv_max Maximum CV threshold
#' @param biobank_df Biobank data frame for linking
#' @return Tibble with ELISA results linked to biobank
#' @export
load_elisa_data <- function(
  dirs = c("data/elisa_pe", "data/elisa_vsg"),
  exclude_pattern = "^251021 Résultats indirect ELISA vF\\.5",
  recursive = TRUE,
  cv_max = 20,
  biobank_df = NULL
) {
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

  # Calculate hash for cache (include version to invalidate on structure changes)
  file_info <- tibble(
    path = file_list,
    mtime = suppressWarnings(file.info(file_list)$mtime)
  )

  hash_val <- digest(list(.elisa_cache_version, file_info$path, file_info$mtime, cv_max, recursive, exclude_pattern))

  # Check cache
  cached <- .elisa_cache_env$data
  cached_hash <- .elisa_cache_env$hash
  cached_version <- .elisa_cache_env$version

  # Invalidate cache if version changed or hash doesn't match
  cache_valid <- !is.null(cached) && !is.null(cached_hash) && identical(cached_hash, hash_val) &&
                 !is.null(cached_version) && identical(cached_version, .elisa_cache_version)

  if (cache_valid) {
    message("✓ Using cached ELISA data (", nrow(cached$data), " rows)")
    message("DEBUG: Cache version: ", cached_version)
    message("DEBUG: Unique elisa_type values in cache: ", paste(unique(cached$data$elisa_type), collapse = ", "))

    # Re-link to biobank in case biobank_df changed
    if (!is.null(biobank_df)) {
      cached$data <- link_elisa_to_biobank(cached$data, biobank_df)
    }
    return(ensure_elisa_columns(cached$data))
  } else {
    if (!is.null(cached_version)) {
      message("DEBUG: Cache invalidated. Old version: ", cached_version, ", New version: ", .elisa_cache_version)
    } else {
      message("DEBUG: No cache found, will parse fresh data")
    }
  }

  # Parse ELISA files
  if (!length(file_list)) {
    message("No ELISA files found in specified directories")
    parsed <- tibble(
      plate_id = character(),
      plate_num = integer(),
      plate_date = as.Date(character()),
      elisa_type = character(),
      sample_type = character(),
      sample = character(),
      sample_code = character(),
      numero_labo = character(),
      code_barres_kps = character()
    )
  } else {
    message("Parsing ", length(file_list), " ELISA file(s)...")
    parsed <- parse_indirect_elisa_folder(
      dirs,
      exclude_pattern = exclude_pattern,
      recursive = recursive,
      cv_max = cv_max
    )
    message("✓ Parsed ", nrow(parsed), " ELISA records")
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

  # Cache results with version
  .elisa_cache_env$data <- list(data = parsed)
  .elisa_cache_env$hash <- hash_val
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
#' @export
clear_elisa_cache <- function() {
  .elisa_cache_env$data <- NULL
  .elisa_cache_env$hash <- NULL
  message("✓ ELISA cache cleared")
}
