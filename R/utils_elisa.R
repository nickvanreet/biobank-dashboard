# R/utils_elisa.R
# Utilities for loading and caching ELISA indirect assay data

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
})

if (!exists(".norm_key", mode = "function")) {
  .norm_key <- function(x, kind = c("barcode", "labid")) {
    kind <- match.arg(kind)
    x <- tolower(trimws(as.character(x)))
    x[x %in% c("", "na", "n/a", "null")] <- NA
    x <- gsub("[^a-z0-9]", "", x)
    if (kind == "barcode") {
      x <- sub("^kps", "", x)
      x <- sub("^0+", "", x)
    }
    x
  }
}

.elisa_cache_env <- new.env(parent = emptyenv())

ensure_elisa_parser <- function() {
  if (!exists("parse_indirect_elisa_folder", mode = "function")) {
    parser_path <- file.path("scripts", "parse_indirect_elisa.R")
    if (file.exists(parser_path)) {
      source(parser_path)
    } else {
      stop("ELISA parser script not found: ", parser_path)
    }
  }
}

prepare_biobank_lookup <- function(biobank_df) {
  if (is.null(biobank_df) || !nrow(biobank_df)) return(NULL)

  biobank_df %>%
    mutate(
      code_barres_kps = if ("code_barres_kps" %in% names(.)) .data$code_barres_kps else NA_character_,
      barcode = if ("barcode" %in% names(.)) .data$barcode else NA_character_,
      lab_id = if ("lab_id" %in% names(.)) .data$lab_id else NA_character_,
      numero = if ("numero" %in% names(.)) .data$numero else NA_character_
    ) %>%
    mutate(
      biobank_barcode = coalesce(.data$code_barres_kps, .data$barcode),
      biobank_lab_id = coalesce(.data$lab_id, .data$numero),
      barcode_norm = .norm_key(biobank_barcode, "barcode"),
      numero_norm = .norm_key(biobank_lab_id, "labid")
    ) %>%
    filter(!is.na(barcode_norm) | !is.na(numero_norm)) %>%
    distinct(barcode_norm, numero_norm, .keep_all = TRUE)
}

link_elisa_to_biobank <- function(elisa_df, biobank_df) {
  if (is.null(elisa_df) || !nrow(elisa_df)) return(tibble())

  elisa_prepped <- elisa_df %>%
    mutate(
      barcode_norm = .norm_key(.data$code_barres_kps, "barcode"),
      numero_norm = .norm_key(coalesce(.data$numero_labo, .data$sample_code, .data$sample), "labid")
    )

  lookup <- prepare_biobank_lookup(biobank_df)
  if (is.null(lookup)) {
    return(elisa_prepped %>% mutate(BiobankMatched = FALSE))
  }

  lookup_barcode <- lookup %>%
    filter(!is.na(barcode_norm)) %>%
    select(
      barcode_norm,
      biobank_barcode,
      biobank_lab_id,
      biobank_province = province,
      biobank_health_zone = health_zone,
      biobank_structure = health_structure,
      biobank_sex = sex,
      biobank_age = age,
      biobank_age_group = age_group,
      biobank_date_sample = date_sample
    )

  lookup_numero <- lookup %>%
    filter(!is.na(numero_norm)) %>%
    select(
      numero_norm,
      biobank_barcode,
      biobank_lab_id,
      biobank_province = province,
      biobank_health_zone = health_zone,
      biobank_structure = health_structure,
      biobank_sex = sex,
      biobank_age = age,
      biobank_age_group = age_group,
      biobank_date_sample = date_sample
    )

  joined <- elisa_prepped %>%
    left_join(lookup_barcode, by = "barcode_norm") %>%
    mutate(BiobankMatched = !is.na(.data$biobank_barcode) | !is.na(.data$biobank_lab_id))

  still_unmatched <- joined %>% filter(!BiobankMatched)
  if (nrow(still_unmatched)) {
    joined <- joined %>%
      left_join(lookup_numero, by = c("numero_norm" = "numero_norm"), suffix = c("", "_numero")) %>%
      mutate(
        biobank_barcode = coalesce(.data$biobank_barcode, .data$biobank_barcode_numero),
        biobank_lab_id = coalesce(.data$biobank_lab_id, .data$biobank_lab_id_numero),
        biobank_province = coalesce(.data$biobank_province, .data$biobank_province_numero),
        biobank_health_zone = coalesce(.data$biobank_health_zone, .data$biobank_health_zone_numero),
        biobank_structure = coalesce(.data$biobank_structure, .data$biobank_structure_numero),
        biobank_sex = coalesce(.data$biobank_sex, .data$biobank_sex_numero),
        biobank_age = coalesce(.data$biobank_age, .data$biobank_age_numero),
        biobank_age_group = coalesce(.data$biobank_age_group, .data$biobank_age_group_numero),
        biobank_date_sample = coalesce(.data$biobank_date_sample, .data$biobank_date_sample_numero),
        BiobankMatched = !is.na(.data$biobank_barcode) | !is.na(.data$biobank_lab_id)
      ) %>%
      select(-ends_with("_numero"))
  }

  joined
}

load_elisa_data <- function(
  dirs = c("data/ELISA_pe", "data/ELISA_vsg"),
  exclude_pattern = "^251021 Résultats indirect ELISA vF\\.5",
  recursive = TRUE,
  cv_max = 20,
  biobank_df = NULL
) {
  ensure_elisa_parser()

  file_list <- unlist(lapply(dirs, function(d) {
    list.files(d, pattern = "\\.xlsx$", recursive = recursive, full.names = TRUE)
  }))

  if (!is.null(exclude_pattern)) {
    file_list <- file_list[!grepl(exclude_pattern, basename(file_list))]
  }

  file_info <- tibble(
    path = file_list,
    mtime = suppressWarnings(file.info(file_list)$mtime)
  )

  hash_val <- digest(list(file_info$path, file_info$mtime, cv_max, recursive, exclude_pattern))

  cached <- .elisa_cache_env$data
  if (!is.null(cached) && identical(.elisa_cache_env$hash, hash_val)) {
    if (!is.null(biobank_df)) {
      cached$data <- link_elisa_to_biobank(cached$data, biobank_df)
    }
    return(cached$data)
  }

  if (!length(file_list)) {
    parsed <- tibble()
  } else {
    parsed <- parse_indirect_elisa_folder(
      dirs,
      exclude_pattern = exclude_pattern,
      recursive = recursive,
      cv_max = cv_max
    )
  }

  parsed <- parsed %>% mutate(elisa_type = as.character(elisa_type))

  if (!is.null(biobank_df)) {
    parsed <- link_elisa_to_biobank(parsed, biobank_df)
  }

  .elisa_cache_env$data <- list(data = parsed)
  .elisa_cache_env$hash <- hash_val

  parsed
}

get_elisa_data <- function(biobank_df = NULL) {
  load_elisa_data(biobank_df = biobank_df)
}
