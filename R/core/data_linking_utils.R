# R/core/data_linking_utils.R
# Utilities for linking extraction data with biobank data
# ============================================================================

#' Normalize barcode for matching
#' @param barcode Character vector of barcodes
#' @return Normalized barcode
#' @export
normalize_barcode <- function(barcode) {
  if (is.null(barcode)) return(character(0))

  # Convert to character and trim
  bc <- trimws(as.character(barcode))

  # Normalize to lowercase
  bc <- tolower(bc)

  # Remove common prefixes (KPS, kps, etc.)
  bc <- gsub("^kps[- _]*", "", bc)

  # Remove all non-alphanumeric characters except hyphens
  bc <- gsub("[^a-z0-9-]", "", bc)

  # Remove leading zeros (but keep if it's the only character)
  bc <- gsub("^0+(?=.)", "", bc, perl = TRUE)

  # Replace empty strings with NA
  bc[bc == "" | bc == "na" | bc == "n/a"] <- NA_character_

  bc
}

#' Link extraction data with biobank data by barcode
#' @param extraction_df Extraction dataset (from load_extraction_dataset)
#' @param biobank_df Biobank dataset (from analyze_data_quality clean_data)
#' @return Merged dataset with linkage indicators
#' @export
link_extraction_to_biobank <- function(extraction_df, biobank_df) {

  if (is.null(extraction_df) || !nrow(extraction_df)) {
    message("No extraction data to link")
    return(tibble::tibble())
  }

  if (is.null(biobank_df) || !nrow(biobank_df)) {
    message("No biobank data to link")
    # Add linkage columns but all will be NA
    extraction_df <- extraction_df %>%
      dplyr::mutate(
        biobank_matched = FALSE,
        biobank_barcode = NA_character_,
        biobank_lab_id = NA_character_,
        biobank_health_facility = NA_character_,
        biobank_study = NA_character_,
        biobank_province = NA_character_,
        biobank_health_zone = NA_character_,
        biobank_date_sample = as.Date(NA),
        health_structure_match = NA,
        barcode_normalized = normalize_barcode(sample_id)
      )
    return(extraction_df)
  }

  # Ensure biobank data has normalized barcode
  if (!".__barcode_norm" %in% names(biobank_df)) {
    # Create normalized barcode column
    biobank_df <- biobank_df %>%
      dplyr::mutate(.__barcode_norm = normalize_barcode(
        dplyr::if_else(
          "barcode" %in% names(.),
          barcode,
          dplyr::if_else(
            "code_barres_kps" %in% names(.),
            code_barres_kps,
            NA_character_
          )
        )
      ))
  }

  # Normalize extraction barcodes
  extraction_df <- extraction_df %>%
    dplyr::mutate(
      barcode_normalized = normalize_barcode(sample_id)
    )

  # Select key biobank columns for linking
  biobank_key_cols <- biobank_df %>%
    dplyr::select(
      biobank_barcode_norm = .__barcode_norm,
      biobank_barcode = dplyr::any_of(c("barcode", "code_barres_kps")),
      biobank_lab_id = dplyr::any_of(c("lab_id", ".__labid_norm", "numero")),
      biobank_health_facility = dplyr::any_of(c("health_facility", "structure_sanitaire")),
      biobank_study = dplyr::any_of(c("study", "etude")),
      biobank_province = dplyr::any_of(c("province")),
      biobank_health_zone = dplyr::any_of(c("health_zone", "zone_de_sante")),
      biobank_date_sample = dplyr::any_of(c("date_sample", ".__date_sample", "date_prelevement"))
    ) %>%
    dplyr::distinct(biobank_barcode_norm, .keep_all = TRUE)

  # Perform left join on normalized barcodes
  linked_df <- extraction_df %>%
    dplyr::left_join(
      biobank_key_cols,
      by = c("barcode_normalized" = "biobank_barcode_norm")
    ) %>%
    dplyr::mutate(
      # Flag if matched
      biobank_matched = !is.na(biobank_barcode) | !is.na(biobank_lab_id),

      # Check if health structures match (normalize for comparison)
      health_structure_match = dplyr::case_when(
        !biobank_matched ~ NA,
        is.na(health_structure) | is.na(biobank_health_facility) ~ NA,
        tolower(trimws(health_structure)) == tolower(trimws(biobank_health_facility)) ~ TRUE,
        TRUE ~ FALSE
      )
    )

  message(sprintf(
    "Linked %d/%d extractions (%.1f%%) to biobank records",
    sum(linked_df$biobank_matched, na.rm = TRUE),
    nrow(linked_df),
    100 * mean(linked_df$biobank_matched, na.rm = TRUE)
  ))

  linked_df
}

#' Summarize linkage quality metrics
#' @param linked_df Linked extraction dataset
#' @return Named list with linkage KPIs
#' @export
summarise_linkage_metrics <- function(linked_df) {
  if (is.null(linked_df) || !nrow(linked_df)) {
    return(list(
      total_extractions = 0,
      matched_to_biobank = 0,
      unmatched_to_biobank = 0,
      pct_matched = NA_real_,
      health_structure_matches = 0,
      health_structure_mismatches = 0,
      pct_structure_match = NA_real_,
      unique_barcodes = 0,
      duplicate_barcodes = 0
    ))
  }

  # Check if linkage columns exist
  has_linkage <- all(c("biobank_matched", "health_structure_match") %in% names(linked_df))

  if (!has_linkage) {
    return(list(
      total_extractions = nrow(linked_df),
      matched_to_biobank = NA_integer_,
      unmatched_to_biobank = NA_integer_,
      pct_matched = NA_real_,
      health_structure_matches = NA_integer_,
      health_structure_mismatches = NA_integer_,
      pct_structure_match = NA_real_,
      unique_barcodes = length(unique(linked_df$barcode_normalized)),
      duplicate_barcodes = sum(duplicated(linked_df$barcode_normalized))
    ))
  }

  matched <- sum(linked_df$biobank_matched, na.rm = TRUE)
  total <- nrow(linked_df)

  structure_match <- sum(linked_df$health_structure_match == TRUE, na.rm = TRUE)
  structure_mismatch <- sum(linked_df$health_structure_match == FALSE, na.rm = TRUE)
  total_with_structure <- structure_match + structure_mismatch

  list(
    total_extractions = total,
    matched_to_biobank = matched,
    unmatched_to_biobank = total - matched,
    pct_matched = if (total > 0) matched / total else NA_real_,
    health_structure_matches = structure_match,
    health_structure_mismatches = structure_mismatch,
    pct_structure_match = if (total_with_structure > 0) {
      structure_match / total_with_structure
    } else {
      NA_real_
    },
    unique_barcodes = length(unique(linked_df$barcode_normalized[!is.na(linked_df$barcode_normalized)])),
    duplicate_barcodes = sum(duplicated(linked_df$barcode_normalized[!is.na(linked_df$barcode_normalized)]))
  )
}

#' Summarize health structure volume collection over time
#' @param linked_df Linked extraction dataset
#' @return Tibble with health structure time series
#' @export
summarise_health_structure_volumes_over_time <- function(linked_df) {
  if (is.null(linked_df) || !nrow(linked_df) ||
      !"health_structure" %in% names(linked_df) ||
      !"extraction_date" %in% names(linked_df)) {
    return(tibble::tibble(
      health_structure = character(),
      month = as.Date(character()),
      n_extractions = integer(),
      total_volume = numeric(),
      median_volume = numeric(),
      pct_ready = numeric(),
      pct_matched_biobank = numeric()
    ))
  }

  # Ensure biobank_matched exists
  if (!"biobank_matched" %in% names(linked_df)) {
    linked_df$biobank_matched <- FALSE
  }

  linked_df %>%
    dplyr::filter(!is.na(health_structure) & health_structure != "Unspecified") %>%
    dplyr::filter(!is.na(extraction_date)) %>%
    dplyr::mutate(month = lubridate::floor_date(extraction_date, "month")) %>%
    dplyr::group_by(health_structure, month) %>%
    dplyr::summarise(
      n_extractions = dplyr::n(),
      total_volume = sum(drs_volume_ml, na.rm = TRUE),
      median_volume = stats::median(drs_volume_ml, na.rm = TRUE),
      mean_volume = mean(drs_volume_ml, na.rm = TRUE),
      pct_ready = mean(ready_for_freezer, na.rm = TRUE),
      pct_matched_biobank = mean(biobank_matched, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(health_structure, month)
}

#' Get unmatched extraction barcodes
#' @param linked_df Linked extraction dataset
#' @return Tibble of unmatched records
#' @export
get_unmatched_extractions <- function(linked_df) {
  if (is.null(linked_df) || !nrow(linked_df)) {
    return(tibble::tibble())
  }

  if (!"biobank_matched" %in% names(linked_df)) {
    return(tibble::tibble())
  }

  linked_df %>%
    dplyr::filter(!biobank_matched | is.na(biobank_matched)) %>%
    dplyr::select(
      sample_id,
      barcode_normalized,
      extraction_date,
      health_structure,
      drs_volume_ml,
      drs_state,
      extract_quality
    ) %>%
    dplyr::arrange(extraction_date)
}

#' Get health structure mismatches
#' @param linked_df Linked extraction dataset
#' @return Tibble of mismatched health structures
#' @export
get_health_structure_mismatches <- function(linked_df) {
  if (is.null(linked_df) || !nrow(linked_df)) {
    return(tibble::tibble())
  }

  if (!"health_structure_match" %in% names(linked_df)) {
    return(tibble::tibble())
  }

  linked_df %>%
    dplyr::filter(health_structure_match == FALSE) %>%
    dplyr::select(
      sample_id,
      barcode_normalized,
      extraction_date,
      extraction_health_structure = health_structure,
      biobank_health_facility,
      drs_volume_ml
    ) %>%
    dplyr::arrange(extraction_date)
}

#' Calculate expected vs actual volume by health structure
#' @param linked_df Linked extraction dataset
#' @param expected_monthly_volume Expected monthly DRS volume per structure (default: 50 mL)
#' @return Tibble with expected vs actual comparison
#' @export
calculate_volume_targets <- function(linked_df, expected_monthly_volume = 50) {
  if (is.null(linked_df) || !nrow(linked_df)) {
    return(tibble::tibble(
      health_structure = character(),
      month = as.Date(character()),
      actual_volume = numeric(),
      expected_volume = numeric(),
      pct_of_target = numeric(),
      meets_target = logical()
    ))
  }

  time_series <- summarise_health_structure_volumes_over_time(linked_df)

  if (!nrow(time_series)) {
    return(tibble::tibble(
      health_structure = character(),
      month = as.Date(character()),
      actual_volume = numeric(),
      expected_volume = numeric(),
      pct_of_target = numeric(),
      meets_target = logical()
    ))
  }

  time_series %>%
    dplyr::mutate(
      expected_volume = expected_monthly_volume,
      pct_of_target = total_volume / expected_volume,
      meets_target = total_volume >= expected_volume
    )
}
