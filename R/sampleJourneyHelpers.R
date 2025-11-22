# ==============================================================================
# SAMPLE JOURNEY MODULE - HELPER FUNCTIONS
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(glue)
})

#' Gather all testing data for a specific sample
#' @param sample_id Sample ID to search for
#' @param biobank_df Biobank data frame
#' @param extraction_df Extraction data frame
#' @param mic_df MIC/qPCR data frame
#' @param elisa_pe_df ELISA PE data frame
#' @param elisa_vsg_df ELISA VSG data frame
#' @param ielisa_df iELISA data frame
#' @return List containing all sample data and journey information
#' @export
gather_sample_journey <- function(sample_id, biobank_df = NULL, extraction_df = NULL,
                                   mic_df = NULL, elisa_pe_df = NULL, elisa_vsg_df = NULL,
                                   ielisa_df = NULL) {

  if (is.null(sample_id) || sample_id == "") {
    return(list(
      sample_id = sample_id,
      found = FALSE,
      biobank_info = NULL,
      extraction_data = tibble(),
      mic_data = tibble(),
      elisa_pe_data = tibble(),
      elisa_vsg_data = tibble(),
      ielisa_data = tibble(),
      timeline = tibble(),
      alerts = list()
    ))
  }

  search_id <- normalize_barcode(sample_id)

  results <- list(
    sample_id = sample_id,
    found = FALSE,
    biobank_info = NULL,
    extraction_data = tibble(),
    mic_data = tibble(),
    elisa_pe_data = tibble(),
    elisa_vsg_data = tibble(),
    ielisa_data = tibble(),
    timeline = tibble(),
    alerts = list()
  )

  # Search biobank master data
  if (!is.null(biobank_df) && nrow(biobank_df) > 0) {
    biobank_match <- biobank_df %>%
      mutate(
        barcode_norm = normalize_barcode(
          dplyr::coalesce(
            if ("code_barres_kps" %in% names(.)) .data$code_barres_kps else NA_character_,
            if ("barcode" %in% names(.)) .data$barcode else NA_character_
          )
        ),
        numero_norm = normalize_barcode(
          as.character(dplyr::coalesce(
            if ("numero_labo" %in% names(.)) .data$numero_labo else NA_character_,
            if ("numero" %in% names(.)) as.character(.data$numero) else NA_character_,
            if ("lab_id" %in% names(.)) .data$lab_id else NA_character_
          ))
        )
      ) %>%
      filter(barcode_norm == search_id | numero_norm == search_id) %>%
      dplyr::slice(1)

    if (nrow(biobank_match) > 0) {
      results$found <- TRUE
      results$biobank_info <- biobank_match
    }
  }

  # Search extraction/QC data
  if (!is.null(extraction_df) && nrow(extraction_df) > 0) {
    results$extraction_data <- extraction_df %>%
      mutate(sample_norm = normalize_barcode(as.character(sample_id))) %>%
      filter(sample_norm == search_id)
    if (nrow(results$extraction_data) > 0) results$found <- TRUE
  }

  # Search MIC/qPCR data
  if (!is.null(mic_df) && nrow(mic_df) > 0) {
    results$mic_data <- mic_df %>%
      mutate(
        sample_norm = normalize_barcode(as.character(coalesce(SampleID, SampleName, Barcode)))
      ) %>%
      filter(sample_norm == search_id)
    if (nrow(results$mic_data) > 0) results$found <- TRUE
  }

  # Search ELISA PE data
  if (!is.null(elisa_pe_df) && nrow(elisa_pe_df) > 0) {
    results$elisa_pe_data <- elisa_pe_df %>%
      mutate(
        barcode_norm = normalize_barcode(
          if ("code_barres_kps" %in% names(.)) as.character(.data$code_barres_kps) else NA_character_
        ),
        numero_norm = normalize_barcode(
          if ("numero_labo" %in% names(.)) as.character(.data$numero_labo) else NA_character_
        )
      ) %>%
      filter(barcode_norm == search_id | numero_norm == search_id)
    if (nrow(results$elisa_pe_data) > 0) results$found <- TRUE
  }

  # Search ELISA VSG data
  if (!is.null(elisa_vsg_df) && nrow(elisa_vsg_df) > 0) {
    results$elisa_vsg_data <- elisa_vsg_df %>%
      mutate(
        barcode_norm = normalize_barcode(
          if ("code_barres_kps" %in% names(.)) as.character(.data$code_barres_kps) else NA_character_
        ),
        numero_norm = normalize_barcode(
          if ("numero_labo" %in% names(.)) as.character(.data$numero_labo) else NA_character_
        )
      ) %>%
      filter(barcode_norm == search_id | numero_norm == search_id)
    if (nrow(results$elisa_vsg_data) > 0) results$found <- TRUE
  }

  # Search iELISA data
  if (!is.null(ielisa_df) && nrow(ielisa_df) > 0) {
    results$ielisa_data <- ielisa_df %>%
      mutate(
        barcode_norm = normalize_barcode(
          if ("code_barres_kps" %in% names(.)) as.character(.data$code_barres_kps) else NA_character_
        ),
        numero_norm = normalize_barcode(
          if ("numero_labo" %in% names(.)) as.character(.data$numero_labo) else NA_character_
        )
      ) %>%
      filter(barcode_norm == search_id | numero_norm == search_id)
    if (nrow(results$ielisa_data) > 0) results$found <- TRUE
  }

  # Create timeline
  results$timeline <- create_sample_timeline(results)

  # Generate alerts
  results$alerts <- generate_sample_alerts(results)

  return(results)
}

#' Create timeline tibble from all test dates
#' @param journey_data List returned from gather_sample_journey
#' @return Tibble with timeline events
#' @export
create_sample_timeline <- function(journey_data) {
  events <- list()

  # Biobank collection
  if (!is.null(journey_data$biobank_info) && nrow(journey_data$biobank_info) > 0) {
    date_col <- journey_data$biobank_info %>%
      select(any_of(c("date_sample", "date_prelevement", ".__date_sample"))) %>%
      dplyr::slice(1) %>%
      unlist() %>%
      as.Date()

    if (!is.na(date_col[1])) {
      events[[length(events) + 1]] <- tibble(
        event = "Sample Collection",
        date = date_col[1],
        category = "Biobank",
        details = "Recorded in biobank"
      )
    }
  }

  # Extractions
  if (nrow(journey_data$extraction_data) > 0) {
    for (i in 1:nrow(journey_data$extraction_data)) {
      row <- journey_data$extraction_data[i, ]
      ext_date <- if ("extraction_date" %in% names(row)) {
        as.Date(row$extraction_date)
      } else {
        NA
      }

      if (!is.na(ext_date)) {
        volume_info <- if ("drs_volume_ml" %in% names(row) && !is.na(row$drs_volume_ml)) {
          sprintf("Volume: %.1f µL", row$drs_volume_ml)
        } else {
          ""
        }

        events[[length(events) + 1]] <- tibble(
          event = sprintf("Extraction #%d", i),
          date = ext_date,
          category = "Extraction",
          details = volume_info
        )
      }
    }
  }

  # MIC tests
  if (nrow(journey_data$mic_data) > 0) {
    for (i in 1:nrow(journey_data$mic_data)) {
      row <- journey_data$mic_data[i, ]
      run_date <- if ("RunDate" %in% names(row)) {
        as.Date(row$RunDate)
      } else if ("RunDateTime" %in% names(row)) {
        as.Date(row$RunDateTime)
      } else {
        NA
      }

      if (!is.na(run_date)) {
        result <- if ("FinalCall" %in% names(row)) row$FinalCall else "Unknown"
        events[[length(events) + 1]] <- tibble(
          event = sprintf("MIC Test #%d", i),
          date = run_date,
          category = "MIC",
          details = sprintf("Result: %s", result)
        )
      }
    }
  }

  # ELISA PE tests
  if (nrow(journey_data$elisa_pe_data) > 0) {
    for (i in 1:nrow(journey_data$elisa_pe_data)) {
      row <- journey_data$elisa_pe_data[i, ]
      plate_date <- if ("plate_date" %in% names(row)) as.Date(row$plate_date) else NA

      if (!is.na(plate_date)) {
        result <- if ("sample_positive" %in% names(row)) {
          ifelse(row$sample_positive, "Positive", "Negative")
        } else {
          "Unknown"
        }
        events[[length(events) + 1]] <- tibble(
          event = sprintf("ELISA PE #%d", i),
          date = plate_date,
          category = "ELISA",
          details = sprintf("Result: %s", result)
        )
      }
    }
  }

  # ELISA VSG tests
  if (nrow(journey_data$elisa_vsg_data) > 0) {
    for (i in 1:nrow(journey_data$elisa_vsg_data)) {
      row <- journey_data$elisa_vsg_data[i, ]
      plate_date <- if ("plate_date" %in% names(row)) as.Date(row$plate_date) else NA

      if (!is.na(plate_date)) {
        result <- if ("sample_positive" %in% names(row)) {
          ifelse(row$sample_positive, "Positive", "Negative")
        } else {
          "Unknown"
        }
        events[[length(events) + 1]] <- tibble(
          event = sprintf("ELISA VSG #%d", i),
          date = plate_date,
          category = "ELISA",
          details = sprintf("Result: %s", result)
        )
      }
    }
  }

  # iELISA tests
  if (nrow(journey_data$ielisa_data) > 0) {
    for (i in 1:nrow(journey_data$ielisa_data)) {
      row <- journey_data$ielisa_data[i, ]
      plate_date <- if ("plate_date" %in% names(row)) as.Date(row$plate_date) else NA

      if (!is.na(plate_date)) {
        events[[length(events) + 1]] <- tibble(
          event = sprintf("iELISA #%d", i),
          date = plate_date,
          category = "iELISA",
          details = "Inhibition ELISA"
        )
      }
    }
  }

  # Combine and sort
  if (length(events) > 0) {
    timeline <- bind_rows(events) %>%
      arrange(date) %>%
      mutate(
        day_number = as.numeric(date - min(date)),
        event_id = row_number()
      )
    return(timeline)
  } else {
    return(tibble(
      event = character(),
      date = as.Date(character()),
      category = character(),
      details = character(),
      day_number = numeric(),
      event_id = integer()
    ))
  }
}

#' Generate alerts for QC issues
#' @param journey_data List returned from gather_sample_journey
#' @return List of alert objects
#' @export
generate_sample_alerts <- function(journey_data) {
  alerts <- list()

  # Check DRS volume (low volume alert if < 30µL)
  if (nrow(journey_data$extraction_data) > 0) {
    for (i in 1:nrow(journey_data$extraction_data)) {
      row <- journey_data$extraction_data[i, ]
      if ("drs_volume_ml" %in% names(row) && !is.na(row$drs_volume_ml)) {
        if (row$drs_volume_ml < 30) {
          alerts[[length(alerts) + 1]] <- list(
            type = "warning",
            category = "Extraction QC",
            message = sprintf("Low DRS volume: %.1f µL (expected ≥30µL)", row$drs_volume_ml),
            test_number = i
          )
        }
      }
    }
  }

  # Check RNAseP Ct values (high Ct alert if > 35)
  if (nrow(journey_data$mic_data) > 0) {
    for (i in 1:nrow(journey_data$mic_data)) {
      row <- journey_data$mic_data[i, ]

      # Check DNA RNAseP
      if ("Cq_median_RNAseP_DNA" %in% names(row) && !is.na(row$Cq_median_RNAseP_DNA)) {
        if (row$Cq_median_RNAseP_DNA > 35) {
          alerts[[length(alerts) + 1]] <- list(
            type = "warning",
            category = "MIC QC",
            message = sprintf("High RNAseP DNA Ct: %.1f (expected <35)", row$Cq_median_RNAseP_DNA),
            test_number = i
          )
        }
      }

      # Check RNA RNAseP
      if ("Cq_median_RNAseP_RNA" %in% names(row) && !is.na(row$Cq_median_RNAseP_RNA)) {
        if (row$Cq_median_RNAseP_RNA > 35) {
          alerts[[length(alerts) + 1]] <- list(
            type = "warning",
            category = "MIC QC",
            message = sprintf("High RNAseP RNA Ct: %.1f (expected <35)", row$Cq_median_RNAseP_RNA),
            test_number = i
          )
        }
      }
    }
  }

  # Check for discordant MIC results (if multiple tests)
  if (nrow(journey_data$mic_data) > 1) {
    final_calls <- journey_data$mic_data %>%
      filter(!is.na(FinalCall)) %>%
      pull(FinalCall) %>%
      unique()

    if (length(final_calls) > 1) {
      alerts[[length(alerts) + 1]] <- list(
        type = "danger",
        category = "MIC Results",
        message = sprintf("Discordant MIC results: %s", paste(final_calls, collapse = ", ")),
        test_number = NA
      )
    }
  }

  # Check for borderline ELISA results
  if (nrow(journey_data$elisa_pe_data) > 0) {
    for (i in 1:nrow(journey_data$elisa_pe_data)) {
      row <- journey_data$elisa_pe_data[i, ]
      if ("PP_percent" %in% names(row) && !is.na(row$PP_percent)) {
        if (row$PP_percent >= 40 && row$PP_percent <= 60) {
          alerts[[length(alerts) + 1]] <- list(
            type = "info",
            category = "ELISA PE",
            message = sprintf("Borderline result: PP = %.1f%% (40-60%%)", row$PP_percent),
            test_number = i
          )
        }
      }
    }
  }

  if (nrow(journey_data$elisa_vsg_data) > 0) {
    for (i in 1:nrow(journey_data$elisa_vsg_data)) {
      row <- journey_data$elisa_vsg_data[i, ]
      if ("PP_percent" %in% names(row) && !is.na(row$PP_percent)) {
        if (row$PP_percent >= 40 && row$PP_percent <= 60) {
          alerts[[length(alerts) + 1]] <- list(
            type = "info",
            category = "ELISA VSG",
            message = sprintf("Borderline result: PP = %.1f%% (40-60%%)", row$PP_percent),
            test_number = i
          )
        }
      }
    }
  }

  return(alerts)
}

#' Get list of all sample IDs for autocomplete
#' @param biobank_df Biobank data frame
#' @param extraction_df Extraction data frame
#' @param mic_df MIC data frame
#' @param elisa_pe_df ELISA PE data frame
#' @param elisa_vsg_df ELISA VSG data frame
#' @param ielisa_df iELISA data frame
#' @return Character vector of unique sample IDs
#' @export
get_sample_autocomplete <- function(biobank_df = NULL, extraction_df = NULL,
                                     mic_df = NULL, elisa_pe_df = NULL,
                                     elisa_vsg_df = NULL, ielisa_df = NULL) {

  all_ids <- c()

  # From biobank
  if (!is.null(biobank_df) && nrow(biobank_df) > 0) {
    biobank_ids <- c(
      if ("code_barres_kps" %in% names(biobank_df)) biobank_df$code_barres_kps else NULL,
      if ("barcode" %in% names(biobank_df)) biobank_df$barcode else NULL,
      if ("numero_labo" %in% names(biobank_df)) biobank_df$numero_labo else NULL,
      if ("numero" %in% names(biobank_df)) biobank_df$numero else NULL,
      if ("lab_id" %in% names(biobank_df)) biobank_df$lab_id else NULL
    )
    all_ids <- c(all_ids, biobank_ids)
  }

  # From extractions
  if (!is.null(extraction_df) && nrow(extraction_df) > 0) {
    extraction_ids <- c(
      if ("sample_id" %in% names(extraction_df)) extraction_df$sample_id else NULL,
      if ("barcode" %in% names(extraction_df)) extraction_df$barcode else NULL,
      if ("numero" %in% names(extraction_df)) extraction_df$numero else NULL,
      if ("record_number" %in% names(extraction_df)) extraction_df$record_number else NULL
    )
    all_ids <- c(all_ids, extraction_ids)
  }

  # From MIC
  if (!is.null(mic_df) && nrow(mic_df) > 0) {
    mic_ids <- c(
      if ("SampleID" %in% names(mic_df)) mic_df$SampleID else NULL,
      if ("SampleName" %in% names(mic_df)) mic_df$SampleName else NULL,
      if ("Barcode" %in% names(mic_df)) mic_df$Barcode else NULL
    )
    all_ids <- c(all_ids, mic_ids)
  }

  # From ELISA PE
  if (!is.null(elisa_pe_df) && nrow(elisa_pe_df) > 0) {
    elisa_pe_ids <- c(
      if ("code_barres_kps" %in% names(elisa_pe_df)) elisa_pe_df$code_barres_kps else NULL,
      if ("numero_labo" %in% names(elisa_pe_df)) elisa_pe_df$numero_labo else NULL
    )
    all_ids <- c(all_ids, elisa_pe_ids)
  }

  # From ELISA VSG
  if (!is.null(elisa_vsg_df) && nrow(elisa_vsg_df) > 0) {
    elisa_vsg_ids <- c(
      if ("code_barres_kps" %in% names(elisa_vsg_df)) elisa_vsg_df$code_barres_kps else NULL,
      if ("numero_labo" %in% names(elisa_vsg_df)) elisa_vsg_df$numero_labo else NULL
    )
    all_ids <- c(all_ids, elisa_vsg_ids)
  }

  # From iELISA
  if (!is.null(ielisa_df) && nrow(ielisa_df) > 0) {
    ielisa_ids <- c(
      if ("code_barres_kps" %in% names(ielisa_df)) ielisa_df$code_barres_kps else NULL,
      if ("numero_labo" %in% names(ielisa_df)) ielisa_df$numero_labo else NULL
    )
    all_ids <- c(all_ids, ielisa_ids)
  }

  # Clean, deduplicate and sort
  all_ids <- all_ids %>%
    as.character() %>%
    na.omit() %>%
    trimws() %>%
    unique() %>%
    sort()

  return(all_ids)
}
