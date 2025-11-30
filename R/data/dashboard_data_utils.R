# R/data/dashboard_data_utils.R
# Helper utilities for preparing tidy assay-level datasets for dashboard visuals

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(stringr)
  library(rlang)
})

#' Default cutoffs used to classify assays
#'
#' Borderline windows are intentionally wide to keep uncertain results visible in
#' the dashboard. Update these thresholds if site-specific SOPs change.
#' @return Named list of cutoffs
assay_cutoffs <- function() {
  list(
    elisa_pp_positive = 20,
    elisa_pp_borderline = c(15, 20),
    elisa_dod_positive = 0.3,
    elisa_dod_borderline = c(0.2, 0.3),
    ielisa_inhibition_positive = 30,
    ielisa_inhibition_borderline = c(25, 30),
    mic_positive_calls = c("positive", "detected"),
    mic_borderline_calls = c("review", "retest", "indeterminate"),
    shared_palette = c(
      Positive = "#2563EB",
      Borderline = "#F59E0B",
      Negative = "#9CA3AF",
      Invalid = "#EF4444",
      Missing = "#E5E7EB"
    )
  )
}

#' Normalize sample identifiers across assays
normalize_sample_id <- function(barcode = NULL, lab_id = NULL) {
  ids <- c(barcode, lab_id)
  ids <- ids[!is.na(ids) & ids != ""]
  if (!length(ids)) return(NA_character_)
  ids <- tolower(trimws(ids))
  ids <- gsub("^kps", "", ids)
  ids <- gsub("^0+", "", ids)
  ids <- gsub("[^a-z0-9]", "", ids)
  ids[1]
}

#' Coalesce the first present column from a set of candidates
#'
#' This helper safely returns `NA` when none of the columns exist, preventing
#' hard errors when upstream datasets omit optional fields.
coalesce_any_column <- function(df, columns, default = NA_character_) {
  present <- columns[columns %in% names(df)]
  if (!length(present)) {
    return(rep(default, nrow(df)))
  }

  df %>%
    transmute(value = coalesce(!!!rlang::syms(present))) %>%
    pull(.data$value)
}

#' Classify ELISA results into Positive/Borderline/Negative
classify_elisa <- function(pp_percent, dod, cutoffs) {
  pp_flag <- !is.na(pp_percent)
  dod_flag <- !is.na(dod)
  if (!pp_flag && !dod_flag) return("Missing")

  positive <- (pp_flag && pp_percent >= cutoffs$elisa_pp_positive) ||
    (dod_flag && dod >= cutoffs$elisa_dod_positive)
  borderline <- (pp_flag && dplyr::between(pp_percent, cutoffs$elisa_pp_borderline[1], cutoffs$elisa_pp_borderline[2])) ||
    (dod_flag && dplyr::between(dod, cutoffs$elisa_dod_borderline[1], cutoffs$elisa_dod_borderline[2]))

  dplyr::case_when(
    positive ~ "Positive",
    borderline ~ "Borderline",
    TRUE ~ "Negative"
  )
}

#' Classify iELISA inhibition results
classify_ielisa <- function(inhibition, cutoffs) {
  if (is.na(inhibition)) return("Missing")
  positive <- inhibition >= cutoffs$ielisa_inhibition_positive
  borderline <- dplyr::between(inhibition, cutoffs$ielisa_inhibition_borderline[1], cutoffs$ielisa_inhibition_borderline[2])
  dplyr::case_when(
    positive ~ "Positive",
    borderline ~ "Borderline",
    TRUE ~ "Negative"
  )
}

#' Classify MIC qPCR calls
classify_mic <- function(call, cutoffs) {
  if (is.null(call) || is.na(call) || call == "") return("Missing")
  call_norm <- tolower(call)
  if (call_norm %in% cutoffs$mic_positive_calls) return("Positive")
  if (call_norm %in% cutoffs$mic_borderline_calls) return("Borderline")
  if (call_norm %in% c("runinvalid", "invalid")) return("Invalid")
  "Negative"
}

#' Prepare a tidy assay-level table for dashboard consumption
#'
#' @param biobank_df Cleaned biobank data frame (for demographics/dates)
#' @param elisa_df ELISA PE/VSG data frame
#' @param ielisa_df iELISA data frame
#' @param mic_data Parsed MIC object (list with `$samples`)
#' @param filters Optional filters list from the sidebar
#' @param cutoffs Optional cutoff list (defaults to `assay_cutoffs()`)
#' @return List containing `tidy_assays`, `sample_matrix`, `pairwise_agreement`,
#'   and `intersections`
prepare_assay_dashboard_data <- function(
    biobank_df = NULL,
    elisa_df = NULL,
    ielisa_df = NULL,
    mic_data = NULL,
    filters = NULL,
    cutoffs = assay_cutoffs()) {

  tibs <- list()

  id_columns <- c("code_barres_kps", "barcode", "SampleID", "sample_id", "Sample_ID")
  lab_columns <- c("numero_labo", "numero", "lab_id", "sample_code", "SampleCode")

  # Biobank scaffold for dates/demographics
  biobank_base <- NULL
  if (!is.null(biobank_df) && nrow(biobank_df)) {
    biobank_base <- biobank_df %>%
      mutate(
        sample_id = normalize_sample_id(
          barcode = coalesce_any_column(., id_columns),
          lab_id = coalesce_any_column(., lab_columns)
        ),
        sample_date = suppressWarnings(lubridate::as_date(
          coalesce_any_column(., c("date_sample", "date_prelevement", "SampleDate"))
        ))
      ) %>%
      select(sample_id, starts_with("province"), starts_with("Province"), starts_with("Health"), starts_with("Structure"),
             starts_with("study"), starts_with("Study"), starts_with("cohort"), sample_date) %>%
      distinct()
  }

  # ELISA
  if (!is.null(elisa_df) && nrow(elisa_df)) {
    tibs$elisa <- elisa_df %>%
      mutate(
        sample_id = normalize_sample_id(
          coalesce_any_column(., id_columns),
          coalesce_any_column(., lab_columns)
        ),
        assay = dplyr::case_when(
          elisa_type %in% c("ELISA_pe", "pe", "ELISA PE") ~ "ELISA PE",
          elisa_type %in% c("ELISA_vsg", "vsg", "ELISA VSG") ~ "ELISA VSG",
          TRUE ~ coalesce(elisa_type, "ELISA")
        ),
        status = map2_chr(PP_percent, DOD, classify_elisa, cutoffs = cutoffs),
        quantitative = coalesce(PP_percent, DOD),
        metric = ifelse(!is.na(PP_percent), "PP%", "DOD"),
        assay_date = suppressWarnings(lubridate::as_date(coalesce(plate_date, SampleDate)))
      ) %>%
      select(sample_id, assay, status, quantitative, metric, assay_date, DOD, PP_percent)
  }

  # iELISA
  if (!is.null(ielisa_df) && nrow(ielisa_df)) {
    date_candidates <- c("PlateDate", "plate_date", "run_date")

    # Allow flexible column names because exports differ by site/version
    inhibition_columns <- function(pattern) {
      grep(pattern, names(ielisa_df), value = TRUE, ignore.case = TRUE)
    }

    positive_columns <- grep("positive|result|interpret", names(ielisa_df), value = TRUE, ignore.case = TRUE)

    antigen_configs <- list(
      list(
        name = "iELISA LiTat 1.3",
        value_cols = c(
          "pct_inh_f2_13", "pct_inh_f1_13", "Inhibition_L13", "Inhibition_percent",
          "Inhibition", "inhibition_percent", inhibition_columns("13"), inhibition_columns("LiTat1\.3")
        ) %>% unlist(),
        positive_cols = c("positive_L13", "Positif_L13", positive_columns[grepl("13", positive_columns, ignore.case = TRUE)])
      ),
      list(
        name = "iELISA LiTat 1.5",
        value_cols = c(
          "pct_inh_f2_15", "pct_inh_f1_15", "Inhibition_L15", "Inhibition_percent",
          "Inhibition", "inhibition_percent", inhibition_columns("15"), inhibition_columns("LiTat1\.5")
        ) %>% unlist(),
        positive_cols = c("positive_L15", "Positif_L15", positive_columns[grepl("15", positive_columns, ignore.case = TRUE)])
      )
    )

    parse_boolean_flag <- function(x) {
      x <- tolower(as.character(x))
      case_when(
        x %in% c("true", "1", "yes", "y", "positive", "pos", "positif", "p", "detected") ~ "Positive",
        x %in% c("false", "0", "no", "n", "negative", "neg", "negatif", "absent") ~ "Negative",
        stringr::str_detect(x, "\\bpos") ~ "Positive",
        stringr::str_detect(x, "\\bneg") ~ "Negative",
        TRUE ~ NA_character_
      )
    }

    parse_numeric_value <- function(x) {
      raw <- str_replace_all(as.character(x), ",", ".")
      extracted <- str_extract(raw, "-?\\d*\\.?\\d+")
      suppressWarnings(as.numeric(extracted))
    }

    build_ielisa_tibble <- function(cfg) {
      value_cols <- unique(cfg$value_cols[cfg$value_cols %in% names(ielisa_df)])
      positive_cols <- unique(cfg$positive_cols[cfg$positive_cols %in% names(ielisa_df)])

      if (!length(value_cols) && !length(positive_cols)) {
        return(NULL)
      }

      date_cols <- date_candidates[date_candidates %in% names(ielisa_df)]

      tib <- ielisa_df %>%
        mutate(
          sample_id = normalize_sample_id(
            coalesce_any_column(., id_columns),
            coalesce_any_column(., lab_columns)
          ),
          assay = cfg$name,
          quantitative = if (length(value_cols)) {
            vals <- coalesce_any_column(., value_cols)
            parse_numeric_value(vals)
          } else {
            NA_real_
          },
          status = vapply(quantitative, classify_ielisa, character(1), cutoffs = cutoffs),
          metric = "% Inhibition",
          assay_date = suppressWarnings(lubridate::as_date(
            if (length(date_cols)) coalesce(!!!syms(date_cols)) else NA
          ))
        )

      if (length(positive_cols)) {
        tib <- tib %>%
          mutate(
            string_call = coalesce_any_column(., positive_cols),
            call_status = parse_boolean_flag(string_call),
            status = case_when(
              !is.na(quantitative) ~ status,
              !is.na(call_status) ~ call_status,
              TRUE ~ status
            )
          ) %>%
          select(-string_call, -call_status)
      } else if (length(positive_columns)) {
        tib <- tib %>%
          mutate(
            string_call = coalesce_any_column(., positive_columns),
            call_status = parse_boolean_flag(string_call),
            status = case_when(
              !is.na(quantitative) ~ status,
              !is.na(call_status) ~ call_status,
              TRUE ~ status
            )
          ) %>%
          select(-string_call, -call_status)
      }

      tib %>% select(sample_id, assay, status, quantitative, metric, assay_date)
    }

    tibs$ielisa <- antigen_configs %>%
      map(build_ielisa_tibble) %>%
      compact() %>%
      bind_rows()
  }

  # MIC qPCR
  if (!is.null(mic_data) && !is.null(mic_data$samples) && nrow(mic_data$samples)) {
    tibs$mic <- mic_data$samples %>%
      mutate(
        sample_id = normalize_sample_id(
          coalesce_any_column(., id_columns),
          coalesce_any_column(., lab_columns)
        ),
        assay = "MIC qPCR",
        status = vapply(FinalCall, classify_mic, character(1), cutoffs = cutoffs),
        quantitative = coalesce(Cq_median_177T, Cq_median_18S2),
        metric = "Cq",
        assay_date = suppressWarnings(lubridate::as_date(
          coalesce_any_column(., c("CollectionDate", "SampleDate", "RunDate", "plate_date"))
        ))
      ) %>%
      select(sample_id, assay, status, quantitative, metric, assay_date, FinalCall, Cq_median_177T, Cq_median_18S2)
  }

  status_levels <- c("Positive", "Borderline", "Negative", "Invalid", "Missing")

  tidy <- bind_rows(tibs) %>%
    filter(!is.na(sample_id)) %>%
    mutate(
      status = factor(status, levels = status_levels),
      status_rank = match(status, status_levels),
      quantitative_missing = is.na(quantitative)
    ) %>%
    group_by(sample_id, assay) %>%
    arrange(status_rank, quantitative_missing) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(-status_rank, -quantitative_missing) %>%
    mutate(assay = factor(assay))

  # Apply global filters if provided
  if (!is.null(filters)) {
    if (!is.null(filters$date_range) && length(filters$date_range) == 2) {
      dr <- filters$date_range
      tidy <- tidy %>% mutate(assay_date = as.Date(assay_date)) %>%
        filter(is.na(assay_date) | (assay_date >= dr[1] & assay_date <= dr[2]))
    }
    if (!is.null(filters$province) && filters$province != "all" && !is.null(biobank_base)) {
      tidy <- tidy %>% left_join(biobank_base, by = "sample_id") %>%
        filter(is.na(province) | province == filters$province | Province == filters$province)
    }
  }

  # Sample matrix (wide for heatmap)
  sample_matrix <- tidy %>%
    select(sample_id, assay, status) %>%
    distinct() %>%
    group_by(sample_id, assay) %>%
    summarise(status = dplyr::first(status), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = assay, values_from = status)

  # Pairwise agreement (% of samples with same status, ignoring Missing)
  assays <- sort(unique(as.character(tidy$assay)))
  if (length(assays) >= 2) {
    pairwise <- expand.grid(assay1 = assays, assay2 = assays, stringsAsFactors = FALSE) %>%
      rowwise() %>%
      mutate(
        agreement = {
          if (assay1 == assay2) {
            100
          } else {
            a1 <- tidy %>% filter(assay == assay1, !is.na(status)) %>% select(sample_id, status)
            a2 <- tidy %>% filter(assay == assay2, !is.na(status)) %>% select(sample_id, status)
            joined <- inner_join(a1, a2, by = "sample_id", suffix = c("1", "2")) %>%
              filter(status1 != "Missing" & status2 != "Missing")
            if (!nrow(joined)) {
              NA_real_
            } else {
              mean(joined$status1 == joined$status2) * 100
            }
          }
        },
        n = {
          a1 <- tidy %>% filter(assay == assay1) %>% select(sample_id)
          a2 <- tidy %>% filter(assay == assay2) %>% select(sample_id)
          nrow(inner_join(a1, a2, by = "sample_id"))
        }
      ) %>%
      ungroup()
  } else {
    pairwise <- tibble()
  }

  # Intersection of positive samples (UpSet-like)
  positive_sets <- tidy %>%
    filter(status == "Positive") %>%
    group_by(sample_id) %>%
    summarise(assays = list(sort(unique(as.character(assay)))), .groups = "drop")

  intersections <- positive_sets %>%
    mutate(combo = vapply(assays, function(x) paste(x, collapse = " + "), character(1))) %>%
    count(combo, name = "n") %>%
    arrange(desc(n))

  list(
    tidy_assays = tidy,
    sample_matrix = sample_matrix,
    pairwise_agreement = pairwise,
    intersections = intersections,
    cutoffs = cutoffs
  )
}
