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
#'
#' Less aggressive normalization to prevent sample ID collisions.
#' Preserves dashes, underscores, and dots to maintain uniqueness.
normalize_sample_id <- function(barcode = NULL, lab_id = NULL) {
  ids <- c(barcode, lab_id)
  ids <- ids[!is.na(ids) & ids != ""]
  if (!length(ids)) return(NA_character_)

  # Convert to string and trim whitespace
  ids <- trimws(as.character(ids))

  # Remove KPS prefix (case-insensitive)
  ids <- gsub("^[Kk][Pp][Ss][-_]?", "", ids)

  # Convert to lowercase for case-insensitive matching
  ids <- tolower(ids)

  # Only remove spaces and special punctuation, but keep dashes, underscores, dots
  # This prevents "001-A" and "001-B" from collapsing to the same ID
  ids <- gsub("[^a-z0-9._-]", "", ids)

  # Remove any resulting empty strings
  ids <- ids[ids != ""]
  if (!length(ids)) return(NA_character_)

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

  id_columns <- c("code_barres_kps", "barcode", "SampleID", "sample_id", "Sample_ID", "Name")
  lab_columns <- c("numero_labo", "numero", "lab_id", "sample_code", "SampleCode")

  # Biobank scaffold for dates/demographics
  biobank_base <- NULL
  if (!is.null(biobank_df) && nrow(biobank_df)) {
    biobank_base <- biobank_df %>%
      mutate(
        sample_id = normalize_sample_id(
          barcode = coalesce_any_column(., id_columns)
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
          barcode = coalesce_any_column(., id_columns)
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

    antigen_configs <- list(
      list(
        name = "iELISA LiTat 1.3",
        value_cols = c("pct_inh_f2_13", "pct_inh_f1_13", "Inhibition_L13", "Inhibition_percent", "Inhibition", "inhibition_percent"),
        positive_col = "positive_L13"
      ),
      list(
        name = "iELISA LiTat 1.5",
        value_cols = c("pct_inh_f2_15", "pct_inh_f1_15", "Inhibition_L15", "Inhibition_percent", "Inhibition", "inhibition_percent"),
        positive_col = "positive_L15"
      )
    )

    build_ielisa_tibble <- function(cfg) {
      value_col <- cfg$value_cols[cfg$value_cols %in% names(ielisa_df)][1]
      positive_col <- cfg$positive_col

      if (is.null(value_col) && !(positive_col %in% names(ielisa_df))) {
        return(NULL)
      }

      date_cols <- date_candidates[date_candidates %in% names(ielisa_df)]

      tib <- ielisa_df %>%
        mutate(
          sample_id = normalize_sample_id(
            barcode = coalesce_any_column(., c("code_barres_kps", "barcode"))
          ),
          assay = cfg$name,
          quantitative = if (!is.null(value_col)) suppressWarnings(as.numeric(.data[[value_col]])) else NA_real_,
          status = vapply(quantitative, classify_ielisa, character(1), cutoffs = cutoffs),
          metric = "% Inhibition",
          assay_date = suppressWarnings(lubridate::as_date(
            if (length(date_cols)) coalesce(!!!syms(date_cols)) else NA
          ))
        )

      if (!is.null(positive_col) && positive_col %in% names(ielisa_df)) {
        tib <- tib %>%
          mutate(
            status = case_when(
              !is.na(quantitative) ~ status,
              .data[[positive_col]] == TRUE ~ "Positive",
              .data[[positive_col]] == FALSE ~ "Negative",
              TRUE ~ status
            )
          )
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
          barcode = coalesce_any_column(., id_columns)
        ),
        assay = "MIC qPCR",
        status = vapply(PipelineCategory, classify_mic, character(1), cutoffs = cutoffs),
        quantitative = coalesce(Avg_177T_Positive_Cq, Avg_18S2_Positive_Cq),
        metric = "Cq",
        assay_date = suppressWarnings(lubridate::as_date(
          coalesce_any_column(., c("CollectionDate", "SampleDate", "RunDate", "plate_date"))
        ))
      ) %>%
      select(sample_id, assay, status, quantitative, metric, assay_date, PipelineCategory, Avg_177T_Positive_Cq, Avg_18S2_Positive_Cq)
  }

  status_levels <- c("Positive", "Borderline", "Negative", "Invalid", "Missing")

  tidy <- bind_rows(tibs) %>%
    filter(!is.na(sample_id) & sample_id != "") %>%
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

  # Enhanced concordance: Molecular (MIC) vs Serology (ELISA + iELISA)
  molecular_serology_concordance <- tidy %>%
    mutate(
      test_category = case_when(
        assay == "MIC qPCR" ~ "Molecular",
        grepl("ELISA", assay) ~ "Serology",
        grepl("iELISA", assay) ~ "Serology",
        TRUE ~ "Other"
      ),
      is_positive = status == "Positive"
    ) %>%
    group_by(sample_id) %>%
    summarise(
      mic_positive = any(test_category == "Molecular" & is_positive),
      serology_positive = any(test_category == "Serology" & is_positive),
      mic_tested = any(test_category == "Molecular"),
      serology_tested = any(test_category == "Serology"),
      n_tests = n(),
      n_positive = sum(is_positive),
      positive_assays = list(as.character(assay[is_positive])),
      .groups = "drop"
    ) %>%
    mutate(
      concordance_category = case_when(
        !mic_tested & !serology_tested ~ "Not Tested",
        !mic_tested & serology_tested ~ "Serology Only",
        mic_tested & !serology_tested ~ "Molecular Only",
        mic_positive & serology_positive ~ "Both Positive",
        !mic_positive & !serology_positive ~ "Both Negative",
        mic_positive & !serology_positive ~ "MIC+ / Serology-",
        !mic_positive & serology_positive ~ "MIC- / Serology+",
        TRUE ~ "Unknown"
      ),
      single_test_positive = n_positive == 1,
      all_tests_positive = n_positive == n_tests & n_positive > 0
    )

  # Summary statistics for molecular vs serology concordance
  mic_serology_summary <- molecular_serology_concordance %>%
    filter(mic_tested & serology_tested) %>%
    summarise(
      n_samples = n(),
      n_both_positive = sum(concordance_category == "Both Positive"),
      n_both_negative = sum(concordance_category == "Both Negative"),
      n_mic_only = sum(concordance_category == "MIC+ / Serology-"),
      n_serology_only = sum(concordance_category == "MIC- / Serology+"),
      n_concordant = n_both_positive + n_both_negative,
      n_discordant = n_mic_only + n_serology_only,
      pct_concordant = if_else(n_samples > 0, (n_concordant / n_samples) * 100, NA_real_),
      pct_discordant = if_else(n_samples > 0, (n_discordant / n_samples) * 100, NA_real_),
      pct_both_positive = if_else(n_samples > 0, (n_both_positive / n_samples) * 100, NA_real_),
      pct_mic_only = if_else(n_samples > 0, (n_mic_only / n_samples) * 100, NA_real_),
      pct_serology_only = if_else(n_samples > 0, (n_serology_only / n_samples) * 100, NA_real_)
    )

  # Single test positive analysis
  single_test_summary <- molecular_serology_concordance %>%
    summarise(
      n_single_test_positive = sum(single_test_positive),
      n_multiple_test_positive = sum(n_positive > 1),
      n_all_tests_positive = sum(all_tests_positive)
    )

  # Detailed test prevalence and overlap analysis
  test_prevalence <- tidy %>%
    group_by(assay) %>%
    summarise(
      total_tests = n(),
      n_positive = sum(status == "Positive", na.rm = TRUE),
      n_negative = sum(status == "Negative", na.rm = TRUE),
      n_borderline = sum(status == "Borderline", na.rm = TRUE),
      n_invalid = sum(status == "Invalid", na.rm = TRUE),
      pct_positive = if_else(total_tests > 0, (n_positive / total_tests) * 100, NA_real_),
      .groups = "drop"
    )

  # Calculate exclusive vs shared positives for each test
  # A positive is "exclusive" if it's the only positive test for that sample
  # A positive is "shared" if the sample is also positive on other tests
  test_overlap_details <- tidy %>%
    filter(status == "Positive") %>%
    group_by(sample_id) %>%
    mutate(
      n_positive_tests = n(),
      is_exclusive = n_positive_tests == 1
    ) %>%
    ungroup() %>%
    group_by(assay) %>%
    summarise(
      n_exclusive = sum(is_exclusive),
      n_shared = sum(!is_exclusive),
      .groups = "drop"
    )

  # Merge prevalence with overlap details
  test_prevalence <- test_prevalence %>%
    left_join(test_overlap_details, by = "assay") %>%
    mutate(
      n_exclusive = if_else(is.na(n_exclusive), 0L, as.integer(n_exclusive)),
      n_shared = if_else(is.na(n_shared), 0L, as.integer(n_shared))
    )

  # Calculate specific overlap combinations
  # 1. Positive on all tests
  all_tests_data <- tidy %>%
    select(sample_id, assay, status) %>%
    group_by(sample_id) %>%
    summarise(
      n_tests = n_distinct(assay),
      n_positive = sum(status == "Positive"),
      all_tests_positive = n_tests > 0 & n_positive == n_tests,
      .groups = "drop"
    )

  # 2. Positive on all serology tests (ELISA PE, ELISA VSG, iELISA L13, iELISA L15)
  serology_tests <- c("ELISA PE", "ELISA VSG", "iELISA LiTat 1.3", "iELISA LiTat 1.5")
  all_serology_positive_data <- tidy %>%
    filter(assay %in% serology_tests) %>%
    group_by(sample_id) %>%
    summarise(
      n_serology_tests = n_distinct(assay),
      n_serology_positive = sum(status == "Positive"),
      all_serology_positive = n_serology_tests > 0 & n_serology_positive == n_serology_tests,
      any_serology_positive = n_serology_positive > 0,
      .groups = "drop"
    )

  # 3. MIC positive + any serology positive
  mic_plus_serology <- tidy %>%
    mutate(
      is_mic = assay == "MIC qPCR",
      is_serology = assay %in% serology_tests,
      is_positive = status == "Positive"
    ) %>%
    group_by(sample_id) %>%
    summarise(
      mic_positive = any(is_mic & is_positive),
      serology_positive = any(is_serology & is_positive),
      mic_and_serology_positive = mic_positive & serology_positive,
      .groups = "drop"
    )

  # 4. Detailed pairwise overlaps between specific tests
  # For each pair of tests, count samples positive on both
  if (length(assays) >= 2) {
    pairwise_overlaps <- expand.grid(
      assay1 = assays,
      assay2 = assays,
      stringsAsFactors = FALSE
    ) %>%
      filter(assay1 != assay2) %>%
      rowwise() %>%
      mutate(
        n_both_positive = {
          a1_pos <- tidy %>% filter(assay == assay1, status == "Positive") %>% pull(sample_id)
          a2_pos <- tidy %>% filter(assay == assay2, status == "Positive") %>% pull(sample_id)
          length(intersect(a1_pos, a2_pos))
        }
      ) %>%
      ungroup()
  } else {
    pairwise_overlaps <- tibble()
  }

  # Summary statistics
  overlap_summary <- tibble(
    total_samples = n_distinct(tidy$sample_id),
    n_all_tests_positive = sum(all_tests_data$all_tests_positive, na.rm = TRUE),
    n_all_serology_positive = sum(all_serology_positive_data$all_serology_positive, na.rm = TRUE),
    n_mic_and_any_serology = sum(mic_plus_serology$mic_and_serology_positive, na.rm = TRUE),
    n_any_positive = n_distinct(tidy$sample_id[tidy$status == "Positive"])
  )

  # Detailed breakdown for each test showing what they overlap with
  test_overlap_breakdown <- tidy %>%
    filter(status == "Positive") %>%
    select(sample_id, assay) %>%
    group_by(sample_id) %>%
    summarise(
      positive_assays = list(as.character(assay)),
      n_positive_assays = n(),
      .groups = "drop"
    )

  # For each test, calculate overlaps with other specific tests
  if (length(assays) > 0) {
    test_specific_overlaps <- map_dfr(assays, function(test) {
      # Get samples positive for this test
      test_positive_samples <- tidy %>%
        filter(assay == test, status == "Positive") %>%
        pull(sample_id)

      if (length(test_positive_samples) == 0) {
        return(tibble(
          assay = test,
          n_positive = 0,
          n_exclusive = 0,
          n_with_mic = 0,
          n_with_elisa_pe = 0,
          n_with_elisa_vsg = 0,
          n_with_ielisa_l13 = 0,
          n_with_ielisa_l15 = 0,
          n_with_any_serology = 0,
          n_with_all_serology = 0
        ))
      }

      # For each test, find overlaps
      overlaps <- test_overlap_breakdown %>%
        filter(sample_id %in% test_positive_samples) %>%
        summarise(
          n_exclusive = sum(n_positive_assays == 1),
          n_with_mic = sum(map_lgl(positive_assays, ~"MIC qPCR" %in% .x)),
          n_with_elisa_pe = sum(map_lgl(positive_assays, ~"ELISA PE" %in% .x)),
          n_with_elisa_vsg = sum(map_lgl(positive_assays, ~"ELISA VSG" %in% .x)),
          n_with_ielisa_l13 = sum(map_lgl(positive_assays, ~"iELISA LiTat 1.3" %in% .x)),
          n_with_ielisa_l15 = sum(map_lgl(positive_assays, ~"iELISA LiTat 1.5" %in% .x))
        )

      # Calculate "with any serology" and "with all serology"
      overlaps <- overlaps %>%
        mutate(
          n_with_any_serology = test_overlap_breakdown %>%
            filter(sample_id %in% test_positive_samples) %>%
            summarise(n = sum(map_lgl(positive_assays, ~any(serology_tests %in% .x)))) %>%
            pull(n),
          n_with_all_serology = test_overlap_breakdown %>%
            filter(sample_id %in% test_positive_samples) %>%
            summarise(n = sum(map_lgl(positive_assays, ~all(serology_tests %in% .x)))) %>%
            pull(n)
        )

      tibble(
        assay = test,
        n_positive = length(test_positive_samples),
        n_exclusive = overlaps$n_exclusive,
        n_with_mic = overlaps$n_with_mic,
        n_with_elisa_pe = overlaps$n_with_elisa_pe,
        n_with_elisa_vsg = overlaps$n_with_elisa_vsg,
        n_with_ielisa_l13 = overlaps$n_with_ielisa_l13,
        n_with_ielisa_l15 = overlaps$n_with_ielisa_l15,
        n_with_any_serology = overlaps$n_with_any_serology,
        n_with_all_serology = overlaps$n_with_all_serology
      )
    })
  } else {
    test_specific_overlaps <- tibble()
  }

  list(
    tidy_assays = tidy,
    sample_matrix = sample_matrix,
    pairwise_agreement = pairwise,
    intersections = intersections,
    molecular_serology_concordance = molecular_serology_concordance,
    mic_serology_summary = mic_serology_summary,
    single_test_summary = single_test_summary,
    test_prevalence = test_prevalence,
    test_specific_overlaps = test_specific_overlaps,
    pairwise_overlaps = pairwise_overlaps,
    overlap_summary = overlap_summary,
    cutoffs = cutoffs
  )
}
