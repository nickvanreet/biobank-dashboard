# R/utils_standardized_counting.R
# ============================================================================
# STANDARDIZED TEST COUNTING UTILITIES
# ============================================================================
#
# This module provides consistent classification and counting logic for all
# test types (MIC, ELISA PE, ELISA VSG, iELISA) across all dashboard modules.
#
# IMPORTANT: All modules should use these functions to ensure consistent
# counts across Geographic, Sample Overview, Study Comparison, and MIC Analysis.
#
# Key principles:
# 1. DEDUPLICATION: One result per sample per test type (keep best/latest)
# 2. CLASSIFICATION: Standardized status categories (Positive, Borderline, Negative, Invalid)
# 3. COUNTING: Consistent definitions of what counts as positive
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(rlang)
})

# ============================================================================
# CONSTANTS AND CUTOFFS
# ============================================================================

#' Standard cutoffs for all test types
#' @return Named list of cutoffs
standardized_cutoffs <- function() {
  list(
    # ELISA PE/VSG cutoffs
    elisa_pp_positive = 20,
    elisa_pp_borderline = c(15, 20),
    elisa_dod_positive = 0.3,
    elisa_dod_borderline = c(0.2, 0.3),

    # iELISA cutoffs (default, can be overridden by user settings)
    ielisa_inhibition_positive = 30,
    ielisa_inhibition_borderline = c(25, 30),

    # MIC FinalCall values that count as positive
    mic_positive_calls = c("Positive", "Positive_DNA", "Positive_RNA"),
    mic_latepositive_calls = c("LatePositive"),
    mic_borderline_calls = c("Indeterminate", "Inconclusive", "Review", "Retest"),
    mic_invalid_calls = c("Invalid", "Invalid_NoDNA", "Failed", "RunInvalid"),

    # MIC marker-level values (177T, 18S2)
    mic_marker_positive = c("positive", "detected"),
    mic_marker_latepositive = c("latepositive", "late positive"),
    mic_marker_borderline = c("indeterminate", "suspect", "review"),
    mic_marker_negative = c("negative", "undetermined", "nd", "not detected", "notdetected"),
    mic_marker_invalid = c("invalid", "failed", "error"),

    # Status priority for deduplication (lower = better)
    status_priority = c(
      "Positive" = 1,
      "LatePositive" = 2,
      "Borderline" = 3,
      "Negative" = 4,
      "Invalid" = 5,
      "Missing" = 6
    )
  )
}

# ============================================================================
# MIC CLASSIFICATION FUNCTIONS
# ============================================================================

#' Classify MIC FinalCall to standardized status
#'
#' Handles all MIC FinalCall values consistently across modules.
#'
#' @param final_call Character vector of FinalCall values
#' @param include_latepositive_as_positive If TRUE, LatePositive counts as Positive
#' @return Character vector of standardized status values
#' @export
classify_mic_finalcall <- function(final_call, include_latepositive_as_positive = FALSE) {
  cutoffs <- standardized_cutoffs()

  result <- dplyr::case_when(
    is.na(final_call) | final_call == "" ~ "Missing",
    final_call %in% cutoffs$mic_positive_calls ~ "Positive",
    final_call %in% cutoffs$mic_latepositive_calls ~ if (include_latepositive_as_positive) "Positive" else "LatePositive",
    final_call %in% cutoffs$mic_borderline_calls ~ "Borderline",
    final_call %in% cutoffs$mic_invalid_calls ~ "Invalid",
    final_call == "Negative" ~ "Negative",
    TRUE ~ "Missing"
  )

  result
}

#' Classify MIC marker status (177T or 18S2) to standardized status
#'
#' Used for per-target classification (DNA via 177T, RNA via 18S2).
#'
#' @param marker_status Character vector of marker status values
#' @return Character vector of standardized status values
#' @export
classify_mic_marker <- function(marker_status) {
  cutoffs <- standardized_cutoffs()

  marker_norm <- tolower(as.character(marker_status))

  result <- dplyr::case_when(
    is.na(marker_status) | marker_status == "" ~ "Missing",
    marker_norm %in% cutoffs$mic_marker_positive ~ "Positive",
    marker_norm %in% cutoffs$mic_marker_latepositive ~ "LatePositive",
    marker_norm %in% cutoffs$mic_marker_borderline ~ "Borderline",
    marker_norm %in% cutoffs$mic_marker_negative ~ "Negative",
    marker_norm %in% cutoffs$mic_marker_invalid ~ "Invalid",
    TRUE ~ "Missing"
  )

  result
}

#' Check if MIC result is positive
#'
#' Standardized function to determine positivity across all modules.
#'
#' @param final_call Character vector of FinalCall values
#' @param include_latepositive If TRUE, LatePositive counts as positive
#' @param include_borderline If TRUE, Borderline counts as positive
#' @return Logical vector
#' @export
is_mic_positive <- function(final_call, include_latepositive = TRUE, include_borderline = FALSE) {
  cutoffs <- standardized_cutoffs()

  positive_values <- cutoffs$mic_positive_calls
  if (include_latepositive) {
    positive_values <- c(positive_values, cutoffs$mic_latepositive_calls)
  }
  if (include_borderline) {
    positive_values <- c(positive_values, cutoffs$mic_borderline_calls)
  }

  final_call %in% positive_values
}

#' Check if MIC DNA target (177T) is positive
#'
#' @param marker_177T Character vector of 177T marker status
#' @param include_latepositive If TRUE, LatePositive counts as positive
#' @return Logical vector
#' @export
is_mic_dna_positive <- function(marker_177T, include_latepositive = TRUE) {
  status <- classify_mic_marker(marker_177T)
  if (include_latepositive) {
    status %in% c("Positive", "LatePositive")
  } else {
    status == "Positive"
  }
}

#' Check if MIC RNA target (18S2) is positive
#'
#' @param marker_18S2 Character vector of 18S2 marker status
#' @param include_latepositive If TRUE, LatePositive counts as positive
#' @return Logical vector
#' @export
is_mic_rna_positive <- function(marker_18S2, include_latepositive = TRUE) {
  status <- classify_mic_marker(marker_18S2)
  if (include_latepositive) {
    status %in% c("Positive", "LatePositive")
  } else {
    status == "Positive"
  }
}

# ============================================================================
# ELISA CLASSIFICATION FUNCTIONS
# ============================================================================

#' Classify ELISA result to standardized status
#'
#' Uses PP% and DOD thresholds consistently.
#'
#' @param pp_percent Numeric vector of PP% values
#' @param dod Numeric vector of DOD values
#' @param status_final Optional character vector of pre-computed status (preferred if available)
#' @return Character vector of standardized status values
#' @export
classify_elisa <- function(pp_percent = NULL, dod = NULL, status_final = NULL) {
  cutoffs <- standardized_cutoffs()


  # If status_final is provided, standardize and return it
  if (!is.null(status_final)) {
    status_norm <- toupper(as.character(status_final))
    return(dplyr::case_when(
      is.na(status_final) | status_final == "" ~ "Missing",
      grepl("^POS", status_norm) ~ "Positive",
      grepl("^NEG", status_norm) ~ "Negative",
      grepl("^BORDER|^IND", status_norm) ~ "Borderline",
      grepl("^INV|^FAIL", status_norm) ~ "Invalid",
      TRUE ~ "Missing"
    ))
  }

  # Calculate from PP% and DOD
  pp_flag <- !is.na(pp_percent)
  dod_flag <- !is.na(dod)

  dplyr::case_when(
    !pp_flag & !dod_flag ~ "Missing",
    # Positive: either metric above positive threshold
    (pp_flag & pp_percent >= cutoffs$elisa_pp_positive) |
      (dod_flag & dod >= cutoffs$elisa_dod_positive) ~ "Positive",
    # Borderline: either metric in borderline range
    (pp_flag & dplyr::between(pp_percent, cutoffs$elisa_pp_borderline[1], cutoffs$elisa_pp_borderline[2])) |
      (dod_flag & dplyr::between(dod, cutoffs$elisa_dod_borderline[1], cutoffs$elisa_dod_borderline[2])) ~ "Borderline",
    # Otherwise negative
    TRUE ~ "Negative"
  )
}

#' Check if ELISA result is positive
#'
#' @param status Character vector of status values (or pp_percent if calculating)
#' @param pp_percent Optional numeric vector of PP% values
#' @param dod Optional numeric vector of DOD values
#' @param include_borderline If TRUE, Borderline counts as positive
#' @return Logical vector
#' @export
is_elisa_positive <- function(status = NULL, pp_percent = NULL, dod = NULL, include_borderline = FALSE) {
  if (is.null(status)) {
    status <- classify_elisa(pp_percent = pp_percent, dod = dod)
  } else {
    status <- classify_elisa(status_final = status)
  }

  if (include_borderline) {
    status %in% c("Positive", "Borderline")
  } else {
    status == "Positive"
  }
}

# ============================================================================
# iELISA CLASSIFICATION FUNCTIONS
# ============================================================================

#' Classify iELISA inhibition to standardized status
#'
#' @param inhibition Numeric vector of % inhibition values
#' @param positive_threshold Numeric positive threshold (default 30)
#' @param borderline_threshold Numeric borderline threshold (default 25)
#' @return Character vector of standardized status values
#' @export
classify_ielisa <- function(inhibition, positive_threshold = 30, borderline_threshold = 25) {
  dplyr::case_when(
    is.na(inhibition) ~ "Missing",
    inhibition >= positive_threshold ~ "Positive",
    inhibition >= borderline_threshold ~ "Borderline",
    TRUE ~ "Negative"
  )
}

#' Check if iELISA L13 is positive
#'
#' @param inhibition_L13 Numeric vector of L13 % inhibition values
#' @param positive_L13 Logical vector of L13 positivity (optional, preferred if available)
#' @param positive_threshold Numeric positive threshold (default 30)
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param borderline_threshold Numeric borderline threshold (default 25)
#' @return Logical vector
#' @export
is_ielisa_L13_positive <- function(inhibition_L13 = NULL, positive_L13 = NULL,
                                    positive_threshold = 30, include_borderline = FALSE,
                                    borderline_threshold = 25) {
  # Prefer boolean positive_L13 if available
  if (!is.null(positive_L13)) {
    result <- positive_L13 == TRUE
    if (include_borderline && !is.null(inhibition_L13)) {
      # Add borderline cases
      result <- result | (!is.na(inhibition_L13) &
                           inhibition_L13 >= borderline_threshold &
                           inhibition_L13 < positive_threshold)
    }
    return(result)
  }

  # Calculate from inhibition
  if (!is.null(inhibition_L13)) {
    status <- classify_ielisa(inhibition_L13, positive_threshold, borderline_threshold)
    if (include_borderline) {
      return(status %in% c("Positive", "Borderline"))
    }
    return(status == "Positive")
  }

  rep(FALSE, max(1, length(inhibition_L13), length(positive_L13)))
}

#' Check if iELISA L15 is positive
#'
#' @param inhibition_L15 Numeric vector of L15 % inhibition values
#' @param positive_L15 Logical vector of L15 positivity (optional, preferred if available)
#' @param positive_threshold Numeric positive threshold (default 30)
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param borderline_threshold Numeric borderline threshold (default 25)
#' @return Logical vector
#' @export
is_ielisa_L15_positive <- function(inhibition_L15 = NULL, positive_L15 = NULL,
                                    positive_threshold = 30, include_borderline = FALSE,
                                    borderline_threshold = 25) {
  # Prefer boolean positive_L15 if available
  if (!is.null(positive_L15)) {
    result <- positive_L15 == TRUE
    if (include_borderline && !is.null(inhibition_L15)) {
      # Add borderline cases
      result <- result | (!is.na(inhibition_L15) &
                           inhibition_L15 >= borderline_threshold &
                           inhibition_L15 < positive_threshold)
    }
    return(result)
  }

  # Calculate from inhibition
  if (!is.null(inhibition_L15)) {
    status <- classify_ielisa(inhibition_L15, positive_threshold, borderline_threshold)
    if (include_borderline) {
      return(status %in% c("Positive", "Borderline"))
    }
    return(status == "Positive")
  }

  rep(FALSE, max(1, length(inhibition_L15), length(positive_L15)))
}

#' Check if iELISA is positive (either L13 or L15)
#'
#' @param positive_L13 Logical vector of L13 positivity
#' @param positive_L15 Logical vector of L15 positivity
#' @param inhibition_L13 Optional numeric vector of L13 % inhibition
#' @param inhibition_L15 Optional numeric vector of L15 % inhibition
#' @param positive_threshold Numeric positive threshold (default 30)
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param borderline_threshold Numeric borderline threshold (default 25)
#' @return Logical vector
#' @export
is_ielisa_any_positive <- function(positive_L13 = NULL, positive_L15 = NULL,
                                    inhibition_L13 = NULL, inhibition_L15 = NULL,
                                    positive_threshold = 30, include_borderline = FALSE,
                                    borderline_threshold = 25) {
  l13_pos <- is_ielisa_L13_positive(inhibition_L13, positive_L13, positive_threshold,
                                     include_borderline, borderline_threshold)
  l15_pos <- is_ielisa_L15_positive(inhibition_L15, positive_L15, positive_threshold,
                                     include_borderline, borderline_threshold)
  l13_pos | l15_pos
}

# ============================================================================
# DEDUPLICATION FUNCTIONS
# ============================================================================

#' Deduplicate test results to one result per sample
#'
#' Keeps the best result according to status priority:
#' Positive > LatePositive > Borderline > Negative > Invalid > Missing
#'
#' For ties, keeps the most recent result (by date if available).
#'
#' @param data Data frame with test results
#' @param sample_id_col Column name for sample identifier
#' @param status_col Column name for status
#' @param date_col Optional column name for date (for tie-breaking)
#' @return Data frame with one row per sample
#' @export
deduplicate_test_results <- function(data, sample_id_col = "sample_id",
                                      status_col = "status", date_col = NULL) {
  if (nrow(data) == 0) return(data)

  cutoffs <- standardized_cutoffs()

  # Add status priority
  data <- data %>%
    mutate(
      .status_priority = dplyr::case_when(
        !!sym(status_col) == "Positive" ~ 1L,
        !!sym(status_col) == "LatePositive" ~ 2L,
        !!sym(status_col) == "Borderline" ~ 3L,
        !!sym(status_col) == "Negative" ~ 4L,
        !!sym(status_col) == "Invalid" ~ 5L,
        TRUE ~ 6L
      )
    )

  # Group and select best
  if (!is.null(date_col) && date_col %in% names(data)) {
    data <- data %>%
      group_by(!!sym(sample_id_col)) %>%
      arrange(.status_priority, desc(!!sym(date_col))) %>%
      slice_head(n = 1) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(!!sym(sample_id_col)) %>%
      arrange(.status_priority) %>%
      slice_head(n = 1) %>%
      ungroup()
  }

  data %>% select(-.status_priority)
}

#' Deduplicate MIC results per sample (considering retests)
#'
#' @param data MIC data frame
#' @param sample_id_col Column name for sample identifier (default "SampleID")
#' @param use_latest If TRUE, prefer latest test. If FALSE, prefer best status.
#' @return Data frame with one row per sample
#' @export
deduplicate_mic_samples <- function(data, sample_id_col = "SampleID", use_latest = FALSE) {
  if (nrow(data) == 0) return(data)

  # Classify FinalCall to status
  if ("FinalCall" %in% names(data)) {
    data <- data %>%
      mutate(.status = classify_mic_finalcall(FinalCall))
  } else {
    data <- data %>%
      mutate(.status = "Missing")
  }

  # Add priority
  data <- data %>%
    mutate(
      .status_priority = dplyr::case_when(
        .status == "Positive" ~ 1L,
        .status == "LatePositive" ~ 2L,
        .status == "Borderline" ~ 3L,
        .status == "Negative" ~ 4L,
        .status == "Invalid" ~ 5L,
        TRUE ~ 6L
      )
    )

  # Deduplicate
  if (use_latest && "RunDateTime" %in% names(data)) {
    data <- data %>%
      group_by(!!sym(sample_id_col)) %>%
      arrange(desc(RunDateTime)) %>%
      slice_head(n = 1) %>%
      ungroup()
  } else {
    data <- data %>%
      group_by(!!sym(sample_id_col)) %>%
      arrange(.status_priority) %>%
      slice_head(n = 1) %>%
      ungroup()
  }

  data %>% select(-.status, -.status_priority)
}

# ============================================================================
# AGGREGATION FUNCTIONS FOR GEOGRAPHIC MODULE
# ============================================================================

#' Aggregate MIC test results by group (e.g., health zone)
#'
#' Provides standardized counting that matches Sample Overview logic.
#'
#' @param data MIC data frame
#' @param group_col Column to group by
#' @param sample_id_col Sample ID column for deduplication
#' @param include_latepositive If TRUE, LatePositive counts as positive
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param deduplicate If TRUE, deduplicate per sample before counting
#' @return Aggregated data frame with counts
#' @export
aggregate_mic_by_group <- function(data, group_col, sample_id_col = "SampleID",
                                    include_latepositive = TRUE,
                                    include_borderline = FALSE,
                                    deduplicate = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble(
      !!group_col := character(),
      mic_dna_pos = integer(),
      mic_rna_pos = integer(),
      mic_tna_pos = integer(),
      mic_any_pos = integer(),
      mic_negative = integer(),
      mic_invalid = integer(),
      mic_total = integer()
    ))
  }

  # Deduplicate if requested
  if (deduplicate) {
    data <- deduplicate_mic_samples(data, sample_id_col)
  }

  # Classify markers
  has_marker_177t <- "marker_177T" %in% names(data) || "Call_177T" %in% names(data)
  has_marker_18s2 <- "marker_18S2" %in% names(data) || "Call_18S2" %in% names(data)

  # Calculate positivity flags using standardized functions
  data <- data %>%
    mutate(
      .is_dna_pos = if (has_marker_177t) {
        is_mic_dna_positive(coalesce(marker_177T, Call_177T), include_latepositive)
      } else if ("FinalCall" %in% names(.)) {
        FinalCall %in% c("Positive", "Positive_DNA") |
          (include_latepositive & FinalCall == "LatePositive")
      } else {
        FALSE
      },
      .is_rna_pos = if (has_marker_18s2) {
        is_mic_rna_positive(coalesce(marker_18S2, Call_18S2), include_latepositive)
      } else if ("FinalCall" %in% names(.)) {
        FinalCall %in% c("Positive", "Positive_RNA") |
          (include_latepositive & FinalCall == "LatePositive")
      } else {
        FALSE
      },
      .is_tna_pos = .is_dna_pos & .is_rna_pos,
      .is_any_pos = if ("FinalCall" %in% names(.)) {
        is_mic_positive(FinalCall, include_latepositive, include_borderline)
      } else {
        .is_dna_pos | .is_rna_pos
      },
      .is_negative = if ("FinalCall" %in% names(.)) {
        FinalCall == "Negative"
      } else {
        !.is_any_pos
      },
      .is_invalid = if ("FinalCall" %in% names(.)) {
        FinalCall %in% standardized_cutoffs()$mic_invalid_calls
      } else {
        FALSE
      }
    )

  # Aggregate by group
  data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      mic_dna_pos = sum(.is_dna_pos, na.rm = TRUE),
      mic_rna_pos = sum(.is_rna_pos, na.rm = TRUE),
      mic_tna_pos = sum(.is_tna_pos, na.rm = TRUE),
      mic_any_pos = sum(.is_any_pos, na.rm = TRUE),
      mic_negative = sum(.is_negative, na.rm = TRUE),
      mic_invalid = sum(.is_invalid, na.rm = TRUE),
      mic_total = n(),
      .groups = "drop"
    )
}

#' Aggregate ELISA test results by group
#'
#' @param data ELISA data frame
#' @param group_col Column to group by
#' @param sample_id_col Sample ID column for deduplication
#' @param elisa_type "pe" or "vsg"
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param deduplicate If TRUE, deduplicate per sample before counting
#' @return Aggregated data frame with counts
#' @export
aggregate_elisa_by_group <- function(data, group_col, sample_id_col = "sample_id",
                                      elisa_type = "pe", include_borderline = FALSE,
                                      deduplicate = TRUE) {
  prefix <- paste0("elisa_", elisa_type, "_")

  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble(
      !!group_col := character(),
      !!paste0(prefix, "pos") := integer(),
      !!paste0(prefix, "neg") := integer(),
      !!paste0(prefix, "border") := integer(),
      !!paste0(prefix, "invalid") := integer(),
      !!paste0(prefix, "total") := integer()
    ))
  }

  # Classify status
  status_col <- if (paste0(prefix, "status_final") %in% names(data)) {
    paste0(prefix, "status_final")
  } else if ("status_final" %in% names(data)) {
    "status_final"
  } else if ("status" %in% names(data)) {
    "status"
  } else {
    NULL
  }

  if (!is.null(status_col)) {
    data <- data %>%
      mutate(.status = classify_elisa(status_final = !!sym(status_col)))
  } else if ("PP_percent" %in% names(data) || "DOD" %in% names(data)) {
    data <- data %>%
      mutate(.status = classify_elisa(
        pp_percent = if ("PP_percent" %in% names(.)) PP_percent else NULL,
        dod = if ("DOD" %in% names(.)) DOD else NULL
      ))
  } else {
    data <- data %>% mutate(.status = "Missing")
  }

  # Deduplicate if requested
  if (deduplicate && sample_id_col %in% names(data)) {
    data <- deduplicate_test_results(data, sample_id_col, ".status")
  }

  # Calculate positivity
  data <- data %>%
    mutate(
      .is_pos = if (include_borderline) {
        .status %in% c("Positive", "Borderline")
      } else {
        .status == "Positive"
      }
    )

  # Aggregate
  result <- data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      pos = sum(.is_pos, na.rm = TRUE),
      neg = sum(.status == "Negative", na.rm = TRUE),
      border = sum(.status == "Borderline", na.rm = TRUE),
      invalid = sum(.status == "Invalid", na.rm = TRUE),
      total = n(),
      .groups = "drop"
    )

  # Rename columns with prefix
  names(result) <- c(group_col, paste0(prefix, c("pos", "neg", "border", "invalid", "total")))
  result
}

#' Aggregate iELISA test results by group
#'
#' @param data iELISA data frame
#' @param group_col Column to group by
#' @param sample_id_col Sample ID column for deduplication
#' @param positive_threshold Numeric positive threshold
#' @param borderline_threshold Numeric borderline threshold
#' @param include_borderline If TRUE, Borderline counts as positive
#' @param deduplicate If TRUE, deduplicate per sample before counting
#' @return Aggregated data frame with counts
#' @export
aggregate_ielisa_by_group <- function(data, group_col, sample_id_col = "sample_id",
                                       positive_threshold = 30, borderline_threshold = 25,
                                       include_borderline = FALSE, deduplicate = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(tibble::tibble(
      !!group_col := character(),
      ielisa_l13_pos = integer(),
      ielisa_l15_pos = integer(),
      ielisa_any_pos = integer(),
      ielisa_both_pos = integer(),
      ielisa_total = integer()
    ))
  }

  # Calculate L13 and L15 positivity
  data <- data %>%
    mutate(
      .is_l13_pos = is_ielisa_L13_positive(
        inhibition_L13 = if ("pct_inh_f1_13" %in% names(.)) pct_inh_f1_13 else
          if ("pct_inh_f2_13" %in% names(.)) pct_inh_f2_13 else NULL,
        positive_L13 = if ("positive_L13" %in% names(.)) positive_L13 else NULL,
        positive_threshold = positive_threshold,
        include_borderline = include_borderline,
        borderline_threshold = borderline_threshold
      ),
      .is_l15_pos = is_ielisa_L15_positive(
        inhibition_L15 = if ("pct_inh_f1_15" %in% names(.)) pct_inh_f1_15 else
          if ("pct_inh_f2_15" %in% names(.)) pct_inh_f2_15 else NULL,
        positive_L15 = if ("positive_L15" %in% names(.)) positive_L15 else NULL,
        positive_threshold = positive_threshold,
        include_borderline = include_borderline,
        borderline_threshold = borderline_threshold
      ),
      .is_any_pos = .is_l13_pos | .is_l15_pos,
      .is_both_pos = .is_l13_pos & .is_l15_pos
    )

  # Deduplicate if requested
  if (deduplicate && sample_id_col %in% names(data)) {
    data <- data %>%
      mutate(.status = dplyr::case_when(
        .is_any_pos ~ "Positive",
        TRUE ~ "Negative"
      ))
    data <- deduplicate_test_results(data, sample_id_col, ".status")
  }

  # Aggregate
  data %>%
    group_by(!!sym(group_col)) %>%
    summarise(
      ielisa_l13_pos = sum(.is_l13_pos, na.rm = TRUE),
      ielisa_l15_pos = sum(.is_l15_pos, na.rm = TRUE),
      ielisa_any_pos = sum(.is_any_pos, na.rm = TRUE),
      ielisa_both_pos = sum(.is_both_pos, na.rm = TRUE),
      ielisa_total = n(),
      .groups = "drop"
    )
}

# ============================================================================
# SUMMARY / VALIDATION FUNCTIONS
# ============================================================================

#' Generate a standardized test summary for validation
#'
#' Use this to verify consistent counts across modules.
#'
#' @param mic_data MIC data frame
#' @param elisa_pe_data ELISA PE data frame
#' @param elisa_vsg_data ELISA VSG data frame
#' @param ielisa_data iELISA data frame
#' @param deduplicate Whether to deduplicate before counting
#' @return List with summary statistics
#' @export
generate_test_summary <- function(mic_data = NULL, elisa_pe_data = NULL,
                                   elisa_vsg_data = NULL, ielisa_data = NULL,
                                   deduplicate = TRUE) {
  summary <- list()

  # MIC summary
  if (!is.null(mic_data) && nrow(mic_data) > 0) {
    if (deduplicate) {
      mic_dedup <- deduplicate_mic_samples(mic_data)
    } else {
      mic_dedup <- mic_data
    }

    has_marker_177t <- "marker_177T" %in% names(mic_dedup) || "Call_177T" %in% names(mic_dedup)
    has_marker_18s2 <- "marker_18S2" %in% names(mic_dedup) || "Call_18S2" %in% names(mic_dedup)

    summary$mic <- list(
      total_samples = nrow(mic_dedup),
      total_tests = nrow(mic_data),
      dna_positive = sum(
        if (has_marker_177t) {
          is_mic_dna_positive(coalesce(mic_dedup$marker_177T, mic_dedup$Call_177T), TRUE)
        } else {
          mic_dedup$FinalCall %in% c("Positive", "Positive_DNA")
        }, na.rm = TRUE
      ),
      rna_positive = sum(
        if (has_marker_18s2) {
          is_mic_rna_positive(coalesce(mic_dedup$marker_18S2, mic_dedup$Call_18S2), TRUE)
        } else {
          mic_dedup$FinalCall %in% c("Positive", "Positive_RNA")
        }, na.rm = TRUE
      ),
      any_positive = sum(is_mic_positive(mic_dedup$FinalCall, TRUE, FALSE), na.rm = TRUE),
      latepositive = sum(mic_dedup$FinalCall == "LatePositive", na.rm = TRUE),
      negative = sum(mic_dedup$FinalCall == "Negative", na.rm = TRUE),
      invalid = sum(mic_dedup$FinalCall %in% standardized_cutoffs()$mic_invalid_calls, na.rm = TRUE)
    )
  }

  # ELISA PE summary
  if (!is.null(elisa_pe_data) && nrow(elisa_pe_data) > 0) {
    pe_status <- classify_elisa(
      status_final = if ("status_final" %in% names(elisa_pe_data)) elisa_pe_data$status_final else NULL,
      pp_percent = if ("PP_percent" %in% names(elisa_pe_data)) elisa_pe_data$PP_percent else NULL,
      dod = if ("DOD" %in% names(elisa_pe_data)) elisa_pe_data$DOD else NULL
    )

    summary$elisa_pe <- list(
      total_tests = nrow(elisa_pe_data),
      positive = sum(pe_status == "Positive", na.rm = TRUE),
      borderline = sum(pe_status == "Borderline", na.rm = TRUE),
      negative = sum(pe_status == "Negative", na.rm = TRUE),
      invalid = sum(pe_status == "Invalid", na.rm = TRUE)
    )
  }

  # ELISA VSG summary
  if (!is.null(elisa_vsg_data) && nrow(elisa_vsg_data) > 0) {
    vsg_status <- classify_elisa(
      status_final = if ("status_final" %in% names(elisa_vsg_data)) elisa_vsg_data$status_final else NULL,
      pp_percent = if ("PP_percent" %in% names(elisa_vsg_data)) elisa_vsg_data$PP_percent else NULL,
      dod = if ("DOD" %in% names(elisa_vsg_data)) elisa_vsg_data$DOD else NULL
    )

    summary$elisa_vsg <- list(
      total_tests = nrow(elisa_vsg_data),
      positive = sum(vsg_status == "Positive", na.rm = TRUE),
      borderline = sum(vsg_status == "Borderline", na.rm = TRUE),
      negative = sum(vsg_status == "Negative", na.rm = TRUE),
      invalid = sum(vsg_status == "Invalid", na.rm = TRUE)
    )
  }

  # iELISA summary
  if (!is.null(ielisa_data) && nrow(ielisa_data) > 0) {
    l13_pos <- is_ielisa_L13_positive(
      inhibition_L13 = if ("pct_inh_f1_13" %in% names(ielisa_data)) ielisa_data$pct_inh_f1_13 else
        if ("pct_inh_f2_13" %in% names(ielisa_data)) ielisa_data$pct_inh_f2_13 else NULL,
      positive_L13 = if ("positive_L13" %in% names(ielisa_data)) ielisa_data$positive_L13 else NULL
    )
    l15_pos <- is_ielisa_L15_positive(
      inhibition_L15 = if ("pct_inh_f1_15" %in% names(ielisa_data)) ielisa_data$pct_inh_f1_15 else
        if ("pct_inh_f2_15" %in% names(ielisa_data)) ielisa_data$pct_inh_f2_15 else NULL,
      positive_L15 = if ("positive_L15" %in% names(ielisa_data)) ielisa_data$positive_L15 else NULL
    )

    summary$ielisa <- list(
      total_tests = nrow(ielisa_data),
      l13_positive = sum(l13_pos, na.rm = TRUE),
      l15_positive = sum(l15_pos, na.rm = TRUE),
      any_positive = sum(l13_pos | l15_pos, na.rm = TRUE),
      both_positive = sum(l13_pos & l15_pos, na.rm = TRUE)
    )
  }

  summary
}

# ============================================================================
# FILTER INVALID RESULTS
# ============================================================================

#' Filter out invalid results from data
#'
#' @param data Data frame with test results
#' @param exclude_invalid Whether to exclude invalid results
#' @param status_col Column name containing status
#' @return Filtered data frame
#' @export
filter_invalid_results <- function(data, exclude_invalid = TRUE, status_col = "FinalCall") {
  if (!exclude_invalid || !status_col %in% names(data)) return(data)

  invalid_values <- c("Invalid", "Invalid_NoDNA", "Failed", "RunInvalid")
  data %>%
    dplyr::filter(!.data[[status_col]] %in% invalid_values)
}
