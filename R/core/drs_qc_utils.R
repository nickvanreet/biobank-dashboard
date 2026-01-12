# R/core/drs_qc_utils.R
# DRS Quality Control Utilities
# ============================================================================
# Utility functions for DRS volume validation, RNAseP Cq anomaly detection,
# and structure sanitaire quality monitoring

# ============================================================================
# DRS VOLUME THRESHOLDS
# ============================================================================
# Critical thresholds based on barcode numbering:
# - Barcode < 2502200: Expected volume ~2 mL
# - Barcode >= 2502200: Expected volume ~4 mL

DRS_QC_THRESHOLDS <- list(
  # Volume thresholds for old barcodes (< 2502200)
  old_barcode = list(
    cutoff = 2502200,
    target_volume = 2.0,
    min_volume = 1.5,
    max_volume = 2.5,
    critical_low = 1.0,
    critical_high = 3.0
  ),
  # Volume thresholds for new barcodes (>= 2502200)
  new_barcode = list(
    target_volume = 4.0,
    min_volume = 3.5,
    max_volume = 4.5,
    critical_low = 2.5,
    critical_high = 5.0
  ),
  # Pre-analytical time thresholds (days)
  preanalytical_time = list(
    optimal = 7,
    acceptable = 14,
    caution = 30,
    critical = 60
  ),
  # RNAseP Cq thresholds
  rnasep = list(
    dna_optimal_max = 25,
    dna_warning_max = 30,
    dna_critical_max = 35,
    rna_optimal_max = 28,
    rna_warning_max = 33,
    rna_critical_max = 38,
    delta_rp_optimal_max = 3,
    delta_rp_warning_max = 5,
    delta_rp_critical_max = 8
  )
)

#' Extract numeric barcode for threshold comparison
#' @param barcode Character vector of barcodes
#' @return Numeric vector of extracted barcode numbers
#' @export
extract_barcode_number <- function(barcode) {
  if (is.null(barcode)) return(NA_real_)

  # Remove common prefixes (KPS, etc.) and extract numeric portion
  bc <- as.character(barcode)
  bc <- gsub("^[Kk][Pp][Ss][-_ ]*", "", bc)
  bc <- gsub("[^0-9]", "", bc)

  # Convert to numeric
  num <- suppressWarnings(as.numeric(bc))

  # Return NA for invalid/empty barcodes
  num[is.na(num) | bc == ""] <- NA_real_

  num
}

#' Determine expected volume based on barcode number
#' @param barcode Character vector of barcodes
#' @return Numeric vector of expected volumes (2 or 4 mL)
#' @export
get_expected_drs_volume <- function(barcode) {
  barcode_num <- extract_barcode_number(barcode)

  dplyr::case_when(
    is.na(barcode_num) ~ NA_real_,
    barcode_num < DRS_QC_THRESHOLDS$old_barcode$cutoff ~ DRS_QC_THRESHOLDS$old_barcode$target_volume,
    TRUE ~ DRS_QC_THRESHOLDS$new_barcode$target_volume
  )
}

#' Classify DRS volume status based on barcode-specific thresholds
#' @param volume_ml Numeric vector of DRS volumes in mL
#' @param barcode Character vector of barcodes
#' @return Character vector with volume status classification
#' @export
classify_drs_volume_status <- function(volume_ml, barcode) {
  barcode_num <- extract_barcode_number(barcode)
  is_new_barcode <- !is.na(barcode_num) & barcode_num >= DRS_QC_THRESHOLDS$old_barcode$cutoff

  # Get thresholds based on barcode type
  thresholds <- dplyr::if_else(
    is_new_barcode,
    list(DRS_QC_THRESHOLDS$new_barcode),
    list(DRS_QC_THRESHOLDS$old_barcode)
  )

  # Vectorized threshold lookup
  old <- DRS_QC_THRESHOLDS$old_barcode
  new <- DRS_QC_THRESHOLDS$new_barcode

  min_vol <- dplyr::if_else(is_new_barcode, new$min_volume, old$min_volume)
  max_vol <- dplyr::if_else(is_new_barcode, new$max_volume, old$max_volume)
  crit_low <- dplyr::if_else(is_new_barcode, new$critical_low, old$critical_low)
  crit_high <- dplyr::if_else(is_new_barcode, new$critical_high, old$critical_high)

  dplyr::case_when(
    is.na(volume_ml) ~ "Missing",
    volume_ml < crit_low ~ "Critical Low",
    volume_ml < min_vol ~ "Low",
    volume_ml > crit_high ~ "Critical High",
    volume_ml > max_vol ~ "High",
    TRUE ~ "Normal"
  )
}

#' Calculate volume deviation from expected value
#' @param volume_ml Numeric vector of DRS volumes in mL
#' @param barcode Character vector of barcodes
#' @return Numeric vector with deviation (actual - expected)
#' @export
calculate_volume_deviation <- function(volume_ml, barcode) {
  expected <- get_expected_drs_volume(barcode)
  volume_ml - expected
}

#' Calculate volume deviation percentage
#' @param volume_ml Numeric vector of DRS volumes in mL
#' @param barcode Character vector of barcodes
#' @return Numeric vector with deviation percentage
#' @export
calculate_volume_deviation_pct <- function(volume_ml, barcode) {
  expected <- get_expected_drs_volume(barcode)
  (volume_ml - expected) / expected * 100
}

# ============================================================================
# PRE-ANALYTICAL TIME CLASSIFICATION
# ============================================================================

#' Classify pre-analytical time status
#' @param days Numeric vector of pre-analytical days
#' @return Character vector with time status classification
#' @export
classify_preanalytical_time <- function(days) {
  thresholds <- DRS_QC_THRESHOLDS$preanalytical_time

  dplyr::case_when(
    is.na(days) ~ "Unknown",
    days < 0 ~ "Invalid",
    days <= thresholds$optimal ~ "Optimal",
    days <= thresholds$acceptable ~ "Acceptable",
    days <= thresholds$caution ~ "Caution",
    days <= thresholds$critical ~ "Problematic",
    TRUE ~ "Critical"
  )
}

# ============================================================================
# RNASEP CQ ANOMALY DETECTION
# ============================================================================

#' Classify RNAseP DNA Cq status
#' @param cq Numeric vector of RNAseP DNA Cq values
#' @return Character vector with Cq status classification
#' @export
classify_rnasep_dna_cq <- function(cq) {
  thresholds <- DRS_QC_THRESHOLDS$rnasep

  dplyr::case_when(
    is.na(cq) | is.infinite(cq) ~ "No Data",
    cq <= thresholds$dna_optimal_max ~ "Optimal",
    cq <= thresholds$dna_warning_max ~ "Acceptable",
    cq <= thresholds$dna_critical_max ~ "Degraded",
    TRUE ~ "Critical"
  )
}

#' Classify RNAseP RNA Cq status
#' @param cq Numeric vector of RNAseP RNA Cq values
#' @return Character vector with Cq status classification
#' @export
classify_rnasep_rna_cq <- function(cq) {
  thresholds <- DRS_QC_THRESHOLDS$rnasep

  dplyr::case_when(
    is.na(cq) | is.infinite(cq) ~ "No Data",
    cq <= thresholds$rna_optimal_max ~ "Optimal",
    cq <= thresholds$rna_warning_max ~ "Acceptable",
    cq <= thresholds$rna_critical_max ~ "Degraded",
    TRUE ~ "Critical"
  )
}

#' Classify Delta RP (RNA-DNA) status
#' @param delta_rp Numeric vector of Delta RP values
#' @return Character vector with Delta RP status classification
#' @export
classify_delta_rp <- function(delta_rp) {
  thresholds <- DRS_QC_THRESHOLDS$rnasep

  dplyr::case_when(
    is.na(delta_rp) | is.infinite(delta_rp) ~ "No Data",
    delta_rp <= thresholds$delta_rp_optimal_max ~ "Optimal",
    delta_rp <= thresholds$delta_rp_warning_max ~ "Acceptable",
    delta_rp <= thresholds$delta_rp_critical_max ~ "Degraded",
    TRUE ~ "Critical"
  )
}

#' Detect RNAseP Cq anomalies (outliers)
#' @param cq Numeric vector of Cq values
#' @param iqr_multiplier IQR multiplier for outlier detection (default 1.5)
#' @return Logical vector indicating anomalies
#' @export
detect_rnasep_anomalies <- function(cq, iqr_multiplier = 1.5) {
  if (all(is.na(cq))) return(rep(FALSE, length(cq)))

  q1 <- quantile(cq, 0.25, na.rm = TRUE)
  q3 <- quantile(cq, 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower_bound <- q1 - iqr_multiplier * iqr
  upper_bound <- q3 + iqr_multiplier * iqr

  !is.na(cq) & (cq < lower_bound | cq > upper_bound)
}

# ============================================================================
# COMPREHENSIVE QC ASSESSMENT
# ============================================================================

#' Generate comprehensive QC warnings for a DRS sample
#' @param volume_ml DRS volume in mL
#' @param barcode Sample barcode
#' @param preanalytical_days Pre-analytical time in days
#' @param rnasep_dna_cq RNAseP DNA Cq value
#' @param rnasep_rna_cq RNAseP RNA Cq value
#' @return List with QC warnings
#' @export
generate_drs_qc_warnings <- function(volume_ml, barcode,
                                      preanalytical_days = NA,
                                      rnasep_dna_cq = NA,
                                      rnasep_rna_cq = NA) {
  warnings <- list()
  warning_count <- 0
  critical_count <- 0

  # Volume warnings
  volume_status <- classify_drs_volume_status(volume_ml, barcode)
  expected_vol <- get_expected_drs_volume(barcode)

  if (volume_status %in% c("Critical Low", "Critical High")) {
    warnings$volume <- list(
      type = "critical",
      message = sprintf("Volume %s: %.2f mL (expected ~%.1f mL)",
                       tolower(volume_status), volume_ml, expected_vol)
    )
    critical_count <- critical_count + 1
  } else if (volume_status %in% c("Low", "High")) {
    warnings$volume <- list(
      type = "warning",
      message = sprintf("Volume %s: %.2f mL (expected ~%.1f mL)",
                       tolower(volume_status), volume_ml, expected_vol)
    )
    warning_count <- warning_count + 1
  }

  # Pre-analytical time warnings
  if (!is.na(preanalytical_days)) {
    time_status <- classify_preanalytical_time(preanalytical_days)
    if (time_status %in% c("Critical", "Invalid")) {
      warnings$time <- list(
        type = "critical",
        message = sprintf("Pre-analytical time: %.0f days (%s)",
                         preanalytical_days, time_status)
      )
      critical_count <- critical_count + 1
    } else if (time_status %in% c("Problematic", "Caution")) {
      warnings$time <- list(
        type = "warning",
        message = sprintf("Pre-analytical time: %.0f days (%s)",
                         preanalytical_days, time_status)
      )
      warning_count <- warning_count + 1
    }
  }

  # RNAseP DNA warnings
  if (!is.na(rnasep_dna_cq) && !is.infinite(rnasep_dna_cq)) {
    dna_status <- classify_rnasep_dna_cq(rnasep_dna_cq)
    if (dna_status == "Critical") {
      warnings$rnasep_dna <- list(
        type = "critical",
        message = sprintf("RNAseP DNA Cq: %.1f (Critical degradation)", rnasep_dna_cq)
      )
      critical_count <- critical_count + 1
    } else if (dna_status == "Degraded") {
      warnings$rnasep_dna <- list(
        type = "warning",
        message = sprintf("RNAseP DNA Cq: %.1f (Degraded)", rnasep_dna_cq)
      )
      warning_count <- warning_count + 1
    }
  }

  # RNAseP RNA warnings
  if (!is.na(rnasep_rna_cq) && !is.infinite(rnasep_rna_cq)) {
    rna_status <- classify_rnasep_rna_cq(rnasep_rna_cq)
    if (rna_status == "Critical") {
      warnings$rnasep_rna <- list(
        type = "critical",
        message = sprintf("RNAseP RNA Cq: %.1f (Critical degradation)", rnasep_rna_cq)
      )
      critical_count <- critical_count + 1
    } else if (rna_status == "Degraded") {
      warnings$rnasep_rna <- list(
        type = "warning",
        message = sprintf("RNAseP RNA Cq: %.1f (Degraded)", rnasep_rna_cq)
      )
      warning_count <- warning_count + 1
    }
  }

  # Delta RP warnings
  if (!is.na(rnasep_dna_cq) && !is.na(rnasep_rna_cq) &&
      !is.infinite(rnasep_dna_cq) && !is.infinite(rnasep_rna_cq)) {
    delta_rp <- rnasep_rna_cq - rnasep_dna_cq
    delta_status <- classify_delta_rp(delta_rp)
    if (delta_status == "Critical") {
      warnings$delta_rp <- list(
        type = "critical",
        message = sprintf("Delta RP: %.1f (Critical RNA degradation)", delta_rp)
      )
      critical_count <- critical_count + 1
    } else if (delta_status == "Degraded") {
      warnings$delta_rp <- list(
        type = "warning",
        message = sprintf("Delta RP: %.1f (RNA degradation)", delta_rp)
      )
      warning_count <- warning_count + 1
    }
  }

  list(
    warnings = warnings,
    warning_count = warning_count,
    critical_count = critical_count,
    has_issues = warning_count > 0 || critical_count > 0,
    overall_status = dplyr::case_when(
      critical_count > 0 ~ "Critical",
      warning_count > 0 ~ "Warning",
      TRUE ~ "OK"
    )
  )
}

#' Add QC flags to extraction dataframe
#' @param df Extraction dataframe
#' @return Dataframe with QC flags added
#' @export
add_drs_qc_flags <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)

  # Get barcode column
  barcode_col <- if ("barcode" %in% names(df)) {
    df$barcode
  } else if ("sample_id" %in% names(df)) {
    df$sample_id
  } else if ("numero" %in% names(df)) {
    df$numero
  } else {
    NA_character_
  }

  # Add QC columns
 df %>%
    dplyr::mutate(
      # Barcode number for threshold
      barcode_number = extract_barcode_number(!!rlang::sym(
        if ("barcode" %in% names(.)) "barcode"
        else if ("sample_id" %in% names(.)) "sample_id"
        else "numero"
      )),

      # Expected volume based on barcode
      expected_volume_ml = dplyr::case_when(
        is.na(barcode_number) ~ NA_real_,
        barcode_number < DRS_QC_THRESHOLDS$old_barcode$cutoff ~ DRS_QC_THRESHOLDS$old_barcode$target_volume,
        TRUE ~ DRS_QC_THRESHOLDS$new_barcode$target_volume
      ),

      # Volume status
      volume_status = classify_drs_volume_status(
        drs_volume_ml,
        !!rlang::sym(if ("barcode" %in% names(.)) "barcode"
                     else if ("sample_id" %in% names(.)) "sample_id"
                     else "numero")
      ),

      # Volume deviation
      volume_deviation_ml = drs_volume_ml - expected_volume_ml,
      volume_deviation_pct = (drs_volume_ml - expected_volume_ml) / expected_volume_ml * 100,

      # Volume warning flag
      volume_warning = volume_status %in% c("Low", "High", "Critical Low", "Critical High"),
      volume_critical = volume_status %in% c("Critical Low", "Critical High")
    )
}

# ============================================================================
# STRUCTURE SANITAIRE STATISTICS
# ============================================================================

#' Calculate QC statistics by structure sanitaire
#' @param df Dataframe with QC flags added
#' @return Tibble with statistics per structure
#' @export
summarise_qc_by_structure <- function(df) {
  df <- ensure_health_structure_column(df)

  if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
    return(tibble::tibble(
      health_structure = character(),
      n_samples = integer(),
      mean_volume = numeric(),
      expected_volume = numeric(),
      volume_deviation_mean = numeric(),
      pct_volume_normal = numeric(),
      pct_volume_warning = numeric(),
      pct_volume_critical = numeric(),
      mean_preanalytical_days = numeric(),
      pct_time_optimal = numeric(),
      pct_time_warning = numeric(),
      mean_rnasep_dna_cq = numeric(),
      mean_rnasep_rna_cq = numeric(),
      mean_delta_rp = numeric(),
      overall_qc_score = numeric()
    ))
  }

  # Ensure required columns exist
  if (!"volume_status" %in% names(df)) {
    df <- add_drs_qc_flags(df)
  }

  df %>%
    dplyr::filter(!is.na(health_structure) & health_structure != "" & health_structure != "Unspecified") %>%
    dplyr::group_by(health_structure) %>%
    dplyr::summarise(
      n_samples = dplyr::n(),

      # Volume metrics
      mean_volume = mean(drs_volume_ml, na.rm = TRUE),
      expected_volume = mean(expected_volume_ml, na.rm = TRUE),
      volume_deviation_mean = mean(volume_deviation_ml, na.rm = TRUE),
      pct_volume_normal = mean(volume_status == "Normal", na.rm = TRUE) * 100,
      pct_volume_warning = mean(volume_status %in% c("Low", "High"), na.rm = TRUE) * 100,
      pct_volume_critical = mean(volume_status %in% c("Critical Low", "Critical High"), na.rm = TRUE) * 100,

      # Pre-analytical time metrics (if available)
      mean_preanalytical_days = if ("preanalytical_days" %in% names(.)) {
        mean(preanalytical_days, na.rm = TRUE)
      } else NA_real_,
      pct_time_optimal = if ("time_status" %in% names(.)) {
        mean(time_status == "Optimal", na.rm = TRUE) * 100
      } else NA_real_,
      pct_time_warning = if ("time_status" %in% names(.)) {
        mean(time_status %in% c("Caution", "Problematic", "Critical"), na.rm = TRUE) * 100
      } else NA_real_,

      # RNAseP metrics (if available)
      mean_rnasep_dna_cq = if ("rnasep_dna_cq" %in% names(.)) {
        mean(rnasep_dna_cq, na.rm = TRUE)
      } else NA_real_,
      mean_rnasep_rna_cq = if ("rnasep_rna_cq" %in% names(.)) {
        mean(rnasep_rna_cq, na.rm = TRUE)
      } else NA_real_,
      mean_delta_rp = if ("delta_rp" %in% names(.)) {
        mean(delta_rp, na.rm = TRUE)
      } else NA_real_,

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Overall QC score (0-100, higher is better)
      overall_qc_score = dplyr::case_when(
        n_samples == 0 ~ NA_real_,
        TRUE ~ pct_volume_normal * 0.4 +
               dplyr::coalesce(pct_time_optimal, 100) * 0.3 +
               dplyr::if_else(
                 is.na(mean_rnasep_dna_cq),
                 100,
                 pmax(0, 100 - (mean_rnasep_dna_cq - 25) * 3)
               ) * 0.3
      )
    ) %>%
    dplyr::arrange(dplyr::desc(n_samples))
}

#' Identify structures with QC issues
#' @param df Dataframe with QC flags added
#' @param volume_warning_threshold Threshold for volume warning rate
#' @param time_warning_threshold Threshold for time warning rate
#' @return Tibble with structures having QC issues
#' @export
identify_structures_with_issues <- function(df,
                                            volume_warning_threshold = 20,
                                            time_warning_threshold = 30) {
  stats <- summarise_qc_by_structure(df)

  stats %>%
    dplyr::filter(
      pct_volume_warning + pct_volume_critical > volume_warning_threshold |
      (!is.na(pct_time_warning) & pct_time_warning > time_warning_threshold) |
      (!is.na(mean_rnasep_dna_cq) & mean_rnasep_dna_cq > 30)
    ) %>%
    dplyr::mutate(
      issues = dplyr::case_when(
        pct_volume_critical > 10 ~ "Critical volume issues",
        pct_volume_warning > volume_warning_threshold ~ "Volume warnings",
        !is.na(pct_time_warning) & pct_time_warning > time_warning_threshold ~ "Pre-analytical time issues",
        !is.na(mean_rnasep_dna_cq) & mean_rnasep_dna_cq > 30 ~ "RNAseP quality issues",
        TRUE ~ "Multiple issues"
      )
    )
}

# Helper function from extraction_data_utils.R
ensure_health_structure_column <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }

  if ("health_structure" %in% names(df)) {
    return(df)
  }

  fallback_cols <- c(
    "structure_sanitaire",
    "health_facility",
    "structure",
    "facility"
  )

  available <- fallback_cols[fallback_cols %in% names(df)]

  if (length(available) == 0) {
    return(df)
  }

  df$health_structure <- df[[available[[1]]]]
  df
}
