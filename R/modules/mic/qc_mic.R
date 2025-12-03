# ==============================================================================
# MIC qPCR - STEP 2: QC & INTERPRETATION
# ==============================================================================
# Applies Cq cutoffs and validates control performance
# ==============================================================================

#' Default MIC QC Settings
#'
#' @return List of default cutoffs for each target
#' @export
mic_default_qc_settings <- function() {
  list(
    thresholds = list(
      "177T"       = list(positive = 35, negative = 40),
      "18S2"       = list(positive = 35, negative = 40),
      "RNAseP_DNA" = list(positive = 32, negative = 999),
      "RNAseP_RNA" = list(positive = 30, negative = 999)
    ),
    late_window = c(38, 40),
    delta_rp_limit = 8,
    delta_good = 5,
    delta_warn = 8,
    min_positive_reps = 2
  )
}

#' QC MIC qPCR Data
#'
#' @param ingestion_data Output from ingest_mic()
#' @param qc_settings List of QC settings (uses defaults if NULL)
#' @param verbose Logical, print diagnostic messages
#' @return List with:
#'   - cq_data_qc: tibble with added 'interpretation' column (Positive/Negative/Indeterminate)
#'   - qc_flags: tibble of control validation results
#'   - qc_settings: the settings used
#' @export
qc_mic <- function(ingestion_data, qc_settings = NULL, verbose = FALSE) {
  if (is.null(qc_settings)) {
    qc_settings <- mic_default_qc_settings()
  }

  if (verbose) cat("🔍 Applying QC cutoffs...\n")

  cq_data <- ingestion_data$cq_data

  if (!nrow(cq_data)) {
    return(list(
      cq_data_qc = cq_data,
      qc_flags = tibble::tibble(),
      qc_settings = qc_settings
    ))
  }

  # ===========================================================================
  # STEP 2.1: Apply cutoffs to classify each Cq value
  # ===========================================================================
  cutoffs <- sanitize_cutoffs(qc_settings$thresholds)

  cq_data_qc <- cq_data %>%
    dplyr::mutate(
      interpretation = purrr::map2_chr(
        target, Cq,
        function(tgt, cq) {
          cq_num <- suppressWarnings(as.numeric(cq))

          # Treat invalid values as Negative (no amplification)
          if (is.na(cq_num) || cq_num < 0 || is.infinite(cq_num)) return("Negative")
          if (!tgt %in% names(cutoffs)) return("Unknown")

          tgt_cutoffs <- cutoffs[[tgt]]
          pos_cutoff <- coerce_cutoff_numeric(tgt_cutoffs$positive)
          neg_cutoff <- coerce_cutoff_numeric(tgt_cutoffs$negative)

          if (is.null(pos_cutoff) || is.na(pos_cutoff)) pos_cutoff <- -Inf
          if (is.null(neg_cutoff) || is.na(neg_cutoff)) neg_cutoff <- Inf

          if (cq_num <= pos_cutoff) {
            "Positive"
          } else if (cq_num >= neg_cutoff) {
            "Negative"
          } else {
            "Indeterminate"
          }
        }
      )
    )

  if (verbose) {
    cat("✓ Interpretation complete:\n")
    for (tgt in unique(cq_data_qc$target)) {
      tgt_data <- cq_data_qc %>% dplyr::filter(target == tgt)
      n_pos <- sum(tgt_data$interpretation == "Positive", na.rm = TRUE)
      n_neg <- sum(tgt_data$interpretation == "Negative", na.rm = TRUE)
      n_ind <- sum(tgt_data$interpretation == "Indeterminate", na.rm = TRUE)
      cat(sprintf("   %s: %d positive, %d negative, %d indeterminate\n",
                  tgt, n_pos, n_neg, n_ind))
    }
  }

  # ===========================================================================
  # STEP 2.2: Validate controls
  # ===========================================================================
  qc_flags <- validate_controls_qc(cq_data_qc, verbose = verbose)

  if (verbose) cat("\n✅ QC complete\n\n")

  list(
    cq_data_qc = cq_data_qc,
    qc_flags = qc_flags,
    qc_settings = qc_settings
  )
}

#' Validate Control Performance
#'
#' @param cq_data_qc Cq data with interpretation column
#' @param verbose Logical
#' @return Tibble of control validation flags
validate_controls_qc <- function(cq_data_qc, verbose = FALSE) {
  controls <- cq_data_qc %>%
    dplyr::filter(is_control == TRUE)

  if (!nrow(controls)) {
    if (verbose) cat("⚠ No controls found\n")
    return(tibble::tibble())
  }

  control_summary <- controls %>%
    dplyr::group_by(Name, Type, target) %>%
    dplyr::summarise(
      n_replicates = dplyr::n(),
      n_positive = sum(interpretation == "Positive", na.rm = TRUE),
      n_negative = sum(interpretation == "Negative", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        Type == "Positive" ~ "Should be positive",
        Type == "Negative" ~ "Should be negative",
        TRUE ~ "Unknown"
      ),
      control_pass = dplyr::case_when(
        Type == "Positive" & n_positive >= 1 ~ TRUE,
        Type == "Negative" & n_positive == 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      flag = dplyr::case_when(
        !control_pass & Type == "Positive" ~ "⚠ Positive control failed",
        !control_pass & Type == "Negative" ~ "⚠ Negative control contaminated",
        control_pass ~ "✓ Control OK",
        TRUE ~ "Unknown"
      )
    )

  if (verbose) {
    n_pass <- sum(control_summary$control_pass, na.rm = TRUE)
    n_total <- nrow(control_summary)
    cat(sprintf("   Controls: %d/%d passed (%.1f%%)\n",
                n_pass, n_total, 100 * n_pass / n_total))
  }

  control_summary
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

coerce_cutoff_numeric <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_real_)

  scalar <- value
  while (is.list(scalar) && length(scalar) > 0) {
    scalar <- scalar[[1]]
  }

  if (length(scalar) == 0 || is.null(scalar)) return(NA_real_)

  if (is.language(scalar)) {
    scalar <- as.character(scalar)
  }

  suppressWarnings(
    tryCatch(
      as.numeric(scalar),
      warning = function(...) NA_real_,
      error = function(...) NA_real_
    )
  )
}

sanitize_cutoffs <- function(cutoffs) {
  if (!is.list(cutoffs) || !length(cutoffs)) return(list())
  out <- lapply(cutoffs, function(th) {
    list(
      positive = coerce_cutoff_numeric(th$positive),
      negative = coerce_cutoff_numeric(th$negative)
    )
  })
  names(out) <- names(cutoffs)
  out
}
