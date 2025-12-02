# =============================================================================
# ELISA-VSG Interpretation Module (STEP 3)
# Calculates PP%, classifies samples, handles retests
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# Note: Shared utilities (utils_elisa.R) should be sourced by the caller

# =============================================================================
# STEP 3: SAMPLE INTERPRETATION
# =============================================================================

#' Calculate PP% and classify samples
#' @param qc_summary QC summary from qc_elisa_pe()
#' @param qc_settings QC settings list
#' @return Data frame with PP%, sample classification, and metrics
calculate_sample_metrics <- function(qc_summary, qc_settings) {
  qc_summary %>%
    mutate(
      # Calculate PP% using plate-level controls
      PP_percent = calculate_pp_percent(DOD, PC_DOD, NC_DOD),

      # Classify sample status
      status_raw = classify_elisa_sample(PP_percent, DOD, qc_settings),

      # Flag invalid samples (failed sample-level QC)
      status_raw = if_else(
        !is.na(qc_overall) & qc_overall == FALSE,
        "Invalid",
        status_raw
      ),

      # Create QC flags
      qc_flags = create_qc_flags(qc_Ag_plus, qc_Ag0)
    )
}

#' Handle retests and consolidate sample status
#' For samples tested multiple times, apply consolidation rules:
#' - Prefer valid runs
#' - If >1 valid: take most recent (or median/consensus)
#' - If conflict: classify as Discordant
#'
#' @param interpreted_data Data frame with interpreted samples
#' @return Data frame with consolidated status per sample
consolidate_retests <- function(interpreted_data) {
  # For now, we'll keep all runs (consolidation will be done at dashboard level)
  # This allows the coordinator to show retest tracking

  # Add n_runs per sample
  interpreted_data <- interpreted_data %>%
    group_by(numero_labo, code_barres_kps, elisa_type) %>%
    mutate(
      n_runs = n_distinct(plate_id, plate_date),
      is_retest = n_runs > 1
    ) %>%
    ungroup()

  # For final status, take most recent valid run
  # (This can be customized based on requirements)
  final_status <- interpreted_data %>%
    filter(sample_type == "sample") %>%
    group_by(numero_labo, code_barres_kps, elisa_type) %>%
    arrange(desc(plate_date), desc(plate_valid)) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    select(numero_labo, code_barres_kps, elisa_type, status_final = status_raw)

  # Join back to get status_final for all runs
  interpreted_data %>%
    left_join(
      final_status,
      by = c("numero_labo", "code_barres_kps", "elisa_type")
    ) %>%
    mutate(
      # If no final status (e.g., control), use raw status
      status_final = if_else(is.na(status_final), status_raw, status_final)
    )
}

#' Main ELISA-VSG interpretation function
#' @param qc_result Result from qc_elisa_vsg()
#' @return List with:
#'   - interpreted_data: Full data with PP%, classification, metrics
#'   - sample_summary: Summary per sample (consolidated across retests)
#'   - discordant_samples: Samples with conflicting results across runs
#' @export
interpret_elisa_vsg <- function(qc_result) {
  cat(sprintf("📊 INTERPRETATION (VSG)\n"))

  # Calculate metrics and classify
  interpreted_data <- calculate_sample_metrics(
    qc_result$qc_summary,
    qc_result$qc_settings
  )

  cat(sprintf("  ✓ Calculated PP%% and classified %d entries\n", nrow(interpreted_data)))

  # Consolidate retests
  interpreted_data <- consolidate_retests(interpreted_data)

  # Count classifications
  if (nrow(interpreted_data) > 0) {
    sample_data <- interpreted_data %>% filter(sample_type == "sample")
    if (nrow(sample_data) > 0) {
      status_counts <- table(sample_data$status_final)
      cat(sprintf("  ✓ Sample classifications: %s\n",
                  paste(names(status_counts), "=", status_counts, collapse = ", ")))
    }
  }

  # Identify discordant samples (conflicting results across runs)
  discordant_samples <- interpreted_data %>%
    filter(sample_type == "sample", n_runs > 1) %>%
    group_by(numero_labo, code_barres_kps, elisa_type) %>%
    filter(n_distinct(status_raw) > 1) %>%
    ungroup()

  if (nrow(discordant_samples) > 0) {
    cat(sprintf("  ⚠ Found %d samples with discordant results across runs\n",
                n_distinct(discordant_samples$numero_labo, discordant_samples$code_barres_kps)))
  }

  # Create sample summary (one row per unique sample)
  sample_summary <- interpreted_data %>%
    filter(sample_type == "sample") %>%
    group_by(numero_labo, code_barres_kps, elisa_type) %>%
    summarise(
      status_final = first(status_final),
      n_runs = first(n_runs),
      latest_date = max(plate_date, na.rm = TRUE),
      mean_PP_percent = mean(PP_percent, na.rm = TRUE),
      mean_DOD = mean(DOD, na.rm = TRUE),
      has_discordance = n_distinct(status_raw) > 1,
      .groups = "drop"
    )

  cat(sprintf("  ✓ Interpretation complete: %d unique samples\n", nrow(sample_summary)))

  return(list(
    interpreted_data = interpreted_data,
    sample_summary = sample_summary,
    discordant_samples = discordant_samples
  ))
}
