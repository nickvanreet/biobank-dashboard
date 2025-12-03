# ==============================================================================
# MIC qPCR - STEP 3: INTERPRETATION & DECISION TREE
# ==============================================================================
# Applies Trypanozoon decision logic and RNA preservation assessment
# ==============================================================================

#' Interpret MIC qPCR Data
#'
#' @param qc_data Output from qc_mic()
#' @param verbose Logical, print diagnostic messages
#' @return List with:
#'   - replicate_decisions: per-replicate calls with decision logic
#'   - sample_summary: aggregated per-sample final calls
#' @export
interpret_mic <- function(qc_data, verbose = FALSE) {
  if (verbose) cat("🎯 Applying decision tree...\n")

  cq_data_qc <- qc_data$cq_data_qc
  qc_settings <- qc_data$qc_settings

  if (!nrow(cq_data_qc)) {
    return(list(
      replicate_decisions = tibble::tibble(),
      sample_summary = tibble::tibble()
    ))
  }

  # ===========================================================================
  # STEP 3.1: Summarize by replicate (pivot wide)
  # ===========================================================================
  replicate_summary <- summarize_by_replicate(cq_data_qc)

  if (verbose) cat("✓ Replicate summary:", nrow(replicate_summary), "replicates\n")

  # ===========================================================================
  # STEP 3.2: Compute nucleic acid quality
  # ===========================================================================
  rnasep_rna_cutoff <- qc_settings$thresholds$RNAseP_RNA$positive
  replicate_summary <- compute_nuc_acid_quality(replicate_summary, rnasep_rna_cutoff)

  # ===========================================================================
  # STEP 3.3: Evaluate RNA preservation
  # ===========================================================================
  delta_good <- if (!is.null(qc_settings$delta_good)) qc_settings$delta_good else 5
  delta_warn <- if (!is.null(qc_settings$delta_warn)) qc_settings$delta_warn else 8

  replicate_summary <- evaluate_rna_preservation(replicate_summary,
                                                   delta_good = delta_good,
                                                   delta_warn = delta_warn)

  if (verbose) cat("✓ RNA preservation assessed\n")

  # ===========================================================================
  # STEP 3.4: Apply Trypanozoon decision tree
  # ===========================================================================
  replicate_decisions <- apply_trypanozoon_decision(
    replicate_summary,
    rnasep_rna_cutoff = rnasep_rna_cutoff,
    delta_good = delta_good,
    delta_warn = delta_warn
  )

  if (verbose) {
    decision_counts <- table(replicate_decisions$decision)
    cat("✓ Decision tree applied:\n")
    for (dec in names(decision_counts)) {
      cat(sprintf("   %s: %d\n", dec, decision_counts[dec]))
    }
  }

  # ===========================================================================
  # STEP 3.5: Summarize by sample
  # ===========================================================================
  sample_summary <- summarize_by_sample(replicate_decisions)

  if (verbose) {
    cat("\n✅ Interpretation complete:\n")
    cat(sprintf("   Total samples: %d\n", nrow(sample_summary)))
    if ("final_category" %in% names(sample_summary)) {
      final_cats <- table(sample_summary$final_category)
      for (cat_name in names(final_cats)) {
        cat(sprintf("   %s: %d\n", cat_name, final_cats[cat_name]))
      }
    }
    cat("\n")
  }

  list(
    replicate_decisions = replicate_decisions,
    sample_summary = sample_summary
  )
}

# ==============================================================================
# INTERNAL FUNCTIONS - Preserve all existing logic
# ==============================================================================

summarize_by_replicate <- function(cq_data) {
  replicate_summary <- cq_data %>%
    dplyr::select(Name, Replicate, target, interpretation, Cq, is_control, Type) %>%
    tidyr::pivot_wider(
      names_from = target,
      values_from = c(interpretation, Cq),
      names_sep = "_"
    )

  names(replicate_summary) <- gsub("interpretation_", "marker_", names(replicate_summary))

  required_markers <- c("marker_177T", "marker_18S2", "marker_RNAseP_DNA", "marker_RNAseP_RNA")
  required_cqs     <- c("Cq_177T", "Cq_18S2", "Cq_RNAseP_DNA", "Cq_RNAseP_RNA")

  for (col in required_markers) if (!col %in% names(replicate_summary)) replicate_summary[[col]] <- NA_character_
  for (col in required_cqs)     if (!col %in% names(replicate_summary)) replicate_summary[[col]] <- NA_real_

  replicate_summary %>%
    dplyr::rename(
      RNAseP_DNA    = marker_RNAseP_DNA,
      RNAseP_RNA    = marker_RNAseP_RNA,
      RNAseP_DNA_Cq = Cq_RNAseP_DNA,
      RNAseP_RNA_Cq = Cq_RNAseP_RNA
    )
}

compute_nuc_acid_quality <- function(df, rnasep_rna_cutoff = 30) {
  df %>%
    dplyr::mutate(
      rnasep_dna = dplyr::case_when(
        is.na(RNAseP_DNA) | RNAseP_DNA == "Indeterminate" ~ "Poor DNA",
        RNAseP_DNA == "Negative" ~ "Poor DNA",
        RNAseP_DNA == "Positive" ~ "Good",
        TRUE ~ "Poor DNA"
      ),
      rnasep_rna = dplyr::case_when(
        is.na(RNAseP_RNA) | RNAseP_RNA == "Indeterminate" ~ "No RNA",
        RNAseP_RNA == "Negative" ~ "No RNA",
        RNAseP_RNA == "Positive" & RNAseP_RNA_Cq <= rnasep_rna_cutoff ~ "Good",
        RNAseP_RNA == "Positive" & RNAseP_RNA_Cq > rnasep_rna_cutoff ~ "Poor RNA",
        TRUE ~ "No RNA"
      )
    )
}

evaluate_rna_preservation <- function(df, delta_good = 5, delta_warn = 8) {
  coerce_num <- function(x) suppressWarnings(as.numeric(ifelse(x %in% c("Undetermined", "No Ct", "No Cq"), NA, x)))

  rna_cq  <- coerce_num(df$RNAseP_RNA_Cq)
  dna_cq  <- coerce_num(df$RNAseP_DNA_Cq)

  rna_nd  <- is.na(rna_cq) | (rna_cq < 0)
  dna_nd  <- is.na(dna_cq) | (dna_cq < 0)

  delta <- rna_cq - dna_cq

  preservation <- dplyr::case_when(
    dna_nd                    ~ "Unknown (no RNAseP-DNA)",
    rna_nd                    ~ "Severe RNA loss (no RNAseP-RNA)",
    is.infinite(delta)        ~ "Unknown",
    is.na(delta)              ~ "Unknown",
    delta <= delta_good       ~ "Good RNA preservation",
    delta <= delta_warn       ~ "Moderate RNA loss",
    delta >  delta_warn       ~ "Poor RNA preservation",
    TRUE                      ~ "Unknown"
  )

  dplyr::mutate(df,
                RNA_Preservation_Status = preservation,
                RNA_Preservation_Delta  = delta
  )
}

apply_trypanozoon_decision <- function(replicate_summary,
                                       rnasep_rna_cutoff = 30,
                                       delta_good = 5,
                                       delta_warn = 8) {
  x <- replicate_summary

  x %>%
    dplyr::mutate(
      dna_pos = (marker_177T == "Positive"),
      rna_pos = (marker_18S2 == "Positive"),

      # Base decision for unknown samples
      decision_base = dplyr::case_when(
        dna_pos & rna_pos ~ "Trypanozoon detected (TNA-based)",
        dna_pos         ~ "Trypanozoon DNA detected",
        rna_pos         ~ "Trypanozoon RNA detected",
        rnasep_dna == "Poor DNA" ~ "Failed DNA extraction",
        rnasep_rna == "Poor RNA" ~ "Negative [RNA extraction failed]",
        TRUE ~ "Negative"
      ),
      category_base = dplyr::case_when(
        dna_pos | rna_pos ~ "positive",
        grepl("^Failed DNA extraction$", decision_base) ~ "failed",
        grepl("RNA extraction failed", decision_base) ~ "negative_rna_failed",
        TRUE ~ "negative"
      ),

      # Control overrides (✓/⚠)
      decision = dplyr::case_when(
        Type %in% c("Negative", "NTC") & (dna_pos | rna_pos) ~ "⚠ Contaminated Negative Control",
        Type %in% c("Negative", "NTC") & !(dna_pos | rna_pos) ~ "✓ Negative control OK",
        Type %in% c("Positive", "Standard", "CP") & (dna_pos & rna_pos) ~ "✓ Positive control OK (TNA)",
        Type %in% c("Positive", "Standard", "CP") & dna_pos ~ "✓ Positive control OK (DNA)",
        Type %in% c("Positive", "Standard", "CP") & rna_pos ~ "✓ Positive control OK (RNA)",
        Type %in% c("Positive", "Standard", "CP") & !(dna_pos | rna_pos) ~ "⚠ Failed Positive Control",
        TRUE ~ decision_base
      ),
      category = dplyr::case_when(
        grepl("control OK", decision) ~ "control_ok",
        grepl("Contaminated Negative Control|Failed Positive Control", decision) ~ "control_fail",
        TRUE ~ category_base
      )
    ) %>%
    dplyr::select(-dna_pos, -rna_pos, -decision_base, -category_base)
}

summarize_by_sample <- function(replicate_decisions) {
  replicate_decisions %>%
    dplyr::group_by(Name, is_control, Type) %>%
    dplyr::summarize(
      n_replicates   = dplyr::n(),
      n_positive     = sum(category == "positive", na.rm = TRUE),
      n_negative     = sum(category == "negative", na.rm = TRUE),
      n_failed       = sum(category == "failed",   na.rm = TRUE),
      n_inconclusive = sum(category == "inconclusive", na.rm = TRUE),
      n_control_ok   = sum(category == "control_ok",   na.rm = TRUE),
      n_control_fail = sum(category == "control_fail", na.rm = TRUE),

      final_decision = {
        d <- decision[!is.na(decision)]
        if (length(d) == 0) "No valid replicates" else names(sort(table(d), decreasing = TRUE))[1]
      },

      avg_177T_Cq = mean(Cq_177T[marker_177T == "Positive"], na.rm = TRUE),
      avg_18S2_Cq = mean(Cq_18S2[marker_18S2 == "Positive"], na.rm = TRUE),

      rna_quality = {
        s <- rnasep_rna[!is.na(rnasep_rna)]
        if (!length(s)) "Unknown" else if (all(s == "Good")) "Good"
        else if (all(s == "No RNA")) "No RNA" else if (any(s == "Poor RNA")) "Poor RNA" else "Mixed"
      },
      dna_quality = {
        s <- rnasep_dna[!is.na(rnasep_dna)]
        if (!length(s)) "Unknown" else if (all(s == "Good")) "Good" else "Poor DNA"
      },

      # RNA preservation consensus & average ΔCq
      rna_preservation = {
        vals <- RNA_Preservation_Status[!is.na(RNA_Preservation_Status)]
        if (!length(vals)) "Unknown" else names(sort(table(vals), decreasing = TRUE))[1]
      },
      avg_preservation_delta = mean(RNA_Preservation_Delta, na.rm = TRUE),

      .groups = "drop"
    ) %>%
    dplyr::mutate(
      final_category = dplyr::case_when(
        n_control_fail > 0 ~ "control_fail",
        n_control_ok   > 0 & is_control ~ "control_ok",
        n_positive >= 2 ~ "positive",
        n_negative >= 2 ~ "negative",
        n_failed   >= 2 ~ "failed",
        TRUE ~ "inconclusive"
      ),
      quality_flag = dplyr::case_when(
        final_category == "control_fail"                     ~ "⚠ Control out of spec",
        rna_preservation == "Severe RNA loss (no RNAseP-RNA)"~ "⚠ Severe RNA loss",
        rna_preservation == "Poor RNA preservation"          ~ "⚠ Poor RNA preservation",
        dna_quality == "Poor DNA"                            ~ "⚠ Poor DNA extraction",
        rna_quality == "Poor RNA"                            ~ "⚠ Poor RNA extraction",
        n_replicates < 3                                     ~ "⚠ Insufficient replicates",
        final_category == "inconclusive"                     ~ "⚠ Inconclusive results",
        TRUE ~ "✓ Good"
      )
    )
}
