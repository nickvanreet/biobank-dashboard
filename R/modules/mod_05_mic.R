# =============================================================================
# Module 05 (MIC-ONLY) — BioMérieux MIC qPCR QC
# =============================================================================
# - Reads ALL MIC *.xlsx files in a directory
# - Runs analyze_qpcr(file) (from your robust MIC pipeline)
# - Aggregates "sample_summary" + "replicate_data" across runs
# - KPIs, tables (samples / controls), and rich plots
# - Export a QC workbook (Run QC, Controls, Samples, Anomalies)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(DT)
  library(plotly)
  library(writexl)
})

# ---------- Utility: control detection (fallback if Type missing) -------------
.mic_is_control <- function(Type, Name) {
  if (!is.null(Type) && !all(is.na(Type))) {
    return(Type %in% c("Positive","Negative","NTC","Standard","Positive Control","Negative Control"))
  }
  grepl("\\b(NTC|NC|CN|NEG|NEGATIVE|PC|CP|POS|POSITIVE|CTRL|CONTROL|BLANK|WATER)\\b",
        Name %||% "", ignore.case = TRUE)
}
.mic_control_type <- function(Type, Name) {
  if (!is.null(Type) && !all(is.na(Type))) {
    return(dplyr::case_when(
      Type %in% c("NTC","Negative","Negative Control") ~ "NTC",
      Type %in% c("Positive","Positive Control","Standard") ~ "PC",
      TRUE ~ NA_character_
    ))
  }
  dplyr::case_when(
    grepl("\\b(NTC|BLANK|WATER)\\b", Name, TRUE) ~ "NTC",
    grepl("\\b(NC|CN|NEG)\\b", Name, TRUE) ~ "NTC",
    grepl("\\b(PC|CP|POS)\\b", Name, TRUE) ~ "PC",
    grepl("\\b(CTRL|CONTROL)\\b", Name, TRUE) ~ "CTRL",
    TRUE ~ NA_character_
  )
}

# ---------- Thresholds & normalization helpers -------------------------------
.mic_default_thresholds <- function() {
  list(
    late_pos_min = 35,
    late_pos_max = 40,
    replicate_spread_warn = 1.5,
    replicate_mad_warn = 0.75,
    confidence_high = 5,
    confidence_medium = 2,
    neg_cutoff = 35,
    ntc_max = 39,
    pc_min = 15,
    pc_max = 30,
    rnp_no_rna_cq = 38
  )
}

.mic_normalize_first <- function(df, columns) {
  if (is.null(df) || !nrow(df) || !length(columns)) {
    return(rep(NA_character_, ifelse(is.null(df), 0, nrow(df))))
  }

  res <- rep(NA_character_, nrow(df))
  for (col in columns) {
    if (!col %in% names(df)) next
    val <- normalize_barcode(df[[col]])
    res <- dplyr::coalesce(res, val)
  }
  res
}

.mic_pick_first <- function(df, columns) {
  if (is.null(df) || !nrow(df) || !length(columns)) {
    return(rep(NA_character_, ifelse(is.null(df), 0, nrow(df))))
  }

  res <- rep(NA_character_, nrow(df))
  for (col in columns) {
    if (!col %in% names(df)) next
    val <- df[[col]]
    if (lubridate::is.Date(val)) {
      val <- as.character(val)
    }
    res <- dplyr::coalesce(res, as.character(val))
  }
  res
}

.mic_make_sample_key <- function(run_id, name, barcode_norm = NA_character_, lab_norm = NA_character_) {
  rid <- dplyr::coalesce(as.character(run_id), "")
  nm  <- dplyr::coalesce(as.character(name), "")
  bcn <- dplyr::coalesce(barcode_norm, "")
  lbn <- dplyr::coalesce(lab_norm, "")
  key <- paste(rid, nm, bcn, lbn, sep = "::")
  key[key == "::::"] <- NA_character_
  key
}

.mic_prepare_biobank_lookup <- function(biobank_df) {
  if (is.null(biobank_df) || !nrow(biobank_df)) {
    return(tibble())
  }

  df <- tibble::as_tibble(biobank_df)

  barcode_norm <- .mic_normalize_first(df, c("barcode", "code_barres_kps", "barcode_normalized", "sample_id", "SampleID"))
  lab_norm <- .mic_normalize_first(df, c("lab_id", "LabID", "numero", "record_number", "record_number_normalized"))

  study_vals <- .mic_pick_first(df, c("study", "biobank_study", "etude"))
  province_vals <- .mic_pick_first(df, c("province", "biobank_province"))
  zone_vals <- .mic_pick_first(df, c("health_zone", "biobank_health_zone", "zone_de_sante"))
  structure_vals <- .mic_pick_first(df, c("health_structure", "health_facility", "structure_sanitaire", "biobank_health_facility", "biobank_structure_sanitaire"))
  date_vals <- suppressWarnings(as.Date(.mic_pick_first(df, c("date_sample", "biobank_date_sample", "date_prelevement"))))
  barcode_raw <- .mic_pick_first(df, c("barcode", "code_barres_kps", "barcode_normalized", "sample_id"))
  lab_raw <- .mic_pick_first(df, c("lab_id", "LabID", "numero", "record_number"))

  by_barcode <- tibble(
    join_key = barcode_norm,
    join_type = "barcode",
    biobank_barcode = barcode_raw,
    biobank_lab_id = lab_raw,
    study = study_vals,
    province = province_vals,
    health_zone = zone_vals,
    structure = structure_vals,
    date_sample = date_vals
  ) %>%
    filter(!is.na(join_key)) %>%
    distinct(join_key, .keep_all = TRUE)

  by_lab <- tibble(
    join_key = lab_norm,
    join_type = "lab",
    biobank_barcode = barcode_raw,
    biobank_lab_id = lab_raw,
    study = study_vals,
    province = province_vals,
    health_zone = zone_vals,
    structure = structure_vals,
    date_sample = date_vals
  ) %>%
    filter(!is.na(join_key)) %>%
    distinct(join_key, .keep_all = TRUE)

  bind_rows(by_barcode, by_lab) %>%
    distinct(join_type, join_key, .keep_all = TRUE)
}

.mic_join_with_biobank <- function(samples_df, replic_df, biobank_df) {
  samples_df <- tibble::as_tibble(samples_df)
  replic_df <- tibble::as_tibble(replic_df)

  if (!nrow(samples_df)) {
    if (!"sample_key" %in% names(samples_df) && nrow(samples_df)) {
      samples_df$sample_key <- .mic_make_sample_key(samples_df$run_id, samples_df$Name)
    }
    if (!"sample_key" %in% names(replic_df) && nrow(replic_df)) {
      replic_df$sample_key <- .mic_make_sample_key(replic_df$run_id, replic_df$Name)
    }
    return(list(samples = samples_df, replicates = replic_df, biobank_lookup = tibble()))
  }

  samples_df <- samples_df %>%
    mutate(
      sample_barcode_norm = .mic_normalize_first(cur_data_all(),
                                                 c("Barcode", "barcode", "SampleID", "Sample_Id", "Name")),
      sample_lab_norm = .mic_normalize_first(cur_data_all(),
                                             c("LabID", "lab_id", "Lab_Id", "numero", "record_number", "Name")),
      sample_key = .mic_make_sample_key(run_id, Name, sample_barcode_norm, sample_lab_norm)
    )

  replic_df <- replic_df %>%
    mutate(
      sample_barcode_norm = .mic_normalize_first(cur_data_all(),
                                                 c("Barcode", "barcode", "SampleID", "Name")),
      sample_lab_norm = .mic_normalize_first(cur_data_all(),
                                             c("LabID", "lab_id", "Lab_Id", "numero", "record_number", "Name")),
      sample_key = .mic_make_sample_key(run_id, Name, sample_barcode_norm, sample_lab_norm)
    )

  if (is.null(biobank_df) || !nrow(biobank_df)) {
    return(list(samples = samples_df, replicates = replic_df, biobank_lookup = tibble()))
  }

  lookup <- .mic_prepare_biobank_lookup(biobank_df)

  if (!nrow(lookup)) {
    return(list(samples = samples_df, replicates = replic_df, biobank_lookup = tibble()))
  }

  filter_keys <- unique(lookup$join_key)

  samples_df <- samples_df %>%
    mutate(
      keep_sample = dplyr::case_when(
        isTRUE(is_control) ~ TRUE,
        ( (!is.na(sample_barcode_norm) & sample_barcode_norm %in% filter_keys) |
            (!is.na(sample_lab_norm) & sample_lab_norm %in% filter_keys) ) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    filter(keep_sample) %>%
    select(-keep_sample)

  keep_keys <- unique(samples_df$sample_key)
  replic_df <- replic_df %>%
    filter(is.na(sample_key) | sample_key %in% keep_keys)

  meta_barcode <- lookup %>%
    filter(join_type == "barcode") %>%
    select(join_key, biobank_barcode, biobank_lab_id, study, province, health_zone, structure, date_sample) %>%
    distinct(join_key, .keep_all = TRUE)

  meta_lab <- lookup %>%
    filter(join_type == "lab") %>%
    select(join_key,
           biobank_barcode_lab = biobank_barcode,
           biobank_lab_id_lab = biobank_lab_id,
           study_lab = study,
           province_lab = province,
           health_zone_lab = health_zone,
           structure_lab = structure,
           date_sample_lab = date_sample) %>%
    distinct(join_key, .keep_all = TRUE)

  samples_df <- samples_df %>%
    left_join(meta_barcode, by = c("sample_barcode_norm" = "join_key")) %>%
    left_join(meta_lab, by = c("sample_lab_norm" = "join_key")) %>%
    mutate(
      biobank_barcode = dplyr::coalesce(biobank_barcode, biobank_barcode_lab),
      biobank_lab_id = dplyr::coalesce(biobank_lab_id, biobank_lab_id_lab),
      study = dplyr::coalesce(study, study_lab),
      province = dplyr::coalesce(province, province_lab),
      health_zone = dplyr::coalesce(health_zone, health_zone_lab),
      structure = dplyr::coalesce(structure, structure_lab),
      date_sample = dplyr::coalesce(date_sample, date_sample_lab)
    ) %>%
    select(-dplyr::ends_with("_lab"))

  list(samples = samples_df, replicates = replic_df, biobank_lookup = lookup)
}

.mic_range_diff <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  diff(range(x))
}

.mic_safe_mad <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 2) return(NA_real_)
  stats::mad(x, constant = 1, na.rm = TRUE)
}

.mic_compute_sample_metrics <- function(samples_df, replic_df, thresholds) {
  if (!nrow(samples_df)) return(samples_df)

  thresholds <- modifyList(.mic_default_thresholds(), thresholds %||% list())

  rep_metrics <- replic_df %>%
    mutate(sample_key = dplyr::coalesce(sample_key, .mic_make_sample_key(run_id, Name, sample_barcode_norm, sample_lab_norm))) %>%
    group_by(sample_key) %>%
    summarise(
      late_pos_flag = if ("Cq_177T" %in% names(.)) any(is.finite(Cq_177T) & Cq_177T >= thresholds$late_pos_min & Cq_177T <= thresholds$late_pos_max) else FALSE,
      replicate_spread_177T = if ("Cq_177T" %in% names(.)) .mic_range_diff(Cq_177T) else NA_real_,
      replicate_mad_177T = if ("Cq_177T" %in% names(.)) .mic_safe_mad(Cq_177T) else NA_real_,
      replicate_spread_18S2 = if ("Cq_18S2" %in% names(.)) .mic_range_diff(Cq_18S2) else NA_real_,
      replicate_mad_18S2 = if ("Cq_18S2" %in% names(.)) .mic_safe_mad(Cq_18S2) else NA_real_,
      mean_rnp_rna = if ("RNAseP_RNA_Cq" %in% names(.)) mean(RNAseP_RNA_Cq, na.rm = TRUE) else NA_real_,
      mean_rnp_dna = if ("RNAseP_DNA_Cq" %in% names(.)) mean(RNAseP_DNA_Cq, na.rm = TRUE) else NA_real_,
      min_cq_177 = if ("Cq_177T" %in% names(.)) suppressWarnings(min(Cq_177T, na.rm = TRUE)) else NA_real_,
      min_cq_ge35 = if ("Cq_177T" %in% names(.)) suppressWarnings(min(Cq_177T[Cq_177T >= thresholds$neg_cutoff], na.rm = TRUE)) else NA_real_,
      .groups = "drop"
    )

  rep_metrics$min_cq_177[is.infinite(rep_metrics$min_cq_177)] <- NA_real_
  rep_metrics$min_cq_ge35[is.infinite(rep_metrics$min_cq_ge35)] <- NA_real_

  samples_df <- samples_df %>%
    mutate(sample_key = dplyr::coalesce(sample_key, .mic_make_sample_key(run_id, Name, sample_barcode_norm, sample_lab_norm))) %>%
    left_join(rep_metrics, by = "sample_key") %>%
    mutate(
      Cq_gap_TNA = if (all(c("avg_18S2_Cq", "avg_177T_Cq") %in% names(cur_data_all()))) {
        as.numeric(avg_18S2_Cq) - as.numeric(avg_177T_Cq)
      } else NA_real_,
      RNP_delta = ifelse(is.finite(mean_rnp_dna) & is.finite(mean_rnp_rna), mean_rnp_dna - mean_rnp_rna, NA_real_),
      RNP_grade = dplyr::case_when(
        isTRUE(is_control) ~ NA_character_,
        is.na(mean_rnp_rna) & !is.na(mean_rnp_dna) ~ "No RNA",
        is.na(mean_rnp_rna) & is.na(mean_rnp_dna) ~ NA_character_,
        is.na(RNP_delta) ~ NA_character_,
        RNP_delta <= 1 ~ "Excellent",
        RNP_delta <= 2 ~ "Good",
        RNP_delta <= 3.5 ~ "Borderline",
        TRUE ~ "Poor"
      ),
      RNP_grade = dplyr::case_when(
        !isTRUE(is_control) & is.na(RNP_grade) & !is.na(mean_rnp_rna) & mean_rnp_rna >= thresholds$rnp_no_rna_cq ~ "No RNA",
        TRUE ~ RNP_grade
      ),
      missing_targets = rowSums(cbind(
        if ("avg_177T_Cq" %in% names(cur_data_all())) is.na(avg_177T_Cq) else FALSE,
        if ("avg_18S2_Cq" %in% names(cur_data_all())) is.na(avg_18S2_Cq) else FALSE
      ), na.rm = TRUE),
      call_basis = dplyr::case_when(
        isTRUE(is_control) ~ NA_character_,
        (!is.na(avg_177T_Cq) & !is.na(avg_18S2_Cq)) ~ "TNA",
        !is.na(avg_177T_Cq) ~ "DNA",
        !is.na(avg_18S2_Cq) ~ "RNA",
        TRUE ~ NA_character_
      ),
      call_confidence_value = dplyr::case_when(
        isTRUE(is_control) ~ NA_real_,
        final_category == "positive" ~ dplyr::coalesce(40 - min_cq_177, NA_real_),
        final_category %in% c("negative", "failed", "inconclusive") ~ dplyr::coalesce(min_cq_ge35 - thresholds$neg_cutoff, NA_real_),
        TRUE ~ dplyr::coalesce(40 - min_cq_177, min_cq_ge35 - thresholds$neg_cutoff)
      ),
      call_confidence = dplyr::case_when(
        is.na(call_confidence_value) ~ NA_character_,
        call_confidence_value >= thresholds$confidence_high ~ "High",
        call_confidence_value >= thresholds$confidence_medium ~ "Medium",
        TRUE ~ "Low"
      ),
      replicate_dispersion_flag = dplyr::coalesce(replicate_spread_177T > thresholds$replicate_spread_warn, FALSE) |
        dplyr::coalesce(replicate_spread_18S2 > thresholds$replicate_spread_warn, FALSE) |
        dplyr::coalesce(replicate_mad_177T > thresholds$replicate_mad_warn, FALSE) |
        dplyr::coalesce(replicate_mad_18S2 > thresholds$replicate_mad_warn, FALSE),
      late_pos_flag = dplyr::coalesce(late_pos_flag, FALSE),
      needs_reextract_auto = !isTRUE(is_control) & final_category %in% c("negative", "failed", "inconclusive") &
        RNP_grade %in% c("Poor", "No RNA"),
      needs_repeat_pcr_auto = !isTRUE(is_control) & (late_pos_flag | replicate_dispersion_flag),
      reason_invalid = dplyr::case_when(
        isTRUE(is_control) ~ NA_character_,
        missing_targets >= 2 ~ "No wells",
        missing_targets == 1 ~ "Missing target",
        late_pos_flag ~ "Late positive",
        replicate_dispersion_flag ~ "High replicate spread",
        RNP_grade %in% c("Poor", "No RNA") ~ "RNAseP high Cq",
        TRUE ~ NA_character_
      )
    )

  samples_df
}

.mic_compute_run_summary <- function(samples_df, replic_df, thresholds) {
  if (!nrow(samples_df)) return(tibble())

  thresholds <- modifyList(.mic_default_thresholds(), thresholds %||% list())

  samples_only <- samples_df %>% filter(!isTRUE(is_control))
  controls <- samples_df %>% filter(isTRUE(is_control))

  run_base <- samples_df %>% distinct(run_id)

  sample_summary <- samples_only %>%
    group_by(run_id) %>%
    summarise(
      total_samples = n(),
      samples_valid = sum(final_category %in% c("positive", "negative"), na.rm = TRUE),
      samples_late_pos = sum(late_pos_flag, na.rm = TRUE),
      samples_missing_targets = sum(missing_targets > 0, na.rm = TRUE),
      auto_reextract = sum(needs_reextract_auto, na.rm = TRUE),
      auto_repeat = sum(needs_repeat_pcr_auto, na.rm = TRUE),
      .groups = "drop"
    )

  control_counts <- controls %>%
    group_by(run_id, control_type) %>%
    summarise(
      control_total = n(),
      control_fail = sum(final_category == "control_fail", na.rm = TRUE),
      control_pass = sum(final_category == "control_ok", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = control_type,
      values_from = c(control_total, control_fail, control_pass),
      names_sep = "_",
      values_fill = 0
    )

  needed_control_cols <- c(
    "control_total_PC", "control_fail_PC", "control_pass_PC",
    "control_total_NTC", "control_fail_NTC", "control_pass_NTC"
  )
  missing_control_cols <- setdiff(needed_control_cols, names(control_counts))
  if (length(missing_control_cols)) {
    control_counts[missing_control_cols] <- 0L
  }

  ntc_reps <- replic_df %>% filter(control_type == "NTC") %>%
    mutate(ntc_min = suppressWarnings(pmin(Cq_177T, Cq_18S2, na.rm = TRUE)))

  ntc_summary <- ntc_reps %>%
    group_by(run_id) %>%
    summarise(
      ntc_fail = sum(ntc_min < thresholds$ntc_max, na.rm = TRUE),
      ntc_total = sum(!is.na(ntc_min)),
      min_ntc_cq = suppressWarnings(min(ntc_min, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      min_ntc_cq = dplyr::na_if(min_ntc_cq, Inf),
      ntc_contam_rate = dplyr::case_when(
        ntc_total > 0 ~ ntc_fail / ntc_total,
        TRUE ~ NA_real_
      )
    )

  pc_reps <- replic_df %>% filter(control_type == "PC", is.finite(Cq_177T))
  global_pc_mean <- if (nrow(pc_reps)) mean(pc_reps$Cq_177T, na.rm = TRUE) else NA_real_
  global_pc_sd <- if (nrow(pc_reps) > 1) stats::sd(pc_reps$Cq_177T, na.rm = TRUE) else NA_real_

  pc_summary <- pc_reps %>%
    group_by(run_id) %>%
    summarise(
      pc_cq_mean = mean(Cq_177T, na.rm = TRUE),
      pc_cq_sd = ifelse(sum(is.finite(Cq_177T)) > 1, stats::sd(Cq_177T, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    mutate(
      pc_z_drift = ifelse(!is.na(global_pc_mean) && !is.na(global_pc_sd) && global_pc_sd > 0,
                          (pc_cq_mean - global_pc_mean) / global_pc_sd,
                          NA_real_)
    )

  run_summary <- run_base %>%
    left_join(sample_summary, by = "run_id") %>%
    left_join(control_counts, by = "run_id") %>%
    left_join(ntc_summary, by = "run_id") %>%
    left_join(pc_summary, by = "run_id") %>%
    mutate(
      total_samples = total_samples %||% 0,
      samples_valid_pct = dplyr::case_when(total_samples > 0 ~ samples_valid / total_samples * 100,
                                           TRUE ~ NA_real_),
      samples_late_pos_pct = dplyr::case_when(total_samples > 0 ~ samples_late_pos / total_samples * 100,
                                              TRUE ~ NA_real_),
      samples_missing_targets_pct = dplyr::case_when(total_samples > 0 ~ samples_missing_targets / total_samples * 100,
                                                     TRUE ~ NA_real_),
      run_status_suggested = dplyr::case_when(
        dplyr::coalesce(ntc_fail, 0) > 0 ~ "INVALID",
        dplyr::coalesce(control_fail_PC, 0) > 0 ~ "REVIEW",
        dplyr::coalesce(samples_late_pos, 0) > 0 ~ "REVIEW",
        dplyr::coalesce(samples_missing_targets, 0) > 0 ~ "REVIEW",
        TRUE ~ "VALID"
      )
    ) %>%
    arrange(run_id)

  run_summary
}

.mic_compute_control_trends <- function(replic_df, thresholds) {
  thresholds <- modifyList(.mic_default_thresholds(), thresholds %||% list())

  if (!nrow(replic_df)) {
    return(list(pc = tibble(), ntc = tibble()))
  }

  pc_reps <- replic_df %>% filter(control_type == "PC", is.finite(Cq_177T))
  pc_trend <- tibble()
  if (nrow(pc_reps)) {
    global_mean <- mean(pc_reps$Cq_177T, na.rm = TRUE)
    global_sd <- if (nrow(pc_reps) > 1) stats::sd(pc_reps$Cq_177T, na.rm = TRUE) else NA_real_
    pc_trend <- pc_reps %>%
      group_by(run_id) %>%
      summarise(
        cq_mean = mean(Cq_177T, na.rm = TRUE),
        cq_sd = ifelse(sum(is.finite(Cq_177T)) > 1, stats::sd(Cq_177T, na.rm = TRUE), NA_real_),
        n = sum(is.finite(Cq_177T)),
        .groups = "drop"
      ) %>%
      arrange(run_id) %>%
      mutate(
        global_mean = global_mean,
        global_sd = global_sd,
        upper_2sd = global_mean + 2 * global_sd,
        lower_2sd = global_mean - 2 * global_sd
      )
  }

  ntc_reps <- replic_df %>% filter(control_type == "NTC") %>%
    mutate(ntc_min = suppressWarnings(pmin(Cq_177T, Cq_18S2, na.rm = TRUE)))

  ntc_trend <- tibble()
  if (nrow(ntc_reps)) {
    ntc_trend <- ntc_reps %>%
      group_by(run_id) %>%
      summarise(
        min_cq = suppressWarnings(min(ntc_min, na.rm = TRUE)),
        mean_cq = mean(ntc_min, na.rm = TRUE),
        n = sum(!is.na(ntc_min)),
        .groups = "drop"
      ) %>%
      arrange(run_id) %>%
      mutate(limit = thresholds$ntc_max)
  }

  list(pc = pc_trend, ntc = ntc_trend)
}

.mic_compute_anomalies <- function(samples_df) {
  if (!nrow(samples_df)) return(tibble())

  samples_df %>%
    filter(!isTRUE(is_control)) %>%
    mutate(
      anomaly_flags = purrr::map_chr(seq_len(n()), function(idx) {
        flags <- character()
        if (!is.na(late_pos_flag) && late_pos_flag) flags <- c(flags, "Late positive")
        if (!is.na(replicate_dispersion_flag) && replicate_dispersion_flag) flags <- c(flags, "High replicate spread")
        if (!is.na(call_basis) && call_basis %in% c("DNA", "RNA")) flags <- c(flags, "Discordant DNA/RNA")
        if (!is.na(RNP_grade) && RNP_grade %in% c("Poor", "No RNA")) flags <- c(flags, "Poor preservation")
        if (!is.na(missing_targets) && missing_targets > 0) flags <- c(flags, "Missing targets")
        if (!length(flags)) return(NA_character_)
        paste(unique(flags), collapse = ", ")
      })
    ) %>%
    filter(!is.na(anomaly_flags))
}

# ---------- Read all MIC files in a directory --------------------------------
# Requires your functions: extract_cq_values(), apply_interpretation(),
# summarize_by_replicate(), apply_trypanozoon_decision(), summarize_by_sample(),
# analyze_qpcr()
.mic_read_dir <- function(dirpath, verbose = TRUE) {
  if (!dir.exists(dirpath)) return(list(
    samples = tibble(), replicates = tibble(), files = character()
  ))
  files <- list.files(dirpath, pattern = "\\.xlsx?$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(list(samples = tibble(), replicates = tibble(), files = character()))
  
  all_samples <- list()
  all_reps    <- list()
  
  for (f in files) {
    if (verbose) message("• MIC file: ", basename(f))
    res <- try(analyze_qpcr(f, verbose = FALSE), silent = TRUE)
    if (inherits(res, "try-error")) {
      warning("Failed: ", basename(f), " — ", as.character(res))
      next
    }
    # Attach run id and file to keep provenance
    run_id <- tools::file_path_sans_ext(basename(f))
    ss <- res$sample_summary %>% mutate(run_id = run_id, file = basename(f))
    rd <- res$replicate_data %>% mutate(run_id = run_id, file = basename(f))
    all_samples[[length(all_samples)+1]] <- ss
    all_reps[[length(all_reps)+1]]      <- rd
  }
  
  samples <- dplyr::bind_rows(all_samples)
  replics <- dplyr::bind_rows(all_reps)
  
  # Robust control flags for combined view
  if (!"is_control" %in% names(samples)) {
    samples <- samples %>%
      mutate(is_control = .mic_is_control(Type, Name),
             control_type = .mic_control_type(Type, Name))
  } else if (!"control_type" %in% names(samples)) {
    samples <- samples %>% mutate(control_type = .mic_control_type(Type, Name))
  }
  
  if (!"is_control" %in% names(replics)) {
    replics <- replics %>%
      mutate(is_control = .mic_is_control(Type, Name),
             control_type = .mic_control_type(Type, Name))
  } else if (!"control_type" %in% names(replics)) {
    replics <- replics %>% mutate(control_type = .mic_control_type(Type, Name))
  }
  
  list(samples = samples, replicates = replics, files = files)
}

# ---------- KPI helpers -------------------------------------------------------
.kpi <- function(samples_df, run_summary = NULL, anomalies_df = NULL, files = NULL) {
  if (is.null(samples_df) || !nrow(samples_df)) {
    return(list(
      n_files = length(files %||% character()),
      n_runs = if (!is.null(run_summary)) nrow(run_summary) else 0,
      n_rows = 0,
      n_samples = 0,
      n_controls = 0,
      n_pos = 0,
      n_neg = 0,
      n_fail = 0,
      n_inconcl = 0,
      runs_valid = 0,
      runs_review = 0,
      runs_invalid = 0,
      pct_samples_valid = NA_real_,
      pct_samples_late_pos = NA_real_,
      pct_samples_missing = NA_real_,
      n_auto_reextract = 0,
      n_auto_repeat = 0,
      n_anomalies = if (!is.null(anomalies_df)) nrow(anomalies_df) else 0
    ))
  }

  tib <- samples_df
  samples_only <- tib %>% filter(!isTRUE(is_control))
  run_summary <- run_summary %||% tibble()
  anomalies_df <- anomalies_df %||% tibble()

  status_counts <- if (nrow(run_summary)) {
    run_summary %>% count(run_status_suggested, name = "n")
  } else {
    tibble(run_status_suggested = character(), n = integer())
  }

  list(
    n_files     = length(unique(c(files %||% character(), tib$file))),
    n_runs      = length(unique(tib$run_id)),
    n_rows      = nrow(tib),
    n_samples   = sum(!tib$is_control, na.rm = TRUE),
    n_controls  = sum(tib$is_control, na.rm = TRUE),
    n_pos       = sum(tib$final_category == "positive", na.rm = TRUE),
    n_neg       = sum(tib$final_category == "negative", na.rm = TRUE),
    n_fail      = sum(tib$final_category == "failed", na.rm = TRUE),
    n_inconcl   = sum(tib$final_category == "inconclusive", na.rm = TRUE),
    runs_valid  = status_counts %>% filter(run_status_suggested == "VALID") %>% pull(n) %||% 0,
    runs_review = status_counts %>% filter(run_status_suggested == "REVIEW") %>% pull(n) %||% 0,
    runs_invalid = status_counts %>% filter(run_status_suggested == "INVALID") %>% pull(n) %||% 0,
    pct_samples_valid = if (nrow(samples_only)) mean(samples_only$final_category %in% c("positive", "negative"), na.rm = TRUE) * 100 else NA_real_,
    pct_samples_late_pos = if (nrow(samples_only)) mean(coalesce(samples_only$late_pos_flag, FALSE), na.rm = TRUE) * 100 else NA_real_,
    pct_samples_missing = if (nrow(samples_only)) mean(coalesce(samples_only$missing_targets > 0, FALSE), na.rm = TRUE) * 100 else NA_real_,
    n_auto_reextract = sum(coalesce(samples_only$needs_reextract_auto, FALSE), na.rm = TRUE),
    n_auto_repeat = sum(coalesce(samples_only$needs_repeat_pcr_auto, FALSE), na.rm = TRUE),
    n_anomalies = nrow(anomalies_df)
  )
}

# ---------- QC Excel (multi-sheet) -------------------------------------------
.mic_export_qc <- function(processed, out_path) {
  samples_df <- processed$samples
  if (is.null(samples_df) || !nrow(samples_df)) stop("No data to export")

  replic_df <- processed$replicates %||% tibble()
  run_qc <- processed$run_summary %||% tibble()
  controls <- samples_df %>% filter(isTRUE(is_control)) %>%
    mutate(
      pass = final_category == "control_ok",
      reason = dplyr::coalesce(reason_invalid, final_decision)
    ) %>%
    select(run_id, file, Name, control_type, pass, final_decision, final_category, reason, dplyr::everything())
  samples <- samples_df %>% filter(!isTRUE(is_control))
  anomalies <- processed$anomalies %||% tibble()

  writexl::write_xlsx(
    list(
      "Run QC"          = run_qc,
      "Controls by Run" = controls,
      "Samples"         = samples,
      "Anomalies"       = anomalies,
      "Replicates Raw"  = replic_df
    ),
    path = out_path
  )
  out_path
}

# ----------------------------- UI --------------------------------------------
#' MIC-only PCR Module UI
#' @export
mod_mic_pcr_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "PCR (05) — MIC",
    icon = icon("dna"),
    div(class = "container-fluid",
        
        # ==== KPIs ===========================================================
        h4(class = "mb-3", icon("dna"), " MIC qPCR Quality Control"),
        
        layout_column_wrap(
          width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          value_box(title = "Files",       value = textOutput(ns("kpi_files")),       showcase = icon("folder-open"),     theme = "primary"),
          value_box(title = "Runs",        value = textOutput(ns("kpi_runs")),        showcase = icon("clipboard-list"),  theme = "secondary"),
          value_box(title = "Samples",     value = textOutput(ns("kpi_samples")),     showcase = icon("vial"),            theme = "success"),
          value_box(title = "Controls",    value = textOutput(ns("kpi_controls")),    showcase = icon("flask"),           theme = "warning"),
          value_box(title = "Run Status",  value = textOutput(ns("kpi_run_status")), showcase = icon("traffic-light"),   theme = "success"),
          value_box(title = "Anomalies",   value = textOutput(ns("kpi_anomalies")),  showcase = icon("exclamation-triangle"), theme = "danger")
        ),

        layout_column_wrap(
          width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          value_box(title = "Pos / Neg",        value = textOutput(ns("kpi_posneg")),        showcase = icon("check-circle"), theme = "info"),
          value_box(title = "% Valid Samples", value = textOutput(ns("kpi_pct_valid")),     showcase = icon("thumbs-up"),    theme = "success"),
          value_box(title = "% Late Positives",value = textOutput(ns("kpi_pct_late")),      showcase = icon("clock"),        theme = "warning"),
          value_box(title = "% Missing Targets",value = textOutput(ns("kpi_pct_missing")), showcase = icon("bullseye"),     theme = "danger"),
          value_box(title = "Auto Re-extract",  value = textOutput(ns("kpi_auto_reextract")), showcase = icon("recycle"), theme = "secondary"),
          value_box(title = "Auto Repeat PCR",  value = textOutput(ns("kpi_auto_repeat")),   showcase = icon("redo"),      theme = "secondary")
        ),
        
        # ==== Controls ========================================================
        card(
          card_header(class = "h5 mb-0", "Directory & Export"),
          card_body(
            layout_columns(
              col_widths = c(6,3,3),
              textInput(ns("mic_dir"), "MIC folder", value = "data/MIC", placeholder = "path/to/MIC/xlsx"),
              actionButton(ns("reload"), "Reload MIC files", class = "btn-primary"),
              actionButton(ns("export"), "Export QC Workbook", class = "btn-success")
            ),
            helpText("Looks for *.xlsx in the folder. Each file is treated as one run.")
          )
        ),
        
        # ==== Tables ==========================================================
        card(
          full_screen = TRUE,
          card_header(class = "d-flex justify-content-between align-items-center",
                      span(class = "h5 mb-0", icon("vial"), " Samples"),
                      span(class = "text-muted small", "Rows = per sample (final summary)")),
          card_body(DTOutput(ns("tbl_samples")))
        ),
        
        card(
          card_header(class = "h5 mb-0", icon("flask"), " Controls by Run"),
          card_body(DTOutput(ns("tbl_controls")))
        ),

        card(
          card_header(class = "h5 mb-0", icon("triangle-exclamation"), " Anomalies"),
          card_body(DTOutput(ns("tbl_anomalies")))
        ),

        # ==== Plots ===========================================================
        h4(class = "mt-4 mb-3", icon("chart-line"), " Run QC & Trends"),

        layout_columns(
          col_widths = c(6,6), gap = "16px",
          card(card_header("Levey–Jennings — PC 177T"), card_body_fill(plotly::plotlyOutput(ns("plot_lj_pc"), height = "320px"))),
          card(card_header("Levey–Jennings — NTC"),     card_body_fill(plotly::plotlyOutput(ns("plot_lj_ntc"), height = "320px")))
        ),

        layout_columns(
          col_widths = c(6,6), gap = "16px",
          card(card_header("Run Quality Overview"),    card_body_fill(plotly::plotlyOutput(ns("plot_run_overview"), height = "320px"))),
          card(card_header("Cq Gap (18S2 − 177T)"),    card_body_fill(plotly::plotlyOutput(ns("plot_gap_tna"), height = "320px")))
        ),

        layout_columns(
          col_widths = c(6,6), gap = "16px",
          card(card_header("RNP Δ per Run"),            card_body_fill(plotly::plotlyOutput(ns("plot_rnp_delta"), height = "320px"))),
          card(card_header("Scatter: 18S2 vs 177T (Samples)"), card_body_fill(plotly::plotlyOutput(ns("plot_scatter_tna"), height = "320px")))
        )
    )
  )
}

# ----------------------------- SERVER -----------------------------------------
#' MIC-only PCR Module Server
#' @param id shiny id
#' @param default_dir default MIC folder (string)
#' @export
mod_mic_pcr_server <- function(id, default_dir = "data/MIC", filtered_biobank = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(TRUE, {
      updateTextInput(session, "mic_dir", value = default_dir)
    }, once = TRUE)

    mic_raw <- reactiveVal(list(samples = tibble(), replicates = tibble(), files = character()))

    reload_now <- function() {
      d <- trimws(input$mic_dir %||% "")
      md <- .mic_read_dir(d, verbose = FALSE)
      mic_raw(md)
    }
    
    observeEvent(input$reload, ignoreInit = TRUE, {
      reload_now()
      showNotification("MIC directory reloaded.", type = "message")
    })
    # initial load
    observeEvent(input$mic_dir, {
      reload_now()
    }, ignoreInit = FALSE)

    biobank_filtered <- reactive({
      if (is.null(filtered_biobank)) {
        return(NULL)
      }
      tryCatch(filtered_biobank(), error = function(e) NULL)
    })

    mic_processed <- reactive({
      md <- mic_raw()
      samples <- md$samples %||% tibble()
      replics <- md$replicates %||% tibble()
      files <- md$files %||% character()
      thresholds <- .mic_default_thresholds()
      bb <- biobank_filtered()

      joined <- .mic_join_with_biobank(samples, replics, bb)
      enriched <- .mic_compute_sample_metrics(joined$samples, joined$replicates, thresholds)
      run_summary <- .mic_compute_run_summary(enriched, joined$replicates, thresholds)
      control_trends <- .mic_compute_control_trends(joined$replicates, thresholds)
      anomalies <- .mic_compute_anomalies(enriched)
      kpi <- .kpi(enriched, run_summary, anomalies, files)

      list(
        samples = enriched,
        replicates = joined$replicates,
        run_summary = run_summary,
        control_trends = control_trends,
        anomalies = anomalies,
        files = files,
        kpi = kpi,
        thresholds = thresholds
      )
    })

    # ====================== KPIs ============================================
    format_pct <- function(x) {
      if (is.na(x)) return("—")
      scales::percent(x / 100, accuracy = 0.1)
    }

    output$kpi_files <- renderText({
      mp <- mic_processed()
      scales::comma(mp$kpi$n_files)
    })
    output$kpi_runs <- renderText({
      mp <- mic_processed()
      scales::comma(mp$kpi$n_runs)
    })
    output$kpi_samples <- renderText({
      mp <- mic_processed()
      scales::comma(mp$kpi$n_samples)
    })
    output$kpi_controls <- renderText({
      mp <- mic_processed()
      scales::comma(mp$kpi$n_controls)
    })
    output$kpi_posneg <- renderText({
      mp <- mic_processed()
      paste0(scales::comma(mp$kpi$n_pos), " / ", scales::comma(mp$kpi$n_neg))
    })
    output$kpi_run_status <- renderText({
      mp <- mic_processed()
      sprintf("%s / %s / %s",
              scales::comma(mp$kpi$runs_valid),
              scales::comma(mp$kpi$runs_review),
              scales::comma(mp$kpi$runs_invalid))
    })
    output$kpi_anomalies <- renderText({
      mp <- mic_processed()
      scales::comma(mp$kpi$n_anomalies)
    })
    output$kpi_pct_valid <- renderText({
      mp <- mic_processed(); format_pct(mp$kpi$pct_samples_valid)
    })
    output$kpi_pct_late <- renderText({
      mp <- mic_processed(); format_pct(mp$kpi$pct_samples_late_pos)
    })
    output$kpi_pct_missing <- renderText({
      mp <- mic_processed(); format_pct(mp$kpi$pct_samples_missing)
    })
    output$kpi_auto_reextract <- renderText({
      mp <- mic_processed(); scales::comma(mp$kpi$n_auto_reextract)
    })
    output$kpi_auto_repeat <- renderText({
      mp <- mic_processed(); scales::comma(mp$kpi$n_auto_repeat)
    })
    
    # ====================== Tables ==========================================
    output$tbl_samples <- renderDT({
      mp <- mic_processed()
      df <- mp$samples %>% filter(!isTRUE(is_control))
      if (!nrow(df)) return(DT::datatable(tibble(note = "No sample rows.")))

      display <- df %>%
        mutate(
          date_sample = suppressWarnings(as.Date(date_sample)),
          across(any_of(c("avg_177T_Cq", "avg_18S2_Cq", "Cq_gap_TNA", "RNP_delta",
                          "replicate_spread_177T", "replicate_mad_177T",
                          "replicate_spread_18S2", "replicate_mad_18S2",
                          "avg_preservation_delta")), ~ round(as.numeric(.x), 2))
        ) %>%
        select(any_of(c(
          "biobank_barcode", "biobank_lab_id", "Name", "run_id", "file",
          "study", "province", "health_zone", "structure", "date_sample",
          "final_decision", "final_category", "quality_flag",
          "call_basis", "call_confidence", "Cq_gap_TNA", "RNP_delta",
          "RNP_grade", "late_pos_flag", "replicate_spread_177T",
          "replicate_mad_177T", "replicate_spread_18S2", "replicate_mad_18S2",
          "missing_targets", "needs_reextract_auto", "needs_repeat_pcr_auto",
          "reason_invalid", "avg_177T_Cq", "avg_18S2_Cq",
          "avg_preservation_delta", "n_replicates", "n_positive", "n_negative",
          "n_failed", "n_inconclusive"
        )))

      datatable(
        display,
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE,
                       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "final_category",
          backgroundColor = styleEqual(
            c("positive", "negative", "failed", "inconclusive"),
            c("#d4edda", "#e8f4fd", "#fdecea", "#fff3cd")
          )
        ) %>%
        formatStyle(
          "call_confidence",
          backgroundColor = styleEqual(
            c("High", "Medium", "Low"),
            c("#d4edda", "#fff3cd", "#f8d7da")
          )
        ) %>%
        formatStyle(
          "RNP_grade",
          backgroundColor = styleEqual(
            c("Excellent", "Good", "Borderline", "Poor", "No RNA"),
            c("#d4edda", "#d1ecf1", "#fff3cd", "#f8d7da", "#f5c6cb")
          )
        ) %>%
        formatStyle(
          "late_pos_flag",
          backgroundColor = styleEqual(c(TRUE), c("#fff3cd"))
        ) %>%
        formatStyle(
          "needs_reextract_auto",
          backgroundColor = styleEqual(c(TRUE), c("#fdebd0"))
        ) %>%
        formatStyle(
          "needs_repeat_pcr_auto",
          backgroundColor = styleEqual(c(TRUE), c("#d6eaff"))
        )
    }, server = TRUE)

    output$tbl_controls <- renderDT({
      mp <- mic_processed()
      ctrl <- mp$samples %>%
        filter(isTRUE(is_control)) %>%
        mutate(
          pass = final_category == "control_ok",
          reason = dplyr::coalesce(reason_invalid, final_decision)
        ) %>%
        mutate(across(any_of(c("avg_177T_Cq", "avg_18S2_Cq", "avg_preservation_delta")), ~ round(as.numeric(.x), 2))) %>%
        select(any_of(c("run_id", "file", "Name", "control_type", "pass",
                        "final_decision", "final_category", "reason",
                        "avg_177T_Cq", "avg_18S2_Cq", "avg_preservation_delta"))) %>%
        arrange(run_id, control_type, Name)
      if (!nrow(ctrl)) return(DT::datatable(tibble(note = "No control rows.")))

      datatable(
        ctrl,
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE,
                       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "pass",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#fdecea"))
        )
    }, server = TRUE)

    output$tbl_anomalies <- renderDT({
      mp <- mic_processed()
      df <- mp$anomalies
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble(note = "No anomalies detected.")))
      }

      display <- df %>%
        mutate(
          date_sample = suppressWarnings(as.Date(date_sample)),
          across(any_of(c("avg_177T_Cq", "avg_18S2_Cq", "Cq_gap_TNA", "RNP_delta")), ~ round(as.numeric(.x), 2))
        ) %>%
        select(any_of(c(
          "biobank_barcode", "biobank_lab_id", "Name", "run_id", "file",
          "study", "province", "health_zone", "structure", "date_sample",
          "final_category", "call_basis", "call_confidence", "Cq_gap_TNA",
          "RNP_delta", "RNP_grade", "late_pos_flag", "replicate_spread_177T",
          "replicate_mad_177T", "missing_targets", "needs_reextract_auto",
          "needs_repeat_pcr_auto", "reason_invalid", "anomaly_flags"
        )))

      datatable(
        display,
        rownames = FALSE,
        options = list(pageLength = 25, scrollX = TRUE,
                       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "final_category",
          backgroundColor = styleEqual(
            c("positive", "negative", "failed", "inconclusive"),
            c("#d4edda", "#e8f4fd", "#fdecea", "#fff3cd")
          )
        ) %>%
        formatStyle(
          "anomaly_flags",
          backgroundColor = styleEqual(unique(display$anomaly_flags), rep("#fce4ec", length(unique(display$anomaly_flags))))
        )
    }, server = TRUE)

    # ====================== Plots ===========================================
    output$plot_lj_pc <- renderPlotly({
      mp <- mic_processed()
      df <- mp$control_trends$pc
      if (is.null(df) || !nrow(df)) {
        return(plotly_empty() %>% layout(title = list(text = "No PC control data")))
      }
      df <- df %>% arrange(run_id)
      gm <- df$global_mean[1]
      gsd <- df$global_sd[1]

      p <- plot_ly(
        df,
        x = ~run_id,
        y = ~cq_mean,
        type = "scatter",
        mode = "lines+markers",
        name = "PC Mean",
        text = ~paste0("Run: ", run_id,
                       "<br>Mean: ", round(cq_mean, 2),
                       "<br>SD: ", round(cq_sd %||% NA_real_, 2),
                       "<br>n: ", n),
        hoverinfo = "text"
      )

      if (!is.na(gm)) {
        runs <- df$run_id
        p <- p %>%
          add_lines(x = runs, y = rep(gm, length(runs)), name = "Mean", line = list(color = "#2c3e50", width = 1.5))
        if (!is.na(gsd) && gsd > 0) {
          p <- p %>%
            add_lines(x = runs, y = rep(gm + 2 * gsd, length(runs)), name = "+2 SD", line = list(color = "#e74c3c", dash = "dash")) %>%
            add_lines(x = runs, y = rep(gm - 2 * gsd, length(runs)), name = "-2 SD", line = list(color = "#e74c3c", dash = "dash"))
        }
      }

      p %>% layout(
        title = "PC 177T Controls",
        xaxis = list(title = "Run"),
        yaxis = list(title = "Mean Cq"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

    output$plot_lj_ntc <- renderPlotly({
      mp <- mic_processed()
      df <- mp$control_trends$ntc
      if (is.null(df) || !nrow(df)) {
        return(plotly_empty() %>% layout(title = list(text = "No NTC control data")))
      }
      df <- df %>% arrange(run_id)
      limit <- df$limit[1]

      p <- plot_ly(
        df,
        x = ~run_id,
        y = ~min_cq,
        type = "scatter",
        mode = "lines+markers",
        name = "Min Cq",
        text = ~paste0("Run: ", run_id,
                       "<br>Min Cq: ", round(min_cq, 2),
                       "<br>Mean Cq: ", round(mean_cq, 2),
                       "<br>n: ", n),
        hoverinfo = "text"
      )

      if (!is.na(limit)) {
        p <- p %>% add_lines(x = df$run_id, y = rep(limit, nrow(df)), name = "NTC max", line = list(color = "#e74c3c", dash = "dash"))
      }

      p %>% layout(
        title = "NTC Minimum Cq",
        xaxis = list(title = "Run"),
        yaxis = list(title = "Minimum Cq"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

    output$plot_run_overview <- renderPlotly({
      mp <- mic_processed()
      df <- mp$run_summary
      if (is.null(df) || !nrow(df)) {
        return(plotly_empty() %>% layout(title = list(text = "No run summaries available")))
      }

      df_long <- df %>%
        select(run_id, samples_valid_pct, samples_late_pos_pct, samples_missing_targets_pct) %>%
        tidyr::pivot_longer(
          cols = c(samples_valid_pct, samples_late_pos_pct, samples_missing_targets_pct),
          names_to = "metric",
          values_to = "value"
        ) %>%
        mutate(metric = factor(metric,
                               levels = c("samples_valid_pct", "samples_late_pos_pct", "samples_missing_targets_pct"),
                               labels = c("% Valid", "% Late", "% Missing")))

      plot_ly(df_long, x = ~run_id, y = ~value, color = ~metric, type = "bar") %>%
        layout(
          title = "% of Samples by Run",
          barmode = "group",
          xaxis = list(title = "Run"),
          yaxis = list(title = "% of samples", tickformat = ".1f"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$plot_gap_tna <- renderPlotly({
      mp <- mic_processed()
      df <- mp$samples %>% filter(!isTRUE(is_control), is.finite(Cq_gap_TNA))
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = list(text = "No Cq gap data")))
      }

      plot_ly(df, x = ~run_id, y = ~Cq_gap_TNA, color = ~call_basis, type = "box", boxpoints = "outliers",
              text = ~paste0("Sample: ", Name, "<br>Gap: ", round(Cq_gap_TNA, 2)), hoverinfo = "text") %>%
        layout(title = "Cq Gap (18S2 − 177T) by Run",
               xaxis = list(title = "Run"),
               yaxis = list(title = "Cq Gap"))
    })

    output$plot_rnp_delta <- renderPlotly({
      mp <- mic_processed()
      df <- mp$samples %>% filter(!isTRUE(is_control), is.finite(RNP_delta))
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = list(text = "No RNP data")))
      }

      plot_ly(df, x = ~run_id, y = ~RNP_delta, type = "scatter", mode = "markers",
              color = ~RNP_grade,
              text = ~paste0("Sample: ", Name,
                             "<br>RNP Δ: ", round(RNP_delta, 2),
                             "<br>Grade: ", RNP_grade),
              hoverinfo = "text") %>%
        layout(title = "RNP Δ by Run",
               xaxis = list(title = "Run"),
               yaxis = list(title = "RNP Δ (DNA − RNA)"))
    })

    output$plot_scatter_tna <- renderPlotly({
      mp <- mic_processed()
      smp <- mp$samples %>% filter(!isTRUE(is_control)) %>%
        mutate(Cq_177T = as.numeric(avg_177T_Cq), Cq_18S2 = as.numeric(avg_18S2_Cq)) %>%
        filter(is.finite(Cq_177T) | is.finite(Cq_18S2))
      if (!nrow(smp)) return(plotly_empty() %>% layout(title = "No sample data"))
      plot_ly(smp, x = ~Cq_177T, y = ~Cq_18S2, type = "scatter", mode = "markers",
              color = ~call_basis,
              text = ~paste0("Run: ", run_id,
                             "<br>Sample: ", Name,
                             "<br>Call: ", final_category,
                             "<br>Confidence: ", call_confidence),
              hoverinfo = "text") %>%
        layout(title = "18S2 vs 177T (lower Cq = stronger signal)",
               xaxis = list(title = "Cq 177T"),
               yaxis = list(title = "Cq 18S2"))
    })
    
    # ====================== Export ==========================================
    observeEvent(input$export, {
      mp <- mic_processed()
      req(nrow(mp$samples) > 0)
      dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
      path <- file.path("outputs", "MIC_PCR_QC.xlsx")
      .mic_export_qc(mp, path)
      showNotification(sprintf("QC workbook written: %s", path), type = "message", duration = 6)
    })
  })
}
