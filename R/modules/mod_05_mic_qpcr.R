# R/modules/mod_05_mic_qpcr.R
# =============================================================================
# Module 05 — MIC qPCR: BioMérieux MIC Analysis with Full QC
# =============================================================================
# Comprehensive qPCR analysis module for BioMérieux MIC Excel exports
# Features:
# - Multi-file batch processing with caching
# - User-adjustable interpretation thresholds
# - Complete run-level and sample-level QC
# - Levey-Jennings control monitoring
# - ΔCq analysis for concordance and preservation
# - Biobank/Extractions linkage
# - Comprehensive flagging and anomaly detection
# - Multi-sheet Excel export
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(DT)
  library(plotly)
  library(writexl)
  library(digest)
})

# ============================================================================
# UTILITIES
# ============================================================================

#' Normalize barcode for matching
normalize_barcode <- function(x) {
  if (is.null(x)) return(NA_character_)
  x %>%
    as.character() %>%
    str_trim() %>%
    str_to_upper() %>%
    str_replace_all("[^A-Z0-9]", "")
}

# ============================================================================
# CONFIGURATION & DEFAULTS
# ============================================================================

.MIC_DEFAULT_THRESHOLDS <- function() {
  list(
    # Target-specific cutoffs
    "177T" = list(positive = 35, negative = 40),
    "18S2" = list(positive = 35, negative = 40),
    "RNAseP_DNA" = list(positive = 32, negative = 999),
    "RNAseP_RNA" = list(positive = 30, negative = 999),
    
    # Late positive window
    late_pos_min = 35,
    late_pos_max = 40,
    
    # RNA preservation ΔCq thresholds
    preservation_delta_good = 5,
    preservation_delta_warn = 8,
    
    # Control limits
    pc_cq_min = 15,
    pc_cq_max = 30,
    ntc_cq_min = 38,
    
    # Quality flags
    replicate_cv_warn = 0.05,  # 5% CV
    replicate_range_warn = 2.0  # 2 Cq units
  )
}

# ============================================================================
# FILE SCANNING & CACHING
# ============================================================================

#' Scan MIC directory and check for changes
.mic_scan_directory <- function(path) {
  if (!dir.exists(path)) {
    return(list(files = character(), hashes = character()))
  }
  
  files <- list.files(path, pattern = "\\.xlsx?$", full.names = TRUE, ignore.case = TRUE)
  files <- files[!grepl("^~\\$", basename(files))]  # Exclude temp files
  
  if (length(files) == 0) {
    return(list(files = character(), hashes = character()))
  }
  
  # Calculate file hashes for change detection
  hashes <- sapply(files, function(f) {
    info <- file.info(f)
    digest::digest(paste(f, info$size, info$mtime), algo = "md5")
  })
  
  list(files = files, hashes = hashes)
}

#' Parse a single MIC file with error handling
.mic_parse_file <- function(filepath, thresholds, verbose = FALSE) {
  if (verbose) message(sprintf("  📄 %s", basename(filepath)))
  
  tryCatch({
    # Use the robust analyze_qpcr pipeline
    result <- analyze_qpcr(
      filepath,
      rnasep_rna_cutoff = thresholds$RNAseP_RNA$positive,
      cutoffs = thresholds[c("177T", "18S2", "RNAseP_DNA", "RNAseP_RNA")],
      verbose = FALSE
    )
    
    # Add file metadata
    run_id <- tools::file_path_sans_ext(basename(filepath))
    result$sample_summary$run_id <- run_id
    result$sample_summary$file <- basename(filepath)
    result$replicate_data$run_id <- run_id
    result$replicate_data$file <- basename(filepath)
    
    if (verbose) message(sprintf("    ✓ %d samples, %d replicates",
                                 nrow(result$sample_summary),
                                 nrow(result$replicate_data)))
    
    result
    
  }, error = function(e) {
    warning(sprintf("Failed to parse %s: %s", basename(filepath), e$message))
    NULL
  })
}

#' Read all MIC files with caching
.mic_read_directory <- function(path, thresholds, cache = NULL, verbose = FALSE) {
  scan <- .mic_scan_directory(path)
  
  if (length(scan$files) == 0) {
    return(list(
      samples = tibble(),
      replicates = tibble(),
      files = character(),
      cache = list()
    ))
  }
  
  if (verbose) message(sprintf("📂 Found %d MIC files", length(scan$files)))
  
  # Initialize cache if needed
  if (is.null(cache)) cache <- list()
  
  all_samples <- list()
  all_replicates <- list()
  new_cache <- list()
  
  for (i in seq_along(scan$files)) {
    filepath <- scan$files[i]
    filehash <- scan$hashes[i]
    
    # Check cache
    if (!is.null(cache[[filehash]])) {
      if (verbose) message(sprintf("  💾 %s (cached)", basename(filepath)))
      result <- cache[[filehash]]
    } else {
      result <- .mic_parse_file(filepath, thresholds, verbose)
      if (!is.null(result)) {
        new_cache[[filehash]] <- result
      }
    }
    
    if (!is.null(result)) {
      all_samples[[length(all_samples) + 1]] <- result$sample_summary
      all_replicates[[length(all_replicates) + 1]] <- result$replicate_data
    }
  }
  
  list(
    samples = bind_rows(all_samples),
    replicates = bind_rows(all_replicates),
    files = scan$files,
    cache = new_cache
  )
}

# ============================================================================
# BIOBANK LINKAGE
# ============================================================================

#' Link MIC samples to biobank records by numero
.mic_link_biobank <- function(samples_df, biobank_df) {
  if (is.null(biobank_df) || !nrow(biobank_df) || !nrow(samples_df)) {
    samples_df$biobank_matched <- FALSE
    samples_df$biobank_match_type <- NA_character_
    return(samples_df)
  }
  
  # Normalize sample identifiers (these are numero, not barcodes)
  samples_df <- samples_df %>%
    mutate(sample_norm = normalize_barcode(Name))
  
  # Check what columns exist
  biobank_cols <- names(biobank_df)
  
  # Prepare biobank lookup with normalized numero
  biobank_lookup <- biobank_df
  
  # Add numero_norm
  if ("numero" %in% biobank_cols) {
    biobank_lookup$numero_raw <- as.character(biobank_lookup$numero)
  } else if ("lab_id" %in% biobank_cols) {
    biobank_lookup$numero_raw <- as.character(biobank_lookup$lab_id)
  } else {
    biobank_lookup$numero_raw <- NA_character_
  }
  
  biobank_lookup$numero_norm <- normalize_barcode(biobank_lookup$numero_raw)
  
  # Add barcode (French column name)
  if ("code_barres_kps" %in% biobank_cols) {
    biobank_lookup$barcode_raw <- biobank_lookup$code_barres_kps
  } else if ("barcode" %in% biobank_cols) {
    biobank_lookup$barcode_raw <- biobank_lookup$barcode
  } else {
    biobank_lookup$barcode_raw <- NA_character_
  }
  
  # Filter to valid numero
  biobank_lookup <- biobank_lookup %>%
    filter(!is.na(numero_norm))
  
  # Create standardized output columns
  biobank_lookup <- biobank_lookup %>%
    mutate(
      biobank_numero = numero_raw,
      biobank_barcode = barcode_raw
    )
  
  # Add optional columns if they exist (French names)
  if ("province" %in% biobank_cols) {
    biobank_lookup$biobank_province <- biobank_lookup$province
  } else {
    biobank_lookup$biobank_province <- NA_character_
  }
  
  if ("zone_de_sante" %in% biobank_cols) {
    biobank_lookup$biobank_health_zone <- biobank_lookup$zone_de_sante
  } else if ("health_zone" %in% biobank_cols) {
    biobank_lookup$biobank_health_zone <- biobank_lookup$health_zone
  } else {
    biobank_lookup$biobank_health_zone <- NA_character_
  }
  
  if ("structure_sanitaire" %in% biobank_cols) {
    biobank_lookup$biobank_structure <- biobank_lookup$structure_sanitaire
  } else if ("centre_de_sante" %in% biobank_cols) {
    biobank_lookup$biobank_structure <- biobank_lookup$centre_de_sante
  } else if ("health_structure" %in% biobank_cols) {
    biobank_lookup$biobank_structure <- biobank_lookup$health_structure
  } else if ("health_facility" %in% biobank_cols) {
    biobank_lookup$biobank_structure <- biobank_lookup$health_facility
  } else {
    biobank_lookup$biobank_structure <- NA_character_
  }
  
  if ("etude" %in% biobank_cols) {
    biobank_lookup$biobank_study <- biobank_lookup$etude
  } else if ("study" %in% biobank_cols) {
    biobank_lookup$biobank_study <- biobank_lookup$study
  } else {
    biobank_lookup$biobank_study <- NA_character_
  }
  
  if ("date_de_prelevement" %in% biobank_cols) {
    biobank_lookup$biobank_date_sample <- as.Date(biobank_lookup$date_de_prelevement)
  } else if ("date_sample" %in% biobank_cols) {
    biobank_lookup$biobank_date_sample <- as.Date(biobank_lookup$date_sample)
  } else {
    biobank_lookup$biobank_date_sample <- as.Date(NA)
  }
  
  # Select only the columns we need
  biobank_lookup <- biobank_lookup %>%
    select(numero_norm, biobank_numero, biobank_barcode, biobank_province, 
           biobank_health_zone, biobank_structure, biobank_study, biobank_date_sample) %>%
    distinct(numero_norm, .keep_all = TRUE)
  
  # Join by normalized numero
  samples_df <- samples_df %>%
    left_join(biobank_lookup, by = c("sample_norm" = "numero_norm")) %>%
    mutate(
      biobank_matched = !is.na(biobank_numero),
      biobank_match_type = if_else(biobank_matched, "numero", NA_character_)
    )
  
  samples_df
}

# ============================================================================
# RUN-LEVEL QC ANALYSIS
# ============================================================================

#' Calculate run-level QC metrics
.mic_calculate_run_qc <- function(samples_df, replicates_df, thresholds) {
  if (!nrow(samples_df)) {
    return(tibble(
      run_id = character(),
      file = character(),
      n_total = integer(),
      n_samples = integer(),
      n_controls = integer(),
      n_positive = integer(),
      n_negative = integer(),
      n_failed = integer(),
      pct_valid = numeric(),
      controls_pass = logical(),
      run_valid = logical()
    ))
  }
  
  # Per-run sample counts
  run_summary <- samples_df %>%
    group_by(run_id, file) %>%
    summarise(
      n_total = n(),
      n_samples = sum(!is_control),
      n_controls = sum(is_control),
      n_positive = sum(final_category == "positive" & !is_control, na.rm = TRUE),
      n_negative = sum(final_category == "negative" & !is_control, na.rm = TRUE),
      n_failed = sum(final_category %in% c("failed", "inconclusive") & !is_control, na.rm = TRUE),
      n_control_ok = sum(final_category == "control_ok", na.rm = TRUE),
      n_control_fail = sum(final_category == "control_fail", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_valid = if_else(n_samples > 0, 
                          (n_positive + n_negative) / n_samples * 100, 
                          NA_real_),
      controls_pass = n_control_fail == 0 & n_control_ok > 0,
      run_valid = controls_pass & (pct_valid >= 80 | is.na(pct_valid))
    )
  
  run_summary
}

# ============================================================================
# CONTROL TREND ANALYSIS (LEVEY-JENNINGS)
# ============================================================================

#' Calculate Levey-Jennings statistics for controls
.mic_calculate_lj_stats <- function(replicates_df, target, control_type = "Positive") {
  if (!nrow(replicates_df)) {
    return(list(
      data = tibble(),
      mean = NA_real_,
      sd = NA_real_,
      n = 0
    ))
  }
  
  # Filter to controls and target
  ctrl_data <- replicates_df %>%
    filter(
      Type %in% c(control_type, paste(control_type, "Control")),
      !is.na(!!sym(paste0("Cq_", target))),
      is.finite(!!sym(paste0("Cq_", target)))
    )
  
  if (!nrow(ctrl_data)) {
    return(list(
      data = tibble(),
      mean = NA_real_,
      sd = NA_real_,
      n = 0
    ))
  }
  
  # Calculate per-run statistics
  run_stats <- ctrl_data %>%
    group_by(run_id) %>%
    summarise(
      mean_cq = mean(!!sym(paste0("Cq_", target)), na.rm = TRUE),
      sd_cq = sd(!!sym(paste0("Cq_", target)), na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Overall statistics
  overall_mean <- mean(ctrl_data[[paste0("Cq_", target)]], na.rm = TRUE)
  overall_sd <- sd(ctrl_data[[paste0("Cq_", target)]], na.rm = TRUE)
  
  # Add control limits
  run_stats <- run_stats %>%
    mutate(
      center = overall_mean,
      ucl_1sd = overall_mean + overall_sd,
      lcl_1sd = overall_mean - overall_sd,
      ucl_2sd = overall_mean + 2 * overall_sd,
      lcl_2sd = overall_mean - 2 * overall_sd,
      ucl_3sd = overall_mean + 3 * overall_sd,
      lcl_3sd = overall_mean - 3 * overall_sd,
      out_of_control = mean_cq > ucl_2sd | mean_cq < lcl_2sd
    )
  
  list(
    data = run_stats,
    mean = overall_mean,
    sd = overall_sd,
    n = nrow(ctrl_data)
  )
}

# ============================================================================
# ΔCq ANALYSIS
# ============================================================================

#' Calculate ΔCq metrics for concordance analysis
.mic_calculate_delta_cq <- function(samples_df) {
  if (!nrow(samples_df)) return(samples_df)
  
  samples_df %>%
    mutate(
      # TNA concordance: 18S2 - 177T (should be small if both positive)
      delta_cq_tna = case_when(
        !is.na(avg_18S2_Cq) & !is.na(avg_177T_Cq) ~ avg_18S2_Cq - avg_177T_Cq,
        TRUE ~ NA_real_
      ),
      
      # RNA preservation delta (already calculated by pipeline)
      delta_cq_rnp = avg_preservation_delta,
      
      # Flag discordant results
      flag_discordant_tna = case_when(
        is.na(delta_cq_tna) ~ FALSE,
        abs(delta_cq_tna) > 5 ~ TRUE,  # More than 5 Cq difference
        TRUE ~ FALSE
      ),
      
      flag_poor_preservation = rna_preservation %in% 
        c("Poor RNA preservation", "Severe RNA loss (no RNAseP-RNA)")
    )
}

# ============================================================================
# ANOMALY DETECTION & FLAGGING
# ============================================================================

#' Detect anomalies and generate flags
.mic_detect_anomalies <- function(samples_df, replicates_df, thresholds) {
  if (!nrow(samples_df)) return(samples_df)
  
  # Calculate replicate variability per sample
  rep_stats <- replicates_df %>%
    filter(!is_control) %>%
    group_by(run_id, Name) %>%
    summarise(
      across(
        c(Cq_177T, Cq_18S2),
        list(
          range = ~ifelse(sum(!is.na(.)) > 1, diff(range(., na.rm = TRUE)), NA_real_),
          cv = ~ifelse(sum(!is.na(.)) > 1 & mean(., na.rm = TRUE) > 0,
                       sd(., na.rm = TRUE) / mean(., na.rm = TRUE), NA_real_)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )
  
  # Join and flag
  samples_df %>%
    left_join(rep_stats, by = c("run_id", "Name")) %>%
    mutate(
      # Replicate variability flags
      flag_high_cv_177T = coalesce(Cq_177T_cv > thresholds$replicate_cv_warn, FALSE),
      flag_high_cv_18S2 = coalesce(Cq_18S2_cv > thresholds$replicate_cv_warn, FALSE),
      flag_high_range_177T = coalesce(Cq_177T_range > thresholds$replicate_range_warn, FALSE),
      flag_high_range_18S2 = coalesce(Cq_18S2_range > thresholds$replicate_range_warn, FALSE),
      
      # Late positive flag (already from pipeline but refine)
      flag_late_positive = case_when(
        !is.na(avg_177T_Cq) & avg_177T_Cq >= thresholds$late_pos_min & 
          avg_177T_Cq <= thresholds$late_pos_max ~ TRUE,
        !is.na(avg_18S2_Cq) & avg_18S2_Cq >= thresholds$late_pos_min & 
          avg_18S2_Cq <= thresholds$late_pos_max ~ TRUE,
        TRUE ~ FALSE
      ),
      
      # Aggregate flags
      any_flag = flag_high_cv_177T | flag_high_cv_18S2 | 
        flag_high_range_177T | flag_high_range_18S2 |
        flag_late_positive | flag_discordant_tna | flag_poor_preservation,
      
      flag_summary = case_when(
        !any_flag ~ NA_character_,
        TRUE ~ paste(
          if_else(flag_high_cv_177T | flag_high_cv_18S2, "High CV", NA_character_),
          if_else(flag_high_range_177T | flag_high_range_18S2, "High range", NA_character_),
          if_else(flag_late_positive, "Late positive", NA_character_),
          if_else(flag_discordant_tna, "Discordant TNA", NA_character_),
          if_else(flag_poor_preservation, "Poor preservation", NA_character_),
          sep = "; "
        ) %>% str_remove_all("NA; |; NA") %>% na_if("")
      )
    )
}

# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

#' Export comprehensive QC workbook
.mic_export_qc_workbook <- function(processed_data, output_path) {
  sheets <- list(
    "Run QC" = processed_data$run_qc %>%
      select(run_id, file, n_total, n_samples, n_controls,
             n_positive, n_negative, n_failed, pct_valid,
             controls_pass, run_valid),
    
    "Sample Summary" = processed_data$samples %>%
      filter(!is_control) %>%
      select(run_id, Name, biobank_matched, biobank_match_type,
             final_decision, final_category, quality_flag,
             avg_177T_Cq, avg_18S2_Cq,
             delta_cq_tna, delta_cq_rnp, rna_preservation,
             flag_summary, any_flag),
    
    "Controls" = processed_data$samples %>%
      filter(is_control) %>%
      select(run_id, Name, Type, final_decision, final_category,
             avg_177T_Cq, avg_18S2_Cq),
    
    "Flagged Samples" = processed_data$samples %>%
      filter(any_flag) %>%
      select(run_id, Name, final_decision, flag_summary,
             avg_177T_Cq, avg_18S2_Cq, delta_cq_tna, delta_cq_rnp),
    
    "L-J Stats 177T" = processed_data$lj_177T$data,
    "L-J Stats 18S2" = processed_data$lj_18S2$data,
    
    "All Replicates" = processed_data$replicates %>%
      select(run_id, Name, Replicate, is_control, Type,
             Cq_177T, Cq_18S2, Cq_RNAseP_DNA, Cq_RNAseP_RNA)
  )
  
  writexl::write_xlsx(sheets, output_path)
  output_path
}

# ============================================================================
# MODULE UI
# ============================================================================

#' MIC qPCR Module UI
#' @export
mod_mic_qpcr_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "MIC qPCR",
    icon = icon("dna"),
    
    div(class = "container-fluid",
        
        # === HEADER & CONTROLS ===============================================
        card(
          card_header(class = "d-flex justify-content-between align-items-center",
                      span(class = "h4 mb-0", icon("dna"), " MIC qPCR Quality Control"),
                      div(
                        actionButton(ns("refresh"), "Refresh Files", 
                                     icon = icon("sync"), class = "btn-primary me-2"),
                        actionButton(ns("export_qc"), "Export QC Report", 
                                     icon = icon("file-excel"), class = "btn-success")
                      )),
          card_body(
            layout_columns(
              col_widths = c(6, 6), gap = "16px",
              
              # Directory input
              div(
                textInput(ns("mic_dir"), "MIC Directory", 
                          value = "data/MIC",
                          placeholder = "path/to/MIC/files"),
                helpText("Directory containing BioMérieux MIC Excel exports (.xlsx)")
              ),
              
              # Threshold panel toggle
              div(
                checkboxInput(ns("show_thresholds"), "Show Threshold Settings", FALSE),
                helpText("Adjust interpretation thresholds for targets and controls")
              )
            ),
            
            # Collapsible threshold panel
            conditionalPanel(
              condition = "input.show_thresholds",
              ns = ns,
              card(
                card_header("Interpretation Thresholds"),
                card_body(
                  layout_columns(
                    col_widths = c(3, 3, 3, 3), gap = "12px",
                    
                    # 177T thresholds
                    div(
                      h6(icon("dna"), " 177T (DNA)"),
                      numericInput(ns("th_177t_pos"), "Positive ≤", 35, min = 0, max = 50, step = 0.5),
                      numericInput(ns("th_177t_neg"), "Negative >", 40, min = 0, max = 50, step = 0.5)
                    ),
                    
                    # 18S2 thresholds
                    div(
                      h6(icon("dna"), " 18S2 (RNA)"),
                      numericInput(ns("th_18s2_pos"), "Positive ≤", 35, min = 0, max = 50, step = 0.5),
                      numericInput(ns("th_18s2_neg"), "Negative >", 40, min = 0, max = 50, step = 0.5)
                    ),
                    
                    # RNAseP thresholds
                    div(
                      h6(icon("shield"), " RNAseP"),
                      numericInput(ns("th_rnp_dna_pos"), "DNA ≤", 32, min = 0, max = 50, step = 0.5),
                      numericInput(ns("th_rnp_rna_pos"), "RNA ≤", 30, min = 0, max = 50, step = 0.5)
                    ),
                    
                    # Quality thresholds
                    div(
                      h6(icon("flag"), " Quality"),
                      numericInput(ns("th_late_min"), "Late pos min", 35, min = 0, max = 50, step = 0.5),
                      numericInput(ns("th_pres_good"), "Preservation good ≤", 5, min = 0, max = 20, step = 0.5)
                    )
                  )
                )
              )
            )
          )
        ),
        
        # === KPI STRIP =======================================================
        layout_column_wrap(
          width = 1/8, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          
          value_box(title = "Files Read", value = textOutput(ns("kpi_files")),
                    showcase = icon("folder-open"), theme = "primary"),
          value_box(title = "Total Samples", value = textOutput(ns("kpi_samples_total")),
                    showcase = icon("vial"), theme = "secondary"),
          value_box(title = "Linked to Biobank", value = textOutput(ns("kpi_linked")),
                    showcase = icon("link"), theme = "info"),
          value_box(title = "Linked to Extractions", value = textOutput(ns("kpi_linked_ext")),
                    showcase = icon("flask"), theme = "info"),
          value_box(title = "Valid Runs", value = textOutput(ns("kpi_runs_valid")),
                    showcase = icon("check-circle"), theme = "success"),
          value_box(title = "Positives", value = textOutput(ns("kpi_positive")),
                    showcase = icon("plus-circle"), theme = "success"),
          value_box(title = "Negatives", value = textOutput(ns("kpi_negative")),
                    showcase = icon("minus-circle"), theme = "primary"),
          value_box(title = "Flagged", value = textOutput(ns("kpi_flagged")),
                    showcase = icon("flag"), theme = "warning")
        ),
        
        # === TABSET ==========================================================
        navset_card_tab(
          
          # --- TAB: RUNS -----------------------------------------------------
          nav_panel(
            "Runs",
            icon = icon("clipboard-list"),
            card_body(
              DTOutput(ns("tbl_runs"))
            )
          ),
          
          # --- TAB: SAMPLES --------------------------------------------------
          nav_panel(
            "Samples",
            icon = icon("vial"),
            card_body(
              DTOutput(ns("tbl_samples"))
            )
          ),
          
          # --- TAB: CONTROLS & L-J -------------------------------------------
          nav_panel(
            "Controls & L-J",
            icon = icon("flask"),
            
            layout_columns(
              col_widths = c(12), gap = "16px",
              
              card(
                card_header("Control Results Summary"),
                card_body(DTOutput(ns("tbl_controls")))
              ),
              
              card(
                card_header("Levey-Jennings: 177T Positive Controls"),
                card_body_fill(plotly::plotlyOutput(ns("plot_lj_177t"), height = "400px"))
              ),
              
              card(
                card_header("Levey-Jennings: 18S2 Positive Controls"),
                card_body_fill(plotly::plotlyOutput(ns("plot_lj_18s2"), height = "400px"))
              )
            )
          ),
          
          # --- TAB: QC SCATTER -----------------------------------------------
          nav_panel(
            "QC Scatter",
            icon = icon("chart-scatter"),
            
            layout_columns(
              col_widths = c(6, 6), gap = "16px",
              
              card(
                card_header("18S2 vs 177T (TNA Concordance)"),
                card_body_fill(plotly::plotlyOutput(ns("plot_scatter_tna"), height = "500px"))
              ),
              
              card(
                card_header("RNAseP: RNA vs DNA (Preservation)"),
                card_body_fill(plotly::plotlyOutput(ns("plot_scatter_rnp"), height = "500px"))
              )
            )
          ),
          
          # --- TAB: FLAGS ----------------------------------------------------
          nav_panel(
            "Flags",
            icon = icon("flag"),
            
            card_body(
              DTOutput(ns("tbl_flagged"))
            )
          ),
          
          # --- TAB: EXPORTS --------------------------------------------------
          nav_panel(
            "Exports",
            icon = icon("download"),
            
            layout_columns(
              col_widths = c(4, 4, 4), gap = "16px",
              
              card(
                card_header("Available Exports"),
                card_body(
                  p("Export comprehensive QC reports in Excel format:"),
                  tags$ul(
                    tags$li("Run metadata and QC status"),
                    tags$li("Sample results with linkage"),
                    tags$li("Control performance"),
                    tags$li("Flagged samples for review"),
                    tags$li("Levey-Jennings statistics"),
                    tags$li("All replicate data")
                  ),
                  downloadButton(ns("download_qc"), "Download QC Workbook", 
                                 class = "btn-primary w-100")
                )
              ),
              
              card(
                card_header("Run Metadata"),
                card_body(
                  downloadButton(ns("download_runs"), "Download Run Summary (CSV)", 
                                 class = "btn-secondary w-100 mb-2"),
                  helpText("Per-run QC metrics and validity status")
                )
              ),
              
              card(
                card_header("Sample Data"),
                card_body(
                  downloadButton(ns("download_samples"), "Download Sample Results (CSV)", 
                                 class = "btn-secondary w-100 mb-2"),
                  downloadButton(ns("download_flagged"), "Download Flagged Samples (CSV)", 
                                 class = "btn-warning w-100"),
                  helpText("All samples or filtered flagged samples")
                )
              )
            )
          )
        )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' MIC qPCR Module Server
#' @param biobank_df Reactive returning biobank data
#' @param extractions_df Reactive returning extractions data
#' @param filters Reactive returning filter list
#' @export
mod_mic_qpcr_server <- function(id, biobank_df = NULL, extractions_df = NULL, filters = NULL) {
  
  moduleServer(id, function(input, output, session) {
    
    # === REACTIVE VALUES ==================================================
    rv <- reactiveValues(
      cache = list(),
      last_scan = NULL
    )
    
    # === USER THRESHOLDS ==================================================
    user_thresholds <- reactive({
      list(
        "177T" = list(
          positive = input$th_177t_pos %||% 35,
          negative = input$th_177t_neg %||% 40
        ),
        "18S2" = list(
          positive = input$th_18s2_pos %||% 35,
          negative = input$th_18s2_neg %||% 40
        ),
        "RNAseP_DNA" = list(
          positive = input$th_rnp_dna_pos %||% 32,
          negative = 999
        ),
        "RNAseP_RNA" = list(
          positive = input$th_rnp_rna_pos %||% 30,
          negative = 999
        ),
        late_pos_min = input$th_late_min %||% 35,
        late_pos_max = input$th_late_min %||% 40,
        preservation_delta_good = input$th_pres_good %||% 5,
        preservation_delta_warn = 8,
        replicate_cv_warn = 0.05,
        replicate_range_warn = 2.0
      )
    })
    
    # === RAW DATA (FILE READING) ==========================================
    raw_data <- eventReactive(input$refresh, {
      req(input$mic_dir)
      
      showNotification("Reading MIC files...", id = "reading", duration = NULL, type = "message")
      
      result <- .mic_read_directory(
        input$mic_dir,
        thresholds = user_thresholds(),
        cache = rv$cache,
        verbose = TRUE
      )
      
      rv$cache <- result$cache
      rv$last_scan <- Sys.time()
      
      removeNotification("reading")
      showNotification(sprintf("Loaded %d files, %d samples", 
                               length(result$files), nrow(result$samples)),
                       duration = 3, type = "message")
      
      result
    }, ignoreNULL = FALSE)
    
    # Auto-load on startup
    observe({
      raw_data()
    })
    
    # === PROCESSED DATA (WITH LINKAGE & QC) ===============================
    # === PROCESSED DATA (WITH LINKAGE & QC) ===============================
    processed_data <- reactive({
      req(raw_data())
      rd <- raw_data()
      
      if (!nrow(rd$samples)) {
        return(list(
          samples = tibble(),
          replicates = tibble(),
          run_qc = tibble(),
          lj_177T = list(data = tibble(), mean = NA, sd = NA, n = 0),
          lj_18S2 = list(data = tibble(), mean = NA, sd = NA, n = 0)
        ))
      }
      
      # Link to biobank (by numero)
      samples <- rd$samples
      if (!is.null(biobank_df)) {
        bb <- biobank_df()
        if (!is.null(bb) && nrow(bb)) {
          samples <- .mic_link_biobank(samples, bb)
        } else {
          samples$biobank_matched <- FALSE
          samples$biobank_match_type <- NA_character_
        }
      } else {
        samples$biobank_matched <- FALSE
        samples$biobank_match_type <- NA_character_
      }
      
      # Link to extractions (via biobank barcode that we got from numero match)
      samples$extractions_matched <- FALSE
      if (!is.null(extractions_df) && "biobank_matched" %in% names(samples) && "biobank_barcode" %in% names(samples)) {
        ex <- extractions_df()
        if (!is.null(ex) && nrow(ex)) {
          # Get barcodes from extractions that have a numero match
          ex_cols <- names(ex)
          
          # Extractions can link by numero OR by barcode
          ex_lookup <- ex %>%
            mutate(
              # Try numero first
              numero_norm = if ("numero" %in% ex_cols) normalize_barcode(as.character(numero)) else NA_character_,
              # Then barcode
              barcode_norm = normalize_barcode(coalesce(
                if ("barcode" %in% ex_cols) barcode else NA_character_,
                if ("code_barres_kps" %in% ex_cols) code_barres_kps else NA_character_
              ))
            ) %>%
            filter(!is.na(numero_norm) | !is.na(barcode_norm))
          
          # Get unique identifiers from extractions
          ex_numeros <- unique(ex_lookup$numero_norm[!is.na(ex_lookup$numero_norm)])
          ex_barcodes <- unique(ex_lookup$barcode_norm[!is.na(ex_lookup$barcode_norm)])
          
          samples <- samples %>%
            mutate(
              extractions_matched = (
                # Match by numero (MIC sample name)
                sample_norm %in% ex_numeros |
                  # OR match by barcode (if we got one from biobank)
                  (!is.na(biobank_barcode) & normalize_barcode(biobank_barcode) %in% ex_barcodes)
              )
            )
        }
      }
      
      # Calculate ΔCq
      samples <- .mic_calculate_delta_cq(samples)
      
      # Detect anomalies
      samples <- .mic_detect_anomalies(samples, rd$replicates, user_thresholds())
      
      # Run-level QC
      run_qc <- .mic_calculate_run_qc(samples, rd$replicates, user_thresholds())
      
      # Levey-Jennings for controls
      lj_177T <- .mic_calculate_lj_stats(rd$replicates, "177T", "Positive")
      lj_18S2 <- .mic_calculate_lj_stats(rd$replicates, "18S2", "Positive")
      
      list(
        samples = samples,
        replicates = rd$replicates,
        run_qc = run_qc,
        lj_177T = lj_177T,
        lj_18S2 = lj_18S2,
        files = rd$files
      )
    })
    
    # === FILTERED DATA ====================================================
    filtered_samples <- reactive({
      pd <- processed_data()
      df <- pd$samples
      
      if (!nrow(df) || is.null(filters)) return(df)
      
      f <- filters()
      if (!is.list(f)) return(df)
      
      # Apply filters if present
      if (!is.null(f$date_range) && length(f$date_range) == 2 && "biobank_date_sample" %in% names(df)) {
        df <- df %>%
          filter(is.na(biobank_date_sample) | 
                   (biobank_date_sample >= f$date_range[1] & 
                      biobank_date_sample <= f$date_range[2]))
      }
      
      if (!is.null(f$province) && f$province != "all" && "biobank_province" %in% names(df)) {
        df <- df %>% filter(is.na(biobank_province) | biobank_province == f$province)
      }
      
      if (!is.null(f$zone) && f$zone != "all" && "biobank_health_zone" %in% names(df)) {
        df <- df %>% filter(is.na(biobank_health_zone) | biobank_health_zone == f$zone)
      }
      
      df
    })
    
    # === KPI OUTPUTS ======================================================
    output$kpi_files <- renderText({
      pd <- processed_data()
      if (is.null(pd) || length(pd$files) == 0) return("0")
      scales::comma(length(pd$files))
    })
    
    output$kpi_samples_total <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(!df$is_control, na.rm = TRUE))
    })
    
    output$kpi_linked <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0 (0%)")
      n <- sum(df$biobank_matched & !df$is_control, na.rm = TRUE)
      total <- sum(!df$is_control, na.rm = TRUE)
      pct <- if (total > 0) n / total * 100 else 0
      sprintf("%s (%.0f%%)", scales::comma(n), pct)
    })
    
    output$kpi_linked_ext <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0 (0%)")
      n <- sum(df$extractions_matched & !df$is_control, na.rm = TRUE)
      total <- sum(!df$is_control, na.rm = TRUE)
      pct <- if (total > 0) n / total * 100 else 0
      sprintf("%s (%.0f%%)", scales::comma(n), pct)
    })
    
    output$kpi_runs_valid <- renderText({
      pd <- processed_data()
      if (is.null(pd) || !nrow(pd$run_qc)) return("0 / 0")
      valid <- sum(pd$run_qc$run_valid, na.rm = TRUE)
      total <- nrow(pd$run_qc)
      sprintf("%s / %s", scales::comma(valid), scales::comma(total))
    })
    
    output$kpi_positive <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(df$final_category == "positive" & !df$is_control, na.rm = TRUE))
    })
    
    output$kpi_negative <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(df$final_category == "negative" & !df$is_control, na.rm = TRUE))
    })
    
    output$kpi_flagged <- renderText({
      df <- filtered_samples()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(df$any_flag & !df$is_control, na.rm = TRUE))
    })
    
    # === TABLE OUTPUTS ====================================================
    output$tbl_runs <- renderDT({
      pd <- processed_data()
      
      if (!nrow(pd$run_qc)) {
        return(datatable(tibble(Message = "No runs available"), 
                         rownames = FALSE, options = list(dom = 't')))
      }
      
      pd$run_qc %>%
        mutate(
          file = basename(file),
          pct_valid = round(pct_valid, 1)
        ) %>%
        select(run_id, file, n_total, n_samples, n_controls,
               n_positive, n_negative, n_failed, pct_valid,
               controls_pass, run_valid) %>%
        datatable(
          rownames = FALSE,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv')
          ),
          class = "table-sm table-striped"
        ) %>%
        formatStyle(
          "run_valid",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          "controls_pass",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
        )
    })
    
    output$tbl_samples <- renderDT({
      df <- filtered_samples() %>%
        filter(!is_control)
      
      if (!nrow(df)) {
        return(datatable(tibble(Message = "No samples available"), 
                         rownames = FALSE, options = list(dom = 't')))
      }
      
      df <- df %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        select(
          run_id, Name, 
          biobank_matched, biobank_match_type, extractions_matched,
          biobank_province, biobank_health_zone, biobank_structure,
          final_decision, final_category, quality_flag,
          avg_177T_Cq, avg_18S2_Cq,
          delta_cq_tna, delta_cq_rnp, rna_preservation,
          n_replicates, any_flag, flag_summary
        )
      
      datatable(
        df,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "final_category",
          backgroundColor = styleEqual(
            c("positive", "negative", "failed", "inconclusive"),
            c("#d4edda", "#d1ecf1", "#f8d7da", "#fff3cd")
          )
        ) %>%
        formatStyle(
          "any_flag",
          backgroundColor = styleEqual(c(TRUE), c("#fff3cd"))
        )
    })
    
    output$tbl_controls <- renderDT({
      df <- filtered_samples() %>%
        filter(is_control)
      
      if (!nrow(df)) {
        return(datatable(tibble(Message = "No controls available"), 
                         rownames = FALSE, options = list(dom = 't')))
      }
      
      df <- df %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        select(run_id, Name, Type, final_decision, final_category,
               avg_177T_Cq, avg_18S2_Cq, quality_flag)
      
      datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "final_category",
          backgroundColor = styleEqual(
            c("control_ok", "control_fail"),
            c("#d4edda", "#f8d7da")
          )
        )
    })
    
    output$tbl_flagged <- renderDT({
      df <- filtered_samples() %>%
        filter(any_flag, !is_control)
      
      if (!nrow(df)) {
        return(datatable(tibble(Message = "No flagged samples"), 
                         rownames = FALSE, options = list(dom = 't')))
      }
      
      df <- df %>%
        mutate(across(where(is.numeric), ~round(.x, 2))) %>%
        select(run_id, Name, biobank_matched,
               final_decision, flag_summary,
               avg_177T_Cq, avg_18S2_Cq,
               delta_cq_tna, delta_cq_rnp, rna_preservation)
      
      datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        class = "table-sm table-striped"
      )
    })
    
    # === PLOT OUTPUTS =====================================================
    output$plot_lj_177t <- renderPlotly({
      pd <- processed_data()
      lj <- pd$lj_177T
      
      if (!nrow(lj$data)) {
        return(plotly_empty() %>% layout(title = "No control data available"))
      }
      
      plot_ly(lj$data, x = ~run_id) %>%
        add_lines(y = ~center, name = "Mean", 
                  line = list(color = "#2c3e50", width = 2)) %>%
        add_lines(y = ~ucl_1sd, name = "+1 SD", 
                  line = list(color = "#3498db", dash = "dot")) %>%
        add_lines(y = ~lcl_1sd, name = "-1 SD", 
                  line = list(color = "#3498db", dash = "dot")) %>%
        add_lines(y = ~ucl_2sd, name = "+2 SD", 
                  line = list(color = "#e67e22", dash = "dash")) %>%
        add_lines(y = ~lcl_2sd, name = "-2 SD", 
                  line = list(color = "#e67e22", dash = "dash")) %>%
        add_lines(y = ~ucl_3sd, name = "+3 SD", 
                  line = list(color = "#e74c3c", dash = "dashdot")) %>%
        add_lines(y = ~lcl_3sd, name = "-3 SD", 
                  line = list(color = "#e74c3c", dash = "dashdot")) %>%
        add_markers(y = ~mean_cq, name = "Run Mean",
                    marker = list(
                      color = ~ifelse(out_of_control, "#e74c3c", "#27ae60"),
                      size = 8
                    ),
                    text = ~sprintf("Run: %s<br>Mean: %.2f<br>SD: %.2f<br>n: %d",
                                    run_id, mean_cq, sd_cq, n),
                    hoverinfo = "text") %>%
        layout(
          title = sprintf("177T Controls (Mean: %.2f, SD: %.2f, n: %d)",
                          lj$mean, lj$sd, lj$n),
          xaxis = list(title = "Run"),
          yaxis = list(title = "Mean Cq"),
          hovermode = "closest",
          legend = list(orientation = "h", y = -0.2)
        )
    })
    
    output$plot_lj_18s2 <- renderPlotly({
      pd <- processed_data()
      lj <- pd$lj_18S2
      
      if (!nrow(lj$data)) {
        return(plotly_empty() %>% layout(title = "No control data available"))
      }
      
      plot_ly(lj$data, x = ~run_id) %>%
        add_lines(y = ~center, name = "Mean", 
                  line = list(color = "#2c3e50", width = 2)) %>%
        add_lines(y = ~ucl_2sd, name = "+2 SD", 
                  line = list(color = "#e67e22", dash = "dash")) %>%
        add_lines(y = ~lcl_2sd, name = "-2 SD", 
                  line = list(color = "#e67e22", dash = "dash")) %>%
        add_markers(y = ~mean_cq, name = "Run Mean",
                    marker = list(
                      color = ~ifelse(out_of_control, "#e74c3c", "#27ae60"),
                      size = 8
                    ),
                    text = ~sprintf("Run: %s<br>Mean: %.2f<br>n: %d",
                                    run_id, mean_cq, n),
                    hoverinfo = "text") %>%
        layout(
          title = sprintf("18S2 Controls (Mean: %.2f, SD: %.2f, n: %d)",
                          lj$mean, lj$sd, lj$n),
          xaxis = list(title = "Run"),
          yaxis = list(title = "Mean Cq"),
          hovermode = "closest",
          legend = list(orientation = "h", y = -0.2)
        )
    })
    
    output$plot_scatter_tna <- renderPlotly({
      df <- filtered_samples() %>%
        filter(!is_control, !is.na(avg_177T_Cq), !is.na(avg_18S2_Cq))
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      plot_ly(df, x = ~avg_177T_Cq, y = ~avg_18S2_Cq, color = ~final_category,
              text = ~sprintf("Run: %s<br>Name: %s<br>177T: %.2f<br>18S2: %.2f<br>ΔCq: %.2f<br>Call: %s",
                              run_id, Name, avg_177T_Cq, avg_18S2_Cq, delta_cq_tna, final_decision),
              hoverinfo = "text",
              type = "scatter", mode = "markers",
              marker = list(size = 8)) %>%
        add_lines(x = c(0, 50), y = c(0, 50), name = "y = x (perfect concordance)",
                  line = list(color = "#95a5a6", dash = "dash"),
                  hoverinfo = "skip", showlegend = TRUE) %>%
        layout(
          title = "TNA Concordance: 18S2 vs 177T",
          xaxis = list(title = "177T Cq (DNA)", range = c(0, 45)),
          yaxis = list(title = "18S2 Cq (RNA)", range = c(0, 45)),
          hovermode = "closest",
          legend = list(orientation = "h", y = -0.2)
        )
    })
    
    output$plot_scatter_rnp <- renderPlotly({
      df <- filtered_samples() %>%
        filter(!is_control, !is.na(delta_cq_rnp))
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      # Reconstruct RNA and DNA Cq from delta and mean
      df <- df %>%
        mutate(
          # Approximate reconstruction (we don't have raw RNP Cq values)
          # Just use delta for x-axis positioning
          RNP_DNA_approx = 25,  # placeholder
          RNP_RNA_approx = 25 + delta_cq_rnp
        )
      
      plot_ly(df, x = ~RNP_DNA_approx, y = ~RNP_RNA_approx, color = ~rna_preservation,
              text = ~sprintf("Run: %s<br>Name: %s<br>ΔCq: %.2f<br>Preservation: %s",
                              run_id, Name, delta_cq_rnp, rna_preservation),
              hoverinfo = "text",
              type = "scatter", mode = "markers",
              marker = list(size = 8)) %>%
        layout(
          title = "RNA Preservation: ΔCq Distribution",
          xaxis = list(title = "ΔCq (RNA - DNA)"),
          yaxis = list(title = "Sample Count"),
          hovermode = "closest",
          legend = list(orientation = "h", y = -0.2)
        )
    })
    
    # === EXPORT HANDLERS ==================================================
    output$download_qc <- downloadHandler(
      filename = function() {
        sprintf("MIC_QC_Report_%s.xlsx", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        pd <- processed_data()
        .mic_export_qc_workbook(pd, file)
      }
    )
    
    output$download_runs <- downloadHandler(
      filename = function() {
        sprintf("MIC_Runs_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        pd <- processed_data()
        readr::write_csv(pd$run_qc, file)
      }
    )
    
    output$download_samples <- downloadHandler(
      filename = function() {
        sprintf("MIC_Samples_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        df <- filtered_samples() %>% filter(!is_control)
        readr::write_csv(df, file)
      }
    )
    
    output$download_flagged <- downloadHandler(
      filename = function() {
        sprintf("MIC_Flagged_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        df <- filtered_samples() %>% filter(any_flag, !is_control)
        readr::write_csv(df, file)
      }
    )
    
    # Export QC button
    observeEvent(input$export_qc, {
      pd <- processed_data()
      
      if (!nrow(pd$samples)) {
        showNotification("No data to export", type = "warning")
        return()
      }
      
      dir.create("outputs", showWarnings = FALSE)
      path <- file.path("outputs", sprintf("MIC_QC_Report_%s.xlsx", 
                                           format(Sys.Date(), "%Y%m%d")))
      
      tryCatch({
        .mic_export_qc_workbook(pd, path)
        showNotification(sprintf("QC report exported: %s", basename(path)),
                         duration = 5, type = "message")
      }, error = function(e) {
        showNotification(sprintf("Export failed: %s", e$message),
                         duration = 10, type = "error")
      })
    })
    
  })
}