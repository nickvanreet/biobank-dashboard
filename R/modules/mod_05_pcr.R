# R/modules/mod_05_pcr.R
# =============================================================================
# Module 05 — PCR: Comprehensive qPCR Analysis with Quality Control
# =============================================================================
# Handles QuantStudio Excel exports with:
# - Multi-target Cq extraction (177T, 18S2, RNAseP)
# - Sample vs Control separation
# - User-defined thresholds and validation
# - Run-level quality flags (VALID/INVALID/REVIEW)
# - Row-level overrides (call, re-extraction, repeat PCR)
# - Persistent override storage
# - Integration with Biobank and Extractions data
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(DT)
  library(plotly)
})

# ----------------------------- HELPERS ----------------------------------------

#' Detect control samples by ID pattern
.pcr_is_control <- function(x) {
  if (is.null(x) || length(x) == 0) return(logical(0))
  grepl("\\b(NTC|NC|CN|NEG|NEGATIVE|PC|CP|POS|POSITIVE|CTRL|CONTROL|BLANK|WATER)\\b", 
        x, ignore.case = TRUE)
}

#' Classify control type
.pcr_control_type <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  dplyr::case_when(
    grepl("\\b(NTC|BLANK|WATER)\\b", x, ignore.case = TRUE) ~ "NTC",
    grepl("\\b(NC|CN|NEG)\\b", x, ignore.case = TRUE) ~ "NEG",
    grepl("\\b(PC|CP|POS)\\b", x, ignore.case = TRUE) ~ "PC",
    grepl("\\b(CTRL|CONTROL)\\b", x, ignore.case = TRUE) ~ "CTRL",
    TRUE ~ NA_character_
  )
}

#' Safe numeric parser for Cq values
.parse_cq <- function(x) {
  if (is.null(x)) return(numeric(0))
  suppressWarnings({
    cq <- as.numeric(x)
    cq[!is.finite(cq) | cq < 0 | cq > 50] <- NA_real_
    cq
  })
}

# ----------------------------- READERS ----------------------------------------

#' Read Results sheet (sample-level final call)
.pcr_read_results <- function(path) {
  sheets <- excel_sheets(path)
  
  # Find results sheet (case-insensitive)
  candidates <- sheets[grepl("result", sheets, ignore.case = TRUE)]
  if (length(candidates) == 0) {
    message("  No 'Results' sheet found, trying first sheet")
    candidates <- sheets[1]
  }
  
  tryCatch({
    df <- read_excel(path, sheet = candidates[1], .name_repair = "minimal") %>%
      clean_names() %>%
      rename_with(~"sample", matches("^sample$|^id$|^sample_name$|^well_sample$|^numero_labo$|^numero$", 
                                     ignore.case = TRUE)) %>%
      rename_with(~"pcr_call", matches("^final$|^call$|^result$|^resultat$|^interpretation$", 
                                       ignore.case = TRUE))
    
    if (!"sample" %in% names(df)) {
      warning(sprintf("No sample ID column found in Results sheet of %s", basename(path)))
      return(tibble(LabID = character(), PCR_call_results = character()))
    }
    
    df %>%
      transmute(
        LabID = as.character(sample),
        PCR_call_results = as.character(if ("pcr_call" %in% names(.)) pcr_call else NA_character_)
      ) %>%
      filter(!is.na(LabID) & nzchar(LabID))
  }, error = function(e) {
    message(sprintf("  Error reading Results sheet: %s", e$message))
    tibble(LabID = character(), PCR_call_results = character())
  })
}

#' Read Replicates sheet (final call per sample)
.pcr_read_replicates <- function(path) {
  sheets <- excel_sheets(path)
  
  # Find replicates/summary sheet
  candidates <- sheets[grepl("replicat|final|summary|call|consensus", sheets, ignore.case = TRUE)]
  if (length(candidates) == 0) {
    message("  No 'Replicates' sheet found")
    return(tibble(LabID = character(), PCR_call_replicates = character()))
  }
  
  tryCatch({
    df <- read_excel(path, sheet = candidates[1], .name_repair = "minimal") %>%
      clean_names() %>%
      rename_with(~"sample", matches("^sample$|^id$|^sample_name$|^well_sample$|^numero_labo$|^numero$", 
                                     ignore.case = TRUE)) %>%
      rename_with(~"pcr_call", matches("^final$|^call$|^result$|^resultat$|^interpretation$|^consensus$", 
                                       ignore.case = TRUE))
    
    if (!"sample" %in% names(df)) {
      warning(sprintf("No sample ID column found in Replicates sheet of %s", basename(path)))
      return(tibble(LabID = character(), PCR_call_replicates = character()))
    }
    
    df %>%
      transmute(
        LabID = as.character(sample),
        PCR_call_replicates = as.character(if ("pcr_call" %in% names(.)) pcr_call else NA_character_)
      ) %>%
      filter(!is.na(LabID) & nzchar(LabID))
  }, error = function(e) {
    message(sprintf("  Error reading Replicates sheet: %s", e$message))
    tibble(LabID = character(), PCR_call_replicates = character())
  })
}

#' Read target sheet and compute mean/SD Cq per sample
.pcr_summarise_target <- function(path, target_candidates, tag) {
  sheets <- excel_sheets(path)
  
  # Find target sheet (case-insensitive, flexible matching)
  idx <- which(tolower(sheets) %in% tolower(target_candidates))
  if (length(idx) == 0) {
    # Try partial matching
    for (tc in target_candidates) {
      idx <- which(grepl(tc, sheets, ignore.case = TRUE))
      if (length(idx) > 0) break
    }
  }
  
  if (length(idx) == 0) {
    message(sprintf("  No sheet found for target '%s'", tag))
    return(tibble(
      LabID = character(),
      !!paste0("Cq_", tag) := numeric(),
      !!paste0("SD_", tag) := numeric(),
      !!paste0("n_wells_", tag) := integer()
    ))
  }
  
  sheet_name <- sheets[idx[1]]
  
  tryCatch({
    df <- read_excel(path, sheet = sheet_name, .name_repair = "minimal") %>%
      clean_names() %>%
      rename_with(~"sample", matches("^sample$|^id$|^sample_name$|^well_sample$|^numero_labo$|^numero$", 
                                     ignore.case = TRUE)) %>%
      rename_with(~"cq", matches("^cq$|^ct$|^c_t$|^cq_mean$|^ct_mean$|^threshold_cycle$", 
                                 ignore.case = TRUE))
    
    if (!"sample" %in% names(df)) {
      warning(sprintf("No sample ID column found in %s sheet of %s", sheet_name, basename(path)))
      return(tibble(
        LabID = character(),
        !!paste0("Cq_", tag) := numeric(),
        !!paste0("SD_", tag) := numeric(),
        !!paste0("n_wells_", tag) := integer()
      ))
    }
    
    df %>%
      mutate(
        sample = as.character(sample),
        cq_num = .parse_cq(if ("cq" %in% names(.)) cq else NA_real_)
      ) %>%
      group_by(sample) %>%
      summarise(
        !!paste0("Cq_", tag) := mean(cq_num, na.rm = TRUE),
        !!paste0("SD_", tag) := sd(cq_num, na.rm = TRUE),
        !!paste0("n_wells_", tag) := sum(!is.na(cq_num)),
        .groups = "drop"
      ) %>%
      rename(LabID = sample) %>%
      filter(!is.na(LabID) & nzchar(LabID))
    
  }, error = function(e) {
    message(sprintf("  Error reading %s sheet: %s", sheet_name, e$message))
    tibble(
      LabID = character(),
      !!paste0("Cq_", tag) := numeric(),
      !!paste0("SD_", tag) := numeric(),
      !!paste0("n_wells_", tag) := integer()
    )
  })
}

#' Read one complete workbook
.pcr_read_workbook <- function(path) {
  fn <- basename(path)
  message(sprintf("Reading PCR file: %s", fn))
  
  # Read all components
  res   <- .pcr_read_results(path)
  repl  <- .pcr_read_replicates(path)
  t177  <- .pcr_summarise_target(path, c("177T", "177 T", "T177", "177"), "177T")
  t18   <- .pcr_summarise_target(path, c("18S2", "18S", "18 s2", "18 S2", "18s2"), "18S2")
  trnp  <- .pcr_summarise_target(path, c("RNAseP", "RNaseP", "RNAse P", "RNP", "RNASEP"), "RNAseP")
  
  # Join all
  all_ids <- unique(c(res$LabID, repl$LabID, t177$LabID, t18$LabID, trnp$LabID))
  
  if (length(all_ids) == 0) {
    warning(sprintf("No sample IDs found in %s", fn))
    return(tibble())
  }
  
  result <- tibble(LabID = all_ids) %>%
    left_join(res, by = "LabID") %>%
    left_join(repl, by = "LabID") %>%
    left_join(t177, by = "LabID") %>%
    left_join(t18, by = "LabID") %>%
    left_join(trnp, by = "LabID") %>%
    mutate(
      file = fn,
      run_id = tools::file_path_sans_ext(fn),
      is_control = .pcr_is_control(LabID),
      control_type = if_else(is_control, .pcr_control_type(LabID), NA_character_)
    )
  
  message(sprintf("  ✓ Extracted %d samples (%d controls, %d samples)", 
                  nrow(result), sum(result$is_control), sum(!result$is_control)))
  
  result
}

# ----------------------------- UI ---------------------------------------------

#' PCR Module UI
#' @export
mod_pcr_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "PCR (05)",
    icon = icon("dna"),
    
    div(class = "container-fluid",
        
        # === KPI STRIP ========================================================
        h4(class = "mb-3", icon("dna"), " PCR Quality Control"),
        
        layout_column_wrap(
          width = 1/7, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          
          value_box(
            title = "PCR Files",
            value = textOutput(ns("kpi_files")),
            showcase = icon("folder-open"),
            theme = "primary"
          ),
          value_box(
            title = "Total Rows",
            value = textOutput(ns("kpi_rows")),
            showcase = icon("table"),
            theme = "secondary"
          ),
          value_box(
            title = "Samples",
            value = textOutput(ns("kpi_samples")),
            showcase = icon("vial"),
            theme = "success"
          ),
          value_box(
            title = "Controls",
            value = textOutput(ns("kpi_controls")),
            showcase = icon("flask"),
            theme = "warning"
          ),
          value_box(
            title = "With Call",
            value = textOutput(ns("kpi_withcall")),
            showcase = icon("check-circle"),
            theme = "info"
          ),
          value_box(
            title = "Runs (Valid)",
            value = textOutput(ns("kpi_runs_valid")),
            showcase = icon("clipboard-check"),
            theme = "success"
          ),
          value_box(
            title = "Runs (Invalid)",
            value = textOutput(ns("kpi_runs_invalid")),
            showcase = icon("ban"),
            theme = "danger"
          )
        ),
        
        # === THRESHOLDS & RUN VALIDATION ======================================
        card(
          card_header(class = "h5 mb-0", "Thresholds & Run Validation"),
          card_body(
            layout_columns(
              col_widths = c(3, 3, 3, 3), gap = "16px",
              
              # Sample thresholds
              div(
                h6(icon("vial"), " Sample Thresholds"),
                numericInput(ns("th_s_177_min"), "177T: min Cq", 10, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_s_177_max"), "177T: max Cq", 40, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_s_18_min"), "18S2: min Cq", 10, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_s_18_max"), "18S2: max Cq", 40, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_s_rnp_max"), "RNAseP: max Cq", 35, min = 0, max = 50, step = 0.5),
                helpText("Valid range for trypanosome targets and internal control")
              ),
              
              # Control thresholds
              div(
                h6(icon("flask"), " Control Thresholds"),
                numericInput(ns("th_ntc_max"), "NTC: max Cq (should be NA or ≥)", 
                             39, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_pc_min"), "PC: min Cq", 15, min = 0, max = 50, step = 0.5),
                numericInput(ns("th_pc_max"), "PC: max Cq", 30, min = 0, max = 50, step = 0.5),
                br(),
                helpText("Negative controls (NTC/NC): should have no amplification or very late Cq"),
                helpText("Positive controls (PC): should amplify within expected range")
              ),
              
              # Run validation
              div(
                h6(icon("clipboard-check"), " Run Validation"),
                selectInput(ns("run_select"), "Select run", choices = NULL),
                selectInput(ns("run_status"), "Set status", 
                            choices = c("REVIEW", "VALID", "INVALID"), 
                            selected = "REVIEW"),
                actionButton(ns("run_apply"), "Apply Status", 
                             class = "btn-primary w-100", icon = icon("check")),
                br(), br(),
                actionButton(ns("save_overrides"), "Save All Overrides", 
                             class = "btn-success w-100", icon = icon("save")),
                helpText("Overrides saved to: outputs/pcr_overrides.csv")
              ),
              
              # Row actions
              div(
                h6(icon("edit"), " Actions on Selected Rows"),
                helpText("Select rows in the Samples table below, then:"),
                actionButton(ns("mark_reextract"), "🔄 Mark for Re-extraction", 
                             class = "btn-warning w-100 mb-2"),
                actionButton(ns("mark_reqpcr"), "🔁 Mark for Repeat PCR", 
                             class = "btn-warning w-100 mb-2"),
                selectInput(ns("override_call"), "Override PCR call", 
                            choices = c("(no change)" = "", "POS", "NEG", "Inconclusive", "Invalid")),
                actionButton(ns("apply_call"), "Apply Call Override", 
                             class = "btn-info w-100", icon = icon("pen"))
              )
            )
          )
        ),
        
        # === SAMPLES TABLE ====================================================
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(class = "h5 mb-0", icon("vial"), " Sample Results"),
            span(class = "text-muted small", "Select rows to apply actions")
          ),
          card_body(
            DTOutput(ns("tbl_samples"))
          )
        ),
        
        # === CONTROLS TABLE ===================================================
        card(
          card_header(class = "h5 mb-0", icon("flask"), " Control Results — Per Run"),
          card_body(
            DTOutput(ns("tbl_controls"))
          )
        ),
        
        # === SAMPLE PLOTS =====================================================
        h4(class = "mt-4 mb-3", icon("chart-line"), " Sample Cq Distributions"),
        
        layout_columns(
          col_widths = c(4, 4, 4), gap = "16px",
          
          card(
            card_header("177T (Samples)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_s_177"), height = "300px")
            )
          ),
          card(
            card_header("18S2 (Samples)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_s_18"), height = "300px")
            )
          ),
          card(
            card_header("RNAseP (Samples)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_s_rnp"), height = "300px")
            )
          )
        ),
        
        # === CONTROL PLOTS ====================================================
        h4(class = "mt-4 mb-3", icon("flask"), " Control Cq Distributions"),
        
        layout_columns(
          col_widths = c(4, 4, 4), gap = "16px",
          
          card(
            card_header("177T (Controls)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_c_177"), height = "300px")
            )
          ),
          card(
            card_header("18S2 (Controls)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_c_18"), height = "300px")
            )
          ),
          card(
            card_header("RNAseP (Controls)"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_c_rnp"), height = "300px")
            )
          )
        ),
        
        # === VALIDITY OVERVIEW ================================================
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          
          card(
            card_header("Sample Validity Summary"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_validity_summary"), height = "350px")
            )
          ),
          card(
            card_header("Control Pass/Fail by Run"),
            card_body_fill(
              plotly::plotlyOutput(ns("plot_control_summary"), height = "350px")
            )
          )
        )
    )
  )
}

# ----------------------------- SERVER -----------------------------------------

#' PCR Module Server
#' @param pcr_dir Directory containing PCR Excel files (default: "data/PCR")
#' @param biobank_df Reactive or tibble with Barcode and LabID columns
#' @param extractions_df Reactive or tibble with Barcode and LabID columns
#' @param filters_reactive Optional reactive returning a filter function
#' @export
mod_pcr_server <- function(id,
                           pcr_dir = "data/PCR",
                           biobank_df = NULL,
                           extractions_df = NULL,
                           filters_reactive = NULL) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === REACTIVE VALUES ====================================================
    
    # Create outputs directory
    dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
    overrides_path <- file.path("outputs", "pcr_overrides.csv")
    
    rv <- reactiveValues(
      overrides = if (file.exists(overrides_path)) {
        readr::read_csv(overrides_path, show_col_types = FALSE)
      } else {
        tibble(
          Barcode = character(),
          LabID = character(),
          run_id = character(),
          PCR_call_override = character(),
          action_reextract = logical(),
          action_reqpcr = logical()
        )
      },
      run_status = tibble(run_id = character(), status = character())
    )
    
    # === STANDARDIZE BIOBANK/EXTRACTIONS ====================================
    
    biobank_std <- reactive({
      if (is.null(biobank_df)) return(tibble())
      
      df <- if (is.function(biobank_df)) biobank_df() else biobank_df
      if (is.null(df) || !nrow(df)) return(tibble())
      
      df %>%
        as_tibble() %>%
        clean_names() %>%
        rename_with(~"Barcode", matches("^barcode$|^code_barres_kps$", ignore.case = TRUE)) %>%
        rename_with(~"LabID", matches("^labid$|^lab_id$|^numero$|^numero_labo$|^sample$", 
                                      ignore.case = TRUE)) %>%
        mutate(across(c(Barcode, LabID), as.character)) %>%
        distinct(Barcode, LabID, .keep_all = TRUE)
    })
    
    extractions_std <- reactive({
      if (is.null(extractions_df)) return(tibble())
      
      df <- if (is.function(extractions_df)) extractions_df() else extractions_df
      if (is.null(df) || !nrow(df)) return(tibble())
      
      df %>%
        as_tibble() %>%
        clean_names() %>%
        rename_with(~"Barcode", matches("^barcode$|^code_barres_kps$|^sample_id$", 
                                        ignore.case = TRUE)) %>%
        rename_with(~"LabID", matches("^labid$|^lab_id$|^numero$|^record_number$|^sample$", 
                                      ignore.case = TRUE)) %>%
        mutate(across(c(Barcode, LabID), as.character)) %>%
        distinct(Barcode, LabID, .keep_all = TRUE)
    })
    
    # === READ ALL PCR FILES =================================================
    
    pcr_files <- reactive({
      if (!dir.exists(pcr_dir)) {
        message(sprintf("PCR directory not found: %s", pcr_dir))
        return(character())
      }
      list.files(pcr_dir, pattern = "\\.(xlsx?|xls)$", full.names = TRUE, ignore.case = TRUE)
    })
    
    pcr_all <- reactive({
      files <- pcr_files()
      if (length(files) == 0) {
        message("No PCR files found")
        return(tibble())
      }
      
      message(sprintf("\n=== Reading %d PCR files ===", length(files)))
      
      result <- map_dfr(files, function(f) {
        tryCatch({
          .pcr_read_workbook(f)
        }, error = function(e) {
          message(sprintf("ERROR reading %s: %s", basename(f), e$message))
          tibble()
        })
      })
      
      if (nrow(result) == 0) return(tibble())
      
      result %>%
        mutate(
          across(starts_with(c("Cq_", "SD_")), ~ if_else(is.nan(.x), NA_real_, .x)),
          LabID = as.character(LabID)
        )
    })
    
    # === JOIN WITH BIOBANK/EXTRACTIONS ======================================
    
    pcr_joined <- reactive({
      df <- pcr_all()
      if (nrow(df) == 0) return(df)
      
      bb <- biobank_std()
      ex <- extractions_std()
      
      result <- df
      
      # Join Biobank (by LabID)
      if (nrow(bb) > 0) {
        result <- result %>%
          left_join(bb %>% select(Barcode, LabID) %>% distinct(), 
                    by = "LabID", suffix = c("", "_biobank"))
      } else {
        result$Barcode <- NA_character_
      }
      
      # Join Extractions (by Barcode and LabID)
      if (nrow(ex) > 0) {
        result <- result %>%
          left_join(ex %>% select(Barcode, LabID) %>% distinct(), 
                    by = c("Barcode", "LabID"), 
                    relationship = "many-to-many",
                    suffix = c("", "_extraction"))
      }
      
      # Merge run status
      result <- result %>%
        left_join(rv$run_status, by = "run_id")
      
      # Apply external filters if provided
      if (!is.null(filters_reactive)) {
        filter_fn <- filters_reactive()
        if (is.function(filter_fn)) {
          result <- filter_fn(result)
        }
      }
      
      result %>%
        relocate(Barcode, LabID, .before = 1)
    })
    
    # === APPLY THRESHOLDS & OVERRIDES =======================================
    
    calc_flags <- reactive({
      df <- pcr_joined()
      if (nrow(df) == 0) return(df)
      
      # Threshold values
      s177_min <- input$th_s_177_min %||% 10
      s177_max <- input$th_s_177_max %||% 40
      s18_min  <- input$th_s_18_min  %||% 10
      s18_max  <- input$th_s_18_max  %||% 40
      rnp_max  <- input$th_s_rnp_max %||% 35
      ntc_max  <- input$th_ntc_max   %||% 39
      pc_min   <- input$th_pc_min    %||% 15
      pc_max   <- input$th_pc_max    %||% 30
      
      # Calculate validity flags
      df <- df %>%
        mutate(
          # Sample validity (only for non-controls)
          sample_valid_177 = if_else(
            is_control, NA,
            if_else(is.na(Cq_177T), NA, Cq_177T >= s177_min & Cq_177T <= s177_max)
          ),
          sample_valid_18 = if_else(
            is_control, NA,
            if_else(is.na(Cq_18S2), NA, Cq_18S2 >= s18_min & Cq_18S2 <= s18_max)
          ),
          sample_valid_rnp = if_else(
            is_control, NA,
            if_else(is.na(Cq_RNAseP), NA, Cq_RNAseP <= rnp_max)
          ),
          
          # Overall sample validity
          sample_overall_valid = if_else(
            is_control, NA,
            coalesce(sample_valid_177, TRUE) & 
              coalesce(sample_valid_18, TRUE) & 
              coalesce(sample_valid_rnp, TRUE)
          ),
          
          # Control validity
          ctrl_valid = case_when(
            control_type == "NTC" ~ 
              (is.na(Cq_177T) | Cq_177T >= ntc_max) &
              (is.na(Cq_18S2) | Cq_18S2 >= ntc_max) &
              (is.na(Cq_RNAseP) | Cq_RNAseP >= ntc_max),
            
            control_type == "PC" ~ 
              (!is.na(Cq_177T) & Cq_177T >= pc_min & Cq_177T <= pc_max) |
              (!is.na(Cq_18S2) & Cq_18S2 >= pc_min & Cq_18S2 <= pc_max),
            
            control_type == "NEG" ~ 
              (is.na(Cq_177T) | Cq_177T >= ntc_max) &
              (is.na(Cq_18S2) | Cq_18S2 >= ntc_max),
            
            TRUE ~ NA
          )
        )
      
      # Apply overrides
      overrides <- rv$overrides
      if (nrow(overrides) > 0) {
        df <- df %>%
          left_join(
            overrides %>% 
              select(Barcode, LabID, run_id, PCR_call_override, 
                     action_reextract, action_reqpcr),
            by = c("Barcode", "LabID", "run_id"),
            suffix = c("", "_new")
          ) %>%
          mutate(
            PCR_call_override = coalesce(PCR_call_override_new, PCR_call_override),
            action_reextract = coalesce(action_reextract_new, action_reextract, FALSE),
            action_reqpcr = coalesce(action_reqpcr_new, action_reqpcr, FALSE)
          ) %>%
          select(-ends_with("_new"))
      } else {
        df <- df %>%
          mutate(
            PCR_call_override = NA_character_,
            action_reextract = FALSE,
            action_reqpcr = FALSE
          )
      }
      
      # Final displayed call: override > replicates > results
      df %>%
        mutate(
          PCR_call_display = coalesce(PCR_call_override, PCR_call_replicates, PCR_call_results)
        )
    })
    
    # === POPULATE RUN SELECTOR ==============================================
    
    observe({
      runs <- sort(unique(pcr_all()$run_id), decreasing = TRUE)
      updateSelectInput(session, "run_select", 
                        choices = if (length(runs)) runs else c("(no runs)" = ""),
                        selected = if (length(runs)) runs[1] else "")
    })
    
    # === KPI OUTPUTS ========================================================
    
    output$kpi_files <- renderText({
      scales::comma(length(pcr_files()))
    })
    
    output$kpi_rows <- renderText({
      scales::comma(nrow(pcr_all()))
    })
    
    output$kpi_samples <- renderText({
      df <- calc_flags()
      scales::comma(sum(!df$is_control))
    })
    
    output$kpi_controls <- renderText({
      df <- calc_flags()
      scales::comma(sum(df$is_control))
    })
    
    output$kpi_withcall <- renderText({
      df <- calc_flags()
      scales::comma(sum(!is.na(df$PCR_call_display) & nzchar(df$PCR_call_display)))
    })
    
    output$kpi_runs_valid <- renderText({
      df <- calc_flags()
      n <- df %>% distinct(run_id, status) %>% filter(status == "VALID") %>% nrow()
      scales::comma(n)
    })
    
    output$kpi_runs_invalid <- renderText({
      df <- calc_flags()
      n <- df %>% distinct(run_id, status) %>% filter(status == "INVALID") %>% nrow()
      scales::comma(n)
    })
    
    # === DATATABLE OUTPUTS ==================================================
    
    output$tbl_samples <- renderDT({
      df <- calc_flags() %>% filter(!is_control)
      
      if (nrow(df) == 0) {
        return(datatable(tibble(note = "No sample rows found.")))
      }
      
      display_df <- df %>%
        select(
          Barcode, LabID, run_id, status,
          PCR_call_results, PCR_call_replicates, PCR_call_override, PCR_call_display,
          action_reextract, action_reqpcr,
          Cq_177T, SD_177T, n_wells_177T, sample_valid_177,
          Cq_18S2, SD_18S2, n_wells_18S2, sample_valid_18,
          Cq_RNAseP, SD_RNAseP, n_wells_RNAseP, sample_valid_rnp,
          sample_overall_valid,
          file
        ) %>%
        mutate(across(starts_with("Cq_"), ~ round(.x, 2))) %>%
        mutate(across(starts_with("SD_"), ~ round(.x, 2)))
      
      datatable(
        display_df,
        selection = "multiple",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "sample_overall_valid",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          "action_reextract",
          backgroundColor = styleEqual(c(TRUE), c("#fff3cd"))
        ) %>%
        formatStyle(
          "action_reqpcr",
          backgroundColor = styleEqual(c(TRUE), c("#cfe2ff"))
        )
    }, server = TRUE)
    
    output$tbl_controls <- renderDT({
      df <- calc_flags() %>% filter(is_control)
      
      if (nrow(df) == 0) {
        return(datatable(tibble(note = "No control rows detected.")))
      }
      
      display_df <- df %>%
        select(
          run_id, file, LabID, control_type, status,
          PCR_call_results, PCR_call_replicates, PCR_call_display,
          Cq_177T, n_wells_177T,
          Cq_18S2, n_wells_18S2,
          Cq_RNAseP, n_wells_RNAseP,
          ctrl_valid
        ) %>%
        arrange(run_id, control_type, LabID) %>%
        mutate(across(starts_with("Cq_"), ~ round(.x, 2)))
      
      datatable(
        display_df,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "table-sm table-striped"
      ) %>%
        formatStyle(
          "ctrl_valid",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
        )
    })
    
    # === ROW-LEVEL ACTIONS ==================================================
    
    selected_rows <- reactive({
      idx <- input$tbl_samples_rows_selected
      if (length(idx) == 0) return(NULL)
      
      samples_df <- calc_flags() %>% filter(!is_control)
      samples_df[idx, c("Barcode", "LabID", "run_id")]
    })
    
    observeEvent(input$mark_reextract, {
      rows <- selected_rows()
      req(nrow(rows) > 0)
      
      new_overrides <- rows %>%
        mutate(
          PCR_call_override = NA_character_,
          action_reextract = TRUE,
          action_reqpcr = FALSE
        )
      
      rv$overrides <- rv$overrides %>%
        rows_upsert(new_overrides, by = c("Barcode", "LabID", "run_id"))
      
      showNotification(sprintf("Marked %d samples for re-extraction", nrow(rows)), 
                       type = "message")
    })
    
    observeEvent(input$mark_reqpcr, {
      rows <- selected_rows()
      req(nrow(rows) > 0)
      
      new_overrides <- rows %>%
        mutate(
          PCR_call_override = NA_character_,
          action_reextract = FALSE,
          action_reqpcr = TRUE
        )
      
      rv$overrides <- rv$overrides %>%
        rows_upsert(new_overrides, by = c("Barcode", "LabID", "run_id"))
      
      showNotification(sprintf("Marked %d samples for repeat PCR", nrow(rows)), 
                       type = "message")
    })
    
    observeEvent(input$apply_call, {
      rows <- selected_rows()
      req(nrow(rows) > 0)
      
      new_call <- input$override_call
      if (is.null(new_call) || nchar(new_call) == 0) {
        showNotification("Please select a call override value", type = "warning")
        return()
      }
      
      new_overrides <- rows %>%
        mutate(
          PCR_call_override = new_call,
          action_reextract = FALSE,
          action_reqpcr = FALSE
        )
      
      rv$overrides <- rv$overrides %>%
        rows_upsert(new_overrides, by = c("Barcode", "LabID", "run_id"))
      
      showNotification(sprintf("Set call override '%s' for %d samples", new_call, nrow(rows)), 
                       type = "message")
    })
    
    # === RUN-LEVEL VALIDATION ===============================================
    
    observeEvent(input$run_apply, {
      req(input$run_select, input$run_status)
      
      run_id <- input$run_select
      status <- input$run_status
      
      new_status <- tibble(run_id = run_id, status = status)
      
      rv$run_status <- rv$run_status %>%
        filter(run_id != !!run_id) %>%
        bind_rows(new_status)
      
      showNotification(sprintf("Set run '%s' to %s", run_id, status), type = "message")
    })
    
    # === SAVE OVERRIDES =====================================================
    
    observeEvent(input$save_overrides, {
      output_data <- rv$overrides %>%
        arrange(run_id, Barcode, LabID)
      
      readr::write_csv(output_data, overrides_path)
      
      showNotification(
        sprintf("Saved %d override records to %s", nrow(output_data), overrides_path),
        type = "message",
        duration = 5
      )
    })
    
    # === PLOTS ==============================================================
    
    .make_histogram <- function(df, var, title, threshold_min = NULL, threshold_max = NULL) {
      if (nrow(df) == 0 || all(is.na(df[[var]]))) {
        return(plotly_empty() %>% 
                 layout(title = list(text = "No data available")))
      }
      
      p <- plot_ly(df, x = ~.data[[var]], type = "histogram", nbinsx = 30,
                   marker = list(color = "#3498DB")) %>%
        layout(
          title = title,
          xaxis = list(title = "Cq"),
          yaxis = list(title = "Count"),
          showlegend = FALSE
        )
      
      # Add threshold lines if provided
      if (!is.null(threshold_min)) {
        p <- p %>% add_trace(
          x = c(threshold_min, threshold_min),
          y = c(0, max(hist(df[[var]], plot = FALSE)$counts)),
          type = "scatter",
          mode = "lines",
          line = list(color = "red", dash = "dash"),
          name = "Min threshold",
          showlegend = TRUE
        )
      }
      
      if (!is.null(threshold_max)) {
        p <- p %>% add_trace(
          x = c(threshold_max, threshold_max),
          y = c(0, max(hist(df[[var]], plot = FALSE)$counts)),
          type = "scatter",
          mode = "lines",
          line = list(color = "red", dash = "dash"),
          name = "Max threshold",
          showlegend = TRUE
        )
      }
      
      p
    }
    
    # Sample plots
    output$plot_s_177 <- renderPlotly({
      df <- calc_flags() %>% filter(!is_control, is.finite(Cq_177T))
      .make_histogram(df, "Cq_177T", "Samples — 177T",
                      input$th_s_177_min, input$th_s_177_max)
    })
    
    output$plot_s_18 <- renderPlotly({
      df <- calc_flags() %>% filter(!is_control, is.finite(Cq_18S2))
      .make_histogram(df, "Cq_18S2", "Samples — 18S2",
                      input$th_s_18_min, input$th_s_18_max)
    })
    
    output$plot_s_rnp <- renderPlotly({
      df <- calc_flags() %>% filter(!is_control, is.finite(Cq_RNAseP))
      .make_histogram(df, "Cq_RNAseP", "Samples — RNAseP",
                      NULL, input$th_s_rnp_max)
    })
    
    # Control plots
    output$plot_c_177 <- renderPlotly({
      df <- calc_flags() %>% filter(is_control, is.finite(Cq_177T))
      .make_histogram(df, "Cq_177T", "Controls — 177T")
    })
    
    output$plot_c_18 <- renderPlotly({
      df <- calc_flags() %>% filter(is_control, is.finite(Cq_18S2))
      .make_histogram(df, "Cq_18S2", "Controls — 18S2")
    })
    
    output$plot_c_rnp <- renderPlotly({
      df <- calc_flags() %>% filter(is_control, is.finite(Cq_RNAseP))
      .make_histogram(df, "Cq_RNAseP", "Controls — RNAseP")
    })
    
    # Validity summary
    output$plot_validity_summary <- renderPlotly({
      df <- calc_flags() %>% filter(!is_control)
      
      if (nrow(df) == 0) {
        return(plotly_empty() %>% layout(title = "No sample data"))
      }
      
      summary <- df %>%
        summarise(
          Valid = sum(sample_overall_valid == TRUE, na.rm = TRUE),
          Invalid = sum(sample_overall_valid == FALSE, na.rm = TRUE),
          Unknown = sum(is.na(sample_overall_valid))
        ) %>%
        pivot_longer(everything(), names_to = "Category", values_to = "Count")
      
      plot_ly(summary, x = ~Category, y = ~Count, type = "bar",
              marker = list(color = c("#27AE60", "#E74C3C", "#95A5A6"))) %>%
        layout(
          title = "Sample Validity (based on thresholds)",
          xaxis = list(title = ""),
          yaxis = list(title = "Number of Samples")
        )
    })
    
    # Control summary
    output$plot_control_summary <- renderPlotly({
      df <- calc_flags() %>% filter(is_control)
      
      if (nrow(df) == 0) {
        return(plotly_empty() %>% layout(title = "No control data"))
      }
      
      summary <- df %>%
        group_by(run_id) %>%
        summarise(
          Pass = sum(ctrl_valid == TRUE, na.rm = TRUE),
          Fail = sum(ctrl_valid == FALSE, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        pivot_longer(c(Pass, Fail), names_to = "Result", values_to = "Count")
      
      plot_ly(summary, x = ~run_id, y = ~Count, color = ~Result, type = "bar",
              colors = c(Pass = "#27AE60", Fail = "#E74C3C")) %>%
        layout(
          title = "Control QC Status per Run",
          xaxis = list(title = "Run ID"),
          yaxis = list(title = "Number of Controls"),
          barmode = "stack"
        )
    })
    
  })
}
