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
.kpi <- function(samples_df) {
  if (!nrow(samples_df)) return(list(
    n_files = 0, n_runs = 0, n_rows = 0, n_samples = 0, n_controls = 0,
    n_pos = 0, n_neg = 0, n_fail = 0, n_inconcl = 0,
    runs_valid = 0, runs_invalid = 0
  ))
  
  tib <- samples_df
  list(
    n_files     = length(unique(tib$file)),
    n_runs      = length(unique(tib$run_id)),
    n_rows      = nrow(tib),
    n_samples   = sum(!tib$is_control, na.rm = TRUE),
    n_controls  = sum( tib$is_control, na.rm = TRUE),
    n_pos       = sum(tib$final_category == "positive", na.rm = TRUE),
    n_neg       = sum(tib$final_category == "negative", na.rm = TRUE),
    n_fail      = sum(tib$final_category == "failed", na.rm = TRUE),
    n_inconcl   = sum(tib$final_category == "inconclusive", na.rm = TRUE),
    # Suggested run status: INVALID if any control_fail in a run; else VALID if none
    runs_valid  = tib %>%
      group_by(run_id) %>%
      summarise(ok = !any(final_category == "control_fail" & is_control, na.rm = TRUE),
                .groups = "drop") %>% filter(ok) %>% nrow(),
    runs_invalid = tib %>%
      group_by(run_id) %>%
      summarise(bad = any(final_category == "control_fail" & is_control, na.rm = TRUE),
                .groups = "drop") %>% filter(bad) %>% nrow()
  )
}

# ---------- QC Excel (multi-sheet) -------------------------------------------
.mic_export_qc <- function(samples_df, replic_df, out_path) {
  if (!nrow(samples_df)) stop("No data to export")
  # Controls by Run
  controls <- samples_df %>%
    filter(is_control) %>%
    transmute(
      run_id, file, Name, control_type,
      decision = final_decision,
      category = final_category
    )
  # Samples
  samples <- samples_df %>%
    filter(!is_control)
  # Run QC summary
  run_qc <- controls %>%
    mutate(pass = grepl("control OK", decision)) %>%
    group_by(run_id) %>%
    summarise(
      n_controls = n(),
      control_fail = sum(category == "control_fail", na.rm = TRUE),
      control_ok   = sum(category == "control_ok",   na.rm = TRUE),
      run_status_suggested = ifelse(control_fail > 0, "INVALID", "VALID"),
      .groups = "drop"
    )
  # Anomalies (quick triage)
  anomalies <- samples_df %>%
    filter(!is_control) %>%
    filter(final_category %in% c("failed","inconclusive") |
             grepl("RNA loss|Poor RNA preservation", rna_preservation %||% ""))
  
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
          width = 1/7, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          value_box(title = "Files",         value = textOutput(ns("kpi_files")),    showcase = icon("folder-open"), theme = "primary"),
          value_box(title = "Runs",          value = textOutput(ns("kpi_runs")),     showcase = icon("clipboard-list"), theme = "secondary"),
          value_box(title = "Rows",          value = textOutput(ns("kpi_rows")),     showcase = icon("table"), theme = "secondary"),
          value_box(title = "Samples",       value = textOutput(ns("kpi_samples")),  showcase = icon("vial"), theme = "success"),
          value_box(title = "Controls",      value = textOutput(ns("kpi_controls")), showcase = icon("flask"), theme = "warning"),
          value_box(title = "Pos / Neg",     value = textOutput(ns("kpi_posneg")),   showcase = icon("check-circle"), theme = "info"),
          value_box(title = "Runs (OK/Bad)", value = textOutput(ns("kpi_run_okbad")),showcase = icon("traffic-light"), theme = "success")
        ),
        
        # ==== Controls ========================================================
        card(
          card_header(class = "h5 mb-0", "Directory & Export"),
          card_body(
            layout_columns(
              col_widths = c(6,3,3),
              textInput(ns("mic_dir"), "MIC folder", value = "data/PCR", placeholder = "path/to/MIC/xlsx"),
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
        
        # ==== Plots ===========================================================
        h4(class = "mt-4 mb-3", icon("chart-line"), " Distributions & Run QC"),
        
        layout_columns(
          col_widths = c(4,4,4), gap = "16px",
          card(card_header("Cq — 177T (Samples)"), card_body_fill(plotly::plotlyOutput(ns("plot_s_177"), height = "300px"))),
          card(card_header("Cq — 18S2 (Samples)"), card_body_fill(plotly::plotlyOutput(ns("plot_s_18"),  height = "300px"))),
          card(card_header("ΔCq — RNAseP (RNA − DNA)"), card_body_fill(plotly::plotlyOutput(ns("plot_delta_rnp"), height = "300px")))
        ),
        
        layout_columns(
          col_widths = c(6,6), gap = "16px",
          card(card_header("Control QC per Run"), card_body_fill(plotly::plotlyOutput(ns("plot_control_summary"), height = "360px"))),
          card(card_header("Scatter: 18S2 vs 177T (Samples)"), card_body_fill(plotly::plotlyOutput(ns("plot_scatter_tna"), height = "360px")))
        )
    )
  )
}

# ----------------------------- SERVER -----------------------------------------
#' MIC-only PCR Module Server
#' @param id shiny id
#' @param default_dir default MIC folder (string)
#' @export
mod_mic_pcr_server <- function(id, default_dir = "data/PCR") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(TRUE, {
      updateTextInput(session, "mic_dir", value = default_dir)
    }, once = TRUE)
    
    mic_data <- reactiveVal(list(samples = tibble(), replicates = tibble(), files = character()))
    
    reload_now <- function() {
      d <- trimws(input$mic_dir %||% "")
      md <- .mic_read_dir(d, verbose = FALSE)
      mic_data(md)
    }
    
    observeEvent(input$reload, ignoreInit = TRUE, {
      reload_now()
      showNotification("MIC directory reloaded.", type = "message")
    })
    # initial load
    observeEvent(input$mic_dir, {
      reload_now()
    }, ignoreInit = FALSE)
    
    # ====================== KPIs ============================================
    output$kpi_files <- renderText({
      md <- mic_data(); scales::comma(length(md$files))
    })
    output$kpi_runs <- renderText({
      md <- mic_data(); scales::comma(dplyr::n_distinct(md$samples$run_id))
    })
    output$kpi_rows <- renderText({
      md <- mic_data(); scales::comma(nrow(md$samples))
    })
    output$kpi_samples <- renderText({
      md <- mic_data(); scales::comma(sum(!md$samples$is_control, na.rm = TRUE))
    })
    output$kpi_controls <- renderText({
      md <- mic_data(); scales::comma(sum(md$samples$is_control, na.rm = TRUE))
    })
    output$kpi_posneg <- renderText({
      md <- mic_data()
      pos <- sum(md$samples$final_category == "positive", na.rm = TRUE)
      neg <- sum(md$samples$final_category == "negative", na.rm = TRUE)
      paste0(scales::comma(pos), " / ", scales::comma(neg))
    })
    output$kpi_run_okbad <- renderText({
      md <- mic_data()
      k <- .kpi(md$samples)
      paste0(scales::comma(k$runs_valid), " / ", scales::comma(k$runs_invalid))
    })
    
    # ====================== Tables ==========================================
    output$tbl_samples <- renderDT({
      md <- mic_data()
      df <- md$samples %>% filter(!is_control) %>%
        select(Name, run_id, file,
               final_decision, final_category, quality_flag,
               avg_177T_Cq, avg_18S2_Cq,
               rna_preservation, avg_preservation_delta,
               n_replicates, n_positive, n_negative, n_failed, n_inconclusive)
      if (!nrow(df)) return(DT::datatable(tibble(note = "No sample rows.")))
      datatable(df, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE,
                                                     dom = 'Bfrtip', buttons = c('copy','csv','excel')),
                class = "table-sm table-striped") %>%
        formatStyle("final_category",
                    backgroundColor = styleEqual(
                      c("positive","negative","failed","inconclusive"),
                      c("#d4edda","#e8f4fd","#fdecea","#fff3cd")
                    )) %>%
        formatStyle("quality_flag",
                    backgroundColor = styleEqual(c("✓ Good"),
                                                 c("#e8f8f5")))
    }, server = TRUE)
    
    output$tbl_controls <- renderDT({
      md <- mic_data()
      ctrl <- md$samples %>%
        filter(is_control) %>%
        transmute(run_id, file, Name, control_type,
                  decision = final_decision,
                  category = final_category) %>%
        arrange(run_id, control_type, Name)
      if (!nrow(ctrl)) return(DT::datatable(tibble(note = "No control rows.")))
      datatable(ctrl, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE,
                                                       dom = 'Bfrtip', buttons = c('copy','csv','excel')),
                class = "table-sm table-striped") %>%
        formatStyle("category",
                    backgroundColor = styleEqual(
                      c("control_ok","control_fail"),
                      c("#d4edda","#fdecea")
                    ))
    }, server = TRUE)
    
    # ====================== Plots ===========================================
    make_hist <- function(x, title) {
      if (!length(x) || all(is.na(x))) {
        return(plotly_empty() %>% layout(title = list(text = "No data")))
      }
      plot_ly(x = ~x, type = "histogram", nbinsx = 30) %>%
        layout(title = title,
               xaxis = list(title = ""),
               yaxis = list(title = "Count"),
               showlegend = FALSE)
    }
    
    output$plot_s_177 <- renderPlotly({
      md <- mic_data()
      rd <- md$replicates %>% filter(!is_control, is.finite(Cq_177T))
      make_hist(rd$Cq_177T, "177T (Samples)")
    })
    
    output$plot_s_18 <- renderPlotly({
      md <- mic_data()
      rd <- md$replicates %>% filter(!is_control, is.finite(Cq_18S2))
      make_hist(rd$Cq_18S2, "18S2 (Samples)")
    })
    
    output$plot_delta_rnp <- renderPlotly({
      md <- mic_data()
      df <- md$replicates
      # Compute ΔCq = RNAseP_RNA_Cq - RNAseP_DNA_Cq, if both exist
      if (!all(c("RNAseP_RNA_Cq","RNAseP_DNA_Cq") %in% names(df))) {
        return(plotly_empty() %>% layout(title = list(text = "No RNA/DNA RNAseP available")))
      }
      dlt <- with(df, as.numeric(RNAseP_RNA_Cq) - as.numeric(RNAseP_DNA_Cq))
      dlt <- dlt[is.finite(dlt)]
      make_hist(dlt, "ΔCq (RNAseP_RNA − RNAseP_DNA)")
    })
    
    output$plot_control_summary <- renderPlotly({
      md <- mic_data()
      ctrl <- md$samples %>%
        filter(is_control) %>%
        mutate(pass = grepl("control OK", final_decision)) %>%
        group_by(run_id) %>%
        summarise(Pass = sum(pass, na.rm = TRUE),
                  Fail = sum(final_category == "control_fail", na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_longer(c(Pass, Fail), names_to = "QC", values_to = "Count")
      if (!nrow(ctrl)) return(plotly_empty() %>% layout(title = "No control data"))
      plot_ly(ctrl, x = ~run_id, y = ~Count, color = ~QC, type = "bar") %>%
        layout(title = "Control QC per Run", barmode = "stack",
               xaxis = list(title = "Run"), yaxis = list(title = "Count"))
    })
    
    output$plot_scatter_tna <- renderPlotly({
      md <- mic_data()
      smp <- md$samples %>% filter(!is_control) %>%
        transmute(run_id, Name,
                  Cq_177T = avg_177T_Cq,
                  Cq_18S2 = avg_18S2_Cq,
                  category = final_category)
      if (!nrow(smp)) return(plotly_empty() %>% layout(title = "No sample data"))
      plot_ly(smp, x = ~Cq_177T, y = ~Cq_18S2, type = "scatter", mode = "markers",
              text = ~paste("Run:", run_id, "<br>Name:", Name, "<br>", category),
              hoverinfo = "text", color = ~category) %>%
        layout(title = "18S2 vs 177T (lower Cq = stronger signal)",
               xaxis = list(title = "Cq 177T"),
               yaxis = list(title = "Cq 18S2"))
    })
    
    # ====================== Export ==========================================
    observeEvent(input$export, {
      md <- mic_data()
      req(nrow(md$samples) > 0)
      dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
      path <- file.path("outputs", "MIC_PCR_QC.xlsx")
      .mic_export_qc(md$samples, md$replicates, path)
      showNotification(sprintf("QC workbook written: %s", path), type = "message", duration = 6)
    })
  })
}
