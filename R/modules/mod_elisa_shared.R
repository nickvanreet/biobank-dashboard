# =============================================================================
# Shared ELISA Module UI/Server Components
# =============================================================================
#
# This module provides generic UI and server components for ELISA data
# visualization and analysis. It is used by both ELISA-PE and ELISA-VSG modules.
#
# Features:
#   - Summary KPIs (plates, samples, QC rate, biobank match rate)
#   - Runs table with download capability
#   - Samples table with filtering and biobank match highlighting
#   - Analysis plots (PP%, DOD, CV, QC trends)
#   - Data quality summary
#
# Author: Biobank Dashboard Team
# Last Updated: 2025-11-20
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(plotly)
  library(DT)
})

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

elisa_value_boxes <- function(ns, summary_df) {
  total_samples <- summary_df$total_samples %||% 0
  qc_rate <- summary_df$qc_rate %||% NA_real_
  total_plates <- summary_df$total_plates %||% 0
  match_rate <- summary_df$match_rate %||% NA_real_

  layout_column_wrap(
    width = 1/4,
    heights_equal = "row",
    gap = "12px",
    value_box(
      title = "Total plates",
      value = scales::comma(total_plates),
      showcase = icon("microscope"),
      theme = "primary"
    ),
    value_box(
      title = "Total samples",
      value = scales::comma(total_samples),
      showcase = icon("vial"),
      theme = "info"
    ),
    value_box(
      title = "QC pass rate",
      value = if (is.na(qc_rate)) "N/A" else sprintf("%0.1f%%", qc_rate * 100),
      showcase = icon("check-circle"),
      theme = "success"
    ),
    value_box(
      title = "Biobank match rate",
      value = if (is.na(match_rate)) "N/A" else sprintf("%0.1f%%", match_rate * 100),
      showcase = icon("link"),
      theme = "warning"
    )
  )
}

elisa_runs_table <- function(ns) {
  tagList(
    div(
      style = "margin-bottom: 10px;",
      downloadButton(ns("download_runs"), "Download Runs Data", class = "btn-sm")
    ),
    DTOutput(ns("runs_table"))
  )
}

elisa_samples_filters <- function(ns) {
  layout_columns(
    col_widths = c(3, 3, 3, 3),
    dateRangeInput(ns("filter_date"), "Plate date"),
    selectInput(ns("filter_province"), "Province", choices = c("All" = "all")),
    selectInput(ns("filter_health_zone"), "Health zone", choices = c("All" = "all")),
    selectInput(ns("filter_qc"), "QC status", choices = c("All" = "all", "Pass" = "pass", "Fail" = "fail"))
  )
}

elisa_samples_table <- function(ns) {
  tagList(
    div(
      style = "margin-bottom: 10px;",
      downloadButton(ns("download_samples"), "Download Samples Data", class = "btn-sm")
    ),
    DTOutput(ns("samples_table"))
  )
}

elisa_data_quality_card <- function(ns) {
  card(
    card_header("Data Quality Summary"),
    card_body(
      uiOutput(ns("quality_summary"))
    )
  )
}

elisa_analysis_plots <- function(ns) {
  layout_columns(
    col_widths = c(6, 6),
    card(card_body(plotlyOutput(ns("plot_pp")))),
    card(card_body(plotlyOutput(ns("plot_dod")))),
    card(card_body(plotlyOutput(ns("plot_cv")))),
    card(card_body(plotlyOutput(ns("plot_qc_time"))))
  )
}

mod_elisa_generic_ui <- function(id, label) {
  ns <- NS(id)

  nav_panel(
    title = label,
    icon = icon("vial-virus"),
    navset_card_tab(
      nav_panel("Runs", value = "runs", card_body(uiOutput(ns("runs_kpis")), elisa_runs_table(ns))),
      nav_panel("Samples", value = "samples", card_body(elisa_samples_filters(ns), elisa_samples_table(ns))),
      nav_panel("Analysis", value = "analysis", card_body(elisa_data_quality_card(ns), elisa_analysis_plots(ns)))
    )
  )
}

mod_elisa_generic_server <- function(id, elisa_type, elisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_filtered <- reactive({
      df <- elisa_data()
      if (is.null(df) || !nrow(df)) return(tibble())
      df %>% filter(.data$elisa_type == elisa_type)
    })

    runs_summary <- reactive({
      df <- data_filtered()
      if (!nrow(df)) return(tibble())

      df %>%
        group_by(plate_id, plate_number, plate_date) %>%
        summarise(
          n_samples = sum(sample_type == "sample", na.rm = TRUE),
          n_controls = sum(sample_type == "control", na.rm = TRUE),
          mean_PC_DOD = mean(PC_DOD, na.rm = TRUE),
          mean_NC_DOD = mean(NC_DOD, na.rm = TRUE),
          mean_PP = mean(PP_percent, na.rm = TRUE),
          qc_overall_rate = mean(qc_overall, na.rm = TRUE),
          .groups = "drop"
        )
    })

    output$runs_kpis <- renderUI({
      df <- data_filtered()
      rs <- runs_summary()
      samples_only <- df %>% filter(sample_type == "sample")

      # Calculate biobank match rate
      match_rate <- if (nrow(samples_only) > 0 && "BiobankMatched" %in% names(samples_only)) {
        mean(samples_only$BiobankMatched, na.rm = TRUE)
      } else {
        NA_real_
      }

      elisa_value_boxes(
        ns,
        list(
          total_plates = nrow(rs),
          total_samples = nrow(samples_only),
          qc_rate = mean(samples_only$qc_overall, na.rm = TRUE),
          match_rate = match_rate
        )
      )
    })

    output$runs_table <- renderDT({
      df <- runs_summary()
      if (!nrow(df)) return(datatable(tibble(Message = "No ELISA runs found")))

      datatable(
        df,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$plot_pp <- renderPlotly({
      df <- data_filtered()
      if (!nrow(df)) return(NULL)

      p <- df %>% ggplot(aes(x = plate_number, y = PP_percent)) +
        geom_boxplot(fill = "#4F46E5", alpha = 0.5) +
        theme_minimal() +
        labs(x = "Plate", y = "PP%", title = "PP% distribution by plate")
      ggplotly(p)
    })

    output$plot_dod <- renderPlotly({
      df <- data_filtered()
      if (!nrow(df)) return(NULL)
      p <- df %>% ggplot(aes(x = plate_date, y = DOD, color = sample_type)) +
        geom_point(alpha = 0.6) +
        theme_minimal() +
        labs(x = "Date", y = "DOD", title = "DOD over time")
      ggplotly(p)
    })

    output$plot_cv <- renderPlotly({
      df <- data_filtered()
      if (!nrow(df)) return(NULL)

      # CV threshold (commonly 15-20%)
      cv_threshold <- 20

      p <- df %>%
        pivot_longer(cols = c(cv_Ag_plus, cv_Ag0), names_to = "metric", values_to = "cv") %>%
        ggplot(aes(x = metric, y = cv, fill = metric)) +
        geom_boxplot(alpha = 0.6) +
        geom_hline(yintercept = cv_threshold, linetype = "dashed", color = "red", size = 0.8) +
        annotate("text", x = 1.5, y = cv_threshold + 2,
                 label = paste0("QC threshold (", cv_threshold, "%)"),
                 color = "red", size = 3) +
        theme_minimal() +
        labs(x = NULL, y = "CV (%)", title = "CV distributions with QC threshold")
      ggplotly(p)
    })

    output$plot_qc_time <- renderPlotly({
      df <- data_filtered()
      if (!nrow(df)) return(NULL)
      p <- df %>%
        mutate(plate_date = as.Date(plate_date)) %>%
        group_by(plate_date) %>%
        summarise(pass = mean(qc_overall, na.rm = TRUE), .groups = "drop") %>%
        ggplot(aes(x = plate_date, y = pass)) +
        geom_line(color = "#10B981") +
        geom_point(color = "#10B981") +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        labs(x = "Plate date", y = "QC pass rate", title = "QC pass over time")
      ggplotly(p)
    })

    observe({
      df <- data_filtered()
      updateSelectInput(session, "filter_province", choices = c("All" = "all", sort(unique(na.omit(df$biobank_province)))))
      updateSelectInput(session, "filter_health_zone", choices = c("All" = "all", sort(unique(na.omit(df$biobank_health_zone)))))
      dates <- suppressWarnings(as.Date(df$plate_date))
      if (any(!is.na(dates))) {
        updateDateRangeInput(session, "filter_date", start = min(dates, na.rm = TRUE), end = max(dates, na.rm = TRUE))
      }
    })

    samples_filtered <- reactive({
      df <- data_filtered()
      if (!nrow(df)) return(df)

      df <- df %>% filter(sample_type == "sample")
      if (!is.null(input$filter_date[1]) && !is.na(input$filter_date[1])) {
        df <- df %>% filter(as.Date(plate_date) >= input$filter_date[1])
      }
      if (!is.null(input$filter_date[2]) && !is.na(input$filter_date[2])) {
        df <- df %>% filter(as.Date(plate_date) <= input$filter_date[2])
      }
      if (!is.null(input$filter_province) && input$filter_province != "all") {
        df <- df %>% filter(biobank_province == input$filter_province)
      }
      if (!is.null(input$filter_health_zone) && input$filter_health_zone != "all") {
        df <- df %>% filter(biobank_health_zone == input$filter_health_zone)
      }
      if (!is.null(input$filter_qc) && input$filter_qc != "all") {
        df <- df %>% filter(if (input$filter_qc == "pass") qc_overall else !qc_overall)
      }
      df
    })

    output$quality_summary <- renderUI({
      df <- data_filtered()
      if (!nrow(df)) return(p("No data available"))

      samples_only <- df %>% filter(sample_type == "sample")

      # Calculate quality metrics
      total_samples <- nrow(samples_only)
      missing_dod <- sum(is.na(samples_only$DOD))
      missing_pp <- sum(is.na(samples_only$PP_percent))
      qc_pass <- sum(samples_only$qc_overall, na.rm = TRUE)
      qc_fail <- sum(!samples_only$qc_overall, na.rm = TRUE)

      # Biobank matching
      if ("BiobankMatched" %in% names(samples_only)) {
        biobank_matched <- sum(samples_only$BiobankMatched, na.rm = TRUE)
        biobank_unmatched <- sum(!samples_only$BiobankMatched, na.rm = TRUE)
      } else {
        biobank_matched <- NA
        biobank_unmatched <- NA
      }

      # Plate validity
      invalid_plates <- df %>%
        filter(!is.na(plate_valid)) %>%
        group_by(plate_number, plate_id) %>%
        summarise(is_valid = any(plate_valid, na.rm = TRUE), .groups = "drop") %>%
        filter(!is_valid) %>%
        nrow()

      total_plates <- n_distinct(df$plate_number)

      # Generate HTML summary
      tagList(
        layout_column_wrap(
          width = 1/3,
          value_box(
            title = "QC Status",
            value = sprintf("%d / %d pass", qc_pass, total_samples),
            p(sprintf("%.1f%% pass rate", 100 * qc_pass / max(total_samples, 1))),
            theme = "success"
          ),
          value_box(
            title = "Biobank Matching",
            value = if (is.na(biobank_matched)) "N/A" else sprintf("%d / %d", biobank_matched, total_samples),
            p(if (!is.na(biobank_matched)) sprintf("%.1f%% matched", 100 * biobank_matched / max(total_samples, 1)) else ""),
            theme = "info"
          ),
          value_box(
            title = "Plate Validity",
            value = sprintf("%d / %d valid", total_plates - invalid_plates, total_plates),
            p(sprintf("%d invalid plates", invalid_plates)),
            theme = if (invalid_plates > 0) "warning" else "success"
          )
        ),
        hr(),
        h5("Data Completeness"),
        tags$ul(
          tags$li(sprintf("Missing DOD values: %d (%.1f%%)", missing_dod, 100 * missing_dod / max(total_samples, 1))),
          tags$li(sprintf("Missing PP%% values: %d (%.1f%%)", missing_pp, 100 * missing_pp / max(total_samples, 1))),
          tags$li(sprintf("Total plates processed: %d", total_plates)),
          tags$li(sprintf("Total samples: %d", total_samples))
        )
      )
    })

    output$samples_table <- renderDT({
      df <- samples_filtered()
      if (!nrow(df)) return(datatable(tibble(Message = "No ELISA samples found")))

      # Select columns, including BiobankMatched if available
      cols_to_show <- c(
        "plate_number", "plate_id", "plate_date", "sample", "numero_labo", "code_barres_kps",
        "DOD", "PP_percent", "cv_Ag_plus", "cv_Ag0", "qc_overall"
      )

      if ("BiobankMatched" %in% names(df)) {
        cols_to_show <- c(cols_to_show, "BiobankMatched")
      }

      cols_to_show <- c(cols_to_show, "biobank_province", "biobank_health_zone",
                        "biobank_sex", "biobank_age_group")

      # Keep only columns that exist in df
      cols_to_show <- intersect(cols_to_show, names(df))

      datatable(
        df %>% select(all_of(cols_to_show)),
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      ) %>%
        formatStyle(
          "BiobankMatched",
          target = "row",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#d4edda", "#f8d7da"))
        )
    })

    # Download handlers
    output$download_runs <- downloadHandler(
      filename = function() {
        paste0("elisa_runs_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(runs_summary(), file, row.names = FALSE)
      }
    )

    output$download_samples <- downloadHandler(
      filename = function() {
        paste0("elisa_samples_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(samples_filtered(), file, row.names = FALSE)
      }
    )

  })
}
