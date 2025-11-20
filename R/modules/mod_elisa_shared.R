# Shared helpers for ELISA modules

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

  layout_column_wrap(
    width = 1/3,
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
    )
  )
}

elisa_runs_table <- function(ns) {
  DTOutput(ns("runs_table"))
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
  DTOutput(ns("samples_table"))
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
      nav_panel("Analysis", value = "analysis", card_body(elisa_analysis_plots(ns)))
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
      elisa_value_boxes(
        ns,
        list(
          total_plates = nrow(rs),
          total_samples = sum(df$sample_type == "sample", na.rm = TRUE),
          qc_rate = mean(df$qc_overall, na.rm = TRUE)
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
      p <- df %>%
        pivot_longer(cols = c(cv_Ag_plus, cv_Ag0), names_to = "metric", values_to = "cv") %>%
        ggplot(aes(x = metric, y = cv, fill = metric)) +
        geom_boxplot(alpha = 0.6) +
        theme_minimal() +
        labs(x = NULL, y = "CV", title = "CV distributions")
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

    output$samples_table <- renderDT({
      df <- samples_filtered()
      if (!nrow(df)) return(datatable(tibble(Message = "No ELISA samples found")))

      datatable(
        df %>% select(
          plate_number, plate_id, plate_date, sample, numero_labo, code_barres_kps,
          DOD, PP_percent, cv_Ag_plus, cv_Ag0, qc_overall,
          biobank_province, biobank_health_zone, biobank_sex, biobank_age_group
        ),
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

  })
}
