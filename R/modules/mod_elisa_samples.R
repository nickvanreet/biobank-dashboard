# R/modules/mod_elisa_samples.R
# ELISA Samples module - Sample-level details with filtering
# Modeled after mod_05c_mic_samples.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
})

#' ELISA Samples UI
#' @param id Module namespace ID
#' @export
mod_elisa_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Filters
    card(
      card_header("Filters"),
      card_body(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          dateRangeInput(
            ns("filter_date"),
            "Plate Date Range",
            start = NULL,
            end = NULL
          ),
          selectInput(
            ns("filter_province"),
            "Province",
            choices = c("All" = "all")
          ),
          selectInput(
            ns("filter_health_zone"),
            "Health Zone",
            choices = c("All" = "all")
          ),
          selectInput(
            ns("filter_qc"),
            "QC Status",
            choices = c("All" = "all", "Pass" = "pass", "Fail" = "fail")
          )
        )
      )
    ),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Summary info
    uiOutput(ns("summary_info")),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Samples table
    card(
      card_header("Sample Results"),
      card_body(
        DTOutput(ns("samples_table"))
      )
    )
  )
}

#' ELISA Samples Server
#' @param id Module namespace ID
#' @param elisa_data Reactive returning ELISA sample data
#' @export
mod_elisa_samples_server <- function(id, elisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # UPDATE FILTER CHOICES DYNAMICALLY
    # ========================================================================

    observe({
      data <- elisa_data()

      # Update date range
      if ("plate_date" %in% names(data) && nrow(data) > 0) {
        dates <- as.Date(data$plate_date)
        dates <- dates[!is.na(dates)]
        if (length(dates) > 0) {
          updateDateRangeInput(
            session,
            "filter_date",
            start = min(dates),
            end = max(dates)
          )
        }
      }

      # Update province choices
      if ("Province" %in% names(data)) {
        provinces <- sort(unique(na.omit(data$Province)))
        updateSelectInput(
          session,
          "filter_province",
          choices = c("All" = "all", setNames(provinces, provinces))
        )
      }

      # Update health zone choices
      if ("HealthZone" %in% names(data)) {
        zones <- sort(unique(na.omit(data$HealthZone)))
        updateSelectInput(
          session,
          "filter_health_zone",
          choices = c("All" = "all", setNames(zones, zones))
        )
      }
    })

    # ========================================================================
    # APPLY LOCAL FILTERS
    # ========================================================================

    samples_filtered <- reactive({
      data <- elisa_data()
      if (!nrow(data)) return(data)

      # Date filter
      if ("plate_date" %in% names(data)) {
        if (!is.null(input$filter_date) && length(input$filter_date) == 2) {
          if (!is.na(input$filter_date[1])) {
            data <- data %>% filter(as.Date(plate_date) >= input$filter_date[1])
          }
          if (!is.na(input$filter_date[2])) {
            data <- data %>% filter(as.Date(plate_date) <= input$filter_date[2])
          }
        }
      }

      # Province filter
      if ("Province" %in% names(data) &&
          !is.null(input$filter_province) &&
          input$filter_province != "all") {
        data <- data %>% filter(Province == input$filter_province)
      }

      # Health zone filter
      if ("HealthZone" %in% names(data) &&
          !is.null(input$filter_health_zone) &&
          input$filter_health_zone != "all") {
        data <- data %>% filter(HealthZone == input$filter_health_zone)
      }

      # QC filter
      if ("qc_overall" %in% names(data) &&
          !is.null(input$filter_qc) &&
          input$filter_qc != "all") {
        if (input$filter_qc == "pass") {
          data <- data %>% filter(qc_overall == TRUE)
        } else {
          data <- data %>% filter(qc_overall == FALSE | is.na(qc_overall))
        }
      }

      data
    })

    # ========================================================================
    # SUMMARY INFO
    # ========================================================================

    output$summary_info <- renderUI({
      data <- samples_filtered()

      n_total <- nrow(data)
      n_matched <- if ("BiobankMatched" %in% names(data)) {
        sum(data$BiobankMatched, na.rm = TRUE)
      } else {
        0
      }
      match_pct <- if (n_total > 0) round(100 * n_matched / n_total, 1) else 0

      n_qc_pass <- if ("qc_overall" %in% names(data)) {
        sum(data$qc_overall, na.rm = TRUE)
      } else {
        0
      }
      qc_pct <- if (n_total > 0) round(100 * n_qc_pass / n_total, 1) else 0

      layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        value_box(
          title = "Samples (filtered)",
          value = scales::comma(n_total),
          showcase = icon("filter"),
          theme = "secondary"
        ),
        value_box(
          title = "Biobank Match",
          value = sprintf("%d (%.1f%%)", n_matched, match_pct),
          showcase = icon("link"),
          theme = if (match_pct >= 80) "success" else if (match_pct >= 50) "warning" else "danger"
        ),
        value_box(
          title = "QC Pass",
          value = sprintf("%d (%.1f%%)", n_qc_pass, qc_pct),
          showcase = icon("check"),
          theme = if (qc_pct >= 90) "success" else if (qc_pct >= 70) "warning" else "danger"
        )
      )
    })

    # ========================================================================
    # SAMPLES TABLE
    # ========================================================================

    output$samples_table <- renderDT({
      data <- samples_filtered()

      if (!nrow(data)) {
        return(datatable(
          tibble(Message = "No samples found matching filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Select columns to display (only those that exist)
      available_cols <- c(
        "plate_number", "plate_date", "sample", "numero_labo", "code_barres_kps",
        "DOD", "PP_percent", "cv_Ag_plus", "cv_Ag0", "qc_overall",
        "Province", "HealthZone", "Structure", "Sex", "AgeGroup", "BiobankMatched"
      )

      cols_to_select <- available_cols[available_cols %in% names(data)]

      display_data <- data %>%
        select(all_of(cols_to_select)) %>%
        mutate(
          plate_date = if ("plate_date" %in% names(.)) as.Date(plate_date) else NA,
          DOD = if ("DOD" %in% names(.)) round(DOD, 3) else NA,
          PP_percent = if ("PP_percent" %in% names(.)) round(PP_percent, 1) else NA,
          cv_Ag_plus = if ("cv_Ag_plus" %in% names(.)) round(cv_Ag_plus, 1) else NA,
          cv_Ag0 = if ("cv_Ag0" %in% names(.)) round(cv_Ag0, 1) else NA,
          qc_overall = if ("qc_overall" %in% names(.)) ifelse(qc_overall, "Pass", "Fail") else NA
        )

      # Rename columns for better display
      col_names <- names(display_data)
      col_names <- gsub("_", " ", col_names)
      col_names <- tools::toTitleCase(col_names)
      names(display_data) <- col_names

      datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          scrollCollapse = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        filter = 'top',
        class = "table table-striped table-hover table-sm"
      ) %>%
        formatStyle(
          columns = which(names(display_data) == "Qc Overall"),
          backgroundColor = styleEqual(c("Pass", "Fail"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          columns = which(names(display_data) == "Biobankmatched"),
          backgroundColor = styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da'))
        )
    })
  })
}
