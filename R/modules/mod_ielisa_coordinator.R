# R/modules/mod_ielisa_coordinator.R
# iELISA Coordinator module - Brings together all iELISA sub-modules

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
})

#' iELISA Coordinator UI
#' @param id Module namespace ID
#' @param label Display label (default: "iELISA")
#' @export
mod_ielisa_coordinator_ui <- function(id, label = "iELISA") {
  ns <- NS(id)

  # Return navigation panel with three sub-tabs
  nav_panel(
    title = label,
    icon = icon("vial-circle-check"),

    # Control QC Settings (collapsible)
    accordion(
      accordion_panel(
        title = "Control QC Settings",
        icon = icon("sliders"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          div(
            tags$label(class = "form-label mb-1", "Negative Control OD Range"),
            div(
              class = "d-flex gap-2 align-items-center",
              numericInput(ns("neg_od_min"), "Min", value = 1, min = 0, max = 5, step = 0.1, width = "100px"),
              tags$span("-"),
              numericInput(ns("neg_od_max"), "Max", value = 3, min = 0, max = 5, step = 0.1, width = "100px")
            ),
            tags$small(class = "text-muted", "Acceptable NEG control OD range")
          ),
          div(
            tags$label(class = "form-label mb-1", "Positive Control OD Range"),
            div(
              class = "d-flex gap-2 align-items-center",
              numericInput(ns("pos_od_min"), "Min", value = 0.3, min = 0, max = 5, step = 0.1, width = "100px"),
              tags$span("-"),
              numericInput(ns("pos_od_max"), "Max", value = 0.7, min = 0, max = 5, step = 0.1, width = "100px")
            ),
            tags$small(class = "text-muted", "Acceptable POS control OD range")
          ),
          div(
            tags$label(class = "form-label mb-1", "Control Inhibition % Range"),
            div(
              class = "d-flex gap-2 align-items-center",
              numericInput(ns("ctrl_inh_min"), "Min", value = 50, min = 0, max = 100, step = 5, width = "100px"),
              tags$span("-"),
              numericInput(ns("ctrl_inh_max"), "Max", value = 80, min = 0, max = 100, step = 5, width = "100px")
            ),
            tags$small(class = "text-muted", "Acceptable POS control inhibition range")
          ),
          div(
            tags$label(class = "form-label mb-1", "Control CV Threshold (%)"),
            numericInput(ns("ctrl_cv_max"), label = NULL, value = 20, min = 0, max = 100, step = 5, width = "100px"),
            tags$small(class = "text-muted", "Maximum allowed CV for controls")
          )
        )
      )
    ),

    # Add controls above the tabs
    card(
      card_body(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-3",
          div(
            class = "d-flex flex-column justify-content-end",
            checkboxInput(
              ns("exclude_invalid_plates"),
              "Exclude invalid plates",
              value = FALSE
            ),
            tags$small(
              class = "text-muted",
              "Removes plates with failed control QC from all analyses"
            )
          ),
          div(
            class = "d-flex flex-column",
            tags$label(
              class = "form-label mb-1",
              "Positivity Threshold (% inhibition)"
            ),
            numericInput(
              ns("positivity_threshold"),
              label = NULL,
              value = 30,
              min = 0,
              max = 100,
              step = 5,
              width = "120px"
            ),
            tags$small(
              class = "text-muted",
              "Samples ≥ threshold are positive"
            )
          ),
          div(
            class = "d-flex flex-column",
            tags$label(
              class = "form-label mb-1",
              "Inhibition Formula"
            ),
            radioButtons(
              ns("formula_choice"),
              label = NULL,
              choices = c(
                "Formula 1 (NEG-based)" = "f1",
                "Formula 2 (NEG-POS normalized)" = "f2"
              ),
              selected = "f2",
              inline = FALSE
            ),
            tags$small(
              class = "text-muted",
              "Formula for calculating % inhibition"
            )
          ),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(
              ns("refresh_data"),
              "Refresh Data",
              icon = icon("arrows-rotate"),
              class = "btn-sm btn-outline-primary"
            ),
            tags$span(
              class = "text-muted",
              style = "font-size: 0.875rem;",
              textOutput(ns("data_status"), inline = TRUE)
            )
          )
        )
      )
    ),
    navset_card_tab(
      id = ns("ielisa_tabs"),
      # Tab 1: Runs (overview with KPIs)
      nav_panel(
        title = "Overview",
        value = "runs",
        mod_ielisa_runs_ui(ns("runs"))
      ),
      # Tab 2: Samples
      nav_panel(
        title = "Samples",
        value = "samples",
        mod_ielisa_samples_ui(ns("samples"))
      ),
      # Tab 3: Analysis (controls and duplicates)
      nav_panel(
        title = "Analysis & Duplicates",
        value = "analysis",
        mod_ielisa_analysis_ui(ns("analysis"))
      )
    )
  )
}

#' iELISA Coordinator Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data frame (optional, for future integration)
#' @param filters Reactive returning global filters (optional, for future integration)
#' @export
mod_ielisa_coordinator_server <- function(id, biobank_df = reactive(NULL), filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # LOAD iELISA DATA
    # ========================================================================

    # Raw data loading
    raw_ielisa_data <- reactive({
      # Trigger refresh when button is clicked or when control QC settings change
      input$refresh_data
      input$neg_od_min
      input$neg_od_max
      input$pos_od_min
      input$pos_od_max
      input$ctrl_inh_min
      input$ctrl_inh_max
      input$ctrl_cv_max

      message("Loading iELISA data...")

      tryCatch({
        data <- load_ielisa_data(
          ielisa_dir = "data/ielisa",
          cache_dir = "data/ielisa_cache",
          # Control QC parameters
          neg_od_min = input$neg_od_min,
          neg_od_max = input$neg_od_max,
          pos_od_min = input$pos_od_min,
          pos_od_max = input$pos_od_max,
          ctrl_inh_min = input$ctrl_inh_min,
          ctrl_inh_max = input$ctrl_inh_max,
          ctrl_cv_max = input$ctrl_cv_max
        )

        if (nrow(data) == 0) {
          showNotification(
            "No iELISA data found. Please check the data/ielisa directory.",
            type = "warning",
            duration = 5
          )
        } else {
          message("Successfully loaded ", nrow(data), " iELISA samples")
        }

        data
      }, error = function(e) {
        showNotification(
          paste("Error loading iELISA data:", e$message),
          type = "error",
          duration = 10
        )
        tibble()
      })
    })

    # Filtered data (apply plate validity filter and custom QC settings)
    filtered_ielisa_data <- reactive({
      data <- raw_ielisa_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Apply exclude invalid plates filter
      if (input$exclude_invalid_plates) {
        data <- data %>%
          filter(plate_valid_L13 == TRUE | plate_valid_L15 == TRUE)
      }

      # Apply custom QC settings (threshold and formula)
      data <- apply_custom_qc(
        data,
        threshold = input$positivity_threshold,
        formula = input$formula_choice
      )

      data
    })

    # ========================================================================
    # DATA STATUS
    # ========================================================================

    output$data_status <- renderText({
      data <- raw_ielisa_data()

      if (nrow(data) == 0) {
        "No data loaded"
      } else {
        n_files <- n_distinct(data$file)
        n_samples <- nrow(data)
        sprintf("Loaded: %d files, %d samples", n_files, n_samples)
      }
    })

    # ========================================================================
    # CALL SUB-MODULES
    # ========================================================================

    # Runs (overview) module
    mod_ielisa_runs_server(
      "runs",
      ielisa_data = filtered_ielisa_data
    )

    # Samples module
    mod_ielisa_samples_server(
      "samples",
      ielisa_data = filtered_ielisa_data
    )

    # Analysis module (duplicates)
    mod_ielisa_analysis_server(
      "analysis",
      ielisa_data = filtered_ielisa_data
    )

    # ========================================================================
    # RETURN DATA FOR OTHER MODULES (if needed)
    # ========================================================================

    return(list(
      ielisa_data = filtered_ielisa_data,
      raw_data = raw_ielisa_data
    ))

  })
}
