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
    # Add controls above the tabs
    card(
      card_body(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center",
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
      # Trigger refresh when button is clicked
      input$refresh_data

      message("Loading iELISA data...")

      tryCatch({
        data <- load_ielisa_data(
          ielisa_dir = "data/ielisa",
          cache_dir = "data/ielisa_cache"
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

    # Filtered data (apply plate validity filter if needed)
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
