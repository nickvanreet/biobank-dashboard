# ==============================================================================
# MODULE 10: SAMPLE JOURNEY - Individual sample tracking across all tests
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(plotly)
})

# Source helper and visualization functions
if (!exists("gather_sample_journey")) {
  source("R/sampleJourneyHelpers.R")
}
if (!exists("plot_sample_timeline")) {
  source("R/sampleJourneyVisualizations.R")
}

#' Sample Journey UI
#' @param id Module namespace ID
#' @export
mod_sample_journey_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Sample Journey",
    icon = icon("route"),

    tagList(
      tags$style(HTML("
        .sample-journey-panel {
          max-height: calc(100vh - 160px);
          overflow-y: auto;
          padding-right: 0.5rem;
          padding-bottom: 1rem;
        }

        .sample-journey-panel::-webkit-scrollbar {
          width: 8px;
        }

        .sample-journey-panel::-webkit-scrollbar-thumb {
          background-color: rgba(79, 70, 229, 0.4);
          border-radius: 4px;
        }

        .sample-journey-panel::-webkit-scrollbar-track {
          background: transparent;
        }

        .alert-card {
          border-left: 4px solid;
          margin-bottom: 0.5rem;
        }

        .alert-card.alert-danger {
          border-left-color: #e41a1c;
        }

        .alert-card.alert-warning {
          border-left-color: #ff7f00;
        }

        .alert-card.alert-info {
          border-left-color: #06B6D4;
        }
      ")),

      div(
        class = "sample-journey-panel",

        # Search box
        card(
          class = "mb-3",
          card_header("Search for Sample"),
          card_body(
            layout_column_wrap(
              width = 1/2,
              textInput(
                ns("sample_search"),
                "Enter Sample ID (Barcode or Numero)",
                placeholder = "e.g., KPS12345 or LAB001",
                width = "100%"
              ),
              div(
                style = "padding-top: 25px;",
                actionButton(
                  ns("search_btn"),
                  "Search",
                  icon = icon("search"),
                  class = "btn-primary",
                  width = "100%"
                )
              )
            ),
            uiOutput(ns("search_status"))
          )
        ),

        # Timeline visualization
        uiOutput(ns("timeline_card")),

        # Results layout (4 columns)
        uiOutput(ns("results_section")),

        # Alerts section
        uiOutput(ns("alerts_section"))
      )
    )
  )
}

#' Sample Journey Server
#' @param id Module namespace ID
#' @param biobank_data Reactive returning biobank data
#' @param extraction_data Reactive returning extraction data
#' @param mic_data Reactive returning MIC data
#' @param elisa_pe_data Reactive returning ELISA PE data
#' @param elisa_vsg_data Reactive returning ELISA VSG data
#' @param ielisa_data Reactive returning iELISA data
#' @export
mod_sample_journey_server <- function(id, biobank_data, extraction_data, mic_data,
                                       elisa_pe_data, elisa_vsg_data, ielisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to store journey data
    journey_data <- reactiveVal(NULL)

    # Search button handler
    observeEvent(input$search_btn, {
      req(input$sample_search)

      sample_id <- trimws(input$sample_search)

      if (sample_id == "") {
        journey_data(NULL)
        return()
      }

      # Gather data from all sources
      withProgress(message = "Searching for sample...", value = 0, {
        incProgress(0.2, detail = "Checking biobank...")
        biobank_df <- tryCatch(biobank_data(), error = function(e) NULL)

        incProgress(0.2, detail = "Checking extractions...")
        extraction_df <- tryCatch(extraction_data(), error = function(e) NULL)

        incProgress(0.2, detail = "Checking MIC tests...")
        mic_df <- tryCatch(mic_data(), error = function(e) NULL)

        incProgress(0.1, detail = "Checking ELISA tests...")
        elisa_pe_df <- tryCatch(elisa_pe_data(), error = function(e) NULL)
        elisa_vsg_df <- tryCatch(elisa_vsg_data(), error = function(e) NULL)

        incProgress(0.1, detail = "Checking iELISA tests...")
        ielisa_df <- tryCatch(ielisa_data(), error = function(e) NULL)

        incProgress(0.2, detail = "Compiling results...")

        # Gather journey data
        result <- gather_sample_journey(
          sample_id = sample_id,
          biobank_df = biobank_df,
          extraction_df = extraction_df,
          mic_df = mic_df,
          elisa_pe_df = elisa_pe_df,
          elisa_vsg_df = elisa_vsg_df,
          ielisa_df = ielisa_df
        )

        journey_data(result)
      })
    })

    # Search status message
    output$search_status <- renderUI({
      data <- journey_data()

      if (is.null(data)) {
        return(NULL)
      }

      if (!data$found) {
        return(
          tags$div(
            class = "alert alert-warning mt-3",
            icon("exclamation-triangle"),
            " No data found for this sample ID. Please check the ID and try again."
          )
        )
      } else {
        # Count data sources
        n_sources <- sum(
          !is.null(data$biobank_info) && nrow(data$biobank_info) > 0,
          nrow(data$extraction_data) > 0,
          nrow(data$mic_data) > 0,
          nrow(data$elisa_pe_data) > 0,
          nrow(data$elisa_vsg_data) > 0,
          nrow(data$ielisa_data) > 0
        )

        return(
          tags$div(
            class = "alert alert-success mt-3",
            icon("check-circle"),
            sprintf(" Found data in %d source(s)", n_sources)
          )
        )
      }
    })

    # Timeline card
    output$timeline_card <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found) {
        return(NULL)
      }

      card(
        class = "mb-3",
        card_header("Sample Timeline"),
        card_body(
          plotlyOutput(ns("timeline_plot"), height = "400px")
        )
      )
    })

    # Timeline plot
    output$timeline_plot <- renderPlotly({
      data <- journey_data()
      req(data, data$found)

      plot_sample_timeline(data$timeline)
    })

    # Results section (4-column layout)
    output$results_section <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found) {
        return(NULL)
      }

      card(
        class = "mb-3",
        card_header("Test Results"),
        card_body(
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",

            # Column 1: Biobank & Extraction/QC
            card(
              card_header("Biobank & Extraction"),
              card_body(
                uiOutput(ns("biobank_info")),
                tags$hr(),
                uiOutput(ns("extraction_info"))
              ),
              height = "700px",
              style = "overflow-y: auto;"
            ),

            # Column 2: MIC
            card(
              card_header("MIC qPCR"),
              card_body(
                uiOutput(ns("mic_results"))
              ),
              height = "700px",
              style = "overflow-y: auto;"
            ),

            # Column 3: ELISA
            card(
              card_header("ELISA (PE/VSG)"),
              card_body(
                uiOutput(ns("elisa_cards"))
              ),
              height = "700px",
              style = "overflow-y: auto;"
            ),

            # Column 4: iELISA
            card(
              card_header("iELISA"),
              card_body(
                uiOutput(ns("ielisa_results"))
              ),
              height = "700px",
              style = "overflow-y: auto;"
            )
          )
        )
      )
    })

    # Biobank info
    output$biobank_info <- renderUI({
      data <- journey_data()
      req(data)

      if (is.null(data$biobank_info) || nrow(data$biobank_info) == 0) {
        return(tags$p(class = "text-muted", "No biobank record"))
      }

      info <- data$biobank_info[1, ]

      province <- if ("province" %in% names(info)) info$province else "Unknown"
      health_zone <- if ("health_zone" %in% names(info)) info$health_zone else "Unknown"
      structure <- if ("health_structure" %in% names(info)) {
        info$health_structure
      } else if ("structure_sanitaire" %in% names(info)) {
        info$structure_sanitaire
      } else {
        "Unknown"
      }

      date_sample <- if ("date_sample" %in% names(info)) {
        format(as.Date(info$date_sample), "%Y-%m-%d")
      } else if (".__date_sample" %in% names(info)) {
        format(as.Date(info$.__date_sample), "%Y-%m-%d")
      } else {
        "Unknown"
      }

      date_sent_inrb <- if ("date_sent_inrb" %in% names(info)) {
        format(as.Date(info$date_sent_inrb), "%Y-%m-%d")
      } else if ("date_envoi_inrb" %in% names(info)) {
        format(as.Date(info$date_envoi_inrb), "%Y-%m-%d")
      } else if ("date_inrb" %in% names(info)) {
        format(as.Date(info$date_inrb), "%Y-%m-%d")
      } else {
        "Not recorded"
      }

      tagList(
        tags$h6("Biobank Information", class = "fw-bold"),
        tags$p(tags$strong("Province: "), province),
        tags$p(tags$strong("Health Zone: "), health_zone),
        tags$p(tags$strong("Structure: "), structure),
        tags$p(tags$strong("Collection Date: "), date_sample),
        tags$p(tags$strong("Date sent to INRB: "), date_sent_inrb)
      )
    })

    # Extraction info with DRS gauge
    output$extraction_info <- renderUI({
      data <- journey_data()
      req(data)

      if (nrow(data$extraction_data) == 0) {
        return(tags$p(class = "text-muted", "No extraction records"))
      }

      # Show most recent extraction
      extraction <- data$extraction_data[nrow(data$extraction_data), ]

      drs_volume <- if ("drs_volume_ml" %in% names(extraction)) {
        extraction$drs_volume_ml
      } else {
        NA_real_
      }

      ext_date <- if ("extraction_date" %in% names(extraction)) {
        format(as.Date(extraction$extraction_date), "%Y-%m-%d")
      } else {
        "Unknown"
      }

      tagList(
        tags$h6(sprintf("Extraction (%d total)", nrow(data$extraction_data)), class = "fw-bold"),
        tags$p(tags$strong("Latest Date: "), ext_date),
        if (!is.na(drs_volume)) {
          plotlyOutput(ns("drs_gauge"), height = "200px")
        } else {
          tags$p(class = "text-muted", "DRS volume not recorded")
        }
      )
    })

    # DRS gauge
    output$drs_gauge <- renderPlotly({
      data <- journey_data()
      req(data, nrow(data$extraction_data) > 0)

      extraction <- data$extraction_data[nrow(data$extraction_data), ]
      drs_volume <- if ("drs_volume_ml" %in% names(extraction)) {
        extraction$drs_volume_ml
      } else {
        NA_real_
      }

      plot_drs_gauge(drs_volume)
    })

    # MIC detailed results
    output$mic_results <- renderUI({
      data <- journey_data()
      req(data)

      if (nrow(data$mic_data) == 0) {
        return(tags$p(class = "text-muted", "No MIC tests"))
      }

      plot_mic_detailed(data$mic_data)
    })

    # ELISA cards
    output$elisa_cards <- renderUI({
      data <- journey_data()
      req(data)

      pe_cards <- if (nrow(data$elisa_pe_data) > 0) {
        plot_elisa_cards(data$elisa_pe_data, "PE")
      } else {
        NULL
      }

      vsg_cards <- if (nrow(data$elisa_vsg_data) > 0) {
        plot_elisa_cards(data$elisa_vsg_data, "VSG")
      } else {
        NULL
      }

      if (is.null(pe_cards) && is.null(vsg_cards)) {
        return(tags$p(class = "text-muted", "No ELISA tests"))
      }

      tagList(
        if (!is.null(pe_cards)) {
          tagList(
            tags$h6("ELISA PE", class = "fw-bold mb-3"),
            pe_cards
          )
        },
        if (!is.null(vsg_cards)) {
          tagList(
            tags$h6("ELISA VSG", class = "fw-bold mb-3 mt-3"),
            vsg_cards
          )
        }
      )
    })

    # iELISA results
    output$ielisa_results <- renderUI({
      data <- journey_data()
      req(data)

      if (nrow(data$ielisa_data) == 0) {
        return(tags$p(class = "text-muted", "No iELISA tests"))
      }

      plot_ielisa_results(data$ielisa_data)
    })

    # Alerts section
    output$alerts_section <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found || length(data$alerts) == 0) {
        return(NULL)
      }

      alert_cards <- lapply(data$alerts, function(alert) {
        alert_class <- switch(
          alert$type,
          "danger" = "alert-danger",
          "warning" = "alert-warning",
          "info" = "alert-info",
          "alert-secondary"
        )

        alert_icon <- switch(
          alert$type,
          "danger" = icon("exclamation-circle"),
          "warning" = icon("exclamation-triangle"),
          "info" = icon("info-circle"),
          icon("circle")
        )

        tags$div(
          class = sprintf("alert %s alert-card", alert_class),
          role = "alert",
          tags$strong(alert$category, ": "),
          alert_icon,
          " ",
          alert$message
        )
      })

      card(
        class = "mb-3",
        card_header(
          sprintf("Quality Alerts (%d)", length(data$alerts)),
          class = "bg-warning text-dark"
        ),
        card_body(
          tagList(alert_cards)
        )
      )
    })
  })
}
