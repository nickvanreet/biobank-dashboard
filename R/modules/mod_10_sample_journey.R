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
if (!exists("generate_sample_journey_pdf_simple")) {
  source("R/sampleJourneyPdfExport.R")
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
              width = 1/3,
              textInput(
                ns("barcode_search"),
                "Search by Barcode",
                placeholder = "e.g., KPS12345",
                width = "100%"
              ),
              textInput(
                ns("numero_search"),
                "Search by Lab Number",
                placeholder = "e.g., LAB001",
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

        # Download PDF button
        uiOutput(ns("download_section")),

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
      # Get both inputs
      barcode_val <- trimws(input$barcode_search)
      numero_val <- trimws(input$numero_search)

      # Determine which input to use (prioritize barcode if both filled)
      sample_id <- if (!is.null(barcode_val) && barcode_val != "") {
        barcode_val
      } else if (!is.null(numero_val) && numero_val != "") {
        numero_val
      } else {
        ""
      }

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

    # Download section
    output$download_section <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found) {
        return(NULL)
      }

      card(
        class = "mb-3",
        card_header(
          div(
            class = "d-flex justify-content-between align-items-center",
            tags$span("Export Report"),
            downloadButton(
              ns("download_pdf"),
              "Download PDF",
              icon = icon("file-pdf"),
              class = "btn-primary"
            )
          )
        )
      )
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

      # Get sample identifiers
      ext_barcode <- if ("barcode" %in% names(extraction) && !is.na(extraction$barcode)) {
        as.character(extraction$barcode)
      } else {
        NA_character_
      }

      ext_numero <- if ("numero" %in% names(extraction) && !is.na(extraction$numero)) {
        as.character(extraction$numero)
      } else {
        NA_character_
      }

      ext_sample_id <- if ("sample_id" %in% names(extraction) && !is.na(extraction$sample_id)) {
        as.character(extraction$sample_id)
      } else {
        NA_character_
      }

      tagList(
        tags$h6(sprintf("Extraction (%d total)", nrow(data$extraction_data)), class = "fw-bold"),
        tags$p(tags$strong("Latest Date: "), ext_date),

        # Sample Identifiers Section
        if (!is.na(ext_barcode) || !is.na(ext_numero) || !is.na(ext_sample_id)) {
          tags$div(
            class = "mb-3 p-2",
            style = "background-color: rgba(16, 185, 129, 0.05); border-left: 3px solid #10B981;",
            if (!is.na(ext_barcode)) {
              tags$p(
                class = "mb-1",
                tags$small(
                  tags$strong("Barcode: "),
                  tags$code(class = "text-success", ext_barcode)
                )
              )
            },
            if (!is.na(ext_numero)) {
              tags$p(
                class = "mb-1",
                tags$small(
                  tags$strong("Lab Number: "),
                  tags$code(class = "text-success", ext_numero)
                )
              )
            },
            if (!is.na(ext_sample_id) && ext_sample_id != ext_barcode && ext_sample_id != ext_numero) {
              tags$p(
                class = "mb-0",
                tags$small(
                  tags$strong("Sample ID: "),
                  tags$code(class = "text-success", ext_sample_id)
                )
              )
            }
          )
        },

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

    # Download PDF handler
    output$download_pdf <- downloadHandler(
      filename = function() {
        data <- journey_data()
        sample_id <- if (!is.null(data) && data$found) {
          # Get the search input that was used
          barcode_val <- trimws(input$barcode_search)
          numero_val <- trimws(input$numero_search)

          if (!is.null(barcode_val) && barcode_val != "") {
            barcode_val
          } else if (!is.null(numero_val) && numero_val != "") {
            numero_val
          } else {
            "unknown"
          }
        } else {
          "unknown"
        }

        sprintf("sample_journey_%s_%s.pdf",
                gsub("[^A-Za-z0-9]", "_", sample_id),
                format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        data <- journey_data()
        req(data, data$found)

        # Get the sample ID
        barcode_val <- trimws(input$barcode_search)
        numero_val <- trimws(input$numero_search)

        sample_id <- if (!is.null(barcode_val) && barcode_val != "") {
          barcode_val
        } else if (!is.null(numero_val) && numero_val != "") {
          numero_val
        } else {
          "unknown"
        }

        # Show progress
        withProgress(message = "Generating PDF report...", value = 0, {
          incProgress(0.2, detail = "Preparing data...")

          # Generate the PDF using the modern Quarto template
          incProgress(0.3, detail = "Rendering report...")

          tryCatch({
            pdf_path <- render_sample_journey_report(sample_id, data)

            incProgress(0.4, detail = "Finalizing...")

            # Copy the generated PDF to the download location
            file.copy(pdf_path, file, overwrite = TRUE)

            incProgress(1.0, detail = "Complete!")
          }, error = function(e) {
            showNotification(
              paste("Error generating PDF:", e$message),
              type = "error",
              duration = 10
            )
            stop(e)
          })
        })
      }
    )
  })
}
