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
if (!exists("render_sample_journey_report")) {
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

        /* Unified info box styling for consistent look */
        .sj-info-box {
          background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
          border: 1px solid #e2e8f0;
          border-radius: 8px;
          padding: 12px 16px;
          margin-bottom: 12px;
          transition: all 0.2s ease;
        }

        .sj-info-box:hover {
          border-color: #cbd5e1;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        }

        .sj-info-box .sj-label {
          font-size: 0.75rem;
          font-weight: 600;
          color: #64748b;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin-bottom: 4px;
        }

        .sj-info-box .sj-value {
          font-size: 0.95rem;
          font-weight: 500;
          color: #1e293b;
        }

        .sj-info-box.sj-highlight {
          border-left: 4px solid;
        }

        .sj-info-box.sj-highlight-primary { border-left-color: #4F46E5; }
        .sj-info-box.sj-highlight-success { border-left-color: #10B981; }
        .sj-info-box.sj-highlight-warning { border-left-color: #F59E0B; }
        .sj-info-box.sj-highlight-danger { border-left-color: #e41a1c; }
        .sj-info-box.sj-highlight-info { border-left-color: #06B6D4; }

        /* Case status badges */
        .sj-case-badge {
          display: inline-flex;
          align-items: center;
          padding: 6px 12px;
          border-radius: 20px;
          font-size: 0.85rem;
          font-weight: 600;
        }

        .sj-case-badge.new-case {
          background-color: rgba(16, 185, 129, 0.15);
          color: #059669;
        }

        .sj-case-badge.treated-case {
          background-color: rgba(245, 158, 11, 0.15);
          color: #D97706;
        }

        .sj-case-badge.ancient-case {
          background-color: rgba(139, 92, 246, 0.15);
          color: #7C3AED;
        }

        .sj-case-badge.unknown-case {
          background-color: rgba(100, 116, 139, 0.15);
          color: #475569;
        }

        /* Extraction item styling */
        .sj-extraction-item {
          background: #fff;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
          padding: 12px;
          margin-bottom: 8px;
        }

        .sj-extraction-item:last-child {
          margin-bottom: 0;
        }

        .sj-extraction-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          margin-bottom: 8px;
          padding-bottom: 8px;
          border-bottom: 1px solid #f1f5f9;
        }

        /* DRS state indicators */
        .sj-drs-indicator {
          display: inline-flex;
          align-items: center;
          gap: 4px;
          padding: 3px 8px;
          border-radius: 4px;
          font-size: 0.75rem;
          font-weight: 600;
        }

        .sj-drs-liquid { background: rgba(16, 185, 129, 0.15); color: #059669; }
        .sj-drs-viscous { background: rgba(245, 158, 11, 0.15); color: #D97706; }
        .sj-drs-coagulated { background: rgba(228, 26, 28, 0.15); color: #e41a1c; }
        .sj-drs-unknown { background: rgba(100, 116, 139, 0.15); color: #475569; }

        .sj-quality-clear { background: rgba(16, 185, 129, 0.15); color: #059669; }
        .sj-quality-fonce { background: rgba(245, 158, 11, 0.15); color: #D97706; }
        .sj-quality-echec { background: rgba(228, 26, 28, 0.15); color: #e41a1c; }
        .sj-quality-unknown { background: rgba(100, 116, 139, 0.15); color: #475569; }

        /* Compact test result cards */
        .sj-test-card {
          background: #fff;
          border: 1px solid #e5e7eb;
          border-radius: 8px;
          margin-bottom: 10px;
          overflow: hidden;
        }

        .sj-test-card-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          padding: 10px 14px;
          background: #f8fafc;
          border-bottom: 1px solid #e5e7eb;
        }

        .sj-test-card-body {
          padding: 12px 14px;
        }

        .sj-test-card-body .row {
          margin-left: -6px;
          margin-right: -6px;
        }

        .sj-test-card-body .row > div {
          padding-left: 6px;
          padding-right: 6px;
        }

        .sj-metric {
          margin-bottom: 8px;
        }

        .sj-metric-label {
          font-size: 0.7rem;
          color: #64748b;
          text-transform: uppercase;
          letter-spacing: 0.3px;
        }

        .sj-metric-value {
          font-size: 0.9rem;
          font-weight: 600;
          color: #1e293b;
        }

        /* Section cards */
        .sj-section-card {
          margin-bottom: 16px;
        }

        .sj-section-card .card-header {
          padding: 10px 16px;
        }

        .sj-section-card .card-body {
          padding: 16px;
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

        # Unified Classification (prominent display)
        uiOutput(ns("classification_card")),

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
            div(
              class = "d-flex gap-2",
              downloadButton(
                ns("download_csv"),
                "Quick CSV",
                icon = icon("file-arrow-down"),
                class = "btn-outline-primary"
              ),
              downloadButton(
                ns("download_pdf"),
                "Download PDF",
                icon = icon("file-pdf"),
                class = "btn-primary"
              )
            )
          )
        )
      )
    })

    # Classification card - shows unified sample classification
    output$classification_card <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found) {
        return(NULL)
      }

      # Calculate classification
      class_data <- calculate_sample_classification(data)

      if (!class_data$tested) {
        return(NULL)
      }

      # Determine card styling based on classification
      class_text <- class_data$classification
      card_class <- dplyr::case_when(
        grepl("Both", class_text) ~ "border-danger",
        grepl("Molecular only", class_text) ~ "border-warning",
        grepl("Serological only", class_text) ~ "border-info",
        grepl("Borderline", class_text) ~ "border-warning",
        TRUE ~ "border-secondary"
      )

      header_class <- dplyr::case_when(
        grepl("Both", class_text) ~ "bg-danger text-white",
        grepl("Molecular only", class_text) ~ "bg-warning",
        grepl("Serological only", class_text) ~ "bg-info text-white",
        grepl("Borderline", class_text) ~ "bg-warning",
        TRUE ~ "bg-light"
      )

      classification_icon_name <- dplyr::case_when(
        grepl("Both", class_text) ~ "triangle-exclamation",
        grepl("Molecular only", class_text) ~ "dna",
        grepl("Serological only", class_text) ~ "vial",
        grepl("Borderline", class_text) ~ "question-circle",
        TRUE ~ "circle-check"
      )
      classification_icon <- icon(classification_icon_name)

      # Build status badges
      molecular_badge <- if (!is.na(class_data$molecular$status)) {
        badge_class <- if (class_data$molecular$positive) "bg-danger" else "bg-success"
        discordant_warning <- if (class_data$molecular$discordant) {
          tags$span(class = "badge bg-warning text-dark ms-1", icon("exclamation-triangle"), " Discordant")
        }
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2",
          tags$strong("MIC (Molecular):"),
          tags$span(class = paste("badge", badge_class), class_data$molecular$status),
          if (class_data$molecular$n_tests > 1) {
            tags$small(class = "text-muted", sprintf("(%d tests)", class_data$molecular$n_tests))
          },
          discordant_warning
        )
      } else {
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2 text-muted",
          tags$strong("MIC (Molecular):"),
          tags$span("Not tested")
        )
      }

      elisa_vsg_badge <- if (!is.na(class_data$serological$elisa_vsg$status)) {
        badge_class <- if (tolower(class_data$serological$elisa_vsg$status) == "positive") "bg-danger" else "bg-success"
        discordant_warning <- if (class_data$serological$elisa_vsg$discordant) {
          tags$span(class = "badge bg-warning text-dark ms-1", icon("exclamation-triangle"), " Discordant")
        }
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2",
          tags$strong("ELISA VSG:"),
          tags$span(class = paste("badge", badge_class), class_data$serological$elisa_vsg$status),
          if (class_data$serological$elisa_vsg$n_tests > 1) {
            tags$small(class = "text-muted", sprintf("(%d tests)", class_data$serological$elisa_vsg$n_tests))
          },
          discordant_warning
        )
      } else {
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2 text-muted",
          tags$strong("ELISA VSG:"),
          tags$span("Not tested")
        )
      }

      elisa_pe_badge <- if (!is.na(class_data$serological$elisa_pe$status)) {
        badge_class <- if (tolower(class_data$serological$elisa_pe$status) == "positive") "bg-danger" else "bg-success"
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2",
          tags$strong("ELISA PE:"),
          tags$span(class = paste("badge", badge_class), class_data$serological$elisa_pe$status),
          if (class_data$serological$elisa_pe$n_tests > 1) {
            tags$small(class = "text-muted", sprintf("(%d tests)", class_data$serological$elisa_pe$n_tests))
          },
          tags$small(class = "text-muted fst-italic", "(development)")
        )
      } else {
        NULL
      }

      ielisa_badge <- if (!is.na(class_data$serological$ielisa$status)) {
        badge_class <- if (tolower(class_data$serological$ielisa$status) == "positive") "bg-danger" else "bg-success"
        discordant_warning <- if (class_data$serological$ielisa$discordant) {
          tags$span(class = "badge bg-warning text-dark ms-1", icon("exclamation-triangle"), " Discordant")
        }
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2",
          tags$strong("iELISA:"),
          tags$span(class = paste("badge", badge_class), class_data$serological$ielisa$status),
          if (class_data$serological$ielisa$n_tests > 1) {
            tags$small(class = "text-muted", sprintf("(%d tests)", class_data$serological$ielisa$n_tests))
          },
          discordant_warning
        )
      } else {
        tags$div(
          class = "d-flex align-items-center gap-2 mb-2 text-muted",
          tags$strong("iELISA:"),
          tags$span("Not tested")
        )
      }

      # Discordance warning
      discordance_alert <- if (class_data$has_any_discordance) {
        tags$div(
          class = "alert alert-warning mb-0 mt-3",
          role = "alert",
          icon("triangle-exclamation"),
          tags$strong(" Discordance Detected: "),
          "This sample has conflicting results from retests. Review individual test results below."
        )
      } else {
        NULL
      }

      card(
        class = paste("mb-3", card_class),
        style = "border-width: 2px;",
        card_header(
          class = header_class,
          div(
            class = "d-flex justify-content-between align-items-center",
            div(
              class = "d-flex align-items-center gap-2",
              classification_icon,
              tags$h5(class = "mb-0", "Sample Classification")
            ),
            tags$h4(class = "mb-0", class_text)
          )
        ),
        card_body(
          div(
            class = "row",
            # Molecular results (left column)
            div(
              class = "col-md-6",
              tags$h6(class = "text-muted mb-3", icon("dna"), " Molecular Testing"),
              molecular_badge
            ),
            # Serological results (right column)
            div(
              class = "col-md-6",
              tags$h6(class = "text-muted mb-3", icon("vial"), " Serological Testing"),
              elisa_vsg_badge,
              if (!is.null(elisa_pe_badge)) elisa_pe_badge,
              ielisa_badge,
              if (!is.na(class_data$serological$confidence)) {
                tags$div(
                  class = "mt-2",
                  tags$small(
                    class = "text-muted",
                    tags$strong("Serology Confidence: "),
                    class_data$serological$confidence
                  )
                )
              }
            )
          ),
          if (!is.null(discordance_alert)) discordance_alert
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

    # Results section (row-based layout)
    output$results_section <- renderUI({
      data <- journey_data()

      if (is.null(data) || !data$found) {
        return(NULL)
      }

      tagList(
        # Row 1: Biobank Information (enhanced)
        card(
          class = "sj-section-card mb-3",
          card_header(
            class = "bg-primary text-white",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(icon("database"), " Biobank Information"),
              uiOutput(ns("case_status_badge"))
            )
          ),
          card_body(
            uiOutput(ns("biobank_info"))
          )
        ),

        # Row 2: Extractions (dedicated section)
        card(
          class = "sj-section-card mb-3",
          card_header(
            class = "text-white",
            style = "background-color: #10B981 !important;",
            div(
              class = "d-flex justify-content-between align-items-center",
              div(icon("flask-vial"), " Extractions & DRS Status"),
              uiOutput(ns("extraction_count_badge"))
            )
          ),
          card_body(
            uiOutput(ns("extraction_section"))
          )
        ),

        # Row 3: MIC qPCR (condensed)
        card(
          class = "sj-section-card mb-3",
          card_header(
            class = "bg-warning",
            icon("dna"), " MIC qPCR (Molecular)"
          ),
          card_body(
            uiOutput(ns("mic_results"))
          )
        ),

        # Row 4: ELISA (PE/VSG) - condensed
        card(
          class = "sj-section-card mb-3",
          card_header(
            class = "bg-info text-white",
            icon("vial"), " ELISA (PE/VSG)"
          ),
          card_body(
            uiOutput(ns("elisa_cards"))
          )
        ),

        # Row 5: iELISA - condensed
        card(
          class = "sj-section-card mb-3",
          card_header(
            class = "bg-purple text-white",
            style = "background-color: #8B5CF6 !important;",
            icon("flask"), " iELISA (Inhibition)"
          ),
          card_body(
            uiOutput(ns("ielisa_results"))
          )
        )
      )
    })

    # Case status badge for header
    output$case_status_badge <- renderUI({
      data <- journey_data()
      req(data)

      if (is.null(data$biobank_info) || nrow(data$biobank_info) == 0) {
        return(NULL)
      }

      info <- data$biobank_info[1, ]

      # Determine case status
      previous_case <- if ("previous_case" %in% names(info)) info$previous_case else NA
      treated <- if ("treated" %in% names(info)) info$treated else NA
      case_category <- if ("case_category" %in% names(info)) info$case_category else NA

      # Derive case status
      case_status <- if (!is.na(case_category) && case_category != "") {
        case_category
      } else if (!is.na(previous_case) || !is.na(treated)) {
        prev_yes <- !is.na(previous_case) && tolower(previous_case) %in% c("oui", "yes", "true", "1")
        treat_yes <- !is.na(treated) && tolower(treated) %in% c("oui", "yes", "true", "1")
        prev_no <- !is.na(previous_case) && tolower(previous_case) %in% c("non", "no", "false", "0")
        treat_no <- !is.na(treated) && tolower(treated) %in% c("non", "no", "false", "0")

        if (prev_yes || treat_yes) {
          "Previous/Treated"
        } else if (prev_no && treat_no) {
          "New Case"
        } else {
          "Unknown"
        }
      } else {
        "Unknown"
      }

      badge_class <- dplyr::case_when(
        case_status == "New Case" ~ "new-case",
        case_status %in% c("Previous/Treated", "Treated", "Previous") ~ "treated-case",
        case_status == "Ancient" ~ "ancient-case",
        TRUE ~ "unknown-case"
      )

      badge_icon <- dplyr::case_when(
        case_status == "New Case" ~ "plus-circle",
        case_status %in% c("Previous/Treated", "Treated", "Previous") ~ "history",
        case_status == "Ancient" ~ "clock-rotate-left",
        TRUE ~ "question-circle"
      )

      tags$span(
        class = paste("sj-case-badge", badge_class),
        icon(badge_icon),
        " ",
        case_status
      )
    })

    # Extraction count badge
    output$extraction_count_badge <- renderUI({
      data <- journey_data()
      req(data)

      num_extractions <- nrow(data$extraction_data)

      if (num_extractions == 0) {
        return(tags$span(class = "badge bg-secondary", "No extractions"))
      }

      tags$span(
        class = "badge bg-light text-dark",
        sprintf("%d extraction%s", num_extractions, if(num_extractions > 1) "s" else "")
      )
    })

    # Biobank info - Enhanced version
    output$biobank_info <- renderUI({
      data <- journey_data()
      req(data)

      if (is.null(data$biobank_info) || nrow(data$biobank_info) == 0) {
        return(tags$div(
          class = "text-center text-muted py-4",
          icon("database", class = "fa-2x mb-2"),
          tags$p("No biobank record found")
        ))
      }

      info <- data$biobank_info[1, ]

      # Extract all available info
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

      # Extract barcode and numero
      barcode <- if ("code_barres_kps" %in% names(info) && !is.na(info$code_barres_kps)) {
        as.character(info$code_barres_kps)
      } else if ("barcode" %in% names(info) && !is.na(info$barcode)) {
        as.character(info$barcode)
      } else {
        NA
      }

      numero <- if ("numero_labo" %in% names(info) && !is.na(info$numero_labo)) {
        as.character(info$numero_labo)
      } else if ("numero" %in% names(info) && !is.na(info$numero)) {
        as.character(info$numero)
      } else {
        NA
      }

      # Sex and age if available
      sex <- if ("sex" %in% names(info) && !is.na(info$sex)) {
        info$sex
      } else if ("sexe" %in% names(info) && !is.na(info$sexe)) {
        info$sexe
      } else {
        NA
      }

      age <- if ("age" %in% names(info) && !is.na(info$age)) {
        info$age
      } else {
        NA
      }

      # Case status details
      previous_case <- if ("previous_case" %in% names(info)) info$previous_case else NA
      treated <- if ("treated" %in% names(info)) info$treated else NA
      study <- if ("study" %in% names(info) && !is.na(info$study)) info$study else NA

      tagList(
        # Sample IDs row
        if (!is.na(barcode) || !is.na(numero)) {
          div(
            class = "row mb-3",
            if (!is.na(barcode)) {
              div(
                class = "col-md-6",
                div(
                  class = "sj-info-box sj-highlight sj-highlight-primary",
                  div(class = "sj-label", icon("barcode"), " Barcode"),
                  div(class = "sj-value", tags$code(barcode))
                )
              )
            },
            if (!is.na(numero)) {
              div(
                class = "col-md-6",
                div(
                  class = "sj-info-box sj-highlight sj-highlight-primary",
                  div(class = "sj-label", icon("hashtag"), " Lab Number"),
                  div(class = "sj-value", tags$code(numero))
                )
              )
            }
          )
        },

        # Location row
        div(
          class = "row mb-3",
          div(
            class = "col-md-4",
            div(
              class = "sj-info-box",
              div(class = "sj-label", icon("map-location-dot"), " Province"),
              div(class = "sj-value", province)
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "sj-info-box",
              div(class = "sj-label", icon("map-pin"), " Health Zone"),
              div(class = "sj-value", health_zone)
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "sj-info-box",
              div(class = "sj-label", icon("hospital"), " Structure"),
              div(class = "sj-value", structure)
            )
          )
        ),

        # Dates and patient info row
        div(
          class = "row",
          div(
            class = "col-md-3",
            div(
              class = "sj-info-box sj-highlight sj-highlight-info",
              div(class = "sj-label", icon("calendar"), " Collection"),
              div(class = "sj-value", date_sample)
            )
          ),
          div(
            class = "col-md-3",
            div(
              class = "sj-info-box sj-highlight sj-highlight-info",
              div(class = "sj-label", icon("paper-plane"), " Sent to INRB"),
              div(class = "sj-value", date_sent_inrb)
            )
          ),
          if (!is.na(sex) || !is.na(age)) {
            div(
              class = "col-md-3",
              div(
                class = "sj-info-box",
                div(class = "sj-label", icon("user"), " Patient Info"),
                div(class = "sj-value",
                  if (!is.na(sex) && !is.na(age)) {
                    sprintf("%s, %d yrs", sex, as.integer(age))
                  } else if (!is.na(sex)) {
                    sex
                  } else {
                    sprintf("%d years", as.integer(age))
                  }
                )
              )
            )
          },
          if (!is.na(study)) {
            div(
              class = "col-md-3",
              div(
                class = "sj-info-box",
                div(class = "sj-label", icon("clipboard-list"), " Study"),
                div(class = "sj-value", study)
              )
            )
          }
        ),

        # Case history details if available
        if (!is.na(previous_case) || !is.na(treated)) {
          div(
            class = "row mt-3",
            div(
              class = "col-12",
              div(
                class = "sj-info-box",
                style = "background: rgba(139, 92, 246, 0.05);",
                div(class = "sj-label", icon("clock-rotate-left"), " Case History"),
                div(
                  class = "d-flex gap-4 mt-2",
                  if (!is.na(previous_case)) {
                    tags$span(
                      tags$strong("Previous Case: "),
                      tags$span(
                        class = if (tolower(previous_case) %in% c("oui", "yes")) "text-warning" else "text-success",
                        previous_case
                      )
                    )
                  },
                  if (!is.na(treated)) {
                    tags$span(
                      tags$strong("Previously Treated: "),
                      tags$span(
                        class = if (tolower(treated) %in% c("oui", "yes")) "text-warning" else "text-success",
                        treated
                      )
                    )
                  }
                )
              )
            )
          )
        }
      )
    })

    # New dedicated Extraction section with each extraction listed
    output$extraction_section <- renderUI({
      data <- journey_data()
      req(data)

      num_extractions <- nrow(data$extraction_data)

      if (num_extractions == 0) {
        return(tags$div(
          class = "text-center text-muted py-4",
          icon("flask-vial", class = "fa-2x mb-2"),
          tags$p("No extraction records found")
        ))
      }

      # Build extraction list items
      extraction_items <- lapply(1:num_extractions, function(i) {
        extraction <- data$extraction_data[i, ]

        ext_date <- if ("extraction_date" %in% names(extraction) && !is.na(extraction$extraction_date)) {
          format(as.Date(extraction$extraction_date), "%Y-%m-%d")
        } else {
          "Unknown"
        }

        # DRS Volume
        volume_ml <- if ("drs_volume_ml" %in% names(extraction) && !is.na(extraction$drs_volume_ml)) {
          extraction$drs_volume_ml
        } else {
          NA
        }

        volume_ul <- if (!is.na(volume_ml)) round(volume_ml * 1000) else NA

        # DRS State (Liquid, Viscous, Coagulated)
        drs_state <- if ("drs_state" %in% names(extraction) && !is.na(extraction$drs_state)) {
          extraction$drs_state
        } else {
          "Unknown"
        }

        drs_state_class <- dplyr::case_when(
          drs_state == "Liquid" ~ "sj-drs-liquid",
          drs_state == "Viscous" ~ "sj-drs-viscous",
          drs_state == "Coagulated" ~ "sj-drs-coagulated",
          TRUE ~ "sj-drs-unknown"
        )

        drs_state_icon <- dplyr::case_when(
          drs_state == "Liquid" ~ "droplet",
          drs_state == "Viscous" ~ "water",
          drs_state == "Coagulated" ~ "plug",
          TRUE ~ "question"
        )

        # Extract Quality (Clear, Foncé, Échec)
        extract_quality <- if ("extract_quality" %in% names(extraction) && !is.na(extraction$extract_quality)) {
          extraction$extract_quality
        } else {
          "Unknown"
        }

        quality_class <- dplyr::case_when(
          extract_quality == "Clear" ~ "sj-quality-clear",
          extract_quality == "Foncé" ~ "sj-quality-fonce",
          extract_quality == "Échec" ~ "sj-quality-echec",
          TRUE ~ "sj-quality-unknown"
        )

        quality_icon <- dplyr::case_when(
          extract_quality == "Clear" ~ "check-circle",
          extract_quality == "Foncé" ~ "circle-half-stroke",
          extract_quality == "Échec" ~ "times-circle",
          TRUE ~ "question-circle"
        )

        # Technician
        technician <- if ("technician" %in% names(extraction) && !is.na(extraction$technician)) {
          extraction$technician
        } else {
          NA
        }

        tags$div(
          class = "sj-extraction-item",
          # Header
          div(
            class = "sj-extraction-header",
            div(
              tags$strong(sprintf("Extraction #%d", i)),
              tags$span(class = "text-muted ms-2", ext_date)
            ),
            div(
              class = "d-flex gap-2",
              tags$span(class = paste("sj-drs-indicator", drs_state_class), icon(drs_state_icon), " ", drs_state),
              tags$span(class = paste("sj-drs-indicator", quality_class), icon(quality_icon), " ", extract_quality)
            )
          ),
          # Body with metrics
          div(
            class = "row",
            div(
              class = "col-4",
              div(
                class = "sj-metric",
                div(class = "sj-metric-label", "Volume"),
                div(class = "sj-metric-value",
                  if (!is.na(volume_ul)) {
                    tags$span(
                      sprintf("%d", volume_ul),
                      tags$small(class = "text-muted", " µL")
                    )
                  } else {
                    tags$span(class = "text-muted", "N/A")
                  }
                )
              )
            ),
            div(
              class = "col-4",
              div(
                class = "sj-metric",
                div(class = "sj-metric-label", "DRS State"),
                div(class = "sj-metric-value", drs_state)
              )
            ),
            div(
              class = "col-4",
              div(
                class = "sj-metric",
                div(class = "sj-metric-label", "Quality"),
                div(class = "sj-metric-value", extract_quality)
              )
            )
          ),
          if (!is.na(technician)) {
            div(
              class = "mt-2 pt-2",
              style = "border-top: 1px dashed #e5e7eb;",
              tags$small(class = "text-muted", icon("user"), " Technician: ", technician)
            )
          }
        )
      })

      tagList(
        # Gauge and extraction list side by side on larger screens
        div(
          class = "row",
          # Left column: DRS Volume Gauge
          div(
            class = "col-lg-5",
            tags$div(
              class = "sj-info-box",
              style = "padding: 8px; min-height: 320px;",
              plotlyOutput(ns("drs_gauge"), height = "300px")
            )
          ),
          # Right column: Extraction list
          div(
            class = "col-lg-7",
            tags$div(
              style = "max-height: 320px; overflow-y: auto;",
              extraction_items
            )
          )
        )
      )
    })

    # DRS gauge - shows initial volume from extraction data minus 300µL per extraction
    output$drs_gauge <- renderPlotly({
      data <- journey_data()
      req(data, nrow(data$extraction_data) > 0)

      num_extractions <- nrow(data$extraction_data)

      # Get initial DRS volume from the first extraction record (in mL, convert to µL)
      first_extraction <- data$extraction_data[1, ]
      initial_volume_ul <- if ("drs_volume_ml" %in% names(first_extraction) && !is.na(first_extraction$drs_volume_ml)) {
        first_extraction$drs_volume_ml * 1000  # Convert mL to µL
      } else {
        2000  # Default fallback if not recorded
      }

      # Each extraction uses 300µL
      plot_drs_gauge(initial_volume_ul = initial_volume_ul, num_extractions = num_extractions)
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

    # Quick CSV export of timeline events
    output$download_csv <- downloadHandler(
      filename = function() {
        data <- journey_data()
        sample_id <- if (!is.null(data) && data$found) {
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

        sprintf(
          "sample_journey_%s_%s.csv",
          gsub("[^A-Za-z0-9]", "_", sample_id),
          format(Sys.time(), "%Y%m%d_%H%M%S")
        )
      },
      content = function(file) {
        data <- journey_data()
        req(data, data$found)

        sample_label <- if (!is.null(data$sample_id) && data$sample_id != "") {
          data$sample_id
        } else {
          "unknown"
        }

        export_df <- data$timeline %>%
          mutate(
            sample_id = sample_label,
            date = as.Date(date)
          ) %>%
          select(sample_id, category, event, date, details) %>%
          arrange(date, category, event)

        if (nrow(export_df) == 0) {
          export_df <- tibble(
            sample_id = sample_label,
            category = "N/A",
            event = "No events recorded",
            date = NA_Date_,
            details = ""
          )
        }

        readr::write_csv(export_df, file)
      }
    )
  })
}
