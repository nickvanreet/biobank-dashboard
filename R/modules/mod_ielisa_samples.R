# R/modules/mod_ielisa_samples.R
# iELISA Samples module - Sample-level results and QC

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(scales)
})

#' iELISA Samples UI
#' @param id Module namespace ID
#' @export
mod_ielisa_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "ielisa-panel",

      # Filters
      card(
        card_header("Sample Filters"),
        card_body(
          class = "p-3",
          layout_columns(
            col_widths = c(3, 3, 3, 3),
            textInput(
              ns("filter_labid"),
              "LabID contains:",
              value = "",
              placeholder = "e.g., KPS-"
            ),
            textInput(
              ns("filter_barcode"),
              "Barcode contains:",
              value = "",
              placeholder = "e.g., 12345"
            ),
            selectInput(
              ns("filter_file"),
              "File:",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              ns("filter_qc"),
              "QC Status:",
              choices = c("All", "Pass (both)", "Fail (either)", "Pass L13", "Pass L15"),
              selected = "All"
            )
          ),
          div(
            class = "d-flex justify-content-end",
            actionButton(ns("reset_filters"), "Reset Filters", class = "btn-sm btn-outline-secondary")
          )
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Sample results table
      card(
        card_header("Sample Results"),
        card_body(
          DTOutput(ns("samples_table"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Visualizations
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Inhibition Distribution (Formula 2)"),
          card_body(plotlyOutput(ns("plot_inhibition_dist"), height = "400px"))
        ),
        card(
          card_header("Formula 1 vs Formula 2 Agreement"),
          card_body(plotlyOutput(ns("plot_formula_agreement"), height = "400px"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # More visualizations
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Inhibition vs OD (LiTat 1.3)"),
          card_body(plotlyOutput(ns("plot_inh_vs_od_13"), height = "400px"))
        ),
        card(
          card_header("Inhibition vs OD (LiTat 1.5)"),
          card_body(plotlyOutput(ns("plot_inh_vs_od_15"), height = "400px"))
        )
      )
    )
  )
}

#' iELISA Samples Server
#' @param id Module namespace ID
#' @param ielisa_data Reactive returning iELISA data frame
#' @export
mod_ielisa_samples_server <- function(id, ielisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # UPDATE FILE FILTER CHOICES
    # ========================================================================

    observe({
      data <- ielisa_data()

      if (nrow(data) > 0 && "file" %in% names(data)) {
        file_choices <- c("All", sort(unique(data$file)))
        updateSelectInput(session, "filter_file", choices = file_choices, selected = "All")
      }
    })

    # ========================================================================
    # FILTERED DATA
    # ========================================================================

    filtered_data <- reactive({
      data <- ielisa_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Apply LabID filter
      if (!is.null(input$filter_labid) && input$filter_labid != "") {
        data <- data %>%
          filter(str_detect(LabID, fixed(input$filter_labid, ignore_case = TRUE)))
      }

      # Apply Barcode filter
      if (!is.null(input$filter_barcode) && input$filter_barcode != "") {
        data <- data %>%
          filter(str_detect(as.character(Barcode), fixed(input$filter_barcode, ignore_case = TRUE)))
      }

      # Apply File filter
      if (!is.null(input$filter_file) && input$filter_file != "All") {
        data <- data %>%
          filter(file == input$filter_file)
      }

      # Apply QC filter
      if (!is.null(input$filter_qc) && input$filter_qc != "All") {
        data <- switch(input$filter_qc,
          "Pass (both)" = data %>% filter(qc_sample_L13 & qc_sample_L15),
          "Fail (either)" = data %>% filter(!qc_sample_L13 | !qc_sample_L15),
          "Pass L13" = data %>% filter(qc_sample_L13),
          "Pass L15" = data %>% filter(qc_sample_L15),
          data
        )
      }

      data
    })

    # ========================================================================
    # RESET FILTERS
    # ========================================================================

    observeEvent(input$reset_filters, {
      updateTextInput(session, "filter_labid", value = "")
      updateTextInput(session, "filter_barcode", value = "")
      updateSelectInput(session, "filter_file", selected = "All")
      updateSelectInput(session, "filter_qc", selected = "All")
    })

    # ========================================================================
    # SAMPLES TABLE
    # ========================================================================

    output$samples_table <- renderDT({
      data <- filtered_data()

      if (!nrow(data)) {
        return(datatable(
          tibble(Message = "No samples match the current filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format data for display
      display_data <- data %>%
        mutate(
          OD_L13 = round(OD_L13, 3),
          OD_L15 = round(OD_L15, 3),
          pct_inh_f1_13 = round(pct_inh_f1_13, 1),
          pct_inh_f1_15 = round(pct_inh_f1_15, 1),
          pct_inh_f2_13 = round(pct_inh_f2_13, 1),
          pct_inh_f2_15 = round(pct_inh_f2_15, 1),
          diff_f1_f2_13 = round(diff_f1_f2_13, 1),
          diff_f1_f2_15 = round(diff_f1_f2_15, 1),
          # Format QC status
          L13_QC = ifelse(qc_sample_L13, "✓ PASS", "✗ FAIL"),
          L15_QC = ifelse(qc_sample_L15, "✓ PASS", "✗ FAIL"),
          # Positivity indicator
          Positive_For = case_when(
            qc_sample_L13 & qc_sample_L15 ~ "Both",
            qc_sample_L13 ~ "LiTat 1.3",
            qc_sample_L15 ~ "LiTat 1.5",
            TRUE ~ "Neither"
          ),
          # Format duplicate flags
          Dup_Flag = case_when(
            duplicate_across_files ~ "🔄 DUP",
            barcode_conflict ~ "⚠️ BC",
            labid_conflict ~ "⚠️ LID",
            TRUE ~ ""
          )
        ) %>%
        select(
          File = file,
          LabID,
          Barcode,
          `Positive For` = Positive_For,
          `OD L13` = OD_L13,
          `Inh% L13 (F1)` = pct_inh_f1_13,
          `Inh% L13 (F2)` = pct_inh_f2_13,
          `L13 QC` = L13_QC,
          `OD L15` = OD_L15,
          `Inh% L15 (F1)` = pct_inh_f1_15,
          `Inh% L15 (F2)` = pct_inh_f2_15,
          `L15 QC` = L15_QC,
          `Δ F1-F2 L13` = diff_f1_f2_13,
          `Δ F1-F2 L15` = diff_f1_f2_15,
          Flags = Dup_Flag
        )

      datatable(
        display_data,
        filter = 'top',
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'Positive For',
          backgroundColor = styleEqual(
            c("Both", "LiTat 1.3", "LiTat 1.5", "Neither"),
            c("#d4edda", "#cfe2ff", "#cfe2ff", "#f8d7da")
          ),
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'L13 QC',
          backgroundColor = styleEqual(c("✓ PASS", "✗ FAIL"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'L15 QC',
          backgroundColor = styleEqual(c("✓ PASS", "✗ FAIL"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'Flags',
          backgroundColor = styleInterval(c(0.5), c("white", "#fff3cd"))
        )
    })

    # ========================================================================
    # INHIBITION DISTRIBUTION
    # ========================================================================

    output$plot_inhibition_dist <- renderPlotly({
      data <- filtered_data()

      if (!nrow(data)) {
        return(plotly_empty("No data available"))
      }

      # Reshape data
      plot_data <- data %>%
        select(pct_inh_f2_13, pct_inh_f2_15) %>%
        pivot_longer(
          cols = everything(),
          names_to = "antigen",
          values_to = "inhibition"
        ) %>%
        mutate(
          antigen = ifelse(antigen == "pct_inh_f2_13", "LiTat 1.3", "LiTat 1.5")
        )

      p <- plot_ly(data = plot_data, x = ~inhibition, color = ~antigen,
                   colors = c("#3498db", "#e74c3c"),
                   type = "histogram", alpha = 0.6) %>%
        layout(
          xaxis = list(title = "Inhibition (%)"),
          yaxis = list(title = "Count"),
          barmode = "overlay",
          shapes = list(
            list(
              type = "line",
              x0 = 30, x1 = 30,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = "red", dash = "dash", width = 2)
            )
          ),
          annotations = list(
            list(
              x = 30,
              y = 0.95,
              yref = "paper",
              text = "Positivity threshold (30%)",
              showarrow = FALSE,
              xanchor = "left",
              font = list(color = "red", size = 10)
            )
          )
        )

      p
    })

    # ========================================================================
    # FORMULA AGREEMENT
    # ========================================================================

    output$plot_formula_agreement <- renderPlotly({
      data <- filtered_data()

      if (!nrow(data)) {
        return(plotly_empty("No data available"))
      }

      # LiTat 1.3
      p1 <- plot_ly(data = data, x = ~pct_inh_f1_13, y = ~pct_inh_f2_13,
                    type = "scatter", mode = "markers",
                    marker = list(size = 8, opacity = 0.6, color = "#3498db"),
                    name = "LiTat 1.3",
                    hovertemplate = paste(
                      "<b>LabID:</b> %{text}<br>",
                      "<b>Formula 1:</b> %{x:.1f}%<br>",
                      "<b>Formula 2:</b> %{y:.1f}%<br>",
                      "<extra></extra>"
                    ),
                    text = ~LabID)

      # LiTat 1.5
      p2 <- plot_ly(data = data, x = ~pct_inh_f1_15, y = ~pct_inh_f2_15,
                    type = "scatter", mode = "markers",
                    marker = list(size = 8, opacity = 0.6, color = "#e74c3c"),
                    name = "LiTat 1.5",
                    hovertemplate = paste(
                      "<b>LabID:</b> %{text}<br>",
                      "<b>Formula 1:</b> %{x:.1f}%<br>",
                      "<b>Formula 2:</b> %{y:.1f}%<br>",
                      "<extra></extra>"
                    ),
                    text = ~LabID)

      # Combine and add identity line
      subplot(p1, p2, nrows = 1, shareX = TRUE, shareY = TRUE) %>%
        layout(
          xaxis = list(title = "Formula 1 Inhibition (%)"),
          yaxis = list(title = "Formula 2 Inhibition (%)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 100,
              y0 = 0, y1 = 100,
              line = list(color = "gray", dash = "dash")
            )
          )
        )
    })

    # ========================================================================
    # INHIBITION VS OD (LiTat 1.3)
    # ========================================================================

    output$plot_inh_vs_od_13 <- renderPlotly({
      data <- filtered_data()

      if (!nrow(data)) {
        return(plotly_empty("No data available"))
      }

      p <- plot_ly(data = data, x = ~OD_L13, y = ~pct_inh_f2_13,
                   type = "scatter", mode = "markers",
                   marker = list(
                     size = 8,
                     color = ~qc_sample_L13,
                     colors = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71"),
                     opacity = 0.7
                   ),
                   hovertemplate = paste(
                     "<b>LabID:</b> %{text}<br>",
                     "<b>OD:</b> %{x:.3f}<br>",
                     "<b>Inhibition:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   ),
                   text = ~LabID) %>%
        layout(
          xaxis = list(title = "OD Value"),
          yaxis = list(title = "Inhibition (%)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 1,
              xref = "paper",
              y0 = 30, y1 = 30,
              line = list(color = "red", dash = "dash", width = 2)
            )
          )
        )

      p
    })

    # ========================================================================
    # INHIBITION VS OD (LiTat 1.5)
    # ========================================================================

    output$plot_inh_vs_od_15 <- renderPlotly({
      data <- filtered_data()

      if (!nrow(data)) {
        return(plotly_empty("No data available"))
      }

      p <- plot_ly(data = data, x = ~OD_L15, y = ~pct_inh_f2_15,
                   type = "scatter", mode = "markers",
                   marker = list(
                     size = 8,
                     color = ~qc_sample_L15,
                     colors = c("FALSE" = "#e74c3c", "TRUE" = "#2ecc71"),
                     opacity = 0.7
                   ),
                   hovertemplate = paste(
                     "<b>LabID:</b> %{text}<br>",
                     "<b>OD:</b> %{x:.3f}<br>",
                     "<b>Inhibition:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   ),
                   text = ~LabID) %>%
        layout(
          xaxis = list(title = "OD Value"),
          yaxis = list(title = "Inhibition (%)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 1,
              xref = "paper",
              y0 = 30, y1 = 30,
              line = list(color = "red", dash = "dash", width = 2)
            )
          )
        )

      p
    })

  })
}
