# R/modules/mod_ielisa_analysis.R
# iELISA Analysis module - Duplicates and detailed analysis

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(scales)
})

#' iELISA Analysis UI
#' @param id Module namespace ID
#' @export
mod_ielisa_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "ielisa-panel",

      # Duplicate samples section
      card(
        card_header("Duplicate Testing Summary"),
        card_body(
          uiOutput(ns("duplicate_kpis")),

          tags$div(style = "height: 16px;"),

          DTOutput(ns("duplicates_table"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Conflict warnings
      card(
        card_header("⚠️ Data Quality Conflicts"),
        card_body(
          uiOutput(ns("conflict_warnings"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Duplicate comparison visualizations
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Duplicate Test Agreement (LiTat 1.3)"),
          card_body(plotlyOutput(ns("plot_dup_agreement_13"), height = "400px"))
        ),
        card(
          card_header("Duplicate Test Agreement (LiTat 1.5)"),
          card_body(plotlyOutput(ns("plot_dup_agreement_15"), height = "400px"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Export section
      card(
        card_header("📥 Data Export"),
        card_body(
          class = "p-3",
          p("Export iELISA data in various formats:"),
          div(
            class = "d-flex gap-2",
            downloadButton(ns("export_full"), "Full Dataset", class = "btn-primary"),
            downloadButton(ns("export_qc_failed"), "Negative Samples", class = "btn-warning"),
            downloadButton(ns("export_duplicates"), "Duplicates Only", class = "btn-info")
          )
        )
      )
    )
  )
}

#' iELISA Analysis Server
#' @param id Module namespace ID
#' @param ielisa_data Reactive returning iELISA data frame
#' @export
mod_ielisa_analysis_server <- function(id, ielisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # DUPLICATE ANALYSIS
    # ========================================================================

    duplicate_data <- reactive({
      data <- ielisa_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Filter to only duplicates
      data %>%
        filter(duplicate_across_files == TRUE) %>%
        arrange(LabID, Barcode, file)
    })

    duplicate_summary <- reactive({
      data <- duplicate_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Summarize by LabID/Barcode
      data %>%
        group_by(LabID, Barcode) %>%
        summarise(
          n_tests = n(),
          files = paste(unique(file), collapse = "; "),
          # LiTat 1.3 stats
          mean_inh_13 = mean(pct_inh_f2_13, na.rm = TRUE),
          sd_inh_13 = sd(pct_inh_f2_13, na.rm = TRUE),
          cv_inh_13 = ifelse(mean_inh_13 != 0, (sd_inh_13 / mean_inh_13) * 100, NA),
          # LiTat 1.5 stats
          mean_inh_15 = mean(pct_inh_f2_15, na.rm = TRUE),
          sd_inh_15 = sd(pct_inh_f2_15, na.rm = TRUE),
          cv_inh_15 = ifelse(mean_inh_15 != 0, (sd_inh_15 / mean_inh_15) * 100, NA),
          # Concordance
          concordant_13 = all(positive_L13) | all(!positive_L13),
          concordant_15 = all(positive_L15) | all(!positive_L15),
          .groups = "drop"
        )
    })

    # ========================================================================
    # CONFLICT ANALYSIS
    # ========================================================================

    conflict_data <- reactive({
      data <- ielisa_data()

      if (!nrow(data)) {
        return(list(
          barcode_conflicts = tibble(),
          labid_conflicts = tibble()
        ))
      }

      # Barcode conflicts (same LabID, different Barcode)
      barcode_conflicts <- data %>%
        filter(barcode_conflict == TRUE) %>%
        select(LabID, Barcode, file) %>%
        distinct() %>%
        arrange(LabID, Barcode)

      # LabID conflicts (same Barcode, different LabID)
      labid_conflicts <- data %>%
        filter(labid_conflict == TRUE) %>%
        select(LabID, Barcode, file) %>%
        distinct() %>%
        arrange(Barcode, LabID)

      list(
        barcode_conflicts = barcode_conflicts,
        labid_conflicts = labid_conflicts
      )
    })

    # ========================================================================
    # DUPLICATE KPIs
    # ========================================================================

    output$duplicate_kpis <- renderUI({
      dup_data <- duplicate_data()
      dup_summ <- duplicate_summary()

      n_duplicates <- nrow(dup_summ)
      n_tests <- nrow(dup_data)

      # Count concordant duplicates
      concordant_13 <- if (nrow(dup_summ) > 0) {
        sum(dup_summ$concordant_13, na.rm = TRUE)
      } else {
        0
      }

      concordant_15 <- if (nrow(dup_summ) > 0) {
        sum(dup_summ$concordant_15, na.rm = TRUE)
      } else {
        0
      }

      pct_concordant_13 <- if (n_duplicates > 0) {
        (concordant_13 / n_duplicates) * 100
      } else {
        0
      }

      pct_concordant_15 <- if (n_duplicates > 0) {
        (concordant_15 / n_duplicates) * 100
      } else {
        0
      }

      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        value_box(
          title = "Samples Tested Multiple Times",
          value = comma(n_duplicates),
          showcase = icon("copy"),
          theme = "primary"
        ),
        value_box(
          title = "Total Duplicate Tests",
          value = comma(n_tests),
          showcase = icon("vials"),
          theme = "info"
        ),
        value_box(
          title = "LiTat 1.3 Concordance",
          value = sprintf("%.1f%%", pct_concordant_13),
          showcase = icon("check-double"),
          theme = if (pct_concordant_13 >= 90) "success" else if (pct_concordant_13 >= 70) "warning" else "danger"
        ),
        value_box(
          title = "LiTat 1.5 Concordance",
          value = sprintf("%.1f%%", pct_concordant_15),
          showcase = icon("check-double"),
          theme = if (pct_concordant_15 >= 90) "success" else if (pct_concordant_15 >= 70) "warning" else "danger"
        )
      )
    })

    # ========================================================================
    # DUPLICATES TABLE
    # ========================================================================

    output$duplicates_table <- renderDT({
      dup_summ <- duplicate_summary()

      if (!nrow(dup_summ)) {
        return(datatable(
          tibble(Message = "No duplicate samples found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format data
      display_data <- dup_summ %>%
        mutate(
          mean_inh_13 = round(mean_inh_13, 1),
          sd_inh_13 = round(sd_inh_13, 1),
          cv_inh_13 = round(cv_inh_13, 1),
          mean_inh_15 = round(mean_inh_15, 1),
          sd_inh_15 = round(sd_inh_15, 1),
          cv_inh_15 = round(cv_inh_15, 1),
          L13_Concordance = ifelse(concordant_13, "✓ Yes", "✗ No"),
          L15_Concordance = ifelse(concordant_15, "✓ Yes", "✗ No")
        ) %>%
        select(
          LabID,
          Barcode,
          `# Tests` = n_tests,
          `Files` = files,
          `Mean Inh% L13` = mean_inh_13,
          `SD L13` = sd_inh_13,
          `CV% L13` = cv_inh_13,
          `L13 Match` = L13_Concordance,
          `Mean Inh% L15` = mean_inh_15,
          `SD L15` = sd_inh_15,
          `CV% L15` = cv_inh_15,
          `L15 Match` = L15_Concordance
        )

      datatable(
        display_data,
        filter = 'top',
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'L13 Match',
          backgroundColor = styleEqual(c("✓ Yes", "✗ No"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'L15 Match',
          backgroundColor = styleEqual(c("✓ Yes", "✗ No"), c("#d4edda", "#f8d7da"))
        )
    })

    # ========================================================================
    # CONFLICT WARNINGS
    # ========================================================================

    output$conflict_warnings <- renderUI({
      conflicts <- conflict_data()

      n_barcode_conflicts <- nrow(conflicts$barcode_conflicts)
      n_labid_conflicts <- nrow(conflicts$labid_conflicts)

      if (n_barcode_conflicts == 0 && n_labid_conflicts == 0) {
        return(
          div(
            class = "alert alert-success",
            icon("circle-check"),
            " No data quality conflicts detected!"
          )
        )
      }

      tagList(
        if (n_barcode_conflicts > 0) {
          div(
            class = "alert alert-warning mb-3",
            tags$h6(
              icon("triangle-exclamation"),
              sprintf(" Barcode Conflicts: %d samples", n_barcode_conflicts)
            ),
            tags$p("The same LabID appears with different Barcodes across files:"),
            tags$pre(
              style = "font-size: 0.875rem; max-height: 200px; overflow-y: auto;",
              paste(
                conflicts$barcode_conflicts %>%
                  group_by(LabID) %>%
                  summarise(Barcodes = paste(unique(Barcode), collapse = ", ")) %>%
                  mutate(text = sprintf("%s → %s", LabID, Barcodes)) %>%
                  pull(text),
                collapse = "\n"
              )
            )
          )
        },
        if (n_labid_conflicts > 0) {
          div(
            class = "alert alert-warning",
            tags$h6(
              icon("triangle-exclamation"),
              sprintf(" LabID Conflicts: %d samples", n_labid_conflicts)
            ),
            tags$p("The same Barcode appears with different LabIDs across files:"),
            tags$pre(
              style = "font-size: 0.875rem; max-height: 200px; overflow-y: auto;",
              paste(
                conflicts$labid_conflicts %>%
                  group_by(Barcode) %>%
                  summarise(LabIDs = paste(unique(LabID), collapse = ", ")) %>%
                  mutate(text = sprintf("%s → %s", Barcode, LabIDs)) %>%
                  pull(text),
                collapse = "\n"
              )
            )
          )
        }
      )
    })

    # ========================================================================
    # DUPLICATE AGREEMENT PLOTS
    # ========================================================================

    output$plot_dup_agreement_13 <- renderPlotly({
      dup_data <- duplicate_data()

      if (!nrow(dup_data)) {
        return(plotly_empty("No duplicate data available"))
      }

      # Get pairs of tests for each sample
      dup_pairs <- dup_data %>%
        group_by(LabID, Barcode) %>%
        filter(n() == 2) %>%  # Only pairs for simplicity
        arrange(file) %>%
        summarise(
          test1 = first(pct_inh_f2_13),
          test2 = last(pct_inh_f2_13),
          .groups = "drop"
        )

      if (!nrow(dup_pairs)) {
        return(plotly_empty("No paired duplicate tests found"))
      }

      p <- plot_ly(data = dup_pairs, x = ~test1, y = ~test2,
                   type = "scatter", mode = "markers",
                   marker = list(size = 10, opacity = 0.6, color = "#3498db"),
                   hovertemplate = paste(
                     "<b>Test 1:</b> %{x:.1f}%<br>",
                     "<b>Test 2:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   )) %>%
        layout(
          xaxis = list(title = "First Test Inhibition (%)"),
          yaxis = list(title = "Second Test Inhibition (%)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 100,
              y0 = 0, y1 = 100,
              line = list(color = "gray", dash = "dash")
            ),
            list(
              type = "line",
              x0 = 30, x1 = 30,
              y0 = 0, y1 = 100,
              line = list(color = "red", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = 0, x1 = 100,
              y0 = 30, y1 = 30,
              line = list(color = "red", dash = "dash", width = 1)
            )
          )
        )

      p
    })

    output$plot_dup_agreement_15 <- renderPlotly({
      dup_data <- duplicate_data()

      if (!nrow(dup_data)) {
        return(plotly_empty("No duplicate data available"))
      }

      # Get pairs of tests for each sample
      dup_pairs <- dup_data %>%
        group_by(LabID, Barcode) %>%
        filter(n() == 2) %>%  # Only pairs for simplicity
        arrange(file) %>%
        summarise(
          test1 = first(pct_inh_f2_15),
          test2 = last(pct_inh_f2_15),
          .groups = "drop"
        )

      if (!nrow(dup_pairs)) {
        return(plotly_empty("No paired duplicate tests found"))
      }

      p <- plot_ly(data = dup_pairs, x = ~test1, y = ~test2,
                   type = "scatter", mode = "markers",
                   marker = list(size = 10, opacity = 0.6, color = "#e74c3c"),
                   hovertemplate = paste(
                     "<b>Test 1:</b> %{x:.1f}%<br>",
                     "<b>Test 2:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   )) %>%
        layout(
          xaxis = list(title = "First Test Inhibition (%)"),
          yaxis = list(title = "Second Test Inhibition (%)"),
          shapes = list(
            list(
              type = "line",
              x0 = 0, x1 = 100,
              y0 = 0, y1 = 100,
              line = list(color = "gray", dash = "dash")
            ),
            list(
              type = "line",
              x0 = 30, x1 = 30,
              y0 = 0, y1 = 100,
              line = list(color = "red", dash = "dash", width = 1)
            ),
            list(
              type = "line",
              x0 = 0, x1 = 100,
              y0 = 30, y1 = 30,
              line = list(color = "red", dash = "dash", width = 1)
            )
          )
        )

      p
    })

    # ========================================================================
    # EXPORT HANDLERS
    # ========================================================================

    output$export_full <- downloadHandler(
      filename = function() {
        paste0("ielisa_full_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(ielisa_data(), file)
      }
    )

    output$export_qc_failed <- downloadHandler(
      filename = function() {
        paste0("ielisa_negative_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        failed_data <- ielisa_data() %>%
          filter(!positive_L13 | !positive_L15)
        writexl::write_xlsx(failed_data, file)
      }
    )

    output$export_duplicates <- downloadHandler(
      filename = function() {
        paste0("ielisa_duplicates_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(duplicate_data(), file)
      }
    )

  })
}
