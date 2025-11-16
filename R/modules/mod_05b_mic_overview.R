# ==============================================================================
# MODULE 1: OVERVIEW - KPIs and Run Summary
# ==============================================================================

mod_mic_overview_ui <- function(id) {
  ns <- NS(id)

  # Note: ns() is already applied by the parent coordinator module
  # So we need to get the parent namespace
  parent_ns <- function(x) paste0(gsub("-overview$", "", id), "-", x)

  tagList(
    # Action bar for MIC controls
    card(
      class = "mb-3",
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(6, 3, 3),
          textInput(
            parent_ns("mic_dir"),
            NULL,
            value = "data/MIC",
            placeholder = "Path to MIC Excel files",
            width = "100%"
          ),
          div(
            class = "d-flex gap-2 align-items-end justify-content-end",
            actionButton(
              parent_ns("refresh"),
              "Refresh",
              icon = icon("sync"),
              class = "btn-primary"
            ),
            actionButton(
              parent_ns("settings"),
              "Settings",
              icon = icon("sliders"),
              class = "btn-outline-secondary"
            )
          ),
          div(
            class = "d-flex flex-column justify-content-end",
            checkboxInput(
              parent_ns("exclude_invalid_runs"),
              "Exclude invalid runs",
              value = TRUE
            ),
            tags$small(
              class = "text-muted",
              "Removes failed runs from all downstream analyses"
            )
          )
        )
      )
    ),

    # KPI Dashboard
    # Row 1: Run Metrics
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "Total Files",
        value = textOutput(ns("kpi_total_files")),
        showcase = icon("folder-open"),
        theme = "primary"
      ),

      value_box(
        title = "Total Runs",
        value = textOutput(ns("kpi_total_runs")),
        showcase = icon("microscope"),
        theme = "info"
      ),

      value_box(
        title = "Valid Runs",
        value = textOutput(ns("kpi_runs_valid")),
        showcase = icon("check-circle"),
        theme = "success"
      ),

      value_box(
        title = "Invalid Runs",
        value = textOutput(ns("kpi_runs_invalid")),
        showcase = icon("triangle-exclamation"),
        theme = "danger"
      )
    ),

    # Row 2: RNA/DNA Quality
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "DNA QC Passing",
        value = textOutput(ns("kpi_dna_quality")),
        showcase = icon("dna"),
        theme = "info"
      ),

      value_box(
        title = "Good RNA (ΔCq ≤5)",
        value = textOutput(ns("kpi_rna_good")),
        showcase = icon("star"),
        theme = "success"
      ),

      value_box(
        title = "Moderate Loss (ΔCq 5-8)",
        value = textOutput(ns("kpi_rna_moderate")),
        showcase = icon("star-half-stroke"),
        theme = "warning"
      ),

      value_box(
        title = "Poor RNA (ΔCq >8)",
        value = textOutput(ns("kpi_rna_poor")),
        showcase = icon("exclamation"),
        theme = "danger"
      ),

    ),

    # Run summary table
    card(
      full_screen = TRUE,
      card_header("Run Metadata"),
      card_body(
        DTOutput(ns("tbl_runs"), width = "100%"),
        class = "p-3"
      )
    ),

    # Levey-Jennings QC trends (moved from QC panel)
    layout_column_wrap(
      width = 1/2,
      heights_equal = "row",
      gap = "16px",
      card(
        full_screen = TRUE,
        card_header("177T Positive Control"),
        card_body(
          plotlyOutput(ns("lj_177t"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("18S2 Positive Control"),
        card_body(
          plotlyOutput(ns("lj_18s2"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("RNAseP-DNA Positive Control"),
        card_body(
          plotlyOutput(ns("lj_rnp_dna"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("RNAseP-RNA Positive Control"),
        card_body(
          plotlyOutput(ns("lj_rnp_rna"), height = "400px", width = "100%")
        )
      )
    ),

    # Control status table (was in QC panel)
    card(
      class = "mt-3",
      full_screen = TRUE,
      card_header("Control Status by Run"),
      card_body(
        DTOutput(ns("tbl_controls"), width = "100%"),
        class = "p-3"
      )
    )
  )
}

mod_mic_overview_server <- function(id, processed_data, filtered_base) {
  moduleServer(id, function(input, output, session) {

    # Row 1: Run metrics
    output$kpi_total_files <- renderText({
      files <- processed_data()$files
      if (is.null(files) || !nrow(files)) return("0")
      scales::comma(nrow(files))
    })

    output$kpi_total_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0")
      scales::comma(nrow(runs))
    })

    output$kpi_runs_valid <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"RunValid" %in% names(runs)) return("0")

      valid <- sum(runs$RunValid, na.rm = TRUE)
      pct <- if (nrow(runs)) round(100 * valid / nrow(runs), 1) else NA

      if (is.na(pct)) {
        return(scales::comma(valid))
      }

      glue::glue("{scales::comma(valid)} ({pct}%)")
    })

    output$kpi_runs_invalid <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"RunValid" %in% names(runs)) return("0")

      invalid <- sum(!runs$RunValid, na.rm = TRUE)
      total <- nrow(runs)
      if (!total) return("0")

      suffix <- if (isTRUE(input$exclude_invalid_runs) && invalid > 0) " (excluded)" else ""
      glue::glue("{scales::comma(invalid)}{suffix}")
    })

    # Row 2: RNA Preservation & QC
    output$kpi_dna_quality <- renderText({
      df <- filtered_base()
      required_cols <- c("ControlType", "Call_RNAseP_DNA")
      if (!nrow(df) || !all(required_cols %in% names(df))) return("N/A")

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      total <- sum(!is.na(df$Call_RNAseP_DNA))
      if (!total) return("N/A")

      good <- sum(df$Call_RNAseP_DNA %in% c("Positive", "LatePositive"), na.rm = TRUE)
      paste0(round(100 * good / total), "%")
    })

    # Row 2 (continued): RNA Preservation & QC
    output$kpi_rna_good <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      good <- sum(!is.na(df$RNA_Preservation_Delta) & df$RNA_Preservation_Delta <= 5, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(good, " (", round(100 * good / total), "%)")
    })

    output$kpi_rna_moderate <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      moderate <- sum(!is.na(df$RNA_Preservation_Delta) &
                      df$RNA_Preservation_Delta > 5 &
                      df$RNA_Preservation_Delta <= 8, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(moderate, " (", round(100 * moderate / total), "%)")
    })

    output$kpi_rna_poor <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      poor <- sum(!is.na(df$RNA_Preservation_Delta) & df$RNA_Preservation_Delta > 8, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(poor, " (", round(100 * poor / total), "%)")
    })

    # Runs table
    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs

      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No runs found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      display <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗")
        )

      available_cols <- intersect(
        c(
          "RunID", "FileName", "RunDateTime", "WellCount", "TotalSamples",
          "TotalControls", "Positives", "Negatives", "Indeterminate",
          "InvalidNoDNA", "Flagged", "RunValid"
        ),
        names(display)
      )

      datatable(
        display %>% select(all_of(available_cols)),
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
          dom = 'lfrtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        formatStyle('RunValid',
                    color = styleEqual(c('✓', '✗'), c('green', 'red')))
    })

    # Control status table (moved from QC module)
    output$tbl_controls <- renderDT({
      ctrl <- processed_data()$control_status

      if (!nrow(ctrl)) {
        return(datatable(
          tibble(Message = "No control data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      available_cols <- intersect(
        c("RunID", "SampleName", "ControlType", "ControlPass", "ControlFlag"),
        names(ctrl)
      )

      ctrl <- ctrl %>%
        mutate(ControlPass = if_else(ControlPass, "✓ Pass", "✗ Fail"))

      datatable(
        ctrl %>% select(all_of(available_cols)),
        options = list(
          dom = 'tp',
          paging = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe"
      ) %>%
        formatStyle('ControlPass',
                    color = styleEqual(c('✓ Pass', '✗ Fail'), c('green', 'red')))
    })

    # Levey-Jennings plots (moved from QC module)
    render_lj_plot <- function(target_name, output_name) {
      output[[output_name]] <- renderPlotly({
        lj <- processed_data()$lj_stats[[target_name]]

        if (is.null(lj) || !is.list(lj)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue::glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        if (is.null(lj$data) || !nrow(lj$data)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue::glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        fig <- plot_ly(lj$data, x = ~RunID, y = ~Cq_mean,
                        type = 'scatter', mode = 'markers+lines',
                        name = 'Run Mean',
                        marker = list(size = 12, color = '#2c3e50'),
                        line = list(width = 3, color = '#2c3e50'),
                        hovertemplate = paste0(
                          "<b>Run: %{x}</b><br>",
                          "Mean Cq: %{y:.2f}<br>",
                          "<extra></extra>"
                        )) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~Mean, name = 'Mean',
                    line = list(color = 'black', width = 2, dash = 'solid'),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus1, name = '+1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus1, name = '-1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus2, name = '+2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus2, name = '-2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~plus3, name = '+3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, x = ~RunID, y = ~minus3, name = '-3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          layout(
            xaxis = list(title = "Run ID", tickangle = -45, automargin = TRUE),
            yaxis = list(title = "Cq Value", automargin = TRUE),
            legend = list(orientation = 'h', y = -0.25, x = 0),
            margin = list(t = 60, r = 40, b = 120, l = 60),
            hovermode = 'closest'
          )

        if (isTRUE(lj$fallback)) {
          fig <- fig %>%
            layout(
              annotations = list(
                list(
                  text = "No control wells detected – showing all samples",
                  x = 0.5,
                  xref = "paper",
                  y = 1.1,
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(color = "#e74c3c", size = 12)
                )
              )
            )
        }

        fig
      })
    }

    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")

  })
}
