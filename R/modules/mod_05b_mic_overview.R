# ==============================================================================
# MODULE 1: OVERVIEW - KPIs and Run Summary
# ==============================================================================

mod_mic_overview_ui <- function(id) {
  ns <- NS(id)

  # Note: ns() is already applied by the parent coordinator module
  # So we need to get the parent namespace
  parent_ns <- function(x) paste0(gsub("-overview$", "", id), "-", x)

  tagList(
    div(
      class = "mic-overview-panel",
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
              value = if (!is.null(config$site_paths)) config$site_paths$mic_dir else "data/MIC",
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
              class = "d-flex flex-column justify-content-start gap-2",
              div(
                class = "d-flex align-items-center",
                checkboxInput(
                  parent_ns("exclude_invalid_runs"),
                  "Exclude invalid runs",
                  value = TRUE
                ),
                tags$small(
                  class = "text-muted ms-2",
                  "Removes failed runs"
                )
              ),
              div(
                class = "d-flex align-items-center",
                checkboxInput(
                  parent_ns("show_retests_only"),
                  "Show retested samples only",
                  value = FALSE
                ),
                tags$small(
                  class = "text-muted ms-2",
                  "Filter samples with multiple run dates"
                )
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

    # Row 2: Control health
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "Runs Passing Controls",
        value = textOutput(ns("kpi_runs_control_ok")),
        showcase = icon("shield-check"),
        theme = "success"
      ),

      value_box(
        title = "Runs With Control Issues",
        value = textOutput(ns("kpi_runs_control_fail")),
        showcase = icon("ban"),
        theme = "danger"
      ),

      value_box(
        title = "Control Wells Passing",
        value = textOutput(ns("kpi_control_wells")),
        showcase = icon("vial-circle-check"),
        theme = "info"
      ),

      value_box(
        title = "Control Pass Rate",
        value = textOutput(ns("kpi_control_rate")),
        showcase = icon("percent"),
        theme = "primary"
      )
    ),

    # Run summary table
    card(
      full_screen = TRUE,
      card_header("Run Metadata"),
      card_body(
        p(class = "text-muted mb-2", "Aligned with MIC parsing: run-level totals, validity and control outcomes are shown together."),
        DTOutput(ns("tbl_runs"), width = "100%"),
        class = "p-3"
      )
    ),

    # Levey-Jennings KPIs
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",
      value_box(
        title = "177T Stability",
        value = textOutput(ns("kpi_lj_177t")),
        showcase = icon("chart-line"),
        theme = "info"
      ),
      value_box(
        title = "18S2 Stability",
        value = textOutput(ns("kpi_lj_18s2")),
        showcase = icon("chart-line"),
        theme = "info"
      ),
      value_box(
        title = "RNAseP DNA Stability",
        value = textOutput(ns("kpi_lj_rnp_dna")),
        showcase = icon("chart-line"),
        theme = "info"
      ),
      value_box(
        title = "RNAseP RNA Stability",
        value = textOutput(ns("kpi_lj_rnp_rna")),
        showcase = icon("chart-line"),
        theme = "info"
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

    # Row 2: Control health
    output$kpi_runs_control_ok <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"ControlsPassing" %in% names(runs)) return("0")

      total_reviewed <- sum(!is.na(runs$ControlsPassing))
      if (!total_reviewed) return("0")

      passing <- sum(runs$ControlsPassing, na.rm = TRUE)
      glue::glue("{scales::comma(passing)} ({round(100 * passing / total_reviewed, 1)}%)")
    })

    output$kpi_runs_control_fail <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"ControlsPassing" %in% names(runs)) return("0")

      total_reviewed <- sum(!is.na(runs$ControlsPassing))
      failing <- sum(!runs$ControlsPassing, na.rm = TRUE)
      suffix <- if (isTRUE(input$exclude_invalid_runs) && failing > 0) " (excluded)" else ""

      glue::glue("{scales::comma(failing)}{suffix}")
    })

    output$kpi_control_wells <- renderText({
      ctrl <- processed_data()$control_status
      if (!nrow(ctrl)) return("0")

      total <- sum(!is.na(ctrl$ControlPass))
      pass <- sum(ctrl$ControlPass, na.rm = TRUE)
      glue::glue("{scales::comma(pass)} / {scales::comma(total)}")
    })

    output$kpi_control_rate <- renderText({
      ctrl <- processed_data()$control_status
      if (!nrow(ctrl)) return("N/A")

      total <- sum(!is.na(ctrl$ControlPass))
      if (!total) return("N/A")

      pass <- sum(ctrl$ControlPass, na.rm = TRUE)
      glue::glue("{round(100 * pass / total, 1)}%")
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

      # Remove any duplicate RunIDs (safety check)
      runs <- runs %>%
        distinct(RunID, .keep_all = TRUE)

      display <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗"),
          ControlsPassing = case_when(
            is.na(ControlsPassing) ~ "N/A",
            ControlsPassing ~ "✓",
            TRUE ~ "✗"
          ),
          ControlPassRate = if_else(is.na(ControlPassRate), "N/A", scales::percent(ControlPassRate, accuracy = 0.1)),
          ControlIssues = if_else(is.na(FailedControls), "All controls passed", FailedControls)
        )

      available_cols <- intersect(
        c(
          "RunID", "FileName", "RunDateTime", "WellCount", "TotalSamples",
          "TotalControls", "ControlsPassed", "ControlsFailed", "ControlPassRate",
          "RunValid", "ControlsPassing", "ControlIssues", "Positives", "Negatives",
          "Indeterminate", "InvalidNoDNA", "Flagged"
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
                    color = styleEqual(c('✓', '✗'), c('green', 'red')),
                    fontWeight = 'bold') %>%
        formatStyle('ControlsPassing',
                    color = styleEqual(c('✓', '✗', 'N/A'), c('green', 'red', '#6c757d')),
                    fontWeight = 'bold')
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

    # Levey-Jennings KPIs
    render_lj_kpi <- function(target_name, output_name) {
      output[[output_name]] <- renderText({
        lj <- processed_data()$lj_stats[[target_name]]

        if (is.null(lj$summary) || !nrow(lj$summary)) return("No data")

        pct <- lj$summary$Within2SD_pct[1]

        if (is.na(pct)) return("No data")

        glue::glue("{pct}% within ±2 SD")
      })
    }

    render_lj_kpi("177T", "kpi_lj_177t")
    render_lj_kpi("18S2", "kpi_lj_18s2")
    render_lj_kpi("RNAseP_DNA", "kpi_lj_rnp_dna")
    render_lj_kpi("RNAseP_RNA", "kpi_lj_rnp_rna")

  })
}
