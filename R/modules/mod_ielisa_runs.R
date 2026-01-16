# R/modules/mod_ielisa_runs.R
# iELISA Runs module - Plate-level overview and KPIs

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
  library(plotly)
  library(scales)
})

#' iELISA Runs UI
#' @param id Module namespace ID
#' @export
mod_ielisa_runs_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "ielisa-panel",
      # KPIs
      uiOutput(ns("kpis")),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Runs summary table
      card(
        card_header("Plate-level Summary"),
        card_body(
          DTOutput(ns("runs_table"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Visualizations
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Control ODs Over Time"),
          card_body(plotlyOutput(ns("plot_controls_time"), height = "400px"))
        ),
        card(
          card_header("Control CVs Over Time"),
          card_body(plotlyOutput(ns("plot_cv_time"), height = "400px"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # More visualizations
      layout_columns(
        col_widths = c(12),
        card(
          card_header("Plate QC Heatmap"),
          card_body(plotlyOutput(ns("plot_qc_heatmap"), height = "400px"))
        )
      ),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Levey-Jennings Control Charts
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Levey-Jennings Control Charts"),
          span(class = "text-muted small", "Quality control monitoring over time")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("LiTat 1.3 Negative Control OD"),
          card_body(plotlyOutput(ns("lj_l13_neg"), height = "400px"))
        ),
        card(
          full_screen = TRUE,
          card_header("LiTat 1.3 Positive Control OD"),
          card_body(plotlyOutput(ns("lj_l13_pos"), height = "400px"))
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("LiTat 1.5 Negative Control OD"),
          card_body(plotlyOutput(ns("lj_l15_neg"), height = "400px"))
        ),
        card(
          full_screen = TRUE,
          card_header("LiTat 1.5 Positive Control OD"),
          card_body(plotlyOutput(ns("lj_l15_pos"), height = "400px"))
        )
      )
    )
  )
}

#' iELISA Runs Server
#' @param id Module namespace ID
#' @param ielisa_data Reactive returning iELISA data frame
#' @export
mod_ielisa_runs_server <- function(id, ielisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # COMPUTE RUN SUMMARY
    # ========================================================================

    runs_summary <- reactive({
      data <- ielisa_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Group by file (plate) and summarize
      data %>%
        group_by(file) %>%
        summarise(
          n_samples = n(),

          # LiTat 1.3 controls
          OD_L13_neg_mean = first(OD_L13_neg_mean),
          OD_L13_pos_mean = first(OD_L13_pos_mean),
          OD_L13_neg_cv = first(OD_L13_neg_cv),
          OD_L13_pos_cv = first(OD_L13_pos_cv),
          pct_inh_pos_13 = first(pct_inh_pos_13),

          # LiTat 1.5 controls
          OD_L15_neg_mean = first(OD_L15_neg_mean),
          OD_L15_pos_mean = first(OD_L15_pos_mean),
          OD_L15_neg_cv = first(OD_L15_neg_cv),
          OD_L15_pos_cv = first(OD_L15_pos_cv),
          pct_inh_pos_15 = first(pct_inh_pos_15),

          # Plate validity
          plate_valid_L13 = first(plate_valid_L13),
          plate_valid_L15 = first(plate_valid_L15),

          # Sample positivity rates
          pos_rate_L13 = mean(positive_L13, na.rm = TRUE),
          pos_rate_L15 = mean(positive_L15, na.rm = TRUE),

          # Mean inhibition values
          mean_inh_f2_13 = mean(pct_inh_f2_13, na.rm = TRUE),
          mean_inh_f2_15 = mean(pct_inh_f2_15, na.rm = TRUE),

          .groups = "drop"
        ) %>%
        mutate(
          plate_date = str_extract(file, "^\\d{6}"),
          plate_date = as.Date(plate_date, format = "%y%m%d")
        ) %>%
        arrange(desc(plate_date))
    })

    # ========================================================================
    # KPIs
    # ========================================================================

    output$kpis <- renderUI({
      data <- ielisa_data()
      runs <- runs_summary()

      total_files <- nrow(runs)
      total_samples <- nrow(data)

      # Count valid runs
      valid_runs_L13 <- if ("plate_valid_L13" %in% names(runs)) {
        sum(runs$plate_valid_L13 == TRUE, na.rm = TRUE)
      } else {
        0
      }

      valid_runs_L15 <- if ("plate_valid_L15" %in% names(runs)) {
        sum(runs$plate_valid_L15 == TRUE, na.rm = TRUE)
      } else {
        0
      }

      # Both antigens valid
      both_valid <- if (all(c("plate_valid_L13", "plate_valid_L15") %in% names(runs))) {
        sum(runs$plate_valid_L13 & runs$plate_valid_L15, na.rm = TRUE)
      } else {
        0
      }

      # Percent valid
      pct_valid <- if (total_files > 0) {
        (both_valid / total_files) * 100
      } else {
        0
      }

      # Sample positivity (threshold-based)
      positive_L13 <- if ("positive_L13" %in% names(data)) {
        sum(data$positive_L13 == TRUE, na.rm = TRUE)
      } else {
        0
      }

      positive_L15 <- if ("positive_L15" %in% names(data)) {
        sum(data$positive_L15 == TRUE, na.rm = TRUE)
      } else {
        0
      }

      # Both antigens positive (L13 AND L15)
      both_positive <- if (all(c("positive_L13", "positive_L15") %in% names(data))) {
        sum(data$positive_L13 & data$positive_L15, na.rm = TRUE)
      } else {
        0
      }

      # Either antigen positive (L13 OR L15)
      either_positive <- if (all(c("positive_L13", "positive_L15") %in% names(data))) {
        sum(data$positive_L13 | data$positive_L15, na.rm = TRUE)
      } else {
        0
      }

      # Single-antigen positives (positive for only one antigen)
      single_positive <- if (all(c("positive_L13", "positive_L15") %in% names(data))) {
        sum(
          (data$positive_L13 & !data$positive_L15) |
            (!data$positive_L13 & data$positive_L15),
          na.rm = TRUE
        )
      } else {
        0
      }

      # Calculate percentages for theme logic
      pct_positive_L13 <- if (total_samples > 0) (positive_L13 / total_samples) * 100 else 0
      pct_positive_L15 <- if (total_samples > 0) (positive_L15 / total_samples) * 100 else 0
      pct_both_positive <- if (total_samples > 0) (both_positive / total_samples) * 100 else 0
      pct_either_positive <- if (total_samples > 0) (either_positive / total_samples) * 100 else 0
      pct_single_positive <- if (total_samples > 0) (single_positive / total_samples) * 100 else 0

      layout_column_wrap(
        width = 1/7,
        heights_equal = "row",
        value_box(
          title = "Total Files",
          value = comma(total_files),
          showcase = icon("file-medical"),
          theme = "primary"
        ),
        value_box(
          title = "Valid Runs (Both)",
          value = format_count_with_denominator(both_valid, total_files, format_style = "full"),
          showcase = icon("circle-check"),
          theme = if (pct_valid >= 80) "success" else if (pct_valid >= 60) "warning" else "danger"
        ),
        value_box(
          title = "Total Samples",
          value = comma(total_samples),
          showcase = icon("vial"),
          theme = "info"
        ),
        value_box(
          title = "Positive LiTat 1.3",
          value = format_count_with_denominator(positive_L13, total_samples, format_style = "full"),
          showcase = icon("flask-vial"),
          theme = if (pct_positive_L13 >= 30) "success" else "secondary"
        ),
        value_box(
          title = "Positive LiTat 1.5",
          value = format_count_with_denominator(positive_L15, total_samples, format_style = "full"),
          showcase = icon("flask"),
          theme = if (pct_positive_L15 >= 30) "success" else "secondary"
        ),
        value_box(
          title = "Positive Both",
          value = format_count_with_denominator(both_positive, total_samples, format_style = "full"),
          showcase = icon("viruses"),
          theme = if (pct_both_positive >= 20) "success" else "secondary"
        ),
        value_box(
          title = "Positive ≥1 Antigen",
          value = format_count_with_denominator(either_positive, total_samples, format_style = "full"),
          showcase = icon("check-double"),
          theme = if (pct_either_positive >= 30) "success" else "secondary"
        ),
        value_box(
          title = "Positive Single Antigen",
          value = format_count_with_denominator(single_positive, total_samples, format_style = "full"),
          showcase = icon("circle-half-stroke"),
          theme = if (pct_single_positive >= 15) "warning" else "info"
        )
      )
    })

    # ========================================================================
    # RUNS TABLE
    # ========================================================================

    output$runs_table <- renderDT({
      runs <- runs_summary()

      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No iELISA plates found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format data for display
      display_data <- runs %>%
        mutate(
          plate_date = as.Date(plate_date),
          OD_L13_neg_mean = round(OD_L13_neg_mean, 3),
          OD_L13_pos_mean = round(OD_L13_pos_mean, 3),
          OD_L15_neg_mean = round(OD_L15_neg_mean, 3),
          OD_L15_pos_mean = round(OD_L15_pos_mean, 3),
          OD_L13_neg_cv = round(OD_L13_neg_cv, 1),
          OD_L13_pos_cv = round(OD_L13_pos_cv, 1),
          OD_L15_neg_cv = round(OD_L15_neg_cv, 1),
          OD_L15_pos_cv = round(OD_L15_pos_cv, 1),
          pct_inh_pos_13 = round(pct_inh_pos_13, 1),
          pct_inh_pos_15 = round(pct_inh_pos_15, 1),
          pos_rate_L13 = round(pos_rate_L13 * 100, 1),
          pos_rate_L15 = round(pos_rate_L15 * 100, 1),
          # Format validation status
          L13_Valid = ifelse(plate_valid_L13, "✓ PASS", "✗ FAIL"),
          L15_Valid = ifelse(plate_valid_L15, "✓ PASS", "✗ FAIL")
        ) %>%
        select(
          File = file,
          Date = plate_date,
          Samples = n_samples,
          `L13 NEG OD` = OD_L13_neg_mean,
          `L13 POS OD` = OD_L13_pos_mean,
          `L13 NEG CV%` = OD_L13_neg_cv,
          `L13 Valid` = L13_Valid,
          `L15 NEG OD` = OD_L15_neg_mean,
          `L15 POS OD` = OD_L15_pos_mean,
          `L15 NEG CV%` = OD_L15_neg_cv,
          `L15 Valid` = L15_Valid,
          `L13 Pos%` = pos_rate_L13,
          `L15 Pos%` = pos_rate_L15
        )

      datatable(
        display_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'L13 Valid',
          backgroundColor = styleEqual(c("✓ PASS", "✗ FAIL"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'L15 Valid',
          backgroundColor = styleEqual(c("✓ PASS", "✗ FAIL"), c("#d4edda", "#f8d7da"))
        )
    })

    # ========================================================================
    # CONTROL ODs OVER TIME
    # ========================================================================

    output$plot_controls_time <- renderPlotly({
      runs <- runs_summary()

      if (!nrow(runs)) {
        return(plotly_empty("No data available"))
      }

      # Reshape for plotting
      plot_data <- runs %>%
        select(plate_date, OD_L13_neg_mean, OD_L13_pos_mean, OD_L15_neg_mean, OD_L15_pos_mean) %>%
        pivot_longer(
          cols = -plate_date,
          names_to = "control_type",
          values_to = "od_value"
        ) %>%
        mutate(
          control_label = case_when(
            control_type == "OD_L13_neg_mean" ~ "LiTat 1.3 NEG",
            control_type == "OD_L13_pos_mean" ~ "LiTat 1.3 POS",
            control_type == "OD_L15_neg_mean" ~ "LiTat 1.5 NEG",
            control_type == "OD_L15_pos_mean" ~ "LiTat 1.5 POS"
          )
        )

      p <- ggplot(plot_data, aes(x = plate_date, y = od_value, color = control_label, group = control_label)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.5) +
        geom_hline(yintercept = c(1, 3), linetype = "dashed", color = "darkgreen", alpha = 0.5) +
        geom_hline(yintercept = c(0.3, 0.7), linetype = "dashed", color = "darkred", alpha = 0.5) +
        scale_color_manual(
          values = c(
            "LiTat 1.3 NEG" = "#2ecc71",
            "LiTat 1.3 POS" = "#e74c3c",
            "LiTat 1.5 NEG" = "#3498db",
            "LiTat 1.5 POS" = "#f39c12"
          )
        ) +
        labs(
          x = "Plate Date",
          y = "OD Value",
          color = "Control Type",
          title = ""
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(hovermode = "closest")
    })

    # ========================================================================
    # CONTROL CVs OVER TIME
    # ========================================================================

    output$plot_cv_time <- renderPlotly({
      runs <- runs_summary()

      if (!nrow(runs)) {
        return(plotly_empty("No data available"))
      }

      # Reshape for plotting
      plot_data <- runs %>%
        select(plate_date, OD_L13_neg_cv, OD_L13_pos_cv, OD_L15_neg_cv, OD_L15_pos_cv) %>%
        pivot_longer(
          cols = -plate_date,
          names_to = "control_type",
          values_to = "cv_value"
        ) %>%
        mutate(
          control_label = case_when(
            control_type == "OD_L13_neg_cv" ~ "LiTat 1.3 NEG",
            control_type == "OD_L13_pos_cv" ~ "LiTat 1.3 POS",
            control_type == "OD_L15_neg_cv" ~ "LiTat 1.5 NEG",
            control_type == "OD_L15_pos_cv" ~ "LiTat 1.5 POS"
          )
        )

      p <- ggplot(plot_data, aes(x = plate_date, y = cv_value, color = control_label, group = control_label)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 2.5) +
        geom_hline(yintercept = 20, linetype = "dashed", color = "red", alpha = 0.7) +
        scale_color_manual(
          values = c(
            "LiTat 1.3 NEG" = "#2ecc71",
            "LiTat 1.3 POS" = "#e74c3c",
            "LiTat 1.5 NEG" = "#3498db",
            "LiTat 1.5 POS" = "#f39c12"
          )
        ) +
        labs(
          x = "Plate Date",
          y = "CV (%)",
          color = "Control Type",
          title = ""
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank()
        )

      ggplotly(p, tooltip = c("x", "y", "colour")) %>%
        layout(hovermode = "closest")
    })

    # ========================================================================
    # QC HEATMAP
    # ========================================================================

    output$plot_qc_heatmap <- renderPlotly({
      runs <- runs_summary()

      if (!nrow(runs)) {
        return(plotly_empty("No data available"))
      }

      # Create QC matrix
      qc_data <- runs %>%
        mutate(
          file_label = str_trunc(file, 30, side = "right")
        ) %>%
        select(file_label, plate_date, plate_valid_L13, plate_valid_L15) %>%
        pivot_longer(
          cols = c(plate_valid_L13, plate_valid_L15),
          names_to = "antigen",
          values_to = "valid"
        ) %>%
        mutate(
          antigen = ifelse(antigen == "plate_valid_L13", "LiTat 1.3", "LiTat 1.5"),
          qc_status = ifelse(valid, "PASS", "FAIL")
        )

      p <- plot_ly(
        data = qc_data,
        x = ~antigen,
        y = ~file_label,
        z = ~as.numeric(valid),
        type = "heatmap",
        colors = c("#e74c3c", "#2ecc71"),
        colorbar = list(
          title = "QC Status",
          tickvals = c(0, 1),
          ticktext = c("FAIL", "PASS")
        ),
        hovertemplate = paste(
          "<b>File:</b> %{y}<br>",
          "<b>Antigen:</b> %{x}<br>",
          "<b>Status:</b> %{text}<br>",
          "<extra></extra>"
        ),
        text = ~qc_status
      ) %>%
        layout(
          xaxis = list(title = "Antigen"),
          yaxis = list(title = "Plate", tickfont = list(size = 10)),
          margin = list(l = 150, r = 50, t = 30, b = 50)
        )

      p
    })

    # ========================================================================
    # LEVEY-JENNINGS CONTROL CHARTS
    # ========================================================================

    # Compute Levey-Jennings statistics for iELISA controls
    compute_lj_ielisa <- function(runs_data, control_col, control_name) {
      if (!nrow(runs_data) || !control_col %in% names(runs_data)) {
        return(list(
          data = tibble(),
          summary = tibble(
            Control = control_name,
            Mean = NA_real_,
            SD = NA_real_,
            N = 0,
            Within2SD_pct = NA_real_
          )
        ))
      }

      # Filter out NA values and arrange by date
      plot_data <- runs_data %>%
        filter(!is.na(.data[[control_col]])) %>%
        arrange(plate_date) %>%
        mutate(
          control_value = .data[[control_col]],
          plate_label = if_else(
            !is.na(plate_date),
            format(plate_date, "%m/%d"),
            str_trunc(file, 15, side = "right")
          )
        )

      if (!nrow(plot_data)) {
        return(list(
          data = tibble(),
          summary = tibble(
            Control = control_name,
            Mean = NA_real_,
            SD = NA_real_,
            N = 0,
            Within2SD_pct = NA_real_
          )
        ))
      }

      # Calculate mean and SD across all plates
      mu <- mean(plot_data$control_value, na.rm = TRUE)
      sdv <- sd(plot_data$control_value, na.rm = TRUE)

      # Add SD bands to plot data
      plot_data <- plot_data %>%
        mutate(
          Mean = mu,
          SD = sdv,
          plus1 = mu + sdv,
          minus1 = mu - sdv,
          plus2 = mu + 2 * sdv,
          minus2 = mu - 2 * sdv,
          plus3 = mu + 3 * sdv,
          minus3 = mu - 3 * sdv
        )

      # Calculate % within 2 SD
      within_2sd <- sum(plot_data$control_value >= plot_data$minus2 &
                        plot_data$control_value <= plot_data$plus2, na.rm = TRUE)
      within_pct <- round(100 * within_2sd / nrow(plot_data), 1)

      list(
        data = plot_data,
        summary = tibble(
          Control = control_name,
          Mean = round(mu, 3),
          SD = round(sdv, 3),
          N = nrow(plot_data),
          Within2SD_pct = within_pct
        )
      )
    }

    # Render Levey-Jennings plot for iELISA
    render_lj_ielisa_plot <- function(lj_data, control_name, y_label = "OD Value") {
      if (is.null(lj_data$data) || !nrow(lj_data$data)) {
        return(plotly_empty() %>%
                 layout(title = list(
                   text = paste("No", control_name, "data available"),
                   font = list(size = 14)
                 )))
      }

      plot_data <- lj_data$data

      fig <- plot_ly(plot_data, x = ~plate_label, y = ~control_value,
                     type = 'scatter', mode = 'markers+lines',
                     name = 'Plate Value',
                     marker = list(size = 10, color = '#2c3e50'),
                     line = list(width = 2, color = '#2c3e50'),
                     hovertemplate = paste0(
                       "<b>Plate: %{x}</b><br>",
                       y_label, ": %{y:.3f}<br>",
                       "<extra></extra>"
                     )) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~Mean, name = 'Mean',
                  line = list(color = 'black', width = 2, dash = 'solid'),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~plus1, name = '+1 SD',
                  line = list(color = '#3498db', dash = 'dot', width = 1),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~minus1, name = '-1 SD',
                  line = list(color = '#3498db', dash = 'dot', width = 1),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~plus2, name = '+2 SD',
                  line = list(color = '#f39c12', dash = 'dash', width = 2),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~minus2, name = '-2 SD',
                  line = list(color = '#f39c12', dash = 'dash', width = 2),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~plus3, name = '+3 SD',
                  line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        add_lines(data = plot_data, x = ~plate_label, y = ~minus3, name = '-3 SD',
                  line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                  mode = 'lines', hoverinfo = 'skip', inherit = FALSE) %>%
        layout(
          xaxis = list(title = "Plate Date", tickangle = -45, automargin = TRUE),
          yaxis = list(title = y_label, automargin = TRUE),
          legend = list(orientation = 'h', y = -0.3, x = 0),
          margin = list(t = 40, r = 40, b = 100, l = 60),
          hovermode = 'closest'
        )

      fig
    }

    # LiTat 1.3 Negative Control OD Levey-Jennings
    output$lj_l13_neg <- renderPlotly({
      runs <- runs_summary()
      lj_data <- compute_lj_ielisa(runs, "OD_L13_neg_mean", "LiTat 1.3 NEG")
      render_lj_ielisa_plot(lj_data, "LiTat 1.3 NEG", "NEG OD")
    })

    # LiTat 1.3 Positive Control OD Levey-Jennings
    output$lj_l13_pos <- renderPlotly({
      runs <- runs_summary()
      lj_data <- compute_lj_ielisa(runs, "OD_L13_pos_mean", "LiTat 1.3 POS")
      render_lj_ielisa_plot(lj_data, "LiTat 1.3 POS", "POS OD")
    })

    # LiTat 1.5 Negative Control OD Levey-Jennings
    output$lj_l15_neg <- renderPlotly({
      runs <- runs_summary()
      lj_data <- compute_lj_ielisa(runs, "OD_L15_neg_mean", "LiTat 1.5 NEG")
      render_lj_ielisa_plot(lj_data, "LiTat 1.5 NEG", "NEG OD")
    })

    # LiTat 1.5 Positive Control OD Levey-Jennings
    output$lj_l15_pos <- renderPlotly({
      runs <- runs_summary()
      lj_data <- compute_lj_ielisa(runs, "OD_L15_pos_mean", "LiTat 1.5 POS")
      render_lj_ielisa_plot(lj_data, "LiTat 1.5 POS", "POS OD")
    })

  })
}
