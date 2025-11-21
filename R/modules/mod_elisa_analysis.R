# R/modules/mod_elisa_analysis.R
# ELISA Analysis module - QC plots and distributions
# Modeled after mod_05e_mic_analysis.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(plotly)
})

#' ELISA Analysis UI
#' @param id Module namespace ID
#' @export
mod_elisa_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "elisa-panel",
      # Section 1: DOD and PP% distributions
      card(
      card_header("Key Metrics Distributions"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput(ns("plot_dod_dist"), height = "500px"),
          plotlyOutput(ns("plot_pp_dist"), height = "500px")
        )
      )
    ),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Section 2: CV distributions
    card(
      card_header("Coefficient of Variation (CV) Analysis"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput(ns("plot_cv_dist"), height = "500px"),
          plotlyOutput(ns("plot_cv_scatter"), height = "500px")
        )
      )
    ),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Section 3: Time trends
    card(
      card_header("Time Trends"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput(ns("plot_dod_time"), height = "500px"),
          plotlyOutput(ns("plot_qc_time"), height = "500px")
        )
      )
    ),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Section 4: Threshold Performance Analysis
    card(
      card_header("Threshold Performance Analysis"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput(ns("plot_dod_threshold"), height = "500px"),
          plotlyOutput(ns("plot_pp_threshold"), height = "500px")
        )
      )
    ),

    # Spacer
    tags$div(style = "height: 16px;"),

    # Section 5: Biobank stratification
    card(
      card_header("Biobank Stratification"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          plotlyOutput(ns("plot_by_province"), height = "500px"),
          plotlyOutput(ns("plot_by_health_zone"), height = "500px")
        )
      )
    )
    )
  )
}

#' ELISA Analysis Server
#' @param id Module namespace ID
#' @param elisa_data Reactive returning all ELISA data
#' @param samples_data Reactive returning sample data only
#' @param controls_data Reactive returning control data only
#' @export
mod_elisa_analysis_server <- function(id, elisa_data, samples_data, controls_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # PLOT 1: DOD Distribution (samples vs controls)
    # ========================================================================

    output$plot_dod_dist <- renderPlotly({
      data <- elisa_data()
      if (!nrow(data) || !all(c("DOD", "sample_type") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        filter(!is.na(DOD)) %>%
        ggplot(aes(x = DOD, fill = sample_type)) +
        geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
        scale_fill_manual(
          values = c("sample" = "#4F46E5", "control" = "#10B981"),
          name = "Type"
        ) +
        theme_minimal() +
        labs(
          x = "DOD (Difference of OD)",
          y = "Count",
          title = "DOD Distribution by Sample Type"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 2: PP% Distribution
    # ========================================================================

    output$plot_pp_dist <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !"PP_percent" %in% names(data)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        filter(!is.na(PP_percent)) %>%
        ggplot(aes(x = PP_percent)) +
        geom_histogram(bins = 50, fill = "#4F46E5", alpha = 0.7) +
        geom_vline(xintercept = 100, linetype = "dashed", color = "#EF4444", linewidth = 0.8) +
        theme_minimal() +
        labs(
          x = "PP% (Percent Positivity)",
          y = "Count",
          title = "PP% Distribution (Samples)"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 3: CV Distribution (Ag+ vs Ag0)
    # ========================================================================

    output$plot_cv_dist <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !all(c("cv_Ag_plus", "cv_Ag0") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        select(cv_Ag_plus, cv_Ag0) %>%
        pivot_longer(cols = everything(), names_to = "metric", values_to = "cv") %>%
        filter(!is.na(cv)) %>%
        mutate(
          metric = recode(metric,
            "cv_Ag_plus" = "CV Ag+",
            "cv_Ag0" = "CV Ag0"
          )
        ) %>%
        ggplot(aes(x = metric, y = cv, fill = metric)) +
        geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
        geom_hline(yintercept = 20, linetype = "dashed", color = "#EF4444", linewidth = 0.7) +
        scale_fill_manual(values = c("CV Ag+" = "#4F46E5", "CV Ag0" = "#06B6D4")) +
        theme_minimal() +
        labs(
          x = NULL,
          y = "Coefficient of Variation (%)",
          title = "CV Distribution"
        ) +
        theme(legend.position = "none")

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 4: CV Scatter (Ag+ vs Ag0)
    # ========================================================================

    output$plot_cv_scatter <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !all(c("cv_Ag_plus", "cv_Ag0", "qc_overall") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        filter(!is.na(cv_Ag_plus) & !is.na(cv_Ag0)) %>%
        ggplot(aes(x = cv_Ag_plus, y = cv_Ag0, color = qc_overall)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_vline(xintercept = 20, linetype = "dashed", color = "#EF4444", linewidth = 0.6) +
        geom_hline(yintercept = 20, linetype = "dashed", color = "#EF4444", linewidth = 0.6) +
        scale_color_manual(
          values = c("TRUE" = "#10B981", "FALSE" = "#EF4444"),
          name = "QC Pass",
          labels = c("TRUE" = "Pass", "FALSE" = "Fail")
        ) +
        theme_minimal() +
        labs(
          x = "CV Ag+ (%)",
          y = "CV Ag0 (%)",
          title = "CV Correlation (Ag+ vs Ag0)"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 5: DOD Over Time
    # ========================================================================

    output$plot_dod_time <- renderPlotly({
      data <- elisa_data()
      if (!nrow(data) || !all(c("plate_date", "DOD", "sample_type") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        filter(!is.na(DOD) & !is.na(plate_date)) %>%
        ggplot(aes(x = as.Date(plate_date), y = DOD, color = sample_type)) +
        geom_point(alpha = 0.5, size = 1.5) +
        geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
        scale_color_manual(
          values = c("sample" = "#4F46E5", "control" = "#10B981"),
          name = "Type"
        ) +
        theme_minimal() +
        labs(
          x = "Plate Date",
          y = "DOD",
          title = "DOD Trend Over Time"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 6: QC Pass Rate Over Time
    # ========================================================================

    output$plot_qc_time <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !all(c("plate_date", "qc_overall") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      p <- data %>%
        filter(!is.na(plate_date)) %>%
        mutate(plate_date = as.Date(plate_date)) %>%
        group_by(plate_date) %>%
        summarise(
          qc_pass_rate = mean(qc_overall, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        ggplot(aes(x = plate_date, y = qc_pass_rate)) +
        geom_line(color = "#10B981", linewidth = 1) +
        geom_point(color = "#10B981", size = 2) +
        geom_hline(yintercept = 0.9, linetype = "dashed", color = "#EF4444", linewidth = 0.7) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_minimal() +
        labs(
          x = "Plate Date",
          y = "QC Pass Rate",
          title = "QC Pass Rate Over Time"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 7: DOD Threshold Performance
    # ========================================================================

    output$plot_dod_threshold <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !"DOD" %in% names(data)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      data_filtered <- data %>% filter(!is.na(DOD))

      if (!nrow(data_filtered)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No DOD data available", x = 0.5)))
      }

      # Calculate positivity rate across a range of DOD thresholds
      thresholds <- seq(0, 1.5, by = 0.02)

      threshold_data <- tibble(threshold = thresholds) %>%
        mutate(
          n_positive = map_dbl(threshold, ~sum(data_filtered$DOD >= .x)),
          positivity_rate = n_positive / nrow(data_filtered) * 100
        )

      # Create plot
      p <- plot_ly(threshold_data, x = ~threshold, y = ~positivity_rate, type = "scatter", mode = "lines+markers",
                   line = list(color = "#4F46E5", width = 2),
                   marker = list(color = "#4F46E5", size = 4),
                   hovertemplate = paste(
                     "<b>DOD Threshold:</b> %{x:.2f}<br>",
                     "<b>Positivity Rate:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   )) %>%
        add_trace(
          x = c(0.3, 0.3),
          y = c(0, max(threshold_data$positivity_rate)),
          type = "scatter",
          mode = "lines",
          line = list(color = "#EF4444", width = 2, dash = "dash"),
          name = "Default (0.3)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        layout(
          title = "Positivity Rate vs DOD Threshold",
          xaxis = list(title = "DOD Threshold"),
          yaxis = list(title = "Positivity Rate (%)", range = c(0, 100)),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(x = 0.8, y = 0.95)
        )

      p
    })

    # ========================================================================
    # PLOT 7b: PP% Threshold Performance
    # ========================================================================

    output$plot_pp_threshold <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !"PP_percent" %in% names(data)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data available", x = 0.5)))
      }

      data_filtered <- data %>% filter(!is.na(PP_percent))

      if (!nrow(data_filtered)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No PP% data available", x = 0.5)))
      }

      # Calculate positivity rate across a range of PP% thresholds
      thresholds <- seq(0, 200, by = 2)

      threshold_data <- tibble(threshold = thresholds) %>%
        mutate(
          n_positive = map_dbl(threshold, ~sum(data_filtered$PP_percent >= .x)),
          positivity_rate = n_positive / nrow(data_filtered) * 100
        )

      # Create plot
      p <- plot_ly(threshold_data, x = ~threshold, y = ~positivity_rate, type = "scatter", mode = "lines+markers",
                   line = list(color = "#06B6D4", width = 2),
                   marker = list(color = "#06B6D4", size = 4),
                   hovertemplate = paste(
                     "<b>PP% Threshold:</b> %{x:.0f}%<br>",
                     "<b>Positivity Rate:</b> %{y:.1f}%<br>",
                     "<extra></extra>"
                   )) %>%
        add_trace(
          x = c(20, 20),
          y = c(0, max(threshold_data$positivity_rate)),
          type = "scatter",
          mode = "lines",
          line = list(color = "#EF4444", width = 2, dash = "dash"),
          name = "Default (20%)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        add_trace(
          x = c(100, 100),
          y = c(0, max(threshold_data$positivity_rate)),
          type = "scatter",
          mode = "lines",
          line = list(color = "#F59E0B", width = 2, dash = "dash"),
          name = "Alt Threshold (100%)",
          showlegend = TRUE,
          hoverinfo = "skip"
        ) %>%
        layout(
          title = "Positivity Rate vs PP% Threshold",
          xaxis = list(title = "PP% Threshold"),
          yaxis = list(title = "Positivity Rate (%)", range = c(0, 100)),
          hovermode = "closest",
          showlegend = TRUE,
          legend = list(x = 0.8, y = 0.95)
        )

      p
    })

    # ========================================================================
    # PLOT 8: PP% by Province
    # ========================================================================

    output$plot_by_province <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !all(c("Province", "PP_percent") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No biobank linkage available", x = 0.5)))
      }

      data_filtered <- data %>%
        filter(!is.na(Province) & !is.na(PP_percent))

      if (!nrow(data_filtered)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No data with province information", x = 0.5)))
      }

      p <- data_filtered %>%
        ggplot(aes(x = reorder(Province, PP_percent, FUN = median), y = PP_percent)) +
        geom_boxplot(fill = "#4F46E5", alpha = 0.6, outlier.alpha = 0.5) +
        coord_flip() +
        theme_minimal() +
        labs(
          x = "Province",
          y = "PP%",
          title = "PP% Distribution by Province"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })

    # ========================================================================
    # PLOT 9: PP% by Health Zone
    # ========================================================================

    output$plot_by_health_zone <- renderPlotly({
      data <- samples_data()
      if (!nrow(data) || !all(c("HealthZone", "PP_percent") %in% names(data))) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "No biobank linkage available", x = 0.5)))
      }

      data_filtered <- data %>%
        filter(!is.na(HealthZone) & !is.na(PP_percent)) %>%
        group_by(HealthZone) %>%
        filter(n() >= 5) %>%  # Only show zones with at least 5 samples
        ungroup()

      if (!nrow(data_filtered)) {
        return(plotly_empty(plot_bgcolor = "#f8f9fa") %>%
          layout(title = list(text = "Insufficient data by health zone", x = 0.5)))
      }

      p <- data_filtered %>%
        ggplot(aes(x = reorder(HealthZone, PP_percent, FUN = median), y = PP_percent)) +
        geom_boxplot(fill = "#06B6D4", alpha = 0.6, outlier.alpha = 0.5) +
        coord_flip() +
        theme_minimal() +
        labs(
          x = "Health Zone",
          y = "PP%",
          title = "PP% Distribution by Health Zone (≥5 samples)"
        )

      ggplotly(p) %>% layout(hovermode = "closest")
    })
  })
}
