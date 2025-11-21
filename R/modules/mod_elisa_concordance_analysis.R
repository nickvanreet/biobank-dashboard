# ELISA Concordance Analysis Module
# Displays visualizations and statistical analyses

# UI ----
mod_elisa_concordance_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Concordance Visualizations", class = "mt-3 mb-3"),

    # Row 1: Concordance Heatmap and Scatter Plot
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Concordance Heatmap"),
        card_body(
          plotly::plotlyOutput(ns("heatmap_plot"), height = "400px")
        )
      ),
      card(
        card_header("PE vs VSG PP% Correlation"),
        card_body(
          plotly::plotlyOutput(ns("scatter_plot"), height = "400px")
        )
      )
    ),

    # Row 2: Venn Diagram and Discordance Patterns
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Positivity Overlap (Venn Diagram)"),
        card_body(
          plotOutput(ns("venn_plot"), height = "400px")
        )
      ),
      card(
        card_header("Discordance Patterns"),
        card_body(
          plotly::plotlyOutput(ns("discordance_plot"), height = "400px")
        )
      )
    ),

    # Row 3: DOD Correlation and Agreement Over Time
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("PE vs VSG DOD Correlation"),
        card_body(
          plotly::plotlyOutput(ns("dod_scatter"), height = "400px")
        )
      ),
      card(
        card_header("Agreement Over Time"),
        card_body(
          plotly::plotlyOutput(ns("time_plot"), height = "400px")
        )
      )
    ),

    # Row 4: Stratified Analysis
    h4("Stratified Analysis", class = "mt-4 mb-3"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Agreement by Province"),
        card_body(
          plotly::plotlyOutput(ns("province_plot"), height = "400px")
        )
      ),
      card(
        card_header("Agreement by Health Zone"),
        card_body(
          plotly::plotlyOutput(ns("healthzone_plot"), height = "400px")
        )
      )
    )
  )
}

# Server ----
mod_elisa_concordance_analysis_server <- function(id, concordance_results) {
  moduleServer(id, function(input, output, session) {

    # Get classified data
    classified_data <- reactive({
      concordance_results()$data
    })

    # Get confusion matrix
    confusion_matrix <- reactive({
      concordance_results()$confusion_matrix
    })

    # Get metrics
    metrics <- reactive({
      concordance_results()$metrics
    })

    # 1. Concordance Heatmap (2x2 confusion matrix)
    output$heatmap_plot <- plotly::renderPlotly({
      cm <- confusion_matrix()

      # Reshape for heatmap
      heatmap_data <- cm %>%
        pivot_longer(
          cols = c(vsg_positive, vsg_negative),
          names_to = "vsg_result",
          values_to = "count"
        ) %>%
        mutate(
          vsg_result = ifelse(vsg_result == "vsg_positive", "VSG+", "VSG-"),
          pe_result = ifelse(pe_result == "Positive", "PE+", "PE-")
        )

      plot_ly(
        data = heatmap_data,
        x = ~vsg_result,
        y = ~pe_result,
        z = ~count,
        type = "heatmap",
        colors = colorRamp(c("#f8f9fa", "#0d6efd")),
        text = ~paste("Count:", count),
        hovertemplate = "%{y} / %{x}<br>Count: %{z}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "VSG Result", side = "bottom"),
          yaxis = list(title = "PE Result"),
          font = list(size = 12)
        ) %>%
        config(displayModeBar = FALSE)
    })

    # 2. PP% Scatter Plot
    output$scatter_plot <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0) {
        return(plotly_empty("No data available"))
      }

      # Color by concordance
      colors <- c(
        "Both Positive" = "#28a745",
        "Both Negative" = "#6c757d",
        "PE+ / VSG-" = "#ffc107",
        "PE- / VSG+" = "#fd7e14"
      )

      plot_ly(
        data = data,
        x = ~pe_PP_percent,
        y = ~vsg_PP_percent,
        color = ~concordance_type,
        colors = colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.7),
        text = ~paste(
          "Sample:", coalesce(pe_sample, vsg_sample),
          "<br>PE PP%:", sprintf("%.1f", pe_PP_percent),
          "<br>VSG PP%:", sprintf("%.1f", vsg_PP_percent),
          "<br>Status:", concordance_type
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
        add_lines(
          x = c(0, 100),
          y = c(0, 100),
          line = list(color = "gray", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "skip",
          inherit = FALSE
        ) %>%
        layout(
          xaxis = list(title = "PE PP%", range = c(0, max(data$pe_PP_percent, data$vsg_PP_percent, 100) + 10)),
          yaxis = list(title = "VSG PP%", range = c(0, max(data$pe_PP_percent, data$vsg_PP_percent, 100) + 10)),
          legend = list(title = list(text = "Concordance"))
        )
    })

    # 3. Venn Diagram
    output$venn_plot <- renderPlot({
      m <- metrics()

      # Create a simple Venn diagram using base R graphics
      par(mar = c(2, 2, 2, 2))
      plot.new()
      plot.window(xlim = c(0, 10), ylim = c(0, 10))

      # Draw circles
      theta <- seq(0, 2 * pi, length.out = 100)

      # PE circle (left)
      pe_x <- 4 + 2.5 * cos(theta)
      pe_y <- 5 + 2.5 * sin(theta)
      polygon(pe_x, pe_y, col = rgb(0.2, 0.4, 0.8, 0.3), border = rgb(0.2, 0.4, 0.8))

      # VSG circle (right)
      vsg_x <- 6 + 2.5 * cos(theta)
      vsg_y <- 5 + 2.5 * sin(theta)
      polygon(vsg_x, vsg_y, col = rgb(0.8, 0.2, 0.2, 0.3), border = rgb(0.8, 0.2, 0.2))

      # Add labels
      text(3, 8, "PE+", cex = 1.5, font = 2, col = rgb(0.2, 0.4, 0.8))
      text(7, 8, "VSG+", cex = 1.5, font = 2, col = rgb(0.8, 0.2, 0.2))

      # Add counts
      # PE only (left)
      text(2.5, 5, m$n_pe_only_positive, cex = 2, font = 2)

      # Both (center)
      text(5, 5, m$n_both_positive, cex = 2, font = 2, col = "#28a745")

      # VSG only (right)
      text(7.5, 5, m$n_vsg_only_positive, cex = 2, font = 2)

      # Add legend
      text(5, 1.5,
           sprintf("Co-positivity: %d / %d (%.1f%%)",
                   m$n_both_positive, m$n_total, m$pct_copositivity),
           cex = 1.2, font = 2)
    })

    # 4. Discordance Patterns
    output$discordance_plot <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0) {
        return(plotly_empty("No data available"))
      }

      # Count by concordance type
      concordance_counts <- data %>%
        count(concordance_type) %>%
        mutate(
          pct = (n / sum(n)) * 100,
          label = sprintf("%d (%.1f%%)", n, pct)
        )

      # Color scheme
      colors <- c(
        "Both Positive" = "#28a745",
        "Both Negative" = "#6c757d",
        "PE+ / VSG-" = "#ffc107",
        "PE- / VSG+" = "#fd7e14"
      )

      plot_ly(
        data = concordance_counts,
        x = ~n,
        y = ~reorder(concordance_type, n),
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~colors[concordance_type]
        ),
        text = ~label,
        textposition = "outside",
        hovertemplate = "%{y}<br>Count: %{x}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Number of Samples"),
          yaxis = list(title = ""),
          showlegend = FALSE
        )
    })

    # 5. DOD Scatter
    output$dod_scatter <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0) {
        return(plotly_empty("No data available"))
      }

      colors <- c(
        "Both Positive" = "#28a745",
        "Both Negative" = "#6c757d",
        "PE+ / VSG-" = "#ffc107",
        "PE- / VSG+" = "#fd7e14"
      )

      plot_ly(
        data = data,
        x = ~pe_DOD,
        y = ~vsg_DOD,
        color = ~concordance_type,
        colors = colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.7),
        text = ~paste(
          "Sample:", coalesce(pe_sample, vsg_sample),
          "<br>PE DOD:", sprintf("%.3f", pe_DOD),
          "<br>VSG DOD:", sprintf("%.3f", vsg_DOD),
          "<br>Status:", concordance_type
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
        add_lines(
          x = c(-1, 3),
          y = c(-1, 3),
          line = list(color = "gray", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "skip",
          inherit = FALSE
        ) %>%
        layout(
          xaxis = list(title = "PE DOD"),
          yaxis = list(title = "VSG DOD"),
          legend = list(title = list(text = "Concordance"))
        )
    })

    # 6. Agreement Over Time
    output$time_plot <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0) {
        return(plotly_empty("No data available"))
      }

      # Calculate agreement rate by date (using PE date)
      time_data <- data %>%
        filter(!is.na(pe_plate_date)) %>%
        group_by(pe_plate_date) %>%
        summarise(
          n_total = n(),
          n_concordant = sum(is_concordant),
          pct_concordant = (n_concordant / n_total) * 100,
          .groups = "drop"
        ) %>%
        arrange(pe_plate_date)

      if (nrow(time_data) == 0) {
        return(plotly_empty("No date information available"))
      }

      plot_ly(
        data = time_data,
        x = ~pe_plate_date,
        y = ~pct_concordant,
        type = "scatter",
        mode = "lines+markers",
        marker = list(size = 8, color = "#0d6efd"),
        line = list(color = "#0d6efd", width = 2),
        text = ~paste(
          "Date:", format(pe_plate_date, "%Y-%m-%d"),
          "<br>Concordant:", n_concordant, "/", n_total,
          "<br>Agreement:", sprintf("%.1f%%", pct_concordant)
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
        add_lines(
          y = 90,
          line = list(color = "red", dash = "dash"),
          showlegend = FALSE,
          hoverinfo = "skip",
          inherit = FALSE
        ) %>%
        layout(
          xaxis = list(title = "Date (PE Test)"),
          yaxis = list(title = "Agreement (%)", range = c(0, 105)),
          annotations = list(
            x = max(time_data$pe_plate_date),
            y = 90,
            text = "90% threshold",
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "bottom",
            font = list(color = "red", size = 10)
          )
        )
    })

    # 7. Agreement by Province
    output$province_plot <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0 || all(is.na(data$Province))) {
        return(plotly_empty("No biobank data available"))
      }

      # Calculate agreement by province
      province_data <- data %>%
        filter(!is.na(Province)) %>%
        group_by(Province) %>%
        summarise(
          n_total = n(),
          n_concordant = sum(is_concordant),
          pct_concordant = (n_concordant / n_total) * 100,
          .groups = "drop"
        ) %>%
        filter(n_total >= 5) %>%  # Only show provinces with at least 5 samples
        arrange(pct_concordant)

      if (nrow(province_data) == 0) {
        return(plotly_empty("Insufficient data (need ≥5 samples per province)"))
      }

      plot_ly(
        data = province_data,
        x = ~pct_concordant,
        y = ~reorder(Province, pct_concordant),
        type = "bar",
        orientation = "h",
        marker = list(color = "#0d6efd"),
        text = ~sprintf("%.1f%% (%d/%d)", pct_concordant, n_concordant, n_total),
        textposition = "outside",
        hovertemplate = "%{y}<br>Agreement: %{x:.1f}%<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Agreement (%)", range = c(0, 105)),
          yaxis = list(title = "")
        )
    })

    # 8. Agreement by Health Zone
    output$healthzone_plot <- plotly::renderPlotly({
      data <- classified_data()

      if (nrow(data) == 0 || all(is.na(data$HealthZone))) {
        return(plotly_empty("No biobank data available"))
      }

      # Calculate agreement by health zone
      zone_data <- data %>%
        filter(!is.na(HealthZone)) %>%
        group_by(HealthZone) %>%
        summarise(
          n_total = n(),
          n_concordant = sum(is_concordant),
          pct_concordant = (n_concordant / n_total) * 100,
          .groups = "drop"
        ) %>%
        filter(n_total >= 5) %>%  # Only show zones with at least 5 samples
        arrange(pct_concordant) %>%
        slice_head(n = 15)  # Show top 15 zones

      if (nrow(zone_data) == 0) {
        return(plotly_empty("Insufficient data (need ≥5 samples per zone)"))
      }

      plot_ly(
        data = zone_data,
        x = ~pct_concordant,
        y = ~reorder(HealthZone, pct_concordant),
        type = "bar",
        orientation = "h",
        marker = list(color = "#20c997"),
        text = ~sprintf("%.1f%% (%d/%d)", pct_concordant, n_concordant, n_total),
        textposition = "outside",
        hovertemplate = "%{y}<br>Agreement: %{x:.1f}%<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Agreement (%)", range = c(0, 105)),
          yaxis = list(title = "")
        )
    })
  })
}

# Helper function for empty plotly plots
plotly_empty <- function(message = "No data available") {
  plot_ly() %>%
    layout(
      xaxis = list(visible = FALSE),
      yaxis = list(visible = FALSE),
      annotations = list(
        text = message,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, color = "gray")
      )
    )
}
