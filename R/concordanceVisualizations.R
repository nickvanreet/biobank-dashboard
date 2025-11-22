# Concordance Visualizations Module
# Visualization functions for test concordance analysis
# ============================================================================

#' Plot interactive confusion matrix heatmap with marginals
#'
#' @param confusion_matrix Matrix or data frame with confusion matrix
#' @param test1_name Character name for test 1 (rows)
#' @param test2_name Character name for test 2 (columns)
#' @return Plotly object
#' @export
plot_confusion_matrix <- function(confusion_matrix,
                                  test1_name = "Test1",
                                  test2_name = "Test2") {

  tryCatch({
    # Convert to matrix if needed
    if (is.data.frame(confusion_matrix)) {
      cm <- as.matrix(confusion_matrix[, -1])
      rownames(cm) <- confusion_matrix[[1]]
    } else {
      cm <- as.matrix(confusion_matrix)
    }

    # Check if matrix is empty or has no dimensions
    if (is.null(cm) || length(cm) == 0 || nrow(cm) == 0 || ncol(cm) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No data available for confusion matrix"))
    }

    # Create data for heatmap
    heatmap_data <- expand.grid(
      test1 = rownames(cm),
      test2 = colnames(cm)
    )
    heatmap_data$count <- as.vector(cm)

    # Calculate percentages
    total <- sum(cm)
    heatmap_data$pct <- (heatmap_data$count / total) * 100

    # Main heatmap
    p <- plotly::plot_ly() %>%
      plotly::add_trace(
        data = heatmap_data,
        x = ~test2,
        y = ~test1,
        z = ~count,
        type = "heatmap",
        colorscale = list(
          c(0, "#f8f9fa"),
          c(0.5, "#6ea8fe"),
          c(1, "#0d6efd")
        ),
        text = ~paste0(count, " (", sprintf("%.1f", pct), "%)"),
        hovertemplate = paste0(
          test1_name, ": %{y}<br>",
          test2_name, ": %{x}<br>",
          "Count: %{text}<extra></extra>"
        ),
        showscale = TRUE,
        colorbar = list(title = "Count")
      ) %>%
      plotly::layout(
        xaxis = list(
          title = test2_name,
          side = "bottom",
          showgrid = FALSE
        ),
        yaxis = list(
          title = test1_name,
          showgrid = FALSE
        ),
        annotations = lapply(1:nrow(heatmap_data), function(i) {
          list(
            x = heatmap_data$test2[i],
            y = heatmap_data$test1[i],
            text = as.character(heatmap_data$count[i]),
            showarrow = FALSE,
            font = list(size = 16, color = "white")
          )
        })
      )

    return(p)

  }, error = function(e) {
    warning(paste("Error in plot_confusion_matrix:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot Sankey diagram showing test result flows
#'
#' @param data Data frame with test1 and test2 columns (logical)
#' @param test1_name Character name for test 1
#' @param test2_name Character name for test 2
#' @return Plotly Sankey diagram
#' @export
plot_sankey_flow <- function(data, test1_name = "Test1", test2_name = "Test2") {

  tryCatch({
    # Remove missing values
    data_clean <- data[!is.na(data$test1) & !is.na(data$test2), ]

    if (nrow(data_clean) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No valid data"))
    }

    # Count flows
    flows <- data_clean %>%
      dplyr::mutate(
        test1_label = ifelse(test1, paste(test1_name, "Positive"),
                            paste(test1_name, "Negative")),
        test2_label = ifelse(test2, paste(test2_name, "Positive"),
                            paste(test2_name, "Negative"))
      ) %>%
      dplyr::count(test1_label, test2_label)

    # Create node labels
    nodes <- unique(c(flows$test1_label, flows$test2_label))
    node_colors <- c(
      "#dc3545", "#28a745",  # Test1: Positive (red), Negative (green)
      "#dc3545", "#28a745"   # Test2: Positive (red), Negative (green)
    )

    # Map to indices
    flows$source <- match(flows$test1_label, nodes) - 1
    flows$target <- match(flows$test2_label, nodes) - 1

    # Create Sankey diagram
    plotly::plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = nodes,
        color = node_colors,
        pad = 15,
        thickness = 20
      ),
      link = list(
        source = flows$source,
        target = flows$target,
        value = flows$n,
        color = "rgba(0,0,0,0.2)"
      )
    ) %>%
      plotly::layout(
        title = list(text = paste(test1_name, "vs", test2_name, "Flow")),
        font = list(size = 12)
      )

  }, error = function(e) {
    warning(paste("Error in plot_sankey_flow:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot geographic concordance by health zone
#'
#' @param data Data frame with health_zone, concordance columns
#' @param map_data Optional spatial data for choropleth
#' @return Plotly choropleth map
#' @export
plot_geographic_concordance <- function(data, map_data = NULL) {

  tryCatch({
    # Calculate concordance by health zone
    zone_concordance <- data %>%
      dplyr::filter(!is.na(health_zone)) %>%
      dplyr::group_by(health_zone) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_concordant = sum(is_concordant, na.rm = TRUE),
        pct_concordant = (n_concordant / n_total) * 100,
        .groups = "drop"
      ) %>%
      dplyr::filter(n_total >= 5)  # Minimum 5 samples

    if (nrow(zone_concordance) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "Insufficient data for geographic analysis"))
    }

    # Create bar chart (choropleth requires geographic boundaries)
    plotly::plot_ly(
      data = zone_concordance,
      x = ~pct_concordant,
      y = ~reorder(health_zone, pct_concordant),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~pct_concordant,
        colorscale = list(
          c(0, "#dc3545"),
          c(0.5, "#ffc107"),
          c(1, "#28a745")
        ),
        colorbar = list(title = "Concordance %")
      ),
      text = ~paste0(sprintf("%.1f", pct_concordant), "% (", n_concordant, "/", n_total, ")"),
      textposition = "outside",
      hovertemplate = paste0(
        "Health Zone: %{y}<br>",
        "Concordance: %{x:.1f}%<br>",
        "Samples: %{text}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "Concordance by Health Zone",
        xaxis = list(title = "Concordance (%)", range = c(0, 105)),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )

  }, error = function(e) {
    warning(paste("Error in plot_geographic_concordance:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot dashboard of metric gauges
#'
#' @param metrics Data frame with metrics
#' @return Plotly subplot with gauges
#' @export
plot_agreement_gauges <- function(metrics) {

  tryCatch({
    # Extract key metrics
    agreement <- metrics$value[metrics$metric == "Percent Agreement"]
    kappa <- metrics$value[metrics$metric == "Cohen's Kappa"]
    sensitivity <- metrics$value[metrics$metric == "Sensitivity"]
    specificity <- metrics$value[metrics$metric == "Specificity"]

    # Convert kappa to percentage for gauge
    kappa_pct <- if (!is.na(kappa)) (kappa + 1) * 50 else 50  # Scale -1 to 1 -> 0 to 100

    # Create gauge plots
    gauge_agreement <- plotly::plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = agreement,
      title = list(text = "Agreement %"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#0d6efd"),
        steps = list(
          list(range = c(0, 70), color = "#dc3545"),
          list(range = c(70, 90), color = "#ffc107"),
          list(range = c(90, 100), color = "#28a745")
        ),
        threshold = list(
          line = list(color = "red", width = 4),
          thickness = 0.75,
          value = 90
        )
      )
    )

    gauge_kappa <- plotly::plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = kappa_pct,
      number = list(suffix = "", valueformat = ".0f"),
      title = list(text = sprintf("Kappa (%.3f)", kappa)),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#6610f2"),
        steps = list(
          list(range = c(0, 30), color = "#dc3545"),
          list(range = c(30, 60), color = "#ffc107"),
          list(range = c(60, 100), color = "#28a745")
        )
      )
    )

    gauge_sensitivity <- plotly::plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = sensitivity * 100,
      title = list(text = "Sensitivity %"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#20c997"),
        steps = list(
          list(range = c(0, 70), color = "#dc3545"),
          list(range = c(70, 90), color = "#ffc107"),
          list(range = c(90, 100), color = "#28a745")
        )
      )
    )

    gauge_specificity <- plotly::plot_ly(
      type = "indicator",
      mode = "gauge+number",
      value = specificity * 100,
      title = list(text = "Specificity %"),
      gauge = list(
        axis = list(range = list(0, 100)),
        bar = list(color = "#fd7e14"),
        steps = list(
          list(range = c(0, 70), color = "#dc3545"),
          list(range = c(70, 90), color = "#ffc107"),
          list(range = c(90, 100), color = "#28a745")
        )
      )
    )

    # Combine into subplot
    plotly::subplot(
      gauge_agreement, gauge_kappa, gauge_sensitivity, gauge_specificity,
      nrows = 2,
      margin = 0.1
    ) %>%
      plotly::layout(
        title = "Concordance Metrics Dashboard"
      )

  }, error = function(e) {
    warning(paste("Error in plot_agreement_gauges:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot ROC curves with AUC
#'
#' @param roc_list List of ROC results from calculate_roc_metrics
#' @return Plotly ROC curve plot
#' @export
plot_roc_curves <- function(roc_list) {

  tryCatch({
    # Handle single ROC result or list
    if (!is.list(roc_list[[1]])) {
      roc_list <- list(roc_list)
    }

    # Create base plot
    p <- plotly::plot_ly()

    # Color palette
    colors <- c("#0d6efd", "#dc3545", "#28a745", "#ffc107", "#6610f2", "#20c997")

    # Add each ROC curve
    for (i in seq_along(roc_list)) {
      roc_data <- roc_list[[i]]

      if (is.null(roc_data$roc_data) || nrow(roc_data$roc_data) == 0) {
        next
      }

      test_name <- roc_data$test_name
      auc <- roc_data$auc

      p <- p %>%
        plotly::add_trace(
          data = roc_data$roc_data,
          x = ~fpr,
          y = ~sensitivity,
          type = "scatter",
          mode = "lines",
          name = sprintf("%s (AUC = %.3f)", test_name, auc),
          line = list(color = colors[i], width = 2),
          hovertemplate = paste0(
            "FPR: %{x:.3f}<br>",
            "Sensitivity: %{y:.3f}<br>",
            "<extra></extra>"
          )
        )

      # Add optimal cutpoint
      optimal_idx <- which.max(roc_data$roc_data$youden_j)
      p <- p %>%
        plotly::add_trace(
          x = roc_data$roc_data$fpr[optimal_idx],
          y = roc_data$roc_data$sensitivity[optimal_idx],
          type = "scatter",
          mode = "markers",
          marker = list(size = 10, color = colors[i], symbol = "star"),
          name = sprintf("%s Optimal", test_name),
          hovertemplate = paste0(
            "Optimal Cutpoint: ", sprintf("%.3f", roc_data$optimal_cutpoint), "<br>",
            "Sensitivity: ", sprintf("%.3f", roc_data$optimal_sensitivity), "<br>",
            "Specificity: ", sprintf("%.3f", roc_data$optimal_specificity),
            "<extra></extra>"
          )
        )
    }

    # Add diagonal reference line
    p <- p %>%
      plotly::add_trace(
        x = c(0, 1),
        y = c(0, 1),
        type = "scatter",
        mode = "lines",
        line = list(dash = "dash", color = "gray"),
        name = "Random Classifier",
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      plotly::layout(
        title = "ROC Curves",
        xaxis = list(title = "False Positive Rate (1 - Specificity)", range = c(0, 1)),
        yaxis = list(title = "True Positive Rate (Sensitivity)", range = c(0, 1)),
        legend = list(x = 0.6, y = 0.2),
        hovermode = "closest"
      )

    return(p)

  }, error = function(e) {
    warning(paste("Error in plot_roc_curves:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot Bland-Altman plot
#'
#' @param bland_altman_result Result from bland_altman_analysis
#' @return Plotly Bland-Altman plot
#' @export
plot_bland_altman <- function(bland_altman_result) {

  tryCatch({
    ba <- bland_altman_result

    if (is.null(ba$plot_data) || nrow(ba$plot_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No data available"))
    }

    # Create plot
    plotly::plot_ly(
      data = ba$plot_data,
      x = ~mean,
      y = ~difference,
      type = "scatter",
      mode = "markers",
      marker = list(size = 8, color = "#0d6efd", opacity = 0.6),
      hovertemplate = paste0(
        "Mean: %{x:.3f}<br>",
        "Difference: %{y:.3f}<extra></extra>"
      )
    ) %>%
      # Mean difference line
      plotly::add_trace(
        x = c(min(ba$plot_data$mean), max(ba$plot_data$mean)),
        y = c(ba$mean_diff, ba$mean_diff),
        type = "scatter",
        mode = "lines",
        line = list(color = "#28a745", width = 2),
        name = sprintf("Mean: %.3f", ba$mean_diff),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # Upper LOA
      plotly::add_trace(
        x = c(min(ba$plot_data$mean), max(ba$plot_data$mean)),
        y = c(ba$upper_loa, ba$upper_loa),
        type = "scatter",
        mode = "lines",
        line = list(color = "#dc3545", width = 2, dash = "dash"),
        name = sprintf("+1.96 SD: %.3f", ba$upper_loa),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # Lower LOA
      plotly::add_trace(
        x = c(min(ba$plot_data$mean), max(ba$plot_data$mean)),
        y = c(ba$lower_loa, ba$lower_loa),
        type = "scatter",
        mode = "lines",
        line = list(color = "#dc3545", width = 2, dash = "dash"),
        name = sprintf("-1.96 SD: %.3f", ba$lower_loa),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # Zero reference
      plotly::add_trace(
        x = c(min(ba$plot_data$mean), max(ba$plot_data$mean)),
        y = c(0, 0),
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", width = 1, dash = "dot"),
        showlegend = FALSE,
        hoverinfo = "skip"
      ) %>%
      plotly::layout(
        title = sprintf("Bland-Altman: %s vs %s", ba$method1_name, ba$method2_name),
        xaxis = list(title = sprintf("Mean of %s and %s", ba$method1_name, ba$method2_name)),
        yaxis = list(title = sprintf("Difference (%s - %s)", ba$method1_name, ba$method2_name)),
        showlegend = TRUE,
        legend = list(x = 0.02, y = 0.98)
      )

  }, error = function(e) {
    warning(paste("Error in plot_bland_altman:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}


#' Plot temporal concordance trends with control charts
#'
#' @param data Data frame with date, concordance columns
#' @param date_col Character name of date column
#' @param group_by Character grouping period ("day", "week", "month")
#' @return Plotly time series plot with control limits
#' @export
plot_quality_trends <- function(data, date_col = "date", group_by = "week") {

  tryCatch({
    # Prepare data
    data_prep <- data %>%
      dplyr::filter(!is.na(.data[[date_col]])) %>%
      dplyr::mutate(
        date = lubridate::as_date(.data[[date_col]]),
        period = lubridate::floor_date(date, unit = group_by)
      ) %>%
      dplyr::group_by(period) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        n_concordant = sum(is_concordant, na.rm = TRUE),
        pct_concordant = (n_concordant / n_total) * 100,
        .groups = "drop"
      ) %>%
      dplyr::arrange(period)

    if (nrow(data_prep) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No temporal data available"))
    }

    # Calculate control limits
    mean_concordance <- mean(data_prep$pct_concordant, na.rm = TRUE)
    sd_concordance <- sd(data_prep$pct_concordant, na.rm = TRUE)
    ucl <- mean_concordance + 3 * sd_concordance  # Upper control limit
    lcl <- max(0, mean_concordance - 3 * sd_concordance)  # Lower control limit
    uwl <- mean_concordance + 2 * sd_concordance  # Upper warning limit
    lwl <- max(0, mean_concordance - 2 * sd_concordance)  # Lower warning limit

    # Identify out-of-control points
    data_prep <- data_prep %>%
      dplyr::mutate(
        status = dplyr::case_when(
          pct_concordant > ucl | pct_concordant < lcl ~ "Out of Control",
          pct_concordant > uwl | pct_concordant < lwl ~ "Warning",
          TRUE ~ "In Control"
        )
      )

    # Create plot
    plotly::plot_ly(
      data = data_prep,
      x = ~period,
      y = ~pct_concordant,
      color = ~status,
      colors = c(
        "In Control" = "#28a745",
        "Warning" = "#ffc107",
        "Out of Control" = "#dc3545"
      ),
      type = "scatter",
      mode = "lines+markers",
      marker = list(size = 10),
      line = list(width = 2),
      text = ~paste0(
        "Period: ", format(period, "%Y-%m-%d"), "<br>",
        "Concordance: ", sprintf("%.1f", pct_concordant), "%<br>",
        "Samples: ", n_concordant, "/", n_total
      ),
      hovertemplate = "%{text}<extra></extra>"
    ) %>%
      # Mean line
      plotly::add_trace(
        x = c(min(data_prep$period), max(data_prep$period)),
        y = c(mean_concordance, mean_concordance),
        type = "scatter",
        mode = "lines",
        line = list(color = "#0d6efd", width = 2),
        name = sprintf("Mean: %.1f%%", mean_concordance),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # UCL
      plotly::add_trace(
        x = c(min(data_prep$period), max(data_prep$period)),
        y = c(ucl, ucl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#dc3545", width = 1, dash = "dash"),
        name = sprintf("UCL: %.1f%%", ucl),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # LCL
      plotly::add_trace(
        x = c(min(data_prep$period), max(data_prep$period)),
        y = c(lcl, lcl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#dc3545", width = 1, dash = "dash"),
        name = sprintf("LCL: %.1f%%", lcl),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # UWL
      plotly::add_trace(
        x = c(min(data_prep$period), max(data_prep$period)),
        y = c(uwl, uwl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#ffc107", width = 1, dash = "dot"),
        name = sprintf("UWL: %.1f%%", uwl),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      # LWL
      plotly::add_trace(
        x = c(min(data_prep$period), max(data_prep$period)),
        y = c(lwl, lwl),
        type = "scatter",
        mode = "lines",
        line = list(color = "#ffc107", width = 1, dash = "dot"),
        name = sprintf("LWL: %.1f%%", lwl),
        showlegend = TRUE,
        hoverinfo = "skip"
      ) %>%
      plotly::layout(
        title = "Concordance Over Time (Control Chart)",
        xaxis = list(title = "Period"),
        yaxis = list(title = "Concordance (%)", range = c(0, min(105, ucl + 10))),
        legend = list(x = 0.02, y = 0.98)
      )

  }, error = function(e) {
    warning(paste("Error in plot_quality_trends:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(
               title = list(text = paste("Error:", e$message), font = list(color = "red"))
             ))
  })
}
