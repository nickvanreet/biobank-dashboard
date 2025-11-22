# Simple Concordance Utility Functions
# Helper functions for simple positive/negative concordance analysis
# ============================================================================

#' Calculate Simple Concordance Metrics
#'
#' @param test1 Logical vector for test 1 results (TRUE = positive)
#' @param test2 Logical vector for test 2 results (TRUE = positive)
#' @param test1_name Character name for test 1
#' @param test2_name Character name for test 2
#' @return List with concordance metrics and confusion matrix
#' @export
calculate_simple_concordance <- function(test1, test2,
                                        test1_name = "Test 1",
                                        test2_name = "Test 2") {

  tryCatch({
    # Remove missing values
    valid_idx <- !is.na(test1) & !is.na(test2)
    test1 <- test1[valid_idx]
    test2 <- test2[valid_idx]

    n_total <- length(test1)

    if (n_total == 0) {
      return(list(
        n_total = 0,
        test1_name = test1_name,
        test2_name = test2_name,
        pct_agreement = NA,
        kappa = NA,
        kappa_interpretation = "N/A",
        n_both_positive = 0,
        n_both_negative = 0,
        n_discordant = 0,
        n_concordant = 0,
        confusion_matrix = data.frame(
          Test1 = c("Positive", "Negative"),
          Positive = c(0, 0),
          Negative = c(0, 0)
        )
      ))
    }

    # Calculate counts
    n_both_positive <- sum(test1 & test2, na.rm = TRUE)
    n_both_negative <- sum(!test1 & !test2, na.rm = TRUE)
    n_test1_only <- sum(test1 & !test2, na.rm = TRUE)
    n_test2_only <- sum(!test1 & test2, na.rm = TRUE)

    n_concordant <- n_both_positive + n_both_negative
    n_discordant <- n_test1_only + n_test2_only

    # Percent agreement
    pct_agreement <- (n_concordant / n_total) * 100

    # Cohen's Kappa
    # Observed agreement
    p_o <- n_concordant / n_total

    # Expected agreement
    n_test1_positive <- sum(test1, na.rm = TRUE)
    n_test2_positive <- sum(test2, na.rm = TRUE)
    n_test1_negative <- n_total - n_test1_positive
    n_test2_negative <- n_total - n_test2_positive

    p_e <- ((n_test1_positive * n_test2_positive) +
            (n_test1_negative * n_test2_negative)) / (n_total^2)

    kappa <- if (p_e < 1) {
      (p_o - p_e) / (1 - p_e)
    } else {
      NA_real_
    }

    # Kappa interpretation
    kappa_interpretation <- dplyr::case_when(
      is.na(kappa) ~ "Cannot calculate",
      kappa < 0 ~ "Poor",
      kappa < 0.20 ~ "Slight",
      kappa < 0.40 ~ "Fair",
      kappa < 0.60 ~ "Moderate",
      kappa < 0.80 ~ "Substantial",
      TRUE ~ "Almost Perfect"
    )

    # Create confusion matrix
    confusion_matrix <- data.frame(
      Test1 = c("Positive", "Negative"),
      Positive = c(n_both_positive, n_test2_only),
      Negative = c(n_test1_only, n_both_negative)
    )
    names(confusion_matrix) <- c(test1_name, paste0(test2_name, " Positive"),
                                  paste0(test2_name, " Negative"))

    # Return results
    list(
      n_total = n_total,
      test1_name = test1_name,
      test2_name = test2_name,
      pct_agreement = pct_agreement,
      kappa = kappa,
      kappa_interpretation = kappa_interpretation,
      n_both_positive = n_both_positive,
      n_both_negative = n_both_negative,
      n_test1_only = n_test1_only,
      n_test2_only = n_test2_only,
      n_concordant = n_concordant,
      n_discordant = n_discordant,
      confusion_matrix = confusion_matrix
    )

  }, error = function(e) {
    warning(paste("Error in calculate_simple_concordance:", e$message))
    return(list(
      n_total = 0,
      test1_name = test1_name,
      test2_name = test2_name,
      pct_agreement = NA,
      kappa = NA,
      kappa_interpretation = "Error",
      n_both_positive = 0,
      n_both_negative = 0,
      n_discordant = 0,
      n_concordant = 0,
      confusion_matrix = data.frame(
        Test1 = c("Positive", "Negative"),
        Positive = c(0, 0),
        Negative = c(0, 0)
      ),
      error = e$message
    ))
  })
}


#' Plot Simple Confusion Matrix
#'
#' @param confusion_matrix Data frame with confusion matrix
#' @param test1_name Character name for test 1 (rows)
#' @param test2_name Character name for test 2 (columns)
#' @return Plotly heatmap
#' @export
plot_simple_confusion_matrix <- function(confusion_matrix,
                                         test1_name = "Test 1",
                                         test2_name = "Test 2") {

  tryCatch({
    # Extract data
    cm_df <- confusion_matrix

    if (nrow(cm_df) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No data available"))
    }

    # Convert to long format for plotting
    cm_long <- cm_df %>%
      tidyr::pivot_longer(
        cols = -1,
        names_to = "test2_result",
        values_to = "count"
      ) %>%
      dplyr::mutate(
        test1_result = .data[[names(cm_df)[1]]],
        test2_result = gsub(paste0(test2_name, " "), "", test2_result)
      )

    # Calculate percentages
    total <- sum(cm_long$count)
    cm_long <- cm_long %>%
      dplyr::mutate(
        pct = (count / total) * 100,
        label = sprintf("%d\n(%.1f%%)", count, pct)
      )

    # Create heatmap
    plotly::plot_ly(
      data = cm_long,
      x = ~test2_result,
      y = ~test1_result,
      z = ~count,
      type = "heatmap",
      colorscale = list(
        c(0, "#f8f9fa"),
        c(0.5, "#a8d5ff"),
        c(1, "#0d6efd")
      ),
      text = ~label,
      hovertemplate = paste0(
        test1_name, ": %{y}<br>",
        test2_name, ": %{x}<br>",
        "Count: %{z}<br>",
        "<extra></extra>"
      ),
      showscale = TRUE,
      colorbar = list(title = "Count")
    ) %>%
      plotly::layout(
        title = paste("Confusion Matrix:", test1_name, "vs", test2_name),
        xaxis = list(
          title = test2_name,
          side = "bottom"
        ),
        yaxis = list(
          title = test1_name,
          autorange = "reversed"  # Positive on top
        ),
        annotations = lapply(1:nrow(cm_long), function(i) {
          list(
            x = cm_long$test2_result[i],
            y = cm_long$test1_result[i],
            text = cm_long$label[i],
            showarrow = FALSE,
            font = list(
              size = 14,
              color = if(cm_long$count[i] > total/4) "white" else "black"
            )
          )
        })
      )

  }, error = function(e) {
    warning(paste("Error in plot_simple_confusion_matrix:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(title = paste("Error:", e$message)))
  })
}


#' Plot Agreement Bars
#'
#' @param metrics List from calculate_simple_concordance
#' @return Plotly bar chart
#' @export
plot_agreement_bars <- function(metrics) {

  tryCatch({
    if (metrics$n_total == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No data available"))
    }

    # Create data for plotting
    plot_data <- tibble::tibble(
      Category = c(
        "Both Positive",
        "Both Negative",
        paste0(metrics$test1_name, " Only"),
        paste0(metrics$test2_name, " Only")
      ),
      Count = c(
        metrics$n_both_positive,
        metrics$n_both_negative,
        metrics$n_test1_only,
        metrics$n_test2_only
      ),
      Type = c(
        "Concordant",
        "Concordant",
        "Discordant",
        "Discordant"
      ),
      Percentage = (Count / metrics$n_total) * 100,
      Color = c(
        "#28a745",  # Green for both positive
        "#6c757d",  # Gray for both negative
        "#ffc107",  # Yellow for test1 only
        "#dc3545"   # Red for test2 only
      )
    )

    # Create bar chart
    plotly::plot_ly(
      data = plot_data,
      x = ~Category,
      y = ~Count,
      type = "bar",
      marker = list(color = ~Color),
      text = ~sprintf("%d (%.1f%%)", Count, Percentage),
      textposition = "outside",
      hovertemplate = paste0(
        "%{x}<br>",
        "Count: %{y}<br>",
        "Percentage: %{text}<br>",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "Test Result Distribution",
        xaxis = list(title = ""),
        yaxis = list(title = "Number of Samples"),
        showlegend = FALSE,
        margin = list(b = 100)
      ) %>%
      plotly::add_annotations(
        text = sprintf(
          "Overall Agreement: %.1f%% | Kappa: %.3f (%s)",
          metrics$pct_agreement,
          metrics$kappa,
          metrics$kappa_interpretation
        ),
        x = 0.5,
        y = 1.15,
        xref = "paper",
        yref = "paper",
        showarrow = FALSE,
        font = list(size = 12, color = "#0d6efd")
      )

  }, error = function(e) {
    warning(paste("Error in plot_agreement_bars:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(title = paste("Error:", e$message)))
  })
}


#' Plot Stratified Agreement
#'
#' @param strat_data Data frame with stratified metrics
#' @param stratify_var Character name of stratification variable
#' @return Plotly bar chart
#' @export
plot_stratified_agreement <- function(strat_data, stratify_var = "Group") {

  tryCatch({
    if (is.null(strat_data) || nrow(strat_data) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = "No stratified data available"))
    }

    # Get the grouping column name
    group_col <- names(strat_data)[1]

    # Create plot
    plotly::plot_ly(
      data = strat_data,
      x = ~pct_agreement,
      y = ~reorder(.data[[group_col]], pct_agreement),
      type = "bar",
      orientation = "h",
      marker = list(
        color = ~pct_agreement,
        colorscale = list(
          c(0, "#dc3545"),
          c(0.5, "#ffc107"),
          c(1, "#28a745")
        ),
        colorbar = list(title = "Agreement %")
      ),
      text = ~sprintf("%.1f%% (%d/%d)", pct_agreement, n_concordant, n_total),
      textposition = "outside",
      hovertemplate = paste0(
        "%{y}<br>",
        "Agreement: %{x:.1f}%<br>",
        "Total: %{customdata[0]}<br>",
        "Concordant: %{customdata[1]}<br>",
        "Discordant: %{customdata[2]}<br>",
        "<extra></extra>"
      ),
      customdata = ~cbind(n_total, n_concordant, n_discordant)
    ) %>%
      plotly::layout(
        title = paste("Agreement by", tools::toTitleCase(gsub("_", " ", stratify_var))),
        xaxis = list(title = "Agreement (%)", range = c(0, 105)),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )

  }, error = function(e) {
    warning(paste("Error in plot_stratified_agreement:", e$message))
    return(plotly::plot_ly() %>%
             plotly::layout(title = paste("Error:", e$message)))
  })
}
