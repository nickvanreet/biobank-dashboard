# ==============================================================================
# SAMPLE JOURNEY MODULE - VISUALIZATION FUNCTIONS
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(plotly)
  library(scales)
})

# Color scheme constants
COLORS <- list(
  positive = "#e41a1c",      # Red for POS/detected
  negative = "#4daf4a",      # Green for NEG
  borderline = "#ff7f00",    # Orange for borderline
  invalid = "#984ea3",       # Purple for invalid
  biobank = "#4F46E5",       # Primary blue
  extraction = "#10B981",    # Green
  mic = "#F59E0B",           # Amber
  elisa = "#06B6D4",         # Cyan
  ielisa = "#8B5CF6"         # Violet
)

#' Plot sample timeline as horizontal Gantt chart
#' @param timeline_data Timeline tibble from create_sample_timeline
#' @return Plotly object
#' @export
plot_sample_timeline <- function(timeline_data) {
  if (is.null(timeline_data) || nrow(timeline_data) == 0) {
    return(plotly_empty() %>%
      layout(
        title = "No timeline data available",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      ))
  }

  # Assign colors based on category
  timeline_data <- timeline_data %>%
    mutate(
      color = case_when(
        category == "Biobank" ~ COLORS$biobank,
        category == "Extraction" ~ COLORS$extraction,
        category == "MIC" ~ COLORS$mic,
        category == "ELISA" ~ COLORS$elisa,
        category == "iELISA" ~ COLORS$ielisa,
        TRUE ~ "#999999"
      ),
      tooltip_text = paste0(
        "<b>", event, "</b><br>",
        "Date: ", format(date, "%Y-%m-%d"), "<br>",
        "Day: ", day_number, "<br>",
        details
      )
    )

  # Create horizontal timeline
  p <- plot_ly(timeline_data) %>%
    add_markers(
      x = ~date,
      y = ~event_id,
      color = ~category,
      colors = c(
        "Biobank" = COLORS$biobank,
        "Extraction" = COLORS$extraction,
        "MIC" = COLORS$mic,
        "ELISA" = COLORS$elisa,
        "iELISA" = COLORS$ielisa
      ),
      marker = list(size = 12),
      text = ~tooltip_text,
      hoverinfo = "text",
      showlegend = TRUE
    ) %>%
    layout(
      title = "Sample Journey Timeline",
      xaxis = list(
        title = "Date",
        type = "date",
        showgrid = TRUE
      ),
      yaxis = list(
        title = "",
        ticktext = timeline_data$event,
        tickvals = timeline_data$event_id,
        showgrid = TRUE,
        autorange = "reversed"
      ),
      hovermode = "closest",
      margin = list(l = 150, r = 50, t = 50, b = 50),
      legend = list(
        orientation = "h",
        yanchor = "bottom",
        y = -0.2,
        xanchor = "center",
        x = 0.5
      )
    )

  return(p)
}

#' Plot DRS volume as radial gauge
#' @param volume_ul DRS volume in microliters
#' @return Plotly gauge chart
#' @export
plot_drs_gauge <- function(volume_ul) {
  if (is.null(volume_ul) || is.na(volume_ul)) {
    volume_ul <- 0
  }

  # Determine color based on thresholds
  gauge_color <- if (volume_ul >= 30) {
    COLORS$negative  # Green for good
  } else if (volume_ul >= 20) {
    COLORS$borderline  # Orange for warning
  } else {
    COLORS$positive  # Red for critical
  }

  p <- plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = volume_ul,
    title = list(text = "DRS Volume (µL)"),
    delta = list(reference = 30),
    gauge = list(
      axis = list(range = list(NULL, 200)),
      bar = list(color = gauge_color),
      steps = list(
        list(range = c(0, 20), color = "rgba(228, 26, 28, 0.2)"),
        list(range = c(20, 30), color = "rgba(255, 127, 0, 0.2)"),
        list(range = c(30, 200), color = "rgba(77, 175, 74, 0.2)")
      ),
      threshold = list(
        line = list(color = "black", width = 4),
        thickness = 0.75,
        value = 30
      )
    )
  ) %>%
    layout(
      margin = list(l = 20, r = 20, t = 50, b = 20),
      height = 250
    )

  return(p)
}

#' Plot MIC targets as heatmap
#' @param mic_data MIC data row(s) for the sample
#' @return Plotly heatmap
#' @export
plot_mic_heatmap <- function(mic_data) {
  if (is.null(mic_data) || nrow(mic_data) == 0) {
    return(plotly_empty() %>%
      layout(title = "No MIC data available"))
  }

  # Define targets to display
  targets <- c("TNA", "gTNA", "TBG", "gTBG", "TCO", "TEV", "RNAseP")

  # Extract Cq values for each target
  results <- tibble(
    test_num = seq_len(nrow(mic_data)),
    test_label = paste0("Test ", seq_len(nrow(mic_data)))
  )

  # Add columns for each target
  for (target in targets) {
    # Try multiple column name patterns
    col_patterns <- c(
      paste0("Cq_median_", target),
      paste0("Cq_", target),
      paste0(target, "_Ct"),
      paste0(target, "_Cq")
    )

    value <- rep(NA_real_, nrow(mic_data))
    for (pattern in col_patterns) {
      if (pattern %in% names(mic_data)) {
        value <- mic_data[[pattern]]
        break
      }
    }

    results[[target]] <- value
  }

  # Reshape to long format for heatmap
  results_long <- results %>%
    pivot_longer(
      cols = all_of(targets),
      names_to = "target",
      values_to = "cq_value"
    ) %>%
    mutate(
      status = case_when(
        is.na(cq_value) ~ "Not Detected",
        cq_value < 40 ~ "Detected",
        TRUE ~ "Not Detected"
      ),
      color_value = case_when(
        is.na(cq_value) ~ 0,
        cq_value < 40 ~ 40 - cq_value,  # Lower Cq = stronger signal
        TRUE ~ 0
      ),
      tooltip_text = ifelse(
        is.na(cq_value),
        paste0(target, ": Not Detected"),
        sprintf("%s: Cq = %.1f", target, cq_value)
      )
    )

  # Create heatmap
  p <- plot_ly(
    data = results_long,
    x = ~target,
    y = ~test_label,
    z = ~color_value,
    type = "heatmap",
    colorscale = list(
      c(0, "#f0f0f0"),      # Gray for not detected
      c(0.5, COLORS$borderline),  # Orange for weak
      c(1, COLORS$positive)       # Red for strong
    ),
    text = ~tooltip_text,
    hoverinfo = "text",
    showscale = FALSE
  ) %>%
    layout(
      title = "MIC Target Detection",
      xaxis = list(title = "Target"),
      yaxis = list(title = "Test"),
      margin = list(l = 80, r = 50, t = 50, b = 80)
    )

  return(p)
}

#' Plot ELISA results as cards
#' @param elisa_data ELISA PE or VSG data for the sample
#' @param elisa_type Either "PE" or "VSG"
#' @return HTML card elements
#' @export
plot_elisa_cards <- function(elisa_data, elisa_type = "PE") {
  if (is.null(elisa_data) || nrow(elisa_data) == 0) {
    return(tags$div(
      class = "alert alert-info",
      sprintf("No ELISA %s data available", elisa_type)
    ))
  }

  # Create a card for each test
  cards <- lapply(1:nrow(elisa_data), function(i) {
    row <- elisa_data[i, ]

    # Extract key metrics
    dod <- if ("DOD" %in% names(row) && !is.na(row$DOD)) {
      sprintf("%.3f", row$DOD)
    } else {
      "N/A"
    }

    pp_percent <- if ("PP_percent" %in% names(row) && !is.na(row$PP_percent)) {
      row$PP_percent
    } else {
      NA_real_
    }

    pp_text <- if (!is.na(pp_percent)) {
      sprintf("%.1f%%", pp_percent)
    } else {
      "N/A"
    }

    is_positive <- if ("sample_positive" %in% names(row)) {
      row$sample_positive
    } else {
      FALSE
    }

    # Determine card color
    card_class <- if (!is.na(pp_percent)) {
      if (pp_percent >= 60) {
        "border-danger"  # Positive
      } else if (pp_percent >= 40) {
        "border-warning"  # Borderline
      } else {
        "border-success"  # Negative
      }
    } else {
      "border-secondary"
    }

    badge_class <- if (!is.na(pp_percent)) {
      if (pp_percent >= 60) {
        "bg-danger"
      } else if (pp_percent >= 40) {
        "bg-warning"
      } else {
        "bg-success"
      }
    } else {
      "bg-secondary"
    }

    result_text <- if (!is.na(pp_percent)) {
      if (pp_percent >= 60) {
        "POSITIVE"
      } else if (pp_percent >= 40) {
        "BORDERLINE"
      } else {
        "NEGATIVE"
      }
    } else {
      "UNKNOWN"
    }

    plate_date <- if ("plate_date" %in% names(row)) {
      format(as.Date(row$plate_date), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    tags$div(
      class = sprintf("card %s mb-3", card_class),
      style = "border-width: 2px;",
      tags$div(
        class = "card-header d-flex justify-content-between align-items-center",
        tags$strong(sprintf("ELISA %s - Test #%d", elisa_type, i)),
        tags$span(
          class = sprintf("badge %s", badge_class),
          result_text
        )
      ),
      tags$div(
        class = "card-body",
        tags$div(
          class = "row",
          tags$div(
            class = "col-6",
            tags$p(
              tags$strong("Date: "),
              plate_date
            )
          ),
          tags$div(
            class = "col-6",
            tags$p(
              tags$strong("DOD: "),
              dod
            )
          )
        ),
        tags$div(
          class = "row",
          tags$div(
            class = "col-12",
            tags$p(
              tags$strong("PP%: "),
              pp_text
            )
          )
        )
      )
    )
  })

  return(tagList(cards))
}

#' Display iELISA results
#' @param ielisa_data iELISA data for the sample
#' @return HTML table or message
#' @export
plot_ielisa_results <- function(ielisa_data) {
  if (is.null(ielisa_data) || nrow(ielisa_data) == 0) {
    return(tags$div(
      class = "alert alert-info",
      "No iELISA data available"
    ))
  }

  # Create table with key results
  table_rows <- lapply(1:nrow(ielisa_data), function(i) {
    row <- ielisa_data[i, ]

    plate_date <- if ("plate_date" %in% names(row)) {
      format(as.Date(row$plate_date), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    # Extract LiTat results
    litat13_result <- if ("LiTat13_Result" %in% names(row)) {
      row$LiTat13_Result
    } else {
      "N/A"
    }

    litat15_result <- if ("LiTat15_Result" %in% names(row)) {
      row$LiTat15_Result
    } else {
      "N/A"
    }

    litat13_pp <- if ("LiTat13_PP" %in% names(row) && !is.na(row$LiTat13_PP)) {
      sprintf("%.1f%%", row$LiTat13_PP)
    } else {
      "N/A"
    }

    litat15_pp <- if ("LiTat15_PP" %in% names(row) && !is.na(row$LiTat15_PP)) {
      sprintf("%.1f%%", row$LiTat15_PP)
    } else {
      "N/A"
    }

    tags$tr(
      tags$td(sprintf("Test #%d", i)),
      tags$td(plate_date),
      tags$td(litat13_result),
      tags$td(litat13_pp),
      tags$td(litat15_result),
      tags$td(litat15_pp)
    )
  })

  return(
    tags$table(
      class = "table table-striped table-hover",
      tags$thead(
        tags$tr(
          tags$th("Test"),
          tags$th("Date"),
          tags$th("LiTat 1.3 Result"),
          tags$th("LiTat 1.3 PP%"),
          tags$th("LiTat 1.5 Result"),
          tags$th("LiTat 1.5 PP%")
        )
      ),
      tags$tbody(table_rows)
    )
  )
}
