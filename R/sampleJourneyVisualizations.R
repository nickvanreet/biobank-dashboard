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
  p <- plot_ly(timeline_data, type = "scatter", mode = "markers") %>%
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
#' @param initial_volume_ul Initial DRS volume in microliters (default 2000)
#' @param num_extractions Number of extractions performed (each uses 300µL)
#' @return Plotly gauge chart showing remaining volume
#' @export
plot_drs_gauge <- function(initial_volume_ul = 2000, num_extractions = 0) {
  # Calculate remaining volume: initial - (300µL per extraction)
  volume_used <- num_extractions * 300
  remaining_volume_ul <- max(0, initial_volume_ul - volume_used)

  # Determine color based on remaining volume thresholds
  # Critical: < 300µL (can't do another extraction)
  # Warning: 300-600µL (only 1-2 extractions left)
  # Good: > 600µL
  gauge_color <- if (remaining_volume_ul >= 600) {
    COLORS$negative  # Green for good
  } else if (remaining_volume_ul >= 300) {
    COLORS$borderline  # Orange for warning
  } else {
    COLORS$positive  # Red for critical
  }

  # Calculate how many more extractions are possible
  extractions_remaining <- floor(remaining_volume_ul / 300)

  p <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = remaining_volume_ul,
    title = list(
      text = sprintf("<b>DRS Volume</b><br><span style='font-size:0.65em;color:#64748b'>Initial: %d µL | Used: %d µL</span>",
                     initial_volume_ul, volume_used),
      font = list(size = 14)
    ),
    number = list(
      suffix = " µL",
      font = list(size = 24, color = "#1e293b")
    ),
    gauge = list(
      axis = list(
        range = list(0, initial_volume_ul),
        tickvals = seq(0, initial_volume_ul, by = 500),
        ticktext = paste0(seq(0, initial_volume_ul, by = 500)),
        tickfont = list(size = 10)
      ),
      bar = list(color = gauge_color, thickness = 0.7),
      bgcolor = "#f1f5f9",
      borderwidth = 0,
      steps = list(
        list(range = c(0, 300), color = "rgba(228, 26, 28, 0.15)"),       # Critical
        list(range = c(300, 600), color = "rgba(255, 127, 0, 0.15)"),     # Warning
        list(range = c(600, initial_volume_ul), color = "rgba(77, 175, 74, 0.15)")  # Good
      ),
      threshold = list(
        line = list(color = "#475569", width = 3),
        thickness = 0.75,
        value = 300  # Threshold for one more extraction
      )
    )
  ) %>%
    layout(
      margin = list(l = 30, r = 30, t = 60, b = 50),
      font = list(family = "system-ui, -apple-system, sans-serif"),
      annotations = list(
        list(
          x = 0.5,
          y = -0.05,
          xref = "paper",
          yref = "paper",
          text = sprintf("<b>%d extraction%s remaining</b>",
                        extractions_remaining,
                        if(extractions_remaining != 1) "s" else ""),
          showarrow = FALSE,
          font = list(
            size = 12,
            color = if (extractions_remaining == 0) COLORS$positive else if (extractions_remaining <= 2) COLORS$borderline else COLORS$negative
          )
        )
      )
    ) %>%
    config(displayModeBar = FALSE)

  return(p)
}

#' Display detailed MIC results with Cq values and delta calculations - Condensed version
#' @param mic_data MIC data row(s) for the sample
#' @return HTML div with detailed MIC information
#' @export
plot_mic_detailed <- function(mic_data) {
  if (is.null(mic_data) || nrow(mic_data) == 0) {
    return(tags$div(
      class = "text-center text-muted py-3",
      icon("dna", class = "fa-2x mb-2"),
      tags$p("No MIC data available")
    ))
  }

  # Create condensed cards for each MIC test
  cards <- lapply(1:nrow(mic_data), function(i) {
    row <- mic_data[i, ]

    # Extract Cq values for main targets
    cq_177t <- if ("Cq_177T" %in% names(row)) row$Cq_177T else NA_real_
    cq_18s2 <- if ("Cq_18S2" %in% names(row)) row$Cq_18S2 else NA_real_
    cq_rnasep_dna <- if ("Cq_RNAseP_DNA" %in% names(row)) row$Cq_RNAseP_DNA else
                     if ("RNAseP_DNA_Cq" %in% names(row)) row$RNAseP_DNA_Cq else NA_real_
    cq_rnasep_rna <- if ("Cq_RNAseP_RNA" %in% names(row)) row$Cq_RNAseP_RNA else
                     if ("RNAseP_RNA_Cq" %in% names(row)) row$RNAseP_RNA_Cq else NA_real_

    # Get interpretation/marker columns
    marker_177t <- if ("marker_177T" %in% names(row)) row$marker_177T else "Unknown"
    marker_18s2 <- if ("marker_18S2" %in% names(row)) row$marker_18S2 else "Unknown"

    # Get test date
    test_date <- if ("RunDate" %in% names(row)) {
      format(as.Date(row$RunDate), "%Y-%m-%d")
    } else if ("RunDateTime" %in% names(row)) {
      format(as.Date(row$RunDateTime), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    # Get result details
    final_call <- if ("FinalCall" %in% names(row) && !is.na(row$FinalCall)) {
      as.character(row$FinalCall)
    } else {
      "Unknown"
    }

    confidence_score <- if ("ConfidenceScore" %in% names(row) && !is.na(row$ConfidenceScore)) {
      as.character(row$ConfidenceScore)
    } else {
      "Unknown"
    }

    rna_quality <- if ("RNA_Quality" %in% names(row) && !is.na(row$RNA_Quality)) {
      as.character(row$RNA_Quality)
    } else {
      NA
    }

    dna_quality <- if ("DNA_Quality" %in% names(row) && !is.na(row$DNA_Quality)) {
      as.character(row$DNA_Quality)
    } else {
      NA
    }

    # Determine badge class for final call
    final_call_badge <- if (grepl("POS|Positive", final_call, ignore.case = TRUE)) {
      list(class = "bg-danger", text = final_call)
    } else if (grepl("NEG|Negative", final_call, ignore.case = TRUE)) {
      list(class = "bg-success", text = final_call)
    } else if (grepl("INVALID|Invalid", final_call, ignore.case = TRUE)) {
      list(class = "bg-secondary", text = final_call)
    } else if (grepl("BORDERLINE|Indeterminate", final_call, ignore.case = TRUE)) {
      list(class = "bg-warning", text = final_call)
    } else {
      list(class = "bg-secondary", text = final_call)
    }

    # Confidence badge
    confidence_class <- dplyr::case_when(
      confidence_score == "High" ~ "text-success",
      confidence_score == "Medium" ~ "text-warning",
      confidence_score == "Low" ~ "text-danger",
      TRUE ~ "text-muted"
    )

    # Detection info
    is_177t_pos <- !is.na(marker_177t) && marker_177t == "Positive"
    is_18s2_pos <- !is.na(marker_18s2) && marker_18s2 == "Positive"
    detection_text <- if (is_177t_pos && is_18s2_pos) {
      "Double detection"
    } else if (is_177t_pos || is_18s2_pos) {
      "Single target"
    } else {
      "No detection"
    }

    tags$div(
      class = "sj-test-card",
      # Header
      div(
        class = "sj-test-card-header",
        div(
          tags$strong(sprintf("MIC #%d", i)),
          tags$span(class = "text-muted ms-2", test_date)
        ),
        tags$span(class = sprintf("badge %s", final_call_badge$class), final_call_badge$text)
      ),
      # Body
      div(
        class = "sj-test-card-body",
        # Row 1: Cq values
        div(
          class = "row",
          div(
            class = "col-3",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "177T (DNA)"),
              div(class = "sj-metric-value",
                if (!is.na(cq_177t)) sprintf("%.1f", cq_177t) else "ND",
                if (is_177t_pos) tags$span(class = "text-danger ms-1", icon("check", class = "fa-xs"))
              )
            )
          ),
          div(
            class = "col-3",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "18S2 (RNA)"),
              div(class = "sj-metric-value",
                if (!is.na(cq_18s2)) sprintf("%.1f", cq_18s2) else "ND",
                if (is_18s2_pos) tags$span(class = "text-danger ms-1", icon("check", class = "fa-xs"))
              )
            )
          ),
          div(
            class = "col-3",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "RNAseP-DNA"),
              div(class = "sj-metric-value", if (!is.na(cq_rnasep_dna)) sprintf("%.1f", cq_rnasep_dna) else "ND")
            )
          ),
          div(
            class = "col-3",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "RNAseP-RNA"),
              div(class = "sj-metric-value", if (!is.na(cq_rnasep_rna)) sprintf("%.1f", cq_rnasep_rna) else "ND")
            )
          )
        ),
        # Row 2: Quality info
        div(
          class = "d-flex justify-content-between align-items-center pt-2 mt-2",
          style = "border-top: 1px solid #f1f5f9;",
          div(
            class = "d-flex gap-3",
            tags$small(
              tags$span(class = "text-muted", "Confidence: "),
              tags$span(class = confidence_class, tags$strong(confidence_score))
            ),
            tags$small(
              tags$span(class = "text-muted", "Detection: "),
              tags$span(detection_text)
            )
          ),
          if (!is.na(rna_quality) || !is.na(dna_quality)) {
            div(
              class = "d-flex gap-2",
              if (!is.na(rna_quality)) {
                tags$span(
                  class = paste("badge", if(rna_quality == "PASS") "bg-success" else "bg-warning"),
                  style = "font-size: 0.65rem;",
                  "RNA"
                )
              },
              if (!is.na(dna_quality)) {
                tags$span(
                  class = paste("badge", if(dna_quality == "PASS") "bg-success" else "bg-warning"),
                  style = "font-size: 0.65rem;",
                  "DNA"
                )
              }
            )
          }
        )
      )
    )
  })

  return(tagList(cards))
}

#' Plot ELISA results as compact cards
#' @param elisa_data ELISA PE or VSG data for the sample
#' @param elisa_type Either "PE" or "VSG"
#' @return HTML card elements
#' @export
plot_elisa_cards <- function(elisa_data, elisa_type = "PE") {
  if (is.null(elisa_data) || nrow(elisa_data) == 0) {
    return(tags$div(
      class = "text-center text-muted py-3",
      icon("vial", class = "fa-2x mb-2"),
      tags$p(sprintf("No ELISA %s data available", elisa_type))
    ))
  }

  # Create compact cards for each test
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

    pp_text <- if (!is.na(pp_percent)) sprintf("%.1f", pp_percent) else "N/A"

    badge_class <- if (!is.na(pp_percent)) {
      if (pp_percent >= 60) "bg-danger"
      else if (pp_percent >= 40) "bg-warning"
      else "bg-success"
    } else {
      "bg-secondary"
    }

    result_text <- if (!is.na(pp_percent)) {
      if (pp_percent >= 60) "POS"
      else if (pp_percent >= 40) "BORDERLINE"
      else "NEG"
    } else {
      "UNK"
    }

    plate_date <- if ("plate_date" %in% names(row)) {
      format(as.Date(row$plate_date), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    tags$div(
      class = "sj-test-card",
      # Header
      div(
        class = "sj-test-card-header",
        div(
          tags$strong(sprintf("ELISA %s #%d", elisa_type, i)),
          tags$span(class = "text-muted ms-2", plate_date)
        ),
        tags$span(class = sprintf("badge %s", badge_class), result_text)
      ),
      # Body
      div(
        class = "sj-test-card-body",
        div(
          class = "row",
          div(
            class = "col-6",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "PP%"),
              div(class = "sj-metric-value",
                pp_text,
                if (!is.na(pp_percent)) tags$small(class = "text-muted", "%")
              )
            )
          ),
          div(
            class = "col-6",
            div(class = "sj-metric",
              div(class = "sj-metric-label", "DOD"),
              div(class = "sj-metric-value", dod)
            )
          )
        )
      )
    )
  })

  return(tagList(cards))
}

#' Display iELISA results - Condensed version
#' @param ielisa_data iELISA data for the sample
#' @return HTML cards with clear positivity indication
#' @export
plot_ielisa_results <- function(ielisa_data) {
  if (is.null(ielisa_data) || nrow(ielisa_data) == 0) {
    return(tags$div(
      class = "text-center text-muted py-3",
      icon("flask", class = "fa-2x mb-2"),
      tags$p("No iELISA data available")
    ))
  }

  # Create compact cards for each test
  cards <- lapply(1:nrow(ielisa_data), function(i) {
    row <- ielisa_data[i, ]

    plate_date <- if ("plate_date" %in% names(row) && !is.na(row$plate_date)) {
      format(as.Date(row$plate_date), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    # Extract LiTat 1.3 results
    litat13_positive <- if ("positive_L13" %in% names(row) && !is.na(row$positive_L13)) {
      row$positive_L13
    } else {
      NA
    }

    litat13_inhibition <- if ("pct_inh_f2_13" %in% names(row) && !is.na(row$pct_inh_f2_13)) {
      row$pct_inh_f2_13
    } else {
      NA_real_
    }

    # Extract LiTat 1.5 results
    litat15_positive <- if ("positive_L15" %in% names(row) && !is.na(row$positive_L15)) {
      row$positive_L15
    } else {
      NA
    }

    litat15_inhibition <- if ("pct_inh_f2_15" %in% names(row) && !is.na(row$pct_inh_f2_15)) {
      row$pct_inh_f2_15
    } else {
      NA_real_
    }

    # Determine overall result
    is_positive <- isTRUE(litat13_positive) || isTRUE(litat15_positive)
    overall_badge <- if (is_positive) {
      list(class = "bg-danger", text = "POS")
    } else if (!is.na(litat13_positive) || !is.na(litat15_positive)) {
      list(class = "bg-success", text = "NEG")
    } else {
      list(class = "bg-secondary", text = "UNK")
    }

    # Helper to create antigen mini-display
    create_antigen_mini <- function(name, positive, inhibition) {
      badge_class <- if (!is.na(positive) && positive) "text-danger" else if (!is.na(positive)) "text-success" else "text-muted"
      status_icon <- if (!is.na(positive) && positive) "check" else if (!is.na(positive)) "times" else "question"
      inh_text <- if (!is.na(inhibition)) sprintf("%.0f%%", inhibition) else "N/A"

      div(
        class = "col-6",
        div(
          class = "sj-metric",
          div(class = "sj-metric-label", name),
          div(class = "sj-metric-value",
            tags$span(class = badge_class, icon(status_icon, class = "fa-xs"), " "),
            inh_text
          )
        )
      )
    }

    tags$div(
      class = "sj-test-card",
      # Header
      div(
        class = "sj-test-card-header",
        div(
          tags$strong(sprintf("iELISA #%d", i)),
          tags$span(class = "text-muted ms-2", plate_date)
        ),
        tags$span(class = sprintf("badge %s", overall_badge$class), overall_badge$text)
      ),
      # Body
      div(
        class = "sj-test-card-body",
        div(
          class = "row",
          create_antigen_mini("LiTat 1.3", litat13_positive, litat13_inhibition),
          create_antigen_mini("LiTat 1.5", litat15_positive, litat15_inhibition)
        )
      )
    )
  })

  return(tagList(cards))
}
