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
      text = sprintf("DRS Remaining Volume<br><span style='font-size:0.7em;color:#666'>Initial: %d µL | Used: %d µL (%d extractions)</span>",
                     initial_volume_ul, volume_used, num_extractions)
    ),
    number = list(
      suffix = " µL",
      font = list(size = 28)
    ),
    gauge = list(
      axis = list(
        range = list(0, initial_volume_ul),
        tickvals = seq(0, initial_volume_ul, by = 500),
        ticktext = paste0(seq(0, initial_volume_ul, by = 500), "µL")
      ),
      bar = list(color = gauge_color),
      steps = list(
        list(range = c(0, 300), color = "rgba(228, 26, 28, 0.2)"),       # Critical
        list(range = c(300, 600), color = "rgba(255, 127, 0, 0.2)"),     # Warning
        list(range = c(600, initial_volume_ul), color = "rgba(77, 175, 74, 0.2)")  # Good
      ),
      threshold = list(
        line = list(color = "black", width = 4),
        thickness = 0.75,
        value = 300  # Threshold for one more extraction
      )
    ),
    height = 280
  ) %>%
    layout(
      margin = list(l = 20, r = 20, t = 80, b = 20),
      annotations = list(
        list(
          x = 0.5,
          y = -0.15,
          xref = "paper",
          yref = "paper",
          text = sprintf("<b>%d extraction(s) remaining</b>", extractions_remaining),
          showarrow = FALSE,
          font = list(
            size = 14,
            color = if (extractions_remaining == 0) COLORS$positive else if (extractions_remaining <= 2) COLORS$borderline else COLORS$negative
          )
        )
      )
    )

  return(p)
}

#' Display detailed MIC results with Cq values and delta calculations
#' @param mic_data MIC data row(s) for the sample
#' @return HTML div with detailed MIC information
#' @export
plot_mic_detailed <- function(mic_data) {
  if (is.null(mic_data) || nrow(mic_data) == 0) {
    return(tags$div(
      class = "alert alert-info",
      "No MIC data available"
    ))
  }

  # Create detailed cards for each MIC test
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
    marker_rnasep_dna <- if ("RNAseP_DNA" %in% names(row)) row$RNAseP_DNA else "Unknown"
    marker_rnasep_rna <- if ("RNAseP_RNA" %in% names(row)) row$RNAseP_RNA else "Unknown"

    # Calculate deltas
    delta_rnasep <- if (!is.na(cq_rnasep_rna) && !is.na(cq_rnasep_dna)) {
      cq_rnasep_rna - cq_rnasep_dna
    } else {
      NA_real_
    }

    delta_177t_18s2 <- if (!is.na(cq_177t) && !is.na(cq_18s2)) {
      abs(cq_177t - cq_18s2)
    } else {
      NA_real_
    }

    # Helper function to create target display
    create_target_display <- function(name, cq, status) {
      cq_text <- if (!is.na(cq)) sprintf("Cq: %.1f", cq) else "ND"
      status_badge <- if (!is.na(status) && status == "Positive") {
        tags$span(class = "badge bg-danger ms-2", "Positive")
      } else if (!is.na(status) && status == "Negative") {
        tags$span(class = "badge bg-success ms-2", "Negative")
      } else {
        tags$span(class = "badge bg-secondary ms-2", "Unknown")
      }

      tags$div(
        class = "d-flex justify-content-between align-items-center mb-2",
        tags$span(tags$strong(paste0(name, ":")), " ", cq_text),
        status_badge
      )
    }

    # Determine RNA preservation quality
    rnasep_quality <- if (!is.na(delta_rnasep)) {
      if (delta_rnasep <= 5) {
        list(text = "Good RNA preservation", class = "success")
      } else if (delta_rnasep <= 8) {
        list(text = "Moderate RNA loss", class = "warning")
      } else {
        list(text = "Poor RNA preservation", class = "danger")
      }
    } else {
      list(text = "Unknown", class = "secondary")
    }

    # Determine double detection status
    double_detection <- if (!is.na(delta_177t_18s2) &&
                           marker_177t == "Positive" &&
                           marker_18s2 == "Positive") {
      list(text = sprintf("Double detection confirmed (Δ: %.1f)", delta_177t_18s2),
           class = "success")
    } else if (marker_177t == "Positive" || marker_18s2 == "Positive") {
      list(text = "Single target detection", class = "warning")
    } else {
      list(text = "No detection", class = "secondary")
    }

    # Get test date
    test_date <- if ("RunDate" %in% names(row)) {
      format(as.Date(row$RunDate), "%Y-%m-%d")
    } else if ("RunDateTime" %in% names(row)) {
      format(as.Date(row$RunDateTime), "%Y-%m-%d")
    } else {
      "Unknown"
    }

    # Get sample identifiers
    linked_barcode <- if ("LinkedBarcode" %in% names(row) && !is.na(row$LinkedBarcode)) {
      as.character(row$LinkedBarcode)
    } else {
      NA_character_
    }

    linked_numero <- if ("LinkedNumero" %in% names(row) && !is.na(row$LinkedNumero)) {
      as.character(row$LinkedNumero)
    } else {
      NA_character_
    }

    # Get additional result details
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

    pipeline_quality <- if ("PipelineQualityFlag" %in% names(row) && !is.na(row$PipelineQualityFlag)) {
      as.character(row$PipelineQualityFlag)
    } else {
      NA_character_
    }

    rna_quality <- if ("RNA_Quality" %in% names(row) && !is.na(row$RNA_Quality)) {
      as.character(row$RNA_Quality)
    } else {
      "Unknown"
    }

    dna_quality <- if ("DNA_Quality" %in% names(row) && !is.na(row$DNA_Quality)) {
      as.character(row$DNA_Quality)
    } else {
      "Unknown"
    }

    rna_preservation <- if ("RNA_Preservation_Status" %in% names(row) && !is.na(row$RNA_Preservation_Status)) {
      as.character(row$RNA_Preservation_Status)
    } else {
      NA_character_
    }

    decision_reason <- if ("DecisionReason" %in% names(row) && !is.na(row$DecisionReason)) {
      as.character(row$DecisionReason)
    } else {
      NA_character_
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

    # Determine badge class for confidence
    confidence_badge <- if (confidence_score == "High") {
      list(class = "bg-success", icon = "check-circle")
    } else if (confidence_score == "Medium") {
      list(class = "bg-warning", icon = "exclamation-triangle")
    } else if (confidence_score == "Low") {
      list(class = "bg-danger", icon = "exclamation-circle")
    } else {
      list(class = "bg-secondary", icon = "question-circle")
    }

    tags$div(
      class = "card mb-3",
      tags$div(
        class = "card-header bg-light d-flex justify-content-between align-items-center",
        tags$div(
          tags$strong(sprintf("MIC Test #%d", i)),
          tags$small(class = "text-muted ms-2", paste("Date:", test_date))
        ),
        tags$span(
          class = sprintf("badge %s", final_call_badge$class),
          final_call_badge$text
        )
      ),
      tags$div(
        class = "card-body",
        # Sample Identifiers Section
        if (!is.na(linked_barcode) || !is.na(linked_numero)) {
          tags$div(
            class = "mb-3 p-2",
            style = "background-color: rgba(79, 70, 229, 0.05); border-left: 3px solid #4F46E5;",
            tags$div(
              class = "row",
              if (!is.na(linked_barcode)) {
                tags$div(
                  class = "col-6",
                  tags$small(
                    tags$strong("Barcode: "),
                    tags$code(class = "text-primary", linked_barcode)
                  )
                )
              },
              if (!is.na(linked_numero)) {
                tags$div(
                  class = "col-6",
                  tags$small(
                    tags$strong("Lab Number: "),
                    tags$code(class = "text-primary", linked_numero)
                  )
                )
              }
            )
          )
        },

        # Final Call and Confidence Section
        tags$div(
          class = "mb-3",
          tags$div(
            class = "d-flex justify-content-between align-items-center mb-2",
            tags$div(
              tags$strong("Confidence: "),
              tags$span(
                class = sprintf("badge %s ms-2", confidence_badge$class),
                icon(confidence_badge$icon),
                " ",
                confidence_score
              )
            )
          ),
          if (!is.na(decision_reason)) {
            tags$div(
              class = "small text-muted",
              tags$strong("Decision: "),
              decision_reason
            )
          }
        ),

        tags$hr(),

        tags$h6("Target Detection:", class = "mb-3"),
        create_target_display("177T (DNA)", cq_177t, marker_177t),
        create_target_display("18S2 (RNA)", cq_18s2, marker_18s2),
        create_target_display("RNAseP-DNA", cq_rnasep_dna, marker_rnasep_dna),
        create_target_display("RNAseP-RNA", cq_rnasep_rna, marker_rnasep_rna),

        tags$hr(),

        tags$h6("Quality Metrics:", class = "mb-3"),
        # RNA and DNA Quality
        tags$div(
          class = "row mb-2",
          tags$div(
            class = "col-6",
            tags$small(
              tags$strong("RNA Quality: "),
              tags$span(class = if (rna_quality == "PASS") "text-success" else "text-warning", rna_quality)
            )
          ),
          tags$div(
            class = "col-6",
            tags$small(
              tags$strong("DNA Quality: "),
              tags$span(class = if (dna_quality == "PASS") "text-success" else "text-warning", dna_quality)
            )
          )
        ),
        # RNA Preservation
        if (!is.na(rna_preservation)) {
          tags$div(
            class = "alert alert-info mb-2 py-2",
            role = "alert",
            tags$small(
              tags$strong("RNA Preservation: "),
              rna_preservation,
              if (!is.na(delta_rnasep)) {
                tags$span(sprintf(" (ΔCq: %.1f)", delta_rnasep))
              }
            )
          )
        } else {
          tags$div(
            class = sprintf("alert alert-%s mb-2 py-2", rnasep_quality$class),
            role = "alert",
            tags$small(
              tags$strong("RNA Preservation: "),
              rnasep_quality$text,
              if (!is.na(delta_rnasep)) {
                tags$span(sprintf(" (ΔCq: %.1f)", delta_rnasep))
              }
            )
          )
        },
        # Double Detection
        tags$div(
          class = sprintf("alert alert-%s mb-2 py-2", double_detection$class),
          role = "alert",
          tags$small(
            tags$strong("Trypanozoon Detection: "),
            double_detection$text
          )
        ),
        # Pipeline Quality Flags
        if (!is.na(pipeline_quality)) {
          tags$div(
            class = "alert alert-warning mb-0 py-2",
            role = "alert",
            tags$small(
              icon("exclamation-triangle"),
              " ",
              tags$strong("Quality Flag: "),
              pipeline_quality
            )
          )
        }
      )
    )
  })

  return(tagList(cards))
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
#' @return HTML cards with clear positivity indication
#' @export
plot_ielisa_results <- function(ielisa_data) {
  if (is.null(ielisa_data) || nrow(ielisa_data) == 0) {
    return(tags$div(
      class = "alert alert-info",
      "No iELISA data available"
    ))
  }

  # Create cards for each test
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

    # Extract OD values
    od_l13 <- if ("OD_L13" %in% names(row) && !is.na(row$OD_L13)) {
      sprintf("%.3f", row$OD_L13)
    } else {
      "N/A"
    }

    od_l15 <- if ("OD_L15" %in% names(row) && !is.na(row$OD_L15)) {
      sprintf("%.3f", row$OD_L15)
    } else {
      "N/A"
    }

    # Plate validity
    plate_valid_l13 <- if ("plate_valid_L13" %in% names(row) && !is.na(row$plate_valid_L13)) {
      row$plate_valid_L13
    } else {
      NA
    }

    plate_valid_l15 <- if ("plate_valid_L15" %in% names(row) && !is.na(row$plate_valid_L15)) {
      row$plate_valid_L15
    } else {
      NA
    }

    # Helper function to create antigen result display
    create_antigen_display <- function(name, positive, inhibition, od, plate_valid) {
      # Determine result badge
      result_badge <- if (!is.na(positive)) {
        if (positive) {
          list(class = "bg-danger", text = "POSITIVE", icon = "check-circle")
        } else {
          list(class = "bg-success", text = "NEGATIVE", icon = "times-circle")
        }
      } else {
        list(class = "bg-secondary", text = "UNKNOWN", icon = "question-circle")
      }

      # Format inhibition percentage
      inhibition_text <- if (!is.na(inhibition)) {
        sprintf("%.1f%%", inhibition)
      } else {
        "N/A"
      }

      # Plate QC badge
      plate_qc_badge <- if (!is.na(plate_valid)) {
        if (plate_valid) {
          tags$span(class = "badge bg-success ms-2", style = "font-size: 0.7em;", "QC: PASS")
        } else {
          tags$span(class = "badge bg-warning ms-2", style = "font-size: 0.7em;", "QC: FAIL")
        }
      } else {
        NULL
      }

      tags$div(
        class = "mb-3 p-3",
        style = "border-left: 4px solid; border-color: var(--bs-gray-300); background-color: var(--bs-light);",
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$h6(class = "mb-0", tags$strong(name)),
          tags$div(
            tags$span(
              class = sprintf("badge %s", result_badge$class),
              icon(result_badge$icon),
              " ",
              result_badge$text
            ),
            plate_qc_badge
          )
        ),
        tags$div(
          class = "row",
          tags$div(
            class = "col-6",
            tags$small(
              tags$strong("Inhibition: "),
              tags$span(
                class = if (!is.na(inhibition) && inhibition >= 30) "text-danger fw-bold" else "text-muted",
                inhibition_text
              )
            )
          ),
          tags$div(
            class = "col-6",
            tags$small(
              tags$strong("OD: "),
              tags$span(class = "text-muted", od)
            )
          )
        )
      )
    }

    tags$div(
      class = "card mb-3",
      tags$div(
        class = "card-header bg-light",
        tags$div(
          class = "d-flex justify-content-between align-items-center",
          tags$strong(sprintf("iELISA Test #%d", i)),
          tags$small(class = "text-muted", paste("Date:", plate_date))
        )
      ),
      tags$div(
        class = "card-body",
        create_antigen_display("LiTat 1.3", litat13_positive, litat13_inhibition, od_l13, plate_valid_l13),
        create_antigen_display("LiTat 1.5", litat15_positive, litat15_inhibition, od_l15, plate_valid_l15)
      )
    )
  })

  return(tagList(cards))
}
