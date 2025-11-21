# R/modules/mod_elisa_runs.R
# ELISA Runs module - Plate-level overview and KPIs
# Modeled after mod_05b_mic_overview.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
  library(plotly)
})

#' ELISA Runs UI
#' @param id Module namespace ID
#' @export
mod_elisa_runs_ui <- function(id) {
  ns <- NS(id)

  tagList(
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
        card_header("PP% Distribution by Plate"),
        card_body(plotlyOutput(ns("plot_pp_by_plate")))
      ),
      card(
        card_header("QC Pass Rate by Plate"),
        card_body(plotlyOutput(ns("plot_qc_by_plate")))
      )
    )
  )
}

#' ELISA Runs Server
#' @param id Module namespace ID
#' @param elisa_data Reactive returning ELISA data frame
#' @export
mod_elisa_runs_server <- function(id, elisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # COMPUTE RUN SUMMARY
    # ========================================================================

    runs_summary <- reactive({
      data <- elisa_data()
      message("DEBUG [mod_elisa_runs]: elisa_data has ", nrow(data), " rows")

      if (!nrow(data)) {
        message("DEBUG [mod_elisa_runs]: Returning empty tibble (no rows)")
        return(tibble())
      }

      # Check required columns exist
      required_cols <- c("plate_id", "plate_number", "plate_date", "sample_type")
      if (!all(required_cols %in% names(data))) {
        message("DEBUG [mod_elisa_runs]: Missing required columns")
        message("DEBUG [mod_elisa_runs]: Required: ", paste(required_cols, collapse = ", "))
        message("DEBUG [mod_elisa_runs]: Available: ", paste(names(data), collapse = ", "))
        return(tibble())
      }

      # Group by plate and summarize
      data %>%
        group_by(plate_id, plate_number, plate_date) %>%
        summarise(
          n_samples = sum(sample_type == "sample", na.rm = TRUE),
          n_controls = sum(sample_type == "control", na.rm = TRUE),
          mean_PC_DOD = if ("PC_DOD" %in% names(.)) mean(PC_DOD, na.rm = TRUE) else NA_real_,
          mean_NC_DOD = if ("NC_DOD" %in% names(.)) mean(NC_DOD, na.rm = TRUE) else NA_real_,
          mean_PP_percent = if ("PP_percent" %in% names(.)) mean(PP_percent, na.rm = TRUE) else NA_real_,
          qc_pass_rate = if ("qc_overall" %in% names(.)) mean(qc_overall, na.rm = TRUE) else NA_real_,
          # Plate control validation fields
          positive_control_od = if ("positive_control_od" %in% names(.)) first(positive_control_od) else NA_real_,
          negative_control_od = if ("negative_control_od" %in% names(.)) first(negative_control_od) else NA_real_,
          plate_positive_control_valid = if ("plate_positive_control_valid" %in% names(.)) first(plate_positive_control_valid) else NA,
          plate_negative_control_valid = if ("plate_negative_control_valid" %in% names(.)) first(plate_negative_control_valid) else NA,
          plate_valid = if ("plate_valid" %in% names(.)) first(plate_valid) else NA,
          .groups = "drop"
        ) %>%
        arrange(desc(plate_date))
    })

    # ========================================================================
    # KPIs
    # ========================================================================

    output$kpis <- renderUI({
      data <- elisa_data()
      runs <- runs_summary()

      total_plates <- nrow(runs)
      total_samples <- if ("sample_type" %in% names(data)) {
        sum(data$sample_type == "sample", na.rm = TRUE)
      } else {
        0
      }

      qc_rate <- if ("qc_overall" %in% names(data)) {
        mean(data$qc_overall, na.rm = TRUE)
      } else {
        NA_real_
      }

      # Count valid/invalid plates
      valid_plates <- if ("plate_valid" %in% names(runs)) {
        sum(runs$plate_valid == TRUE, na.rm = TRUE)
      } else {
        NA
      }

      invalid_plates <- if ("plate_valid" %in% names(runs)) {
        sum(runs$plate_valid == FALSE, na.rm = TRUE)
      } else {
        NA
      }

      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",
        value_box(
          title = "Total Plates",
          value = scales::comma(total_plates),
          showcase = icon("microscope"),
          theme = "primary"
        ),
        value_box(
          title = "Valid Plates",
          value = if (is.na(valid_plates)) "N/A" else scales::comma(valid_plates),
          showcase = icon("circle-check"),
          theme = if (is.na(valid_plates)) "secondary" else "success"
        ),
        value_box(
          title = "Invalid Plates",
          value = if (is.na(invalid_plates)) "N/A" else scales::comma(invalid_plates),
          showcase = icon("circle-xmark"),
          theme = if (is.na(invalid_plates)) "secondary" else if (invalid_plates > 0) "danger" else "success"
        ),
        value_box(
          title = "Sample QC Pass Rate",
          value = if (is.na(qc_rate)) "N/A" else sprintf("%.1f%%", qc_rate * 100),
          showcase = icon("vial-circle-check"),
          theme = if (is.na(qc_rate)) "secondary" else if (qc_rate >= 0.9) "success" else if (qc_rate >= 0.7) "warning" else "danger"
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
          tibble(Message = "No ELISA plates found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format data for display
      display_data <- runs %>%
        mutate(
          plate_date = as.Date(plate_date),
          mean_PC_DOD = round(mean_PC_DOD, 3),
          mean_NC_DOD = round(mean_NC_DOD, 3),
          mean_PP_percent = round(mean_PP_percent, 1),
          qc_pass_rate = round(qc_pass_rate * 100, 1),
          positive_control_od = round(positive_control_od, 3),
          negative_control_od = round(negative_control_od, 3),
          # Format validation status as text
          PC_Valid = case_when(
            is.na(plate_positive_control_valid) ~ "N/A",
            plate_positive_control_valid == TRUE ~ "PASS",
            plate_positive_control_valid == FALSE ~ "FAIL"
          ),
          NC_Valid = case_when(
            is.na(plate_negative_control_valid) ~ "N/A",
            plate_negative_control_valid == TRUE ~ "PASS",
            plate_negative_control_valid == FALSE ~ "FAIL"
          ),
          Plate_Valid = case_when(
            is.na(plate_valid) ~ "N/A",
            plate_valid == TRUE ~ "VALID",
            plate_valid == FALSE ~ "INVALID"
          )
        ) %>%
        select(-plate_positive_control_valid, -plate_negative_control_valid, -plate_valid) %>%
        rename(
          `Plate ID` = plate_id,
          `Plate #` = plate_number,
          `Date` = plate_date,
          `Samples` = n_samples,
          `Controls` = n_controls,
          `Mean PC DOD` = mean_PC_DOD,
          `Mean NC DOD` = mean_NC_DOD,
          `Mean PP%` = mean_PP_percent,
          `QC Pass %` = qc_pass_rate,
          `PC OD` = positive_control_od,
          `NC OD` = negative_control_od,
          `PC QC` = PC_Valid,
          `NC QC` = NC_Valid,
          `Plate Status` = Plate_Valid
        )

      datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        class = "table table-striped table-hover table-sm"
      ) %>%
        formatStyle(
          'QC Pass %',
          backgroundColor = styleInterval(c(70, 90), c('#fee', '#fff3cd', '#d4edda', '#d1ecf1'))
        ) %>%
        formatStyle(
          'PC OD',
          backgroundColor = styleInterval(c(0.5, 1.5), c('#fee', '#d4edda', '#fee'))
        ) %>%
        formatStyle(
          'NC OD',
          backgroundColor = styleEqual(
            levels = c(TRUE, FALSE),
            values = c('#d4edda', '#fee'),
            default = '#fff'
          ),
          target = 'cell',
          backgroundColor = JS(
            "function(value, type) {",
            "  if (type === 'display' && value !== null && !isNaN(value)) {",
            "    return value < 0.5 ? '#d4edda' : '#fee';",
            "  }",
            "  return '#fff';",
            "}"
          )
        ) %>%
        formatStyle(
          'PC QC',
          backgroundColor = styleEqual(
            levels = c('PASS', 'FAIL', 'N/A'),
            values = c('#d4edda', '#fee', '#f8f9fa')
          )
        ) %>%
        formatStyle(
          'NC QC',
          backgroundColor = styleEqual(
            levels = c('PASS', 'FAIL', 'N/A'),
            values = c('#d4edda', '#fee', '#f8f9fa')
          )
        ) %>%
        formatStyle(
          'Plate Status',
          backgroundColor = styleEqual(
            levels = c('VALID', 'INVALID', 'N/A'),
            values = c('#d4edda', '#fee', '#f8f9fa')
          ),
          fontWeight = 'bold'
        )
    })

    # ========================================================================
    # PLOTS
    # ========================================================================

    output$plot_pp_by_plate <- renderPlotly({
      data <- elisa_data()
      if (!nrow(data) || !all(c("plate_number", "PP_percent") %in% names(data))) {
        return(NULL)
      }

      p <- data %>%
        filter(sample_type == "sample") %>%
        ggplot(aes(x = factor(plate_number), y = PP_percent)) +
        geom_boxplot(fill = "#4F46E5", alpha = 0.6, outlier.alpha = 0.5) +
        geom_hline(yintercept = 100, linetype = "dashed", color = "#10B981", linewidth = 0.7) +
        theme_minimal() +
        labs(
          x = "Plate Number",
          y = "PP%",
          title = NULL
        ) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "closest")
    })

    output$plot_qc_by_plate <- renderPlotly({
      runs <- runs_summary()
      if (!nrow(runs) || !"qc_pass_rate" %in% names(runs)) {
        return(NULL)
      }

      p <- runs %>%
        mutate(
          plate_number = factor(plate_number),
          qc_pass_pct = qc_pass_rate * 100
        ) %>%
        ggplot(aes(x = plate_number, y = qc_pass_pct)) +
        geom_col(fill = "#10B981", alpha = 0.7) +
        geom_hline(yintercept = 90, linetype = "dashed", color = "#EF4444", linewidth = 0.7) +
        theme_minimal() +
        labs(
          x = "Plate Number",
          y = "QC Pass Rate (%)",
          title = NULL
        ) +
        ylim(0, 100) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      ggplotly(p, tooltip = c("x", "y")) %>%
        layout(hovermode = "closest")
    })
  })
}
