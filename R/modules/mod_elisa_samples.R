# R/modules/mod_elisa_samples.R
# ELISA Samples module - Sample-level details with filtering
# Modeled after mod_05c_mic_samples.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
})

#' ELISA Samples UI
#' @param id Module namespace ID
#' @export
mod_elisa_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "elisa-panel",
      # Summary info
      uiOutput(ns("summary_info")),

      # Spacer
      tags$div(style = "height: 16px;"),

      # Samples table
      card(
        card_header("Sample Results"),
        card_body(
          DTOutput(ns("samples_table"))
        )
      )
    )
  )
}

#' ELISA Samples Server
#' @param id Module namespace ID
#' @param elisa_data Reactive returning ELISA sample data
#' @export
mod_elisa_samples_server <- function(id, elisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # ========================================================================
    # SUMMARY INFO
    # ========================================================================

    output$summary_info <- renderUI({
      data <- elisa_data()

      n_total <- nrow(data)
      n_matched <- if ("BiobankMatched" %in% names(data)) {
        sum(data$BiobankMatched, na.rm = TRUE)
      } else {
        0
      }
      match_pct <- if (n_total > 0) round(100 * n_matched / n_total, 1) else 0

      n_qc_pass <- if ("qc_overall" %in% names(data)) {
        sum(data$qc_overall, na.rm = TRUE)
      } else {
        0
      }
      qc_pct <- if (n_total > 0) round(100 * n_qc_pass / n_total, 1) else 0

      layout_column_wrap(
        width = 1/3,
        heights_equal = "row",
        value_box(
          title = "Samples",
          value = scales::comma(n_total),
          showcase = icon("vial"),
          theme = "secondary"
        ),
        value_box(
          title = "Biobank Match",
          value = sprintf("%d (%.1f%%)", n_matched, match_pct),
          showcase = icon("link"),
          theme = if (match_pct >= 80) "success" else if (match_pct >= 50) "warning" else "danger"
        ),
        value_box(
          title = "QC Pass",
          value = sprintf("%d (%.1f%%)", n_qc_pass, qc_pct),
          showcase = icon("check"),
          theme = if (qc_pct >= 90) "success" else if (qc_pct >= 70) "warning" else "danger"
        )
      )
    })

    # ========================================================================
    # SAMPLES TABLE
    # ========================================================================

    output$samples_table <- renderDT({
      data <- elisa_data()

      if (!nrow(data)) {
        return(datatable(
          tibble(),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Select columns to display (only those that exist)
      available_cols <- c(
        "plate_number", "plate_date", "sample", "numero_labo", "code_barres_kps",
        "DOD", "PP_percent", "cv_Ag_plus", "cv_Ag0", "qc_overall",
        "Province", "HealthZone", "Structure", "Sex", "AgeGroup", "BiobankMatched"
      )

      cols_to_select <- available_cols[available_cols %in% names(data)]

      display_data <- data %>%
        select(all_of(cols_to_select)) %>%
        mutate(
          plate_date = if ("plate_date" %in% names(.)) as.Date(plate_date) else NA,
          DOD = if ("DOD" %in% names(.)) round(DOD, 3) else NA,
          PP_percent = if ("PP_percent" %in% names(.)) round(PP_percent, 1) else NA,
          cv_Ag_plus = if ("cv_Ag_plus" %in% names(.)) round(cv_Ag_plus, 1) else NA,
          cv_Ag0 = if ("cv_Ag0" %in% names(.)) round(cv_Ag0, 1) else NA,
          qc_overall = if ("qc_overall" %in% names(.)) ifelse(qc_overall, "Pass", "Fail") else NA
        )

      # Rename columns for better display
      col_names <- names(display_data)
      col_names <- gsub("_", " ", col_names)
      col_names <- tools::toTitleCase(col_names)
      names(display_data) <- col_names

      datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          scrollCollapse = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        filter = 'top',
        class = "table table-striped table-hover table-sm"
      ) %>%
        formatStyle(
          columns = which(names(display_data) == "Qc Overall"),
          backgroundColor = styleEqual(c("Pass", "Fail"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          columns = which(names(display_data) == "Biobankmatched"),
          backgroundColor = styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da'))
        )
    })
  })
}
