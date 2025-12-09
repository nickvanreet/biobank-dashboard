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

      # Calculate unique samples and screening stats (only for samples, not controls)
      samples_only <- data %>% filter(sample_type == "sample")
      n_unique <- samples_only %>%
        mutate(sample_id = coalesce(code_barres_kps, numero_labo)) %>%
        filter(!is.na(sample_id)) %>%
        distinct(sample_id) %>%
        nrow()

      n_first <- if ("is_first_screening" %in% names(samples_only)) {
        sum(samples_only$is_first_screening, na.rm = TRUE)
      } else {
        0
      }

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

      n_positive <- if ("sample_positive" %in% names(data)) {
        sum(data$sample_positive, na.rm = TRUE)
      } else {
        0
      }
      pos_pct <- if (n_total > 0) round(100 * n_positive / n_total, 1) else 0

      layout_column_wrap(
        width = 1/5,
        heights_equal = "row",
        value_box(
          title = "Total Tests",
          value = scales::comma(n_total),
          showcase = icon("vial"),
          theme = "secondary"
        ),
        value_box(
          title = "Unique Samples",
          value = scales::comma(n_unique),
          showcase = icon("fingerprint"),
          theme = "info"
        ),
        value_box(
          title = "First Screenings",
          value = scales::comma(n_first),
          showcase = icon("1-circle"),
          theme = "success"
        ),
        value_box(
          title = "Positive Samples",
          value = sprintf("%d (%.1f%%)", n_positive, pos_pct),
          showcase = icon("plus-circle"),
          theme = "primary"
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

      # Detect ELISA type from column names to get the right prefix
      elisa_prefix <- if (any(grepl("elisa_vsg_", names(data)))) "vsg" else "pe"
      status_final_col <- paste0("elisa_", elisa_prefix, "_status_final")
      is_discordant_col <- paste0("elisa_", elisa_prefix, "_is_discordant")
      is_retest_col <- paste0("elisa_", elisa_prefix, "_is_retest")
      n_tests_col <- paste0("elisa_", elisa_prefix, "_n_tests")
      confidence_col <- paste0("elisa_", elisa_prefix, "_confidence")

      # Add discordance indicator column for display
      if (status_final_col %in% names(data) && is_discordant_col %in% names(data)) {
        data <- data %>%
          mutate(
            ConsolidatedStatus = dplyr::case_when(
              !!sym(is_discordant_col) == TRUE ~ paste0(!!sym(status_final_col), " \u26A0\uFE0F"),
              !!sym(is_retest_col) == TRUE ~ paste0(!!sym(status_final_col), " (", !!sym(n_tests_col), "x)"),
              TRUE ~ !!sym(status_final_col)
            ),
            DiscordanceFlag = dplyr::case_when(
              !!sym(is_discordant_col) == TRUE ~ "Discordant",
              !!sym(is_retest_col) == TRUE ~ "Retested",
              TRUE ~ "Single"
            )
          )
      }

      # Select columns to display (only those that exist)
      # Note: "sample" is the PLATE POSITION (e.g., S1, S2, PC1), not the actual sample ID
      # Actual sample IDs are in "numero_labo" (lab ID) or "code_barres_kps" (barcode)
      # Wells are aggregated across replicates, so well_id is not preserved
      available_cols <- c(
        "screening_num", "plate_number", "plate_date", "numero_labo", "code_barres_kps", "sample",
        "DOD", "PP_percent", "sample_positive", "ConsolidatedStatus", "DiscordanceFlag",
        confidence_col, "cv_Ag_plus", "cv_Ag0", "qc_overall",
        "Province", "HealthZone", "Structure", "Sex", "AgeGroup", "BiobankMatched"
      )

      cols_to_select <- available_cols[available_cols %in% names(data)]

      display_data <- data %>%
        select(all_of(cols_to_select)) %>%
        mutate(
          plate_date = if ("plate_date" %in% names(.)) as.Date(plate_date) else NA,
          DOD = if ("DOD" %in% names(.)) round(DOD, 3) else NA,
          PP_percent = if ("PP_percent" %in% names(.)) round(PP_percent, 1) else NA,
          sample_positive = if ("sample_positive" %in% names(.)) ifelse(sample_positive, "Positive", "Negative") else NA,
          cv_Ag_plus = if ("cv_Ag_plus" %in% names(.)) round(cv_Ag_plus, 1) else NA,
          cv_Ag0 = if ("cv_Ag0" %in% names(.)) round(cv_Ag0, 1) else NA,
          qc_overall = if ("qc_overall" %in% names(.)) ifelse(qc_overall, "Pass", "Fail") else NA
        )

      # Rename columns for better display
      col_names <- names(display_data)
      col_names <- gsub("_", " ", col_names)
      col_names <- tools::toTitleCase(col_names)
      # Clarify specific columns
      col_names[col_names == "Sample"] <- "Plate Position"
      col_names[col_names == "Numero Labo"] <- "Sample ID (Numero)"
      col_names[col_names == "Code Barres Kps"] <- "Sample ID (Barcode)"
      names(display_data) <- col_names

      dt <- datatable(
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
        ) %>%
        formatStyle(
          columns = which(names(display_data) == "Sample Positive"),
          backgroundColor = styleEqual(c("Positive", "Negative"), c('#cfe2ff', '#f8f9fa')),
          fontWeight = styleEqual(c("Positive", "Negative"), c('bold', 'normal'))
        )

      # Add discordance styling if columns exist
      if ("Discordanceflag" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle(
            columns = which(names(display_data) == "Discordanceflag"),
            backgroundColor = styleEqual(
              c('Discordant', 'Retested', 'Single'),
              c('#f8d7da', '#fff3cd', 'transparent')
            ),
            fontWeight = styleEqual(
              c('Discordant', 'Retested', 'Single'),
              c('bold', 'normal', 'normal')
            )
          )
      }

      if ("Consolidatedstatus" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle(
            columns = which(names(display_data) == "Consolidatedstatus"),
            backgroundColor = styleEqual(
              c('Positive', 'Negative', 'Borderline', 'Invalid'),
              c('#d4edda', '#f8f9fa', '#fff3cd', '#f8d7da')
            )
          )
      }

      dt
    })
  })
}
