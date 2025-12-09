# R/modules/mod_ielisa_samples.R
# iELISA Samples module - Sample-level results table only
# Visualizations moved to Analysis module

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
})

#' iELISA Samples UI
#' @param id Module namespace ID
#' @export
mod_ielisa_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "ielisa-panel",

      # Sample results table
      card(
        card_header("Sample Results"),
        card_body(
          DTOutput(ns("samples_table"))
        )
      )
    )
  )
}

#' iELISA Samples Server
#' @param id Module namespace ID
#' @param ielisa_data Reactive returning iELISA data frame
#' @export
mod_ielisa_samples_server <- function(id, ielisa_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # SAMPLES TABLE
    # ========================================================================

    output$samples_table <- renderDT({
      data <- ielisa_data()

      if (!nrow(data)) {
        return(datatable(
          tibble(Message = "No data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Format data for display
      display_data <- data %>%
        mutate(
          OD_L13 = round(OD_L13, 3),
          OD_L15 = round(OD_L15, 3),
          pct_inh_f1_13 = round(pct_inh_f1_13, 1),
          pct_inh_f1_15 = round(pct_inh_f1_15, 1),
          pct_inh_f2_13 = round(pct_inh_f2_13, 1),
          pct_inh_f2_15 = round(pct_inh_f2_15, 1),
          diff_f1_f2_13 = round(diff_f1_f2_13, 1),
          diff_f1_f2_15 = round(diff_f1_f2_15, 1),
          # Format positivity status
          L13_Result = ifelse(positive_L13, "✓ POS", "✗ NEG"),
          L15_Result = ifelse(positive_L15, "✓ POS", "✗ NEG"),
          # Positivity indicator
          Positive_For = case_when(
            positive_L13 & positive_L15 ~ "Both",
            positive_L13 ~ "LiTat 1.3",
            positive_L15 ~ "LiTat 1.5",
            TRUE ~ "Neither"
          ),
          # Consolidated status with discordance indicator
          ConsolidatedStatus = if ("ielisa_status_final" %in% names(.) && "ielisa_is_discordant" %in% names(.)) {
            dplyr::case_when(
              ielisa_is_discordant == TRUE ~ paste0(ielisa_status_final, " \u26A0\uFE0F"),
              ielisa_is_retest == TRUE ~ paste0(ielisa_status_final, " (", ielisa_n_tests, "x)"),
              TRUE ~ ielisa_status_final
            )
          } else {
            NA_character_
          },
          # Discordance flag
          DiscordanceFlag = if ("ielisa_is_discordant" %in% names(.)) {
            dplyr::case_when(
              ielisa_is_discordant == TRUE ~ "Discordant",
              ielisa_is_retest == TRUE ~ "Retested",
              TRUE ~ "Single"
            )
          } else {
            NA_character_
          },
          # Format duplicate flags
          Dup_Flag = case_when(
            duplicate_across_files ~ "🔄 DUP",
            barcode_conflict ~ "⚠️ BC",
            labid_conflict ~ "⚠️ LID",
            TRUE ~ ""
          )
        )

      # Select columns based on what's available
      base_cols <- c("file", "LabID", "Barcode", "Positive_For",
                     "ConsolidatedStatus", "DiscordanceFlag",
                     "OD_L13", "pct_inh_f1_13", "pct_inh_f2_13", "L13_Result",
                     "OD_L15", "pct_inh_f1_15", "pct_inh_f2_15", "L15_Result",
                     "diff_f1_f2_13", "diff_f1_f2_15", "Dup_Flag")

      available_cols <- base_cols[base_cols %in% names(display_data)]

      display_data <- display_data %>%
        select(all_of(available_cols))

      # Rename columns for display
      names(display_data) <- gsub("file", "File", names(display_data))
      names(display_data) <- gsub("Positive_For", "Positive For", names(display_data))
      names(display_data) <- gsub("ConsolidatedStatus", "Status", names(display_data))
      names(display_data) <- gsub("DiscordanceFlag", "Retest", names(display_data))
      names(display_data) <- gsub("OD_L13", "OD L13", names(display_data))
      names(display_data) <- gsub("OD_L15", "OD L15", names(display_data))
      names(display_data) <- gsub("pct_inh_f1_13", "Inh% L13 (F1)", names(display_data))
      names(display_data) <- gsub("pct_inh_f2_13", "Inh% L13 (F2)", names(display_data))
      names(display_data) <- gsub("pct_inh_f1_15", "Inh% L15 (F1)", names(display_data))
      names(display_data) <- gsub("pct_inh_f2_15", "Inh% L15 (F2)", names(display_data))
      names(display_data) <- gsub("L13_Result", "L13 Result", names(display_data))
      names(display_data) <- gsub("L15_Result", "L15 Result", names(display_data))
      names(display_data) <- gsub("diff_f1_f2_13", "Δ F1-F2 L13", names(display_data))
      names(display_data) <- gsub("diff_f1_f2_15", "Δ F1-F2 L15", names(display_data))
      names(display_data) <- gsub("Dup_Flag", "Flags", names(display_data))

      dt <- datatable(
        display_data,
        filter = 'top',
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'Positive For',
          backgroundColor = styleEqual(
            c("Both", "LiTat 1.3", "LiTat 1.5", "Neither"),
            c("#d4edda", "#cfe2ff", "#cfe2ff", "#f8d7da")
          ),
          fontWeight = 'bold'
        ) %>%
        formatStyle(
          'L13 Result',
          backgroundColor = styleEqual(c("✓ POS", "✗ NEG"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'L15 Result',
          backgroundColor = styleEqual(c("✓ POS", "✗ NEG"), c("#d4edda", "#f8d7da"))
        ) %>%
        formatStyle(
          'Flags',
          backgroundColor = styleInterval(c(0.5), c("white", "#fff3cd"))
        )

      # Add discordance styling if columns exist
      if ("Retest" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle(
            'Retest',
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

      if ("Status" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle(
            'Status',
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
