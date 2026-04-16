# R/modules/mod_results_export.R
# Lab Results Export Module
# ============================================================================
# Generates a formatted Excel file assembling biobank demographics with test
# results from all assay modules (MIC, ELISA-PE, ELISA-VSG, iELISA).
# Mirrors the layout of the manual "Registre résultats analyses KPS" file.
# ============================================================================

mod_results_export_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Results Export",
    icon  = icon("file-excel"),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5(icon("download"), " Export Settings"),
        hr(),
        checkboxGroupInput(
          ns("tests_to_include"),
          "Tests to include:",
          choices  = c(
            "Biologie Moléculaire" = "mic",
            "ELISA indirect PE"    = "elisa_pe",
            "ELISA indirect VSG"   = "elisa_vsg",
            "iELISA"               = "ielisa"
          ),
          selected = "mic"
        ),
        hr(),
        radioButtons(
          ns("mic_cq_format"),
          "MIC Cq display:",
          choices = c(
            "Mean +/- SD" = "mean_sd",
            "Median only" = "median"
          ),
          selected = "mean_sd"
        ),
        hr(),
        downloadButton(ns("download_xlsx"), "Download Excel",
                       class = "btn-success w-100")
      ),
      # Main panel: preview table
      card(
        card_header("Preview"),
        card_body(
          div(
            class = "mb-2",
            textOutput(ns("summary_text"))
          ),
          div(style = "overflow-x: auto;",
              DT::dataTableOutput(ns("preview_table"))
          )
        )
      )
    )
  )
}


mod_results_export_server <- function(id,
                                      biobank_df,
                                      mic_df      = NULL,
                                      elisa_pe_df = NULL,
                                      elisa_vsg_df = NULL,
                                      ielisa_df   = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # HELPER: format Cq value as "mean +/- sd" with comma decimal (FR locale)
    # ========================================================================
    fmt_cq <- function(mean_val, sd_val, use_mean_sd = TRUE) {
      if (is.na(mean_val)) return(NA_character_)
      if (!use_mean_sd) {
        return(gsub("\\.", ",", formatC(mean_val, format = "f", digits = 1)))
      }
      m <- gsub("\\.", ",", formatC(round(mean_val, 1), format = "f", digits = 1))
      s <- if (is.na(sd_val) || sd_val == 0) {
        "0"
      } else {
        gsub("\\.", ",", formatC(round(sd_val, 1), format = "f", digits = 1))
      }
      paste0(m, " +/- ", s)
    }

    fmt_cq_vec <- function(mean_vec, sd_vec, use_mean_sd = TRUE) {
      mapply(fmt_cq, mean_vec, sd_vec,
             MoreArgs = list(use_mean_sd = use_mean_sd),
             USE.NAMES = FALSE)
    }

    # ========================================================================
    # MAP MIC FinalCall to short result code
    # ========================================================================
    mic_result_code <- function(call) {
      dplyr::case_when(
        grepl("Positive|Detected|Trypanozoon", call, ignore.case = TRUE) ~ "P",
        grepl("Negative", call, ignore.case = TRUE) ~ "N",
        grepl("Invalid|RunInvalid|Failed", call, ignore.case = TRUE) ~ "F",
        grepl("Indeterminate|Inconclusive|Borderline", call, ignore.case = TRUE) ~ "I",
        is.na(call) ~ NA_character_,
        TRUE ~ call
      )
    }

    # ========================================================================
    # ASSEMBLE EXPORT DATA
    # ========================================================================
    export_data <- reactive({
      bb <- biobank_df()
      req(bb, nrow(bb) > 0)

      # ----- Biobank base columns -------------------------------------------
      base <- bb %>%
        dplyr::transmute(
          `Numéro labo`  = lab_id,
          Etude          = study,
          `Structure sanitaire` = health_facility,
          `Zone de santé`       = health_zone,
          Province              = province,
          `Responsable`         = responsible_person,
          Contact               = contact,
          `Code-barres KPS`     = barcode,
          `Date envoi INRB`     = if ("date_sent_inrb" %in% names(bb)) {
            suppressWarnings(as.Date(date_sent_inrb))
          } else {
            as.Date(NA)
          },
          .lab_id_norm = tolower(trimws(as.character(lab_id))),
          .barcode_norm = {
            b <- tolower(trimws(as.character(barcode)))
            sub("^kps[-_]?", "", b)
          }
        )

      # ----- MIC (Biologie Moléculaire) ------------------------------------
      if ("mic" %in% input$tests_to_include && !is.null(mic_df)) {
        mic <- tryCatch(mic_df(), error = function(e) NULL)
        if (!is.null(mic) && nrow(mic) > 0) {
          use_mean_sd <- identical(input$mic_cq_format, "mean_sd")

          # Use consolidation columns if available, else FinalCall
          status_col <- if ("mic_status_final" %in% names(mic)) "mic_status_final" else "FinalCall"
          mic_nms <- names(mic)

          # Ensure columns exist (add NAs for missing ones)
          safe_col <- function(df, col, default = NA_character_) {
            if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
          }

          # Prepare columns before grouping
          mic_prep <- mic %>%
            dplyr::mutate(
              .mic_date_raw = dplyr::coalesce(
                if ("RunDate" %in% mic_nms) suppressWarnings(as.character(RunDate)) else NA_character_,
                if ("RunDateTime" %in% mic_nms) suppressWarnings(as.character(RunDateTime)) else NA_character_
              ),
              .mic_barcode  = safe_col(mic, "LinkedBarcode"),
              .mic_numero   = safe_col(mic, "LinkedNumero"),
              .mic_status   = .data[[status_col]],
              .cq_mean_177T = safe_col(mic, "Cq_mean_177T", NA_real_),
              .cq_sd_177T   = safe_col(mic, "Cq_sd_177T", NA_real_),
              .cq_mean_18S2 = safe_col(mic, "Cq_mean_18S2", NA_real_),
              .cq_sd_18S2   = safe_col(mic, "Cq_sd_18S2", NA_real_),
              .cq_mean_RP_DNA = safe_col(mic, "Cq_mean_RNAseP_DNA", NA_real_),
              .cq_sd_RP_DNA   = safe_col(mic, "Cq_sd_RNAseP_DNA", NA_real_),
              .cq_mean_RP_RNA = safe_col(mic, "Cq_mean_RNAseP_RNA", NA_real_),
              .cq_sd_RP_RNA   = safe_col(mic, "Cq_sd_RNAseP_RNA", NA_real_)
            )

          # Build one-row-per-sample MIC summary
          mic_summary <- mic_prep %>%
            dplyr::group_by(SampleID) %>%
            dplyr::summarise(
              mic_run_date  = suppressWarnings(as.Date(dplyr::first(.mic_date_raw))),
              mic_barcode   = dplyr::first(.mic_barcode),
              mic_numero    = dplyr::first(.mic_numero),
              mic_result    = mic_result_code(dplyr::first(.mic_status)),
              mic_177T      = fmt_cq(dplyr::first(.cq_mean_177T),
                                     dplyr::first(.cq_sd_177T), use_mean_sd),
              mic_18S2      = fmt_cq(dplyr::first(.cq_mean_18S2),
                                     dplyr::first(.cq_sd_18S2), use_mean_sd),
              mic_RNAseP_DNA = fmt_cq(dplyr::first(.cq_mean_RP_DNA),
                                      dplyr::first(.cq_sd_RP_DNA), use_mean_sd),
              mic_RNAseP_RNA = fmt_cq(dplyr::first(.cq_mean_RP_RNA),
                                      dplyr::first(.cq_sd_RP_RNA), use_mean_sd),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              .mic_lab_norm = tolower(trimws(as.character(SampleID)))
            )

          # Join MIC to base by lab_id
          base <- base %>%
            dplyr::left_join(
              mic_summary %>% dplyr::select(-SampleID),
              by = c(".lab_id_norm" = ".mic_lab_norm")
            )
        }
      }

      # Ensure MIC columns exist even if no data
      mic_cols <- c("mic_run_date", "mic_numero", "mic_barcode",
                    "mic_result", "mic_177T", "mic_18S2",
                    "mic_RNAseP_DNA", "mic_RNAseP_RNA")
      for (col in mic_cols) {
        if (!col %in% names(base)) base[[col]] <- NA
      }

      # ----- Build final output with nice column names ----------------------
      out <- base %>%
        dplyr::transmute(
          `Numéro labo`        = `Numéro labo`,
          Etude                = Etude,
          `Structure sanitaire` = `Structure sanitaire`,
          `Zone de santé`      = `Zone de santé`,
          Province             = Province,
          Responsable          = `Responsable`,
          Contact              = Contact,
          `Code-barres KPS`    = `Code-barres KPS`,
          `Date envoi INRB`    = `Date envoi INRB`,
          # Biologie moléculaire
          `Date analyse (Mol)`    = mic_run_date,
          `Numéro labo (Mol)`     = mic_numero,
          `Code-barres KPS (Mol)` = mic_barcode,
          `Résultat final (Mol)`  = mic_result,
          `177T`                  = mic_177T,
          `18S2`                  = mic_18S2,
          `RNAseP-DNA`            = mic_RNAseP_DNA,
          `RNAseP-RNA`            = mic_RNAseP_RNA
        )

      out
    })

    # ========================================================================
    # PREVIEW TABLE
    # ========================================================================
    output$summary_text <- renderText({
      df <- export_data()
      n_total <- nrow(df)
      n_mic   <- sum(!is.na(df$`Résultat final (Mol)`))
      paste0(n_total, " samples total | ",
             n_mic, " with MIC results")
    })

    output$preview_table <- DT::renderDataTable({
      df <- export_data()

      DT::datatable(
        df,
        options = list(
          scrollX   = TRUE,
          pageLength = 25,
          dom = "frtip"
        ),
        rownames = FALSE,
        class = "compact stripe hover"
      )
    })

    # ========================================================================
    # EXCEL DOWNLOAD
    # ========================================================================
    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0(format(Sys.Date(), "%y%m%d"),
               " Registre résultats analyses KPS.xlsx")
      },
      content = function(file) {
        df <- export_data()

        wb <- openxlsx::createWorkbook()
        sheet_name <- "Résultats analyses KPS"
        openxlsx::addWorksheet(wb, sheet_name)

        # ---- Styles --------------------------------------------------------
        header_style <- openxlsx::createStyle(
          textDecoration = "bold",
          halign = "center",
          valign = "center",
          wrapText = TRUE,
          border = "TopBottomLeftRight",
          fgFill = "#D9E1F2",
          fontSize = 10
        )
        group_style <- openxlsx::createStyle(
          textDecoration = "bold",
          halign = "center",
          valign = "center",
          fontSize = 11,
          fgFill = "#B4C6E7",
          border = "TopBottomLeftRight"
        )
        data_style <- openxlsx::createStyle(
          halign = "center",
          valign = "center",
          border = "TopBottomLeftRight",
          fontSize = 10
        )
        date_style <- openxlsx::createStyle(
          halign = "center",
          valign = "center",
          border = "TopBottomLeftRight",
          fontSize = 10,
          numFmt = "DD/MM/YYYY"
        )

        # ---- Row 1: Group headers ------------------------------------------
        # Biobank columns: 1-9 (no group header needed)
        # MIC group: columns 10-18
        mic_start <- 10
        mic_end   <- 18
        openxlsx::mergeCells(wb, sheet_name, cols = mic_start:mic_end, rows = 1)
        openxlsx::writeData(wb, sheet_name, "Biologie moléculaire",
                            startCol = mic_start, startRow = 1)
        openxlsx::addStyle(wb, sheet_name, group_style,
                           rows = 1, cols = mic_start:mic_end, gridExpand = TRUE)

        # ---- Row 2: Column headers -----------------------------------------
        col_names <- names(df)
        # Write display names (strip the parenthetical suffixes for the MIC section)
        display_names <- col_names
        display_names[display_names == "Date analyse (Mol)"]    <- "Date analyse"
        display_names[display_names == "Numéro labo (Mol)"]     <- "Numéro labo"
        display_names[display_names == "Code-barres KPS (Mol)"] <- "Code-barres KPS"
        display_names[display_names == "Résultat final (Mol)"]  <- "Résultat final"

        for (i in seq_along(display_names)) {
          openxlsx::writeData(wb, sheet_name, display_names[i],
                              startCol = i, startRow = 2)
        }
        openxlsx::addStyle(wb, sheet_name, header_style,
                           rows = 2, cols = seq_along(col_names), gridExpand = TRUE)

        # ---- Row 3+: Data --------------------------------------------------
        if (nrow(df) > 0) {
          openxlsx::writeData(wb, sheet_name, df,
                              startCol = 1, startRow = 3,
                              colNames = FALSE)

          n_data <- nrow(df)
          data_rows <- 3:(2 + n_data)

          # Apply general data style
          openxlsx::addStyle(wb, sheet_name, data_style,
                             rows = data_rows,
                             cols = seq_along(col_names),
                             gridExpand = TRUE)

          # Apply date formatting to date columns
          date_cols <- which(col_names %in% c("Date envoi INRB", "Date analyse (Mol)"))
          if (length(date_cols)) {
            openxlsx::addStyle(wb, sheet_name, date_style,
                               rows = data_rows, cols = date_cols,
                               gridExpand = TRUE)
          }
        }

        # ---- Column widths -------------------------------------------------
        col_widths <- c(
          12,  # Numéro labo
          8,   # Etude
          30,  # Structure sanitaire
          18,  # Zone de santé
          18,  # Province
          25,  # Responsable
          15,  # Contact
          16,  # Code-barres KPS
          16,  # Date envoi INRB
          15,  # Date analyse (Mol)
          12,  # Numéro labo (Mol)
          16,  # Code-barres KPS (Mol)
          14,  # Résultat final (Mol)
          16,  # 177T
          16,  # 18S2
          16,  # RNAseP-DNA
          16   # RNAseP-RNA
        )
        for (i in seq_along(col_widths)) {
          openxlsx::setColWidths(wb, sheet_name, cols = i, widths = col_widths[i])
        }

        # Freeze panes: freeze first 2 rows and first 9 columns
        openxlsx::freezePane(wb, sheet_name, firstActiveRow = 3, firstActiveCol = 10)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
