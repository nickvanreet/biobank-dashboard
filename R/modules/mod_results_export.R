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
        checkboxInput(
          ns("show_all_runs"),
          "Show all runs (retested samples)",
          value = FALSE
        ),
        tags$small(class = "text-muted d-block mb-2",
                   "When checked: one row per run. When unchecked: consolidated (latest run)."),
        hr(),
        h6("RNA interpretation thresholds"),
        numericInput(
          ns("tryp_rna_delta"),
          "Tryp RNA delta (|18S2 - 177T|):",
          value = 5, min = 0, max = 20, step = 0.5
        ),
        tags$small(class = "text-muted d-block mb-2",
                   "< threshold = High trypanosome RNA content"),
        numericInput(
          ns("human_rna_delta"),
          "Human RNA delta (|RP-RNA - RP-DNA|):",
          value = 8, min = 0, max = 20, step = 0.5
        ),
        tags$small(class = "text-muted d-block mb-2",
                   "< threshold = Excellent RNA quality"),
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
    #         Returns "-" when target was not detected (NA) on a tested sample
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

    # Replace NA with "-" for Cq columns on tested samples
    # (tested = has a result code; "-" = not detected; blank = not tested)
    dash_na <- function(cq_str, has_result) {
      dplyr::if_else(!is.na(has_result) & is.na(cq_str), "\u2013", cq_str)
    }

    # ========================================================================
    # MAP MIC FinalCall -> short result code (P/S/R/N/T/F)
    # ========================================================================
    mic_result_code <- function(call) {
      dplyr::case_when(
        grepl("Positive|Detected|Trypanozoon", call, ignore.case = TRUE) ~ "P",
        grepl("^Negative$", call, ignore.case = TRUE) ~ "N",
        grepl("Invalid|RunInvalid|Failed", call, ignore.case = TRUE) ~ "F",
        grepl("Indeterminate|Inconclusive|Borderline|Suspect", call, ignore.case = TRUE) ~ "S",
        is.na(call) ~ NA_character_,
        TRUE ~ call
      )
    }

    # ========================================================================
    # RNA INTERPRETATION (from qPCR template Decision/Settings logic)
    # ========================================================================

    classify_tryp_rna <- function(cq_177T, cq_18S2, threshold) {
      dplyr::case_when(
        is.na(cq_177T) & is.na(cq_18S2) ~ NA_character_,
        is.na(cq_177T) & !is.na(cq_18S2) ~ "RNA only",
        !is.na(cq_177T) & is.na(cq_18S2) ~ "DNA only",
        abs(cq_18S2 - cq_177T) < threshold ~ "High",
        TRUE ~ "Low"
      )
    }

    classify_human_rna <- function(cq_rp_dna, cq_rp_rna, threshold) {
      dplyr::case_when(
        is.na(cq_rp_dna) & is.na(cq_rp_rna)  ~ NA_character_,
        is.na(cq_rp_dna)                       ~ "Failed",
        is.na(cq_rp_rna)                       ~ "No RNA",
        abs(cq_rp_rna - cq_rp_dna) < threshold ~ "Excellent",
        TRUE ~ "Degraded"
      )
    }

    mic_auto_remark <- function(result, tryp_rna, human_rna,
                                cq_rp_dna, cq_rp_rna,
                                cq_177T, cq_18S2) {
      dplyr::case_when(
        is.na(result) ~ NA_character_,
        # Failed
        result == "F" & is.na(cq_rp_dna) ~
          "extraction fail: no human DNA detected",
        result == "F" ~
          "insufficient blood quantity or RNA control failed",
        # Negative — but check for sub-threshold signal
        result == "N" & !is.na(cq_177T) & !is.na(cq_18S2) & human_rna == "Degraded" ~
          "weak 177T + 18S2 signal below threshold; degraded RNA",
        result == "N" & !is.na(cq_177T) & !is.na(cq_18S2) ~
          "weak 177T + 18S2 signal below threshold",
        result == "N" & !is.na(cq_177T) & human_rna == "Degraded" ~
          "weak 177T signal below threshold; degraded RNA",
        result == "N" & !is.na(cq_177T) ~
          "weak 177T signal below threshold",
        result == "N" & !is.na(cq_18S2) & human_rna == "Degraded" ~
          "weak 18S2 signal below threshold; degraded RNA",
        result == "N" & !is.na(cq_18S2) ~
          "weak 18S2 signal below threshold",
        result == "N" & human_rna == "Degraded" ~
          "no trypanosome DNA or RNA detected; degraded RNA",
        result == "N" & human_rna == "No RNA" ~
          "no trypanosome DNA or RNA detected; RNA control failed",
        result == "N" ~
          "no trypanosome DNA or RNA detected",
        # Positive — but latest retest may be negative (discordant)
        result == "P" & is.na(cq_177T) & is.na(cq_18S2) ~
          "positive in earlier run; latest retest negative",
        result == "P" & tryp_rna == "High" ~
          "trypanosome DNA and RNA detected",
        result == "P" & tryp_rna == "DNA only" ~
          "trypanosome DNA detected (no RNA)",
        result == "P" & tryp_rna == "RNA only" ~
          "trypanosome RNA detected (no DNA)",
        result == "P" ~
          "trypanosome DNA detected",
        # Suspect
        result == "S" ~
          "suspect: weak signal, repeat recommended",
        TRUE ~ NA_character_
      )
    }

    # ========================================================================
    # ASSEMBLE EXPORT DATA
    # ========================================================================
    export_data <- reactive({
      bb <- biobank_df()
      req(bb, nrow(bb) > 0)

      tryp_delta  <- input$tryp_rna_delta  %||% 5
      human_delta <- input$human_rna_delta %||% 8
      all_runs    <- isTRUE(input$show_all_runs)

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
          },
          .bb_remarks = if ("remarks" %in% names(bb)) {
            as.character(remarks)
          } else {
            NA_character_
          }
        )

      # ----- MIC (Biologie Moléculaire) ------------------------------------
      if ("mic" %in% input$tests_to_include && !is.null(mic_df)) {
        mic <- tryCatch(mic_df(), error = function(e) NULL)
        if (!is.null(mic) && nrow(mic) > 0) {
          use_mean_sd <- identical(input$mic_cq_format, "mean_sd")

          # For consolidated view use mic_status_final; for all-runs use FinalCall
          status_col_consolidated <- if ("mic_status_final" %in% names(mic)) "mic_status_final" else "FinalCall"
          status_col_per_run <- "FinalCall"
          mic_nms <- names(mic)

          safe_col <- function(df, col, default = NA_character_) {
            if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
          }

          mic_prep <- mic %>%
            dplyr::mutate(
              .mic_date_raw = dplyr::coalesce(
                if ("RunDate" %in% mic_nms) suppressWarnings(as.character(RunDate)) else NA_character_,
                if ("RunDateTime" %in% mic_nms) suppressWarnings(as.character(RunDateTime)) else NA_character_
              ),
              .mic_date_parsed = suppressWarnings(as.Date(.mic_date_raw)),
              .mic_barcode    = safe_col(mic, "LinkedBarcode"),
              .mic_numero     = safe_col(mic, "LinkedNumero"),
              .mic_run_id     = safe_col(mic, "RunID"),
              .cq_mean_177T   = safe_col(mic, "Cq_mean_177T", NA_real_),
              .cq_sd_177T     = safe_col(mic, "Cq_sd_177T", NA_real_),
              .cq_mean_18S2   = safe_col(mic, "Cq_mean_18S2", NA_real_),
              .cq_sd_18S2     = safe_col(mic, "Cq_sd_18S2", NA_real_),
              .cq_mean_RP_DNA = safe_col(mic, "Cq_mean_RNAseP_DNA", NA_real_),
              .cq_sd_RP_DNA   = safe_col(mic, "Cq_sd_RNAseP_DNA", NA_real_),
              .cq_mean_RP_RNA = safe_col(mic, "Cq_mean_RNAseP_RNA", NA_real_),
              .cq_sd_RP_RNA   = safe_col(mic, "Cq_sd_RNAseP_RNA", NA_real_),
              # Consolidated status for one-row-per-sample mode
              .status_consolidated = .data[[status_col_consolidated]],
              # Per-run status for all-runs mode
              .status_per_run      = .data[[status_col_per_run]],
              # Retest info
              .mic_n_tests = safe_col(mic, "mic_n_tests", NA_integer_),
              .mic_is_discordant = safe_col(mic, "mic_is_discordant", NA)
            )

          if (all_runs) {
            # ---- ALL RUNS MODE: one row per run per sample -------------------
            mic_detail <- mic_prep %>%
              dplyr::mutate(
                mic_run_date   = .mic_date_parsed,
                mic_barcode    = .mic_barcode,
                mic_numero     = .mic_numero,
                mic_run_id     = .mic_run_id,
                mic_result     = mic_result_code(.status_per_run),
                mic_177T       = fmt_cq(.cq_mean_177T, .cq_sd_177T, use_mean_sd),
                mic_18S2       = fmt_cq(.cq_mean_18S2, .cq_sd_18S2, use_mean_sd),
                mic_RNAseP_DNA = fmt_cq(.cq_mean_RP_DNA, .cq_sd_RP_DNA, use_mean_sd),
                mic_RNAseP_RNA = fmt_cq(.cq_mean_RP_RNA, .cq_sd_RP_RNA, use_mean_sd),
                mic_tryp_rna   = classify_tryp_rna(.cq_mean_177T, .cq_mean_18S2, tryp_delta),
                mic_human_rna  = classify_human_rna(.cq_mean_RP_DNA, .cq_mean_RP_RNA, human_delta),
                mic_remark     = mic_auto_remark(mic_result, mic_tryp_rna, mic_human_rna,
                                                 .cq_mean_RP_DNA, .cq_mean_RP_RNA,
                                                 .cq_mean_177T, .cq_mean_18S2),
                # Tag retests
                mic_remark = dplyr::if_else(
                  !is.na(.mic_n_tests) & .mic_n_tests > 1,
                  paste0(dplyr::coalesce(mic_remark, ""),
                         dplyr::if_else(
                           !is.na(.mic_is_discordant) & .mic_is_discordant,
                           " [RETEST - discordant]",
                           " [RETEST]"
                         )),
                  mic_remark
                ),
                .mic_lab_norm = tolower(trimws(as.character(SampleID)))
              ) %>%
              dplyr::select(
                .mic_lab_norm, mic_run_date, mic_barcode, mic_numero, mic_run_id,
                mic_result, mic_177T, mic_18S2, mic_RNAseP_DNA, mic_RNAseP_RNA,
                mic_tryp_rna, mic_human_rna, mic_remark
              )

            base <- base %>%
              dplyr::left_join(mic_detail, by = c(".lab_id_norm" = ".mic_lab_norm"),
                               relationship = "many-to-many")

          } else {
            # ---- CONSOLIDATED MODE: one row per sample (latest run) ----------
            # Always show Cq values from the LATEST run — if a sample was
            # retested it is because there was doubt about the earlier run.
            # The consolidated result (mic_status_final) still uses the
            # "any positive = positive" rule from the MIC pipeline.
            mic_summary <- mic_prep %>%
              dplyr::arrange(SampleID, dplyr::desc(.mic_date_parsed)) %>%
              dplyr::group_by(SampleID) %>%
              dplyr::summarise(
                # Latest run date
                mic_run_date    = dplyr::first(.mic_date_parsed),
                mic_barcode     = dplyr::first(.mic_barcode),
                mic_numero      = dplyr::first(.mic_numero),
                # Consolidated status (handles retests: any positive = positive)
                mic_result      = mic_result_code(dplyr::first(.status_consolidated)),
                # Cq values from latest run
                mic_177T        = fmt_cq(dplyr::first(.cq_mean_177T),
                                         dplyr::first(.cq_sd_177T), use_mean_sd),
                mic_18S2        = fmt_cq(dplyr::first(.cq_mean_18S2),
                                         dplyr::first(.cq_sd_18S2), use_mean_sd),
                mic_RNAseP_DNA  = fmt_cq(dplyr::first(.cq_mean_RP_DNA),
                                         dplyr::first(.cq_sd_RP_DNA), use_mean_sd),
                mic_RNAseP_RNA  = fmt_cq(dplyr::first(.cq_mean_RP_RNA),
                                         dplyr::first(.cq_sd_RP_RNA), use_mean_sd),
                .raw_177T       = dplyr::first(.cq_mean_177T),
                .raw_18S2       = dplyr::first(.cq_mean_18S2),
                .raw_RP_DNA     = dplyr::first(.cq_mean_RP_DNA),
                .raw_RP_RNA     = dplyr::first(.cq_mean_RP_RNA),
                .n_tests        = dplyr::first(.mic_n_tests),
                .is_discordant  = dplyr::first(.mic_is_discordant),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                mic_tryp_rna  = classify_tryp_rna(.raw_177T, .raw_18S2, tryp_delta),
                mic_human_rna = classify_human_rna(.raw_RP_DNA, .raw_RP_RNA, human_delta),
                mic_remark    = mic_auto_remark(mic_result, mic_tryp_rna, mic_human_rna,
                                                .raw_RP_DNA, .raw_RP_RNA,
                                                .raw_177T, .raw_18S2),
                # Tag retests in remark
                mic_remark = dplyr::if_else(
                  !is.na(.n_tests) & .n_tests > 1,
                  paste0(dplyr::coalesce(mic_remark, ""),
                         dplyr::if_else(
                           !is.na(.is_discordant) & .is_discordant,
                           sprintf(" [retested %dx - discordant]", .n_tests),
                           sprintf(" [retested %dx]", .n_tests)
                         )),
                  mic_remark
                ),
                .mic_lab_norm = tolower(trimws(as.character(SampleID)))
              ) %>%
              dplyr::select(
                .mic_lab_norm, mic_run_date, mic_barcode, mic_numero,
                mic_result, mic_177T, mic_18S2, mic_RNAseP_DNA, mic_RNAseP_RNA,
                mic_tryp_rna, mic_human_rna, mic_remark
              )

            base <- base %>%
              dplyr::left_join(mic_summary, by = c(".lab_id_norm" = ".mic_lab_norm"))
          }
        }
      }

      # Ensure MIC columns exist even if no data
      mic_cols <- c("mic_run_date", "mic_numero", "mic_barcode",
                    "mic_result", "mic_177T", "mic_18S2",
                    "mic_RNAseP_DNA", "mic_RNAseP_RNA",
                    "mic_tryp_rna", "mic_human_rna", "mic_remark")
      if (isTRUE(input$show_all_runs)) {
        mic_cols <- c(mic_cols, "mic_run_id")
      }
      for (col in mic_cols) {
        if (!col %in% names(base)) base[[col]] <- NA
      }

      # Replace NA Cq with "-" for tested samples (has result but no Cq)
      base <- base %>%
        dplyr::mutate(
          mic_177T       = dash_na(mic_177T, mic_result),
          mic_18S2       = dash_na(mic_18S2, mic_result),
          mic_RNAseP_DNA = dash_na(mic_RNAseP_DNA, mic_result),
          mic_RNAseP_RNA = dash_na(mic_RNAseP_RNA, mic_result)
        )

      # ----- Build final output with nice column names ----------------------
      if (isTRUE(input$show_all_runs)) {
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
            # Biologie moléculaire — all runs
            `Date analyse (Mol)`    = mic_run_date,
            `Run (Mol)`             = mic_run_id,
            `Numéro labo (Mol)`     = mic_numero,
            `Code-barres KPS (Mol)` = mic_barcode,
            `Résultat final (Mol)`  = mic_result,
            `177T`                  = mic_177T,
            `18S2`                  = mic_18S2,
            `RNAseP-DNA`            = mic_RNAseP_DNA,
            `RNAseP-RNA`            = mic_RNAseP_RNA,
            `Tryp RNA content`      = mic_tryp_rna,
            `Human RNA quality`     = mic_human_rna,
            Remarques               = dplyr::coalesce(mic_remark, .bb_remarks)
          )
      } else {
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
            # Biologie moléculaire — consolidated
            `Date analyse (Mol)`    = mic_run_date,
            `Numéro labo (Mol)`     = mic_numero,
            `Code-barres KPS (Mol)` = mic_barcode,
            `Résultat final (Mol)`  = mic_result,
            `177T`                  = mic_177T,
            `18S2`                  = mic_18S2,
            `RNAseP-DNA`            = mic_RNAseP_DNA,
            `RNAseP-RNA`            = mic_RNAseP_RNA,
            `Tryp RNA content`      = mic_tryp_rna,
            `Human RNA quality`     = mic_human_rna,
            Remarques               = dplyr::coalesce(mic_remark, .bb_remarks)
          )
      }

      out
    })

    # ========================================================================
    # PREVIEW TABLE
    # ========================================================================
    output$summary_text <- renderText({
      df <- export_data()
      n_total <- nrow(df)
      n_mic   <- sum(!is.na(df$`Résultat final (Mol)`))
      n_retest <- sum(grepl("\\[retest|\\[RETEST", df$Remarques, ignore.case = TRUE), na.rm = TRUE)
      paste0(n_total, " rows | ",
             n_mic, " with MIC results",
             if (n_retest > 0) paste0(" | ", n_retest, " retested") else "")
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
        suffix <- if (isTRUE(input$show_all_runs)) " all runs" else ""
        paste0(format(Sys.Date(), "%y%m%d"),
               " Registre résultats analyses KPS", suffix, ".xlsx")
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
        remark_style <- openxlsx::createStyle(
          halign = "left",
          valign = "center",
          border = "TopBottomLeftRight",
          fontSize = 10,
          wrapText = TRUE
        )

        # ---- Row 1: Group headers ------------------------------------------
        col_names <- names(df)
        n_cols    <- length(col_names)

        # MIC group: from "Date analyse (Mol)" to "Remarques"
        mic_start <- which(col_names == "Date analyse (Mol)")
        mic_end   <- n_cols
        if (length(mic_start) == 1) {
          openxlsx::mergeCells(wb, sheet_name, cols = mic_start:mic_end, rows = 1)
          openxlsx::writeData(wb, sheet_name, "Biologie moléculaire",
                              startCol = mic_start, startRow = 1)
          openxlsx::addStyle(wb, sheet_name, group_style,
                             rows = 1, cols = mic_start:mic_end, gridExpand = TRUE)
        }

        # ---- Row 2: Column headers -----------------------------------------
        display_names <- col_names
        display_names[display_names == "Date analyse (Mol)"]    <- "Date analyse"
        display_names[display_names == "Run (Mol)"]             <- "Run"
        display_names[display_names == "Numéro labo (Mol)"]     <- "Numéro labo"
        display_names[display_names == "Code-barres KPS (Mol)"] <- "Code-barres KPS"
        display_names[display_names == "Résultat final (Mol)"]  <- "Résultat final"

        for (i in seq_along(display_names)) {
          openxlsx::writeData(wb, sheet_name, display_names[i],
                              startCol = i, startRow = 2)
        }
        openxlsx::addStyle(wb, sheet_name, header_style,
                           rows = 2, cols = seq_len(n_cols), gridExpand = TRUE)

        # ---- Row 3+: Data --------------------------------------------------
        if (nrow(df) > 0) {
          openxlsx::writeData(wb, sheet_name, df,
                              startCol = 1, startRow = 3,
                              colNames = FALSE)

          data_rows <- 3:(2 + nrow(df))

          openxlsx::addStyle(wb, sheet_name, data_style,
                             rows = data_rows,
                             cols = seq_len(n_cols),
                             gridExpand = TRUE)

          # Date columns
          date_cols <- which(col_names %in% c("Date envoi INRB", "Date analyse (Mol)"))
          if (length(date_cols)) {
            openxlsx::addStyle(wb, sheet_name, date_style,
                               rows = data_rows, cols = date_cols,
                               gridExpand = TRUE)
          }

          # Remarks column: left-aligned
          remark_col <- which(col_names == "Remarques")
          if (length(remark_col)) {
            openxlsx::addStyle(wb, sheet_name, remark_style,
                               rows = data_rows, cols = remark_col,
                               gridExpand = TRUE)
          }
        }

        # ---- Column widths (dynamic based on actual columns) ---------------
        width_map <- c(
          "Numéro labo" = 12, "Etude" = 8,
          "Structure sanitaire" = 30, "Zone de santé" = 18,
          "Province" = 18, "Responsable" = 25,
          "Contact" = 15, "Code-barres KPS" = 16,
          "Date envoi INRB" = 16,
          "Date analyse (Mol)" = 15, "Run (Mol)" = 14,
          "Numéro labo (Mol)" = 12,
          "Code-barres KPS (Mol)" = 16,
          "Résultat final (Mol)" = 14,
          "177T" = 16, "18S2" = 16,
          "RNAseP-DNA" = 16, "RNAseP-RNA" = 16,
          "Tryp RNA content" = 18, "Human RNA quality" = 18,
          "Remarques" = 42
        )
        for (i in seq_along(col_names)) {
          w <- width_map[col_names[i]]
          if (is.na(w)) w <- 14
          openxlsx::setColWidths(wb, sheet_name, cols = i, widths = w)
        }

        # Freeze panes
        openxlsx::freezePane(wb, sheet_name, firstActiveRow = 3,
                             firstActiveCol = if (length(mic_start)) mic_start else 10)

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
