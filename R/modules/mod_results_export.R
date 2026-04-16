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
      # SD=0 or NA means single replicate — don't report SD
      if (is.na(sd_val) || sd_val == 0) return(m)
      s <- gsub("\\.", ",", formatC(round(sd_val, 1), format = "f", digits = 1))
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

    # ========================================================================
    # ELISA / iELISA HELPERS
    # ========================================================================

    # Map ELISA status to short code
    elisa_result_code <- function(status) {
      dplyr::case_when(
        grepl("Positive", status, ignore.case = TRUE) ~ "P",
        grepl("Borderline", status, ignore.case = TRUE) ~ "B",
        grepl("Negative", status, ignore.case = TRUE) ~ "N",
        grepl("Invalid", status, ignore.case = TRUE) ~ "Inv",
        is.na(status) ~ NA_character_,
        TRUE ~ status
      )
    }

    # Format percentage with comma decimal separator (FR locale)
    fmt_pct <- function(val) {
      dplyr::if_else(
        is.na(val), NA_character_,
        gsub("\\.", ",", formatC(round(val, 1), format = "f", digits = 1))
      )
    }

    # Format OD value
    fmt_od <- function(val) {
      dplyr::if_else(
        is.na(val), NA_character_,
        gsub("\\.", ",", formatC(round(val, 3), format = "f", digits = 3))
      )
    }

    # Auto-remark for ELISA/iELISA
    elisa_auto_remark <- function(result, n_tests, is_discordant) {
      retest_tag <- dplyr::case_when(
        is.na(n_tests) | n_tests <= 1 ~ "",
        !is.na(is_discordant) & is_discordant ~
          sprintf(" [retested %dx - discordant]", n_tests),
        TRUE ~ sprintf(" [retested %dx]", n_tests)
      )
      remark <- dplyr::case_when(
        is.na(result) ~ NA_character_,
        result == "Inv" ~ "invalid plate or sample QC failed",
        result == "B" ~ "borderline: repeat recommended",
        TRUE ~ NA_character_
      )
      # Only add retest tag if there's something to tag or there's a remark
      dplyr::case_when(
        !is.na(remark) ~ paste0(remark, retest_tag),
        nchar(retest_tag) > 0 ~ trimws(retest_tag),
        TRUE ~ NA_character_
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
        # Positive
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
            # Always use the LATEST run — if a sample was retested it is
            # because there was doubt about the earlier run, so the latest
            # result is the most reliable. Both the result code and the Cq
            # values come from the latest run.
            mic_summary <- mic_prep %>%
              dplyr::arrange(SampleID, dplyr::desc(.mic_date_parsed)) %>%
              dplyr::group_by(SampleID) %>%
              dplyr::summarise(
                # Latest run
                mic_run_date    = dplyr::first(.mic_date_parsed),
                mic_barcode     = dplyr::first(.mic_barcode),
                mic_numero      = dplyr::first(.mic_numero),
                # Result from latest run (not the pipeline's "any positive" rule)
                mic_result      = mic_result_code(dplyr::first(.status_per_run)),
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

      # Replace NA Cq with en-dash for tested samples (has result but no Cq)
      base <- base %>%
        dplyr::mutate(
          mic_177T       = dash_na(mic_177T, mic_result),
          mic_18S2       = dash_na(mic_18S2, mic_result),
          mic_RNAseP_DNA = dash_na(mic_RNAseP_DNA, mic_result),
          mic_RNAseP_RNA = dash_na(mic_RNAseP_RNA, mic_result)
        )

      # =====================================================================
      # ELISA-PE
      # =====================================================================
      if ("elisa_pe" %in% input$tests_to_include && !is.null(elisa_pe_df)) {
        epe <- tryCatch(elisa_pe_df(), error = function(e) NULL)
        if (!is.null(epe) && nrow(epe) > 0) {
          epe <- epe %>% dplyr::filter(sample_type == "sample" | is.na(sample_type))
          if (nrow(epe) > 0) {
            # Use latest run result (consistent with MIC approach)
            status_col <- if ("elisa_pe_status_final" %in% names(epe))
              "elisa_pe_status_final" else "status_final"
            n_tests_col <- if ("elisa_pe_n_tests" %in% names(epe))
              "elisa_pe_n_tests" else NULL
            disc_col <- if ("elisa_pe_is_discordant" %in% names(epe))
              "elisa_pe_is_discordant" else NULL

            epe_prep <- epe %>%
              dplyr::mutate(
                .epe_date = suppressWarnings(as.Date(test_date)),
                .epe_lab_norm = tolower(trimws(as.character(numero_labo))),
                .epe_barcode = as.character(code_barres_kps),
                .epe_numero  = as.character(numero_labo),
                .epe_status  = .data[[status_col]],
                .epe_pp      = PP_percent,
                .epe_dod     = DOD,
                .epe_n_tests = if (!is.null(n_tests_col)) .data[[n_tests_col]] else NA_integer_,
                .epe_disc    = if (!is.null(disc_col)) .data[[disc_col]] else NA
              )

            epe_summary <- epe_prep %>%
              dplyr::arrange(.epe_lab_norm, dplyr::desc(.epe_date)) %>%
              dplyr::group_by(.epe_lab_norm) %>%
              dplyr::summarise(
                epe_date    = dplyr::first(.epe_date),
                epe_numero  = dplyr::first(.epe_numero),
                epe_barcode = dplyr::first(.epe_barcode),
                epe_result  = elisa_result_code(dplyr::first(.epe_status)),
                epe_pp      = dplyr::first(.epe_pp),
                epe_dod     = dplyr::first(.epe_dod),
                .epe_n_tests = dplyr::first(.epe_n_tests),
                .epe_disc    = dplyr::first(.epe_disc),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                epe_pp_fmt  = fmt_pct(epe_pp),
                epe_dod_fmt = fmt_od(epe_dod),
                epe_remark  = elisa_auto_remark(epe_result, .epe_n_tests, .epe_disc)
              )

            base <- base %>%
              dplyr::left_join(
                epe_summary %>% dplyr::select(.epe_lab_norm, epe_date, epe_numero, epe_barcode,
                                              epe_result, epe_pp_fmt, epe_dod_fmt, epe_remark),
                by = c(".lab_id_norm" = ".epe_lab_norm")
              )
          }
        }
      }

      epe_cols <- c("epe_date", "epe_numero", "epe_barcode", "epe_result",
                     "epe_pp_fmt", "epe_dod_fmt", "epe_remark")
      for (col in epe_cols) {
        if (!col %in% names(base)) base[[col]] <- NA
      }

      # =====================================================================
      # ELISA-VSG
      # =====================================================================
      if ("elisa_vsg" %in% input$tests_to_include && !is.null(elisa_vsg_df)) {
        evsg <- tryCatch(elisa_vsg_df(), error = function(e) NULL)
        if (!is.null(evsg) && nrow(evsg) > 0) {
          evsg <- evsg %>% dplyr::filter(sample_type == "sample" | is.na(sample_type))
          if (nrow(evsg) > 0) {
            status_col <- if ("elisa_vsg_status_final" %in% names(evsg))
              "elisa_vsg_status_final" else "status_final"
            n_tests_col <- if ("elisa_vsg_n_tests" %in% names(evsg))
              "elisa_vsg_n_tests" else NULL
            disc_col <- if ("elisa_vsg_is_discordant" %in% names(evsg))
              "elisa_vsg_is_discordant" else NULL

            evsg_prep <- evsg %>%
              dplyr::mutate(
                .evsg_date = suppressWarnings(as.Date(test_date)),
                .evsg_lab_norm = tolower(trimws(as.character(numero_labo))),
                .evsg_barcode = as.character(code_barres_kps),
                .evsg_numero  = as.character(numero_labo),
                .evsg_status  = .data[[status_col]],
                .evsg_pp      = PP_percent,
                .evsg_dod     = DOD,
                .evsg_n_tests = if (!is.null(n_tests_col)) .data[[n_tests_col]] else NA_integer_,
                .evsg_disc    = if (!is.null(disc_col)) .data[[disc_col]] else NA
              )

            evsg_summary <- evsg_prep %>%
              dplyr::arrange(.evsg_lab_norm, dplyr::desc(.evsg_date)) %>%
              dplyr::group_by(.evsg_lab_norm) %>%
              dplyr::summarise(
                evsg_date    = dplyr::first(.evsg_date),
                evsg_numero  = dplyr::first(.evsg_numero),
                evsg_barcode = dplyr::first(.evsg_barcode),
                evsg_result  = elisa_result_code(dplyr::first(.evsg_status)),
                evsg_pp      = dplyr::first(.evsg_pp),
                evsg_dod     = dplyr::first(.evsg_dod),
                .evsg_n_tests = dplyr::first(.evsg_n_tests),
                .evsg_disc    = dplyr::first(.evsg_disc),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                evsg_pp_fmt  = fmt_pct(evsg_pp),
                evsg_dod_fmt = fmt_od(evsg_dod),
                evsg_remark  = elisa_auto_remark(evsg_result, .evsg_n_tests, .evsg_disc)
              )

            base <- base %>%
              dplyr::left_join(
                evsg_summary %>% dplyr::select(.evsg_lab_norm, evsg_date, evsg_numero, evsg_barcode,
                                               evsg_result, evsg_pp_fmt, evsg_dod_fmt, evsg_remark),
                by = c(".lab_id_norm" = ".evsg_lab_norm")
              )
          }
        }
      }

      evsg_cols <- c("evsg_date", "evsg_numero", "evsg_barcode", "evsg_result",
                      "evsg_pp_fmt", "evsg_dod_fmt", "evsg_remark")
      for (col in evsg_cols) {
        if (!col %in% names(base)) base[[col]] <- NA
      }

      # =====================================================================
      # iELISA
      # =====================================================================
      if ("ielisa" %in% input$tests_to_include && !is.null(ielisa_df)) {
        iel <- tryCatch(ielisa_df(), error = function(e) NULL)
        if (!is.null(iel) && nrow(iel) > 0) {
          status_col <- if ("ielisa_status_final" %in% names(iel))
            "ielisa_status_final" else "status_final"
          n_tests_col <- if ("ielisa_n_tests" %in% names(iel))
            "ielisa_n_tests" else NULL
          disc_col <- if ("ielisa_is_discordant" %in% names(iel))
            "ielisa_is_discordant" else NULL

          iel_prep <- iel %>%
            dplyr::mutate(
              .iel_date = suppressWarnings(as.Date(plate_date)),
              .iel_lab_norm = tolower(trimws(as.character(numero_labo))),
              .iel_barcode = as.character(code_barres_kps),
              .iel_numero  = as.character(numero_labo),
              .iel_status  = .data[[status_col]],
              .iel_inh_13  = pct_inh_f1_13,
              .iel_inh_15  = pct_inh_f1_15,
              .iel_n_tests = if (!is.null(n_tests_col)) .data[[n_tests_col]] else NA_integer_,
              .iel_disc    = if (!is.null(disc_col)) .data[[disc_col]] else NA
            )

          iel_summary <- iel_prep %>%
            dplyr::arrange(.iel_lab_norm, dplyr::desc(.iel_date)) %>%
            dplyr::group_by(.iel_lab_norm) %>%
            dplyr::summarise(
              iel_date    = dplyr::first(.iel_date),
              iel_numero  = dplyr::first(.iel_numero),
              iel_barcode = dplyr::first(.iel_barcode),
              iel_result  = elisa_result_code(dplyr::first(.iel_status)),
              iel_inh_13  = dplyr::first(.iel_inh_13),
              iel_inh_15  = dplyr::first(.iel_inh_15),
              .iel_n_tests = dplyr::first(.iel_n_tests),
              .iel_disc    = dplyr::first(.iel_disc),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              iel_inh_13_fmt = fmt_pct(iel_inh_13),
              iel_inh_15_fmt = fmt_pct(iel_inh_15),
              iel_remark     = elisa_auto_remark(iel_result, .iel_n_tests, .iel_disc)
            )

          base <- base %>%
            dplyr::left_join(
              iel_summary %>% dplyr::select(.iel_lab_norm, iel_date, iel_numero, iel_barcode,
                                            iel_result, iel_inh_13_fmt, iel_inh_15_fmt, iel_remark),
              by = c(".lab_id_norm" = ".iel_lab_norm")
            )
        }
      }

      iel_cols <- c("iel_date", "iel_numero", "iel_barcode", "iel_result",
                     "iel_inh_13_fmt", "iel_inh_15_fmt", "iel_remark")
      for (col in iel_cols) {
        if (!col %in% names(base)) base[[col]] <- NA
      }

      # =====================================================================
      # BUILD FINAL OUTPUT with display column names
      # =====================================================================
      # Combine remarks from all tests (only non-NA, semicolon-separated)
      base <- base %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          .all_remarks = {
            parts <- c()
            if (!is.na(mic_remark))  parts <- c(parts, mic_remark)
            if (!is.na(epe_remark))  parts <- c(parts, paste0("PE: ", epe_remark))
            if (!is.na(evsg_remark)) parts <- c(parts, paste0("VSG: ", evsg_remark))
            if (!is.na(iel_remark))  parts <- c(parts, paste0("iELISA: ", iel_remark))
            if (length(parts) > 0) paste(parts, collapse = "; ") else NA_character_
          },
          .all_remarks = dplyr::coalesce(.all_remarks, .bb_remarks)
        ) %>%
        dplyr::ungroup()

      # Base biobank columns (always present)
      out <- base %>%
        dplyr::transmute(
          `Numéro labo`         = `Numéro labo`,
          Etude                 = Etude,
          `Structure sanitaire` = `Structure sanitaire`,
          `Zone de santé`       = `Zone de santé`,
          Province              = Province,
          Responsable           = `Responsable`,
          Contact               = Contact,
          `Code-barres KPS`     = `Code-barres KPS`,
          `Date envoi INRB`     = `Date envoi INRB`,
          # MIC
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
          # ELISA-PE
          `Date analyse (PE)`     = epe_date,
          `Numéro labo (PE)`      = epe_numero,
          `Code-barres KPS (PE)`  = epe_barcode,
          `Résultat final (PE)`   = epe_result,
          `PP% (PE)`              = epe_pp_fmt,
          `DOD (PE)`              = epe_dod_fmt,
          # ELISA-VSG
          `Date analyse (VSG)`    = evsg_date,
          `Numéro labo (VSG)`     = evsg_numero,
          `Code-barres KPS (VSG)` = evsg_barcode,
          `Résultat final (VSG)`  = evsg_result,
          `PP% (VSG)`             = evsg_pp_fmt,
          `DOD (VSG)`             = evsg_dod_fmt,
          # iELISA
          `Date analyse (iELISA)`    = iel_date,
          `Numéro labo (iELISA)`     = iel_numero,
          `Code-barres KPS (iELISA)` = iel_barcode,
          `Résultat final (iELISA)`  = iel_result,
          `%Inh L1.3`               = iel_inh_13_fmt,
          `%Inh L1.5`               = iel_inh_15_fmt,
          # Remarks
          Remarques = .all_remarks
        )

      # Add Run column for all-runs mode (MIC only)
      if (isTRUE(input$show_all_runs)) {
        mic_run_id_col <- base$mic_run_id
        # Insert after Date analyse (Mol)
        pos <- which(names(out) == "Date analyse (Mol)")
        if (length(pos) == 1) {
          out <- tibble::add_column(out, `Run (Mol)` = mic_run_id_col, .after = pos)
        }
      }

      out
    })

    # ========================================================================
    # PREVIEW TABLE
    # ========================================================================
    output$summary_text <- renderText({
      df <- export_data()
      n_total <- nrow(df)
      parts <- c(paste0(n_total, " rows"))
      if ("Résultat final (Mol)" %in% names(df))
        parts <- c(parts, paste0(sum(!is.na(df$`Résultat final (Mol)`)), " MIC"))
      if ("Résultat final (PE)" %in% names(df))
        parts <- c(parts, paste0(sum(!is.na(df$`Résultat final (PE)`)), " PE"))
      if ("Résultat final (VSG)" %in% names(df))
        parts <- c(parts, paste0(sum(!is.na(df$`Résultat final (VSG)`)), " VSG"))
      if ("Résultat final (iELISA)" %in% names(df))
        parts <- c(parts, paste0(sum(!is.na(df$`Résultat final (iELISA)`)), " iELISA"))
      paste(parts, collapse = " | ")
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

        # Define test groups: label, start column, end column
        group_defs <- list(
          list(label = "Biologie mol\u00e9culaire",
               start = "Date analyse (Mol)",
               end   = "Human RNA quality"),
          list(label = "ELISA indirect PE",
               start = "Date analyse (PE)",
               end   = "DOD (PE)"),
          list(label = "ELISA indirect VSG",
               start = "Date analyse (VSG)",
               end   = "DOD (VSG)"),
          list(label = "iELISA",
               start = "Date analyse (iELISA)",
               end   = "%Inh L1.5")
        )

        for (gdef in group_defs) {
          gs <- which(col_names == gdef$start)
          ge <- which(col_names == gdef$end)
          if (length(gs) == 1 && length(ge) == 1) {
            openxlsx::mergeCells(wb, sheet_name, cols = gs:ge, rows = 1)
            openxlsx::writeData(wb, sheet_name, gdef$label,
                                startCol = gs, startRow = 1)
            openxlsx::addStyle(wb, sheet_name, group_style,
                               rows = 1, cols = gs:ge, gridExpand = TRUE)
          }
        }

        # ---- Row 2: Column headers -----------------------------------------
        # Strip the "(Mol)", "(PE)", etc. suffixes for display
        display_names <- col_names
        display_names <- gsub(" \\(Mol\\)$", "", display_names)
        display_names <- gsub(" \\(PE\\)$", "", display_names)
        display_names <- gsub(" \\(VSG\\)$", "", display_names)
        display_names <- gsub(" \\(iELISA\\)$", "", display_names)

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
          date_cols <- which(grepl("^Date", col_names))
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
          # MIC
          "Date analyse (Mol)" = 15, "Run (Mol)" = 14,
          "Numéro labo (Mol)" = 12, "Code-barres KPS (Mol)" = 16,
          "Résultat final (Mol)" = 14,
          "177T" = 16, "18S2" = 16,
          "RNAseP-DNA" = 16, "RNAseP-RNA" = 16,
          "Tryp RNA content" = 18, "Human RNA quality" = 18,
          # ELISA-PE
          "Date analyse (PE)" = 15, "Numéro labo (PE)" = 12,
          "Code-barres KPS (PE)" = 16, "Résultat final (PE)" = 14,
          "PP% (PE)" = 10, "DOD (PE)" = 10,
          # ELISA-VSG
          "Date analyse (VSG)" = 15, "Numéro labo (VSG)" = 12,
          "Code-barres KPS (VSG)" = 16, "Résultat final (VSG)" = 14,
          "PP% (VSG)" = 10, "DOD (VSG)" = 10,
          # iELISA
          "Date analyse (iELISA)" = 15, "Numéro labo (iELISA)" = 12,
          "Code-barres KPS (iELISA)" = 16, "Résultat final (iELISA)" = 14,
          "%Inh L1.3" = 12, "%Inh L1.5" = 12,
          # Remarks
          "Remarques" = 50
        )
        for (i in seq_along(col_names)) {
          w <- width_map[col_names[i]]
          if (is.na(w)) w <- 14
          openxlsx::setColWidths(wb, sheet_name, cols = i, widths = w)
        }

        # Freeze panes — freeze at first test section
        first_test_col <- min(which(grepl("^Date analyse", col_names)), Inf)
        if (is.finite(first_test_col)) {
          openxlsx::freezePane(wb, sheet_name, firstActiveRow = 3,
                               firstActiveCol = first_test_col)
        } else {
          openxlsx::freezePane(wb, sheet_name, firstActiveRow = 3)
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
