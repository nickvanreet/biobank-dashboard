# ==============================================================================
# MODULE 2: SAMPLES - Main results table with filters
# ==============================================================================

mod_mic_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(
      "
      .mic-samples-panel {
        padding-bottom: 1.5rem;
      }

      .mic-samples-panel .mic-samples-table-body {
        overflow: visible;
      }

      .mic-samples-panel .mic-samples-table-body > .dataTables_wrapper {
        overflow: visible;
      }
      "
    )),
    div(
      class = "mic-samples-panel container-fluid",
      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Sample KPIs"),
          div(
            class = "d-flex align-items-center gap-2",
            div(
              class = "form-check form-switch m-0",
              checkboxInput(ns("secondary_only"), label = NULL, value = FALSE, width = "auto")
            ),
            tags$span(
              "Use secondary result when available",
              class = "text-muted small"
            )
          )
        ),
        card_body(
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "Total Samples Tested",
              value = textOutput(ns("kpi_samples_total")),
              showcase = icon("vial"),
              theme = "primary"
            ),

            value_box(
              title = "Unique Samples",
              value = textOutput(ns("kpi_samples_unique")),
              showcase = icon("id-card"),
              theme = "info"
            ),

            value_box(
              title = "Tested Once",
              value = textOutput(ns("kpi_samples_once")),
              showcase = icon("circle-check"),
              theme = "success"
            ),

            value_box(
              title = "Retested",
              value = textOutput(ns("kpi_samples_multiple")),
              showcase = icon("redo"),
              theme = "warning"
            )
          ),

          layout_column_wrap(
            width = 1/3,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "% Linked to Biobank",
              value = textOutput(ns("kpi_biobank_linked")),
              showcase = icon("warehouse"),
              theme = "info"
            ),

            value_box(
              title = "% Linked to Extraction",
              value = textOutput(ns("kpi_extraction_linked")),
              showcase = icon("flask"),
              theme = "success"
            ),

            value_box(
              title = "% Linked to Runs",
              value = textOutput(ns("kpi_run_linked")),
              showcase = icon("play-circle"),
              theme = "primary"
            )
          ),

          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "TNA Positive",
              value = textOutput(ns("kpi_samples_tna")),
              showcase = icon("dna"),
              theme = "success"
            ),

            value_box(
              title = "DNA Only Positive",
              value = textOutput(ns("kpi_samples_dna")),
              showcase = icon("circle-plus"),
              theme = "info"
            ),

            value_box(
              title = "RNA Only Positive",
              value = textOutput(ns("kpi_samples_rna")),
              showcase = icon("wave-square"),
              theme = "info"
            ),

            value_box(
              title = "Indeterminate",
              value = textOutput(ns("kpi_samples_indeterminate")),
              showcase = icon("question-circle"),
              theme = "warning"
            ),

            value_box(
              title = "Negative",
              value = textOutput(ns("kpi_samples_negative")),
              showcase = icon("circle-minus"),
              theme = "secondary"
            ),

            value_box(
              title = "Invalid",
              value = textOutput(ns("kpi_samples_invalid")),
              showcase = icon("ban"),
              theme = "danger"
            ),

            value_box(
              title = "TNA Prevalence",
              value = textOutput(ns("kpi_samples_tna_prev")),
              showcase = icon("percent"),
              theme = "primary"
            ),

            value_box(
              title = "Any Positive Prevalence",
              value = textOutput(ns("kpi_samples_any_prev")),
              showcase = icon("chart-pie"),
              theme = "primary"
            )
          ),

          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "High Confidence",
              value = textOutput(ns("kpi_confidence_high")),
              showcase = icon("star"),
              theme = "success"
            ),

            value_box(
              title = "Medium Confidence",
              value = textOutput(ns("kpi_confidence_medium")),
              showcase = icon("star-half-stroke"),
              theme = "info"
            ),

            value_box(
              title = "Low Confidence",
              value = textOutput(ns("kpi_confidence_low")),
              showcase = icon("triangle-exclamation"),
              theme = "warning"
            ),

            value_box(
              title = "Samples with Conflicts",
              value = textOutput(ns("kpi_conflicts")),
              showcase = icon("exclamation-circle"),
              theme = "danger"
            )
          )
        )
      ),

      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Decision & Confidence Heatmap"),
          tags$small("Hover to see quality context", class = "text-muted")
        ),
        card_body(
          plotlyOutput(ns("decision_confidence_heatmap"), height = "420px"),
          tags$small(
            "Tile intensity reflects sample counts. Labels show conflicts or invalid rates when present.",
            class = "text-muted"
          )
        )
      ),

      # Decision Tree Summary
      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Decision Tree Summary"),
          tags$small("Uses one result per sample based on the toggle", class = "text-muted")
        ),
        card_body(
          layout_column_wrap(
            width = 1/3,
            gap = "12px",

            card(
              card_header("Decision Step Distribution"),
              card_body(DTOutput(ns("tbl_decision_steps")))
            ),

            card(
              card_header("Confidence Distribution"),
              card_body(DTOutput(ns("tbl_confidence_metrics")))
            ),

            card(
              card_header("Quality Metrics"),
              card_body(DTOutput(ns("tbl_quality_metrics")))
            )
          )
        )
      ),

      # Results table - filters are applied from sidebar
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            span("Sample Results"),
            tags$small("Showing all available tests; toggle to view primary only", class = "text-muted d-block")
          ),
          div(
            class = "d-flex align-items-center gap-3",
            div(
              class = "form-check form-switch m-0",
              checkboxInput(
                ns("primary_only"),
                label = tags$span("Primary results only", class = "text-muted small"),
                value = FALSE,
                width = "auto"
              )
            ),
            downloadButton(ns("dl_filtered"), "Download", class = "btn-sm btn-outline-primary")
          )
        ),
        card_body(
          DTOutput(ns("tbl_samples")),
          class = "p-3 mic-samples-table-body"
        )
      )
    )
  )
}

mod_mic_samples_server <- function(id, filtered_base, processed_data) {
  moduleServer(id, function(input, output, session) {

    ordinal_label <- function(n) {
      n <- as.integer(n)
      suffix <- ifelse(
        n %% 100L %in% c(11L, 12L, 13L), "th",
        ifelse(
          n %% 10L == 1L, "st",
          ifelse(n %% 10L == 2L, "nd",
                 ifelse(n %% 10L == 3L, "rd", "th"))
        )
      )
      paste0(n, suffix)
    }

    append_test_order <- function(df, drop_helper_cols = TRUE) {
      if (!nrow(df)) return(df)

      has_id <- "SampleID" %in% names(df)
      has_name <- "SampleName" %in% names(df)

      if (!has_id && !has_name) return(df)

      result <- df %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_id) as.character(SampleID) else NA_character_,
            if (has_name) as.character(SampleName) else NA_character_,
            paste0("row_", dplyr::row_number())
          ),
          RunKey = dplyr::coalesce(
            if ("RunID" %in% names(.)) as.character(RunID) else NA_character_,
            paste0("run_", dplyr::row_number())
          ),
          RunDateTimeParsed = if ("RunDateTime" %in% names(.)) {
            suppressWarnings(lubridate::ymd_hms(as.character(RunDateTime), tz = "UTC"))
          } else {
            as.POSIXct(NA)
          },
          RunDateParsed = if ("RunDate" %in% names(.)) {
            suppressWarnings(lubridate::ymd(as.character(RunDate)))
          } else {
            as.Date(NA)
          }
        ) %>%
        arrange(SampleKey, RunDateTimeParsed, RunDateParsed, RunKey) %>%
        group_by(SampleKey) %>%
        mutate(
          TestNumber = dplyr::row_number(),
          TestOrderLabel = dplyr::case_when(
            TestNumber == 1L ~ "Primary",
            TestNumber == 2L ~ "Secondary",
            TestNumber == 3L ~ "Tertiary",
            TRUE ~ paste(ordinal_label(TestNumber), "Test")
          )
        ) %>%
        ungroup()

      if (isTRUE(drop_helper_cols)) {
        result <- result %>% select(-SampleKey, -RunKey, -RunDateTimeParsed, -RunDateParsed)
      }

      result
    }

    drop_helper_columns <- function(df) {
      df %>% select(-any_of(c("SampleKey", "RunKey", "RunDateTimeParsed", "RunDateParsed", "TargetTest")))
    }

    samples_with_order <- reactive({
      df <- filtered_base()

      if (is.null(df) || !nrow(df) || !"ControlType" %in% names(df)) return(tibble())

      df %>%
        filter(ControlType == "Sample") %>%
        append_test_order(drop_helper_cols = FALSE)
    })

    table_results <- reactive({
      df <- samples_with_order()

      if (!nrow(df)) return(df)

      if (isTRUE(input$primary_only)) {
        df <- df %>% filter(TestNumber == 1L)
      }

      df
    })

    selected_results <- reactive({
      df <- samples_with_order()

      if (!nrow(df)) return(df)

      df %>%
        group_by(SampleKey) %>%
        mutate(
          TargetTest = if_else(isTRUE(input$secondary_only) & max(TestNumber) >= 2, 2L, 1L),
          TargetTest = pmin(TargetTest, max(TestNumber))
        ) %>%
        filter(TestNumber == TargetTest) %>%
        slice_head(n = 1) %>%
        ungroup()
    })

    decision_tree_summary <- reactive({
      df <- selected_results()

      if (!nrow(df)) {
        return(list(
          total = 0,
          confidence = tibble(),
          confidence_total = 0,
          quality = tibble()
        ))
      }

      confidence <- if ("ConfidenceScore" %in% names(df)) {
        df %>%
          filter(!is.na(ConfidenceScore)) %>%
          count(ConfidenceScore, name = "Count")
      } else {
        tibble()
      }

      confidence_total <- sum(confidence$Count)

      if (nrow(confidence) && confidence_total > 0) {
        confidence <- confidence %>%
          mutate(Percentage = round(100 * Count / confidence_total, 1))
      }

      total_samples <- nrow(df)
      quality_rows <- list()

      if ("WellAggregateConflict" %in% names(df)) {
        conflict_count <- sum(df$WellAggregateConflict == TRUE, na.rm = TRUE)

        quality_rows <- append(quality_rows, list(tibble(
          Metric = "Samples with Conflicts",
          Count = conflict_count,
          Percentage = round(100 * conflict_count / total_samples, 1)
        )))
      }

      if ("FinalCall" %in% names(df)) {
        invalid_count <- sum(df$FinalCall %in% c("Invalid", "Invalid_NoDNA", "RunInvalid"), na.rm = TRUE)
        indet_count <- sum(df$FinalCall == "Indeterminate", na.rm = TRUE)

        if (invalid_count > 0) {
          quality_rows <- append(quality_rows, list(tibble(
            Metric = "Invalid Results",
            Count = invalid_count,
            Percentage = round(100 * invalid_count / total_samples, 1)
          )))
        }

        quality_rows <- append(quality_rows, list(tibble(
          Metric = "Indeterminate Results",
          Count = indet_count,
          Percentage = round(100 * indet_count / total_samples, 1)
        )))
      }

      quality_summary <- if (length(quality_rows)) bind_rows(quality_rows) else tibble()

      list(
        total = total_samples,
        confidence = confidence,
        confidence_total = confidence_total,
        quality = quality_summary
      )
    })

    decision_confidence_data <- reactive({
      df <- selected_results()

      if (!nrow(df)) return(tibble())

      call_levels <- c(
        "Positive", "Positive_DNA", "Positive_RNA", "LatePositive",
        "Negative", "Indeterminate", "Invalid_NoDNA", "Invalid",
        "RunInvalid", "Control", "Control_Fail", "Other", "Unknown"
      )

      conf_levels <- c("High", "Medium", "Low", "Not specified", "Other")
      invalid_calls <- c("Invalid_NoDNA", "Invalid", "RunInvalid")

      df %>%
        mutate(
          FinalCall = case_when(
            is.na(FinalCall) ~ "Unknown",
            FinalCall %in% call_levels ~ FinalCall,
            TRUE ~ "Other"
          ),
          ConfidenceScore = case_when(
            is.na(ConfidenceScore) ~ "Not specified",
            ConfidenceScore %in% c("High", "Medium", "Low") ~ ConfidenceScore,
            TRUE ~ "Other"
          ),
          QualityFlag = case_when(
            "WellAggregateConflict" %in% names(df) & WellAggregateConflict ~ "Conflict",
            FinalCall %in% invalid_calls ~ "Invalid",
            FinalCall == "Indeterminate" ~ "Indeterminate",
            TRUE ~ "Clean"
          )
        ) %>%
        group_by(FinalCall, ConfidenceScore) %>%
        summarise(
          Samples = n(),
          Conflicts = sum(if ("WellAggregateConflict" %in% names(df)) WellAggregateConflict == TRUE else FALSE, na.rm = TRUE),
          Invalids = sum(FinalCall %in% invalid_calls, na.rm = TRUE),
          DominantQuality = names(sort(table(QualityFlag), decreasing = TRUE))[1],
          .groups = "drop"
        ) %>%
        mutate(
          FinalCall = factor(FinalCall, levels = call_levels),
          ConfidenceScore = factor(ConfidenceScore, levels = conf_levels),
          ConflictRate = if_else(Samples > 0, round(100 * Conflicts / Samples, 1), NA_real_),
          InvalidRate = if_else(Samples > 0, round(100 * Invalids / Samples, 1), NA_real_),
          TileLabel = dplyr::case_when(
            !is.na(ConflictRate) & ConflictRate > 0 ~ paste0(Samples, " (", ConflictRate, "% conflicts)"),
            !is.na(InvalidRate) & InvalidRate > 0 ~ paste0(Samples, " (", InvalidRate, "% invalid)"),
            TRUE ~ as.character(Samples)
          ),
          Tooltip = paste0(
            "Final call: ", FinalCall,
            "<br>Confidence: ", ConfidenceScore,
            "<br>Samples: ", Samples,
            ifelse(Conflicts > 0, paste0("<br>Conflicts: ", Conflicts, " (", ConflictRate, "%)"), ""),
            ifelse(Invalids > 0, paste0("<br>Invalid/RunInvalid: ", Invalids, " (", InvalidRate, "%)"), ""),
            "<br>Dominant quality: ", DominantQuality
          )
        ) %>%
        arrange(FinalCall, ConfidenceScore)
    })

    format_count_pct <- function(count, total, percentage = NULL) {
      pct_value <- if (!is.null(percentage)) {
        percentage
      } else if (!is.null(total) && total > 0) {
        round(100 * count / total, 1)
      } else {
        NA_real_
      }

      formatted_count <- scales::comma(count)

      if (is.na(pct_value)) return(formatted_count)

      paste0(formatted_count, " (", sprintf("%.1f%%", pct_value), ")")
    }

    sample_metrics <- reactive({
      df <- samples_with_order()
      if (!nrow(df)) return(NULL)

      total_instances <- nrow(df)

      sample_counts <- df %>% count(SampleKey, name = "TimesTested")

      deduped <- selected_results()

      unique_samples <- nrow(sample_counts)
      tested_once <- sum(sample_counts$TimesTested == 1)
      tested_multiple <- sum(sample_counts$TimesTested > 1)
      deduped_total <- nrow(deduped)

      final_calls <- if ("FinalCall" %in% names(deduped)) {
        deduped$FinalCall
      } else {
        rep(NA_character_, deduped_total)
      }

      tna <- sum(final_calls == "Positive", na.rm = TRUE)
      dna <- sum(final_calls == "Positive_DNA", na.rm = TRUE)
      rna <- sum(final_calls == "Positive_RNA", na.rm = TRUE)
      indeterminate <- sum(final_calls == "Indeterminate", na.rm = TRUE)
      negative <- sum(final_calls == "Negative", na.rm = TRUE)
      invalid <- sum(final_calls %in% c("Invalid", "Invalid_NoDNA", "RunInvalid"), na.rm = TRUE)
      any_positive <- sum(final_calls %in% c("Positive", "Positive_DNA", "Positive_RNA"), na.rm = TRUE)

      tna_prev <- if (deduped_total) round(100 * tna / deduped_total, 1) else NA_real_
      any_prev <- if (deduped_total) round(100 * any_positive / deduped_total, 1) else NA_real_

      linked_biobank <- if ("BiobankMatched" %in% names(deduped)) {
        sum(deduped$BiobankMatched, na.rm = TRUE)
      } else {
        NA_integer_
      }

      linked_extraction <- if ("ExtractionMatched" %in% names(deduped)) {
        sum(deduped$ExtractionMatched, na.rm = TRUE)
      } else {
        NA_integer_
      }

      run_links <- if ("RunID" %in% names(deduped)) {
        run_df <- tryCatch(processed_data()$runs, error = function(e) tibble())
        if (!is.null(run_df) && nrow(run_df) && "RunID" %in% names(run_df)) {
          sum(deduped$RunID %in% run_df$RunID, na.rm = TRUE)
        } else {
          sum(!is.na(deduped$RunID))
        }
      } else {
        NA_integer_
      }

      biobank_pct <- if (deduped_total && !is.na(linked_biobank)) round(100 * linked_biobank / deduped_total, 1) else NA_real_
      extraction_pct <- if (deduped_total && !is.na(linked_extraction)) round(100 * linked_extraction / deduped_total, 1) else NA_real_
      run_pct <- if (deduped_total && !is.na(run_links)) round(100 * run_links / deduped_total, 1) else NA_real_

      # Decision tree quality metrics
      confidence_scores <- if ("ConfidenceScore" %in% names(deduped)) {
        deduped$ConfidenceScore
      } else {
        rep(NA_character_, deduped_total)
      }

      conflicts <- if ("WellAggregateConflict" %in% names(deduped)) {
        deduped$WellAggregateConflict
      } else {
        rep(FALSE, deduped_total)
      }

      high_confidence <- sum(confidence_scores == "High", na.rm = TRUE)
      medium_confidence <- sum(confidence_scores == "Medium", na.rm = TRUE)
      low_confidence <- sum(confidence_scores == "Low", na.rm = TRUE)
      conflict_count <- sum(conflicts == TRUE, na.rm = TRUE)

      list(
        total_instances = total_instances,
        unique_samples = unique_samples,
        tested_once = tested_once,
        tested_multiple = tested_multiple,
        deduped_total = deduped_total,
        tna = tna,
        dna = dna,
        rna = rna,
        indeterminate = indeterminate,
        negative = negative,
        invalid = invalid,
        tna_prev = tna_prev,
        any_prev = any_prev,
        biobank_pct = biobank_pct,
        extraction_pct = extraction_pct,
        run_pct = run_pct,
        high_confidence = high_confidence,
        medium_confidence = medium_confidence,
        low_confidence = low_confidence,
        conflict_count = conflict_count
      )
    })

    output$kpi_samples_total <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$total_instances)
    })

    output$kpi_samples_unique <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$unique_samples)
    })

    output$kpi_samples_once <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$tested_once)
    })

    output$kpi_samples_multiple <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      suffix <- if (isTRUE(input$secondary_only) && metrics$tested_multiple > 0) " (secondary applied)" else ""
      paste0(scales::comma(metrics$tested_multiple), suffix)
    })

    output$kpi_biobank_linked <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$biobank_pct)) return("0%")
      sprintf("%.1f%%", metrics$biobank_pct)
    })

    output$kpi_extraction_linked <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$extraction_pct)) return("0%")
      sprintf("%.1f%%", metrics$extraction_pct)
    })

    output$kpi_run_linked <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$run_pct)) return("0%")
      sprintf("%.1f%%", metrics$run_pct)
    })

    output$kpi_samples_tna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$tna)
    })

    output$kpi_samples_dna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$dna)
    })

    output$kpi_samples_rna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$rna)
    })

    output$kpi_samples_indeterminate <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$indeterminate)
    })

    output$kpi_samples_negative <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$negative)
    })

    output$kpi_samples_invalid <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$invalid)
    })

    output$kpi_samples_tna_prev <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$tna_prev)) return("0%")
      sprintf("%.1f%%", metrics$tna_prev)
    })

    output$kpi_samples_any_prev <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$any_prev)) return("0%")
      sprintf("%.1f%%", metrics$any_prev)
    })

    output$kpi_confidence_high <- renderText({
      summary <- decision_tree_summary()
      conf <- summary$confidence

      if (!nrow(conf)) return("0")

      row <- conf %>% filter(ConfidenceScore == "High")
      count <- if (nrow(row)) row$Count else 0
      pct <- if (nrow(row) && "Percentage" %in% names(row)) row$Percentage else NA_real_

      format_count_pct(count, summary$confidence_total, pct)
    })

    output$kpi_confidence_medium <- renderText({
      summary <- decision_tree_summary()
      conf <- summary$confidence

      if (!nrow(conf)) return("0")

      row <- conf %>% filter(ConfidenceScore == "Medium")
      count <- if (nrow(row)) row$Count else 0
      pct <- if (nrow(row) && "Percentage" %in% names(row)) row$Percentage else NA_real_

      format_count_pct(count, summary$confidence_total, pct)
    })

    output$kpi_confidence_low <- renderText({
      summary <- decision_tree_summary()
      conf <- summary$confidence

      if (!nrow(conf)) return("0")

      row <- conf %>% filter(ConfidenceScore == "Low")
      count <- if (nrow(row)) row$Count else 0
      pct <- if (nrow(row) && "Percentage" %in% names(row)) row$Percentage else NA_real_

      format_count_pct(count, summary$confidence_total, pct)
    })

    output$kpi_conflicts <- renderText({
      summary <- decision_tree_summary()
      quality <- summary$quality

      if (!nrow(quality)) return("0")

      row <- quality %>% filter(Metric == "Samples with Conflicts")
      count <- if (nrow(row)) row$Count else 0
      pct <- if (nrow(row) && "Percentage" %in% names(row)) row$Percentage else NA_real_

      format_count_pct(count, summary$total, pct)
    })

    # Decision tree summary tables
    output$tbl_decision_steps <- renderDT({
      df <- selected_results()

      if (!nrow(df) || !"DecisionStep" %in% names(df)) {
        return(datatable(
          tibble(Message = "No decision step data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      step_summary <- df %>%
        filter(!is.na(DecisionStep)) %>%
        group_by(DecisionStep) %>%
        summarise(
          Count = n(),
          .groups = "drop"
        ) %>%
        arrange(DecisionStep) %>%
        mutate(
          Percentage = sprintf("%.1f%%", 100 * Count / sum(Count)),
          DecisionStep = case_when(
            DecisionStep == "Step0" ~ "Step 0: Control Handling",
            DecisionStep == "Step1" ~ "Step 1: QC Validity Check",
            DecisionStep == "Step2" ~ "Step 2: TNA Positive (≥2 wells)",
            DecisionStep == "Step3" ~ "Step 3: Single TNA Well",
            DecisionStep == "Step4a" ~ "Step 4a: DNA-only Pattern",
            DecisionStep == "Step4b" ~ "Step 4b: RNA-only Pattern",
            DecisionStep == "Step5" ~ "Step 5: Late Positive TNA",
            DecisionStep == "Step6a" ~ "Step 6a: Single Outlier + Negatives",
            DecisionStep == "Step6b" ~ "Step 6b: Weak Mixed Signals",
            DecisionStep == "Step7" ~ "Step 7: Clear Negative",
            DecisionStep == "Step8" ~ "Step 8: Insufficient Data",
            TRUE ~ DecisionStep
          )
        ) %>%
        select(`Decision Step` = DecisionStep, Count, Percentage)

      datatable(
        step_summary,
        options = list(
          pageLength = 15,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "display compact stripe"
      )
    })

    output$tbl_confidence_metrics <- renderDT({
      summary <- decision_tree_summary()
      conf_summary <- summary$confidence

      if (!nrow(conf_summary)) {
        return(datatable(
          tibble(Message = "No confidence scores available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      conf_summary <- conf_summary %>%
        mutate(Percentage = sprintf("%.1f%%", Percentage)) %>%
        arrange(desc(Count))

      datatable(
        conf_summary,
        options = list(
          pageLength = 15,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "display compact stripe"
      )
    })

    output$tbl_quality_metrics <- renderDT({
      summary <- decision_tree_summary()
      quality_summary <- summary$quality

      if (!nrow(quality_summary)) {
        return(datatable(
          tibble(Message = "No quality metrics available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      quality_summary <- quality_summary %>%
        mutate(Percentage = sprintf("%.1f%%", Percentage))

      datatable(
        quality_summary,
        options = list(
          pageLength = 15,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "display compact stripe"
      )
    })

    output$decision_confidence_heatmap <- plotly::renderPlotly({
      df <- decision_confidence_data()

      if (!nrow(df)) {
        return(
          plotly::plotly_empty(type = "heatmap", hoverinfo = "none") %>%
            plotly::layout(title = "No decision data available")
        )
      }

      df <- df %>% filter(!is.na(FinalCall), !is.na(ConfidenceScore))

      heatmap_plot <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = ConfidenceScore,
          y = FinalCall,
          fill = Samples,
          text = Tooltip,
          label = TileLabel
        )
      ) +
        ggplot2::geom_tile(color = "#f8fafc") +
        ggplot2::geom_text(size = 3, color = "#111827") +
        ggplot2::scale_fill_gradient(
          low = "#e0ecf4",
          high = "#1d4ed8",
          na.value = "#f8fafc",
          name = "Samples"
        ) +
        ggplot2::labs(
          x = "Confidence",
          y = "Final call",
          title = "Decision confidence matrix"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 12, face = "bold"),
          legend.position = "right"
        )

      plotly::ggplotly(heatmap_plot, tooltip = "text") %>%
        plotly::layout(margin = list(l = 60, r = 40, b = 80, t = 60))
    })

    output$decision_tree_calls_plot <- plotly::renderPlotly({
      df <- selected_results()

      if (!nrow(df)) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = "No data available"))
      }

      missing_required <- setdiff(c("FinalCall", "DecisionStep"), names(df))
      if (length(missing_required)) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = "Call or decision data not available"))
      }

      if (!"ConfidenceScore" %in% names(df)) {
        df$ConfidenceScore <- NA_character_
      }

      call_levels <- c(
        "Positive", "Positive_DNA", "Positive_RNA", "LatePositive",
        "Negative", "Indeterminate", "Invalid_NoDNA", "Invalid",
        "RunInvalid", "Control", "Control_Fail"
      )

      plot_data <- df %>%
        mutate(
          FinalCall = case_when(
            is.na(FinalCall) ~ "Unknown",
            FinalCall %in% call_levels ~ FinalCall,
            TRUE ~ "Other"
          ),
          ConfidenceScore = case_when(
            is.na(ConfidenceScore) ~ "Not specified",
            ConfidenceScore %in% c("High", "Medium", "Low") ~ ConfidenceScore,
            TRUE ~ "Other"
          ),
          DecisionStep = if_else(is.na(DecisionStep) | DecisionStep == "", "Unknown step", as.character(DecisionStep))
        ) %>%
        count(DecisionStep, FinalCall, ConfidenceScore, name = "Count")

      if (!nrow(plot_data)) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = "No call data available"))
      }

      plot_data <- plot_data %>%
        mutate(
          FinalCall = factor(FinalCall, levels = c(call_levels, "Other", "Unknown")),
          ConfidenceScore = factor(ConfidenceScore, levels = c("High", "Medium", "Low", "Not specified", "Other")),
          DecisionStep = factor(DecisionStep, levels = sort(unique(DecisionStep)))
        )

      chart <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = FinalCall,
          y = Count,
          fill = ConfidenceScore,
          text = paste0(
            "Decision step: ", DecisionStep,
            "<br>Final call: ", FinalCall,
            "<br>Confidence: ", ConfidenceScore,
            "<br>Samples: ", Count
          )
        )
      ) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::facet_wrap(~DecisionStep, scales = "free_y") +
        ggplot2::labs(
          x = "Final Call",
          y = "Samples",
          fill = "Confidence",
          title = "Calls and Confidence by Decision Step"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 12, face = "bold")
        )

      plotly::ggplotly(chart, tooltip = "text") %>%
        plotly::layout(legend = list(title = list(text = "Confidence")))
    })

    # Main samples table
    output$tbl_samples <- renderDT({
      df <- table_results()

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples match filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- drop_helper_columns(df)

      # Round numeric columns
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA",
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(df)
      )
      
      if (length(numeric_cols) > 0) {
        df <- df %>% mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }
      
      # Select columns to display - including Barcode, replicate counts, and decision tree columns
      available_cols <- intersect(
        c("RunID", "SampleName", "Barcode", "TestOrderLabel", "TestNumber", "FinalCall",
          "DecisionStep", "DecisionReason", "ConfidenceScore", "WellSummary", "WellAggregateConflict",
          "Wells_TNA_Positive", "Wells_DNA_Positive", "Wells_RNA_Positive",
          "ReplicatesTotal", "Replicates_Positive", "Replicates_Negative", "Replicates_Failed",
          "Cq_median_177T", "Cq_median_18S2",
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "Province", "Structure", "HealthZone",
          "BiobankMatched", "ExtractionMatched", "Flags"),
        names(df)
      )
      
      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = c('FinalCall', 'BiobankMatched', 'ExtractionMatched'))
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        filter = 'top'
      ) %>%
        formatStyle('FinalCall',
                    backgroundColor = styleEqual(
                      c('Positive', 'Positive_DNA', 'Positive_RNA', 'LatePositive', 'Negative', 'Indeterminate', 'Invalid_NoDNA', 'Invalid', 'RunInvalid', 'Control', 'Control_Fail'),
                      c('#d4edda', '#b3e0f2', '#d4b3f2', '#ffe8a1', '#f8f9fa', '#fff3cd', '#f8d7da', '#f8d7da', '#f8d7da', '#dbe9ff', '#f5c6cb')
                    )) %>%
        {
          if ("ConfidenceScore" %in% available_cols) {
            formatStyle(.,
                        'ConfidenceScore',
                        backgroundColor = styleEqual(
                          c('High', 'Medium', 'Low'),
                          c('#d4edda', '#cfe2ff', '#fff3cd')
                        ))
          } else {
            .
          }
        } %>%
        {
          if ("WellAggregateConflict" %in% available_cols) {
            formatStyle(.,
                        'WellAggregateConflict',
                        backgroundColor = styleEqual(
                          c(TRUE, FALSE),
                          c('#f8d7da', 'transparent')
                        ))
          } else {
            .
          }
        }
    })

    # Download filtered
    output$dl_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- table_results() %>% drop_helper_columns()

        # Respect current table filters/search terms by using the visible rows
        visible_rows <- input$tbl_samples_rows_all
        if (!is.null(visible_rows) && length(visible_rows)) {
          df <- df[visible_rows, , drop = FALSE]
        }

        # Use a semicolon delimiter and comma decimal mark so numbers open correctly in
        # Belgian/French versions of Excel.
        write_csv2(df, file)
      }
    )
    
  })
}
