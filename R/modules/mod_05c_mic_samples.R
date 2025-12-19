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
      # KPIs - Simplified layout with Testing Volume and Detection Results
      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Sample Overview"),
          div(
            class = "d-flex align-items-center gap-2",
            div(
              class = "form-check form-switch m-0",
              checkboxInput(ns("use_final_result"), label = NULL, value = FALSE, width = "auto")
            ),
            tags$span(
              "Show final results only",
              class = "text-muted small"
            ),
            tags$small(
              "(consolidates retested samples)",
              class = "text-muted ms-1"
            )
          )
        ),
        card_body(
          # Section 1: Testing Volume (compact 2-row layout)
          layout_column_wrap(
            width = 1/4,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "Total Tests",
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

          # Section 2: Detection Results (streamlined)
          tags$hr(class = "my-3"),
          layout_column_wrap(
            width = 1/6,
            heights_equal = "row",
            gap = "12px",

            value_box(
              title = "TNA Positive",
              value = textOutput(ns("kpi_samples_tna")),
              showcase = icon("dna"),
              theme = "success"
            ),

            value_box(
              title = "DNA Only",
              value = textOutput(ns("kpi_samples_dna")),
              showcase = icon("circle-plus"),
              theme = "info"
            ),

            value_box(
              title = "RNA Only",
              value = textOutput(ns("kpi_samples_rna")),
              showcase = icon("wave-square"),
              theme = "info"
            ),

            value_box(
              title = "Negative",
              value = textOutput(ns("kpi_samples_negative")),
              showcase = icon("circle-minus"),
              theme = "secondary"
            ),

            value_box(
              title = "Indeterminate",
              value = textOutput(ns("kpi_samples_indeterminate")),
              showcase = icon("question-circle"),
              theme = "warning"
            ),

            value_box(
              title = "Invalid",
              value = textOutput(ns("kpi_samples_invalid")),
              showcase = icon("ban"),
              theme = "danger"
            )
          ),

          # Prevalence summary (inline text)
          div(
            class = "mt-3 text-muted small",
            tags$strong("Prevalence: "),
            "TNA+ ", textOutput(ns("kpi_samples_tna_prev"), inline = TRUE),
            " | Any positive ", textOutput(ns("kpi_samples_any_prev"), inline = TRUE)
          )
        )
      ),

      # Decision Summary - compact layout with clickable heatmap
      card(
        class = "mb-3",
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Decision Summary"),
          div(
            class = "d-flex align-items-center gap-2",
            tags$small("Click heatmap cells to filter samples", class = "text-muted"),
            conditionalPanel(
              condition = sprintf("input['%s'] !== null && input['%s'] !== ''", ns("active_filter"), ns("active_filter")),
              ns = ns,
              actionButton(ns("clear_filter"), "Clear filter", class = "btn-xs btn-outline-secondary")
            )
          )
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            gap = "16px",

            # Heatmap (larger, clickable)
            div(
              plotlyOutput(ns("decision_confidence_heatmap"), height = "360px"),
              uiOutput(ns("active_filter_display"))
            ),

            # Decision Tree Summary (compact sidebar)
            div(
              tags$h6("Decision Steps", class = "mb-2"),
              DTOutput(ns("tbl_decision_steps")),
              tags$h6("Quality Flags", class = "mt-3 mb-2"),
              DTOutput(ns("tbl_quality_metrics"))
            )
          )
        )
      ),

      # Results table - simplified with marker stats per sample
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            span("Sample Results"),
            tags$small("One row per sample. Click to view decision details.", class = "text-muted d-block")
          ),
          div(
            class = "d-flex align-items-center gap-3",
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

    # Reactive value for heatmap filter
    heatmap_filter <- reactiveVal(list(call = NULL, confidence = NULL))

    # Ensure expected columns exist so downstream formatting does not fail
    ensure_columns <- function(df, defaults) {
      for (nm in names(defaults)) {
        if (!nm %in% names(df)) {
          df[[nm]] <- defaults[[nm]]
        }
      }
      df
    }

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

    quality_metric_label <- function(conflict_col, final_call_col) {
      invalid_calls <- c("Invalid_NoDNA", "Invalid", "RunInvalid")

      case_when(
        conflict_col %in% TRUE ~ "Conflict",
        final_call_col %in% invalid_calls ~ "Invalid",
        final_call_col == "Indeterminate" ~ "Indeterminate",
        TRUE ~ "Clean"
      )
    }

    # Format marker stats: Mean ± SD (n+/total)
    format_marker_stats <- function(cq_values, n_positive, n_total) {
      if (all(is.na(cq_values)) || length(cq_values) == 0) {
        return("-")
      }
      mean_cq <- mean(cq_values, na.rm = TRUE)
      sd_cq <- sd(cq_values, na.rm = TRUE)
      if (is.na(sd_cq)) sd_cq <- 0

      sprintf("%.1f +/- %.1f (%d/%d)", mean_cq, sd_cq, n_positive, n_total)
    }

    samples_with_order <- reactive({
      df <- filtered_base()

      if (is.null(df) || !nrow(df) || !"ControlType" %in% names(df)) return(tibble())

      df %>%
        filter(ControlType == "Sample") %>%
        append_test_order(drop_helper_cols = FALSE)
    })

    # Selected results - use final result toggle
    selected_results <- reactive({
      df <- samples_with_order()

      if (!nrow(df)) return(df)

      # If "Show final results only" is enabled, get consolidated results
      if (isTRUE(input$use_final_result)) {
        # Get one row per sample - prefer the final/consolidated result
        df <- df %>%
          group_by(SampleKey) %>%
          arrange(desc(TestNumber)) %>%  # Latest test first
          slice_head(n = 1) %>%
          ungroup()
      }

      df
    })

    # Table results - one row per sample with marker stats
    table_results <- reactive({
      df <- selected_results()

      if (!nrow(df)) return(df)

      df <- ensure_columns(df, list(
        WellAggregateConflict = FALSE,
        FinalCall = NA_character_,
        ConfidenceScore = NA_character_,
        Cq_median_177T = NA_real_,
        Wells_DNA_Positive = NA_integer_,
        Cq_median_18S2 = NA_real_,
        Wells_RNA_Positive = NA_integer_
      ))

      # Add quality metric
      df <- df %>%
        mutate(QualityMetric = quality_metric_label(WellAggregateConflict, FinalCall))

      # Create marker summary columns
      df <- df %>%
        mutate(
          FinalCall = as.character(FinalCall),
          ConfidenceScore = as.character(ConfidenceScore),
          # 177T marker stats
          Marker_177T = sprintf(
            "%.1f (%s)",
            round(Cq_median_177T, 1),
            ifelse(!is.na(Wells_DNA_Positive), paste0(Wells_DNA_Positive, "+"), "?")
          ),
          # 18S2 marker stats
          Marker_18S2 = sprintf(
            "%.1f (%s)",
            round(Cq_median_18S2, 1),
            ifelse(!is.na(Wells_RNA_Positive), paste0(Wells_RNA_Positive, "+"), "?")
          )
        )

      # Apply heatmap filter if set
      filter_state <- heatmap_filter()
      if (!is.null(filter_state$call)) {
        df <- df %>% filter(FinalCall == filter_state$call)
      }
      if (!is.null(filter_state$confidence)) {
        df <- df %>% filter(ConfidenceScore == filter_state$confidence)
      }

      df
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

      df <- df %>%
        mutate(QualityMetric = quality_metric_label(WellAggregateConflict, FinalCall))

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
      quality_summary <- df %>%
        mutate(
          QualityMetric = factor(QualityMetric, levels = c("Conflict", "Invalid", "Indeterminate", "Clean"))
        ) %>%
        count(QualityMetric, name = "Count") %>%
        tidyr::complete(QualityMetric = c("Conflict", "Invalid", "Indeterminate"), fill = list(Count = 0)) %>%
        filter(QualityMetric %in% c("Conflict", "Invalid", "Indeterminate")) %>%
        mutate(
          Metric = dplyr::recode(
            QualityMetric,
            Conflict = "Samples with Conflicts",
            Invalid = "Invalid Results",
            Indeterminate = "Indeterminate Results"
          ),
          Percentage = round(100 * Count / total_samples, 1)
        ) %>%
        select(Metric, Count, Percentage)

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
      suffix <- if (isTRUE(input$use_final_result) && metrics$tested_multiple > 0) " (final)" else ""
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
          label = TileLabel,
          customdata = paste(FinalCall, ConfidenceScore, sep = "|||")
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
          title = "Click a cell to filter samples"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 11, face = "bold"),
          legend.position = "right"
        )

      plotly::ggplotly(heatmap_plot, tooltip = "text", source = "heatmap_click") %>%
        plotly::layout(margin = list(l = 60, r = 40, b = 80, t = 60)) %>%
        plotly::event_register("plotly_click")
    })

    # Handle heatmap clicks
    observeEvent(plotly::event_data("plotly_click", source = "heatmap_click"), {
      click_data <- plotly::event_data("plotly_click", source = "heatmap_click")
      if (!is.null(click_data)) {
        # Extract FinalCall (y) and ConfidenceScore (x) from click using customdata to
        # avoid factor index lookups from ggplotly
        if (!is.null(click_data$customdata)) {
          parts <- strsplit(as.character(click_data$customdata), "\\|\\|\\|", fixed = FALSE)[[1]]
          clicked_call <- dplyr::coalesce(parts[[1]], NA_character_)
          clicked_conf <- if (length(parts) >= 2) dplyr::coalesce(parts[[2]], NA_character_) else NA_character_
        } else {
          clicked_call <- click_data$y
          clicked_conf <- click_data$x
        }

        heatmap_filter(list(call = clicked_call, confidence = clicked_conf))
      }
    })

    # Clear filter button
    observeEvent(input$clear_filter, {
      heatmap_filter(list(call = NULL, confidence = NULL))
    })

    # Active filter display
    output$active_filter_display <- renderUI({
      filter_state <- heatmap_filter()
      if (is.null(filter_state$call) && is.null(filter_state$confidence)) {
        return(NULL)
      }

      filter_text <- paste0(
        "Filtered: ",
        if (!is.null(filter_state$call)) paste0("Call = ", filter_state$call) else "",
        if (!is.null(filter_state$call) && !is.null(filter_state$confidence)) " & " else "",
        if (!is.null(filter_state$confidence)) paste0("Confidence = ", filter_state$confidence) else ""
      )

      div(
        class = "alert alert-info py-1 px-2 mt-2 mb-0 d-flex align-items-center justify-content-between",
        tags$small(filter_text),
        actionButton(session$ns("clear_filter_inline"), "Clear", class = "btn-xs btn-outline-secondary ms-2")
      )
    })

    observeEvent(input$clear_filter_inline, {
      heatmap_filter(list(call = NULL, confidence = NULL))
    })

    # Hidden input for conditionalPanel
    output$active_filter <- reactive({
      filter_state <- heatmap_filter()
      if (!is.null(filter_state$call) || !is.null(filter_state$confidence)) {
        return("active")
      }
      return("")
    })
    outputOptions(output, "active_filter", suspendWhenHidden = FALSE)

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

    # Main samples table - simplified with one row per sample
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

      df <- ensure_columns(df, list(
        SampleName = NA_character_,
        FinalCall = "Unknown",
        mic_is_discordant = FALSE,
        mic_n_tests = NA_integer_
      ))

      safe_parse_date <- function(x) {
        suppressWarnings(tryCatch(lubridate::ymd(as.character(x)), error = function(e) as.Date(NA)))
      }

      safe_parse_datetime <- function(x) {
        suppressWarnings(tryCatch(lubridate::ymd_hms(as.character(x), tz = "UTC"), error = function(e) as.POSIXct(NA)))
      }

      # Create simplified display columns
      display_df <- df %>%
        mutate(
          # Format marker columns: Cq (n+)
          `177T (DNA)` = ifelse(
            is.na(Cq_median_177T),
            "-",
            sprintf("%.1f (%s+)", Cq_median_177T, ifelse(is.na(Wells_DNA_Positive), "?", Wells_DNA_Positive))
          ),
          `18S2 (RNA)` = ifelse(
            is.na(Cq_median_18S2),
            "-",
            sprintf("%.1f (%s+)", Cq_median_18S2, ifelse(is.na(Wells_RNA_Positive), "?", Wells_RNA_Positive))
          ),
          # Simplified status - combine discordance info
          Status = dplyr::case_when(
            "mic_is_discordant" %in% names(.) & mic_is_discordant == TRUE ~ paste0(FinalCall, " [DISCORDANT]"),
            "mic_n_tests" %in% names(.) & mic_n_tests > 1 ~ paste0(FinalCall, " (", mic_n_tests, "x)"),
            TRUE ~ FinalCall
          ),
          # Quality flag
          Quality = QualityMetric,
          RunDateRaw = as.character(RunDate),
          RunDateTimeRaw = as.character(RunDateTime),
          RunDateParsed = safe_parse_date(RunDateRaw),
          RunDateTimeParsed = safe_parse_datetime(RunDateTimeRaw),
          RunDateDisplay = dplyr::case_when(
            !is.na(RunDateParsed) ~ format(RunDateParsed, "%Y-%m-%d"),
            !is.na(RunDateTimeParsed) ~ format(RunDateTimeParsed, "%Y-%m-%d"),
            !is.na(RunDateRaw) & RunDateRaw != "" ~ RunDateRaw,
            !is.na(RunDateTimeRaw) & RunDateTimeRaw != "" ~ RunDateTimeRaw,
            TRUE ~ NA_character_
          )
        )

      display_df$RunDateRaw <- NULL
      display_df$RunDateTimeRaw <- NULL
      display_df$RunDateParsed <- NULL
      display_df$RunDateTimeParsed <- NULL

      # Flatten list-like columns so DataTables receives scalar values rather than objects
      display_df <- display_df %>%
        mutate(across(where(is.list), ~vapply(.x, function(val) {
          if (is.null(val) || (is.atomic(val) && length(val) == 0)) return(NA_character_)
          paste(as.character(unlist(val)), collapse = ", ")
        }, character(1)))) %>%
        # Ensure DT receives only standard vectors (no list columns)
        mutate(across(everything(), as.vector)) %>%
        as.data.frame()

      # Select simplified columns for display
      simplified_cols <- c(
        "SampleName", "Status", "177T (DNA)", "18S2 (RNA)",
        "ConfidenceScore", "Quality", "DecisionStep", "RunDateDisplay"
      )

      # Only include columns that exist
      available_cols <- intersect(simplified_cols, names(display_df))

      # Rename for cleaner display
      display_df <- display_df %>%
        select(all_of(available_cols)) %>%
        rename_with(~case_when(
          . == "ConfidenceScore" ~ "Confidence",
          . == "DecisionStep" ~ "Step",
          . == "RunDateDisplay" ~ "Run date",
          TRUE ~ .
        )) %>%
        as.data.frame(stringsAsFactors = FALSE, check.names = FALSE)

      datatable(
        display_df,
        selection = 'single',
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'lfrtip',
          lengthMenu = list(c(10, 25, 50, 100), c('10', '25', '50', '100')),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        filter = 'top'
      ) %>%
        formatStyle('Status',
                    backgroundColor = styleEqual(
                      c('Positive', 'Positive_DNA', 'Positive_RNA', 'LatePositive', 'Negative',
                        'Indeterminate', 'Invalid_NoDNA', 'Invalid', 'RunInvalid'),
                      c('#d4edda', '#b3e0f2', '#d4b3f2', '#ffe8a1', '#f8f9fa',
                        '#fff3cd', '#f8d7da', '#f8d7da', '#f8d7da')
                    )) %>%
        formatStyle('Quality',
                    backgroundColor = styleEqual(
                      c('Conflict', 'Invalid', 'Indeterminate', 'Clean'),
                      c('#f8d7da', '#f8d7da', '#fff3cd', '#d4edda')
                    )) %>%
        {
          if ("Confidence" %in% names(display_df)) {
            formatStyle(.,
                        'Confidence',
                        backgroundColor = styleEqual(
                          c('High', 'Medium', 'Low'),
                          c('#d4edda', '#cfe2ff', '#fff3cd')
                        ))
          } else {
            .
          }
        }
    })

    # Handle row selection to show decision path
    observeEvent(input$tbl_samples_rows_selected, {
      selected_row <- input$tbl_samples_rows_selected

      if (length(selected_row) == 0) return()

      # Get the full data including all columns (not just the displayed ones)
      df_full <- table_results()

      if (selected_row > nrow(df_full)) return()

      sample_data <- df_full[selected_row, , drop = FALSE]

      # Generate the decision path visualization
      decision_path_text <- tryCatch({
        visualize_decision_path(sample_data)
      }, error = function(e) {
        paste("Error generating decision path:", e$message)
      })

      # Show modal with the decision path
      showModal(modalDialog(
        title = sprintf("Decision Path: %s", sample_data$SampleName),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        tags$pre(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; font-family: 'Courier New', monospace; font-size: 12px; overflow-x: auto;",
          decision_path_text
        )
      ))
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
