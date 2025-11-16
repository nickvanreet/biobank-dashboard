# ==============================================================================
# MODULE 4: ANALYSIS - Scatter plots and correlations
# ==============================================================================

mod_mic_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML(
      "
      .mic-plot-card .card-body {
        overflow: visible;
      }

      .mic-plot-card .plotly.html-widget {
        height: auto !important;
      }

      .mic-analysis-container > *:not(style) {
        margin-bottom: 1.5rem;
      }
      "
    )),
    div(
      class = "container-fluid mic-analysis-container pb-4",

      # === SECTION 1: Detection Scatter Plots ===
      h4("Detection Analysis", class = "mt-3 mb-3"),
      layout_column_wrap(
        width = 1/2,
        gap = "16px",
        heights_equal = "row",

        # Trypanozoon scatter
        card(
          class = "mic-plot-card",
          card_header("Trypanozoon Detection: 18S2 vs 177T"),
          card_body(
            plotlyOutput(ns("scatter_tryp"), height = "550px"),
            class = "p-3"
          )
        ),

        # RNAseP quality scatter
        card(
          class = "mic-plot-card",
          card_header("RNA Preservation Quality: RNAseP RNA vs DNA"),
          card_body(
            plotlyOutput(ns("scatter_rnp"), height = "550px"),
            class = "p-3"
          )
        )
      ),

      # === SECTION 2: Cq Distributions ===
      h4("Cq Value Distributions", class = "mt-4 mb-3"),
      layout_column_wrap(
        width = 1/2,
        gap = "16px",
        heights_equal = "row",

        card(
          class = "mic-plot-card",
          card_header("Cq Distribution by Target"),
          card_body(
            plotlyOutput(ns("box_cq_by_target"), height = "450px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("Cq Distribution by Final Call"),
          card_body(
            plotlyOutput(ns("box_cq_by_call"), height = "450px"),
            class = "p-3"
          )
        )
      ),

      # === SECTION 3: Replicate Concordance ===
      h4("Replicate Concordance Analysis", class = "mt-4 mb-3"),
      layout_columns(
        col_widths = c(8, 4),
        gap = "16px",

        card(
          class = "mic-plot-card",
          card_header("Replicate Positivity Heatmap"),
          card_body(
            plotlyOutput(ns("heatmap_replicates"), height = "500px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("Positive Replicate Distribution"),
          card_body(
            plotlyOutput(ns("bar_replicate_counts"), height = "500px"),
            class = "p-3"
          )
        )
      ),

      card(
        class = "mic-plot-card",
        card_header("Sample Testing Frequency"),
        card_body(
          div(
            class = "mb-3",
            textOutput(ns("sample_repeat_caption"))
          ),
          tableOutput(ns("sample_repeat_table")),
          hr(),
          div(
            class = "mt-3 mb-2",
            textOutput(ns("sample_transition_caption"))
          ),
          tableOutput(ns("sample_transition_table")),
          class = "p-3"
        )
      ),

      # === SECTION 4: Quality Metrics ===
      h4("Quality Control Metrics", class = "mt-4 mb-3"),
      layout_column_wrap(
        width = 1/3,
        gap = "16px",
        heights_equal = "row",

        card(
          class = "mic-plot-card",
          card_header("RNA Preservation Distribution"),
          card_body(
            plotlyOutput(ns("hist_delta_rp"), height = "400px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("RNA Quality by Detection"),
          card_body(
            plotlyOutput(ns("violin_quality"), height = "400px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("QC Pass Rates by Call"),
          card_body(
            plotlyOutput(ns("bar_qc_rates"), height = "400px"),
            class = "p-3"
          )
        )
      ),

      # === SECTION 5: Clinical Decision Matrix ===
      h4("Clinical Decision Validation", class = "mt-4 mb-3"),
      layout_column_wrap(
        width = 1/2,
        gap = "16px",
        heights_equal = "row",

        card(
          class = "mic-plot-card",
          card_header("Detection Pattern vs Final Call"),
          card_body(
            plotlyOutput(ns("heatmap_decision_matrix"), height = "450px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("ΔCq Distribution: 18S2 - 177T"),
          card_body(
            plotlyOutput(ns("hist_delta_tryp"), height = "450px"),
            class = "p-3"
          )
        )
      ),

      # === SECTION 6: Temporal and Geographic ===
      h4("Trends and Geographic Analysis", class = "mt-4 mb-3"),
      layout_columns(
        col_widths = c(12),
        gap = "16px",

        card(
          class = "mic-plot-card",
          card_header("Temporal Trends: Volume and Positivity"),
          card_body(
            plotlyOutput(ns("line_temporal"), height = "400px"),
            class = "p-3"
          )
        )
      ),

      layout_column_wrap(
        width = 1/2,
        gap = "16px",
        heights_equal = "row",

        card(
          class = "mic-plot-card",
          card_header("Positivity by Province"),
          card_body(
            plotlyOutput(ns("bar_geo_positivity"), height = "450px"),
            class = "p-3"
          )
        ),

        card(
          class = "mic-plot-card",
          card_header("RNA Quality by Province"),
          card_body(
            plotlyOutput(ns("box_geo_quality"), height = "450px"),
            class = "p-3"
          )
        )
      )
    )
  )
}

mod_mic_analysis_server <- function(id, filtered_base, filtered_replicates = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # Trypanozoon scatter plot
    output$scatter_tryp <- renderPlotly({
      df <- filtered_base()
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      required_cols <- c("ControlType", "Cq_median_177T", "Cq_median_18S2", "FinalCall", "SampleName")
      missing_cols <- setdiff(required_cols, names(df))
      
      if (length(missing_cols) > 0) {
        return(plotly_empty() %>% 
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }
      
      df <- df %>%
        filter(
          ControlType == "Sample", 
          !is.na(Cq_median_177T), 
          !is.na(Cq_median_18S2)
        )
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with both 177T and 18S2 data"))
      }
      
      plot_ly(df, x = ~Cq_median_177T, y = ~Cq_median_18S2,
              color = ~FinalCall,
              colors = c(
                "Positive" = "#27ae60",
                "Positive_DNA" = "#3498db",
                "Positive_RNA" = "#9b59b6",
                "LatePositive" = "#f39c12",
                "Negative" = "#95a5a6",
                "Indeterminate" = "#f1c40f",
                "Invalid_NoDNA" = "#e74c3c"
              ),
              type = 'scatter', mode = 'markers',
              text = ~SampleName, 
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "177T Cq: %{x:.2f}<br>",
                "18S2 Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          xaxis = list(title = "177T Cq (DNA)"),
          yaxis = list(title = "18S2 Cq (RNA)"),
          legend = list(title = list(text = "Call")),
          hovermode = 'closest'
        )
    })
    
    # RNAseP quality scatter plot
    output$scatter_rnp <- renderPlotly({
      df <- filtered_base()
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      required_cols <- c("ControlType", "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA", "SampleName", "Delta_RP")
      missing_cols <- setdiff(required_cols, names(df))
      
      if (length(missing_cols) > 0) {
        return(plotly_empty() %>% 
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }
      
      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_RNAseP_DNA),
          !is.na(Cq_median_RNAseP_RNA)
        ) %>%
        mutate(
          Quality = case_when(
            is.na(Delta_RP) ~ "Unknown",
            Delta_RP <= 5 ~ "Good",
            Delta_RP <= 8 ~ "Moderate",
            TRUE ~ "Poor"
          )
        )
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with RNAseP data"))
      }
      
      plot_ly(df, x = ~Cq_median_RNAseP_DNA, y = ~Cq_median_RNAseP_RNA,
              color = ~Quality,
              colors = c("Good" = "#27ae60", "Moderate" = "#f39c12", 
                         "Poor" = "#e74c3c", "Unknown" = "#95a5a6"),
              type = 'scatter', mode = 'markers',
              text = ~paste0(SampleName, "<br>ΔCq: ", round(Delta_RP, 2)),
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "DNA Cq: %{x:.2f}<br>",
                "RNA Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          xaxis = list(title = "RNAseP-DNA Cq"),
          yaxis = list(title = "RNAseP-RNA Cq"),
          legend = list(title = list(text = "RNA Quality")),
          hovermode = 'closest'
        )
    })
    
    # Delta histograms
    output$hist_delta_tryp <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(Delta_18S2_177T))
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No delta data"))
      }
      
      plot_ly(df, x = ~Delta_18S2_177T, type = 'histogram',
              marker = list(color = '#3498db', line = list(color = 'white', width = 1))) %>%
        layout(
          xaxis = list(title = "ΔCq (18S2 - 177T)"),
          yaxis = list(title = "Count"),
          bargap = 0.1
        )
    })
    
    output$hist_delta_rp <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(Delta_RP))

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No RNA preservation data"))
      }

      plot_ly(df, x = ~Delta_RP, type = 'histogram',
              marker = list(color = '#e74c3c', line = list(color = 'white', width = 1))) %>%
        layout(
          title = paste0("n = ", nrow(df)),
          xaxis = list(title = "ΔCq (RNA - DNA)"),
          yaxis = list(title = "Count"),
          bargap = 0.1,
          shapes = list(
            list(
              type = "line",
              x0 = 5, x1 = 5,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = 'green', dash = 'dash', width = 2)
            ),
            list(
              type = "line",
              x0 = 8, x1 = 8,
              y0 = 0, y1 = 1,
              yref = "paper",
              line = list(color = 'orange', dash = 'dash', width = 2)
            )
          ),
          annotations = list(
            list(x = 5, y = 0.95, text = "Good", showarrow = FALSE, yref = "paper"),
            list(x = 6.5, y = 0.95, text = "Moderate", showarrow = FALSE, yref = "paper"),
            list(x = 9, y = 0.95, text = "Poor", showarrow = FALSE, yref = "paper")
          )
        )
    })

    # === NEW: Cq Distribution by Target ===
    output$box_cq_by_target <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      # Reshape to long format
      df_long <- df %>%
        select(SampleName,
               `177T` = Cq_median_177T,
               `18S2` = Cq_median_18S2,
               `RNAseP-DNA` = Cq_median_RNAseP_DNA,
               `RNAseP-RNA` = Cq_median_RNAseP_RNA) %>%
        tidyr::pivot_longer(cols = -SampleName, names_to = "Target", values_to = "Cq") %>%
        filter(!is.na(Cq))

      if (!nrow(df_long)) {
        return(plotly_empty() %>% layout(title = "No Cq data available"))
      }

      plot_ly(df_long, x = ~Target, y = ~Cq, color = ~Target,
              type = "box",
              colors = c("177T" = "#3498db", "18S2" = "#9b59b6",
                        "RNAseP-DNA" = "#27ae60", "RNAseP-RNA" = "#e74c3c")) %>%
        layout(
          title = paste0("n = ", length(unique(df_long$SampleName)), " samples"),
          xaxis = list(title = ""),
          yaxis = list(title = "Cq Value"),
          showlegend = FALSE
        )
    })

    # === NEW: Cq Distribution by Call ===
    output$box_cq_by_call <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(FinalCall))

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      # Focus on 177T for detection calls
      df_plot <- df %>%
        filter(!is.na(Cq_median_177T)) %>%
        mutate(CallSimplified = case_when(
          grepl("Positive", FinalCall) ~ "Positive",
          FinalCall == "Negative" ~ "Negative",
          FinalCall == "Indeterminate" ~ "Indeterminate",
          TRUE ~ "Other"
        ))

      if (!nrow(df_plot)) {
        return(plotly_empty() %>% layout(title = "No 177T Cq data"))
      }

      plot_ly(df_plot, x = ~CallSimplified, y = ~Cq_median_177T,
              color = ~CallSimplified,
              type = "box",
              colors = c("Positive" = "#27ae60", "Negative" = "#95a5a6",
                        "Indeterminate" = "#f39c12", "Other" = "#e74c3c")) %>%
        layout(
          title = paste0("177T Cq by Call (n = ", nrow(df_plot), ")"),
          xaxis = list(title = "Final Call"),
          yaxis = list(title = "177T Cq Value"),
          showlegend = FALSE
        )
    })

    # Helper: Sample repeat frequency summary ---------------------------------
    sample_repeat_summary <- reactive({
      df <- filtered_base()

      if (is.null(df) || !nrow(df)) {
        return(NULL)
      }

      if (!"ControlType" %in% names(df)) {
        return(list(message = "Control information is unavailable for the current selection."))
      }

      has_sample_id <- "SampleID" %in% names(df)
      has_sample_name <- "SampleName" %in% names(df)

      if (!has_sample_id && !has_sample_name) {
        return(list(message = "No sample identifiers available to summarise repeats."))
      }

      if (!"RunID" %in% names(df)) {
        return(list(message = "Run identifiers are missing, so repeat testing cannot be calculated."))
      }

      sample_base <- df %>%
        filter(ControlType == "Sample") %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_sample_id) as.character(SampleID) else NA_character_,
            if (has_sample_name) as.character(SampleName) else NA_character_,
            "Unknown sample"
          ),
          RunKey = dplyr::coalesce(as.character(RunID), "Unknown run")
        )

      sample_runs <- sample_base %>%
        distinct(SampleKey, RunKey)

      if (!nrow(sample_runs)) {
        return(list(message = "No samples available after filters are applied."))
      }

      sample_counts <- sample_runs %>%
        count(SampleKey, name = "TimesTested")

      if (!nrow(sample_counts)) {
        return(list(message = "Unable to determine repeat testing counts."))
      }

      distribution <- sample_counts %>%
        count(TimesTested, name = "NumberOfSamples") %>%
        arrange(TimesTested) %>%
        mutate(
          Percent = if (sum(NumberOfSamples) > 0) {
            sprintf("%.1f%%", NumberOfSamples / sum(NumberOfSamples) * 100)
          } else {
            "0.0%"
          }
        )

      repeated_keys <- sample_counts %>%
        filter(TimesTested > 1) %>%
        pull(SampleKey)

      transition_info <- list(
        table = tibble(),
        total_retests = length(repeated_keys),
        total_pairs = 0,
        changed_pairs = 0,
        change_percent = 0,
        missing_pairs = 0,
        top_change = NULL
      )

      if (length(repeated_keys)) {
        sample_history <- sample_base %>%
          filter(SampleKey %in% repeated_keys) %>%
          distinct(SampleKey, RunKey, .keep_all = TRUE) %>%
          mutate(
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
          mutate(TestNumber = dplyr::row_number()) %>%
          ungroup()

        retest_pairs <- sample_history %>%
          filter(TestNumber <= 2) %>%
          group_by(SampleKey) %>%
          filter(n() == 2) %>%
          ungroup()

        if (nrow(retest_pairs)) {
          retest_wide <- retest_pairs %>%
            select(SampleKey, TestNumber, FinalCall) %>%
            tidyr::pivot_wider(
              names_from = TestNumber,
              values_from = FinalCall,
              names_prefix = "Test",
              values_fn = list(FinalCall = dplyr::first),
              values_fill = list(FinalCall = NA_character_)
            )

          valid_pairs <- retest_wide %>%
            filter(!is.na(Test1) & !is.na(Test2))

          changed_pairs <- valid_pairs %>%
            filter(Test1 != Test2)

          transition_raw <- retest_wide %>%
            transmute(
              `Primary Call` = tidyr::replace_na(Test1, "No Result"),
              `Secondary Call` = tidyr::replace_na(Test2, "No Result")
            ) %>%
            count(`Primary Call`, `Secondary Call`, name = "Samples") %>%
            arrange(desc(Samples))

          transition_table <- transition_raw %>%
            mutate(
              `Percent of Retests` = if (sum(Samples) > 0) {
                sprintf("%.1f%%", Samples / sum(Samples) * 100)
              } else {
                "0.0%"
              }
            ) %>%
            transmute(
              `Primary Call` = `Primary Call`,
              `Secondary Call` = `Secondary Call`,
              `Number of Samples` = Samples,
              `Percent of Retests`
            )

          top_change_row <- transition_raw %>%
            filter(`Primary Call` != `Secondary Call`) %>%
            slice_head(n = 1)

          top_change <- if (nrow(top_change_row)) {
            list(
              primary = top_change_row$`Primary Call`[1],
              secondary = top_change_row$`Secondary Call`[1],
              samples = top_change_row$Samples[1]
            )
          } else {
            NULL
          }

          transition_info <- list(
            table = transition_table,
            total_retests = nrow(retest_wide),
            total_pairs = nrow(valid_pairs),
            changed_pairs = nrow(changed_pairs),
            change_percent = if (nrow(valid_pairs)) {
              round(100 * nrow(changed_pairs) / nrow(valid_pairs), 1)
            } else {
              0
            },
            missing_pairs = nrow(retest_wide) - nrow(valid_pairs),
            top_change = top_change
          )
        }
      }

      list(
        table = distribution %>%
          transmute(
            `Times Tested` = TimesTested,
            `Number of Samples` = NumberOfSamples,
            `Percent of Samples` = Percent
          ),
        total_samples = nrow(sample_counts),
        total_instances = nrow(sample_runs),
        repeated_samples = sum(sample_counts$TimesTested > 1),
        retest_rate = if (nrow(sample_counts)) {
          mean(sample_counts$TimesTested > 1)
        } else {
          NA_real_
        },
        transitions = transition_info
      )
    })

    output$sample_repeat_table <- renderTable({
      summary <- sample_repeat_summary()

      if (is.null(summary)) {
        return(tibble(`Times Tested` = integer(), `Number of Samples` = integer(), `Percent of Samples` = character()))
      }

      if (!is.null(summary$message)) {
        return(tibble(Message = summary$message))
      }

      table_data <- summary$table

      if (is.null(table_data)) {
        return(tibble(`Times Tested` = integer(), `Number of Samples` = integer(), `Percent of Samples` = character()))
      }

      table_data
    },
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    spacing = "s",
    align = "c",
    rownames = FALSE)

    output$sample_repeat_caption <- renderText({
      summary <- sample_repeat_summary()

      if (is.null(summary)) {
        return("No sample data available for the current filters.")
      }

      if (!is.null(summary$message)) {
        return(summary$message)
      }

      total_samples <- summary$total_samples
      total_instances <- summary$total_instances
      repeated <- summary$repeated_samples
      rate <- summary$retest_rate

      if (is.null(total_samples) || !length(total_samples)) {
        total_samples <- 0L
      }

      if (is.null(total_instances) || !length(total_instances)) {
        total_instances <- 0L
      }

      if (is.null(repeated) || !length(repeated)) {
        repeated <- 0L
      }

      rate_available <- !is.null(rate) && length(rate) == 1 && !is.na(rate)

      repeated_text <- if (!is.na(repeated) && repeated == 1) {
        " sample was"
      } else {
        " samples were"
      }

      retest_text <- if (rate_available && total_samples > 0 && repeated >= 0) {
        paste0(
          " Retest rate: ",
          scales::percent(rate, accuracy = 0.1),
          " (",
          format(repeated, big.mark = ","),
          " of ",
          format(total_samples, big.mark = ","),
          ")."
        )
      } else {
        ""
      }

      paste0(
        format(total_samples, big.mark = ","),
        " unique samples covering ",
        format(total_instances, big.mark = ","),
        " run-sample combinations. ",
        format(repeated, big.mark = ","),
        repeated_text,
        " tested more than once.",
        retest_text
      )
    })

      output$sample_transition_table <- renderTable({
        summary <- sample_repeat_summary()

        if (is.null(summary)) {
          return(tibble(`Primary Call` = character(), `Secondary Call` = character(), `Number of Samples` = integer(), `Percent of Retests` = character()))
        }

        if (!is.null(summary$message)) {
          return(tibble(Message = summary$message))
        }

        transitions <- summary$transitions
        table_data <- if (is.null(transitions)) NULL else transitions$table

        if (is.null(table_data) || !nrow(table_data)) {
          return(tibble(Message = "No paired primary/secondary results available."))
        }

        table_data
      },
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    spacing = "s",
    align = "c",
    rownames = FALSE)

    output$sample_transition_caption <- renderText({
      summary <- sample_repeat_summary()

      if (is.null(summary)) {
        return("No sample data available for the current filters.")
      }

      if (!is.null(summary$message)) {
        return(summary$message)
      }

        transitions <- summary$transitions

        if (is.null(transitions)) {
          return("No samples required both a primary and secondary MIC run within the filtered data.")
        }

        total_retests <- transitions$total_retests
        total_pairs <- transitions$total_pairs
        changed_pairs <- transitions$changed_pairs

        if (is.null(total_retests) || !length(total_retests)) {
          total_retests <- 0L
        }

        if (is.null(total_pairs) || !length(total_pairs)) {
          total_pairs <- 0L
        }

        if (is.null(changed_pairs) || !length(changed_pairs)) {
          changed_pairs <- 0L
        }

        if (!length(transitions$top_change)) {
          transitions$top_change <- NULL
        }

        if (total_retests == 0) {
          return("No samples required both a primary and secondary MIC run within the filtered data.")
        }

        base_text <- paste0(
          format(total_retests, big.mark = ","),
          " retested samples had both primary and secondary results."
        )

        if (total_pairs == 0) {
          return(paste(base_text, "However, Final Call information was missing for comparison."))
        }

        change_text <- paste0(
          format(changed_pairs, big.mark = ","),
          " (",
          scales::percent(
            if (total_pairs > 0) changed_pairs / total_pairs else 0,
            accuracy = 0.1
          ),
          ") changed their Final Call between the first two runs."
        )

        top_change_text <- NULL
        if (!is.null(transitions$top_change)) {
          tc <- transitions$top_change
          top_change_text <- paste0(
            " Most common change: ",
            tc$primary,
          " → ",
          tc$secondary,
          " (",
          format(tc$samples, big.mark = ","),
          " samples)."
        )
      }

      paste0(base_text, " ", change_text, if (!is.null(top_change_text)) top_change_text else "")
    })

    # === NEW: Replicate Concordance Heatmap ===
    output$heatmap_replicates <- renderPlotly({
      replicates <- if (is.null(filtered_replicates)) tibble() else filtered_replicates()

      if (!nrow(replicates)) {
        return(plotly_empty() %>% layout(title = "No replicate data available"))
      }

      target_map <- c(
        "177T" = "177T",
        "18S2" = "18S2",
        "RNAseP_DNA" = "RNAseP-DNA",
        "RNAseP_RNA" = "RNAseP-RNA"
      )

      summary_df <- replicates %>%
        filter(ControlType == "Sample", Target %in% names(target_map)) %>%
        mutate(TargetLabel = target_map[Target]) %>%
        group_by(SampleName, TargetLabel) %>%
        summarise(
          TotalReps = sum(!is.na(Cq)),
          PositiveReps = sum(Call == "Positive", na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(TotalReps > 0)

      if (!nrow(summary_df)) {
        return(plotly_empty() %>% layout(title = "No replicate data available"))
      }

      sample_levels <- summary_df %>%
        group_by(SampleName) %>%
        summarise(
          TotalPositive = sum(PositiveReps, na.rm = TRUE),
          TotalReps = sum(TotalReps, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(TotalPositive), desc(TotalReps), SampleName) %>%
        slice_head(n = 50) %>%
        pull(SampleName)

      if (!length(sample_levels)) {
        return(plotly_empty() %>% layout(title = "No replicate data available"))
      }

      target_levels <- unname(target_map[c("177T", "18S2", "RNAseP_DNA", "RNAseP_RNA")])

      prop_mat <- matrix(NA_real_,
                         nrow = length(target_levels),
                         ncol = length(sample_levels),
                         dimnames = list(target_levels, sample_levels))

      text_mat <- matrix("",
                         nrow = length(target_levels),
                         ncol = length(sample_levels),
                         dimnames = list(target_levels, sample_levels))

      summary_filtered <- summary_df %>%
        filter(SampleName %in% sample_levels) %>%
        mutate(
          SampleName = factor(SampleName, levels = sample_levels),
          TargetLabel = factor(TargetLabel, levels = target_levels)
        ) %>%
        arrange(TargetLabel, SampleName)

      if (nrow(summary_filtered)) {
        for (i in seq_len(nrow(summary_filtered))) {
          row <- summary_filtered[i, ]
          target <- as.character(row$TargetLabel)
          sample <- as.character(row$SampleName)
          pos <- row$PositiveReps
          total <- row$TotalReps

          if (is.na(pos)) pos <- 0

          if (!is.na(total) && total > 0) {
            prop <- pos / total
            prop_mat[target, sample] <- prop
            percent_lbl <- paste0(round(prop * 100), "%")
            text_mat[target, sample] <- paste0(
              "Sample: ", sample, "<br>",
              "Target: ", target, "<br>",
              "Positive replicates: ", pos, "/", total, "<br>",
              "Percent positive: ", percent_lbl
            )
          } else {
            text_mat[target, sample] <- paste0(
              "Sample: ", sample, "<br>",
              "Target: ", target, "<br>",
              "No valid replicates"
            )
          }
        }
      }

      for (target in target_levels) {
        for (sample in sample_levels) {
          if (text_mat[target, sample] == "") {
            text_mat[target, sample] <- paste0(
              "Sample: ", sample, "<br>",
              "Target: ", target, "<br>",
              "No replicate data"
            )
          }
        }
      }

      plot_ly(
        x = sample_levels,
        y = target_levels,
        z = prop_mat,
        type = "heatmap",
        colorscale = list(c(0, "#f5f7fa"), c(1, "#27ae60")),
        zmin = 0,
        zmax = 1,
        text = text_mat,
        hoverinfo = "text",
        colorbar = list(title = "% Positive", tickformat = ".0%")
      ) %>%
        layout(
          title = paste0("Top ", length(sample_levels), " samples by replicate positivity"),
          xaxis = list(title = "Sample", tickangle = -45),
          yaxis = list(title = "Target")
        )
    })

    # === NEW: Replicate Count Bar Chart ===
    output$bar_replicate_counts <- renderPlotly({
      replicates <- if (is.null(filtered_replicates)) tibble() else filtered_replicates()

      if (!nrow(replicates)) {
        return(plotly_empty() %>% layout(title = "No replicate data available"))
      }

      rep_counts <- replicates %>%
        filter(ControlType == "Sample", Target == "177T", !is.na(Cq)) %>%
        group_by(SampleName) %>%
        summarise(
          TotalReps = sum(!is.na(Cq)),
          PositiveReps = sum(Call == "Positive", na.rm = TRUE),
          .groups = "drop"
        )

      if (!nrow(rep_counts)) {
        return(plotly_empty() %>% layout(title = "No 177T replicate data available"))
      }

      max_reps <- max(rep_counts$TotalReps, na.rm = TRUE)
      if (!is.finite(max_reps)) {
        max_reps <- 4
      }
      max_reps <- max(0, as.integer(ceiling(max(max_reps, 4))))

      rep_distribution <- rep_counts %>%
        count(PositiveReps, name = "SampleCount") %>%
        tidyr::complete(PositiveReps = 0:max_reps, fill = list(SampleCount = 0)) %>%
        mutate(
          PositiveReps = factor(PositiveReps, levels = 0:max_reps),
          PositiveNumeric = as.numeric(as.character(PositiveReps)),
          HoverLabel = paste0(
            SampleCount, " sample", if_else(SampleCount == 1, "", "s"),
            " with ", PositiveNumeric, " positive replicate",
            if_else(PositiveNumeric == 1, "", "s")
          )
        )

      marker_colors <- rep("#27ae60", nrow(rep_distribution))
      marker_colors[rep_distribution$PositiveNumeric == 0] <- "#95a5a6"
      marker_colors[rep_distribution$PositiveNumeric == 1] <- "#f39c12"

      plot_ly(
        rep_distribution,
        x = ~PositiveReps,
        y = ~SampleCount,
        type = "bar",
        marker = list(color = marker_colors),
        text = ~SampleCount,
        textposition = "outside",
        customdata = ~HoverLabel,
        hovertemplate = "%{customdata}<extra></extra>"
      ) %>%
        layout(
          title = paste0("Samples analysed: ", sum(rep_distribution$SampleCount)),
          xaxis = list(title = "# Positive Replicates (177T)"),
          yaxis = list(title = "Sample Count"),
          showlegend = FALSE
        )
    })

    # === NEW: Violin Plot - RNA Quality by Detection ===
    output$violin_quality <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(Delta_RP), !is.na(FinalCall))

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      df_plot <- df %>%
        mutate(Detection = case_when(
          grepl("Positive", FinalCall) ~ "Positive",
          FinalCall == "Negative" ~ "Negative",
          TRUE ~ "Other"
        )) %>%
        filter(Detection %in% c("Positive", "Negative"))

      plot_ly(df_plot, x = ~Detection, y = ~Delta_RP,
              split = ~Detection,
              type = 'violin',
              box = list(visible = TRUE),
              meanline = list(visible = TRUE),
              colors = c("Positive" = "#27ae60", "Negative" = "#95a5a6")) %>%
        layout(
          title = paste0("n = ", nrow(df_plot)),
          xaxis = list(title = "Detection Status"),
          yaxis = list(title = "ΔCq RNA Preservation"),
          showlegend = FALSE,
          shapes = list(
            list(type = "line", x0 = -0.5, x1 = 1.5, y0 = 5, y1 = 5,
                 line = list(color = 'green', dash = 'dash')),
            list(type = "line", x0 = -0.5, x1 = 1.5, y0 = 8, y1 = 8,
                 line = list(color = 'orange', dash = 'dash'))
          )
        )
    })

    # === NEW: QC Pass Rates ===
    output$bar_qc_rates <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(FinalCall))

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      df_qc <- df %>%
        mutate(
          QC_Status = case_when(
            is.na(Delta_RP) ~ "Unknown",
            Delta_RP <= 5 ~ "Good",
            Delta_RP <= 8 ~ "Moderate",
            TRUE ~ "Poor"
          ),
          CallSimplified = case_when(
            grepl("Positive", FinalCall) ~ "Positive",
            FinalCall == "Negative" ~ "Negative",
            TRUE ~ "Other"
          )
        ) %>%
        count(CallSimplified, QC_Status) %>%
        group_by(CallSimplified) %>%
        mutate(pct = n / sum(n) * 100)

      plot_ly(df_qc, x = ~CallSimplified, y = ~pct, color = ~QC_Status,
              type = "bar",
              colors = c("Good" = "#27ae60", "Moderate" = "#f39c12",
                        "Poor" = "#e74c3c", "Unknown" = "#95a5a6"),
              text = ~paste0(round(pct, 1), "%"),
              textposition = "inside") %>%
        layout(
          title = paste0("n = ", sum(df_qc$n)),
          xaxis = list(title = "Final Call"),
          yaxis = list(title = "Percentage"),
          barmode = "stack",
          legend = list(title = list(text = "RNA Quality"))
        )
    })

    # === NEW: Clinical Decision Matrix ===
    output$heatmap_decision_matrix <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(FinalCall))

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      df_matrix <- df %>%
        mutate(
          Pattern = case_when(
            Call_177T == "Positive" & Call_18S2 == "Positive" ~ "Both+",
            Call_177T == "Positive" & Call_18S2 != "Positive" ~ "DNA only",
            Call_177T != "Positive" & Call_18S2 == "Positive" ~ "RNA only",
            TRUE ~ "Both-"
          ),
          CallSimplified = case_when(
            grepl("Positive", FinalCall) ~ FinalCall,
            TRUE ~ FinalCall
          )
        ) %>%
        count(Pattern, CallSimplified) %>%
        tidyr::pivot_wider(names_from = CallSimplified, values_from = n, values_fill = 0)

      mat <- as.matrix(df_matrix[, -1])
      rownames(mat) <- df_matrix$Pattern

      plot_ly(z = mat,
              x = colnames(mat),
              y = rownames(mat),
              type = "heatmap",
              colorscale = "Greens",
              text = mat,
              texttemplate = "%{text}",
              hovertemplate = paste0(
                "Pattern: %{y}<br>",
                "Call: %{x}<br>",
                "Count: %{z}<br>",
                "<extra></extra>"
              )) %>%
        layout(
          title = paste0("n = ", sum(mat)),
          xaxis = list(title = "Final Call", tickangle = -45),
          yaxis = list(title = "Detection Pattern")
        )
    })

    # === NEW: Temporal Trends ===
    output$line_temporal <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No temporal data available"))
      }

      plot_dates <- NULL

      if ("SampleDate" %in% names(df)) {
        plot_dates <- suppressWarnings(as.Date(df$SampleDate))
      } else if ("Date" %in% names(df)) {
        plot_dates <- suppressWarnings(as.Date(df$Date))
      } else if ("RunDate" %in% names(df)) {
        plot_dates <- suppressWarnings(as.Date(df$RunDate))
      }

      if (is.null(plot_dates)) {
        return(plotly_empty() %>% layout(title = "No temporal data available"))
      }

      df_temporal <- df %>%
        mutate(PlotDate = plot_dates) %>%
        filter(!is.na(PlotDate)) %>%
        mutate(Week = lubridate::floor_date(PlotDate, "week")) %>%
        group_by(Week) %>%
        summarise(
          Volume = n(),
          Positives = sum(grepl("Positive", FinalCall), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(PositivityRate = Positives / Volume * 100)

      if (!nrow(df_temporal)) {
        return(plotly_empty() %>% layout(title = "No temporal data"))
      }

      # Create dual-axis plot
      fig <- plot_ly(df_temporal, x = ~Week)

      fig <- fig %>%
        add_bars(y = ~Volume, name = "Sample Volume",
                marker = list(color = '#3498db'), yaxis = "y1")

      fig <- fig %>%
        add_lines(y = ~PositivityRate, name = "Positivity %",
                 line = list(color = '#e74c3c', width = 3), yaxis = "y2")

      fig <- fig %>%
        layout(
          title = paste0("n = ", sum(df_temporal$Volume), " samples"),
          xaxis = list(title = "Week"),
          yaxis = list(title = "Sample Volume", side = "left"),
          yaxis2 = list(title = "Positivity Rate (%)",
                       overlaying = "y", side = "right"),
          legend = list(x = 0.1, y = 0.9)
        )

      fig
    })

    # === NEW: Geographic - Positivity by Province ===
    output$bar_geo_positivity <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample")

      if (!nrow(df) || !"Province" %in% names(df)) {
        return(plotly_empty() %>% layout(title = "No geographic data available"))
      }

      df_geo <- df %>%
        filter(!is.na(Province), !is.na(FinalCall)) %>%
        mutate(CallSimplified = case_when(
          grepl("Positive", FinalCall) ~ "Positive",
          FinalCall == "Negative" ~ "Negative",
          TRUE ~ "Other"
        )) %>%
        count(Province, CallSimplified) %>%
        group_by(Province) %>%
        mutate(pct = n / sum(n) * 100)

      if (!nrow(df_geo)) {
        return(plotly_empty() %>% layout(title = "No geographic data"))
      }

      plot_ly(df_geo, x = ~Province, y = ~n, color = ~CallSimplified,
              type = "bar",
              colors = c("Positive" = "#27ae60", "Negative" = "#95a5a6", "Other" = "#f39c12"),
              text = ~paste0(n, " (", round(pct, 1), "%)"),
              textposition = "inside") %>%
        layout(
          title = paste0("n = ", sum(df_geo$n)),
          xaxis = list(title = "Province"),
          yaxis = list(title = "Sample Count"),
          barmode = "stack"
        )
    })

    # === NEW: Geographic - RNA Quality by Province ===
    output$box_geo_quality <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample", !is.na(Delta_RP))

      if (!nrow(df) || !"Province" %in% names(df)) {
        return(plotly_empty() %>% layout(title = "No geographic data available"))
      }

      df_geo <- df %>%
        filter(!is.na(Province))

      if (!nrow(df_geo)) {
        return(plotly_empty() %>% layout(title = "No geographic quality data"))
      }

      plot_ly(df_geo, x = ~Province, y = ~Delta_RP,
              type = "box",
              color = ~Province) %>%
        layout(
          title = paste0("n = ", nrow(df_geo)),
          xaxis = list(title = "Province"),
          yaxis = list(title = "ΔCq RNA Preservation"),
          showlegend = FALSE,
          shapes = list(
            list(type = "line", x0 = -0.5, x1 = 10, y0 = 5, y1 = 5,
                 line = list(color = 'green', dash = 'dash')),
            list(type = "line", x0 = -0.5, x1 = 10, y0 = 8, y1 = 8,
                 line = list(color = 'orange', dash = 'dash'))
          )
        )
    })

  })
}
