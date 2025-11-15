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
          h6("Testing distribution"),
          tableOutput(ns("sample_repeat_table")),
          tags$hr(),
          h6("Final call changes across reruns"),
          div(
            class = "mb-2",
            textOutput(ns("sample_transition_caption"))
          ),
          tableOutput(ns("sample_repeat_transition_table")),
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

      sample_runs <- df %>%
        filter(ControlType == "Sample") %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_sample_id) as.character(SampleID) else NA_character_,
            if (has_sample_name) as.character(SampleName) else NA_character_,
            "Unknown sample"
          ),
          RunKey = dplyr::coalesce(as.character(RunID), "Unknown run")
        ) %>%
        distinct(SampleKey, RunKey, .keep_all = TRUE)

      if (!nrow(sample_runs)) {
        return(list(message = "No samples available after filters are applied."))
      }

      if ("FinalCall" %in% names(sample_runs)) {
        calls <- as.character(sample_runs$FinalCall)
        calls[is.na(calls) | calls == ""] <- "Missing"
        sample_runs$FinalCallClean <- calls
      } else {
        sample_runs$FinalCallClean <- rep("Missing", nrow(sample_runs))
      }

      run_order_value <- rep(NA_real_, nrow(sample_runs))

      if ("RunDateTime" %in% names(sample_runs)) {
        parsed <- suppressWarnings(as.POSIXct(sample_runs$RunDateTime, tz = "UTC"))
        if (any(!is.na(parsed))) {
          run_order_value <- as.numeric(parsed)
        }
      }

      if (all(is.na(run_order_value)) && "RunDate" %in% names(sample_runs)) {
        parsed_date <- suppressWarnings(as.POSIXct(sample_runs$RunDate, tz = "UTC"))
        if (any(!is.na(parsed_date))) {
          run_order_value <- as.numeric(parsed_date)
        }
      }

      if (all(is.na(run_order_value))) {
        run_order_value <- match(sample_runs$RunKey, sort(unique(sample_runs$RunKey)))
      }

      sample_runs$RunOrderValue <- run_order_value

      sample_sequences <- sample_runs %>%
        group_by(SampleKey) %>%
        arrange(RunOrderValue, RunKey, .by_group = TRUE) %>%
        summarise(
          TimesTested = dplyr::n(),
          FirstCall = dplyr::first(FinalCallClean),
          MostRecentCall = dplyr::last(FinalCallClean),
          EverChanged = dplyr::n_distinct(FinalCallClean) > 1,
          .groups = "drop"
        )

      sample_counts <- sample_sequences

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

      repeated_details <- sample_sequences %>%
        filter(TimesTested > 1)

      repeated_samples <- nrow(repeated_details)
      repeated_changes <- 0
      repeated_reverted <- 0
      transition_table <- NULL

      if (repeated_samples > 0) {
        repeated_changes <- sum(repeated_details$EverChanged)
        repeated_reverted <- sum(repeated_details$EverChanged &
                                    repeated_details$FirstCall == repeated_details$MostRecentCall)

        transition_table <- repeated_details %>%
          transmute(
            FirstCall,
            MostRecentCall,
            CallStatus = dplyr::case_when(
              EverChanged & FirstCall != MostRecentCall ~ "Changed",
              EverChanged ~ "Changed (reverted)",
              TRUE ~ "No change"
            )
          ) %>%
          count(FirstCall, MostRecentCall, CallStatus, name = "Number of Samples") %>%
          mutate(`Percent of Repeated Samples` = sprintf("%.1f%%", `Number of Samples` / repeated_samples * 100)) %>%
          arrange(desc(`Number of Samples`), FirstCall, MostRecentCall) %>%
          transmute(
            `First Run Call` = FirstCall,
            `Most Recent Call` = MostRecentCall,
            `Call Status` = CallStatus,
            `Number of Samples`,
            `Percent of Repeated Samples`
          )
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
        repeated_samples = repeated_samples,
        repeated_samples_changed = repeated_changes,
        repeated_samples_reverted = repeated_reverted,
        transition_table = transition_table
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

      summary$table
    },
    striped = TRUE,
    bordered = TRUE,
    hover = TRUE,
    spacing = "s",
    align = "c",
    rownames = FALSE)

    output$sample_repeat_transition_table <- renderTable({
      summary <- sample_repeat_summary()

      if (is.null(summary)) {
        return(tibble(`First Run Call` = character(), `Most Recent Call` = character(), `Call Status` = character(),
                      `Number of Samples` = integer(), `Percent of Repeated Samples` = character()))
      }

      if (!is.null(summary$message)) {
        return(tibble(Message = summary$message))
      }

      if (is.null(summary$transition_table)) {
        return(tibble(Message = "No retested samples available for transition analysis."))
      }

      summary$transition_table
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

      repeated <- summary$repeated_samples

      base_text <- paste0(
        format(summary$total_samples, big.mark = ","),
        " unique samples covering ",
        format(summary$total_instances, big.mark = ","),
        " run-sample combinations. ",
        format(repeated, big.mark = ","),
        if (repeated == 1) " sample was" else " samples were",
        " tested more than once."
      )

      if (repeated == 0) {
        return(paste0(base_text, " No samples were retested."))
      }

      changed <- summary$repeated_samples_changed
      reverted <- summary$repeated_samples_reverted
      changed_no_revert <- max(changed - reverted, 0)
      stable <- repeated - changed

      details <- c()

      if (changed > 0) {
        details <- c(details, paste0(
          format(changed, big.mark = ","),
          if (changed == 1) " retested sample changed" else " retested samples changed",
          " final call at least once"
        ))
      } else {
        details <- c(details, "None of the retested samples changed final call")
      }

      if (changed_no_revert > 0) {
        details <- c(details, paste0(
          format(changed_no_revert, big.mark = ","),
          if (changed_no_revert == 1) " sample ended" else " samples ended",
          " with a different final call"
        ))
      }

      if (reverted > 0) {
        details <- c(details, paste0(
          format(reverted, big.mark = ","),
          if (reverted == 1) " sample returned" else " samples returned",
          " to the original call after intermediate changes"
        ))
      }

      if (stable > 0) {
        details <- c(details, paste0(
          format(stable, big.mark = ","),
          if (stable == 1) " sample maintained" else " samples maintained",
          " the same call across reruns"
        ))
      }

      paste0(base_text, " ", paste0(details, collapse = "; "), ".")
    })

    output$sample_transition_caption <- renderText({
      summary <- sample_repeat_summary()

      if (is.null(summary)) {
        return("No sample data available for the current filters.")
      }

      if (!is.null(summary$message)) {
        return(summary$message)
      }

      if (is.null(summary$transition_table)) {
        return("No samples were retested under the current filters.")
      }

      changed <- summary$repeated_samples_changed
      reverted <- summary$repeated_samples_reverted
      changed_no_revert <- max(changed - reverted, 0)
      stable <- summary$repeated_samples - changed

      parts <- character()

      if (changed_no_revert > 0) {
        parts <- c(parts, paste0(
          format(changed_no_revert, big.mark = ","),
          if (changed_no_revert == 1) " sample ended" else " samples ended",
          " with a different final call than their first run."
        ))
      }

      if (reverted > 0) {
        parts <- c(parts, paste0(
          format(reverted, big.mark = ","),
          if (reverted == 1) " sample returned" else " samples returned",
          " to the original call after intermediate changes."
        ))
      }

      if (stable > 0) {
        parts <- c(parts, paste0(
          format(stable, big.mark = ","),
          if (stable == 1) " sample maintained" else " samples maintained",
          " the same call across reruns."
        ))
      }

      paste(parts, collapse = " ")
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
