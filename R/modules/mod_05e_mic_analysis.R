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

mod_mic_analysis_server <- function(id, filtered_base) {
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

    # === NEW: Replicate Concordance Heatmap ===
    output$heatmap_replicates <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      # Calculate % positive for each target across replicates
      replicate_cols <- grep("^Cq_[1-4]_177T$", names(df), value = TRUE)

      if (length(replicate_cols) == 0) {
        return(plotly_empty() %>% layout(title = "No replicate data available"))
      }

      df_summary <- df %>%
        head(50) %>%  # Limit to 50 samples for readability
        mutate(
          Pos_177T = rowSums(select(., matches("^Cq_[1-4]_177T$")) < 40, na.rm = TRUE),
          Pos_18S2 = rowSums(select(., matches("^Cq_[1-4]_18S2$")) < 40, na.rm = TRUE),
          Pos_RNP_DNA = rowSums(select(., matches("^Cq_[1-4]_RNAseP_DNA$")) < 40, na.rm = TRUE),
          Pos_RNP_RNA = rowSums(select(., matches("^Cq_[1-4]_RNAseP_RNA$")) < 40, na.rm = TRUE)
        ) %>%
        select(SampleName, Pos_177T, Pos_18S2, Pos_RNP_DNA, Pos_RNP_RNA)

      # Convert to matrix for heatmap
      mat <- as.matrix(df_summary[, -1])
      rownames(mat) <- df_summary$SampleName

      plot_ly(z = t(mat),
              x = rownames(mat),
              y = c("177T", "18S2", "RNP-DNA", "RNP-RNA"),
              type = "heatmap",
              colors = colorRamp(c("#ffffff", "#27ae60")),
              hovertemplate = paste0(
                "Sample: %{x}<br>",
                "Target: %{y}<br>",
                "Positive: %{z}/4<br>",
                "<extra></extra>"
              )) %>%
        layout(
          title = paste0("First 50 samples (n = ", nrow(df_summary), ")"),
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Target")
        )
    })

    # === NEW: Replicate Count Bar Chart ===
    output$bar_replicate_counts <- renderPlotly({
      df <- filtered_base() %>%
        filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      # Count positive replicates for 177T
      if (!"PosCount_177T" %in% names(df)) {
        # Calculate on the fly
        df <- df %>%
          mutate(PosCount_177T = rowSums(select(., matches("^Cq_[1-4]_177T$")) < 40, na.rm = TRUE))
      }

      df_counts <- df %>%
        count(PosCount_177T) %>%
        mutate(PosCount_177T = factor(PosCount_177T, levels = 0:4))

      plot_ly(df_counts, x = ~PosCount_177T, y = ~n,
              type = "bar",
              marker = list(color = c("#95a5a6", "#f39c12", "#27ae60", "#27ae60", "#27ae60")),
              text = ~n,
              textposition = "outside") %>%
        layout(
          title = paste0("n = ", sum(df_counts$n)),
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
