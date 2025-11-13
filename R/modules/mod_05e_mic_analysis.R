# ==============================================================================
# MODULE 4: ANALYSIS - Scatter plots and correlations
# ==============================================================================

mod_mic_analysis_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_columns(
      col_widths = c(6, 6),
      
      # Trypanozoon scatter
      card(
        card_header("Trypanozoon Detection: 18S2 vs 177T"),
        card_body(
          plotlyOutput(ns("scatter_tryp"), height = "550px"),
          class = "p-3"
        )
      ),
      
      # RNAseP quality scatter
      card(
        card_header("RNA Preservation Quality: RNAseP RNA vs DNA"),
        card_body(
          plotlyOutput(ns("scatter_rnp"), height = "550px"),
          class = "p-3"
        )
      )
    ),
    
    # Optional: Delta distributions
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("ΔCq Distribution: 18S2 - 177T"),
        card_body(
          plotlyOutput(ns("hist_delta_tryp"), height = "400px"),
          class = "p-3"
        )
      ),
      
      card(
        card_header("ΔCq Distribution: RNA Preservation"),
        card_body(
          plotlyOutput(ns("hist_delta_rp"), height = "400px"),
          class = "p-3"
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
            list(x = 5, y = 0, text = "Good", showarrow = FALSE, yshift = 10),
            list(x = 8, y = 0, text = "Moderate", showarrow = FALSE, yshift = 10)
          )
        )
    })
    
  })
}
