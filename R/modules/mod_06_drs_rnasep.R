# R/modules/mod_06_drs_rnasep.R
# DRS Volume vs RNAseP Analysis Module
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

mod_drs_rnasep_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "DRS vs RNAseP",
    icon = icon("chart-line"),

    div(
      class = "container-fluid",

      # ==== KPI SECTION =====================================================
      h4(class = "mb-3", icon("flask"), " DRS Volume & RNAseP Analysis"),

      # KPI Cards
      layout_column_wrap(
        width = 1/5, fixed_width = TRUE, heights_equal = "row", gap = "12px",

        value_box(
          title = "Samples with DRS Volume",
          value = textOutput(ns("kpi_samples_with_volume")),
          showcase = icon("vial"),
          theme = "primary"
        ),
        value_box(
          title = "Mean DRS Volume (μL)",
          value = textOutput(ns("kpi_mean_volume")),
          showcase = icon("droplet"),
          theme = "info"
        ),
        value_box(
          title = "Samples with RNAseP DNA",
          value = textOutput(ns("kpi_samples_rnasep_dna")),
          showcase = icon("dna"),
          theme = "success"
        ),
        value_box(
          title = "Samples with RNAseP RNA",
          value = textOutput(ns("kpi_samples_rnasep_rna")),
          showcase = icon("bacteria"),
          theme = "warning"
        ),
        value_box(
          title = "Linked Samples",
          value = textOutput(ns("kpi_linked_samples")),
          showcase = icon("link"),
          theme = "secondary"
        )
      ),

      # ==== MAIN VISUALIZATIONS =============================================

      # DRS Volume Distribution
      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          full_screen = TRUE,
          card_header("DRS Volume Distribution Over Time"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_timeline"), height = "550px")
          )
        )
      ),

      # Correlation Plots
      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          full_screen = TRUE,
          card_header("DRS Volume vs RNAseP DNA (Cq)"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_vs_rnasep_dna"), height = "550px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("DRS Volume vs RNAseP RNA (Cq)"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_vs_rnasep_rna"), height = "550px")
          )
        )
      ),

      # Combined Analysis
      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          full_screen = TRUE,
          card_header("RNAseP DNA vs RNA by DRS Volume Categories"),
          card_body_fill(
            plotly::plotlyOutput(ns("rnasep_comparison"), height = "550px")
          )
        )
      ),

      # Summary Table
      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header(
            "Summary Statistics by Volume Range",
            class = "d-flex justify-content-between align-items-center"
          ),
          card_body(
            DT::DTOutput(ns("summary_table"))
          )
        )
      ),

      # Detailed Data Table
      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header(
            "Detailed Sample Data",
            class = "d-flex justify-content-between align-items-center"
          ),
          card_body(
            DT::DTOutput(ns("detail_table"))
          )
        )
      )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

mod_drs_rnasep_server <- function(id, extractions_df, qpcr_data, filters) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # REACTIVE DATA PREPARATION
    # ========================================================================

    # Combine extraction and qPCR data
    combined_data <- reactive({
      req(extractions_df())

      ext_data <- extractions_df()

      if (is.null(ext_data) || nrow(ext_data) == 0) {
        return(tibble::tibble())
      }

      # Get qPCR data if available
      if (!is.null(qpcr_data) && !is.null(qpcr_data())) {
        qpcr <- qpcr_data()

        # Try to link qPCR data to extraction data by sample_id
        if (nrow(qpcr) > 0 && "sample_id" %in% names(ext_data)) {
          # Normalize sample IDs for matching
          ext_data <- ext_data %>%
            mutate(barcode_norm = toupper(trimws(as.character(sample_id))))

          # Prepare qPCR data with RNAseP values
          qpcr_summary <- qpcr %>%
            mutate(barcode_norm = toupper(trimws(as.character(SampleID)))) %>%
            group_by(barcode_norm) %>%
            summarise(
              rnasep_dna_cq = mean(Cq[Target == "RNAseP_DNA" & !is.na(Cq)], na.rm = TRUE),
              rnasep_rna_cq = mean(Cq[Target == "RNAseP_RNA" & !is.na(Cq)], na.rm = TRUE),
              .groups = "drop"
            )

          # Join with extraction data
          ext_data <- ext_data %>%
            left_join(qpcr_summary, by = "barcode_norm")
        }
      }

      # Add volume categories
      ext_data <- ext_data %>%
        mutate(
          volume_category = case_when(
            is.na(drs_volume_ml) ~ "No volume",
            drs_volume_ml < 0.05 ~ "< 50 μL",
            drs_volume_ml >= 0.05 & drs_volume_ml < 0.1 ~ "50-100 μL",
            drs_volume_ml >= 0.1 & drs_volume_ml < 0.15 ~ "100-150 μL",
            drs_volume_ml >= 0.15 ~ "≥ 150 μL",
            TRUE ~ "Unknown"
          ),
          volume_category = factor(
            volume_category,
            levels = c("< 50 μL", "50-100 μL", "100-150 μL", "≥ 150 μL", "No volume", "Unknown")
          )
        )

      ext_data
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_samples_with_volume <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("0")

      n <- sum(!is.na(data$drs_volume_ml), na.rm = TRUE)
      scales::comma(n)
    })

    output$kpi_mean_volume <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("—")

      mean_vol <- mean(data$drs_volume_ml, na.rm = TRUE) * 1000  # Convert mL to μL
      if (is.na(mean_vol)) return("—")

      sprintf("%.1f", mean_vol)
    })

    output$kpi_samples_rnasep_dna <- renderText({
      data <- combined_data()
      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) return("0")

      n <- sum(!is.na(data$rnasep_dna_cq) & !is.infinite(data$rnasep_dna_cq), na.rm = TRUE)
      scales::comma(n)
    })

    output$kpi_samples_rnasep_rna <- renderText({
      data <- combined_data()
      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) return("0")

      n <- sum(!is.na(data$rnasep_rna_cq) & !is.infinite(data$rnasep_rna_cq), na.rm = TRUE)
      scales::comma(n)
    })

    output$kpi_linked_samples <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("0")

      n <- sum(
        !is.na(data$drs_volume_ml) &
        ((!is.na(data$rnasep_dna_cq) & !is.infinite(data$rnasep_dna_cq)) |
         (!is.na(data$rnasep_rna_cq) & !is.infinite(data$rnasep_rna_cq))),
        na.rm = TRUE
      )
      scales::comma(n)
    })

    # ========================================================================
    # VISUALIZATIONS
    # ========================================================================

    # Volume Timeline
    output$volume_timeline <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"extraction_date" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No data available",
            xaxis = list(title = "Date"),
            yaxis = list(title = "DRS Volume (μL)")
          ))
      }

      plot_data <- data %>%
        filter(!is.na(drs_volume_ml), !is.na(extraction_date)) %>%
        arrange(extraction_date) %>%
        mutate(volume_ul = drs_volume_ml * 1000)  # Convert mL to μL for display

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No volume data available",
            xaxis = list(title = "Date"),
            yaxis = list(title = "DRS Volume (μL)")
          ))
      }

      plotly::plot_ly(plot_data, x = ~extraction_date, y = ~volume_ul, type = "scatter",
                      mode = "markers", marker = list(size = 8, opacity = 0.6),
                      text = ~paste0(
                        "Date: ", as.character(extraction_date), "<br>",
                        "Volume: ", round(volume_ul, 1), " μL<br>",
                        "Sample ID: ", as.character(sample_id)
                      ),
                      hoverinfo = "text") %>%
        plotly::layout(
          xaxis = list(title = "Extraction Date"),
          yaxis = list(title = "DRS Volume (μL)"),
          hovermode = "closest"
        )
    })

    # Volume vs RNAseP DNA
    output$volume_vs_rnasep_dna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No RNAseP DNA data available",
            xaxis = list(title = "DRS Volume (μL)"),
            yaxis = list(title = "RNAseP DNA Cq")
          ))
      }

      plot_data <- data %>%
        filter(!is.na(drs_volume_ml), !is.na(rnasep_dna_cq), !is.infinite(rnasep_dna_cq)) %>%
        mutate(volume_ul = drs_volume_ml * 1000)  # Convert mL to μL for display

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No linked data available",
            xaxis = list(title = "DRS Volume (μL)"),
            yaxis = list(title = "RNAseP DNA Cq")
          ))
      }

      plotly::plot_ly(plot_data, x = ~volume_ul, y = ~rnasep_dna_cq,
                      type = "scatter", mode = "markers",
                      marker = list(size = 10, opacity = 0.6, color = "#3498DB"),
                      text = ~paste0(
                        "Volume: ", round(volume_ul, 1), " μL<br>",
                        "RNAseP DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
                        "Sample ID: ", as.character(sample_id)
                      ),
                      hoverinfo = "text") %>%
        plotly::layout(
          xaxis = list(title = "DRS Volume (μL)"),
          yaxis = list(title = "RNAseP DNA Cq (lower = better)"),
          hovermode = "closest"
        )
    })

    # Volume vs RNAseP RNA
    output$volume_vs_rnasep_rna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No RNAseP RNA data available",
            xaxis = list(title = "DRS Volume (μL)"),
            yaxis = list(title = "RNAseP RNA Cq")
          ))
      }

      plot_data <- data %>%
        filter(!is.na(drs_volume_ml), !is.na(rnasep_rna_cq), !is.infinite(rnasep_rna_cq)) %>%
        mutate(volume_ul = drs_volume_ml * 1000)  # Convert mL to μL for display

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No linked data available",
            xaxis = list(title = "DRS Volume (μL)"),
            yaxis = list(title = "RNAseP RNA Cq")
          ))
      }

      plotly::plot_ly(plot_data, x = ~volume_ul, y = ~rnasep_rna_cq,
                      type = "scatter", mode = "markers",
                      marker = list(size = 10, opacity = 0.6, color = "#E67E22"),
                      text = ~paste0(
                        "Volume: ", round(volume_ul, 1), " μL<br>",
                        "RNAseP RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
                        "Sample ID: ", as.character(sample_id)
                      ),
                      hoverinfo = "text") %>%
        plotly::layout(
          xaxis = list(title = "DRS Volume (μL)"),
          yaxis = list(title = "RNAseP RNA Cq (lower = better)"),
          hovermode = "closest"
        )
    })

    # RNAseP DNA vs RNA comparison
    output$rnasep_comparison <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 ||
          !"rnasep_dna_cq" %in% names(data) ||
          !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No RNAseP data available",
            xaxis = list(title = "RNAseP DNA Cq"),
            yaxis = list(title = "RNAseP RNA Cq")
          ))
      }

      plot_data <- data %>%
        filter(
          !is.na(rnasep_dna_cq), !is.infinite(rnasep_dna_cq),
          !is.na(rnasep_rna_cq), !is.infinite(rnasep_rna_cq),
          !is.na(volume_category)
        ) %>%
        mutate(volume_ul = drs_volume_ml * 1000)  # Convert mL to μL for display

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = "No linked data available",
            xaxis = list(title = "RNAseP DNA Cq"),
            yaxis = list(title = "RNAseP RNA Cq")
          ))
      }

      plotly::plot_ly(plot_data, x = ~rnasep_dna_cq, y = ~rnasep_rna_cq,
                      color = ~volume_category, colors = "Set2",
                      type = "scatter", mode = "markers",
                      marker = list(size = 10, opacity = 0.7),
                      text = ~paste0(
                        "RNAseP DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
                        "RNAseP RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
                        "Volume: ", round(volume_ul, 1), " μL<br>",
                        "Category: ", as.character(volume_category), "<br>",
                        "Sample ID: ", as.character(sample_id)
                      ),
                      hoverinfo = "text") %>%
        plotly::layout(
          xaxis = list(title = "RNAseP DNA Cq"),
          yaxis = list(title = "RNAseP RNA Cq"),
          hovermode = "closest",
          legend = list(title = list(text = "Volume Category"))
        )
    })

    # ========================================================================
    # TABLES
    # ========================================================================

    # Summary table
    output$summary_table <- DT::renderDT({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      summary <- data %>%
        filter(!is.na(volume_category)) %>%
        group_by(volume_category) %>%
        summarise(
          n_samples = n(),
          mean_volume = mean(drs_volume_ml * 1000, na.rm = TRUE),  # Convert mL to μL
          sd_volume = sd(drs_volume_ml * 1000, na.rm = TRUE),  # Convert mL to μL
          mean_rnasep_dna = if ("rnasep_dna_cq" %in% names(data)) {
            mean(rnasep_dna_cq[!is.infinite(rnasep_dna_cq)], na.rm = TRUE)
          } else NA_real_,
          mean_rnasep_rna = if ("rnasep_rna_cq" %in% names(data)) {
            mean(rnasep_rna_cq[!is.infinite(rnasep_rna_cq)], na.rm = TRUE)
          } else NA_real_,
          .groups = "drop"
        ) %>%
        mutate(
          volume_category = as.character(volume_category),  # Convert factor to character
          mean_volume = round(mean_volume, 1),
          sd_volume = round(sd_volume, 1),
          mean_rnasep_dna = round(mean_rnasep_dna, 2),
          mean_rnasep_rna = round(mean_rnasep_rna, 2)
        )

      DT::datatable(
        summary,
        colnames = c(
          "Volume Category",
          "N Samples",
          "Mean Volume (μL)",
          "SD Volume",
          "Mean RNAseP DNA Cq",
          "Mean RNAseP RNA Cq"
        ),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'tp',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "table table-striped table-hover"
      ) %>%
        DT::formatStyle(
          columns = 1:6,
          fontSize = '14px'
        )
    })

    # Detail table
    output$detail_table <- DT::renderDT({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      detail_cols <- c("sample_id", "extraction_date", "drs_volume_ml", "volume_category")

      if ("rnasep_dna_cq" %in% names(data)) {
        detail_cols <- c(detail_cols, "rnasep_dna_cq")
      }

      if ("rnasep_rna_cq" %in% names(data)) {
        detail_cols <- c(detail_cols, "rnasep_rna_cq")
      }

      # Add study/province if available
      if ("biobank_study" %in% names(data)) {
        detail_cols <- c(detail_cols, "biobank_study")
      }

      if ("biobank_province" %in% names(data)) {
        detail_cols <- c(detail_cols, "biobank_province")
      }

      detail_data <- data %>%
        select(any_of(detail_cols)) %>%
        filter(!is.na(drs_volume_ml)) %>%
        mutate(
          drs_volume_ml = drs_volume_ml * 1000,  # Convert to μL for display
          # Ensure all complex types are converted to simple types
          volume_category = if ("volume_category" %in% names(.)) as.character(volume_category) else NULL,
          extraction_date = if ("extraction_date" %in% names(.)) as.character(extraction_date) else NULL,
          sample_id = if ("sample_id" %in% names(.)) as.character(sample_id) else NULL
        ) %>%
        select(-any_of(c("NULL")))  # Remove NULL columns

      # Build the data table
      dt <- DT::datatable(
        detail_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        class = "table table-striped table-hover table-sm",
        filter = "top"
      )

      # Format numeric columns only if they exist
      numeric_cols <- intersect(c("drs_volume_ml", "rnasep_dna_cq", "rnasep_rna_cq"), names(detail_data))
      if (length(numeric_cols) > 0) {
        dt <- dt %>% DT::formatRound(columns = numeric_cols, digits = 2)
      }

      dt %>%
        DT::formatStyle(
          columns = 1:ncol(detail_data),
          fontSize = '13px'
        )
    })
  })
}
