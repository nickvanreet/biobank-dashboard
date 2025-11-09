# R/modules/mod_01_data_quality.R
# Data Quality Module - Clean, self-contained module example
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Data Quality Module UI
#' @param id Module namespace ID
#' @export
mod_data_quality_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Data Quality",
    icon = safe_icon("shield-check-fill"),
    
    # Summary cards
    div(
      class = "container-fluid",
      
      # KPI Value Boxes
      layout_columns(
        fill = FALSE,
        col_widths = rep(2, 6),
        
        value_box(
          title = "Total Rows",
          value = textOutput(ns("total_rows")),
          showcase = safe_icon("database"),
          theme = "primary"
        ),
        
        value_box(
          title = "Valid Rows",
          value = textOutput(ns("valid_rows")),
          showcase = safe_icon("check-circle-fill"),
          theme = "success"
        ),
        
        value_box(
          title = "Missing Barcode",
          value = textOutput(ns("missing_barcode")),
          showcase = safe_icon("exclamation-triangle-fill"),
          theme = "warning"
        ),
        
        value_box(
          title = "Missing Lab ID",
          value = textOutput(ns("missing_labid")),
          showcase = safe_icon("exclamation-triangle-fill"),
          theme = "warning"
        ),
        
        value_box(
          title = "Duplicates",
          value = textOutput(ns("duplicates")),
          showcase = safe_icon("copy"),
          theme = "danger"
        ),
        
        value_box(
          title = "Completeness",
          value = textOutput(ns("completeness")),
          showcase = safe_icon("chart-bar"),
          theme = "info"
        )
      ),
      
      # Detailed analyses
      layout_columns(
        col_widths = c(6, 6),
        
        # Duplicates analysis
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Duplicate Records"),
            download_button_ui(ns("download_duplicates"))
          ),
          card_body(
            DTOutput(ns("duplicates_table"))
          )
        ),
        
        # Conflicts analysis
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Barcode Conflicts"),
            download_button_ui(ns("download_conflicts"))
          ),
          card_body(
            DTOutput(ns("conflicts_table"))
          )
        )
      ),
      
      # Column completeness
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          span("Column Completeness Analysis"),
          download_button_ui(ns("download_completeness"))
        ),
        card_body(
          DTOutput(ns("completeness_table"))
        )
      ),
      
      # Quality flags distribution
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          card_header("Quality Flag Distribution"),
          card_body(
            plotlyOutput(ns("quality_flags_plot"), height = "350px")
          )
        ),
        
        card(
          card_header("Data Entry Timeline"),
          card_body(
            plotlyOutput(ns("entry_timeline_plot"), height = "350px")
          )
        )
      )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' Data Quality Module Server
#' @param id Module namespace ID
#' @param raw_data Reactive expression containing raw data
#' @param clean_data Reactive expression containing cleaned data
#' @param quality_report Reactive expression containing quality analysis
#' @export
mod_data_quality_server <- function(id, raw_data, clean_data, quality_report) {
  
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # REACTIVE CALCULATIONS
    # ========================================================================
    
    # Quality metrics
    quality_metrics <- reactive({
      req(quality_report())
      report <- quality_report()
      
      list(
        total_rows = report$summary$rows_raw,
        valid_rows = report$summary$rows_clean,
        dropped_rows = report$summary$rows_dropped,
        drop_rate = report$summary$drop_rate,
        
        missing_barcode = report$missing_barcode,
        missing_labid = report$missing_labid,
        
        duplicate_count = nrow(report$duplicates),
        conflict_count = nrow(report$barcode_conflicts),
        
        avg_completeness = mean(report$completeness$percent_complete, na.rm = TRUE)
      )
    })
    
    # ========================================================================
    # OUTPUTS - KPI BOXES
    # ========================================================================
    
    output$total_rows <- renderText({
      metrics <- quality_metrics()
      scales::comma(metrics$total_rows)
    })
    
    output$valid_rows <- renderText({
      metrics <- quality_metrics()
      sprintf("%s (%.1f%%)", 
              scales::comma(metrics$valid_rows),
              100 - metrics$drop_rate)
    })
    
    output$missing_barcode <- renderText({
      metrics <- quality_metrics()
      scales::comma(metrics$missing_barcode)
    })
    
    output$missing_labid <- renderText({
      metrics <- quality_metrics()
      scales::comma(metrics$missing_labid)
    })
    
    output$duplicates <- renderText({
      metrics <- quality_metrics()
      scales::comma(metrics$duplicate_count)
    })
    
    output$completeness <- renderText({
      metrics <- quality_metrics()
      sprintf("%.1f%%", metrics$avg_completeness)
    })
    
    # ========================================================================
    # OUTPUTS - TABLES
    # ========================================================================
    
    output$duplicates_table <- renderDT({
      req(quality_report())
      report <- quality_report()
      
      if (nrow(report$duplicates) == 0) {
        datatable(
          data.frame(Message = "No duplicate records found"),
          options = list(dom = 't', paging = FALSE)
        )
      } else {
        report$duplicates %>%
          select(barcode, lab_id, date_sample, province, health_zone) %>%
          datatable(
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'frtip'
            ),
            class = "table-sm"
          )
      }
    })
    
    output$conflicts_table <- renderDT({
      req(quality_report())
      report <- quality_report()
      
      if (nrow(report$barcode_conflicts) == 0) {
        datatable(
          data.frame(Message = "No barcode conflicts found"),
          options = list(dom = 't', paging = FALSE)
        )
      } else {
        report$barcode_conflicts %>%
          datatable(
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'frtip'
            ),
            class = "table-sm"
          )
      }
    })
    
    output$completeness_table <- renderDT({
      req(quality_report())
      report <- quality_report()
      
      report$completeness %>%
        mutate(
          percent_complete = round(percent_complete, 1),
          status = case_when(
            percent_complete >= 90 ~ "Good",
            percent_complete >= 70 ~ "Fair",
            percent_complete >= 50 ~ "Poor",
            TRUE ~ "Critical"
          )
        ) %>%
        datatable(
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'frtip'
          ),
          class = "table-sm"
        ) %>%
        formatStyle(
          "percent_complete",
          background = styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          "status",
          color = styleEqual(
            c("Good", "Fair", "Poor", "Critical"),
            c("#27AE60", "#F39C12", "#E67E22", "#E74C3C")
          ),
          fontWeight = "bold"
        )
    })
    
    # ========================================================================
    # OUTPUTS - PLOTS
    # ========================================================================
    
    output$quality_flags_plot <- renderPlotly({
      req(quality_report())
      report <- quality_report()
      
      if (nrow(report$quality_flags) == 0) {
        plotly::plotly_empty()
      } else {
        p <- report$quality_flags %>%
          plot_ly(
            x = ~quality_flag,
            y = ~n,
            type = 'bar',
            marker = list(
              color = ~case_when(
                quality_flag == "OK" ~ "#27AE60",
                grepl("Missing", quality_flag) ~ "#E74C3C",
                TRUE ~ "#F39C12"
              )
            ),
            text = ~paste("Count:", n),
            hovertemplate = "%{x}<br>%{text}<extra></extra>"
          ) %>%
          layout(
            xaxis = list(title = "Quality Flag"),
            yaxis = list(title = "Count"),
            showlegend = FALSE,
            margin = list(b = 100)
          )
        
        p
      }
    })
    
    output$entry_timeline_plot <- renderPlotly({
      req(clean_data())
      data <- clean_data()
      
      if (nrow(data) == 0 || !"date_sample" %in% names(data)) {
        plotly::plotly_empty()
      } else {
        timeline_data <- data %>%
          filter(!is.na(date_sample)) %>%
          mutate(week = floor_date(date_sample, "week")) %>%
          count(week) %>%
          arrange(week)
        
        plot_ly(
          timeline_data,
          x = ~week,
          y = ~n,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(color = "#3498DB", width = 2),
          marker = list(color = "#3498DB", size = 6),
          hovertemplate = "Week: %{x|%Y-%m-%d}<br>Samples: %{y}<extra></extra>"
        ) %>%
          layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Samples Collected"),
            showlegend = FALSE
          )
      }
    })
    
    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================
    
    download_duplicates <- download_handler_server(
      id = "download_duplicates",
      filename = function() {
        sprintf("duplicates_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        report <- quality_report()
        write_csv(report$duplicates, file)
      }
    )
    
    download_conflicts <- download_handler_server(
      id = "download_conflicts",
      filename = function() {
        sprintf("conflicts_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        report <- quality_report()
        write_csv(report$barcode_conflicts, file)
      }
    )
    
    download_completeness <- download_handler_server(
      id = "download_completeness",
      filename = function() {
        sprintf("completeness_%s.csv", format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        report <- quality_report()
        write_csv(report$completeness, file)
      }
    )
    
  })
}

# ============================================================================
# HELPER FUNCTIONS (Module-specific)
# ============================================================================

#' Create download button UI
download_button_ui <- function(id) {
  downloadButton(
    id,
    label = NULL,
    icon = icon("download"),
    class = "btn-sm btn-outline-primary"
  )
}

#' Download handler server logic
download_handler_server <- function(id, filename, content) {
  downloadHandler(
    filename = filename,
    content = content
  )
}
