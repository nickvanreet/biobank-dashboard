# R/modules/mod_03_transport.R
# Transport Module (03) - focuses on shipment timelines and throughput
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Transport Module UI
#' @param id Module namespace ID
#' @export
mod_transport_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Transport",
    icon = icon("truck"),

    div(class = "container-fluid",

        # ==== KPI STRIP ======================================================
        layout_column_wrap(
          width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "12px",

          value_box(
            title = "Median Days: Field → CPLTHA",
            value = textOutput(ns("kpi_field_to_cpltha")),
            showcase = icon("route"),
            theme = "info"
          ),
          value_box(
            title = "Median Days: Sent → Received (CPLTHA)",
            value = textOutput(ns("kpi_cpltha_receipt")),
            showcase = icon("clock"),
            theme = "primary"
          ),
          value_box(
            title = "Median Days: CPLTHA → INRB",
            value = textOutput(ns("kpi_cpltha_to_inrb")),
            showcase = icon("plane-departure"),
            theme = "info"
          ),
          value_box(
            title = "Median Total Transport Days",
            value = textOutput(ns("kpi_total_transport")),
            showcase = icon("hourglass-half"),
            theme = "secondary"
          ),
          value_box(
            title = "% Shipped to CPLTHA",
            value = textOutput(ns("kpi_pct_shipped_cpltha")),
            showcase = icon("truck-moving"),
            theme = "success"
          ),
          value_box(
            title = "% Received at CPLTHA",
            value = textOutput(ns("kpi_pct_received_cpltha")),
            showcase = icon("warehouse"),
            theme = "success"
          ),
          value_box(
            title = "% Shipped to INRB",
            value = textOutput(ns("kpi_pct_shipped_inrb")),
            showcase = icon("plane"),
            theme = "success"
          ),
          value_box(
            title = "Avg Transport Temp (°C)",
            value = textOutput(ns("kpi_avg_temperature")),
            showcase = icon("temperature-high"),
            theme = "warning"
          )
        ),

        # ==== CHARTS =========================================================
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          card(
            card_header("Transport Durations by Week"),
            card_body_fill(
              plotly::plotlyOutput(ns("transport_timeline_plot"), height = "500px")
            )
          ),
          card(
            card_header("Stage Throughput"),
            card_body_fill(
              plotly::plotlyOutput(ns("transport_funnel_plot"), height = "500px")
            )
          )
        ),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          card(
            card_header("Transport Temperature Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("transport_temperature_plot"), height = "500px")
            )
          ),
          card(
            card_header("Transport Durations by Province"),
            card_body_fill(
              plotly::plotlyOutput(ns("transport_province_plot"), height = "500px")
            )
          )
        ),

        # ==== TABLES =========================================================
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Delayed Shipments"),
              span(class = "text-muted small", sprintf("> %d days field → CPLTHA", config$qc$max_transport_days))
            ),
            card_body(
              DT::DTOutput(ns("delayed_shipments_table"))
            )
          ),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Missing Transport Dates"),
              span(class = "text-muted small", "Any leg without a recorded date")
            ),
            card_body(
              DT::DTOutput(ns("missing_dates_table"))
            )
          )
        )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' Transport Module Server
#' @param id Module namespace ID
#' @param filtered_data Reactive expression containing filtered biobank data
#' @export
mod_transport_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    safe_median <- function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        return(NA_real_)
      }
      stats::median(x, na.rm = TRUE)
    }

    safe_mean <- function(x) {
      if (length(x) == 0 || all(is.na(x))) {
        return(NA_real_)
      }
      base::mean(x, na.rm = TRUE)
    }

    fmt_days <- function(x) {
      if (is.na(x)) {
        return("N/A")
      }
      scales::number(x, accuracy = 0.1, suffix = " d")
    }

    fmt_percent <- function(x) {
      if (is.na(x)) {
        return("N/A")
      }
      scales::percent(x / 100, accuracy = 0.1)
    }

    fmt_temperature <- function(x) {
      if (is.na(x)) {
        return("N/A")
      }
      scales::number(x, accuracy = 0.1)
    }

    transport_metrics <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) {
        return(NULL)
      }

      tibble::tibble(
        median_field_to_cpltha = safe_median(data$transport_field_to_cpltha),
        median_cpltha_receipt = safe_median(data$transport_received_at_cpltha),
        median_cpltha_to_inrb = safe_median(data$transport_cpltha_to_inrb),
        median_total_transport = safe_median(data$total_transport_days),
        pct_shipped_cpltha = safe_mean(as.numeric(data$shipped_to_cpltha) * 100),
        pct_received_cpltha = safe_mean(as.numeric(data$received_at_cpltha) * 100),
        pct_shipped_inrb = safe_mean(as.numeric(data$shipped_to_inrb) * 100),
        avg_transport_temp = safe_mean(data$transport_temperature)
      )
    })

    # KPI renderers -----------------------------------------------------------
    output$kpi_field_to_cpltha <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_days(metrics$median_field_to_cpltha)
    })

    output$kpi_cpltha_receipt <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_days(metrics$median_cpltha_receipt)
    })

    output$kpi_cpltha_to_inrb <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_days(metrics$median_cpltha_to_inrb)
    })

    output$kpi_total_transport <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_days(metrics$median_total_transport)
    })

    output$kpi_pct_shipped_cpltha <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_percent(metrics$pct_shipped_cpltha)
    })

    output$kpi_pct_received_cpltha <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_percent(metrics$pct_received_cpltha)
    })

    output$kpi_pct_shipped_inrb <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_percent(metrics$pct_shipped_inrb)
    })

    output$kpi_avg_temperature <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_temperature(metrics$avg_transport_temp)
    })

    # Timeline ---------------------------------------------------------------
    transport_timeline <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) {
        return(tibble::tibble())
      }

      data %>%
        dplyr::filter(!is.na(date_sample)) %>%
        dplyr::mutate(sample_week = lubridate::floor_date(date_sample, "week")) %>%
        dplyr::filter(!is.na(sample_week)) %>%
        dplyr::group_by(sample_week) %>%
        dplyr::summarise(
          field_to_cpltha = safe_median(transport_field_to_cpltha),
          cpltha_to_inrb = safe_median(transport_cpltha_to_inrb),
          .groups = "drop"
        )
    })

    output$transport_timeline_plot <- plotly::renderPlotly({
      data <- transport_timeline()
      if (nrow(data) == 0) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines", hoverinfo = "none") %>%
                 plotly::layout(title = list(text = "No data available for current filters")))
      }

      plotly::plot_ly(data, x = ~sample_week) %>%
        plotly::add_lines(y = ~field_to_cpltha, name = "Field → CPLTHA", line = list(color = config$ui$theme_info)) %>%
        plotly::add_lines(y = ~cpltha_to_inrb, name = "CPLTHA → INRB", line = list(color = config$ui$theme_success)) %>%
        plotly::layout(
          yaxis = list(title = "Median days"),
          xaxis = list(title = "Week"),
          hovermode = "x unified"
        )
    })

    # Funnel -----------------------------------------------------------------
    transport_funnel <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) {
        return(tibble::tibble())
      }

      tibble::tibble(
        stage = factor(
          c("Collected", "Shipped to CPLTHA", "Received at CPLTHA", "Shipped to INRB"),
          levels = c("Collected", "Shipped to CPLTHA", "Received at CPLTHA", "Shipped to INRB")
        ),
        count = c(
          nrow(data),
          sum(data$shipped_to_cpltha, na.rm = TRUE),
          sum(data$received_at_cpltha, na.rm = TRUE),
          sum(data$shipped_to_inrb, na.rm = TRUE)
        )
      )
    })

    output$transport_funnel_plot <- plotly::renderPlotly({
      data <- transport_funnel()
      if (nrow(data) == 0) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = list(text = "No data available for current filters")))
      }

      plotly::plot_ly(
        data = data,
        x = ~count,
        y = ~stage,
        type = "bar",
        orientation = "h",
        marker = list(color = config$ui$theme_primary)
      ) %>%
        plotly::layout(
          xaxis = list(title = "Samples"),
          yaxis = list(title = ""),
          bargap = 0.3
        )
    })

    # Temperature distribution -----------------------------------------------
    output$transport_temperature_plot <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      temps <- data$transport_temperature
      if (length(temps) == 0 || all(is.na(temps))) {
        return(plotly::plotly_empty(type = "histogram", hoverinfo = "none") %>%
                 plotly::layout(title = list(text = "No temperature data for current filters")))
      }

      plotly::plot_ly(
        x = ~temps,
        type = "histogram",
        nbinsx = 20,
        marker = list(color = config$ui$theme_warning)
      ) %>%
        plotly::layout(
          xaxis = list(title = "Temperature (°C)"),
          yaxis = list(title = "Samples"),
          bargap = 0.05
        )
    })

    # Provincial transport durations -----------------------------------------
    output$transport_province_plot <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data) || !"province" %in% names(data)) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = list(text = "No province data available")))
      }

      summary <- data %>%
        dplyr::filter(!is.na(province)) %>%
        dplyr::group_by(province) %>%
        dplyr::summarise(
          median_total = safe_median(total_transport_days),
          samples = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(median_total))

      if (nrow(summary) == 0) {
        return(plotly::plotly_empty(type = "bar", hoverinfo = "none") %>%
                 plotly::layout(title = list(text = "No province data available")))
      }

      plotly::plot_ly(
        summary,
        x = ~median_total,
        y = ~reorder(province, median_total),
        type = "bar",
        orientation = "h",
        marker = list(color = config$ui$theme_info),
        text = ~sprintf("Median: %s\nSamples: %s", scales::number(median_total, accuracy = 0.1), scales::comma(samples)),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Median total transport days"),
          yaxis = list(title = "Province")
        )
    })

    # Tables -----------------------------------------------------------------
    delayed_shipments <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) {
        return(tibble::tibble(message = "No data available for current filters."))
      }

      breaches <- data %>%
        dplyr::filter(!is.na(transport_field_to_cpltha)) %>%
        dplyr::filter(transport_field_to_cpltha > config$qc$max_transport_days)

      if (nrow(breaches) == 0) {
        return(tibble::tibble(message = "No delayed shipments for current filters."))
      }

      breaches %>%
        dplyr::select(
          barcode,
          lab_id,
          date_sample,
          date_sent_cpltha,
          date_received_cpltha,
          transport_field_to_cpltha
        ) %>%
        dplyr::arrange(dplyr::desc(transport_field_to_cpltha))
    })

    output$delayed_shipments_table <- DT::renderDT({
      DT::datatable(
        delayed_shipments(),
        rownames = FALSE,
        options = list(pageLength = 10)
      )
    })

    missing_dates <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) {
        return(tibble::tibble(message = "No data available for current filters."))
      }

      missing <- data %>%
        dplyr::filter(is.na(date_sent_cpltha) | is.na(date_received_cpltha) | is.na(date_sent_inrb))

      if (nrow(missing) == 0) {
        return(tibble::tibble(message = "All transport dates recorded for current filters."))
      }

      missing %>%
        dplyr::select(
          barcode,
          lab_id,
          date_sample,
          date_sent_cpltha,
          date_received_cpltha,
          date_sent_inrb
        )
    })

    output$missing_dates_table <- DT::renderDT({
      DT::datatable(
        missing_dates(),
        rownames = FALSE,
        options = list(pageLength = 10)
      )
    })
  })
}
