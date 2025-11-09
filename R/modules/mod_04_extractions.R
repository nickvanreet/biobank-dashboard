# R/modules/mod_04_extractions.R
# Extraction Quality Module (04)
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Extraction Quality Module UI
#' @param id Module namespace ID
#' @export
mod_extractions_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Extraction Quality",
    icon = icon("vials"),

    div(
      class = "container-fluid",

      layout_column_wrap(
        width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "12px",
        value_box(
          title = "Total Extractions",
          value = textOutput(ns("kpi_total")),
          showcase = icon("droplet"),
          theme = "primary"
        ),
        value_box(
          title = "Ready for Freezer",
          value = textOutput(ns("kpi_ready")),
          showcase = icon("snowflake"),
          theme = "success"
        ),
        value_box(
          title = "Median DRS Volume (mL)",
          value = textOutput(ns("kpi_volume")),
          showcase = icon("flask"),
          theme = "info"
        ),
        value_box(
          title = "% Liquid DRS",
          value = textOutput(ns("kpi_liquid")),
          showcase = icon("water"),
          theme = "secondary"
        ),
        value_box(
          title = "% Extracts Clear",
          value = textOutput(ns("kpi_clear")),
          showcase = icon("eye"),
          theme = "warning"
        ),
        value_box(
          title = "Flagged Extractions",
          value = textOutput(ns("kpi_flagged")),
          showcase = icon("triangle-exclamation"),
          theme = "danger"
        ),
        value_box(
          title = "% Within Target Volume",
          value = textOutput(ns("kpi_volume_target")),
          showcase = icon("bullseye"),
          theme = "info"
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Extraction Filters"),
            actionButton(ns("refresh_data"), label = "Refresh", icon = icon("arrows-rotate"), class = "btn btn-sm btn-outline-primary")
          ),
          card_body(
            layout_columns(
              col_widths = c(4, 2, 2, 2, 2, 2), gap = "12px",
              dateRangeInput(ns("filter_date"), "Extraction Date", start = Sys.Date() - 90, end = Sys.Date()),
              selectizeInput(ns("filter_state"), "DRS State", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_filter"), "Filter Type", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_quality"), "Visual Quality", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_technician"), "Technician", choices = character(0), multiple = TRUE),
              sliderInput(ns("volume_target"), "Target DRS Volume (mL)", min = 0.5, max = 5, value = c(1.5, 2.5), step = 0.1)
            ),
            div(
              class = "mt-2",
              actionButton(ns("reset_filters"), "Reset Filters", icon = icon("undo"), class = "btn btn-sm btn-secondary")
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("DRS State Distribution by Week"),
          card_body_fill(
            plotly::plotlyOutput(ns("drs_state_plot"), height = "320px")
          )
        ),
        card(
          card_header("DRS Volume by Filter Type"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_filter_plot"), height = "320px")
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Health Structure Volume Performance"),
          card_body_fill(
            plotly::plotlyOutput(ns("structure_performance_plot"), height = "320px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("Visual Quality Trend"),
          card_body_fill(
            plotly::plotlyOutput(ns("quality_trend_plot"), height = "320px")
          )
        ),
        card(
          card_header("Technician Productivity"),
          card_body_fill(
            plotly::plotlyOutput(ns("technician_plot"), height = "320px")
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Extraction Detail Table"),
          card_body(
            DT::DTOutput(ns("extraction_table"))
          )
        )
      )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' Extraction Quality Module Server
#' @param id Module namespace ID
#' @export
mod_extractions_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    extraction_data <- reactiveVal(load_extraction_dataset())

    observeEvent(input$refresh_data, {
      extraction_data(load_extraction_dataset())
      showNotification("Extraction data refreshed", type = "message", duration = 3)
    })

    observeEvent(extraction_data(), {
      df <- extraction_data()
      if (!nrow(df)) {
        return()
      }

      min_date <- min(df$extraction_date, na.rm = TRUE)
      max_date <- max(df$extraction_date, na.rm = TRUE)
      updateDateRangeInput(session, "filter_date", start = min_date, end = max_date, min = min_date, max = max_date)

      updateSelectizeInput(session, "filter_state", choices = sort(unique(df$drs_state)), selected = sort(unique(df$drs_state)))
      updateSelectizeInput(session, "filter_filter", choices = sort(unique(df$filter_type)), selected = sort(unique(df$filter_type)))
      updateSelectizeInput(session, "filter_quality", choices = sort(unique(df$extract_quality)), selected = sort(unique(df$extract_quality)))
      updateSelectizeInput(session, "filter_technician", choices = sort(unique(df$technician)), selected = sort(unique(df$technician)))

      vols <- df$drs_volume_ml
      vols <- vols[is.finite(vols)]
      if (length(vols)) {
        vmin <- floor(min(vols) * 10) / 10
        vmax <- ceiling(max(vols) * 10) / 10
      } else {
        vmin <- 0.5
        vmax <- 5
      }

      default_range <- c(max(1.5, vmin), min(2.5, vmax))
      if (default_range[1] > default_range[2]) {
        default_range <- c(vmin, vmax)
      }

      updateSliderInput(session, "volume_target", min = vmin, max = vmax, value = default_range)
    }, ignoreNULL = FALSE)

    observeEvent(input$reset_filters, {
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return()
      }
      min_date <- min(df$extraction_date, na.rm = TRUE)
      max_date <- max(df$extraction_date, na.rm = TRUE)
      updateDateRangeInput(session, "filter_date", start = min_date, end = max_date)
      updateSelectizeInput(session, "filter_state", selected = sort(unique(df$drs_state)))
      updateSelectizeInput(session, "filter_filter", selected = sort(unique(df$filter_type)))
      updateSelectizeInput(session, "filter_quality", selected = sort(unique(df$extract_quality)))
      updateSelectizeInput(session, "filter_technician", selected = sort(unique(df$technician)))

      vols <- df$drs_volume_ml
      vols <- vols[is.finite(vols)]
      if (length(vols)) {
        vmin <- floor(min(vols) * 10) / 10
        vmax <- ceiling(max(vols) * 10) / 10
        default_range <- c(max(1.5, vmin), min(2.5, vmax))
        if (default_range[1] > default_range[2]) {
          default_range <- c(vmin, vmax)
        }
        updateSliderInput(session, "volume_target", value = default_range)
      } else {
        updateSliderInput(session, "volume_target", value = c(1.5, 2.5))
      }
    })

    filtered_data <- reactive({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(df)
      }

      volume_range <- input$volume_target
      if (is.null(volume_range) || length(volume_range) != 2) {
        volume_range <- c(1.5, 2.5)
      }
      volume_min <- min(volume_range)
      volume_max <- max(volume_range)

      if (!is.null(input$filter_date) && length(input$filter_date) == 2) {
        start <- suppressWarnings(as.Date(input$filter_date[1]))
        end <- suppressWarnings(as.Date(input$filter_date[2]))
        if (!is.na(start) && !is.na(end)) {
          df <- df %>% dplyr::filter(!is.na(extraction_date) & extraction_date >= start & extraction_date <= end)
        }
      }

      if (!is.null(input$filter_state) && length(input$filter_state)) {
        df <- df %>% dplyr::filter(drs_state %in% input$filter_state)
      }

      if (!is.null(input$filter_filter) && length(input$filter_filter)) {
        df <- df %>% dplyr::filter(filter_type %in% input$filter_filter)
      }

      if (!is.null(input$filter_quality) && length(input$filter_quality)) {
        df <- df %>% dplyr::filter(extract_quality %in% input$filter_quality)
      }

      if (!is.null(input$filter_technician) && length(input$filter_technician)) {
        df <- df %>% dplyr::filter(technician %in% input$filter_technician)
      }

      df %>%
        dplyr::mutate(
          within_target_volume = dplyr::case_when(
            is.na(drs_volume_ml) ~ NA,
            drs_volume_ml >= volume_min & drs_volume_ml <= volume_max ~ TRUE,
            TRUE ~ FALSE
          )
        )
    })

    metrics <- reactive({
      summarise_extraction_metrics(filtered_data(), input$volume_target)
    })

    output$kpi_total <- renderText({
      m <- metrics()
      scales::comma(m$total)
    })

    output$kpi_ready <- renderText({
      m <- metrics()
      scales::comma(m$ready)
    })

    output$kpi_volume <- renderText({
      m <- metrics()
      if (is.na(m$median_volume)) "--" else scales::number(m$median_volume, accuracy = 0.1)
    })

    output$kpi_liquid <- renderText({
      m <- metrics()
      if (is.na(m$pct_liquid)) "--" else scales::percent(m$pct_liquid, accuracy = 0.1)
    })

    output$kpi_clear <- renderText({
      m <- metrics()
      if (is.na(m$pct_clear)) "--" else scales::percent(m$pct_clear, accuracy = 0.1)
    })

    output$kpi_flagged <- renderText({
      m <- metrics()
      scales::comma(m$flagged)
    })

    output$kpi_volume_target <- renderText({
      m <- metrics()
      if (is.na(m$pct_within_target)) "--" else scales::percent(m$pct_within_target, accuracy = 0.1)
    })

    output$drs_state_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "bar", mode = "stack") %>% plotly::layout(title = "No extraction data available"))
      }

      plot_df <- df %>%
        dplyr::mutate(week = lubridate::floor_date(extraction_date, "week")) %>%
        dplyr::filter(!is.na(week)) %>%
        dplyr::count(week, drs_state, name = "n") %>%
        dplyr::arrange(week)

      if (!nrow(plot_df)) {
        return(plotly::plotly_empty(type = "bar", mode = "stack") %>% plotly::layout(title = "No dated extraction records"))
      }

      plotly::plot_ly(plot_df, x = ~week, y = ~n, color = ~drs_state, type = "bar") %>%
        plotly::layout(barmode = "stack", xaxis = list(title = "Week"), yaxis = list(title = "Extractions"))
    })

    output$volume_filter_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || all(is.na(df$drs_volume_ml))) {
        return(plotly::plotly_empty(type = "box") %>% plotly::layout(title = "No volume data available"))
      }

      plot_df <- df %>% dplyr::mutate(filter_type = forcats::fct_lump(filter_type, n = 8))

      plotly::plot_ly(plot_df, x = ~filter_type, y = ~drs_volume_ml, color = ~drs_state, type = "box") %>%
        plotly::layout(xaxis = list(title = "Filter"), yaxis = list(title = "DRS Volume (mL)"))
    })

    output$quality_trend_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") %>% plotly::layout(title = "No extraction data available"))
      }

      plot_df <- df %>%
        dplyr::mutate(month = lubridate::floor_date(extraction_date, "month")) %>%
        dplyr::filter(!is.na(month)) %>%
        dplyr::group_by(month) %>%
        dplyr::summarise(
          pct_clear = mean(extract_quality == "Clear", na.rm = TRUE),
          median_volume = stats::median(drs_volume_ml, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        )

      if (!nrow(plot_df)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") %>% plotly::layout(title = "No monthly data available"))
      }

      scale_factor <- max(plot_df$median_volume, na.rm = TRUE)
      if (!is.finite(scale_factor) || scale_factor == 0) {
        scale_factor <- 1
      }

      plotly::plot_ly(plot_df, x = ~month, y = ~pct_clear, type = "scatter", mode = "lines+markers", name = "% Clear") %>%
        plotly::add_trace(y = ~median_volume / scale_factor, name = "Median Volume (scaled)", yaxis = "y2", mode = "lines+markers") %>%
        plotly::layout(
          yaxis = list(title = "% Clear", tickformat = ".0%"),
          yaxis2 = list(overlaying = "y", side = "right", title = "Median Volume (scaled)"),
          xaxis = list(title = "Month")
        )
    })

    output$technician_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No extraction data available"))
      }

      plot_df <- df %>%
        dplyr::mutate(
          volume_label = dplyr::case_when(
            is.na(within_target_volume) ~ "Missing Volume",
            within_target_volume ~ "Within Target",
            TRUE ~ "Outside Target"
          )
        ) %>%
        dplyr::count(technician, volume_label, name = "n")

      plotly::plot_ly(plot_df, x = ~technician, y = ~n, color = ~volume_label,
                      type = "bar", barmode = "stack") %>%
        plotly::layout(
          xaxis = list(title = "Technician"),
          yaxis = list(title = "Extractions"),
          legend = list(title = list(text = "Volume Status"))
        )
    })

    output$structure_performance_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") %>%
                 plotly::layout(title = "No extraction data available"))
      }

      plot_df <- df %>%
        dplyr::mutate(period = lubridate::floor_date(extraction_date, "month")) %>%
        dplyr::filter(!is.na(period)) %>%
        dplyr::group_by(period, health_structure) %>%
        dplyr::summarise(
          pct_within = mean(within_target_volume, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::mutate(pct_within = ifelse(is.nan(pct_within), NA_real_, pct_within)) %>%
        dplyr::filter(!is.na(health_structure), health_structure != "Unspecified", !is.na(pct_within))

      if (!nrow(plot_df)) {
        return(plotly::plotly_empty(type = "scatter", mode = "lines") %>%
                 plotly::layout(title = "No health structure data available"))
      }

      plotly::plot_ly(
        plot_df,
        x = ~period,
        y = ~pct_within,
        color = ~health_structure,
        type = "scatter",
        mode = "lines+markers",
        text = ~n,
        hovertemplate = paste0(
          "%{fullData.name}<br>",
          "%{x|%Y-%m}<br>",
          "% Within Target: %{y:.0%}<br>",
          "Extractions: %{text}<extra></extra>"
        )
      ) %>%
        plotly::layout(
          yaxis = list(title = "% Within Target", tickformat = ".0%"),
          xaxis = list(title = "Month"),
          legend = list(title = list(text = "Health Structure"))
        )
    })

    output$extraction_table <- DT::renderDT({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No extraction data available for the selected filters."), options = list(dom = "t"), rownames = FALSE))
      }

      table_df <- df %>%
        dplyr::arrange(dplyr::desc(extraction_date)) %>%
        dplyr::mutate(
          extraction_date = format(extraction_date, "%Y-%m-%d"),
          drs_volume_ml = ifelse(is.na(drs_volume_ml), "", scales::number(drs_volume_ml, accuracy = 0.1)),
          ready_for_freezer = dplyr::if_else(ready_for_freezer, "Yes", "No"),
          flag_issue = dplyr::if_else(flag_issue, "Yes", "No"),
          within_target_volume = dplyr::case_when(
            is.na(within_target_volume) ~ "Unknown",
            within_target_volume ~ "Yes",
            TRUE ~ "No"
          )
        )

      DT::datatable(
        table_df,
        options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 15)),
        rownames = FALSE,
        filter = "top"
      )
    })
  })
}
