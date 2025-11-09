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
        width = 1/5, fixed_width = TRUE, heights_equal = "row", gap = "12px",
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
          title = "% Extracts Clear",
          value = textOutput(ns("kpi_clear")),
          showcase = icon("eye"),
          theme = "secondary"
        ),
        value_box(
          title = "Flagged Extractions",
          value = textOutput(ns("kpi_flagged")),
          showcase = icon("triangle-exclamation"),
          theme = "danger"
        )
      ),

      layout_column_wrap(
        width = 1/5, fixed_width = TRUE, heights_equal = "row", gap = "12px",
        value_box(
          title = "Valid Sample IDs",
          value = textOutput(ns("kpi_valid_ids")),
          showcase = icon("check-circle"),
          theme = "success"
        ),
        value_box(
          title = "Duplicates Found",
          value = textOutput(ns("kpi_duplicates")),
          showcase = icon("clone"),
          theme = "warning"
        ),
        value_box(
          title = "Suspicious Barcodes",
          value = textOutput(ns("kpi_suspicious")),
          showcase = icon("exclamation-triangle"),
          theme = "danger"
        ),
        value_box(
          title = "Validation Rate",
          value = textOutput(ns("kpi_validation_rate")),
          showcase = icon("clipboard-check"),
          theme = "info"
        ),
        value_box(
          title = "Health Structures",
          value = textOutput(ns("kpi_structures")),
          showcase = icon("hospital"),
          theme = "primary"
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
              col_widths = c(3, 2, 2, 2, 3), gap = "12px",
              dateRangeInput(ns("filter_date"), "Extraction Date", start = Sys.Date() - 90, end = Sys.Date()),
              selectizeInput(ns("filter_state"), "DRS State", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_filter"), "Filter Type", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_quality"), "Visual Quality", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_structure"), "Health Structure", choices = character(0), multiple = TRUE)
            ),
            layout_columns(
              col_widths = c(3, 3, 3, 3), gap = "12px",
              selectizeInput(ns("filter_technician"), "Technician", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_validation"), "Validation Status", choices = character(0), multiple = TRUE),
              selectizeInput(ns("filter_project"), "Project", choices = character(0), multiple = TRUE),
              div()
            ),
            div(
              class = "mt-2",
              actionButton(ns("reset_filters"), "Reset Filters", icon = icon("undo"), class = "btn btn-sm btn-secondary")
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Health Structure Volume Processing Analysis"),
          card_body_fill(
            plotly::plotlyOutput(ns("health_structure_volume_plot"), height = "400px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("Health Structure Performance Metrics"),
          card_body_fill(
            plotly::plotlyOutput(ns("health_structure_metrics_plot"), height = "320px")
          )
        ),
        card(
          card_header("Validation Status Distribution"),
          card_body_fill(
            plotly::plotlyOutput(ns("validation_status_plot"), height = "320px")
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
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("Extraction Volume Time Series"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_timeseries_plot"), height = "320px")
          )
        ),
        card(
          card_header("Project Distribution"),
          card_body_fill(
            plotly::plotlyOutput(ns("project_distribution_plot"), height = "320px")
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

      # Add new filter options
      if ("health_structure" %in% names(df)) {
        structures <- sort(unique(df$health_structure[df$health_structure != "Unspecified"]))
        updateSelectizeInput(session, "filter_structure", choices = structures, selected = structures)
      }

      if ("validation_status" %in% names(df)) {
        statuses <- sort(unique(df$validation_status))
        updateSelectizeInput(session, "filter_validation", choices = statuses, selected = statuses)
      }

      if ("project" %in% names(df)) {
        projects <- sort(unique(df$project[df$project != "UNSPECIFIED"]))
        updateSelectizeInput(session, "filter_project", choices = projects, selected = projects)
      }
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

      # Reset new filters
      if ("health_structure" %in% names(df)) {
        structures <- sort(unique(df$health_structure[df$health_structure != "Unspecified"]))
        updateSelectizeInput(session, "filter_structure", selected = structures)
      }

      if ("validation_status" %in% names(df)) {
        statuses <- sort(unique(df$validation_status))
        updateSelectizeInput(session, "filter_validation", selected = statuses)
      }

      if ("project" %in% names(df)) {
        projects <- sort(unique(df$project[df$project != "UNSPECIFIED"]))
        updateSelectizeInput(session, "filter_project", selected = projects)
      }
    })

    filtered_data <- reactive({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(df)
      }

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

      # New filters
      if (!is.null(input$filter_structure) && length(input$filter_structure) && "health_structure" %in% names(df)) {
        df <- df %>% dplyr::filter(health_structure %in% input$filter_structure)
      }

      if (!is.null(input$filter_validation) && length(input$filter_validation) && "validation_status" %in% names(df)) {
        df <- df %>% dplyr::filter(validation_status %in% input$filter_validation)
      }

      if (!is.null(input$filter_project) && length(input$filter_project) && "project" %in% names(df)) {
        df <- df %>% dplyr::filter(project %in% input$filter_project)
      }

      df
    })

    metrics <- reactive({
      summarise_extraction_metrics(filtered_data())
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

    output$kpi_valid_ids <- renderText({
      m <- metrics()
      if (is.na(m$valid_ids)) "--" else scales::comma(m$valid_ids)
    })

    output$kpi_duplicates <- renderText({
      m <- metrics()
      if (is.na(m$duplicates)) "--" else scales::comma(m$duplicates)
    })

    output$kpi_suspicious <- renderText({
      m <- metrics()
      if (is.na(m$suspicious_barcodes)) "--" else scales::comma(m$suspicious_barcodes)
    })

    output$kpi_validation_rate <- renderText({
      m <- metrics()
      if (is.na(m$validation_rate)) "--" else scales::percent(m$validation_rate, accuracy = 0.1)
    })

    output$kpi_structures <- renderText({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
        return("--")
      }
      n_structures <- length(unique(df$health_structure[df$health_structure != "Unspecified"]))
      scales::comma(n_structures)
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
        dplyr::count(technician, ready_for_freezer, name = "n")

      plotly::plot_ly(plot_df, x = ~technician, y = ~n, color = ~ready_for_freezer,
                      type = "bar", barmode = "stack") %>%
        plotly::layout(xaxis = list(title = "Technician"), yaxis = list(title = "Extractions"), legend = list(title = list(text = "Ready for Freezer")))
    })

    # NEW VISUALIZATIONS

    output$health_structure_volume_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "Health structure data not available"))
      }

      summary_df <- summarise_by_health_structure(df)

      if (!nrow(summary_df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No health structure data available"))
      }

      # Create grouped bar chart showing extractions and volume by health structure
      plotly::plot_ly(summary_df, x = ~health_structure, y = ~n_extractions,
                     type = "bar", name = "Number of Extractions",
                     marker = list(color = "#3498DB")) %>%
        plotly::add_trace(y = ~total_volume, name = "Total Volume (mL)",
                         yaxis = "y2",
                         marker = list(color = "#27AE60")) %>%
        plotly::layout(
          title = "DRS Processing Volume by Health Structure",
          xaxis = list(title = "Health Structure", tickangle = -45),
          yaxis = list(title = "Number of Extractions", side = "left"),
          yaxis2 = list(title = "Total Volume (mL)", overlaying = "y", side = "right"),
          barmode = "group",
          legend = list(x = 0.8, y = 1),
          margin = list(b = 120)
        )
    })

    output$health_structure_metrics_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "Health structure data not available"))
      }

      summary_df <- summarise_by_health_structure(df)

      if (!nrow(summary_df)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No health structure data available"))
      }

      # Bubble chart: Ready rate vs Volume, bubble size = number of extractions
      plotly::plot_ly(summary_df, x = ~median_volume, y = ~pct_ready,
                     size = ~n_extractions, sizes = c(10, 500),
                     color = ~health_structure, type = "scatter", mode = "markers",
                     text = ~paste0("<b>", health_structure, "</b><br>",
                                   "Extractions: ", n_extractions, "<br>",
                                   "Median Volume: ", round(median_volume, 1), " mL<br>",
                                   "Ready Rate: ", scales::percent(pct_ready, accuracy = 0.1)),
                     hoverinfo = "text") %>%
        plotly::layout(
          title = "Health Structure Performance: Quality vs Volume",
          xaxis = list(title = "Median DRS Volume (mL)"),
          yaxis = list(title = "% Ready for Freezer", tickformat = ".0%"),
          showlegend = TRUE
        )
    })

    output$validation_status_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || !"validation_status" %in% names(df)) {
        return(plotly::plotly_empty(type = "pie") %>% plotly::layout(title = "Validation data not available"))
      }

      plot_df <- df %>%
        dplyr::count(validation_status, name = "n") %>%
        dplyr::arrange(dplyr::desc(n))

      colors <- c("Valid" = "#27AE60", "Duplicate" = "#F39C12",
                 "Invalid ID" = "#E74C3C", "Suspicious Barcode" = "#E67E22")

      plotly::plot_ly(plot_df, labels = ~validation_status, values = ~n, type = "pie",
                     marker = list(colors = colors[plot_df$validation_status]),
                     textinfo = "label+percent",
                     hoverinfo = "text",
                     text = ~paste0(validation_status, ": ", n, " samples")) %>%
        plotly::layout(title = "Sample ID Validation Status")
    })

    output$volume_timeseries_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No extraction data available"))
      }

      plot_df <- df %>%
        dplyr::mutate(week = lubridate::floor_date(extraction_date, "week")) %>%
        dplyr::filter(!is.na(week)) %>%
        dplyr::group_by(week) %>%
        dplyr::summarise(
          total_volume = sum(drs_volume_ml, na.rm = TRUE),
          median_volume = stats::median(drs_volume_ml, na.rm = TRUE),
          n = dplyr::n(),
          .groups = "drop"
        )

      if (!nrow(plot_df)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No dated extraction records"))
      }

      plotly::plot_ly(plot_df, x = ~week, y = ~total_volume, type = "scatter", mode = "lines+markers",
                     name = "Total Volume", line = list(color = "#3498DB"),
                     text = ~paste0("Week: ", format(week, "%Y-%m-%d"), "<br>",
                                   "Total: ", round(total_volume, 1), " mL<br>",
                                   "Extractions: ", n),
                     hoverinfo = "text") %>%
        plotly::add_trace(y = ~median_volume, name = "Median Volume", mode = "lines",
                         line = list(color = "#27AE60", dash = "dash"),
                         yaxis = "y2") %>%
        plotly::layout(
          title = "DRS Volume Over Time",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Total Volume (mL)", side = "left"),
          yaxis2 = list(title = "Median Volume (mL)", overlaying = "y", side = "right"),
          hovermode = "closest"
        )
    })

    output$project_distribution_plot <- plotly::renderPlotly({
      df <- filtered_data()
      if (is.null(df) || !nrow(df) || !"project" %in% names(df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "Project data not available"))
      }

      plot_df <- df %>%
        dplyr::filter(project != "UNSPECIFIED") %>%
        dplyr::count(project, drs_state, name = "n")

      if (!nrow(plot_df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No project data available"))
      }

      plotly::plot_ly(plot_df, x = ~project, y = ~n, color = ~drs_state,
                     type = "bar") %>%
        plotly::layout(
          title = "Extractions by Project and DRS State",
          xaxis = list(title = "Project"),
          yaxis = list(title = "Number of Extractions"),
          barmode = "stack"
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
          flag_issue = dplyr::if_else(flag_issue, "Yes", "No")
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
