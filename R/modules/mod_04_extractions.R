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
          title = "Matched to Biobank",
          value = textOutput(ns("kpi_matched")),
          showcase = icon("link"),
          theme = "success"
        ),
        value_box(
          title = "Unmatched Samples",
          value = textOutput(ns("kpi_unmatched")),
          showcase = icon("unlink"),
          theme = "warning"
        ),
        value_box(
          title = "Structure Matches",
          value = textOutput(ns("kpi_structure_match")),
          showcase = icon("hospital"),
          theme = "info"
        ),
        value_box(
          title = "Structure Mismatches",
          value = textOutput(ns("kpi_structure_mismatch")),
          showcase = icon("exclamation-triangle"),
          theme = "danger"
        ),
        value_box(
          title = "Linkage Rate",
          value = textOutput(ns("kpi_linkage_rate")),
          showcase = icon("percent"),
          theme = "primary"
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
          card_header("Health Structure DRS Volume Collection Over Time"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_evolution_plot"), height = "450px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("Volume Target Achievement by Health Structure"),
          card_body_fill(
            plotly::plotlyOutput(ns("target_achievement_plot"), height = "350px")
          )
        ),
        card(
          card_header("Biobank Linkage Status Distribution"),
          card_body_fill(
            plotly::plotlyOutput(ns("linkage_status_plot"), height = "350px")
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Unmatched Samples & Health Structure Mismatches"),
          card_body(
            navset_card_tab(
              nav_panel(
                "Unmatched to Biobank",
                DT::DTOutput(ns("unmatched_table"))
              ),
              nav_panel(
                "Health Structure Mismatches",
                DT::DTOutput(ns("mismatch_table"))
              )
            )
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
#' @param filtered_data Reactive expression returning extraction data filtered by the shared data manager
#' @param biobank_data Reactive expression returning cleaned biobank data for linkage
#' @export
mod_extractions_server <- function(id, filtered_data, biobank_data = NULL) {
  if (missing(filtered_data)) {
    stop("filtered_data reactive is required for mod_extractions_server()", call. = FALSE)
  }

  moduleServer(
    id,
    function(input, output, session, filtered_data, biobank_data) {

      extraction_data <- reactive({
        df <- filtered_data()

        if (is.null(df)) {
          return(tibble::tibble())
        }

        if (!is.data.frame(df)) {
          df <- tibble::as_tibble(df)
        }

        if (!nrow(df)) {
          return(df)
        }

        bio_df <- NULL
        if (!is.null(biobank_data)) {
          bio_df <- biobank_data()
        }

        tryCatch(
          link_extraction_to_biobank(df, bio_df),
          error = function(e) {
            message("Failed to link extraction data: ", e$message)
            df
          }
        )
      })

    metrics <- reactive({
      summarise_extraction_metrics(extraction_data())
    })

    linkage_metrics <- reactive({
      summarise_linkage_metrics(extraction_data())
    })

    output$kpi_total <- renderText({
      m <- metrics()
      if (is.null(m$total) || is.na(m$total)) "--" else scales::comma(m$total)
    })

    output$kpi_ready <- renderText({
      m <- metrics()
      if (is.null(m$ready) || is.na(m$ready)) "--" else scales::comma(m$ready)
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
      if (is.null(m$flagged) || is.na(m$flagged)) "--" else scales::comma(m$flagged)
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
      df <- extraction_data()
      if (is.null(df) || !nrow(df) || !"health_structure" %in% names(df)) {
        return("--")
      }
      n_structures <- length(unique(df$health_structure[df$health_structure != "Unspecified"]))
      scales::comma(n_structures)
    })

    # NEW LINKAGE KPIs
    output$kpi_matched <- renderText({
      lm <- linkage_metrics()
      if (is.na(lm$matched_to_biobank)) "--" else scales::comma(lm$matched_to_biobank)
    })

    output$kpi_unmatched <- renderText({
      lm <- linkage_metrics()
      if (is.na(lm$unmatched_to_biobank)) "--" else scales::comma(lm$unmatched_to_biobank)
    })

    output$kpi_structure_match <- renderText({
      lm <- linkage_metrics()
      if (is.na(lm$health_structure_matches)) "--" else scales::comma(lm$health_structure_matches)
    })

    output$kpi_structure_mismatch <- renderText({
      lm <- linkage_metrics()
      if (is.na(lm$health_structure_mismatches)) "--" else scales::comma(lm$health_structure_mismatches)
    })

    output$kpi_linkage_rate <- renderText({
      lm <- linkage_metrics()
      if (is.na(lm$pct_matched)) "--" else scales::percent(lm$pct_matched, accuracy = 0.1)
    })

    output$drs_state_plot <- plotly::renderPlotly({
      df <- extraction_data()
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
      df <- extraction_data()
      if (is.null(df) || !nrow(df) || all(is.na(df$drs_volume_ml))) {
        return(plotly::plotly_empty(type = "box") %>% plotly::layout(title = "No volume data available"))
      }

      plot_df <- df %>% dplyr::mutate(filter_type = forcats::fct_lump(filter_type, n = 8))

      plotly::plot_ly(plot_df, x = ~filter_type, y = ~drs_volume_ml, color = ~drs_state, type = "box") %>%
        plotly::layout(xaxis = list(title = "Filter"), yaxis = list(title = "DRS Volume (mL)"))
    })

    output$quality_trend_plot <- plotly::renderPlotly({
      df <- extraction_data()
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
      df <- extraction_data()
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
      df <- extraction_data()
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
      df <- extraction_data()
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
      df <- extraction_data()
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
      df <- extraction_data()
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
      df <- extraction_data()
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

    # NEW VISUALIZATIONS FOR VOLUME MONITORING
    output$volume_evolution_plot <- plotly::renderPlotly({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No data available"))
      }

      time_series <- summarise_health_structure_volumes_over_time(df)

      if (!nrow(time_series)) {
        return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No time series data available"))
      }

      # Create multi-line chart with one line per health structure
      plotly::plot_ly(time_series, x = ~month, y = ~total_volume, color = ~health_structure,
                     type = "scatter", mode = "lines+markers",
                     text = ~paste0("<b>", health_structure, "</b><br>",
                                   "Month: ", format(month, "%b %Y"), "<br>",
                                   "Total Volume: ", round(total_volume, 1), " mL<br>",
                                   "Extractions: ", n_extractions, "<br>",
                                   "Median Volume: ", round(median_volume, 1), " mL<br>",
                                   "Ready Rate: ", scales::percent(pct_ready, accuracy = 0.1)),
                     hoverinfo = "text") %>%
        plotly::layout(
          title = "DRS Volume Collection Trend by Health Structure",
          xaxis = list(title = "Month"),
          yaxis = list(title = "Total Volume Collected (mL)"),
          hovermode = "closest",
          legend = list(title = list(text = "Health Structure"))
        )
    })

    output$target_achievement_plot <- plotly::renderPlotly({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No data available"))
      }

      targets <- calculate_volume_targets(df, expected_monthly_volume = 50)

      if (!nrow(targets)) {
        return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No target data available"))
      }

      # Aggregate by health structure (latest month)
      latest_targets <- targets %>%
        dplyr::group_by(health_structure) %>%
        dplyr::filter(month == max(month)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          status = dplyr::if_else(meets_target, "Meets Target", "Below Target")
        )

      plotly::plot_ly(latest_targets, x = ~health_structure, y = ~pct_of_target,
                     type = "bar",
                     color = ~status,
                     colors = c("Meets Target" = "#27AE60", "Below Target" = "#E74C3C"),
                     text = ~paste0(health_structure, "<br>",
                                   round(pct_of_target * 100, 1), "% of target<br>",
                                   "Actual: ", round(actual_volume, 1), " mL<br>",
                                   "Expected: ", expected_volume, " mL"),
                     hoverinfo = "text") %>%
        plotly::layout(
          title = "Volume Target Achievement (Latest Month)",
          xaxis = list(title = "Health Structure", tickangle = -45),
          yaxis = list(title = "% of Target", tickformat = ".0%"),
          showlegend = TRUE,
          margin = list(b = 120)
        ) %>%
        plotly::add_shape(
          type = "line",
          x0 = -0.5, x1 = length(unique(latest_targets$health_structure)) - 0.5,
          y0 = 1, y1 = 1,
          line = list(color = "red", dash = "dash", width = 2)
        )
    })

    output$linkage_status_plot <- plotly::renderPlotly({
      df <- extraction_data()
      if (is.null(df) || !nrow(df) || !"biobank_matched" %in% names(df)) {
        return(plotly::plotly_empty(type = "pie") %>% plotly::layout(title = "Linkage data not available"))
      }

      plot_df <- df %>%
        dplyr::mutate(
          linkage_status = dplyr::case_when(
            !biobank_matched ~ "Unmatched to Biobank",
            health_structure_match == TRUE ~ "Matched - Structure OK",
            health_structure_match == FALSE ~ "Matched - Structure Mismatch",
            TRUE ~ "Matched - No Structure Info"
          )
        ) %>%
        dplyr::count(linkage_status, name = "n")

      colors <- c(
        "Matched - Structure OK" = "#27AE60",
        "Matched - No Structure Info" = "#3498DB",
        "Matched - Structure Mismatch" = "#F39C12",
        "Unmatched to Biobank" = "#E74C3C"
      )

      plotly::plot_ly(plot_df, labels = ~linkage_status, values = ~n, type = "pie",
                     marker = list(colors = colors[plot_df$linkage_status]),
                     textinfo = "label+percent",
                     hoverinfo = "text",
                     text = ~paste0(linkage_status, ": ", n, " samples")) %>%
        plotly::layout(title = "Biobank Linkage Quality")
    })

    output$unmatched_table <- DT::renderDT({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(
          tibble::tibble(Message = "No data available"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      unmatched <- get_unmatched_extractions(df)

      if (!nrow(unmatched)) {
        return(DT::datatable(
          tibble::tibble(Message = "All samples are matched to biobank!"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      DT::datatable(
        unmatched,
        options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 10)),
        rownames = FALSE,
        filter = "top",
        caption = sprintf("Showing %d unmatched extraction records", nrow(unmatched))
      )
    })

    output$mismatch_table <- DT::renderDT({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(
          tibble::tibble(Message = "No data available"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      mismatches <- get_health_structure_mismatches(df)

      if (!nrow(mismatches)) {
        return(DT::datatable(
          tibble::tibble(Message = "No health structure mismatches found!"),
          options = list(dom = "t"), rownames = FALSE
        ))
      }

      DT::datatable(
        mismatches,
        options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 10)),
        rownames = FALSE,
        filter = "top",
        caption = sprintf("Showing %d health structure mismatches", nrow(mismatches))
      )
    })

    output$extraction_table <- DT::renderDT({
      df <- extraction_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No extraction data available for the selected filters."), options = list(dom = "t"), rownames = FALSE))
      }

      # Select key columns for display
      display_cols <- c("sample_id", "extraction_date", "health_structure", "drs_state",
                       "drs_volume_ml", "extract_quality", "ready_for_freezer",
                       "biobank_matched", "health_structure_match")

      table_df <- df %>%
        dplyr::select(dplyr::any_of(display_cols)) %>%
        dplyr::arrange(dplyr::desc(extraction_date)) %>%
        dplyr::mutate(
          extraction_date = format(extraction_date, "%Y-%m-%d"),
          drs_volume_ml = ifelse(is.na(drs_volume_ml), "", scales::number(drs_volume_ml, accuracy = 0.1)),
          ready_for_freezer = dplyr::if_else(ready_for_freezer, "Yes", "No"),
          biobank_matched = dplyr::if_else(biobank_matched %||% FALSE, "Yes", "No"),
          health_structure_match = dplyr::case_when(
            is.na(health_structure_match) ~ "N/A",
            health_structure_match ~ "Match",
            TRUE ~ "Mismatch"
          )
        )

      DT::datatable(
        table_df,
        options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 15)),
        rownames = FALSE,
        filter = "top"
      )
    })
  }, filtered_data = filtered_data, biobank_data = biobank_data)
}
