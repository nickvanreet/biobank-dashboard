# R/modules/mod_04_extractions.R
# Extraction Quality Module (04)
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

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
          title = "Linked to Biobank",
          value = textOutput(ns("kpi_linked")),
          showcase = icon("link"),
          theme = "success"
        ),
        value_box(
          title = "Total Samples",
          value = textOutput(ns("kpi_total")),
          showcase = icon("vial"),
          theme = "primary"
        ),
        value_box(
          title = "Mean Volume (mL)",
          value = textOutput(ns("kpi_mean_volume")),
          showcase = icon("flask"),
          theme = "info"
        ),
        value_box(
          title = "Top RSC Run",
          value = textOutput(ns("kpi_top_run")),
          showcase = icon("gauge"),
          theme = "secondary"
        ),
        value_box(
          title = "Top RSC Position",
          value = textOutput(ns("kpi_top_position")),
          showcase = icon("border-all"),
          theme = "warning"
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("État DRS Distribution"),
          card_body_fill(
            plotly::plotlyOutput(ns("drs_state_plot"), height = "320px")
          )
        ),
        card(
          card_header("Évaluation de l'extrait"),
          card_body_fill(
            plotly::plotlyOutput(ns("extract_quality_plot"), height = "320px")
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Extraction Volume Over Time"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_timeseries_plot"), height = "360px")
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Volume par Structure Sanitaire"),
          card_body_fill(
            plotly::plotlyOutput(ns("structure_volume_plot"), height = "360px")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",
        card(
          card_header("RSC Usage"),
          card_body_fill(
            navset_card_tab(
              nav_panel(
                "Positions",
                plotly::plotlyOutput(ns("rsc_position_plot"), height = "320px")
              ),
              nav_panel(
                "Runs",
                plotly::plotlyOutput(ns("rsc_run_plot"), height = "320px")
              )
            )
          )
        ),
        card(
          card_header("Technician Activity"),
          card_body(
            DT::DTOutput(ns("technician_table"))
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Structure Sanitaire Volume Summary"),
          card_body(
            DT::DTOutput(ns("structure_volume_table"))
          )
        )
      ),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header("Extraction Detail"),
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

mod_extractions_server <- function(id, filtered_data, biobank_data = NULL) {
  if (missing(filtered_data)) {
    stop("filtered_data reactive is required for mod_extractions_server()", call. = FALSE)
  }

  moduleServer(
    id,
    function(input, output, session) {
      safe_mean <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) return(NA_real_)
        mean(x)
      }

      safe_median <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) return(NA_real_)
        stats::median(x)
      }

      safe_max_date <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) return(as.Date(NA))
        max(x)
      }

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

        needs_link <- TRUE
        if ("biobank_matched" %in% names(df)) {
          if (any(!is.na(df$biobank_matched))) {
            needs_link <- FALSE
          }
        }

        linked_df <- if (!is.null(bio_df) && needs_link) {
          tryCatch(
            link_extraction_to_biobank(df, bio_df),
            error = function(e) {
              message("Failed to link extraction data: ", e$message)
              df
            }
          )
        } else {
          df
        }

        if (!"health_structure" %in% names(linked_df)) {
          linked_df$health_structure <- NA_character_
        }

        if ("biobank_health_facility" %in% names(linked_df)) {
          linked_df <- linked_df %>%
            dplyr::mutate(
              health_structure = dplyr::coalesce(
                dplyr::na_if(.data$health_structure, "Unspecified"),
                dplyr::na_if(.data$health_structure, ""),
                .data$biobank_health_facility,
                .data$health_structure
              )
            )
        }

        linked_df
      })

      metrics <- reactive({
        summarise_extraction_metrics(extraction_data())
      })

      rsc_usage <- reactive({
        df <- extraction_data()
        empty_positions <- tibble::tibble(rsc_position = character(), n = integer())
        empty_runs <- tibble::tibble(rsc_run = character(), n = integer())

        if (is.null(df) || !nrow(df)) {
          return(list(
            total = 0,
            top_run = NA_character_,
            top_position = NA_character_,
            positions = empty_positions,
            runs = empty_runs
          ))
        }

        positions <- df %>%
          dplyr::filter(!is.na(.data$rsc_position) & .data$rsc_position != "") %>%
          dplyr::count(.data$rsc_position, name = "n") %>%
          dplyr::arrange(dplyr::desc(.data$n))

        runs <- df %>%
          dplyr::filter(!is.na(.data$rsc_run) & .data$rsc_run != "") %>%
          dplyr::count(.data$rsc_run, name = "n") %>%
          dplyr::arrange(dplyr::desc(.data$n))

        list(
          total = nrow(df),
          top_run = if (nrow(runs)) sprintf("%s (%s)", runs$rsc_run[1], scales::comma(runs$n[1])) else NA_character_,
          top_position = if (nrow(positions)) sprintf("%s (%s)", positions$rsc_position[1], scales::comma(positions$n[1])) else NA_character_,
          positions = positions,
          runs = runs
        )
      })

      structure_summary <- reactive({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(tibble::tibble())
        }

        df %>%
          dplyr::mutate(
            structure = dplyr::coalesce(
              dplyr::na_if(.data$health_structure, ""),
              dplyr::na_if(.data$health_structure, "Unspecified"),
              .data$biobank_health_facility,
              "Unspecified"
            )
          ) %>%
          dplyr::group_by(.data$structure) %>%
          dplyr::summarise(
            samples = dplyr::n(),
            linked = sum(dplyr::coalesce(.data$biobank_matched, FALSE)),
            total_volume = sum(.data$drs_volume_ml, na.rm = TRUE),
            mean_volume = safe_mean(.data$drs_volume_ml),
            median_volume = safe_median(.data$drs_volume_ml),
            latest_extraction = safe_max_date(.data$extraction_date),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            linked_rate = dplyr::if_else(.data$samples > 0, .data$linked / .data$samples, NA_real_)
          ) %>%
          dplyr::arrange(dplyr::desc(.data$total_volume))
      })

      technician_summary <- reactive({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(tibble::tibble())
        }

        df %>%
          dplyr::mutate(
            technician = dplyr::coalesce(
              dplyr::na_if(.data$technician, ""),
              "Unspecified"
            )
          ) %>%
          dplyr::group_by(.data$technician) %>%
          dplyr::summarise(
            samples = dplyr::n(),
            linked = sum(dplyr::coalesce(.data$biobank_matched, FALSE)),
            total_volume = sum(.data$drs_volume_ml, na.rm = TRUE),
            mean_volume = safe_mean(.data$drs_volume_ml),
            latest_extraction = safe_max_date(.data$extraction_date),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            linked_rate = dplyr::if_else(.data$samples > 0, .data$linked / .data$samples, NA_real_)
          ) %>%
          dplyr::arrange(dplyr::desc(.data$samples))
      })

      volume_timeseries <- reactive({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(tibble::tibble())
        }

        df %>%
          dplyr::filter(!is.na(.data$extraction_date)) %>%
          dplyr::mutate(week = lubridate::floor_date(.data$extraction_date, "week")) %>%
          dplyr::filter(!is.na(.data$week)) %>%
          dplyr::group_by(.data$week) %>%
          dplyr::summarise(
            samples = dplyr::n(),
            total_volume = sum(.data$drs_volume_ml, na.rm = TRUE),
            mean_volume = safe_mean(.data$drs_volume_ml),
            .groups = "drop"
          ) %>%
          dplyr::arrange(.data$week)
      })

      output$kpi_linked <- renderText({
        m <- metrics()
        if (is.null(m$linked_total) || is.na(m$linked_total)) {
          "--"
        } else {
          scales::comma(m$linked_total)
        }
      })

      output$kpi_total <- renderText({
        m <- metrics()
        if (is.null(m$total) || is.na(m$total)) {
          "--"
        } else {
          scales::comma(m$total)
        }
      })

      output$kpi_mean_volume <- renderText({
        m <- metrics()
        if (is.null(m$mean_volume) || is.na(m$mean_volume)) {
          "--"
        } else {
          scales::number(m$mean_volume, accuracy = 0.1)
        }
      })

      output$kpi_top_run <- renderText({
        usage <- rsc_usage()
        if (is.null(usage$top_run) || is.na(usage$top_run)) {
          "--"
        } else {
          usage$top_run
        }
      })

      output$kpi_top_position <- renderText({
        usage <- rsc_usage()
        if (is.null(usage$top_position) || is.na(usage$top_position)) {
          "--"
        } else {
          usage$top_position
        }
      })

      output$drs_state_plot <- plotly::renderPlotly({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No extraction data available"))
        }

        plot_df <- df %>%
          dplyr::mutate(
            drs_state = dplyr::coalesce(.data$drs_state, "Unknown"),
            drs_state_code = dplyr::coalesce(.data$drs_state_code, stringr::str_sub(.data$drs_state, 1, 1)),
            drs_state = factor(.data$drs_state, levels = c("Liquid", "Viscous", "Coagulated", "Unknown"), ordered = TRUE),
            code_display = dplyr::if_else(is.na(.data$drs_state_code), "?", .data$drs_state_code)
          ) %>%
          dplyr::count(.data$drs_state, .data$code_display, name = "samples")

        if (!nrow(plot_df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No état DRS information"))
        }

        plotly::plot_ly(
          plot_df,
          x = ~drs_state,
          y = ~samples,
          type = "bar",
          text = ~paste0("Code: ", code_display),
          hovertemplate = "%{x}<br>Samples: %{y}<br>%{text}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "État DRS"),
            yaxis = list(title = "Samples"),
            showlegend = FALSE
          )
      })

      output$extract_quality_plot <- plotly::renderPlotly({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No extraction data available"))
        }

        plot_df <- df %>%
          dplyr::mutate(
            extract_quality = dplyr::coalesce(.data$extract_quality, "Unknown"),
            extract_quality_code = dplyr::coalesce(.data$extract_quality_code, stringr::str_sub(.data$extract_quality, 1, 1)),
            extract_quality = factor(.data$extract_quality, levels = c("Clear", "Foncé", "Échec", "Unknown"), ordered = TRUE),
            code_display = dplyr::if_else(is.na(.data$extract_quality_code), "?", .data$extract_quality_code)
          ) %>%
          dplyr::count(.data$extract_quality, .data$code_display, name = "samples")

        if (!nrow(plot_df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No extract evaluation data"))
        }

        plotly::plot_ly(
          plot_df,
          x = ~extract_quality,
          y = ~samples,
          type = "bar",
          text = ~paste0("Code: ", code_display),
          hovertemplate = "%{x}<br>Samples: %{y}<br>%{text}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Évaluation"),
            yaxis = list(title = "Samples"),
            showlegend = FALSE
          )
      })

      output$volume_timeseries_plot <- plotly::renderPlotly({
        ts_df <- volume_timeseries()
        if (is.null(ts_df) || !nrow(ts_df)) {
          return(plotly::plotly_empty(type = "scatter") %>% plotly::layout(title = "No dated extraction records"))
        }

        p <- plotly::plot_ly(
          ts_df,
          x = ~week,
          y = ~total_volume,
          type = "bar",
          name = "Total Volume (mL)",
          marker = list(color = "#2980B9"),
          customdata = ~samples,
          hovertemplate = "Week of %{x|%Y-%m-%d}<br>Total volume: %{y:.1f} mL<br>Samples: %{customdata}<extra></extra>"
        )

        if (any(!is.na(ts_df$mean_volume))) {
          p <- p %>%
            plotly::add_trace(
              y = ~mean_volume,
              type = "scatter",
              mode = "lines+markers",
              name = "Mean Volume (mL)",
              yaxis = "y2",
              line = list(color = "#E67E22"),
              hovertemplate = "Week of %{x|%Y-%m-%d}<br>Mean volume: %{y:.1f} mL<extra></extra>"
            )
        }

        p %>%
          plotly::layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Total Volume (mL)"),
            yaxis2 = list(title = "Mean Volume (mL)", overlaying = "y", side = "right"),
            barmode = "group",
            hovermode = "x unified",
            legend = list(orientation = "h")
          )
      })

      output$structure_volume_plot <- plotly::renderPlotly({
        summary_df <- structure_summary()
        if (is.null(summary_df) || !nrow(summary_df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No structure data available"))
        }

        plot_df <- summary_df %>%
          dplyr::mutate(
            structure = factor(.data$structure, levels = summary_df$structure),
            total_display = dplyr::if_else(is.na(.data$total_volume), "--", scales::number(.data$total_volume, accuracy = 0.1)),
            mean_display = dplyr::if_else(is.na(.data$mean_volume), "--", scales::number(.data$mean_volume, accuracy = 0.1)),
            linkage_display = dplyr::if_else(is.na(.data$linked_rate), "--", scales::percent(.data$linked_rate, accuracy = 0.1)),
            hover_text = paste0(
              "Structure: ", .data$structure, "<br>",
              "Samples: ", scales::comma(.data$samples), "<br>",
              "Linked: ", scales::comma(.data$linked), " (", linkage_display, ")<br>",
              "Mean volume: ", mean_display, " mL"
            )
          )

        plotly::plot_ly(
          plot_df,
          x = ~structure,
          y = ~total_volume,
          type = "bar",
          text = ~hover_text,
          hoverinfo = "text",
          name = "Total Volume (mL)"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Structure Sanitaire", tickangle = -45),
            yaxis = list(title = "Total Volume (mL)"),
            margin = list(b = 140)
          )
      })

      output$rsc_position_plot <- plotly::renderPlotly({
        usage <- rsc_usage()
        pos_df <- usage$positions
        if (is.null(pos_df) || !nrow(pos_df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No RSC position data available"))
        }

        plot_df <- pos_df %>%
          dplyr::mutate(
            rsc_position = factor(.data$rsc_position, levels = rev(.data$rsc_position)),
            hover_text = paste0("Position: ", .data$rsc_position, "<br>Samples: ", scales::comma(.data$n))
          )

        plotly::plot_ly(
          plot_df,
          x = ~n,
          y = ~rsc_position,
          type = "bar",
          orientation = "h",
          text = ~hover_text,
          hoverinfo = "text",
          marker = list(color = "#8E44AD")
        ) %>%
          plotly::layout(
            xaxis = list(title = "Samples"),
            yaxis = list(title = "RSC Position"),
            margin = list(l = 140)
          )
      })

      output$rsc_run_plot <- plotly::renderPlotly({
        usage <- rsc_usage()
        run_df <- usage$runs
        if (is.null(run_df) || !nrow(run_df)) {
          return(plotly::plotly_empty(type = "bar") %>% plotly::layout(title = "No RSC run data available"))
        }

        plot_df <- run_df %>%
          dplyr::mutate(
            rsc_run = factor(.data$rsc_run, levels = rev(.data$rsc_run)),
            hover_text = paste0("Run: ", .data$rsc_run, "<br>Samples: ", scales::comma(.data$n))
          )

        plotly::plot_ly(
          plot_df,
          x = ~n,
          y = ~rsc_run,
          type = "bar",
          orientation = "h",
          text = ~hover_text,
          hoverinfo = "text",
          marker = list(color = "#1ABC9C")
        ) %>%
          plotly::layout(
            xaxis = list(title = "Samples"),
            yaxis = list(title = "RSC Run"),
            margin = list(l = 140)
          )
      })

      output$technician_table <- DT::renderDT({
        summary_df <- technician_summary()
        if (is.null(summary_df) || !nrow(summary_df)) {
          return(DT::datatable(
            tibble::tibble(Message = "No technician data available"),
            options = list(dom = "t"),
            rownames = FALSE
          ))
        }

        display_df <- summary_df %>%
          dplyr::mutate(
            linkage_rate = dplyr::if_else(is.na(.data$linked_rate), "--", scales::percent(.data$linked_rate, accuracy = 0.1)),
            total_volume = dplyr::if_else(is.na(.data$total_volume), "--", scales::number(.data$total_volume, accuracy = 0.1)),
            mean_volume = dplyr::if_else(is.na(.data$mean_volume), "--", scales::number(.data$mean_volume, accuracy = 0.1)),
            latest_extraction = dplyr::if_else(is.na(.data$latest_extraction), "", format(.data$latest_extraction, "%Y-%m-%d"))
          ) %>%
          dplyr::select(
            Technician = technician,
            Samples = samples,
            Linked = linked,
            `Linkage Rate` = linkage_rate,
            `Total Volume (mL)` = total_volume,
            `Mean Volume (mL)` = mean_volume,
            `Last Extraction` = latest_extraction
          )

        DT::datatable(
          display_df,
          options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 8)),
          rownames = FALSE
        )
      })

      output$structure_volume_table <- DT::renderDT({
        summary_df <- structure_summary()
        if (is.null(summary_df) || !nrow(summary_df)) {
          return(DT::datatable(
            tibble::tibble(Message = "No structure data available"),
            options = list(dom = "t"),
            rownames = FALSE
          ))
        }

        display_df <- summary_df %>%
          dplyr::mutate(
            linkage_rate = dplyr::if_else(is.na(.data$linked_rate), "--", scales::percent(.data$linked_rate, accuracy = 0.1)),
            total_volume = dplyr::if_else(is.na(.data$total_volume), "--", scales::number(.data$total_volume, accuracy = 0.1)),
            mean_volume = dplyr::if_else(is.na(.data$mean_volume), "--", scales::number(.data$mean_volume, accuracy = 0.1)),
            median_volume = dplyr::if_else(is.na(.data$median_volume), "--", scales::number(.data$median_volume, accuracy = 0.1)),
            latest_extraction = dplyr::if_else(is.na(.data$latest_extraction), "", format(.data$latest_extraction, "%Y-%m-%d"))
          ) %>%
          dplyr::select(
            `Structure Sanitaire` = structure,
            Samples = samples,
            Linked = linked,
            `Linkage Rate` = linkage_rate,
            `Total Volume (mL)` = total_volume,
            `Mean Volume (mL)` = mean_volume,
            `Median Volume (mL)` = median_volume,
            `Last Extraction` = latest_extraction
          )

        DT::datatable(
          display_df,
          options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 10)),
          rownames = FALSE,
          filter = "top"
        )
      })

      output$extraction_table <- DT::renderDT({
        df <- extraction_data()
        if (is.null(df) || !nrow(df)) {
          return(DT::datatable(
            tibble::tibble(Message = "No extraction data available for the selected filters."),
            options = list(dom = "t"),
            rownames = FALSE
          ))
        }

        table_df <- df %>%
          dplyr::arrange(dplyr::desc(.data$extraction_date), .data$sample_id) %>%
          dplyr::transmute(
            `Sample ID (KPS)` = .data$sample_id,
            `KPS Normalized` = dplyr::coalesce(.data$barcode_normalized, ""),
            `Numero` = dplyr::coalesce(.data$record_number, ""),
            `Numero Normalized` = dplyr::coalesce(.data$record_number_normalized, ""),
            `Biobank Barcode` = dplyr::coalesce(.data$biobank_barcode, ""),
            `Biobank Numero` = dplyr::coalesce(.data$biobank_lab_id, ""),
            `Numero Match` = dplyr::case_when(
              is.na(.data$numero_match) ~ "Unknown",
              .data$numero_match ~ "Match",
              TRUE ~ "Mismatch"
            ),
            `Linked to Biobank` = dplyr::case_when(
              is.na(.data$biobank_matched) ~ "Unknown",
              .data$biobank_matched ~ "Yes",
              TRUE ~ "No"
            ),
            `Extraction Date` = dplyr::if_else(
              is.na(.data$extraction_date),
              "",
              format(.data$extraction_date, "%Y-%m-%d")
            ),
            Technician = dplyr::coalesce(.data$technician, ""),
            `Structure Sanitaire` = dplyr::coalesce(.data$health_structure, ""),
            `Biobank Structure` = dplyr::coalesce(.data$biobank_health_facility, ""),
            `État DRS` = dplyr::coalesce(.data$drs_state, ""),
            `État Code` = dplyr::coalesce(.data$drs_state_code, ""),
            `Évaluation` = dplyr::coalesce(.data$extract_quality, ""),
            `Évaluation Code` = dplyr::coalesce(.data$extract_quality_code, ""),
            `Volume (mL)` = dplyr::if_else(
              is.na(.data$drs_volume_ml),
              "",
              scales::number(.data$drs_volume_ml, accuracy = 0.1)
            ),
            `RSC Run` = dplyr::coalesce(.data$rsc_run, ""),
            `RSC Position` = dplyr::coalesce(.data$rsc_position, ""),
            `Rack` = dplyr::coalesce(.data$rack, ""),
            `Rack Row` = dplyr::coalesce(.data$rack_row, ""),
            `Rack Column` = dplyr::coalesce(.data$rack_column, ""),
            Remarks = dplyr::coalesce(.data$remarks, "")
          )

        DT::datatable(
          table_df,
          options = c(APP_CONSTANTS$DT_OPTIONS, list(pageLength = 15)),
          rownames = FALSE,
          filter = "top"
        )
      })
    }
  )
}
