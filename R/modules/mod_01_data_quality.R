# R/modules/mod_01_data_quality.R
# Data Quality Module - Redesigned with cleaner, less redundant UI
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
    icon = icon("shield"),

    div(class = "container-fluid",

        # ==== HERO SECTION: Quality Score + Key Metrics ========================
        layout_columns(
          col_widths = c(4, 8),
          gap = "16px",

          # Quality Score Gauge Card
          card(
            card_header(
              class = "bg-primary text-white",
              span(class = "fw-bold", "Data Quality Score")
            ),
            card_body(
              class = "d-flex flex-column align-items-center justify-content-center",
              style = "min-height: 280px;",
              plotly::plotlyOutput(ns("quality_gauge"), height = "220px"),
              div(
                class = "text-center mt-2",
                uiOutput(ns("quality_grade_badge"))
              )
            )
          ),

          # Key Metrics Summary
          card(
            card_header("Quality Overview"),
            card_body(
              layout_column_wrap(
                width = 1/3, gap = "12px",

                # Valid Records
                div(
                  class = "p-3 rounded",
                  style = "background: linear-gradient(135deg, #d4edda 0%, #c3e6cb 100%); border-left: 4px solid #28a745;",
                  div(class = "d-flex align-items-center gap-2",
                      icon("check-circle", class = "text-success fa-2x"),
                      div(
                        div(class = "text-muted small", "Valid Records"),
                        div(class = "fs-4 fw-bold text-success", textOutput(ns("valid_rows"), inline = TRUE))
                      )
                  )
                ),

                # Invalid Records
                div(
                  class = "p-3 rounded",
                  style = "background: linear-gradient(135deg, #f8d7da 0%, #f5c6cb 100%); border-left: 4px solid #dc3545;",
                  div(class = "d-flex align-items-center gap-2",
                      icon("exclamation-triangle", class = "text-danger fa-2x"),
                      div(
                        div(class = "text-muted small", "Invalid Records"),
                        div(class = "fs-4 fw-bold text-danger", textOutput(ns("invalid_rows"), inline = TRUE))
                      )
                  )
                ),

                # Completeness
                div(
                  class = "p-3 rounded",
                  style = "background: linear-gradient(135deg, #cce5ff 0%, #b8daff 100%); border-left: 4px solid #007bff;",
                  div(class = "d-flex align-items-center gap-2",
                      icon("chart-pie", class = "text-primary fa-2x"),
                      div(
                        div(class = "text-muted small", "Avg. Completeness"),
                        div(class = "fs-4 fw-bold text-primary", textOutput(ns("avg_completeness"), inline = TRUE))
                      )
                  )
                )
              ),

              # Issues Summary Mini Cards
              div(class = "mt-3",
                  div(class = "text-muted small mb-2 fw-bold", "Issues Detected"),
                  layout_column_wrap(
                    width = 1/5, gap = "8px",

                    div(class = "text-center p-2 rounded bg-light",
                        div(class = "small text-muted", "Missing Barcode"),
                        div(class = "fw-bold", textOutput(ns("missing_barcode"), inline = TRUE))
                    ),
                    div(class = "text-center p-2 rounded bg-light",
                        div(class = "small text-muted", "Missing Lab ID"),
                        div(class = "fw-bold", textOutput(ns("missing_labid"), inline = TRUE))
                    ),
                    div(class = "text-center p-2 rounded bg-light",
                        div(class = "small text-muted", "Duplicates"),
                        div(class = "fw-bold", textOutput(ns("duplicates"), inline = TRUE))
                    ),
                    div(class = "text-center p-2 rounded bg-light",
                        div(class = "small text-muted", "Barcode Conflicts"),
                        div(class = "fw-bold", textOutput(ns("conflicts"), inline = TRUE))
                    ),
                    div(class = "text-center p-2 rounded bg-light",
                        div(class = "small text-muted", "Lab ID Conflicts"),
                        div(class = "fw-bold", textOutput(ns("labid_conflicts"), inline = TRUE))
                    )
                  )
              )
            )
          )
        ),

        # ==== TABBED CONTENT ===================================================
        navset_card_tab(
          id = ns("quality_tabs"),

          # ---- TAB 1: Issues Analysis -----------------------------------------
          nav_panel(
            title = "Issues Analysis",
            icon = icon("chart-pie"),

            layout_columns(
              col_widths = c(5, 7),
              gap = "16px",

              # Issues Breakdown Sunburst
              card(
                card_header("Issues Breakdown"),
                card_body_fill(
                  plotly::plotlyOutput(ns("issues_sunburst"), height = "400px")
                )
              ),

              # Quality Timeline (combined)
              card(
                card_header(
                  class = "d-flex justify-content-between align-items-center",
                  span("Quality Trends Over Time"),
                  checkboxInput(
                    ns("include_flagged"),
                    label = "Show flagged rows",
                    value = TRUE
                  )
                ),
                card_body_fill(
                  plotly::plotlyOutput(ns("quality_timeline"), height = "400px")
                )
              )
            ),

            # Invalid Rows Table (consolidated)
            card(
              card_header(
                class = "d-flex justify-content-between align-items-center",
                div(
                  span("Invalid Records"),
                  span(class = "badge bg-danger ms-2", textOutput(ns("invalid_count_badge"), inline = TRUE))
                ),
                div(
                  class = "d-flex gap-2 align-items-center",
                  selectInput(
                    ns("issue_filter"),
                    label = NULL,
                    choices = c("All Issues" = "all"),
                    width = "200px"
                  )
                )
              ),
              card_body(
                DT::DTOutput(ns("invalid_rows_table"))
              )
            )
          ),

          # ---- TAB 2: Completeness Analysis -----------------------------------
          nav_panel(
            title = "Completeness",
            icon = icon("tasks"),

            layout_columns(
              col_widths = c(8, 4),
              gap = "16px",

              # Interactive Completeness Chart
              card(
                card_header("Column Completeness"),
                card_body_fill(
                  plotly::plotlyOutput(ns("completeness_chart"), height = "500px")
                )
              ),

              # Completeness Summary
              card(
                card_header("Completeness Summary"),
                card_body(
                  div(
                    class = "mb-3",
                    div(class = "d-flex justify-content-between align-items-center mb-2",
                        span(class = "fw-bold", "Overall Completeness"),
                        span(class = "badge bg-primary fs-6", textOutput(ns("overall_completeness"), inline = TRUE))
                    ),
                    div(class = "progress", style = "height: 20px;",
                        div(
                          class = "progress-bar bg-success",
                          role = "progressbar",
                          style = htmltools::css(width = textOutput(ns("completeness_bar_width"))),
                          textOutput(ns("completeness_bar_text"), inline = TRUE)
                        )
                    )
                  ),
                  hr(),
                  div(class = "small",
                      div(class = "d-flex justify-content-between py-1",
                          div(span(class = "badge bg-success me-2", " "), "Good (≥90%)"),
                          textOutput(ns("good_count"), inline = TRUE)
                      ),
                      div(class = "d-flex justify-content-between py-1",
                          div(span(class = "badge bg-warning me-2", " "), "Fair (70-89%)"),
                          textOutput(ns("fair_count"), inline = TRUE)
                      ),
                      div(class = "d-flex justify-content-between py-1",
                          div(span(class = "badge bg-orange me-2", style = "background-color: #E67E22 !important;", " "), "Poor (50-69%)"),
                          textOutput(ns("poor_count"), inline = TRUE)
                      ),
                      div(class = "d-flex justify-content-between py-1",
                          div(span(class = "badge bg-danger me-2", " "), "Critical (<50%)"),
                          textOutput(ns("critical_count"), inline = TRUE)
                      )
                  ),
                  hr(),
                  div(class = "small text-muted",
                      icon("info-circle"), " Columns with low completeness may need attention or could indicate optional fields."
                  )
                )
              )
            )
          ),

          # ---- TAB 3: Geographic Analysis -------------------------------------
          nav_panel(
            title = "Geographic View",
            icon = icon("map"),

            card(
              card_header("Data Quality Issues by Health Zone and Week"),
              card_body_fill(
                plotly::plotlyOutput(ns("quality_heatmap"), height = "500px")
              ),
              card_footer(
                class = "text-muted small",
                icon("info-circle"), " Darker colors indicate more issues. White cells have no issues or no data."
              )
            )
          ),

          # ---- TAB 4: Detailed Records ----------------------------------------
          nav_panel(
            title = "Detailed Records",
            icon = icon("table"),

            layout_columns(
              col_widths = c(4, 4, 4),
              gap = "16px",

              card(
                card_header(
                  class = "bg-light",
                  div(class = "d-flex align-items-center gap-2",
                      icon("copy", class = "text-primary"),
                      span("Duplicate Records")
                  )
                ),
                card_body(DT::DTOutput(ns("duplicates_table")))
              ),

              card(
                card_header(
                  class = "bg-light",
                  div(class = "d-flex align-items-center gap-2",
                      icon("barcode", class = "text-danger"),
                      span("Barcode Conflicts")
                  )
                ),
                card_body(DT::DTOutput(ns("conflicts_table")))
              ),

              card(
                card_header(
                  class = "bg-light",
                  div(class = "d-flex align-items-center gap-2",
                      icon("id-card", class = "text-warning"),
                      span("Lab ID Conflicts")
                  )
                ),
                card_body(DT::DTOutput(ns("labid_conflicts_table")))
              )
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

    # ---- small helpers -------------------------------------------------------
    .find_col <- function(nms, patterns) {
      for (p in patterns) {
        hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
        if (length(hit)) return(hit[1])
      }
      NULL
    }
    .match_cols <- function(nms, patterns) {
      unique(unlist(lapply(patterns, function(p) {
        grep(p, nms, ignore.case = TRUE, value = TRUE)
      })))
    }
    .nz <- function(x) {
      x <- trimws(as.character(x))
      x[x == ""] <- NA
      x
    }

    # ========================================================================
    # REACTIVE CALCULATIONS
    # ========================================================================
    quality_metrics <- reactive({
      req(quality_report())
      report <- quality_report()

      # defensively ensure fields exist
      if (is.null(report$summary)) report$summary <- list(rows_raw = NA_integer_, rows_clean = NA_integer_)
      if (is.null(report$duplicates)) report$duplicates <- data.frame()
      if (is.null(report$barcode_conflicts)) report$barcode_conflicts <- data.frame()
      if (is.null(report$completeness)) report$completeness <- data.frame(percent_complete = numeric())
      if (is.null(report$labid_conflicts)) report$labid_conflicts <- data.frame()

      # Compute adjusted "total_rows" excluding ghost rows (both ID fields NA)
      rd  <- try(raw_data(), silent = TRUE)
      nms <- if (!inherits(rd, "try-error")) names(rd) else character()

      barcode_col <- if ("barcode" %in% nms) "barcode" else
        .find_col(nms, c("code.*barres.*kps", "\\bbarcode\\b"))
      labid_col <- if ("lab_id" %in% nms) "lab_id" else
        .find_col(nms, c("^num[eé]ro$", "^numero$", "lab.?id"))

      if (!is.null(barcode_col) && !is.null(labid_col) && !inherits(rd, "try-error")) {
        b <- .nz(rd[[barcode_col]])
        l <- .nz(rd[[labid_col]])

        keep <- !(is.na(b) & is.na(l))
        adj_total <- sum(keep)

        missing_barcode_adj <- sum(is.na(b[keep]))
        missing_labid_adj   <- sum(is.na(l[keep]))
      } else {
        adj_total <- suppressWarnings(as.integer(report$summary$rows_raw))
        missing_barcode_adj <- if (!is.null(report$missing_barcode)) report$missing_barcode else NA_integer_
        missing_labid_adj   <- if (!is.null(report$missing_labid))   report$missing_labid   else NA_integer_
      }

      valid_rows   <- suppressWarnings(as.integer(report$summary$rows_clean))
      dropped_rows <- if (is.finite(adj_total) && is.finite(valid_rows)) max(adj_total - valid_rows, 0L) else NA_integer_
      drop_rate    <- if (isTRUE(adj_total > 0) && is.finite(dropped_rows)) dropped_rows / adj_total * 100 else NA_real_

      avg_comp <- if (nrow(report$completeness)) {
        mean(report$completeness$percent_complete, na.rm = TRUE)
      } else NA_real_

      # Calculate overall quality score (0-100)
      validity_rate <- if (isTRUE(adj_total > 0) && is.finite(valid_rows)) {
        (valid_rows / adj_total) * 100
      } else NA_real_

      quality_score <- if (is.finite(validity_rate) && is.finite(avg_comp)) {
        (validity_rate * 0.7) + (avg_comp * 0.3)
      } else if (is.finite(validity_rate)) {
        validity_rate * 0.7
      } else if (is.finite(avg_comp)) {
        avg_comp * 0.3
      } else NA_real_

      list(
        total_rows        = adj_total,
        valid_rows        = valid_rows,
        dropped_rows      = dropped_rows,
        drop_rate         = drop_rate,
        missing_barcode   = missing_barcode_adj,
        missing_labid     = missing_labid_adj,
        duplicate_count   = nrow(report$duplicates),
        conflict_count    = nrow(report$barcode_conflicts),
        labid_conflict_count = nrow(report$labid_conflicts),
        avg_completeness  = avg_comp,
        quality_score     = quality_score
      )
    })

    # Update issue filter choices
    observe({
      req(quality_report())
      report <- quality_report()

      choices <- c("All Issues" = "all")
      if (!is.null(report$row_flags_detailed)) {
        reasons <- unique(report$row_flags_detailed$reason)
        reasons <- reasons[reasons != "OK" & !is.na(reasons)]
        if (length(reasons)) {
          named_reasons <- setNames(reasons, reasons)
          choices <- c(choices, named_reasons)
        }
      }

      updateSelectInput(session, "issue_filter", choices = choices)
    })

    # ========================================================================
    # OUTPUTS - KEY METRICS
    # ========================================================================
    output$valid_rows <- renderText({
      m <- quality_metrics()
      if (is.na(m$valid_rows) || is.na(m$drop_rate)) "—" else
        sprintf("%s (%.1f%%)", scales::comma(m$valid_rows), 100 - m$drop_rate)
    })

    output$invalid_rows <- renderText({
      m <- quality_metrics()
      if (is.na(m$dropped_rows) || is.na(m$drop_rate)) "—" else
        sprintf("%s (%.1f%%)", scales::comma(m$dropped_rows), m$drop_rate)
    })

    output$avg_completeness <- renderText({
      m <- quality_metrics()
      if (is.na(m$avg_completeness)) "—" else sprintf("%.1f%%", m$avg_completeness)
    })

    output$missing_barcode <- renderText({
      m <- quality_metrics()
      if (is.na(m$missing_barcode)) "0" else scales::comma(m$missing_barcode)
    })

    output$missing_labid <- renderText({
      m <- quality_metrics()
      if (is.na(m$missing_labid)) "0" else scales::comma(m$missing_labid)
    })

    output$duplicates <- renderText({
      m <- quality_metrics()
      if (is.na(m$duplicate_count)) "0" else scales::comma(m$duplicate_count)
    })

    output$conflicts <- renderText({
      m <- quality_metrics()
      if (is.na(m$conflict_count)) "0" else scales::comma(m$conflict_count)
    })

    output$labid_conflicts <- renderText({
      m <- quality_metrics()
      if (is.na(m$labid_conflict_count)) "0" else scales::comma(m$labid_conflict_count)
    })

    output$invalid_count_badge <- renderText({
      m <- quality_metrics()
      if (is.na(m$dropped_rows)) "0" else scales::comma(m$dropped_rows)
    })

    # ========================================================================
    # QUALITY SCORE GAUGE
    # ========================================================================
    output$quality_gauge <- plotly::renderPlotly({
      m <- quality_metrics()
      score <- if (is.na(m$quality_score)) 0 else round(m$quality_score, 1)

      # Determine color based on score
      gauge_color <- if (score >= 90) "#28a745" else if (score >= 80) "#5cb85c" else if (score >= 70) "#f0ad4e" else if (score >= 60) "#f39c12" else "#dc3545"

      plotly::plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = score,
        number = list(suffix = "%", font = list(size = 36, color = gauge_color)),
        gauge = list(
          axis = list(
            range = list(0, 100),
            tickwidth = 1,
            tickcolor = "#dee2e6",
            tickvals = c(0, 25, 50, 75, 100),
            ticktext = c("0", "25", "50", "75", "100")
          ),
          bar = list(color = gauge_color, thickness = 0.7),
          bgcolor = "#f8f9fa",
          borderwidth = 2,
          bordercolor = "#dee2e6",
          steps = list(
            list(range = c(0, 60), color = "#ffebee"),
            list(range = c(60, 70), color = "#fff3e0"),
            list(range = c(70, 80), color = "#fffde7"),
            list(range = c(80, 90), color = "#e8f5e9"),
            list(range = c(90, 100), color = "#c8e6c9")
          ),
          threshold = list(
            line = list(color = "#212529", width = 3),
            thickness = 0.8,
            value = score
          )
        )
      ) %>%
        plotly::layout(
          margin = list(t = 30, b = 30, l = 30, r = 30),
          paper_bgcolor = "rgba(0,0,0,0)",
          plot_bgcolor = "rgba(0,0,0,0)"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    output$quality_grade_badge <- renderUI({
      m <- quality_metrics()
      score <- if (is.na(m$quality_score)) 0 else round(m$quality_score, 1)

      grade <- if (score >= 90) "A" else if (score >= 80) "B" else if (score >= 70) "C" else if (score >= 60) "D" else "F"
      grade_color <- if (score >= 90) "success" else if (score >= 80) "primary" else if (score >= 70) "warning" else if (score >= 60) "secondary" else "danger"
      grade_text <- if (score >= 90) "Excellent" else if (score >= 80) "Good" else if (score >= 70) "Fair" else if (score >= 60) "Needs Improvement" else "Critical"

      div(
        class = "text-center",
        span(class = paste0("badge bg-", grade_color, " fs-3 px-4 py-2"), paste("Grade:", grade)),
        div(class = "text-muted small mt-1", grade_text)
      )
    })

    # ========================================================================
    # ISSUES SUNBURST CHART
    # ========================================================================
    output$issues_sunburst <- plotly::renderPlotly({
      m <- quality_metrics()

      # Build issue data
      issues <- data.frame(
        category = c("Valid", "Missing Barcode", "Missing Lab ID", "Duplicates", "Barcode Conflicts", "Lab ID Conflicts"),
        count = c(
          if (is.na(m$valid_rows)) 0 else m$valid_rows,
          if (is.na(m$missing_barcode)) 0 else m$missing_barcode,
          if (is.na(m$missing_labid)) 0 else m$missing_labid,
          if (is.na(m$duplicate_count)) 0 else m$duplicate_count,
          if (is.na(m$conflict_count)) 0 else m$conflict_count,
          if (is.na(m$labid_conflict_count)) 0 else m$labid_conflict_count
        ),
        color = c("#28a745", "#ffc107", "#fd7e14", "#dc3545", "#e83e8c", "#6f42c1")
      )

      # Only show categories with values
      issues <- issues[issues$count > 0, ]

      if (nrow(issues) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(
                   text = "No data available",
                   x = 0.5, y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 16, color = "#6c757d")
                 ) %>%
                 plotly::layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE)
                 ))
      }

      plotly::plot_ly(
        issues,
        labels = ~category,
        values = ~count,
        type = "pie",
        hole = 0.5,
        marker = list(colors = ~color, line = list(color = "#ffffff", width = 2)),
        textinfo = "label+percent",
        textposition = "outside",
        hovertemplate = "<b>%{label}</b><br>Count: %{value:,}<br>Percentage: %{percent}<extra></extra>"
      ) %>%
        plotly::layout(
          showlegend = FALSE,
          margin = list(t = 20, b = 20, l = 20, r = 20),
          annotations = list(
            list(
              text = paste0("<b>", scales::comma(sum(issues$count)), "</b><br>Total"),
              x = 0.5, y = 0.5,
              font = list(size = 16, color = "#212529"),
              showarrow = FALSE
            )
          )
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # QUALITY TIMELINE (Combined)
    # ========================================================================
    output$quality_timeline <- plotly::renderPlotly({
      req(quality_report())
      report <- quality_report()

      weekly_flags <- NULL
      if (is.data.frame(report$quality_flags_by_week) &&
          all(c("week", "quality_flag", "n") %in% names(report$quality_flags_by_week))) {
        weekly_flags <- report$quality_flags_by_week %>%
          dplyr::mutate(week = suppressWarnings(as.Date(week))) %>%
          dplyr::filter(!is.na(week))
      }

      if (is.null(weekly_flags) || !nrow(weekly_flags)) {
        detailed <- report$row_flags_detailed
        if (is.data.frame(detailed) && all(c("date_sample", "reason") %in% names(detailed))) {
          weekly_flags <- detailed %>%
            dplyr::mutate(
              date_sample = as.Date(suppressWarnings(lubridate::parse_date_time(date_sample, orders = c("ymd", "dmy", "mdy"), quiet = TRUE))),
              week = lubridate::floor_date(date_sample, "week"),
              quality_flag = dplyr::case_when(
                is.na(reason) | reason == "" | reason == "OK" ~ "Valid",
                reason == "Duplicate key" ~ "Duplicate",
                reason == "Barcode conflict" ~ "Barcode conflict",
                reason == "Missing barcode" ~ "Missing barcode",
                reason == "Missing lab ID" ~ "Missing lab ID",
                reason == "Lab ID conflict" ~ "Lab ID conflict",
                TRUE ~ reason
              )
            ) %>%
            dplyr::filter(!is.na(week)) %>%
            dplyr::count(week, quality_flag, name = "n")
        }
      }

      if (is.null(weekly_flags) || !nrow(weekly_flags)) {
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = "No timeline data available",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 14, color = "#6c757d")
            ) %>%
            plotly::layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      df <- weekly_flags %>%
        dplyr::filter(!is.na(week)) %>%
        dplyr::mutate(quality_flag = as.character(quality_flag)) %>%
        dplyr::arrange(week, quality_flag)

      if (!isTRUE(input$include_flagged)) {
        df <- df %>%
          dplyr::filter(tolower(quality_flag) %in% c("valid", "ok"))
      }

      if (!nrow(df)) {
        return(
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = "No data to display",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(size = 14, color = "#6c757d")
            ) %>%
            plotly::layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE)
            )
        )
      }

      # Color scheme
      colors <- c(
        "Valid" = "#28a745",
        "Missing barcode" = "#ffc107",
        "Missing lab ID" = "#fd7e14",
        "Duplicate" = "#dc3545",
        "Barcode conflict" = "#e83e8c",
        "Lab ID conflict" = "#6f42c1"
      )

      preferred_levels <- names(colors)
      df$quality_flag <- factor(
        df$quality_flag,
        levels = unique(c(preferred_levels, setdiff(df$quality_flag, preferred_levels)))
      )

      plotly::plot_ly(
        df,
        x = ~week, y = ~n, color = ~quality_flag,
        colors = colors,
        type = "bar",
        hovertemplate = "Week: %{x|%Y-%m-%d}<br>%{fullData.name}: %{y:,}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Week", tickformat = "%b %Y"),
          yaxis = list(title = "Records"),
          legend = list(
            orientation = "h",
            y = -0.15,
            x = 0.5,
            xanchor = "center"
          ),
          margin = list(b = 80)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # INVALID ROWS TABLE (Consolidated)
    # ========================================================================
    output$invalid_rows_table <- DT::renderDT({
      req(quality_report())
      req(raw_data())
      report <- quality_report()
      rd <- raw_data()

      if (is.null(report$row_flags_detailed) || !nrow(report$row_flags_detailed)) {
        return(DT::datatable(
          data.frame(Message = "No invalid rows found"),
          options = list(dom = 't', paging = FALSE),
          class = "table-sm"
        ))
      }

      # Get rows that are invalid
      invalid_mask <- report$row_flags_detailed$reason != "OK"

      # Apply filter if selected
      filter_val <- input$issue_filter
      if (!is.null(filter_val) && filter_val != "all") {
        invalid_mask <- invalid_mask & (report$row_flags_detailed$reason == filter_val)
      }

      invalid_idx <- which(invalid_mask)

      if (!length(invalid_idx)) {
        return(DT::datatable(
          data.frame(Message = "No invalid rows found matching the filter"),
          options = list(dom = 't', paging = FALSE),
          class = "table-sm"
        ))
      }

      nms <- names(rd)

      barcode_col <- .find_col(nms, c("code.*barres.*kps", "\\bbarcode\\b"))
      labid_col <- .find_col(nms, c("^num[eé]ro$", "^numero$", "lab.?id"))
      date_col <- .find_col(nms, c("date.*pr[eé]l[eè]v", "date.*sample"))
      province_col <- .find_col(nms, c("^province$"))
      zone_col <- .find_col(nms, c("zone.*sant[eé]", "health.*zone"))
      study_col <- .find_col(nms, c("^[eé]tude$", "^study$"))

      display_df <- data.frame(
        Row = invalid_idx,
        Issue = report$row_flags_detailed$reason[invalid_idx],
        stringsAsFactors = FALSE
      )

      if (!is.null(barcode_col)) display_df$Barcode <- rd[[barcode_col]][invalid_idx]
      if (!is.null(labid_col)) display_df$`Lab ID` <- rd[[labid_col]][invalid_idx]
      if (!is.null(date_col)) display_df$Date <- as.character(rd[[date_col]][invalid_idx])
      if (!is.null(study_col)) display_df$Study <- rd[[study_col]][invalid_idx]
      if (!is.null(province_col)) display_df$Province <- rd[[province_col]][invalid_idx]
      if (!is.null(zone_col)) display_df$`Health Zone` <- rd[[zone_col]][invalid_idx]

      DT::datatable(
        display_df,
        rownames = FALSE,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(0, "asc"))
        ),
        class = "table-sm table-hover"
      ) %>%
        DT::formatStyle(
          "Issue",
          backgroundColor = DT::styleEqual(
            c("Missing barcode", "Missing lab ID", "Barcode conflict", "Lab ID conflict", "Duplicate key", "Missing sampling date"),
            c("#fff3cd", "#ffe0b2", "#f8d7da", "#e1bee7", "#ffcdd2", "#fff9c4")
          ),
          fontWeight = "bold"
        )
    })

    # ========================================================================
    # COMPLETENESS OUTPUTS
    # ========================================================================
    output$completeness_chart <- plotly::renderPlotly({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$completeness) || !nrow(report$completeness)) {
        return(plotly::plotly_empty())
      }

      df <- report$completeness %>%
        dplyr::mutate(
          percent_complete = round(percent_complete, 1),
          status = dplyr::case_when(
            percent_complete >= 90 ~ "Good",
            percent_complete >= 70 ~ "Fair",
            percent_complete >= 50 ~ "Poor",
            TRUE ~ "Critical"
          )
        ) %>%
        dplyr::arrange(percent_complete) %>%
        dplyr::mutate(column = factor(column, levels = column))

      colors <- c(
        "Good" = "#28a745",
        "Fair" = "#ffc107",
        "Poor" = "#E67E22",
        "Critical" = "#dc3545"
      )

      plotly::plot_ly(
        df,
        y = ~column,
        x = ~percent_complete,
        color = ~status,
        colors = colors,
        type = "bar",
        orientation = "h",
        hovertemplate = "<b>%{y}</b><br>Completeness: %{x:.1f}%<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(
            title = "Completeness (%)",
            range = c(0, 105),
            ticksuffix = "%"
          ),
          yaxis = list(
            title = "",
            automargin = TRUE
          ),
          barmode = "overlay",
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            y = -0.15,
            x = 0.5,
            xanchor = "center",
            title = list(text = "")
          ),
          margin = list(l = 120, b = 60)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    output$overall_completeness <- renderText({
      m <- quality_metrics()
      if (is.na(m$avg_completeness)) "—" else sprintf("%.1f%%", m$avg_completeness)
    })

    output$completeness_bar_width <- renderText({
      m <- quality_metrics()
      paste0(if (is.na(m$avg_completeness)) 0 else round(m$avg_completeness), "%")
    })

    output$completeness_bar_text <- renderText({
      m <- quality_metrics()
      if (is.na(m$avg_completeness)) "" else sprintf("%.0f%%", m$avg_completeness)
    })

    # Completeness category counts
    completeness_stats <- reactive({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$completeness) || !nrow(report$completeness)) {
        return(list(good = 0, fair = 0, poor = 0, critical = 0))
      }

      df <- report$completeness
      list(
        good = sum(df$percent_complete >= 90),
        fair = sum(df$percent_complete >= 70 & df$percent_complete < 90),
        poor = sum(df$percent_complete >= 50 & df$percent_complete < 70),
        critical = sum(df$percent_complete < 50)
      )
    })

    output$good_count <- renderText({
      stats <- completeness_stats()
      paste(stats$good, "columns")
    })

    output$fair_count <- renderText({
      stats <- completeness_stats()
      paste(stats$fair, "columns")
    })

    output$poor_count <- renderText({
      stats <- completeness_stats()
      paste(stats$poor, "columns")
    })

    output$critical_count <- renderText({
      stats <- completeness_stats()
      paste(stats$critical, "columns")
    })

    # ========================================================================
    # QUALITY HEATMAP
    # ========================================================================
    output$quality_heatmap <- plotly::renderPlotly({
      req(quality_report())
      report <- quality_report()

      if (is.null(report$row_flags_detailed) || !nrow(report$row_flags_detailed)) {
        return(plotly::plotly_empty())
      }

      has_zone <- "health_zone" %in% names(report$row_flags_detailed)
      has_date <- "date_sample" %in% names(report$row_flags_detailed)

      if (!has_zone || !has_date) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(
                   text = "Health zone or date information not available",
                   x = 0.5, y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 14, color = "#6c757d")
                 ) %>%
                 plotly::layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE)
                 ))
      }

      df_heatmap <- report$row_flags_detailed %>%
        dplyr::filter(!is.na(date_sample), !is.na(health_zone), reason != "OK") %>%
        dplyr::mutate(
          week = lubridate::floor_date(date_sample, "week")
        ) %>%
        dplyr::count(health_zone, week, name = "issues") %>%
        dplyr::arrange(week, health_zone)

      if (!nrow(df_heatmap)) {
        return(plotly::plot_ly() %>%
                 plotly::add_annotations(
                   text = "No quality issues to display - Great job!",
                   x = 0.5, y = 0.5,
                   showarrow = FALSE,
                   font = list(size = 16, color = "#28a745")
                 ) %>%
                 plotly::layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE)
                 ))
      }

      plotly::plot_ly(
        df_heatmap,
        x = ~week,
        y = ~health_zone,
        z = ~issues,
        type = "heatmap",
        colorscale = list(
          c(0, "#FFFFFF"),
          c(0.2, "#FFF9C4"),
          c(0.4, "#FFE082"),
          c(0.6, "#FFB74D"),
          c(0.8, "#FF8A65"),
          c(1, "#E57373")
        ),
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          "Week: %{x|%Y-%m-%d}<br>",
          "Issues: %{z}<extra></extra>"
        ),
        showscale = TRUE,
        colorbar = list(title = "Issues", len = 0.8)
      ) %>%
        plotly::layout(
          xaxis = list(title = "Week", tickformat = "%b %Y"),
          yaxis = list(title = "", automargin = TRUE),
          margin = list(l = 150)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # DETAIL TABLES
    # ========================================================================
    output$duplicates_table <- DT::renderDT({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$duplicates)) report$duplicates <- data.frame()

      if (!nrow(report$duplicates)) {
        DT::datatable(
          data.frame(Message = "No duplicate records found"),
          options = list(dom = 't', paging = FALSE),
          class = "table-sm"
        )
      } else {
        dup_cols <- names(report$duplicates)
        dup_cols <- dup_cols[!grepl("^\\.__", dup_cols)]

        prioritized <- c(
          .match_cols(dup_cols, "barcode"),
          .match_cols(dup_cols, "lab"),
          .match_cols(dup_cols, "date"),
          .match_cols(dup_cols, "province"),
          .match_cols(dup_cols, "zone")
        )
        cols_to_show <- unique(c(prioritized, dup_cols))

        report$duplicates %>%
          dplyr::select(dplyr::all_of(cols_to_show)) %>%
          DT::datatable(
            options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
            class = "table-sm table-hover"
          )
      }
    })

    output$conflicts_table <- DT::renderDT({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$barcode_conflicts)) report$barcode_conflicts <- data.frame()

      if (!nrow(report$barcode_conflicts)) {
        DT::datatable(
          data.frame(Message = "No barcode conflicts found"),
          options = list(dom = 't', paging = FALSE),
          class = "table-sm"
        )
      } else {
        display_cols <- c()
        if ("barcode_original" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "barcode_original")
        } else if ("barcode_norm" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "barcode_norm")
        }

        if ("n_lab_ids" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "n_lab_ids")
        }

        if ("labids_original" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "labids_original")
        } else if ("labids_norm" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "labids_norm")
        } else if ("lab_ids" %in% names(report$barcode_conflicts)) {
          display_cols <- c(display_cols, "lab_ids")
        }

        if (!length(display_cols)) {
          cols <- names(report$barcode_conflicts)
          display_cols <- cols[!grepl("^\\.__", cols)]
        }

        df_display <- report$barcode_conflicts %>%
          dplyr::select(dplyr::all_of(display_cols))

        names(df_display) <- gsub("_original$", "", names(df_display))
        names(df_display) <- gsub("_norm$", "", names(df_display))
        names(df_display) <- gsub("^n_", "# ", names(df_display))
        names(df_display) <- gsub("_", " ", names(df_display))
        names(df_display) <- tools::toTitleCase(names(df_display))

        DT::datatable(
          df_display,
          rownames = FALSE,
          options = list(pageLength = 10, dom = 'frtip', scrollX = TRUE),
          class = "table-sm table-hover"
        ) %>%
          DT::formatStyle(
            columns = 1:ncol(df_display),
            backgroundColor = "#fff5f5"
          )
      }
    })

    output$labid_conflicts_table <- DT::renderDT({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$labid_conflicts)) report$labid_conflicts <- data.frame()

      if (!nrow(report$labid_conflicts)) {
        DT::datatable(
          data.frame(Message = "No lab ID conflicts found"),
          options = list(dom = 't', paging = FALSE),
          class = "table-sm"
        )
      } else {
        display_cols <- c()
        if ("labid_original" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "labid_original")
        } else if ("labid_norm" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "labid_norm")
        }

        if ("n_barcodes" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "n_barcodes")
        }

        if ("barcodes_original" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "barcodes_original")
        } else if ("barcodes_norm" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "barcodes_norm")
        } else if ("barcodes" %in% names(report$labid_conflicts)) {
          display_cols <- c(display_cols, "barcodes")
        }

        if (!length(display_cols)) {
          cols <- names(report$labid_conflicts)
          display_cols <- cols[!grepl("^\\.__", cols)]
        }

        df_display <- report$labid_conflicts %>%
          dplyr::select(dplyr::all_of(display_cols))

        names(df_display) <- gsub("_original$", "", names(df_display))
        names(df_display) <- gsub("_norm$", "", names(df_display))
        names(df_display) <- gsub("^n_", "# ", names(df_display))
        names(df_display) <- gsub("_", " ", names(df_display))
        names(df_display) <- tools::toTitleCase(names(df_display))

        DT::datatable(
          df_display,
          rownames = FALSE,
          options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
          class = "table-sm table-hover"
        ) %>%
          DT::formatStyle(
            columns = 1:ncol(df_display),
            backgroundColor = "#f3e5f5"
          )
      }
    })

  })
}
