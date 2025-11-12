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
    icon = icon("shield"),  # safer than "shield-alt"
    
    div(class = "container-fluid",
        
        # ==== KPIs (8 cards) ===================================================
        layout_column_wrap(
          width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "12px",

          value_box(
            title  = "Total Rows",
            value  = textOutput(ns("total_rows")),
            showcase = icon("database"),
            theme  = "primary"
          ),
          value_box(
            title  = "Valid Rows",
            value  = textOutput(ns("valid_rows")),
            showcase = icon("check-circle"),
            theme  = "success"
          ),
          value_box(
            title  = "Invalid Rows",
            value  = textOutput(ns("invalid_rows")),
            showcase = icon("ban"),
            theme  = "danger"
          ),
          value_box(
            title  = "Avg. Completeness",
            value  = textOutput(ns("avg_completeness")),
            showcase = icon("percent"),
            theme  = "info"
          ),
          value_box(
            title  = "Missing Barcode",
            value  = textOutput(ns("missing_barcode")),
            showcase = icon("exclamation-triangle"),
            theme  = "warning"
          ),
          value_box(
            title  = "Missing Lab ID",
            value  = textOutput(ns("missing_labid")),
            showcase = icon("exclamation-triangle"),
            theme  = "warning"
          ),
          value_box(
            title  = "Duplicates",
            value  = textOutput(ns("duplicates")),
            showcase = icon("copy"),
            theme  = "danger"
          ),
          value_box(
            title  = "Barcode Conflicts",
            value  = textOutput(ns("conflicts")),
            showcase = icon("barcode"),
            theme  = "danger"
          ),
          value_box(
            title  = "Lab ID Conflicts",
            value  = textOutput(ns("labid_conflicts")),
            showcase = icon("id-card"),
            theme  = "danger"
          )
        ),
        
        # ==== CHARTS: quality over time + entries over time ======================
        layout_columns(col_widths = c(6, 6), gap = "16px",
                       card(
                         card_header(
                           class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
                           span("Quality Flags Over Time"),
                           checkboxInput(
                             ns("include_flagged"),
                             label = "Include flagged rows",
                             value = TRUE
                           )
                         ),
                         card_body_fill(
                           plotly::plotlyOutput(ns("quality_flags_timeline_plot"), height = "500px")
                         )
                       ),
                       card(
                         card_header("Data Entry Timeline"),
                         card_body_fill(
                           plotly::plotlyOutput(ns("entry_timeline_plot"), height = "500px")
                         )
                       )
        ),
        
        # ==== COMPLETENESS (full width) =========================================
        card(
          card_header("Column Completeness Analysis"),
          card_body(
            DT::DTOutput(ns("completeness_table"))
          )
        ),
        
        # ==== INVALID REASONS OVERVIEW (full width) ==============================
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span("Invalid Reasons Overview"),
            span(class = "text-muted small", "Counts and % of excluded rows")
          ),
          card_body(
            DT::DTOutput(ns("invalid_reasons_table"))
          )
        ),
        
        # ==== TABLES: duplicates + conflicts ====================================
        layout_columns(col_widths = c(4, 4, 4), gap = "16px",
                       card(
                         card_header(
                           class = "d-flex justify-content-between align-items-center",
                           span("Duplicate Records")
                         ),
                         card_body(DT::DTOutput(ns("duplicates_table")))
                       ),
                       card(
                         card_header(
                           class = "d-flex justify-content-between align-items-center",
                           span("Barcode Conflicts")
                         ),
                         card_body(DT::DTOutput(ns("conflicts_table")))
                       ),
                       card(
                         card_header(
                           class = "d-flex justify-content-between align-items-center",
                           span("Lab ID Conflicts")
                         ),
                         card_body(DT::DTOutput(ns("labid_conflicts_table")))
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
        
        keep <- !(is.na(b) & is.na(l))            # ghost rows filtered out
        adj_total <- sum(keep)
        
        missing_barcode_adj <- sum(is.na(b[keep]))  # adjusted counts among kept rows
        missing_labid_adj   <- sum(is.na(l[keep]))
      } else {
        adj_total <- suppressWarnings(as.integer(report$summary$rows_raw))
        missing_barcode_adj <- if (!is.null(report$missing_barcode)) report$missing_barcode else NA_integer_
        missing_labid_adj   <- if (!is.null(report$missing_labid))   report$missing_labid   else NA_integer_
      }
      
      valid_rows   <- suppressWarnings(as.integer(report$summary$rows_clean))
      dropped_rows <- if (is.finite(adj_total) && is.finite(valid_rows)) max(adj_total - valid_rows, 0L) else NA_integer_
      drop_rate    <- if (isTRUE(adj_total > 0) && is.finite(dropped_rows)) dropped_rows / adj_total * 100 else NA_real_
      drop_prop    <- if (isTRUE(adj_total > 0) && is.finite(dropped_rows)) dropped_rows / adj_total else NA_real_

      avg_comp <- if (nrow(report$completeness)) {
        mean(report$completeness$percent_complete, na.rm = TRUE)
      } else NA_real_

      list(
        total_rows        = adj_total,
        valid_rows        = valid_rows,
        dropped_rows      = dropped_rows,
        drop_rate         = drop_rate,
        drop_prop         = drop_prop,
        missing_barcode   = missing_barcode_adj,
        missing_labid     = missing_labid_adj,
        duplicate_count   = nrow(report$duplicates),
        conflict_count    = nrow(report$barcode_conflicts),
        labid_conflict_count = nrow(report$labid_conflicts),
        avg_completeness  = avg_comp
      )
    })
    
    # ========================================================================
    # OUTPUTS - KPI BOXES
    # ========================================================================
    output$total_rows <- renderText({
      m <- quality_metrics()
      if (is.na(m$total_rows)) "—" else scales::comma(m$total_rows)
    })
    
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

    output$missing_barcode <- renderText({
      m <- quality_metrics()
      if (is.na(m$missing_barcode)) "—" else scales::comma(m$missing_barcode)
    })

    output$missing_labid <- renderText({
      m <- quality_metrics()
      if (is.na(m$missing_labid)) "—" else scales::comma(m$missing_labid)
    })
    
    output$duplicates <- renderText({
      m <- quality_metrics()
      if (is.na(m$duplicate_count)) "—" else scales::comma(m$duplicate_count)
    })
    
    output$conflicts <- renderText({
      m <- quality_metrics()
      if (is.na(m$conflict_count)) "—" else scales::comma(m$conflict_count)
    })

    output$labid_conflicts <- renderText({
      m <- quality_metrics()
      if (is.na(m$labid_conflict_count)) "—" else scales::comma(m$labid_conflict_count)
    })

    output$avg_completeness <- renderText({
      m <- quality_metrics()
      if (is.na(m$avg_completeness)) "—" else sprintf("%.1f%%", m$avg_completeness)
    })
    
    # ========================================================================
    # OUTPUTS - TABLES
    # ========================================================================
    output$completeness_table <- DT::renderDT({
      req(quality_report())
      report <- quality_report()
      if (is.null(report$completeness)) {
        report$completeness <- data.frame(column = character(), percent_complete = numeric())
      }
      
      report$completeness %>%
        dplyr::mutate(
          percent_complete = round(percent_complete, 1),
          status = dplyr::case_when(
            percent_complete >= 90 ~ "Good",
            percent_complete >= 70 ~ "Fair",
            percent_complete >= 50 ~ "Poor",
            TRUE ~ "Critical"
          )
        ) %>%
        DT::datatable(
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'frtip'
          ),
          class = "table-sm"
        ) %>%
        DT::formatStyle(
          "percent_complete",
          background = DT::styleColorBar(c(0, 100), "lightblue"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        DT::formatStyle(
          "status",
          color = DT::styleEqual(
            c("Good", "Fair", "Poor", "Critical"),
            c("#27AE60", "#F39C12", "#E67E22", "#E74C3C")
          ),
          fontWeight = "bold"
        )
    })
    
    # ---- NEW: Invalid Reasons Overview --------------------------------------
    output$invalid_reasons_table <- DT::renderDT({
      req(quality_report())
      report <- quality_report()
      m <- quality_metrics()
      
      # If a detailed breakdown already exists, use it directly.
      if (!is.null(report$invalid_reasons) &&
          is.data.frame(report$invalid_reasons) &&
          all(c("reason", "count") %in% names(report$invalid_reasons))) {
        
        df <- report$invalid_reasons
        
        # compute % of excluded if possible
        excl <- if (is.na(m$dropped_rows) || m$dropped_rows <= 0) NA_real_ else m$dropped_rows
        if (is.finite(excl) && excl > 0) {
          df <- df %>%
            dplyr::mutate(percent_of_excluded = count / excl)
        } else {
          df <- df %>% dplyr::mutate(percent_of_excluded = NA_real_)
        }
        
      } else {
        # Construct a breakdown from available KPIs (best-effort, may overlap)
        known <- list()
        
        if (is.finite(m$missing_barcode)) known[["Missing barcode"]] <- m$missing_barcode
        if (is.finite(m$missing_labid))   known[["Missing lab ID"]]   <- m$missing_labid
        
        # These are row counts, not unique-barcode adjusted; we accept minor overlap here
        known[["Duplicate barcode(s)"]] <- if (!is.null(report$duplicates)) nrow(report$duplicates) else 0L
        known[["Barcode conflict(s)"]]  <- if (!is.null(report$barcode_conflicts)) nrow(report$barcode_conflicts) else 0L
        known[["Lab ID conflict(s)"]]   <- if (!is.null(report$labid_conflicts))   nrow(report$labid_conflicts)   else 0L
        
        known_df <- tibble::tibble(
          reason = names(known),
          count  = as.integer(unlist(known))
        ) %>%
          dplyr::filter(count > 0)
        
        excl <- if (is.na(m$dropped_rows)) 0L else m$dropped_rows
        known_sum <- sum(known_df$count, na.rm = TRUE)
        other <- max(excl - known_sum, 0L)
        
        df <- dplyr::bind_rows(
          known_df,
          tibble::tibble(reason = "Other / Unspecified", count = as.integer(other))
        ) %>%
          dplyr::arrange(dplyr::desc(count)) %>%
          dplyr::mutate(
            percent_of_excluded = if (excl > 0) count / excl else NA_real_
          )
      }

      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(1, "desc"))
        ),
        class = "table-sm"
      ) %>%
        DT::formatPercentage("percent_of_excluded", 1)
    })
    
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
            class = "table-sm"
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
        cols <- names(report$barcode_conflicts)
        cols <- cols[!grepl("^\\.__", cols)]

        report$barcode_conflicts %>%
          dplyr::select(dplyr::all_of(cols)) %>%
          DT::datatable(
            options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
            class = "table-sm"
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
        cols <- names(report$labid_conflicts)
        cols <- cols[!grepl("^\\.__", cols)]

        report$labid_conflicts %>%
          dplyr::select(dplyr::all_of(cols)) %>%
          DT::datatable(
            options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
            class = "table-sm"
          )
      }
    })
    
    # ========================================================================
    # OUTPUTS - PLOTS
    # ========================================================================
    output$quality_flags_timeline_plot <- plotly::renderPlotly({
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
              date_sample = suppressWarnings(lubridate::as_date(date_sample)),
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
          plotly::plot_ly(
            data = data.frame(x = 0, y = 0, label = "No timeline data available"),
            x = ~x, y = ~y, text = ~label,
            type = "scatter", mode = "text", hoverinfo = "none"
          ) %>%
            plotly::layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list()
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
          plotly::plot_ly(
            data = data.frame(x = 0, y = 0, label = "No timeline data available"),
            x = ~x, y = ~y, text = ~label,
            type = "scatter", mode = "text", hoverinfo = "none"
          ) %>%
            plotly::layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list()
            )
        )
      }

      preferred_levels <- c("Valid", "Missing barcode", "Missing lab ID", "Duplicate", "Barcode conflict")
      df$quality_flag <- factor(
        df$quality_flag,
        levels = unique(c(preferred_levels, setdiff(df$quality_flag, preferred_levels)))
      )

      plotly::plot_ly(
        df,
        x = ~week, y = ~n, color = ~quality_flag, type = "bar",
        hovertemplate = "Week: %{x|%Y-%m-%d}<br>Flag: %{fullData.name}<br>Count: %{y}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Rows"),
          legend = list(orientation = "h", y = -0.2)
        )
    })
    
    output$entry_timeline_plot <- plotly::renderPlotly({
      req(clean_data())
      data <- clean_data()
      
      if (nrow(data) == 0 || !"date_sample" %in% names(data)) {
        plotly::plotly_empty()
      } else {
        timeline_data <- data %>%
          dplyr::filter(!is.na(date_sample)) %>%
          dplyr::mutate(week = lubridate::floor_date(date_sample, "week")) %>%
          dplyr::count(week) %>%
          dplyr::arrange(week)
        
        plotly::plot_ly(
          timeline_data,
          x = ~week,
          y = ~n,
          type = 'scatter',
          mode = 'lines+markers',
          line = list(width = 2),
          marker = list(size = 6),
          hovertemplate = "Week: %{x|%Y-%m-%d}<br>Samples: %{y}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Samples Collected"),
            showlegend = FALSE
          )
      }
    })
    
  })
}
