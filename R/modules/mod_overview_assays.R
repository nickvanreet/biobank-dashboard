# Assay Overview Module
# High-level KPI and concordance view across MIC qPCR, ELISA, and iELISA assays

mod_overview_assays_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Overview",
    icon = icon("dashboard"),
    page_fluid(
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(icon("filter"), "Filters"),
          card_body(
            dateRangeInput(ns("assay_date"), "Assay date", start = NULL, end = NULL),
            selectInput(ns("assay_filter"), "Assays", choices = NULL, multiple = TRUE),
            checkboxGroupInput(ns("status_filter"), "Statuses", choices = c("Positive", "Borderline", "Negative", "Invalid"),
                               selected = c("Positive", "Borderline", "Negative")),
            checkboxInput(ns("available_only"), "Samples with any assay", value = TRUE),
            tags$small(class = "text-muted",
                       "Cutoffs: ELISA PP% ≥20 or DOD ≥0.3 positive; PP% 15-20/DOD 0.2-0.3 borderline. iELISA ≥30% inhibition positive; 25-30% borderline. MIC status based on final call string.")
          )
        ),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            div(icon("chart-pie"), "Key Indicators"),
            tags$small(class = "text-muted", "Click a KPI to drill into the table")
          ),
          card_body(
            layout_column_wrap(
              width = 1/4,
              value_box(title = "MIC qPCR positives", value = uiOutput(ns("kpi_mic")), showcase = icon("dna"), theme = "primary"),
              value_box(title = "ELISA PE positives", value = uiOutput(ns("kpi_pe")), showcase = icon("vial"), theme = "info"),
              value_box(title = "ELISA VSG positives", value = uiOutput(ns("kpi_vsg")), showcase = icon("vials"), theme = "warning"),
              value_box(title = "iELISA positives", value = uiOutput(ns("kpi_ielisa")), showcase = icon("flask"), theme = "success"),
              value_box(title = "Pairwise agreement", value = uiOutput(ns("kpi_agreement")), showcase = icon("handshake"), theme = "secondary"),
              value_box(title = "Unique positives", value = uiOutput(ns("kpi_unique")), showcase = icon("star"), theme = "danger"),
              value_box(title = "Positive concordance", value = uiOutput(ns("kpi_concordant")), showcase = icon("check"), theme = "success"),
              value_box(title = "Tests completed", value = uiOutput(ns("kpi_total")), showcase = icon("list-check"), theme = "dark")
            )
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("bars"), "Stacked results by assay"),
          card_body_fill(plotlyOutput(ns("assay_bars"), height = "350px"))
        ),
        card(
          card_header(icon("th"), "Pairwise agreement"),
          card_body_fill(plotlyOutput(ns("agreement_heatmap"), height = "350px"))
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("layer-group"), "Positive overlaps"),
          card_body_fill(plotlyOutput(ns("upset_plot"), height = "320px"),
                         tags$small(class = "text-muted", "Bars show counts of samples positive in each assay combination."))
        ),
        card(
          card_header(icon("table-cells"), "Sample × assay status"),
          card_body_fill(plotlyOutput(ns("sample_heatmap"), height = "400px"),
                         tags$small(class = "text-muted", "Hover to view quantitative values and cutoffs."))
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("line-chart"), "Trends"),
          card_body_fill(plotlyOutput(ns("trend_plot"), height = "320px"))
        ),
        card(
          card_header(icon("wave-square"), "Quantitative distributions"),
          card_body_fill(plotlyOutput(ns("quant_plot"), height = "320px"))
        )
      ),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("table"), "Sample drilldown"),
          downloadButton(ns("export_table"), "Export CSV", class = "btn-sm btn-primary")
        ),
        card_body(DT::DTOutput(ns("sample_table")))
      )
    )
  )
}

mod_overview_assays_server <- function(id, biobank_df, elisa_df, ielisa_df, mic_df, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    drill_state <- reactiveValues(status = NULL, assay = NULL)

    assay_palette <- reactive({
      assay_cutoffs()$shared_palette
    })

    prepared <- reactive({
      prepare_assay_dashboard_data(
        biobank_df = if (is.null(biobank_df)) NULL else biobank_df(),
        elisa_df = if (is.null(elisa_df)) NULL else elisa_df(),
        ielisa_df = if (is.null(ielisa_df)) NULL else ielisa_df(),
        mic_data = if (is.null(mic_df)) NULL else mic_df(),
        filters = if (is.null(filters)) NULL else filters()
      )
    })

    observeEvent(prepared(), {
      assays <- prepared()$tidy_assays %>% distinct(assay) %>% arrange(assay) %>% pull()
      updateSelectInput(session, "assay_filter", choices = assays, selected = assays)
    }, ignoreNULL = FALSE)

    filtered_tidy <- reactive({
      req(prepared())
      df <- prepared()$tidy_assays
      if (!is.null(input$assay_filter) && length(input$assay_filter)) {
        df <- df %>% filter(as.character(assay) %in% input$assay_filter)
      }
      if (length(input$status_filter)) {
        df <- df %>% filter(as.character(status) %in% input$status_filter)
      }
      if (!is.null(input$assay_date)) {
        dr <- as.Date(input$assay_date)
        if (any(!is.na(dr))) {
          if (!is.na(dr[1])) {
            df <- df %>% filter(is.na(assay_date) | assay_date >= dr[1])
          }
          if (!is.na(dr[2])) {
            df <- df %>% filter(is.na(assay_date) | assay_date <= dr[2])
          }
        }
      }
      if (isTRUE(input$available_only)) {
        df <- df %>% filter(!is.na(status) & status != "Missing")
      }
      if (!is.null(drill_state$status)) {
        df <- df %>% filter(status == drill_state$status)
      }
      if (!is.null(drill_state$assay)) {
        df <- df %>% filter(as.character(assay) == drill_state$assay)
      }
      df
    })

    sample_summary <- reactive({
      filtered_tidy() %>%
        mutate(is_positive = status == "Positive") %>%
        group_by(sample_id) %>%
        summarise(
          assays_tested = n_distinct(assay),
          positives = sum(is_positive, na.rm = TRUE),
          unique_positive = positives == 1,
          concordant_positive = positives == assays_tested & positives > 0,
          any_result = assays_tested > 0,
          .groups = "drop"
        )
    })

    render_kpi <- function(expr) {
      renderUI({
        val <- expr()
        tagList(strong(val$label), tags$br(), tags$span(class = "fs-4", val$detail))
      })
    }

    output$kpi_mic <- render_kpi(function() {
      df <- prepared()$tidy_assays %>% filter(assay == "MIC qPCR", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of MIC tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "MIC qPCR")))))
    })

    output$kpi_pe <- render_kpi(function() {
      df <- prepared()$tidy_assays %>% filter(assay == "ELISA PE", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of PE tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "ELISA PE")))))
    })

    output$kpi_vsg <- render_kpi(function() {
      df <- prepared()$tidy_assays %>% filter(assay == "ELISA VSG", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of VSG tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "ELISA VSG")))))
    })

    output$kpi_ielisa <- render_kpi(function() {
      df <- prepared()$tidy_assays %>% filter(grepl("iELISA", assay), status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of iELISA tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(grepl("iELISA", assay))))))
    })

    output$kpi_unique <- render_kpi(function() {
      ss <- sample_summary()
      list(label = sum(ss$unique_positive, na.rm = TRUE), detail = "Single-assay positives")
    })

    output$kpi_concordant <- render_kpi(function() {
      ss <- sample_summary()
      list(label = sum(ss$concordant_positive, na.rm = TRUE), detail = "Positive across all tested")
    })

    output$kpi_total <- render_kpi(function() {
      df <- prepared()$tidy_assays
      list(label = nrow(df), detail = "Total assay rows")
    })

    output$kpi_agreement <- render_kpi(function() {
      pw <- prepared()$pairwise_agreement %>% filter(!is.na(agreement))
      list(label = sprintf("%.1f%%", mean(pw$agreement, na.rm = TRUE)), detail = "Mean pairwise agreement")
    })

    output$assay_bars <- renderPlotly({
      df <- filtered_tidy() %>%
        count(assay, status) %>%
        mutate(status = factor(status, levels = names(assay_palette())))
      if (!nrow(df)) return(NULL)
      p <- ggplot(df, aes(x = assay, y = n, fill = status, text = paste("Assay:", assay, "<br>Status:", status, "<br>n:", n))) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = assay_palette()) +
        theme_minimal() + labs(x = NULL, y = "Samples")
      ggplotly(p, tooltip = "text") %>% event_register("plotly_click")
    })

    observeEvent(plotly::event_data("plotly_click", source = NULL), {
      ev <- plotly::event_data("plotly_click")
      if (!is.null(ev) && !is.null(ev$x) && !is.null(ev$curveNumber)) {
        drill_state$assay <- as.character(ev$x)
        drill_state$status <- levels(filtered_tidy()$status)[ev$curveNumber + 1]
      }
    })

    output$agreement_heatmap <- renderPlotly({
      pw <- prepared()$pairwise_agreement
      if (!nrow(pw)) return(NULL)
      p <- plot_ly(
        data = pw,
        x = ~assay2,
        y = ~assay1,
        z = ~agreement,
        type = "heatmap",
        text = ~paste0("Agreement: ", round(agreement, 1), "%", "<br>n=", n),
        hoverinfo = "text",
        colors = colorRamp(c("#E0F2FE", "#1D4ED8"))
      ) %>%
        layout(xaxis = list(title = "Assay"), yaxis = list(title = "Assay"))
      p
    })

    output$upset_plot <- renderPlotly({
      inter <- prepared()$intersections
      if (!nrow(inter)) return(NULL)
      p <- ggplot(inter, aes(x = reorder(combo, n), y = n, text = paste(combo, "<br>n=", n))) +
        geom_col(fill = "#2563EB") +
        coord_flip() +
        theme_minimal() + labs(x = "Assay combination", y = "Positive samples")
      ggplotly(p, tooltip = "text")
    })

    output$sample_heatmap <- renderPlotly({
      mat <- prepared()$sample_matrix
      if (!nrow(mat)) return(NULL)
      long <- mat %>% pivot_longer(-sample_id, names_to = "assay", values_to = "status") %>%
        mutate(status = factor(status, levels = names(assay_palette())))
      pal <- assay_palette()
      p <- plot_ly(
        data = long,
        x = ~assay,
        y = ~sample_id,
        type = "heatmap",
        z = ~as.numeric(status),
        colors = unname(pal),
        text = ~paste("Sample:", sample_id, "<br>Assay:", assay, "<br>Status:", status),
        hoverinfo = "text"
      )
      p
    })

    output$trend_plot <- renderPlotly({
      df <- filtered_tidy()
      if (!"assay_date" %in% names(df) || all(is.na(df$assay_date))) return(NULL)
      df <- df %>% filter(!is.na(assay_date)) %>%
        mutate(month = floor_date(assay_date, "month")) %>%
        group_by(month, assay) %>%
        summarise(pos_rate = mean(status == "Positive"), n = n(), .groups = "drop")
      if (!nrow(df)) return(NULL)
      p <- ggplot(df, aes(x = month, y = pos_rate, color = assay, group = assay,
                         text = paste("Month:", month, "<br>Positivity:", scales::percent(pos_rate), "<br>n=", n))) +
        geom_line() + geom_point() + scale_y_continuous(labels = scales::percent) +
        theme_minimal() + labs(x = NULL, y = "Positivity")
      ggplotly(p, tooltip = "text")
    })

    output$quant_plot <- renderPlotly({
      df <- filtered_tidy() %>% filter(!is.na(quantitative))
      if (!nrow(df)) return(NULL)
      cuts <- prepared()$cutoffs
      p <- ggplot(df, aes(x = quantitative, fill = assay, text = paste("Assay:", assay, "<br>", metric, ":", round(quantitative, 2)))) +
        geom_histogram(alpha = 0.7, position = "identity", bins = 30) +
        theme_minimal() + labs(x = "Quantitative value", y = "Count")
      p <- ggplotly(p, tooltip = "text")
      # Add cutoff lines for ELISA and iELISA metrics when present
      if ("elisa_pp_positive" %in% names(cuts)) {
        p <- p %>% add_segments(x = cuts$elisa_pp_positive, xend = cuts$elisa_pp_positive, y = 0, yend = max(df$quantitative, na.rm = TRUE),
                                line = list(color = "#EF4444", dash = "dash"), inherit = FALSE, showlegend = FALSE)
      }
      p
    })

    output$sample_table <- DT::renderDT({
      df <- filtered_tidy()
      DT::datatable(
        df %>% select(sample_id, assay, status, metric, quantitative, assay_date),
        options = list(pageLength = 15, order = list(list(0, 'asc'))),
        selection = "single"
      )
    })

    output$export_table <- downloadHandler(
      filename = function() paste0("assay_drilldown_", Sys.Date(), ".csv"),
      content = function(file) {
        readr::write_csv(filtered_tidy(), file)
      }
    )
  })
}
