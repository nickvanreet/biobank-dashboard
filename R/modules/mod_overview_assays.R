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
            div(icon("chart-pie"), "Test Results by Assay"),
            tags$small(class = "text-muted", "Click a KPI to drill into the table")
          ),
          card_body(
            layout_column_wrap(
              width = 1/4,
              value_box(title = "MIC qPCR positives", value = uiOutput(ns("kpi_mic")), showcase = icon("dna"), theme = "primary"),
              value_box(title = "ELISA PE positives", value = uiOutput(ns("kpi_pe")), showcase = icon("vial"), theme = "info"),
              value_box(title = "ELISA VSG positives", value = uiOutput(ns("kpi_vsg")), showcase = icon("vials"), theme = "warning"),
              value_box(title = "iELISA positives", value = uiOutput(ns("kpi_ielisa")), showcase = icon("flask"), theme = "success")
            )
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "d-flex justify-content-between align-items-center",
            div(icon("microscope"), "Molecular vs Serology Concordance"),
            tags$small(class = "text-muted", "Concordance between MIC qPCR and any serological test (ELISA/iELISA)")
          ),
          card_body(
            # Top row: Key KPIs
            layout_column_wrap(
              width = 1/4,
              value_box(title = "Samples tested", value = uiOutput(ns("kpi_samples_tested")), showcase = icon("users"), theme = "secondary"),
              value_box(title = "Concordance", value = uiOutput(ns("kpi_mic_serology_concordance")), showcase = icon("handshake"), theme = "info"),
              value_box(title = "Both Positive", value = uiOutput(ns("kpi_both_positive")), showcase = icon("check-double"), theme = "success"),
              value_box(title = "Tests completed", value = uiOutput(ns("kpi_total")), showcase = icon("list-check"), theme = "dark")
            ),
            # Full concordance table
            div(class = "mt-3",
                h5("Concordance Details"),
                DT::DTOutput(ns("concordance_table"))
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
          card_header(icon("project-diagram"), "Molecular vs Serology Concordance"),
          card_body_fill(plotlyOutput(ns("concordance_sankey"), height = "400px"),
                         tags$small(class = "text-muted", "Flow diagram showing concordance between MIC qPCR (molecular) and any serological test (ELISA PE/VSG, iELISA)."))
        ),
        card(
          card_header(icon("layer-group"), "Concordance categories"),
          card_body_fill(plotlyOutput(ns("concordance_bars"), height = "400px"),
                         tags$small(class = "text-muted", "Sample distribution across molecular-serology concordance categories."))
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
      if (!is.null(input$assay_date[1])) {
        dr <- as.Date(input$assay_date)
        df <- df %>% filter(is.na(assay_date) | (assay_date >= dr[1] & assay_date <= dr[2]))
      }
      if (isTRUE(input$available_only)) {
        df <- df %>% filter(!is.na(status) & status != "Missing")
      }
      if (!is.null(drill_state$status)) {
        df <- df %>% filter(status == drill_state$status)
      }
      if (!is.null(drill_state$assay)) {
        df <- df %>% filter(as.character(assay) %in% drill_state$assay)
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

    set_drill <- function(status = NULL, assay = NULL) {
      if (identical(drill_state$status, status) && identical(drill_state$assay, assay)) {
        drill_state$status <- NULL
        drill_state$assay <- NULL
      } else {
        drill_state$status <- status
        drill_state$assay <- assay
      }
    }

    render_kpi <- function(output_id, expr, drill_status = NULL, drill_assay = NULL) {
      output[[output_id]] <- renderUI({
        val <- expr()
        actionLink(
          ns(paste0(output_id, "_click")),
          label = tagList(strong(val$label), tags$br(), tags$span(class = "fs-4", val$detail)),
          class = "text-reset text-decoration-none w-100 d-block"
        )
      })

      if (!is.null(drill_status) || !is.null(drill_assay)) {
        observeEvent(input[[paste0(output_id, "_click")]], {
          set_drill(status = drill_status, assay = drill_assay)
        }, ignoreInit = TRUE)
      }
    }

    render_kpi("kpi_mic", function() {
      df <- prepared()$tidy_assays %>% filter(assay == "MIC qPCR", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of MIC tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "MIC qPCR")))))
    }, drill_status = "Positive", drill_assay = "MIC qPCR")

    render_kpi("kpi_pe", function() {
      df <- prepared()$tidy_assays %>% filter(assay == "ELISA PE", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of PE tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "ELISA PE")))))
    }, drill_status = "Positive", drill_assay = "ELISA PE")

    render_kpi("kpi_vsg", function() {
      df <- prepared()$tidy_assays %>% filter(assay == "ELISA VSG", status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of VSG tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(assay == "ELISA VSG")))))
    }, drill_status = "Positive", drill_assay = "ELISA VSG")

    render_kpi("kpi_ielisa", function() {
      df <- prepared()$tidy_assays %>% filter(grepl("iELISA", assay), status == "Positive")
      list(label = nrow(df), detail = sprintf("%.1f%% of iELISA tests", 100 * nrow(df) / max(1, nrow(prepared()$tidy_assays %>% filter(grepl("iELISA", assay))))))
    }, drill_status = "Positive", drill_assay = c("iELISA LiTat 1.3", "iELISA LiTat 1.5"))

    render_kpi("kpi_both_positive", function() {
      summary <- prepared()$mic_serology_summary
      if (nrow(summary) == 0 || summary$n_samples == 0) {
        list(label = "0", detail = "0% of tested samples")
      } else {
        list(
          label = summary$n_both_positive,
          detail = sprintf("%.1f%% of tested samples", summary$pct_both_positive)
        )
      }
    })

    render_kpi("kpi_mic_only", function() {
      summary <- prepared()$mic_serology_summary
      if (nrow(summary) == 0 || summary$n_samples == 0) {
        list(label = "0", detail = "0% of tested samples")
      } else {
        list(
          label = summary$n_mic_only,
          detail = sprintf("%.1f%% of tested samples", summary$pct_mic_only)
        )
      }
    })

    render_kpi("kpi_serology_only", function() {
      summary <- prepared()$mic_serology_summary
      if (nrow(summary) == 0 || summary$n_samples == 0) {
        list(label = "0", detail = "0% of tested samples")
      } else {
        list(
          label = summary$n_serology_only,
          detail = sprintf("%.1f%% of tested samples", summary$pct_serology_only)
        )
      }
    })

    render_kpi("kpi_mic_serology_concordance", function() {
      summary <- prepared()$mic_serology_summary
      if (nrow(summary) == 0 || summary$n_samples == 0) {
        list(label = "N/A", detail = "No samples with both tests")
      } else {
        list(
          label = sprintf("%.1f%%", summary$pct_concordant),
          detail = sprintf("%d/%d samples concordant", summary$n_concordant, summary$n_samples)
        )
      }
    })

    render_kpi("kpi_single_test", function() {
      summary <- prepared()$single_test_summary
      list(
        label = summary$n_single_test_positive,
        detail = "Positive on only one test"
      )
    })

    render_kpi("kpi_all_tests", function() {
      summary <- prepared()$single_test_summary
      list(
        label = summary$n_all_tests_positive,
        detail = "Positive on all tests"
      )
    })

    render_kpi("kpi_samples_tested", function() {
      concordance <- prepared()$molecular_serology_concordance
      n_samples <- length(unique(concordance$sample_id))
      list(
        label = n_samples,
        detail = "Unique samples"
      )
    })

    render_kpi("kpi_total", function() {
      df <- prepared()$tidy_assays
      list(label = nrow(df), detail = "Total assay rows")
    })

    # Concordance table
    output$concordance_table <- DT::renderDT({
      concordance <- prepared()$molecular_serology_concordance

      if (nrow(concordance) == 0) {
        return(DT::datatable(
          data.frame(Message = "No concordance data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }

      # Create summary table with all concordance categories
      summary_table <- concordance %>%
        filter(mic_tested & serology_tested) %>%
        count(concordance_category) %>%
        mutate(
          percentage = n / sum(n) * 100,
          concordance_category = factor(concordance_category,
            levels = c("Both Positive", "Both Negative", "MIC+ / Serology-", "MIC- / Serology+"))
        ) %>%
        arrange(concordance_category) %>%
        rename(
          Category = concordance_category,
          Count = n,
          Percentage = percentage
        )

      # Add a summary row
      totals <- concordance %>%
        filter(mic_tested & serology_tested) %>%
        summarise(
          Category = "TOTAL",
          Count = n(),
          Percentage = 100
        )

      # Add concordant/discordant summary
      concordant <- sum(summary_table$Category %in% c("Both Positive", "Both Negative"))
      discordant <- sum(summary_table$Category %in% c("MIC+ / Serology-", "MIC- / Serology+"))

      summary_stats <- data.frame(
        Category = c("Concordant (Both +/Both -)", "Discordant (Mismatch)"),
        Count = c(concordant, discordant),
        Percentage = c(concordant / totals$Count * 100, discordant / totals$Count * 100)
      )

      # Combine all
      final_table <- bind_rows(summary_table, summary_stats, totals)

      DT::datatable(
        final_table,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 't',
          paging = FALSE
        ),
        class = "table-sm table-striped",
        rownames = FALSE
      ) %>%
        DT::formatPercentage("Percentage", digits = 1) %>%
        DT::formatStyle(
          "Category",
          target = "row",
          backgroundColor = DT::styleEqual(
            c("Both Positive", "Both Negative", "Concordant (Both +/Both -)", "TOTAL"),
            c("#d4edda", "#f8f9fa", "#cfe2ff", "#f0f0f0")
          ),
          fontWeight = DT::styleEqual(
            c("Concordant (Both +/Both -)", "Discordant (Mismatch)", "TOTAL"),
            c("bold", "bold", "bold")
          )
        )
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

    output$concordance_sankey <- renderPlotly({
      concordance <- prepared()$molecular_serology_concordance

      if (nrow(concordance) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No concordance data available",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      # Filter samples with both tests
      concordance_filtered <- concordance %>%
        filter(mic_tested & serology_tested)

      if (nrow(concordance_filtered) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = sprintf("No samples with both MIC and serology tests\n(Total samples: %d)", nrow(concordance)),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      # Define nodes: MIC Positive/Negative -> Serology Positive/Serology Negative
      nodes <- data.frame(
        name = c("MIC Positive", "MIC Negative", "Serology Positive", "Serology Negative")
      )

      # Calculate links
      mic_pos_serology_pos <- sum(concordance_filtered$mic_positive & concordance_filtered$serology_positive, na.rm = TRUE)
      mic_pos_serology_neg <- sum(concordance_filtered$mic_positive & !concordance_filtered$serology_positive, na.rm = TRUE)
      mic_neg_serology_pos <- sum(!concordance_filtered$mic_positive & concordance_filtered$serology_positive, na.rm = TRUE)
      mic_neg_serology_neg <- sum(!concordance_filtered$mic_positive & !concordance_filtered$serology_positive, na.rm = TRUE)

      # Create links (source and target are 0-indexed)
      links <- data.frame(
        source = c(0, 0, 1, 1),  # MIC Pos, MIC Pos, MIC Neg, MIC Neg
        target = c(2, 3, 2, 3),  # Serology Pos, Serology Neg, Serology Pos, Serology Neg
        value = c(mic_pos_serology_pos, mic_pos_serology_neg, mic_neg_serology_pos, mic_neg_serology_neg),
        color = c("rgba(16, 185, 129, 0.5)", "rgba(255, 165, 0, 0.5)",
                  "rgba(255, 165, 0, 0.5)", "rgba(156, 163, 175, 0.5)")
      )

      # Filter out zero values
      links <- links %>% filter(value > 0)

      if (nrow(links) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No data to display in flow diagram",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = nodes$name,
          color = c("#2563EB", "#9CA3AF", "#10B981", "#9CA3AF"),
          pad = 15,
          thickness = 20
        ),
        link = list(
          source = links$source,
          target = links$target,
          value = links$value,
          color = links$color
        )
      ) %>%
        layout(
          title = "",
          font = list(size = 12),
          margin = list(l = 20, r = 20, t = 20, b = 20)
        )
    })

    output$concordance_bars <- renderPlotly({
      concordance <- prepared()$molecular_serology_concordance

      if (nrow(concordance) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No concordance data available",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      # Filter samples with both tests
      concordance_filtered <- concordance %>%
        filter(mic_tested & serology_tested)

      if (nrow(concordance_filtered) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = sprintf("No samples with both MIC and serology tests\n(Total samples: %d)", nrow(concordance)),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      category_counts <- concordance_filtered %>%
        count(concordance_category) %>%
        mutate(
          concordance_category = factor(concordance_category,
            levels = c("Both Positive", "Both Negative", "MIC+ / Serology-", "MIC- / Serology+")),
          percentage = n / sum(n) * 100,
          color = case_when(
            concordance_category == "Both Positive" ~ "#10B981",
            concordance_category == "Both Negative" ~ "#9CA3AF",
            concordance_category == "MIC+ / Serology-" ~ "#2563EB",
            concordance_category == "MIC- / Serology+" ~ "#F59E0B",
            TRUE ~ "#E5E7EB"
          )
        ) %>%
        filter(!is.na(concordance_category))

      if (nrow(category_counts) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No data to display",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      p <- plot_ly(
        data = category_counts,
        x = ~concordance_category,
        y = ~n,
        type = "bar",
        marker = list(color = ~color),
        text = ~paste0(concordance_category, "<br>n=", n, " (", sprintf("%.1f%%", percentage), ")"),
        hoverinfo = "text"
      ) %>%
        layout(
          xaxis = list(
            title = "Concordance Category",
            tickangle = -45
          ),
          yaxis = list(title = "Number of Samples"),
          showlegend = FALSE,
          margin = list(b = 100)
        )

      p
    })

    output$upset_plot <- renderPlotly({
      inter <- prepared()$intersections
      if (!nrow(inter)) return(NULL)

      # Add a column to indicate if it's a single test or multiple tests
      inter <- inter %>%
        mutate(
          n_assays = str_count(combo, "\\+") + 1,
          is_single = n_assays == 1,
          fill_color = if_else(is_single, "#EF4444", "#2563EB")  # Red for single, blue for multiple
        )

      p <- ggplot(inter, aes(x = reorder(combo, n), y = n,
                            text = paste(combo, "<br>n=", n, "<br>",
                                        if_else(is_single, "Single test positive", "Multiple tests positive")),
                            fill = fill_color)) +
        geom_col() +
        scale_fill_identity() +
        coord_flip() +
        theme_minimal() +
        labs(x = "Assay combination", y = "Positive samples") +
        theme(legend.position = "none")
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
