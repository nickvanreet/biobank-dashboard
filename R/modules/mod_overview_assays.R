# Assay Overview Module
# Comprehensive prevalence and concordance analysis across MIC qPCR, ELISA, and iELISA assays

mod_overview_assays_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Overview",
    icon = icon("dashboard"),
    page_fluid(
      layout_columns(
        col_widths = c(3, 9),
        card(
          card_header(icon("info-circle"), "Test Classification"),
          card_body(
            tags$small(class = "text-muted",
                       "Cutoffs: ELISA PP% ≥20 or DOD ≥0.3 positive; PP% 15-20/DOD 0.2-0.3 borderline. iELISA ≥30% inhibition positive; 25-30% borderline. MIC status based on final call string.")
          )
        ),
        div(
          # Summary KPIs
          card(
            card_header(icon("chart-line"), "Summary Statistics"),
            card_body(
              layout_column_wrap(
                width = 1/3,
                value_box(title = "Total Samples", value = uiOutput(ns("kpi_total_samples")), showcase = icon("users"), theme = "primary"),
                value_box(title = "Total Tests Completed", value = uiOutput(ns("kpi_total_tests")), showcase = icon("vials"), theme = "info"),
                value_box(title = "Samples with Any Positive", value = uiOutput(ns("kpi_any_positive")), showcase = icon("check"), theme = "success")
              )
            )
          ),

          # Test Prevalence Section
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(icon("chart-pie"), "Test Prevalence & Overlaps"),
              tags$small(class = "text-muted", "Click a test to view details in the sample table")
            ),
            card_body(
              h5("MIC qPCR"),
              layout_column_wrap(
                width = 1/4,
                value_box(
                  title = "MIC Positive",
                  value = uiOutput(ns("kpi_mic_positive")),
                  showcase = icon("dna"),
                  theme = "primary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "Exclusive (MIC only)",
                  value = uiOutput(ns("kpi_mic_exclusive")),
                  showcase = icon("circle"),
                  theme = "secondary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "Shared with Serology",
                  value = uiOutput(ns("kpi_mic_with_serology")),
                  showcase = icon("share-nodes"),
                  theme = "info",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "MIC Tested",
                  value = uiOutput(ns("kpi_mic_tested")),
                  showcase = icon("flask"),
                  theme = "dark",
                  showcase_layout = "left center"
                )
              ),

              tags$hr(),
              h5("ELISA Tests"),
              layout_column_wrap(
                width = 1/3,
                # ELISA PE
                value_box(
                  title = "ELISA PE Positive",
                  value = uiOutput(ns("kpi_pe_positive")),
                  showcase = icon("vial"),
                  theme = "info",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "PE Exclusive",
                  value = uiOutput(ns("kpi_pe_exclusive")),
                  showcase = icon("circle"),
                  theme = "secondary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "PE Shared",
                  value = uiOutput(ns("kpi_pe_shared")),
                  showcase = icon("share-nodes"),
                  theme = "dark",
                  showcase_layout = "left center"
                )
              ),

              layout_column_wrap(
                width = 1/3,
                # ELISA VSG
                value_box(
                  title = "ELISA VSG Positive",
                  value = uiOutput(ns("kpi_vsg_positive")),
                  showcase = icon("vials"),
                  theme = "warning",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "VSG Exclusive",
                  value = uiOutput(ns("kpi_vsg_exclusive")),
                  showcase = icon("circle"),
                  theme = "secondary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "VSG Shared",
                  value = uiOutput(ns("kpi_vsg_shared")),
                  showcase = icon("share-nodes"),
                  theme = "dark",
                  showcase_layout = "left center"
                )
              ),

              tags$hr(),
              h5("iELISA Tests"),
              layout_column_wrap(
                width = 1/3,
                # iELISA L13
                value_box(
                  title = "iELISA LiTat 1.3 Positive",
                  value = uiOutput(ns("kpi_l13_positive")),
                  showcase = icon("flask"),
                  theme = "success",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "L13 Exclusive",
                  value = uiOutput(ns("kpi_l13_exclusive")),
                  showcase = icon("circle"),
                  theme = "secondary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "L13 Shared",
                  value = uiOutput(ns("kpi_l13_shared")),
                  showcase = icon("share-nodes"),
                  theme = "dark",
                  showcase_layout = "left center"
                )
              ),

              layout_column_wrap(
                width = 1/3,
                # iELISA L15
                value_box(
                  title = "iELISA LiTat 1.5 Positive",
                  value = uiOutput(ns("kpi_l15_positive")),
                  showcase = icon("flask-vial"),
                  theme = "success",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "L15 Exclusive",
                  value = uiOutput(ns("kpi_l15_exclusive")),
                  showcase = icon("circle"),
                  theme = "secondary",
                  showcase_layout = "left center"
                ),
                value_box(
                  title = "L15 Shared",
                  value = uiOutput(ns("kpi_l15_shared")),
                  showcase = icon("share-nodes"),
                  theme = "dark",
                  showcase_layout = "left center"
                )
              )
            )
          ),

          # Overlap Summary KPIs
          card(
            card_header(icon("diagram-project"), "Test Overlap Summary"),
            card_body(
              layout_column_wrap(
                width = 1/3,
                value_box(
                  title = "Positive on ALL Tests",
                  value = uiOutput(ns("kpi_all_tests_positive")),
                  showcase = icon("check-double"),
                  theme = "success"
                ),
                value_box(
                  title = "Positive on ALL Serology",
                  value = uiOutput(ns("kpi_all_serology_positive")),
                  showcase = icon("layer-group"),
                  theme = "info"
                ),
                value_box(
                  title = "MIC + Any Serology",
                  value = uiOutput(ns("kpi_mic_and_serology")),
                  showcase = icon("handshake"),
                  theme = "warning"
                )
              )
            )
          )
        )
      ),

      # Detailed Tables Section
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header(icon("table"), "Detailed Test Prevalence Table"),
          card_body(
            DT::DTOutput(ns("prevalence_table"))
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(icon("th"), "Pairwise Test Overlaps"),
          card_body(
            tags$p(class = "text-muted small", "Number of samples positive on both tests"),
            DT::DTOutput(ns("overlap_table"))
          )
        ),
        card(
          full_screen = TRUE,
          card_header(icon("heat"), "Overlap Heatmap"),
          card_body_fill(
            plotlyOutput(ns("overlap_heatmap"), height = "400px"),
            tags$small(class = "text-muted", "Darker colors indicate more samples positive on both tests")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("bars"), "Status Distribution by Test"),
          card_body_fill(plotlyOutput(ns("assay_bars"), height = "350px"))
        ),
        card(
          card_header(icon("project-diagram"), "Positive Sample Overlaps"),
          card_body_fill(
            plotlyOutput(ns("upset_plot"), height = "350px"),
            tags$small(class = "text-muted", "Red bars = single test positive, Blue bars = multiple tests positive")
          )
        )
      ),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("table"), "Sample Drilldown"),
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
      tryCatch({
        result <- prepare_assay_dashboard_data(
          biobank_df = if (is.null(biobank_df)) NULL else biobank_df(),
          elisa_df = if (is.null(elisa_df)) NULL else elisa_df(),
          ielisa_df = if (is.null(ielisa_df)) NULL else ielisa_df(),
          mic_data = if (is.null(mic_df)) NULL else mic_df(),
          filters = if (is.null(filters)) NULL else filters()
        )

        # Validate result structure
        if (is.null(result) || !is.list(result)) {
          warning("prepare_assay_dashboard_data returned NULL or invalid structure")
          return(NULL)
        }

        return(result)
      }, error = function(e) {
        warning(sprintf("Error in prepare_assay_dashboard_data: %s", e$message))
        return(NULL)
      })
    })

    filtered_tidy <- reactive({
      req(prepared())
      df <- prepared()$tidy_assays
      if (!is.null(drill_state$status)) {
        df <- df %>% filter(status == drill_state$status)
      }
      if (!is.null(drill_state$assay)) {
        df <- df %>% filter(as.character(assay) %in% drill_state$assay)
      }
      df
    })

    # Helper function to get stats for a specific test
    get_test_stats <- function(test_name) {
      tryCatch({
        prep_data <- prepared()
        if (is.null(prep_data)) {
          return(list(
            n_positive = 0,
            pct_positive = 0,
            total_tests = 0,
            n_exclusive = 0,
            n_shared = 0,
            n_with_serology = 0
          ))
        }

        prevalence <- prep_data$test_prevalence %>% filter(assay == test_name)
        overlaps <- prep_data$test_specific_overlaps %>% filter(assay == test_name)

        if (nrow(prevalence) == 0) {
          return(list(
            n_positive = 0,
            pct_positive = 0,
            total_tests = 0,
            n_exclusive = 0,
            n_shared = 0,
            n_with_serology = 0
          ))
        }

        n_with_serology <- if (nrow(overlaps) > 0) {
          overlaps$n_with_any_serology
        } else {
          0
        }

        list(
          n_positive = prevalence$n_positive,
          pct_positive = prevalence$pct_positive,
          total_tests = prevalence$total_tests,
          n_exclusive = prevalence$n_exclusive,
          n_shared = prevalence$n_shared,
          n_with_serology = n_with_serology
        )
      }, error = function(e) {
        warning(sprintf("Error in get_test_stats for %s: %s", test_name, e$message))
        return(list(
          n_positive = 0,
          pct_positive = 0,
          total_tests = 0,
          n_exclusive = 0,
          n_shared = 0,
          n_with_serology = 0
        ))
      })
    }

    # Summary KPIs
    output$kpi_total_samples <- renderUI({
      req(prepared())
      summary <- prepared()$overlap_summary
      if (is.null(summary)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$total_samples %||% 0),
        tags$br(),
        tags$span(class = "text-muted", "Unique samples")
      )
    })

    output$kpi_total_tests <- renderUI({
      req(prepared())
      tidy <- prepared()$tidy_assays
      if (is.null(tidy)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      n_tests <- nrow(tidy)
      tags$div(
        tags$strong(style = "font-size: 2em;", n_tests),
        tags$br(),
        tags$span(class = "text-muted", "Total test results")
      )
    })

    output$kpi_any_positive <- renderUI({
      req(prepared())
      summary <- prepared()$overlap_summary
      if (is.null(summary)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      pct <- if (summary$total_samples > 0) {
        (summary$n_any_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_any_positive %||% 0),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    # MIC KPIs
    output$kpi_mic_positive <- renderUI({
      stats <- get_test_stats("MIC qPCR")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d MIC tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_mic_exclusive <- renderUI({
      stats <- get_test_stats("MIC qPCR")
      pct <- if (stats$n_positive > 0) {
        (stats$n_exclusive / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of MIC positives", pct))
      )
    })

    output$kpi_mic_with_serology <- renderUI({
      stats <- get_test_stats("MIC qPCR")
      pct <- if (stats$n_positive > 0) {
        (stats$n_with_serology / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_with_serology),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of MIC positives", pct))
      )
    })

    output$kpi_mic_tested <- renderUI({
      stats <- get_test_stats("MIC qPCR")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$total_tests),
        tags$br(),
        tags$span(class = "text-muted", "Total MIC tests")
      )
    })

    # ELISA PE KPIs
    output$kpi_pe_positive <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_pe_exclusive <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only PE positive")
      )
    })

    output$kpi_pe_shared <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # ELISA VSG KPIs
    output$kpi_vsg_positive <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_vsg_exclusive <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only VSG positive")
      )
    })

    output$kpi_vsg_shared <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # iELISA L13 KPIs
    output$kpi_l13_positive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_l13_exclusive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only L13 positive")
      )
    })

    output$kpi_l13_shared <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # iELISA L15 KPIs
    output$kpi_l15_positive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_l15_exclusive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only L15 positive")
      )
    })

    output$kpi_l15_shared <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # Overlap Summary KPIs
    output$kpi_all_tests_positive <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_all_tests_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_all_tests_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    output$kpi_all_serology_positive <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_all_serology_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_all_serology_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    output$kpi_mic_and_serology <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_mic_and_any_serology / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_mic_and_any_serology),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    # Prevalence Table
    output$prevalence_table <- DT::renderDT({
      prevalence <- prepared()$test_prevalence
      overlaps <- prepared()$test_specific_overlaps

      if (nrow(prevalence) == 0) {
        return(DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }

      # Join prevalence with detailed overlaps
      table_data <- prevalence %>%
        left_join(overlaps, by = "assay") %>%
        select(
          Test = assay,
          `Total Tests` = total_tests,
          Positive = n_positive,
          `% Positive` = pct_positive,
          `Exclusive` = n_exclusive,
          `Shared` = n_shared,
          Negative = n_negative,
          Borderline = n_borderline,
          Invalid = n_invalid
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip'
        ),
        class = "table-sm table-striped",
        rownames = FALSE
      ) %>%
        DT::formatPercentage("% Positive", digits = 1) %>%
        DT::formatStyle(
          "Positive",
          background = DT::styleColorBar(range(table_data$Positive, na.rm = TRUE), "#2563EB"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Overlap Table
    output$overlap_table <- DT::renderDT({
      overlaps <- prepared()$pairwise_overlaps

      if (nrow(overlaps) == 0) {
        return(DT::datatable(
          data.frame(Message = "No overlap data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }

      # Create a symmetric matrix for display
      overlap_matrix <- overlaps %>%
        select(`Test 1` = assay1, `Test 2` = assay2, `Both Positive` = n_both_positive)

      DT::datatable(
        overlap_matrix,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        class = "table-sm table-striped",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Both Positive",
          background = DT::styleColorBar(range(overlap_matrix$`Both Positive`, na.rm = TRUE), "#10B981"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Overlap Heatmap
    output$overlap_heatmap <- renderPlotly({
      overlaps <- prepared()$pairwise_overlaps

      if (nrow(overlaps) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No overlap data available",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      p <- plot_ly(
        data = overlaps,
        x = ~assay2,
        y = ~assay1,
        z = ~n_both_positive,
        type = "heatmap",
        text = ~paste0(assay1, " ∩ ", assay2, "<br>", n_both_positive, " samples"),
        hoverinfo = "text",
        colors = colorRamp(c("#FFFFFF", "#10B981", "#047857"))
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          margin = list(l = 100, r = 20, t = 20, b = 100)
        )

      p
    })

    # Status bars
    output$assay_bars <- renderPlotly({
      df <- filtered_tidy() %>%
        count(assay, status) %>%
        mutate(status = factor(status, levels = names(assay_palette())))
      if (!nrow(df)) return(NULL)
      p <- ggplot(df, aes(x = assay, y = n, fill = status, text = paste("Assay:", assay, "<br>Status:", status, "<br>n:", n))) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = assay_palette()) +
        theme_minimal() + labs(x = NULL, y = "Samples") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
    })

    # UpSet plot
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
        labs(x = "Test combination", y = "Positive samples") +
        theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    })

    # Sample table
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
