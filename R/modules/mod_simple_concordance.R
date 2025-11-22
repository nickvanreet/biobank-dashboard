# Simple Concordance Module
# Simple qualitative (positive/negative) test concordance analysis
# ============================================================================

#' Simple Concordance Module UI
#'
#' @param id Module namespace ID
#' @export
mod_simple_concordance_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Simple Concordance",
    icon = icon("check-double"),

    page_fluid(
      # Header card
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("check-circle-fill", size = "1.5rem"),
            "Simple Test Concordance Analysis",
            span(class = "badge bg-success ms-2", "Qualitative")
          )
        ),
        card_body(
          p("Compare diagnostic test results based on simple positive/negative classification."),
          p(class = "text-muted mb-0 small",
            "Includes agreement metrics, Cohen's Kappa, and confusion matrices.")
        )
      ),

      # Control panel
      layout_columns(
        col_widths = c(4, 4, 4),
        card(
          card_header("Test Comparison"),
          card_body(
            selectInput(
              ns("test_pair"),
              "Select Test Pair",
              choices = c(
                "ELISA-PE vs ELISA-VSG" = "pe_vsg",
                "MIC vs ELISA-PE" = "mic_pe",
                "MIC vs ELISA-VSG" = "mic_vsg",
                "iELISA-L13 vs ELISA-PE" = "ielisa_l13_pe",
                "iELISA-L13 vs ELISA-VSG" = "ielisa_l13_vsg",
                "iELISA-L15 vs ELISA-PE" = "ielisa_l15_pe",
                "iELISA-L15 vs ELISA-VSG" = "ielisa_l15_vsg"
              ),
              selected = "pe_vsg"
            )
          )
        ),
        card(
          card_header("Filters"),
          card_body(
            checkboxInput(
              ns("exclude_qc_fail"),
              "Exclude QC failures",
              value = TRUE
            ),
            selectInput(
              ns("stratify_by"),
              "Stratify By",
              choices = c(
                "None" = "none",
                "Province" = "province",
                "Health Zone" = "health_zone"
              ),
              selected = "none"
            )
          )
        ),
        card(
          card_header("Export"),
          card_body(
            downloadButton(
              ns("download_results"),
              "Download Results",
              class = "btn-primary btn-sm w-100"
            )
          )
        )
      ),

      # Summary KPIs
      layout_column_wrap(
        width = 1/4,
        value_box(
          title = "Total Samples",
          value = textOutput(ns("kpi_total")),
          showcase = icon("vials"),
          theme = "primary"
        ),
        value_box(
          title = "Agreement",
          value = textOutput(ns("kpi_agreement")),
          showcase = icon("check-circle"),
          theme = "success"
        ),
        value_box(
          title = "Cohen's Kappa",
          value = textOutput(ns("kpi_kappa")),
          showcase = icon("balance-scale"),
          theme = "info"
        ),
        value_box(
          title = "Concordant",
          value = textOutput(ns("kpi_concordant")),
          showcase = icon("handshake"),
          theme = "success"
        )
      ),

      # Main content
      layout_columns(
        col_widths = c(6, 6),

        # Confusion matrix
        card(
          card_header("Confusion Matrix"),
          card_body(
            plotly::plotlyOutput(ns("confusion_plot"), height = "400px")
          )
        ),

        # Agreement summary
        card(
          card_header("Agreement Summary"),
          card_body(
            plotly::plotlyOutput(ns("summary_plot"), height = "400px")
          )
        )
      ),

      # Metrics table and stratified results
      layout_columns(
        col_widths = c(6, 6),

        # Metrics table
        card(
          card_header("Concordance Metrics"),
          card_body(
            tableOutput(ns("metrics_table"))
          )
        ),

        # Stratified analysis (conditional)
        card(
          card_header("Stratified Analysis"),
          card_body(
            conditionalPanel(
              condition = "input.stratify_by == 'none'",
              ns = ns,
              div(
                class = "text-center text-muted p-4",
                icon("info-circle", class = "fa-2x mb-2"),
                p("Select a stratification variable to view results by group")
              )
            ),
            conditionalPanel(
              condition = "input.stratify_by != 'none'",
              ns = ns,
              plotly::plotlyOutput(ns("stratified_plot"), height = "400px")
            )
          )
        )
      ),

      # Data table
      card(
        card_header("Matched Samples Data"),
        card_body(
          DT::DTOutput(ns("data_table"))
        )
      )
    )
  )
}


#' Simple Concordance Module Server
#'
#' @param id Module namespace ID
#' @param biobank_df Reactive containing biobank data
#' @param mic_df Reactive containing MIC qPCR data
#' @param elisa_pe_df Reactive containing ELISA-PE data
#' @param elisa_vsg_df Reactive containing ELISA-VSG data
#' @param ielisa_df Reactive containing iELISA data
#' @export
mod_simple_concordance_server <- function(id,
                                          biobank_df = reactive(NULL),
                                          mic_df = reactive(NULL),
                                          elisa_pe_df = reactive(NULL),
                                          elisa_vsg_df = reactive(NULL),
                                          ielisa_df = reactive(NULL)) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # REACTIVE DATA
    # ========================================================================

    # Load and match test data
    matched_data <- reactive({
      req(input$test_pair)

      tryCatch({
        pair <- input$test_pair

        # Get appropriate data based on test pair
        if (pair == "pe_vsg") {
          pe_data <- elisa_pe_df()
          vsg_data <- elisa_vsg_df()

          if (is.null(pe_data) || is.null(vsg_data) ||
              nrow(pe_data) == 0 || nrow(vsg_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match samples
          matched <- match_elisa_samples(pe_data, vsg_data)

          # Create simple concordance format
          matched %>%
            dplyr::mutate(
              test1_name = "ELISA-PE",
              test2_name = "ELISA-VSG",
              test1_positive = (pe_PP_percent >= 20) | (pe_DOD >= 0.3),
              test2_positive = (vsg_PP_percent >= 20) | (vsg_DOD >= 0.3),
              test1_value = pe_PP_percent,
              test2_value = vsg_PP_percent,
              province = Province,
              health_zone = HealthZone
            )

        } else if (pair == "mic_pe") {
          mic_data <- mic_df()
          pe_data <- elisa_pe_df()

          if (is.null(mic_data) || is.null(pe_data) ||
              nrow(mic_data) == 0 || nrow(pe_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_mic_elisa(mic_data, pe_data, "PE") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else if (pair == "mic_vsg") {
          mic_data <- mic_df()
          vsg_data <- elisa_vsg_df()

          if (is.null(mic_data) || is.null(vsg_data) ||
              nrow(mic_data) == 0 || nrow(vsg_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_mic_elisa(mic_data, vsg_data, "VSG") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else if (pair == "ielisa_l13_pe") {
          ielisa_data <- ielisa_df()
          pe_data <- elisa_pe_df()

          if (is.null(ielisa_data) || is.null(pe_data) ||
              nrow(ielisa_data) == 0 || nrow(pe_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data <- ielisa_data %>% dplyr::filter(plate_valid_L13 == TRUE)
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_ielisa_elisa(ielisa_data, pe_data, "L13", "PE") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else if (pair == "ielisa_l13_vsg") {
          ielisa_data <- ielisa_df()
          vsg_data <- elisa_vsg_df()

          if (is.null(ielisa_data) || is.null(vsg_data) ||
              nrow(ielisa_data) == 0 || nrow(vsg_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data <- ielisa_data %>% dplyr::filter(plate_valid_L13 == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_ielisa_elisa(ielisa_data, vsg_data, "L13", "VSG") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else if (pair == "ielisa_l15_pe") {
          ielisa_data <- ielisa_df()
          pe_data <- elisa_pe_df()

          if (is.null(ielisa_data) || is.null(pe_data) ||
              nrow(ielisa_data) == 0 || nrow(pe_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data <- ielisa_data %>% dplyr::filter(plate_valid_L15 == TRUE)
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_ielisa_elisa(ielisa_data, pe_data, "L15", "PE") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else if (pair == "ielisa_l15_vsg") {
          ielisa_data <- ielisa_df()
          vsg_data <- elisa_vsg_df()

          if (is.null(ielisa_data) || is.null(vsg_data) ||
              nrow(ielisa_data) == 0 || nrow(vsg_data) == 0) {
            return(NULL)
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data <- ielisa_data %>% dplyr::filter(plate_valid_L15 == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and format
          match_ielisa_elisa(ielisa_data, vsg_data, "L15", "VSG") %>%
            dplyr::rename(
              test1_positive = test1_binary,
              test2_positive = test2_binary
            )

        } else {
          return(NULL)
        }

      }, error = function(e) {
        message("Error in matched_data: ", e$message)
        return(NULL)
      })
    })

    # Calculate simple concordance metrics
    concordance_metrics <- reactive({
      req(matched_data())
      data <- matched_data()

      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      calculate_simple_concordance(
        test1 = data$test1_positive,
        test2 = data$test2_positive,
        test1_name = data$test1_name[1],
        test2_name = data$test2_name[1]
      )
    })

    # Stratified metrics
    stratified_metrics <- reactive({
      req(matched_data())
      req(input$stratify_by != "none")

      data <- matched_data()

      if (is.null(data) || nrow(data) == 0) {
        return(NULL)
      }

      stratify_col <- input$stratify_by

      if (!stratify_col %in% names(data)) {
        return(NULL)
      }

      # Calculate metrics by group
      data %>%
        dplyr::filter(!is.na(.data[[stratify_col]])) %>%
        dplyr::group_by(.data[[stratify_col]]) %>%
        dplyr::summarise(
          n_total = dplyr::n(),
          n_concordant = sum(test1_positive == test2_positive, na.rm = TRUE),
          n_discordant = sum(test1_positive != test2_positive, na.rm = TRUE),
          n_both_positive = sum(test1_positive & test2_positive, na.rm = TRUE),
          n_both_negative = sum(!test1_positive & !test2_positive, na.rm = TRUE),
          pct_agreement = (n_concordant / n_total) * 100,
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(pct_agreement))
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_total <- renderText({
      metrics <- concordance_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$n_total)
    })

    output$kpi_agreement <- renderText({
      metrics <- concordance_metrics()
      if (is.null(metrics)) return("N/A")
      sprintf("%.1f%%", metrics$pct_agreement)
    })

    output$kpi_kappa <- renderText({
      metrics <- concordance_metrics()
      if (is.null(metrics)) return("N/A")
      sprintf("%.3f", metrics$kappa)
    })

    output$kpi_concordant <- renderText({
      metrics <- concordance_metrics()
      if (is.null(metrics)) return("0")
      sprintf("%s/%s", scales::comma(metrics$n_concordant), scales::comma(metrics$n_total))
    })

    # ========================================================================
    # VISUALIZATIONS
    # ========================================================================

    output$confusion_plot <- plotly::renderPlotly({
      metrics <- concordance_metrics()
      if (is.null(metrics)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      plot_simple_confusion_matrix(
        metrics$confusion_matrix,
        metrics$test1_name,
        metrics$test2_name
      )
    })

    output$summary_plot <- plotly::renderPlotly({
      metrics <- concordance_metrics()
      if (is.null(metrics)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      plot_agreement_bars(metrics)
    })

    output$stratified_plot <- plotly::renderPlotly({
      strat <- stratified_metrics()
      if (is.null(strat) || nrow(strat) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No stratified data available"))
      }

      plot_stratified_agreement(strat, input$stratify_by)
    })

    # ========================================================================
    # TABLES
    # ========================================================================

    output$metrics_table <- renderTable({
      metrics <- concordance_metrics()
      if (is.null(metrics)) return(NULL)

      tibble::tibble(
        Metric = c(
          "Total Samples",
          "Agreement (%)",
          "Cohen's Kappa",
          "Kappa Interpretation",
          "Both Positive",
          "Both Negative",
          "Discordant"
        ),
        Value = c(
          as.character(metrics$n_total),
          sprintf("%.1f%%", metrics$pct_agreement),
          sprintf("%.3f", metrics$kappa),
          metrics$kappa_interpretation,
          sprintf("%d (%.1f%%)", metrics$n_both_positive,
                  (metrics$n_both_positive / metrics$n_total) * 100),
          sprintf("%d (%.1f%%)", metrics$n_both_negative,
                  (metrics$n_both_negative / metrics$n_total) * 100),
          sprintf("%d (%.1f%%)", metrics$n_discordant,
                  (metrics$n_discordant / metrics$n_total) * 100)
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$data_table <- DT::renderDT({
      data <- matched_data()
      if (is.null(data) || nrow(data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      # Select display columns
      display_data <- data %>%
        dplyr::mutate(
          concordance = dplyr::case_when(
            test1_positive & test2_positive ~ "Both Positive",
            !test1_positive & !test2_positive ~ "Both Negative",
            test1_positive & !test2_positive ~ paste0(test1_name[1], " Only"),
            !test1_positive & test2_positive ~ paste0(test2_name[1], " Only")
          )
        ) %>%
        dplyr::select(
          dplyr::any_of(c(
            "test1_name", "test2_name",
            "test1_positive", "test2_positive",
            "concordance",
            "province", "health_zone",
            "test1_value", "test2_value"
          ))
        )

      DT::datatable(
        display_data,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = "table-sm table-striped"
      )
    })

    # ========================================================================
    # DOWNLOAD HANDLER
    # ========================================================================

    output$download_results <- downloadHandler(
      filename = function() {
        paste0("simple_concordance_", input$test_pair, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        data <- matched_data()
        metrics <- concordance_metrics()

        if (is.null(data) || is.null(metrics)) {
          return(NULL)
        }

        wb <- openxlsx::createWorkbook()

        # Summary sheet
        openxlsx::addWorksheet(wb, "Summary")
        summary_df <- tibble::tibble(
          Metric = c("Test 1", "Test 2", "Total Samples", "Agreement (%)",
                    "Cohen's Kappa", "Both Positive", "Both Negative", "Discordant"),
          Value = c(
            metrics$test1_name,
            metrics$test2_name,
            as.character(metrics$n_total),
            sprintf("%.1f%%", metrics$pct_agreement),
            sprintf("%.3f", metrics$kappa),
            as.character(metrics$n_both_positive),
            as.character(metrics$n_both_negative),
            as.character(metrics$n_discordant)
          )
        )
        openxlsx::writeData(wb, "Summary", summary_df)

        # Confusion matrix
        openxlsx::addWorksheet(wb, "Confusion Matrix")
        openxlsx::writeData(wb, "Confusion Matrix", metrics$confusion_matrix)

        # Data
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeData(wb, "Data", data)

        # Stratified (if available)
        if (!is.null(stratified_metrics())) {
          openxlsx::addWorksheet(wb, "Stratified")
          openxlsx::writeData(wb, "Stratified", stratified_metrics())
        }

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  })
}
