# Concordance Analysis Module
# Comprehensive test concordance analysis with statistics, visualizations, and predictions
# ============================================================================

#' Concordance Analysis Module UI
#'
#' @param id Module namespace ID
#' @export
mod_concordance_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Concordance Analysis",
    icon = icon("project-diagram"),

    page_fluid(
      # Header card
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("diagram-3", size = "1.5rem"),
            "Comprehensive Test Concordance Analysis",
            span(class = "badge bg-info ms-2", "Advanced Analytics")
          )
        ),
        card_body(
          p("Advanced concordance analysis comparing diagnostic test results with statistical modeling, interactive visualizations, and predictive analytics."),
          p(class = "text-muted mb-0 small",
            "Includes Cohen's Kappa, ROC analysis, Bland-Altman plots, and machine learning predictions.")
        )
      ),

      # Control panel
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        card(
          card_header("Test Comparison"),
          card_body(
            selectInput(
              ns("test_comparison"),
              "Select Comparison",
              choices = c(
                "MIC vs ELISA-PE" = "mic_pe",
                "MIC vs ELISA-VSG" = "mic_vsg",
                "ELISA-PE vs ELISA-VSG" = "pe_vsg",
                "iELISA-L13 vs ELISA-PE" = "ielisa_l13_pe",
                "iELISA-L13 vs ELISA-VSG" = "ielisa_l13_vsg",
                "iELISA-L15 vs ELISA-PE" = "ielisa_l15_pe",
                "iELISA-L15 vs ELISA-VSG" = "ielisa_l15_vsg",
                "iELISA-L13 vs iELISA-L15" = "ielisa_l13_l15",
                "Multi-way (All Tests)" = "multiway"
              ),
              selected = "pe_vsg"
            ),
            checkboxInput(
              ns("exclude_qc_fail"),
              "Exclude QC failures",
              value = TRUE
            )
          )
        ),
        card(
          card_header("Stratification"),
          card_body(
            selectInput(
              ns("stratify_by"),
              "Stratify By",
              choices = c(
                "None" = "none",
                "Province" = "province",
                "Health Zone" = "health_zone",
                "Study Phase" = "study_phase",
                "Time Period" = "time_period"
              )
            ),
            conditionalPanel(
              condition = "input.stratify_by == 'time_period'",
              ns = ns,
              selectInput(
                ns("time_grouping"),
                "Group By",
                choices = c("Week" = "week", "Month" = "month", "Quarter" = "quarter")
              )
            )
          )
        ),
        card(
          card_header("Analysis Options"),
          card_body(
            checkboxInput(
              ns("calculate_bootstrap"),
              "Bootstrap CIs",
              value = TRUE
            ),
            checkboxInput(
              ns("enable_prediction"),
              "Enable ML Predictions",
              value = FALSE
            ),
            numericInput(
              ns("bootstrap_iterations"),
              "Bootstrap N",
              value = 1000,
              min = 100,
              max = 10000,
              step = 100
            )
          )
        ),
        card(
          card_header("Export"),
          card_body(
            downloadButton(ns("download_report_pdf"), "PDF Report",
                          class = "btn-primary btn-sm w-100 mb-2"),
            downloadButton(ns("download_data_excel"), "Excel Data",
                          class = "btn-success btn-sm w-100 mb-2"),
            downloadButton(ns("download_slides_pptx"), "PowerPoint",
                          class = "btn-info btn-sm w-100")
          )
        )
      ),

      # Main content tabs
      navset_card_tab(
        id = ns("main_tabs"),

        # Tab 1: Summary Dashboard
        nav_panel(
          title = "Dashboard",
          icon = icon("tachometer-alt"),

          # KPI cards
          layout_column_wrap(
            width = 1/5,
            value_box(
              title = "Total Samples",
              value = textOutput(ns("kpi_total_samples")),
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
              title = "Sensitivity",
              value = textOutput(ns("kpi_sensitivity")),
              showcase = icon("bullseye"),
              theme = "warning"
            ),
            value_box(
              title = "Specificity",
              value = textOutput(ns("kpi_specificity")),
              showcase = icon("shield-alt"),
              theme = "danger"
            )
          ),

          # Gauges and confusion matrix
          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("Agreement Metrics Dashboard"),
              card_body_fill(
                plotly::plotlyOutput(ns("gauges_plot"), height = "400px")
              )
            ),
            card(
              card_header("Confusion Matrix"),
              card_body_fill(
                plotly::plotlyOutput(ns("confusion_heatmap"), height = "400px")
              )
            )
          ),

          # Summary table
          card(
            card_header("Concordance Metrics Summary"),
            card_body(
              DT::DTOutput(ns("metrics_table"))
            )
          )
        ),

        # Tab 2: Visualizations
        nav_panel(
          title = "Visualizations",
          icon = icon("chart-bar"),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Sankey Flow Diagram"),
              card_body_fill(
                plotly::plotlyOutput(ns("sankey_plot"), height = "400px")
              )
            ),
            card(
              card_header("Geographic Concordance"),
              card_body_fill(
                plotly::plotlyOutput(ns("geographic_plot"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Bland-Altman Plot"),
              card_body_fill(
                plotly::plotlyOutput(ns("bland_altman_plot"), height = "450px")
              )
            ),
            card(
              card_header("Quality Trends Over Time"),
              card_body_fill(
                plotly::plotlyOutput(ns("quality_trends_plot"), height = "450px")
              )
            )
          )
        ),

        # Tab 3: ROC Analysis
        nav_panel(
          title = "ROC Analysis",
          icon = icon("chart-line"),

          card(
            card_header("ROC Curves"),
            card_body_fill(
              plotly::plotlyOutput(ns("roc_plot"), height = "500px")
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("ROC Statistics"),
              card_body(
                DT::DTOutput(ns("roc_stats_table"))
              )
            ),
            card(
              card_header("Optimal Cutpoints"),
              card_body(
                DT::DTOutput(ns("optimal_cutpoints_table"))
              )
            )
          )
        ),

        # Tab 4: Stratified Analysis
        nav_panel(
          title = "Stratified Analysis",
          icon = icon("layer-group"),

          card(
            card_header("Concordance by Strata"),
            card_body_fill(
              plotly::plotlyOutput(ns("stratified_plot"), height = "500px")
            )
          ),

          card(
            card_header("Stratified Metrics Table"),
            card_body(
              DT::DTOutput(ns("stratified_table"))
            )
          )
        ),

        # Tab 5: Predictive Models
        nav_panel(
          title = "Predictions",
          icon = icon("brain"),

          conditionalPanel(
            condition = "input.enable_prediction == false",
            ns = ns,
            card(
              card_body(
                div(
                  class = "text-center p-5",
                  icon("info-circle", class = "fa-3x text-info mb-3"),
                  h4("Predictive Modeling Disabled"),
                  p("Enable 'ML Predictions' in Analysis Options to access predictive analytics.")
                )
              )
            )
          ),

          conditionalPanel(
            condition = "input.enable_prediction == true",
            ns = ns,

            card(
              card_header("Model Performance"),
              card_body(
                layout_columns(
                  col_widths = c(6, 6),
                  DT::DTOutput(ns("model_performance_table")),
                  DT::DTOutput(ns("cv_results_table"))
                )
              )
            ),

            layout_columns(
              col_widths = c(6, 6),
              card(
                card_header("Variable Importance"),
                card_body_fill(
                  plotly::plotlyOutput(ns("var_importance_plot"), height = "400px")
                )
              ),
              card(
                card_header("Discordance Risk Distribution"),
                card_body_fill(
                  plotly::plotlyOutput(ns("risk_distribution_plot"), height = "400px")
                )
              )
            ),

            card(
              card_header("High-Risk Samples"),
              card_body(
                DT::DTOutput(ns("high_risk_samples_table"))
              )
            )
          )
        ),

        # Tab 6: Data Table
        nav_panel(
          title = "Data",
          icon = icon("table"),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Concordance Data"),
              span(textOutput(ns("data_count"), inline = TRUE), class = "badge bg-primary")
            ),
            card_body(
              DT::DTOutput(ns("concordance_data_table"))
            )
          )
        )
      )
    )
  )
}


#' Concordance Analysis Module Server
#'
#' @param id Module namespace ID
#' @param biobank_df Reactive containing biobank data
#' @param mic_df Reactive containing MIC qPCR data
#' @param elisa_pe_df Reactive containing ELISA-PE data (optional, for future use)
#' @param elisa_vsg_df Reactive containing ELISA-VSG data (optional, for future use)
#' @param ielisa_df Reactive containing iELISA data (optional, for future use)
#' @param filters Reactive containing filter settings
#' @export
mod_concordance_server <- function(id,
                                   biobank_df = reactive(NULL),
                                   mic_df = reactive(NULL),
                                   elisa_pe_df = reactive(NULL),
                                   elisa_vsg_df = reactive(NULL),
                                   ielisa_df = reactive(NULL),
                                   filters = reactive(NULL)) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # REACTIVE DATA LOADING
    # ========================================================================

    # Load test data based on comparison type
    test_data <- reactive({
      req(input$test_comparison)

      tryCatch({
        comparison <- input$test_comparison

        if (comparison == "pe_vsg") {
          # Load ELISA PE and VSG data from passed parameters
          pe_data <- elisa_pe_df()
          vsg_data <- elisa_vsg_df()

          # Check if data is available
          if (is.null(pe_data) || is.null(vsg_data) ||
              nrow(pe_data) == 0 || nrow(vsg_data) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match samples
          matched <- match_elisa_samples(pe_data, vsg_data)

          # Create concordance data
          matched %>%
            dplyr::mutate(
              test1_value = pe_PP_percent,
              test2_value = vsg_PP_percent,
              test1_binary = calculate_positivity(pe_PP_percent, pe_DOD),
              test2_binary = calculate_positivity(vsg_PP_percent, vsg_DOD),
              test1_name = "ELISA-PE",
              test2_name = "ELISA-VSG",
              health_zone = HealthZone,
              province = Province,
              date = coalesce(pe_plate_date, vsg_plate_date)
            )

        } else if (comparison == "ielisa_l13_pe") {
          # iELISA L13 vs ELISA-PE
          ielisa_data_raw <- ielisa_df()
          pe_data <- elisa_pe_df()

          # Debug: Log data availability
          message("=== iELISA-L13 vs ELISA-PE Concordance Debug ===")
          message(sprintf("iELISA data: %s rows", if (is.null(ielisa_data_raw)) "NULL" else nrow(ielisa_data_raw)))
          message(sprintf("PE data: %s rows", if (is.null(pe_data)) "NULL" else nrow(pe_data)))

          # Check if data is available
          if (is.null(ielisa_data_raw) || is.null(pe_data) ||
              nrow(ielisa_data_raw) == 0 || nrow(pe_data) == 0) {
            message("Returning empty tibble - no data available")
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_before <- nrow(ielisa_data_raw)
            ielisa_data_raw <- ielisa_data_raw %>% dplyr::filter(plate_valid_L13 == TRUE)
            pe_before <- nrow(pe_data)
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
            message(sprintf("iELISA after QC: %s rows (removed %s)", nrow(ielisa_data_raw), ielisa_before - nrow(ielisa_data_raw)))
            message(sprintf("PE after QC: %s rows (removed %s)", nrow(pe_data), pe_before - nrow(pe_data)))
          }

          # Match and create concordance data
          matched <- match_ielisa_elisa(ielisa_data_raw, pe_data, "L13", "PE")
          message(sprintf("Matched samples: %s rows", nrow(matched)))
          matched

        } else if (comparison == "ielisa_l13_vsg") {
          # iELISA L13 vs ELISA-VSG
          ielisa_data_raw <- ielisa_df()
          vsg_data <- elisa_vsg_df()

          # Check if data is available
          if (is.null(ielisa_data_raw) || is.null(vsg_data) ||
              nrow(ielisa_data_raw) == 0 || nrow(vsg_data) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data_raw <- ielisa_data_raw %>% dplyr::filter(plate_valid_L13 == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and create concordance data
          match_ielisa_elisa(ielisa_data_raw, vsg_data, "L13", "VSG")

        } else if (comparison == "ielisa_l15_pe") {
          # iELISA L15 vs ELISA-PE
          ielisa_data_raw <- ielisa_df()
          pe_data <- elisa_pe_df()

          # Check if data is available
          if (is.null(ielisa_data_raw) || is.null(pe_data) ||
              nrow(ielisa_data_raw) == 0 || nrow(pe_data) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data_raw <- ielisa_data_raw %>% dplyr::filter(plate_valid_L15 == TRUE)
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and create concordance data
          match_ielisa_elisa(ielisa_data_raw, pe_data, "L15", "PE")

        } else if (comparison == "ielisa_l15_vsg") {
          # iELISA L15 vs ELISA-VSG
          ielisa_data_raw <- ielisa_df()
          vsg_data <- elisa_vsg_df()

          # Check if data is available
          if (is.null(ielisa_data_raw) || is.null(vsg_data) ||
              nrow(ielisa_data_raw) == 0 || nrow(vsg_data) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data_raw <- ielisa_data_raw %>% dplyr::filter(plate_valid_L15 == TRUE)
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and create concordance data
          match_ielisa_elisa(ielisa_data_raw, vsg_data, "L15", "VSG")

        } else if (comparison == "ielisa_l13_l15") {
          # iELISA L13 vs L15 (same samples, different antigens)
          ielisa_data_raw <- ielisa_df()

          # Check if data is available
          if (is.null(ielisa_data_raw) || nrow(ielisa_data_raw) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            ielisa_data_raw <- ielisa_data_raw %>%
              dplyr::filter(plate_valid_L13 == TRUE, plate_valid_L15 == TRUE)
          }

          # Create concordance data (both antigens are in same dataset)
          ielisa_data_raw %>%
            dplyr::mutate(
              test1_value = pct_inh_f2_13,
              test2_value = pct_inh_f2_15,
              test1_binary = positive_L13,
              test2_binary = positive_L15,
              test1_name = "iELISA-L13",
              test2_name = "iELISA-L15",
              health_zone = as.character(NA),
              province = as.character(NA),
              date = plate_date
            )

        } else if (comparison == "mic_pe") {
          # MIC vs ELISA-PE
          mic_data_raw <- mic_df()
          pe_data <- elisa_pe_df()

          # Debug: Log data availability
          message("=== MIC vs ELISA-PE Concordance Debug ===")
          message(sprintf("MIC data: %s rows", if (is.null(mic_data_raw)) "NULL" else nrow(mic_data_raw)))
          message(sprintf("PE data: %s rows", if (is.null(pe_data)) "NULL" else nrow(pe_data)))

          # Check if data is available
          if (is.null(mic_data_raw) || is.null(pe_data) ||
              nrow(mic_data_raw) == 0 || nrow(pe_data) == 0) {
            message("Returning empty tibble - no data available")
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            pe_before <- nrow(pe_data)
            pe_data <- pe_data %>% dplyr::filter(qc_overall == TRUE)
            message(sprintf("PE data after QC filter: %s rows (removed %s)", nrow(pe_data), pe_before - nrow(pe_data)))
          }

          # Match and create concordance data
          matched <- match_mic_elisa(mic_data_raw, pe_data, "PE")
          message(sprintf("Matched samples: %s rows", nrow(matched)))
          matched

        } else if (comparison == "mic_vsg") {
          # MIC vs ELISA-VSG
          mic_data_raw <- mic_df()
          vsg_data <- elisa_vsg_df()

          # Check if data is available
          if (is.null(mic_data_raw) || is.null(vsg_data) ||
              nrow(mic_data_raw) == 0 || nrow(vsg_data) == 0) {
            return(tibble::tibble())
          }

          # Apply QC filters
          if (input$exclude_qc_fail) {
            vsg_data <- vsg_data %>% dplyr::filter(qc_overall == TRUE)
          }

          # Match and create concordance data
          match_mic_elisa(mic_data_raw, vsg_data, "VSG")

        } else {
          # Multi-way comparison
          tibble::tibble()
        }

      }, error = function(e) {
        warning(paste("Error loading test data:", e$message))
        tibble::tibble()
      })
    })

    # Apply stratification
    stratified_data <- reactive({
      req(test_data())
      data <- test_data()

      if (nrow(data) == 0) return(data)

      if (input$stratify_by != "none") {
        stratify_col <- switch(
          input$stratify_by,
          "province" = "province",
          "health_zone" = "health_zone",
          "study_phase" = "Cohort",
          "time_period" = "period"
        )

        if (input$stratify_by == "time_period" && "date" %in% names(data)) {
          data <- data %>%
            dplyr::mutate(
              period = lubridate::floor_date(date, unit = input$time_grouping)
            )
        }

        if (stratify_col %in% names(data)) {
          data <- data %>%
            dplyr::filter(!is.na(.data[[stratify_col]]))
        }
      }

      data
    })

    # ========================================================================
    # CONCORDANCE CALCULATIONS
    # ========================================================================

    concordance_metrics <- reactive({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) {
        # Create empty confusion matrix with proper row/column names
        cm_empty <- matrix(0, nrow = 2, ncol = 2)
        rownames(cm_empty) <- c("Positive", "Negative")
        colnames(cm_empty) <- c("Positive", "Negative")

        return(list(
          n = 0,
          test1_name = "Test1",
          test2_name = "Test2",
          metrics = data.frame(),
          confusion_matrix = cm_empty
        ))
      }

      tryCatch({
        calculate_concordance_metrics(
          test1 = data$test1_binary,
          test2 = data$test2_binary,
          test1_name = data$test1_name[1],
          test2_name = data$test2_name[1],
          bootstrap = input$calculate_bootstrap,
          n_bootstrap = input$bootstrap_iterations
        )
      }, error = function(e) {
        warning(paste("Error calculating concordance:", e$message))

        # Create empty confusion matrix with proper row/column names
        cm_empty <- matrix(0, nrow = 2, ncol = 2)
        rownames(cm_empty) <- c("Positive", "Negative")
        colnames(cm_empty) <- c("Positive", "Negative")

        list(
          n = 0,
          test1_name = "Test1",
          test2_name = "Test2",
          metrics = data.frame(),
          confusion_matrix = cm_empty
        )
      })
    })

    # ROC analysis
    roc_results <- reactive({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) return(NULL)

      tryCatch({
        # Calculate ROC for test1 predicting test2
        roc1 <- calculate_roc_metrics(
          test_values = data$test1_value,
          reference = data$test2_binary,
          test_name = data$test1_name[1]
        )

        # Calculate ROC for test2 predicting test1
        roc2 <- calculate_roc_metrics(
          test_values = data$test2_value,
          reference = data$test1_binary,
          test_name = data$test2_name[1]
        )

        list(roc1 = roc1, roc2 = roc2)

      }, error = function(e) {
        warning(paste("Error in ROC analysis:", e$message))
        NULL
      })
    })

    # Bland-Altman analysis
    bland_altman_result <- reactive({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) return(NULL)

      tryCatch({
        bland_altman_analysis(
          method1 = data$test1_value,
          method2 = data$test2_value,
          method1_name = data$test1_name[1],
          method2_name = data$test2_name[1]
        )
      }, error = function(e) {
        warning(paste("Error in Bland-Altman:", e$message))
        NULL
      })
    })

    # Stratified metrics
    stratified_metrics <- reactive({
      req(stratified_data())
      data <- stratified_data()

      if (input$stratify_by == "none" || nrow(data) == 0) {
        return(NULL)
      }

      stratify_col <- switch(
        input$stratify_by,
        "province" = "province",
        "health_zone" = "health_zone",
        "study_phase" = "Cohort",
        "time_period" = "period"
      )

      if (!stratify_col %in% names(data)) {
        return(NULL)
      }

      tryCatch({
        data %>%
          dplyr::group_by(.data[[stratify_col]]) %>%
          dplyr::summarise(
            n = dplyr::n(),
            agreement = mean(test1_binary == test2_binary, na.rm = TRUE) * 100,
            both_positive = sum(test1_binary & test2_binary, na.rm = TRUE),
            both_negative = sum(!test1_binary & !test2_binary, na.rm = TRUE),
            discordant = sum(test1_binary != test2_binary, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::filter(n >= 5) %>%
          dplyr::arrange(dplyr::desc(agreement))

      }, error = function(e) {
        warning(paste("Error in stratified analysis:", e$message))
        NULL
      })
    })

    # ========================================================================
    # PREDICTIVE MODELING
    # ========================================================================

    predictive_model <- reactive({
      req(input$enable_prediction)
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) < 100) {
        return(list(
          model = NULL,
          error = "Insufficient data for predictive modeling (minimum 100 samples required)"
        ))
      }

      # Prepare features
      feature_cols <- c("test1_value", "test2_value")
      if ("Age" %in% names(data)) feature_cols <- c(feature_cols, "Age")
      if ("Sex" %in% names(data)) {
        data$Sex_binary <- as.numeric(data$Sex == "M")
        feature_cols <- c(feature_cols, "Sex_binary")
      }

      tryCatch({
        # Build model to predict discordance
        data$is_discordant <- data$test1_binary != data$test2_binary

        build_predictive_model(
          data = data,
          outcome_col = "is_discordant",
          feature_cols = feature_cols,
          model_type = "rf",
          train_fraction = 0.75,
          cv_folds = 5
        )

      }, error = function(e) {
        warning(paste("Error in predictive modeling:", e$message))
        list(model = NULL, error = e$message)
      })
    })

    discordance_risk <- reactive({
      req(input$enable_prediction)
      req(predictive_model())

      if (is.null(predictive_model()$model)) {
        return(NULL)
      }

      tryCatch({
        predict_discordance_risk(
          data = stratified_data(),
          test1_col = "test1_binary",
          test2_col = "test2_binary",
          feature_cols = predictive_model()$feature_cols
        )

      }, error = function(e) {
        warning(paste("Error predicting discordance risk:", e$message))
        NULL
      })
    })

    # ========================================================================
    # OUTPUTS - KPIs
    # ========================================================================

    output$kpi_total_samples <- renderText({
      metrics <- concordance_metrics()
      if (metrics$n == 0) "0" else scales::comma(metrics$n)
    })

    output$kpi_agreement <- renderText({
      metrics <- concordance_metrics()
      if (nrow(metrics$metrics) == 0) return("N/A")

      agreement <- metrics$metrics$value[metrics$metrics$metric == "Percent Agreement"]
      if (is.na(agreement)) "N/A" else sprintf("%.1f%%", agreement)
    })

    output$kpi_kappa <- renderText({
      metrics <- concordance_metrics()
      if (nrow(metrics$metrics) == 0) return("N/A")

      kappa <- metrics$metrics$value[metrics$metrics$metric == "Cohen's Kappa"]
      if (is.na(kappa)) "N/A" else sprintf("%.3f", kappa)
    })

    output$kpi_sensitivity <- renderText({
      metrics <- concordance_metrics()
      if (nrow(metrics$metrics) == 0) return("N/A")

      sens <- metrics$metrics$value[metrics$metrics$metric == "Sensitivity"]
      if (is.na(sens)) "N/A" else sprintf("%.1f%%", sens * 100)
    })

    output$kpi_specificity <- renderText({
      metrics <- concordance_metrics()
      if (nrow(metrics$metrics) == 0) return("N/A")

      spec <- metrics$metrics$value[metrics$metrics$metric == "Specificity"]
      if (is.na(spec)) "N/A" else sprintf("%.1f%%", spec * 100)
    })

    # ========================================================================
    # OUTPUTS - VISUALIZATIONS
    # ========================================================================

    output$gauges_plot <- plotly::renderPlotly({
      req(concordance_metrics())
      metrics <- concordance_metrics()

      if (nrow(metrics$metrics) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      tryCatch({
        plot_agreement_gauges(metrics$metrics)
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$confusion_heatmap <- plotly::renderPlotly({
      req(concordance_metrics())
      metrics <- concordance_metrics()

      tryCatch({
        plot_confusion_matrix(
          confusion_matrix = metrics$confusion_matrix,
          test1_name = metrics$test1_name,
          test2_name = metrics$test2_name
        )
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$sankey_plot <- plotly::renderPlotly({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      tryCatch({
        plot_sankey_flow(
          data = data %>%
            dplyr::rename(test1 = test1_binary, test2 = test2_binary),
          test1_name = data$test1_name[1],
          test2_name = data$test2_name[1]
        )
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$geographic_plot <- plotly::renderPlotly({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No data available"))
      }

      tryCatch({
        data_with_concordance <- data %>%
          dplyr::mutate(is_concordant = test1_binary == test2_binary)

        plot_geographic_concordance(data_with_concordance)
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$bland_altman_plot <- plotly::renderPlotly({
      req(bland_altman_result())
      ba <- bland_altman_result()

      tryCatch({
        plot_bland_altman(ba)
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$quality_trends_plot <- plotly::renderPlotly({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0 || !"date" %in% names(data)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No temporal data available"))
      }

      tryCatch({
        data_with_concordance <- data %>%
          dplyr::mutate(is_concordant = test1_binary == test2_binary)

        plot_quality_trends(
          data = data_with_concordance,
          date_col = "date",
          group_by = "week"
        )
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$roc_plot <- plotly::renderPlotly({
      req(roc_results())
      roc_list <- roc_results()

      tryCatch({
        plot_roc_curves(list(roc_list$roc1, roc_list$roc2))
      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$stratified_plot <- plotly::renderPlotly({
      req(stratified_metrics())
      strat_metrics <- stratified_metrics()

      if (is.null(strat_metrics) || nrow(strat_metrics) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "No stratified data available"))
      }

      tryCatch({
        strat_col <- names(strat_metrics)[1]

        plotly::plot_ly(
          data = strat_metrics,
          x = ~agreement,
          y = stats::reorder(strat_metrics[[strat_col]], strat_metrics$agreement),
          type = "bar",
          orientation = "h",
          marker = list(
            color = ~agreement,
            colorscale = list(c(0, "#dc3545"), c(0.5, "#ffc107"), c(1, "#28a745")),
            colorbar = list(title = "Agreement %")
          ),
          text = ~sprintf("%.1f%% (%d/%d)", agreement, both_positive + both_negative, n),
          textposition = "outside",
          hovertemplate = paste0(
            "%{y}<br>",
            "Agreement: %{x:.1f}%<br>",
            "N: %{text}<extra></extra>"
          )
        ) %>%
          plotly::layout(
            title = paste("Concordance by", tools::toTitleCase(input$stratify_by)),
            xaxis = list(title = "Agreement (%)", range = c(0, 105)),
            yaxis = list(title = ""),
            margin = list(l = 150)
          )

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$var_importance_plot <- plotly::renderPlotly({
      req(input$enable_prediction)
      req(predictive_model())
      model <- predictive_model()

      if (is.null(model$model)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "Model not available"))
      }

      tryCatch({
        var_imp <- model$variable_importance %>%
          dplyr::slice_head(n = 10)

        plotly::plot_ly(
          data = var_imp,
          x = ~importance,
          y = ~reorder(feature, importance),
          type = "bar",
          orientation = "h",
          marker = list(color = "#6610f2")
        ) %>%
          plotly::layout(
            title = "Top 10 Important Features",
            xaxis = list(title = "Importance"),
            yaxis = list(title = "")
          )

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    output$risk_distribution_plot <- plotly::renderPlotly({
      req(input$enable_prediction)
      req(discordance_risk())
      risk <- discordance_risk()

      if (is.null(risk$risk_scores)) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = "Risk scores not available"))
      }

      tryCatch({
        plotly::plot_ly(
          data = risk$risk_scores,
          x = ~predicted_prob,
          type = "histogram",
          nbinsx = 30,
          marker = list(
            color = "#0d6efd",
            line = list(color = "white", width = 1)
          )
        ) %>%
          plotly::layout(
            title = "Discordance Risk Distribution",
            xaxis = list(title = "Predicted Risk of Discordance"),
            yaxis = list(title = "Count")
          )

      }, error = function(e) {
        plotly::plot_ly() %>%
          plotly::layout(title = paste("Error:", e$message))
      })
    })

    # ========================================================================
    # OUTPUTS - TABLES
    # ========================================================================

    output$metrics_table <- DT::renderDT({
      req(concordance_metrics())
      metrics <- concordance_metrics()

      if (nrow(metrics$metrics) == 0) {
        return(DT::datatable(data.frame(Message = "No data")))
      }

      metrics_df <- metrics$metrics %>%
        dplyr::mutate(
          value = round(value, 4),
          formatted_value = dplyr::case_when(
            metric %in% c("N", "Kappa Z-score") ~ as.character(round(value)),
            metric %in% c("Kappa P-value", "McNemar P-value") ~
              ifelse(value < 0.001, "<0.001", sprintf("%.4f", value)),
            metric == "Percent Agreement" ~ sprintf("%.2f%%", value),
            TRUE ~ sprintf("%.4f", value)
          )
        ) %>%
        dplyr::select(Metric = metric, Value = formatted_value, dplyr::everything())

      if ("ci_lower" %in% names(metrics_df)) {
        metrics_df <- metrics_df %>%
          dplyr::mutate(
            CI_95 = sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
          ) %>%
          dplyr::select(Metric, Value, CI_95)
      } else {
        metrics_df <- metrics_df %>%
          dplyr::select(Metric, Value)
      }

      DT::datatable(
        metrics_df,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          dom = 't',
          scrollX = TRUE
        ),
        class = "table-sm"
      )
    })

    output$roc_stats_table <- DT::renderDT({
      req(roc_results())
      roc_list <- roc_results()

      stats_df <- data.frame(
        Test = c(roc_list$roc1$test_name, roc_list$roc2$test_name),
        AUC = c(roc_list$roc1$auc, roc_list$roc2$auc),
        AUC_CI_Lower = c(roc_list$roc1$auc_ci_lower, roc_list$roc2$auc_ci_lower),
        AUC_CI_Upper = c(roc_list$roc1$auc_ci_upper, roc_list$roc2$auc_ci_upper),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't'),
        class = "table-sm"
      ) %>%
        DT::formatRound(c("AUC", "AUC_CI_Lower", "AUC_CI_Upper"), 3)
    })

    output$optimal_cutpoints_table <- DT::renderDT({
      req(roc_results())
      roc_list <- roc_results()

      cutpoints_df <- data.frame(
        Test = c(roc_list$roc1$test_name, roc_list$roc2$test_name),
        Optimal_Cutpoint = c(roc_list$roc1$optimal_cutpoint, roc_list$roc2$optimal_cutpoint),
        Sensitivity = c(roc_list$roc1$optimal_sensitivity, roc_list$roc2$optimal_sensitivity),
        Specificity = c(roc_list$roc1$optimal_specificity, roc_list$roc2$optimal_specificity),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        cutpoints_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't'),
        class = "table-sm"
      ) %>%
        DT::formatRound(c("Optimal_Cutpoint", "Sensitivity", "Specificity"), 3)
    })

    output$stratified_table <- DT::renderDT({
      req(stratified_metrics())
      strat_metrics <- stratified_metrics()

      if (is.null(strat_metrics)) {
        return(DT::datatable(data.frame(Message = "No stratified data")))
      }

      DT::datatable(
        strat_metrics,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        ),
        class = "table-sm"
      ) %>%
        DT::formatRound("agreement", 1)
    })

    output$model_performance_table <- DT::renderDT({
      req(input$enable_prediction)
      req(predictive_model())
      model <- predictive_model()

      if (is.null(model$model)) {
        return(DT::datatable(data.frame(Message = "Model not available")))
      }

      perf_df <- dplyr::left_join(
        model$train_performance %>% dplyr::rename(Train = value),
        model$test_performance %>% dplyr::rename(Test = value),
        by = "metric"
      )

      DT::datatable(
        perf_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't'),
        class = "table-sm"
      ) %>%
        DT::formatRound(c("Train", "Test"), 4)
    })

    output$cv_results_table <- DT::renderDT({
      req(input$enable_prediction)
      req(predictive_model())
      model <- predictive_model()

      if (is.null(model$cv_results)) {
        return(DT::datatable(data.frame(Message = "CV results not available")))
      }

      DT::datatable(
        model$cv_results$summary,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't'),
        class = "table-sm"
      ) %>%
        DT::formatRound(c("mean", "sd"), 4)
    })

    output$high_risk_samples_table <- DT::renderDT({
      req(input$enable_prediction)
      req(discordance_risk())
      risk <- discordance_risk()

      if (is.null(risk$risk_scores)) {
        return(DT::datatable(data.frame(Message = "Risk scores not available")))
      }

      high_risk <- risk$risk_scores %>%
        dplyr::filter(predicted_prob >= 0.5) %>%
        dplyr::arrange(dplyr::desc(predicted_prob)) %>%
        dplyr::slice_head(n = 100)

      DT::datatable(
        high_risk,
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(0, "desc"))
        ),
        class = "table-sm"
      ) %>%
        DT::formatRound("predicted_prob", 3) %>%
        DT::formatStyle(
          "predicted_prob",
          background = DT::styleColorBar(c(0, 1), "#dc3545"),
          backgroundSize = "100% 90%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
    })

    output$data_count <- renderText({
      data <- stratified_data()
      sprintf("%s samples", scales::comma(nrow(data)))
    })

    output$concordance_data_table <- DT::renderDT({
      req(stratified_data())
      data <- stratified_data()

      if (nrow(data) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      # Select key columns
      display_cols <- c("test1_name", "test2_name", "test1_value", "test2_value",
                       "test1_binary", "test2_binary")
      if ("health_zone" %in% names(data)) display_cols <- c(display_cols, "health_zone")
      if ("province" %in% names(data)) display_cols <- c(display_cols, "province")
      if ("date" %in% names(data)) display_cols <- c(display_cols, "date")

      display_cols <- display_cols[display_cols %in% names(data)]

      DT::datatable(
        data %>% dplyr::select(dplyr::all_of(display_cols)),
        rownames = FALSE,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = "table-sm"
      )
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    output$download_report_pdf <- downloadHandler(
      filename = function() {
        paste0("concordance_report_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        tryCatch({
          # Create temporary Rmd file
          temp_rmd <- tempfile(fileext = ".Rmd")

          # Write report content
          writeLines(c(
            "---",
            "title: 'Concordance Analysis Report'",
            "output: pdf_document",
            "---",
            "",
            "## Summary",
            "",
            "```{r echo=FALSE}",
            "metrics <- concordance_metrics()",
            "knitr::kable(metrics$metrics)",
            "```"
          ), temp_rmd)

          # Render
          rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)

        }, error = function(e) {
          warning(paste("Error generating PDF:", e$message))
        })
      }
    )

    output$download_data_excel <- downloadHandler(
      filename = function() {
        paste0("concordance_data_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        tryCatch({
          wb <- openxlsx::createWorkbook()

          # Add sheets
          openxlsx::addWorksheet(wb, "Metrics")
          openxlsx::writeData(wb, "Metrics", concordance_metrics()$metrics)

          openxlsx::addWorksheet(wb, "Data")
          openxlsx::writeData(wb, "Data", stratified_data())

          # Save
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

        }, error = function(e) {
          warning(paste("Error generating Excel:", e$message))
        })
      }
    )

    output$download_slides_pptx <- downloadHandler(
      filename = function() {
        paste0("concordance_slides_", Sys.Date(), ".pptx")
      },
      content = function(file) {
        tryCatch({
          # Create PowerPoint presentation
          ppt <- officer::read_pptx()

          # Add title slide
          ppt <- officer::add_slide(ppt, layout = "Title Slide", master = "Office Theme")
          ppt <- officer::ph_with(ppt, value = "Concordance Analysis",
                                  location = officer::ph_location_type(type = "ctrTitle"))

          # Add metrics slide
          ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")
          ppt <- officer::ph_with(ppt, value = "Key Metrics",
                                  location = officer::ph_location_type(type = "title"))

          # Save
          print(ppt, target = file)

        }, error = function(e) {
          warning(paste("Error generating PowerPoint:", e$message))
        })
      }
    )
  })
}
