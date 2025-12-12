# Predictive Analytics Module
# Epidemiological forecasting and risk prediction for HAT surveillance
# ============================================================================

#' Predictive Analytics Module UI
#'
#' @param id Module namespace ID
#' @export
mod_predictive_analytics_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Predictive Analytics",
    icon = icon("chart-line"),

    page_fluid(
      # Header card
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("graph-up-arrow", size = "1.5rem"),
            "Predictive Analytics & Risk Forecasting",
            span(class = "badge bg-warning ms-2", "Surveillance Intelligence")
          )
        ),
        card_body(
          p("Predict which health zones, structures, and demographic groups are at highest risk for HAT cases based on historical patterns."),
          p(class = "text-muted mb-0 small",
            "Uses molecular (MIC qPCR) and serological (ELISA-PE, ELISA-VSG, iELISA) testing data to forecast where to focus surveillance efforts.")
        )
      ),

      # Control panel
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        card(
          card_header("Prediction Focus"),
          card_body(
            selectInput(
              ns("prediction_type"),
              "Analysis Type",
              choices = c(
                "Priority Watchlist" = "watchlist",
                "Health Zone Risk" = "zone_risk",
                "Structure Risk" = "structure_risk",
                "Demographic Profile" = "demographic",
                "Temporal Trends" = "temporal"
              ),
              selected = "watchlist"
            ),
            numericInput(
              ns("top_n"),
              "Top N Results",
              value = 15,
              min = 5,
              max = 50,
              step = 5
            )
          )
        ),
        card(
          card_header("Risk Factors"),
          card_body(
            checkboxGroupInput(
              ns("risk_factors"),
              "Include in Analysis",
              choices = c(
                "Molecular (MIC qPCR)" = "molecular",
                "Serological (ELISA-PE/VSG)" = "serological",
                "Serological (iELISA)" = "ielisa"
              ),
              selected = c("molecular", "serological", "ielisa")
            )
          )
        ),
        card(
          card_header("Time Window"),
          card_body(
            selectInput(
              ns("lookback_period"),
              "Historical Data",
              choices = c(
                "Last 3 months" = "3",
                "Last 6 months" = "6",
                "Last 12 months" = "12",
                "All time" = "all"
              ),
              selected = "all"
            ),
            numericInput(
              ns("forecast_months"),
              "Forecast Ahead (months)",
              value = 3,
              min = 1,
              max = 12
            )
          )
        ),
        card(
          card_header("Export"),
          card_body(
            downloadButton(ns("download_watchlist"), "Download Watchlist",
                          class = "btn-warning btn-sm w-100 mb-2"),
            downloadButton(ns("download_report"), "Full Report (Excel)",
                          class = "btn-success btn-sm w-100")
          )
        )
      ),

      # Main content tabs
      navset_card_tab(
        id = ns("main_tabs"),

        # Tab 1: Priority Watchlist
        nav_panel(
          title = "Watchlist",
          icon = icon("exclamation-triangle"),

          layout_column_wrap(
            width = 1/4,
            value_box(
              title = "High Risk Zones",
              value = textOutput(ns("kpi_high_risk_zones")),
              showcase = icon("map-marked-alt"),
              theme = "danger"
            ),
            value_box(
              title = "Priority Structures",
              value = textOutput(ns("kpi_priority_structures")),
              showcase = icon("hospital"),
              theme = "warning"
            ),
            value_box(
              title = "Recent Positives",
              value = textOutput(ns("kpi_recent_positives")),
              showcase = icon("virus"),
              theme = "info"
            ),
            value_box(
              title = "Prediction Confidence",
              value = textOutput(ns("kpi_confidence")),
              showcase = icon("brain"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header(
                class = "bg-danger text-white",
                icon("map-marker-alt"), " Priority Health Zones to Watch"
              ),
              card_body(
                DT::DTOutput(ns("priority_zones_table"))
              )
            ),
            card(
              card_header(
                class = "bg-warning",
                icon("hospital"), " Priority Structures to Monitor"
              ),
              card_body(
                DT::DTOutput(ns("priority_structures_table"))
              )
            )
          ),

          card(
            card_header(
              icon("fire"), " Emerging Hotspots (High Activity + Recent Positives)"
            ),
            card_body(
              DT::DTOutput(ns("emerging_hotspots_table"))
            )
          )
        ),

        # Tab 2: Health Zone Risk
        nav_panel(
          title = "Zone Risk",
          icon = icon("map"),

          layout_column_wrap(
            width = 1/5,
            value_box(
              title = "Zones Analyzed",
              value = textOutput(ns("kpi_zones_total")),
              showcase = icon("globe-africa"),
              theme = "primary"
            ),
            value_box(
              title = "Very High Risk",
              value = textOutput(ns("kpi_zones_very_high")),
              showcase = icon("exclamation-circle"),
              theme = "danger"
            ),
            value_box(
              title = "High Risk",
              value = textOutput(ns("kpi_zones_high")),
              showcase = icon("exclamation-triangle"),
              theme = "warning"
            ),
            value_box(
              title = "Medium Risk",
              value = textOutput(ns("kpi_zones_medium")),
              showcase = icon("question-circle"),
              theme = "info"
            ),
            value_box(
              title = "Low Risk",
              value = textOutput(ns("kpi_zones_low")),
              showcase = icon("check-circle"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("Health Zone Risk Ranking"),
              card_body_fill(
                plotly::plotlyOutput(ns("zone_risk_bar"), height = "500px")
              )
            ),
            card(
              card_header("Risk Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("zone_risk_pie"), height = "400px")
              )
            )
          ),

          card(
            card_header("Detailed Zone Risk Analysis (Molecular + Serological)"),
            card_body(
              DT::DTOutput(ns("zone_risk_table"))
            )
          )
        ),

        # Tab 3: Structure Risk
        nav_panel(
          title = "Structure Risk",
          icon = icon("hospital"),

          # KPIs for Structure Risk
          layout_column_wrap(
            width = 1/5,
            value_box(
              title = "Structures Analyzed",
              value = textOutput(ns("kpi_structures_total")),
              showcase = icon("hospital"),
              theme = "primary"
            ),
            value_box(
              title = "Very High Risk",
              value = textOutput(ns("kpi_structures_very_high")),
              showcase = icon("exclamation-circle"),
              theme = "danger"
            ),
            value_box(
              title = "High Risk",
              value = textOutput(ns("kpi_structures_high")),
              showcase = icon("exclamation-triangle"),
              theme = "warning"
            ),
            value_box(
              title = "With Positives",
              value = textOutput(ns("kpi_structures_with_pos")),
              showcase = icon("virus"),
              theme = "info"
            ),
            value_box(
              title = "Active (30d)",
              value = textOutput(ns("kpi_structures_active")),
              showcase = icon("clock"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Structures Most Likely to Find Positives"),
              card_body_fill(
                plotly::plotlyOutput(ns("structure_risk_bar"), height = "450px")
              )
            ),
            card(
              card_header("Structure Risk by Health Zone"),
              card_body_fill(
                plotly::plotlyOutput(ns("structure_heatmap"), height = "450px")
              )
            )
          ),

          card(
            card_header("Structure-Level Predictions"),
            card_body(
              p(class = "text-muted small mb-3",
                "Predictions based on historical positivity rates (molecular + serological), sampling frequency, and recency of activity."),
              DT::DTOutput(ns("structure_risk_table"))
            )
          )
        ),

        # Tab 4: Demographic Risk Profile
        nav_panel(
          title = "Demographics",
          icon = icon("users"),

          layout_column_wrap(
            width = 1/4,
            value_box(
              title = "Highest Risk Sex",
              value = textOutput(ns("kpi_risk_sex")),
              showcase = icon("venus-mars"),
              theme = "info"
            ),
            value_box(
              title = "Highest Risk Age Group",
              value = textOutput(ns("kpi_risk_age")),
              showcase = icon("birthday-cake"),
              theme = "warning"
            ),
            value_box(
              title = "Samples Analyzed",
              value = textOutput(ns("kpi_demo_samples")),
              showcase = icon("vials"),
              theme = "primary"
            ),
            value_box(
              title = "With Demographics",
              value = textOutput(ns("kpi_demo_complete")),
              showcase = icon("check-double"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Sample Distribution by Sex"),
              card_body_fill(
                plotly::plotlyOutput(ns("sex_risk_plot"), height = "400px")
              )
            ),
            card(
              card_header("Sample Distribution by Age Group"),
              card_body_fill(
                plotly::plotlyOutput(ns("age_risk_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Combined Demographic Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("demographic_heatmap"), height = "350px")
            )
          ),

          card(
            card_header(class = "bg-info text-white", icon("lightbulb"), " Targeting Recommendations"),
            card_body(
              uiOutput(ns("demographic_recommendations"))
            )
          )
        ),

        # Tab 5: Temporal Predictions
        nav_panel(
          title = "Trends & Forecast",
          icon = icon("chart-area"),

          layout_columns(
            col_widths = c(8, 4),
            card(
              card_header("Positivity Trend & Forecast"),
              card_body_fill(
                plotly::plotlyOutput(ns("trend_forecast_plot"), height = "450px")
              )
            ),
            card(
              card_header("Trend Summary"),
              card_body(
                uiOutput(ns("trend_summary_box"))
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Monthly Sampling Activity"),
              card_body_fill(
                plotly::plotlyOutput(ns("monthly_activity_plot"), height = "350px")
              )
            ),
            card(
              card_header("Seasonal Patterns"),
              card_body_fill(
                plotly::plotlyOutput(ns("seasonal_plot"), height = "350px")
              )
            )
          )
        ),

        # Tab 6: Geography
        nav_panel(
          title = "Geography",
          icon = icon("globe-africa"),

          layout_column_wrap(
            width = 1/4,
            value_box(
              title = "Provinces Covered",
              value = textOutput(ns("kpi_provinces")),
              showcase = icon("map"),
              theme = "primary"
            ),
            value_box(
              title = "Health Zones",
              value = textOutput(ns("kpi_health_zones")),
              showcase = icon("map-marker-alt"),
              theme = "info"
            ),
            value_box(
              title = "Structures",
              value = textOutput(ns("kpi_total_structures")),
              showcase = icon("hospital"),
              theme = "warning"
            ),
            value_box(
              title = "Geographic Coverage",
              value = textOutput(ns("kpi_coverage")),
              showcase = icon("globe"),
              theme = "success"
            )
          ),

          card(
            card_header("Geographic Distribution of Risk"),
            card_body(
              p(class = "text-muted small mb-3",
                "Risk levels by health zone - larger bubbles indicate higher sample volumes, colors indicate risk category."),
              plotly::plotlyOutput(ns("geographic_bubble_chart"), height = "500px")
            )
          ),

          card(
            card_header("Risk Summary by Province"),
            card_body(
              DT::DTOutput(ns("province_summary_table"))
            )
          )
        ),

        # Tab 7: Model Explanation
        nav_panel(
          title = "Model",
          icon = icon("info-circle"),

          card(
            card_header(class = "bg-primary text-white", icon("brain"), " Predictive Model Explanation"),
            card_body(
              h4("Overview"),
              p("This predictive analytics module uses historical laboratory data to identify health zones and structures at highest risk for HAT (Human African Trypanosomiasis) cases."),

              h4("Data Sources"),
              tags$ul(
                tags$li(tags$strong("Biobank Samples:"), " Core demographic and geographic data from collected samples"),
                tags$li(tags$strong("MIC qPCR (Molecular):"), " DNA/RNA detection using FinalCall results (Positive, Positive_DNA, Positive_RNA, LatePositive, Indeterminate)"),
                tags$li(tags$strong("ELISA-PE:"), " Serological testing with Plasmodium Extract antigen (status_final: Positive, Borderline, Negative)"),
                tags$li(tags$strong("ELISA-VSG:"), " Serological testing with Variable Surface Glycoprotein antigen"),
                tags$li(tags$strong("iELISA:"), " Inhibition ELISA using LiTat 1.3 (L13) and LiTat 1.5 (L15) antigens")
              ),

              h4("Weighted Scoring System"),
              p("Results are weighted to account for different confidence levels:"),

              h5("Molecular (MIC qPCR) Weights:"),
              tags$table(class = "table table-sm table-bordered",
                tags$thead(tags$tr(tags$th("Result"), tags$th("Weight"), tags$th("Description"))),
                tags$tbody(
                  tags$tr(tags$td("Positive"), tags$td("1.0"), tags$td("Confirmed DNA+RNA detection")),
                  tags$tr(tags$td("Positive_DNA / Positive_RNA"), tags$td("0.8"), tags$td("Single target detection")),
                  tags$tr(tags$td("LatePositive"), tags$td("0.6"), tags$td("Late cycle amplification")),
                  tags$tr(tags$td("Indeterminate"), tags$td("0.3"), tags$td("Inconclusive result"))
                )
              ),

              h5("Serological (ELISA) Weights:"),
              tags$table(class = "table table-sm table-bordered",
                tags$thead(tags$tr(tags$th("Result"), tags$th("Weight"), tags$th("Description"))),
                tags$tbody(
                  tags$tr(tags$td("Positive"), tags$td("1.0"), tags$td("Clear positive (PP% >= 20 or DOD >= 0.3)")),
                  tags$tr(tags$td("Borderline"), tags$td("0.5"), tags$td("Uncertain (PP% 15-20 or DOD 0.2-0.3)")),
                  tags$tr(tags$td("Negative"), tags$td("0.0"), tags$td("No antibodies detected"))
                )
              ),

              h4("Risk Score Calculation"),
              p("Health Zone Risk Score (0-100) is calculated as:"),
              tags$pre(class = "bg-light p-3",
                "Risk Score = (Molecular Risk × 0.35) +
             (Serological Risk × 0.30) +
             (Sample Density × 0.20) +
             (Recency Score × 0.15)"
              ),

              tags$ul(
                tags$li(tags$strong("Molecular Risk:"), " Weighted MIC positivity rate × 10 (capped at 100)"),
                tags$li(tags$strong("Serological Risk:"), " Weighted ELISA positivity rate × 10 (capped at 100)"),
                tags$li(tags$strong("Sample Density:"), " Normalized sample count (0-100 scale)"),
                tags$li(tags$strong("Recency Score:"), " 100 - (days since last sample / 2), minimum 0")
              ),

              h4("Risk Categories"),
              tags$table(class = "table table-sm table-bordered",
                tags$thead(tags$tr(tags$th("Category"), tags$th("Score Range"), tags$th("Action"))),
                tags$tbody(
                  tags$tr(class = "table-danger", tags$td("Very High"), tags$td(">= 75"), tags$td("Immediate priority surveillance")),
                  tags$tr(class = "table-warning", tags$td("High"), tags$td("50-74"), tags$td("Enhanced monitoring recommended")),
                  tags$tr(class = "table-info", tags$td("Medium"), tags$td("25-49"), tags$td("Regular surveillance")),
                  tags$tr(class = "table-success", tags$td("Low"), tags$td("< 25"), tags$td("Routine monitoring"))
                )
              ),

              h4("Structure Risk Calculation"),
              p("Structure-level risk uses historical positivity with recency adjustment:"),
              tags$ul(
                tags$li("Historical positivity > 10%: Base score 90+"),
                tags$li("Historical positivity 5-10%: Base score 70-90"),
                tags$li("Historical positivity 0-5%: Base score 40-70"),
                tags$li("Recency multiplier: 1.0 (<=30d), 0.9 (31-90d), 0.7 (91-180d), 0.5 (>180d)")
              ),

              h4("Limitations"),
              tags$ul(
                tags$li("Predictions are based on available data only - zones with no recent samples may have artificially low scores"),
                tags$li("Small sample sizes reduce prediction confidence"),
                tags$li("Model assumes historical patterns predict future occurrence"),
                tags$li("Absence of positives does not guarantee absence of disease")
              )
            )
          )
        ),

        # Tab 8: Data
        nav_panel(
          title = "Data",
          icon = icon("table"),

          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Predictive Analytics Data"),
              span(textOutput(ns("data_count"), inline = TRUE), class = "badge bg-primary")
            ),
            card_body(
              DT::DTOutput(ns("analytics_data_table"))
            )
          )
        )
      )
    )
  )
}


#' Predictive Analytics Module Server
#'
#' @param id Module namespace ID
#' @param biobank_df Reactive containing biobank data
#' @param mic_df Reactive containing MIC qPCR data
#' @param elisa_pe_df Reactive containing ELISA-PE data
#' @param elisa_vsg_df Reactive containing ELISA-VSG data
#' @param ielisa_df Reactive containing iELISA data
#' @param filters Reactive containing filter settings
#' @export
mod_predictive_analytics_server <- function(id,
                                             biobank_df = reactive(NULL),
                                             mic_df = reactive(NULL),
                                             elisa_pe_df = reactive(NULL),
                                             elisa_vsg_df = reactive(NULL),
                                             ielisa_df = reactive(NULL),
                                             filters = reactive(NULL)) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # REACTIVE DATA PROCESSING
    # ========================================================================

    # Helper to check if risk factor is selected (default to TRUE if input not initialized)
    include_molecular <- reactive({
      is.null(input$risk_factors) || "molecular" %in% input$risk_factors
    })
    include_serological <- reactive({
      is.null(input$risk_factors) || "serological" %in% input$risk_factors
    })
    include_ielisa <- reactive({
      is.null(input$risk_factors) || "ielisa" %in% input$risk_factors
    })

    # Calculate health zone risk
    zone_risk <- reactive({
      req(biobank_df())

      tryCatch({
        calculate_healthzone_risk(
          biobank_df = biobank_df(),
          mic_df = if (include_molecular()) mic_df() else NULL,
          elisa_pe_df = if (include_serological()) elisa_pe_df() else NULL,
          elisa_vsg_df = if (include_serological()) elisa_vsg_df() else NULL,
          ielisa_df = if (include_ielisa()) ielisa_df() else NULL
        )
      }, error = function(e) {
        warning(paste("Error calculating zone risk:", e$message))
        data.frame()
      })
    })

    # Calculate structure risk
    structure_risk <- reactive({
      req(biobank_df())

      tryCatch({
        calculate_structure_risk(
          biobank_df = biobank_df(),
          mic_df = if (include_molecular()) mic_df() else NULL,
          elisa_pe_df = if (include_serological()) elisa_pe_df() else NULL,
          elisa_vsg_df = if (include_serological()) elisa_vsg_df() else NULL,
          ielisa_df = if (include_ielisa()) ielisa_df() else NULL
        )
      }, error = function(e) {
        warning(paste("Error calculating structure risk:", e$message))
        data.frame()
      })
    })

    # Calculate demographic risk
    demographic_risk <- reactive({
      req(biobank_df())

      tryCatch({
        calculate_demographic_risk(
          biobank_df = biobank_df(),
          mic_df = if (include_molecular()) mic_df() else NULL,
          elisa_pe_df = if (include_serological()) elisa_pe_df() else NULL,
          elisa_vsg_df = if (include_serological()) elisa_vsg_df() else NULL,
          ielisa_df = if (include_ielisa()) ielisa_df() else NULL
        )
      }, error = function(e) {
        warning(paste("Error calculating demographic risk:", e$message))
        list()
      })
    })

    # Calculate temporal predictions
    temporal_predictions <- reactive({
      req(biobank_df())

      tryCatch({
        calculate_temporal_predictions(
          biobank_df = biobank_df(),
          mic_df = if (include_molecular()) mic_df() else NULL,
          elisa_pe_df = if (include_serological()) elisa_pe_df() else NULL,
          forecast_months = if (is.null(input$forecast_months)) 3 else input$forecast_months
        )
      }, error = function(e) {
        warning(paste("Error calculating temporal predictions:", e$message))
        list()
      })
    })

    # Generate watchlist
    watchlist <- reactive({
      req(zone_risk(), structure_risk())

      tryCatch({
        generate_watchlist(
          zone_risk = zone_risk(),
          structure_risk = structure_risk(),
          demographic_risk = demographic_risk(),
          top_n = if (is.null(input$top_n)) 15 else input$top_n
        )
      }, error = function(e) {
        warning(paste("Error generating watchlist:", e$message))
        list()
      })
    })

    # ========================================================================
    # WATCHLIST TAB OUTPUTS
    # ========================================================================

    output$kpi_high_risk_zones <- renderText({
      wl <- watchlist()
      if (is.null(wl$summary)) return("--")
      scales::comma(wl$summary$high_risk_zones)
    })

    output$kpi_priority_structures <- renderText({
      wl <- watchlist()
      if (is.null(wl$summary)) return("--")
      scales::comma(wl$summary$high_risk_structures)
    })

    output$kpi_recent_positives <- renderText({
      wl <- watchlist()
      if (is.null(wl$summary)) return("--")
      scales::comma(wl$summary$structures_with_positives)
    })

    output$kpi_confidence <- renderText({
      bio <- biobank_df()
      if (is.null(bio) || nrow(bio) == 0) return("--")

      n_samples <- nrow(bio)
      if (n_samples >= 5000) return("High")
      if (n_samples >= 1000) return("Moderate")
      return("Low")
    })

    output$priority_zones_table <- DT::renderDT({
      wl <- watchlist()
      if (is.null(wl$priority_zones) || nrow(wl$priority_zones) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      DT::datatable(
        wl$priority_zones,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', ordering = TRUE),
        class = "table-sm table-striped"
      ) %>%
        DT::formatRound("risk_score", 1) %>%
        DT::formatStyle(
          "risk_category",
          backgroundColor = DT::styleEqual(
            c("Low", "Medium", "High", "Very High"),
            c("#d4edda", "#fff3cd", "#ffe0b2", "#f8d7da")
          )
        )
    })

    output$priority_structures_table <- DT::renderDT({
      wl <- watchlist()
      if (is.null(wl$priority_structures) || nrow(wl$priority_structures) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      DT::datatable(
        wl$priority_structures %>%
          dplyr::select(health_zone, structure, risk_score, risk_category, prediction,
                        dplyr::any_of(c("mic_positive", "sero_positive"))),
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', ordering = TRUE),
        class = "table-sm table-striped"
      ) %>%
        DT::formatRound("risk_score", 1) %>%
        DT::formatStyle(
          "prediction",
          color = DT::styleEqual(
            c("Likely to find positives", "Possible positives", "Monitor closely", "Low probability"),
            c("#dc3545", "#fd7e14", "#ffc107", "#28a745")
          ),
          fontWeight = "bold"
        )
    })

    output$emerging_hotspots_table <- DT::renderDT({
      wl <- watchlist()
      if (is.null(wl$emerging_hotspots) || nrow(wl$emerging_hotspots) == 0) {
        return(DT::datatable(data.frame(Message = "No emerging hotspots identified")))
      }

      cols_to_show <- intersect(
        c("health_zone", "structure", "risk_score", "historical_positivity", "days_since_last", "prediction"),
        names(wl$emerging_hotspots)
      )

      DT::datatable(
        wl$emerging_hotspots %>% dplyr::select(dplyr::all_of(cols_to_show)),
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', ordering = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatRound(intersect(c("risk_score", "historical_positivity"), cols_to_show), 1)
    })

    # ========================================================================
    # ZONE RISK TAB OUTPUTS
    # ========================================================================

    output$kpi_zones_total <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      scales::comma(nrow(zr))
    })

    output$kpi_zones_very_high <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      sum(zr$risk_category == "Very High", na.rm = TRUE)
    })

    output$kpi_zones_high <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      sum(zr$risk_category == "High", na.rm = TRUE)
    })

    output$kpi_zones_medium <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      sum(zr$risk_category == "Medium", na.rm = TRUE)
    })

    output$kpi_zones_low <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      sum(zr$risk_category == "Low", na.rm = TRUE)
    })

    output$zone_risk_bar <- plotly::renderPlotly({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      color_palette <- c("Low" = "#28a745", "Medium" = "#ffc107",
                         "High" = "#fd7e14", "Very High" = "#dc3545")

      zr_top <- zr %>% dplyr::slice_head(n = input$top_n)

      plotly::plot_ly(
        data = zr_top,
        x = ~reorder(health_zone, risk_score),
        y = ~risk_score,
        color = ~risk_category,
        colors = color_palette,
        type = "bar",
        hovertemplate = "<b>%{x}</b><br>Risk Score: %{y:.1f}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Risk Score (0-100)"),
          showlegend = TRUE,
          legend = list(title = list(text = "Risk Level")),
          margin = list(b = 120)
        )
    })

    output$zone_risk_pie <- plotly::renderPlotly({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      color_palette <- c("Low" = "#28a745", "Medium" = "#ffc107",
                         "High" = "#fd7e14", "Very High" = "#dc3545")

      risk_dist <- zr %>%
        dplyr::count(risk_category) %>%
        dplyr::mutate(pct = n / sum(n) * 100)

      plotly::plot_ly(
        data = risk_dist,
        labels = ~risk_category,
        values = ~n,
        type = "pie",
        marker = list(colors = color_palette[risk_dist$risk_category])
      ) %>%
        plotly::layout(showlegend = TRUE)
    })

    output$zone_risk_table <- DT::renderDT({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      # Include all relevant columns including serology
      display_cols <- c("health_zone", "risk_score", "risk_category", "total_samples")
      if ("mic_tested" %in% names(zr)) display_cols <- c(display_cols, "mic_tested", "mic_positive", "mic_positivity_rate")
      if ("sero_tested" %in% names(zr)) display_cols <- c(display_cols, "sero_tested", "sero_positive", "sero_borderline", "sero_positivity_rate")

      display_cols <- display_cols[display_cols %in% names(zr)]

      DT::datatable(
        zr %>% dplyr::select(dplyr::all_of(display_cols)),
        rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE, order = list(list(1, "desc"))),
        class = "table-sm"
      ) %>%
        DT::formatRound(intersect(c("risk_score", "mic_positivity_rate", "sero_positivity_rate"), display_cols), 2) %>%
        DT::formatStyle(
          "risk_category",
          backgroundColor = DT::styleEqual(
            c("Low", "Medium", "High", "Very High"),
            c("#d4edda", "#fff3cd", "#ffe0b2", "#f8d7da")
          )
        )
    })

    # ========================================================================
    # STRUCTURE RISK TAB OUTPUTS
    # ========================================================================

    output$kpi_structures_total <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) return("--")
      scales::comma(nrow(sr))
    })

    output$kpi_structures_very_high <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) return("--")
      sum(sr$risk_category == "Very High", na.rm = TRUE)
    })

    output$kpi_structures_high <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) return("--")
      sum(sr$risk_category == "High", na.rm = TRUE)
    })

    output$kpi_structures_with_pos <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0 || !"historical_positivity" %in% names(sr)) return("--")
      sum(sr$historical_positivity > 0, na.rm = TRUE)
    })

    output$kpi_structures_active <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0 || !"days_since_last" %in% names(sr)) return("--")
      sum(sr$days_since_last <= 30, na.rm = TRUE)
    })

    output$structure_risk_bar <- plotly::renderPlotly({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      color_palette <- c("Low" = "#28a745", "Medium" = "#ffc107",
                         "High" = "#fd7e14", "Very High" = "#dc3545")

      sr_top <- sr %>%
        dplyr::filter(risk_category %in% c("High", "Very High")) %>%
        dplyr::slice_head(n = input$top_n)

      if (nrow(sr_top) == 0) {
        sr_top <- sr %>% dplyr::slice_head(n = input$top_n)
      }

      plotly::plot_ly(
        data = sr_top,
        x = ~reorder(paste(structure, "(", health_zone, ")"), risk_score),
        y = ~risk_score,
        color = ~risk_category,
        colors = color_palette,
        type = "bar"
      ) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Risk Score"),
          showlegend = TRUE,
          margin = list(b = 150)
        )
    })

    output$structure_heatmap <- plotly::renderPlotly({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      top_zones <- sr %>%
        dplyr::group_by(health_zone) %>%
        dplyr::summarise(max_risk = max(risk_score, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(max_risk)) %>%
        dplyr::slice_head(n = 8) %>%
        dplyr::pull(health_zone)

      sr_filtered <- sr %>%
        dplyr::filter(health_zone %in% top_zones) %>%
        dplyr::group_by(health_zone) %>%
        dplyr::slice_max(order_by = risk_score, n = 5) %>%
        dplyr::ungroup()

      if (nrow(sr_filtered) < 3) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Insufficient data for heatmap"))
      }

      plotly::plot_ly(
        data = sr_filtered,
        x = ~health_zone,
        y = ~structure,
        z = ~risk_score,
        type = "heatmap",
        colorscale = list(c(0, "#28a745"), c(0.5, "#ffc107"), c(1, "#dc3545"))
      ) %>%
        plotly::layout(
          xaxis = list(title = "Health Zone", tickangle = -45),
          yaxis = list(title = "Structure"),
          margin = list(b = 100, l = 150)
        )
    })

    output$structure_risk_table <- DT::renderDT({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      display_cols <- c("health_zone", "structure", "risk_score", "risk_category", "prediction", "total_samples")
      display_cols <- c(display_cols, intersect(c("mic_positive", "sero_positive", "historical_positivity", "days_since_last"), names(sr)))
      display_cols <- display_cols[display_cols %in% names(sr)]

      DT::datatable(
        sr %>% dplyr::select(dplyr::all_of(display_cols)),
        rownames = FALSE,
        filter = "top",
        options = list(pageLength = 20, scrollX = TRUE, order = list(list(2, "desc"))),
        class = "table-sm"
      ) %>%
        DT::formatRound(intersect(c("risk_score", "historical_positivity"), display_cols), 1) %>%
        DT::formatStyle(
          "prediction",
          color = DT::styleEqual(
            c("Likely to find positives", "Possible positives", "Monitor closely", "Low probability"),
            c("#dc3545", "#fd7e14", "#ffc107", "#28a745")
          ),
          fontWeight = "bold"
        )
    })

    # ========================================================================
    # DEMOGRAPHIC TAB OUTPUTS
    # ========================================================================

    output$kpi_risk_sex <- renderText({
      dr <- demographic_risk()
      if (is.null(dr$sex_risk) || nrow(dr$sex_risk) == 0) return("--")
      # Return sex with highest sample count
      dr$sex_risk %>%
        dplyr::filter(total_samples == max(total_samples, na.rm = TRUE)) %>%
        dplyr::pull(Sex) %>%
        head(1)
    })

    output$kpi_risk_age <- renderText({
      dr <- demographic_risk()
      if (is.null(dr$age_risk) || nrow(dr$age_risk) == 0) return("--")
      # Return age group with highest sample count
      dr$age_risk %>%
        dplyr::filter(total_samples == max(total_samples, na.rm = TRUE)) %>%
        dplyr::pull(age_group) %>%
        head(1)
    })

    output$kpi_demo_samples <- renderText({
      bio <- biobank_df()
      if (is.null(bio)) return("--")
      scales::comma(nrow(bio))
    })

    output$kpi_demo_complete <- renderText({
      bio <- biobank_df()
      if (is.null(bio)) return("--")

      sex_col <- if ("sex" %in% names(bio)) "sex" else if ("Sex" %in% names(bio)) "Sex" else NULL
      age_col <- if ("age" %in% names(bio)) "age" else if ("Age" %in% names(bio)) "Age" else NULL

      if (is.null(sex_col) || is.null(age_col)) return("N/A")

      complete <- sum(!is.na(bio[[sex_col]]) & !is.na(bio[[age_col]]), na.rm = TRUE)
      paste0(scales::comma(complete), " (", round(complete/nrow(bio)*100, 1), "%)")
    })

    output$sex_risk_plot <- plotly::renderPlotly({
      dr <- demographic_risk()
      if (is.null(dr$sex_risk) || nrow(dr$sex_risk) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No sex data available"))
      }

      plot_data <- dr$sex_risk

      plotly::plot_ly(
        data = plot_data,
        x = ~Sex,
        y = ~total_samples,
        type = "bar",
        marker = list(color = c("#17a2b8", "#e83e8c")),
        text = ~scales::comma(total_samples),
        textposition = "outside"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Total Samples")
        )
    })

    output$age_risk_plot <- plotly::renderPlotly({
      dr <- demographic_risk()
      if (is.null(dr$age_risk) || nrow(dr$age_risk) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No age data available"))
      }

      plotly::plot_ly(
        data = dr$age_risk,
        x = ~age_group,
        y = ~total_samples,
        type = "bar",
        marker = list(color = "#fd7e14"),
        text = ~scales::comma(total_samples),
        textposition = "outside"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Total Samples")
        )
    })

    output$demographic_heatmap <- plotly::renderPlotly({
      dr <- demographic_risk()
      if (is.null(dr$combined_risk) || nrow(dr$combined_risk) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No demographic data available"))
      }

      plotly::plot_ly(
        data = dr$combined_risk,
        x = ~age_group,
        y = ~Sex,
        z = ~total_samples,
        type = "heatmap",
        colorscale = "Blues"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Sex")
        )
    })

    output$demographic_recommendations <- renderUI({
      dr <- demographic_risk()

      recommendations <- list()

      if (!is.null(dr$sex_risk) && nrow(dr$sex_risk) > 0) {
        high_sample_sex <- dr$sex_risk %>%
          dplyr::filter(total_samples == max(total_samples, na.rm = TRUE)) %>%
          dplyr::pull(Sex)

        if (length(high_sample_sex) > 0) {
          recommendations <- c(recommendations, tags$li(
            tags$strong(high_sample_sex), " individuals represent the largest sample group.",
            " Ensure balanced surveillance across both sexes."
          ))
        }
      }

      if (!is.null(dr$age_risk) && nrow(dr$age_risk) > 0) {
        high_sample_age <- dr$age_risk %>%
          dplyr::filter(total_samples == max(total_samples, na.rm = TRUE)) %>%
          dplyr::pull(age_group)

        if (length(high_sample_age) > 0) {
          recommendations <- c(recommendations, tags$li(
            "Age group ", tags$strong(high_sample_age), " has the most samples.",
            " Consider if this reflects population at risk or sampling bias."
          ))
        }
      }

      if (length(recommendations) == 0) {
        recommendations <- list(tags$li(
          "No demographic data available to generate recommendations.",
          " Ensure biobank data includes sex and age information."
        ))
      }

      tags$ul(class = "mb-0", recommendations)
    })

    # ========================================================================
    # TEMPORAL TAB OUTPUTS
    # ========================================================================

    output$trend_forecast_plot <- plotly::renderPlotly({
      tp <- temporal_predictions()

      # Try positivity trend first
      if (!is.null(tp$positivity_trend) && nrow(tp$positivity_trend) > 0) {
        p <- plotly::plot_ly() %>%
          plotly::add_lines(
            data = tp$positivity_trend,
            x = ~sample_month,
            y = ~positivity_rate,
            name = "Actual",
            line = list(color = "#007bff", width = 2)
          )

        if ("positivity_ma" %in% names(tp$positivity_trend)) {
          p <- p %>%
            plotly::add_lines(
              data = tp$positivity_trend %>% dplyr::filter(!is.na(positivity_ma)),
              x = ~sample_month,
              y = ~positivity_ma,
              name = "3-Month MA",
              line = list(color = "#6c757d", width = 2, dash = "dash")
            )
        }

        if (!is.null(tp$forecast) && nrow(tp$forecast) > 0) {
          p <- p %>%
            plotly::add_lines(
              data = tp$forecast,
              x = ~sample_month,
              y = ~predicted_positivity,
              name = "Forecast",
              line = list(color = "#dc3545", width = 2, dash = "dot")
            )
        }

        return(p %>%
          plotly::layout(
            xaxis = list(title = "Month"),
            yaxis = list(title = "Positivity Rate (%)"),
            showlegend = TRUE,
            legend = list(orientation = "h", y = -0.2)
          ))
      }

      # Fall back to monthly trend (sample counts)
      if (!is.null(tp$monthly_trend) && nrow(tp$monthly_trend) > 0) {
        return(plotly::plot_ly(
          data = tp$monthly_trend,
          x = ~sample_month,
          y = ~total_samples,
          type = "scatter",
          mode = "lines+markers",
          name = "Samples"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Month"),
            yaxis = list(title = "Samples Collected"),
            title = "Monthly Sampling Activity (No positivity data available)"
          ))
      }

      plotly::plot_ly() %>% plotly::layout(title = "No trend data available")
    })

    output$trend_summary_box <- renderUI({
      tp <- temporal_predictions()

      if (!is.null(tp$trend_summary)) {
        trend_icon <- switch(
          tp$trend_summary$trend_direction,
          "Increasing" = icon("arrow-up", class = "text-danger"),
          "Decreasing" = icon("arrow-down", class = "text-success"),
          icon("minus", class = "text-warning")
        )

        return(tagList(
          div(class = "mb-3",
              h6("Recent Average Positivity"),
              h3(paste0(tp$trend_summary$recent_avg_positivity, "%"))
          ),
          div(class = "mb-3",
              h6("Trend Direction"),
              h3(trend_icon, " ", tp$trend_summary$trend_direction)
          ),
          div(class = "mb-3",
              h6("Forecast Confidence"),
              span(
                class = paste0("badge ", ifelse(tp$trend_summary$confidence == "High", "bg-success", "bg-warning")),
                tp$trend_summary$confidence
              )
          )
        ))
      }

      # Show monthly trend summary if available
      if (!is.null(tp$monthly_trend) && nrow(tp$monthly_trend) > 0) {
        total <- sum(tp$monthly_trend$total_samples, na.rm = TRUE)
        months <- nrow(tp$monthly_trend)

        return(tagList(
          div(class = "mb-3",
              h6("Total Samples"),
              h3(scales::comma(total))
          ),
          div(class = "mb-3",
              h6("Months of Data"),
              h3(months)
          ),
          div(class = "mb-3",
              h6("Avg Samples/Month"),
              h3(scales::comma(round(total / months)))
          ),
          hr(),
          p(class = "text-muted small", "Positivity trend requires MIC or ELISA data with results.")
        ))
      }

      div(
        class = "text-center p-4",
        icon("info-circle", class = "fa-2x text-muted mb-3"),
        p("Insufficient data for trend analysis")
      )
    })

    output$monthly_activity_plot <- plotly::renderPlotly({
      tp <- temporal_predictions()
      if (is.null(tp$monthly_trend) || nrow(tp$monthly_trend) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No activity data available"))
      }

      plotly::plot_ly(
        data = tp$monthly_trend,
        x = ~sample_month,
        y = ~total_samples,
        type = "bar",
        marker = list(color = "#17a2b8")
      ) %>%
        plotly::layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Samples Collected")
        )
    })

    output$seasonal_plot <- plotly::renderPlotly({
      tp <- temporal_predictions()
      if (is.null(tp$seasonal_pattern) || nrow(tp$seasonal_pattern) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No seasonal data available"))
      }

      plotly::plot_ly(
        data = tp$seasonal_pattern,
        x = ~month_of_year,
        y = ~avg_samples,
        type = "bar",
        marker = list(color = "#28a745")
      ) %>%
        plotly::layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Total Samples")
        )
    })

    # ========================================================================
    # GEOGRAPHY TAB OUTPUTS
    # ========================================================================

    output$kpi_provinces <- renderText({
      bio <- biobank_df()
      if (is.null(bio)) return("--")
      province_col <- if ("province" %in% names(bio)) "province" else if ("Province" %in% names(bio)) "Province" else NULL
      if (is.null(province_col)) return("N/A")
      dplyr::n_distinct(bio[[province_col]], na.rm = TRUE)
    })

    output$kpi_health_zones <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      nrow(zr)
    })

    output$kpi_total_structures <- renderText({
      sr <- structure_risk()
      if (is.null(sr) || nrow(sr) == 0) return("--")
      nrow(sr)
    })

    output$kpi_coverage <- renderText({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) return("--")
      high_coverage <- sum(zr$total_samples >= 50, na.rm = TRUE)
      paste0(round(high_coverage / nrow(zr) * 100, 0), "%")
    })

    output$geographic_bubble_chart <- plotly::renderPlotly({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No geographic data available"))
      }

      color_palette <- c("Low" = "#28a745", "Medium" = "#ffc107",
                         "High" = "#fd7e14", "Very High" = "#dc3545")

      plotly::plot_ly(
        data = zr,
        x = ~reorder(health_zone, -risk_score),
        y = ~risk_score,
        size = ~total_samples,
        color = ~risk_category,
        colors = color_palette,
        type = "scatter",
        mode = "markers",
        marker = list(sizemode = "diameter", sizeref = max(zr$total_samples) / 50),
        text = ~paste0(health_zone, "\nRisk: ", round(risk_score, 1),
                       "\nSamples: ", scales::comma(total_samples)),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Health Zone", tickangle = -45),
          yaxis = list(title = "Risk Score"),
          showlegend = TRUE,
          margin = list(b = 150)
        )
    })

    output$province_summary_table <- DT::renderDT({
      bio <- biobank_df()
      zr <- zone_risk()

      if (is.null(bio) || is.null(zr) || nrow(zr) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      province_col <- if ("province" %in% names(bio)) "province" else if ("Province" %in% names(bio)) "Province" else NULL

      if (is.null(province_col)) {
        # Show zone summary instead
        return(DT::datatable(
          zr %>%
            dplyr::group_by(risk_category) %>%
            dplyr::summarise(
              zones = dplyr::n(),
              total_samples = sum(total_samples, na.rm = TRUE),
              avg_risk_score = round(mean(risk_score, na.rm = TRUE), 1),
              .groups = "drop"
            ),
          rownames = FALSE,
          options = list(dom = 't')
        ))
      }

      # Province-level summary
      bio <- bio %>% dplyr::rename(province = !!rlang::sym(province_col))

      province_zones <- bio %>%
        dplyr::filter(!is.na(province), !is.na(health_zone)) %>%
        dplyr::distinct(province, health_zone)

      province_summary <- province_zones %>%
        dplyr::left_join(zr %>% dplyr::select(health_zone, risk_score, risk_category), by = "health_zone") %>%
        dplyr::group_by(province) %>%
        dplyr::summarise(
          health_zones = dplyr::n(),
          high_risk_zones = sum(risk_category %in% c("High", "Very High"), na.rm = TRUE),
          avg_risk_score = round(mean(risk_score, na.rm = TRUE), 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(avg_risk_score))

      DT::datatable(
        province_summary,
        rownames = FALSE,
        options = list(pageLength = 15, dom = 't')
      ) %>%
        DT::formatStyle(
          "high_risk_zones",
          backgroundColor = DT::styleInterval(c(1, 3), c("#d4edda", "#fff3cd", "#f8d7da"))
        )
    })

    # ========================================================================
    # DATA TAB OUTPUTS
    # ========================================================================

    output$data_count <- renderText({
      bio <- biobank_df()
      if (is.null(bio)) return("0 samples")
      sprintf("%s samples", scales::comma(nrow(bio)))
    })

    output$analytics_data_table <- DT::renderDT({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      DT::datatable(
        zr,
        rownames = FALSE,
        filter = "top",
        extensions = "Buttons",
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "table-sm"
      )
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    output$download_watchlist <- downloadHandler(
      filename = function() {
        paste0("HAT_watchlist_", Sys.Date(), ".csv")
      },
      content = function(file) {
        wl <- watchlist()
        if (!is.null(wl$priority_zones)) {
          write.csv(wl$priority_zones, file, row.names = FALSE)
        }
      }
    )

    output$download_report <- downloadHandler(
      filename = function() {
        paste0("predictive_analytics_report_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        tryCatch({
          wb <- openxlsx::createWorkbook()

          wl <- watchlist()
          if (!is.null(wl$priority_zones) && nrow(wl$priority_zones) > 0) {
            openxlsx::addWorksheet(wb, "Priority Zones")
            openxlsx::writeData(wb, "Priority Zones", wl$priority_zones)
          }

          if (!is.null(wl$priority_structures) && nrow(wl$priority_structures) > 0) {
            openxlsx::addWorksheet(wb, "Priority Structures")
            openxlsx::writeData(wb, "Priority Structures", wl$priority_structures)
          }

          zr <- zone_risk()
          if (!is.null(zr) && nrow(zr) > 0) {
            openxlsx::addWorksheet(wb, "Zone Risk Analysis")
            openxlsx::writeData(wb, "Zone Risk Analysis", zr)
          }

          sr <- structure_risk()
          if (!is.null(sr) && nrow(sr) > 0) {
            openxlsx::addWorksheet(wb, "Structure Risk Analysis")
            openxlsx::writeData(wb, "Structure Risk Analysis", sr)
          }

          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

        }, error = function(e) {
          warning(paste("Error generating Excel:", e$message))
        })
      }
    )
  })
}
