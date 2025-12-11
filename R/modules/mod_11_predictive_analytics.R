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
            "Uses molecular and serological testing data to forecast where to focus surveillance efforts.")
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
                "Serological (ELISA)" = "serological",
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

          # Alert summary
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

          # Priority lists
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

          # Emerging hotspots
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

          # Risk KPIs
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

          # Visualizations
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

          # Risk table
          card(
            card_header("Detailed Zone Risk Analysis"),
            card_body(
              DT::DTOutput(ns("zone_risk_table"))
            )
          )
        ),

        # Tab 3: Structure Risk
        nav_panel(
          title = "Structure Risk",
          icon = icon("hospital"),

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
                "Predictions based on historical positivity rates, sampling frequency, and recency of activity."),
              DT::DTOutput(ns("structure_risk_table"))
            )
          )
        ),

        # Tab 4: Demographic Risk Profile
        nav_panel(
          title = "Demographics",
          icon = icon("users"),

          # Demographic KPIs
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

          # Demographics visualizations
          layout_columns(
            col_widths = c(6, 6),
            card(
              card_header("Positivity Rate by Sex"),
              card_body_fill(
                plotly::plotlyOutput(ns("sex_risk_plot"), height = "400px")
              )
            ),
            card(
              card_header("Positivity Rate by Age Group"),
              card_body_fill(
                plotly::plotlyOutput(ns("age_risk_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Combined Demographic Risk Profile"),
            card_body_fill(
              plotly::plotlyOutput(ns("demographic_heatmap"), height = "350px")
            )
          ),

          # Recommendation box
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

        # Tab 6: Data
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

    # Calculate health zone risk
    zone_risk <- reactive({
      req(biobank_df())

      tryCatch({
        calculate_healthzone_risk(
          biobank_df = biobank_df(),
          mic_df = if ("molecular" %in% input$risk_factors) mic_df() else NULL,
          elisa_df = if ("serological" %in% input$risk_factors) {
            dplyr::bind_rows(elisa_pe_df(), elisa_vsg_df())
          } else NULL,
          ielisa_df = if ("ielisa" %in% input$risk_factors) ielisa_df() else NULL
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
          mic_df = if ("molecular" %in% input$risk_factors) mic_df() else NULL,
          elisa_df = if ("serological" %in% input$risk_factors) {
            dplyr::bind_rows(elisa_pe_df(), elisa_vsg_df())
          } else NULL,
          ielisa_df = if ("ielisa" %in% input$risk_factors) ielisa_df() else NULL
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
          mic_df = if ("molecular" %in% input$risk_factors) mic_df() else NULL,
          elisa_df = if ("serological" %in% input$risk_factors) {
            dplyr::bind_rows(elisa_pe_df(), elisa_vsg_df())
          } else NULL,
          ielisa_df = if ("ielisa" %in% input$risk_factors) ielisa_df() else NULL
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
          mic_df = if ("molecular" %in% input$risk_factors) mic_df() else NULL,
          forecast_months = input$forecast_months
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
          top_n = input$top_n
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
      # Based on data completeness
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
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = TRUE
        ),
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
          dplyr::select(HealthZone, Structure, risk_score, risk_category, prediction),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = TRUE
        ),
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

      DT::datatable(
        wl$emerging_hotspots %>%
          dplyr::select(HealthZone, Structure, risk_score, historical_positivity, days_since_last, prediction),
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = 't',
          ordering = TRUE
        ),
        class = "table-sm"
      ) %>%
        DT::formatRound(c("risk_score", "historical_positivity"), 1) %>%
        DT::formatStyle(
          "risk_score",
          background = DT::styleColorBar(c(0, 100), "#dc3545"),
          backgroundSize = "100% 90%",
          backgroundRepeat = "no-repeat",
          backgroundPosition = "center"
        )
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
        x = ~reorder(HealthZone, risk_score),
        y = ~risk_score,
        color = ~risk_category,
        colors = color_palette,
        type = "bar",
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Risk Score: %{y:.1f}<br>",
          "<extra></extra>"
        )
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
        marker = list(colors = color_palette[risk_dist$risk_category]),
        hovertemplate = paste(
          "<b>%{label}</b><br>",
          "Zones: %{value}<br>",
          "Percentage: %{percent}<br>",
          "<extra></extra>"
        )
      ) %>%
        plotly::layout(showlegend = TRUE)
    })

    output$zone_risk_table <- DT::renderDT({
      zr <- zone_risk()
      if (is.null(zr) || nrow(zr) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      display_cols <- c("HealthZone", "risk_score", "risk_category", "total_samples")
      if ("mic_any_positive" %in% names(zr)) display_cols <- c(display_cols, "mic_any_positive", "mic_positivity_rate")
      if ("elisa_positive" %in% names(zr)) display_cols <- c(display_cols, "elisa_positive", "elisa_positivity_rate")

      display_cols <- display_cols[display_cols %in% names(zr)]

      DT::datatable(
        zr %>% dplyr::select(dplyr::all_of(display_cols)),
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        ),
        class = "table-sm"
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

    # ========================================================================
    # STRUCTURE RISK TAB OUTPUTS
    # ========================================================================

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
        x = ~reorder(paste(Structure, "(", HealthZone, ")"), risk_score),
        y = ~risk_score,
        color = ~risk_category,
        colors = color_palette,
        type = "bar",
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Risk Score: %{y:.1f}<br>",
          "<extra></extra>"
        )
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

      # Get top zones and structures for heatmap
      top_zones <- sr %>%
        dplyr::group_by(HealthZone) %>%
        dplyr::summarise(max_risk = max(risk_score, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(max_risk)) %>%
        dplyr::slice_head(n = 8) %>%
        dplyr::pull(HealthZone)

      sr_filtered <- sr %>%
        dplyr::filter(HealthZone %in% top_zones) %>%
        dplyr::group_by(HealthZone) %>%
        dplyr::slice_max(order_by = risk_score, n = 5) %>%
        dplyr::ungroup()

      if (nrow(sr_filtered) < 3) {
        return(plotly::plot_ly() %>% plotly::layout(title = "Insufficient data for heatmap"))
      }

      plotly::plot_ly(
        data = sr_filtered,
        x = ~HealthZone,
        y = ~Structure,
        z = ~risk_score,
        type = "heatmap",
        colorscale = list(c(0, "#28a745"), c(0.5, "#ffc107"), c(1, "#dc3545")),
        hovertemplate = paste(
          "<b>%{y}</b> (%{x})<br>",
          "Risk Score: %{z:.1f}<br>",
          "<extra></extra>"
        )
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

      DT::datatable(
        sr %>%
          dplyr::select(HealthZone, Structure, risk_score, risk_category, prediction,
                        total_samples, dplyr::any_of(c("historical_positivity", "days_since_last"))),
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          order = list(list(2, "desc"))
        ),
        class = "table-sm"
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

    # ========================================================================
    # DEMOGRAPHIC TAB OUTPUTS
    # ========================================================================

    output$kpi_risk_sex <- renderText({
      dr <- demographic_risk()
      if (is.null(dr$sex_risk) || nrow(dr$sex_risk) == 0) return("--")
      if (!"mic_positivity" %in% names(dr$sex_risk)) return("N/A")

      dr$sex_risk %>%
        dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
        dplyr::pull(Sex) %>%
        head(1)
    })

    output$kpi_risk_age <- renderText({
      dr <- demographic_risk()
      if (is.null(dr$age_risk) || nrow(dr$age_risk) == 0) return("--")
      if (!"mic_positivity" %in% names(dr$age_risk)) return("N/A")

      dr$age_risk %>%
        dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
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
      complete <- sum(!is.na(bio$Sex) & !is.na(bio$Age), na.rm = TRUE)
      paste0(scales::comma(complete), " (", round(complete/nrow(bio)*100, 1), "%)")
    })

    output$sex_risk_plot <- plotly::renderPlotly({
      dr <- demographic_risk()
      if (is.null(dr$sex_risk) || nrow(dr$sex_risk) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No sex data available"))
      }

      plot_data <- dr$sex_risk

      if ("mic_positivity" %in% names(plot_data)) {
        plotly::plot_ly(
          data = plot_data,
          x = ~Sex,
          y = ~mic_positivity,
          type = "bar",
          marker = list(color = c("#17a2b8", "#e83e8c")),
          text = ~paste0(round(mic_positivity, 2), "%"),
          textposition = "outside",
          hovertemplate = paste(
            "<b>%{x}</b><br>",
            "Positivity: %{y:.2f}%<br>",
            "Tested: %{customdata}<br>",
            "<extra></extra>"
          ),
          customdata = ~mic_tested
        ) %>%
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Positivity Rate (%)")
          )
      } else {
        plotly::plot_ly(
          data = plot_data,
          x = ~Sex,
          y = ~total_samples,
          type = "bar",
          marker = list(color = c("#17a2b8", "#e83e8c"))
        ) %>%
          plotly::layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Total Samples")
          )
      }
    })

    output$age_risk_plot <- plotly::renderPlotly({
      dr <- demographic_risk()
      if (is.null(dr$age_risk) || nrow(dr$age_risk) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No age data available"))
      }

      plot_data <- dr$age_risk

      if ("mic_positivity" %in% names(plot_data)) {
        plotly::plot_ly(
          data = plot_data,
          x = ~age_group,
          y = ~mic_positivity,
          type = "bar",
          marker = list(color = "#fd7e14"),
          text = ~paste0(round(mic_positivity, 2), "%"),
          textposition = "outside",
          hovertemplate = paste(
            "<b>Age %{x}</b><br>",
            "Positivity: %{y:.2f}%<br>",
            "Tested: %{customdata}<br>",
            "<extra></extra>"
          ),
          customdata = ~mic_tested
        ) %>%
          plotly::layout(
            xaxis = list(title = "Age Group"),
            yaxis = list(title = "Positivity Rate (%)")
          )
      } else {
        plotly::plot_ly(
          data = plot_data,
          x = ~age_group,
          y = ~total_samples,
          type = "bar",
          marker = list(color = "#fd7e14")
        ) %>%
          plotly::layout(
            xaxis = list(title = "Age Group"),
            yaxis = list(title = "Total Samples")
          )
      }
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
        colorscale = "Blues",
        hovertemplate = paste(
          "<b>%{y}, Age %{x}</b><br>",
          "Samples: %{z}<br>",
          "<extra></extra>"
        )
      ) %>%
        plotly::layout(
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Sex")
        )
    })

    output$demographic_recommendations <- renderUI({
      dr <- demographic_risk()

      recommendations <- list()

      if (!is.null(dr$sex_risk) && "mic_positivity" %in% names(dr$sex_risk)) {
        high_risk_sex <- dr$sex_risk %>%
          dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
          dplyr::pull(Sex)

        if (length(high_risk_sex) > 0) {
          recommendations <- c(recommendations, tags$li(
            tags$strong(high_risk_sex), " individuals show higher molecular positivity rates.",
            " Consider targeted screening in this group."
          ))
        }
      }

      if (!is.null(dr$age_risk) && "mic_positivity" %in% names(dr$age_risk)) {
        high_risk_age <- dr$age_risk %>%
          dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
          dplyr::pull(age_group)

        if (length(high_risk_age) > 0) {
          recommendations <- c(recommendations, tags$li(
            "Age group ", tags$strong(high_risk_age), " shows highest positivity.",
            " Focus surveillance efforts on this demographic."
          ))
        }
      }

      if (length(recommendations) == 0) {
        recommendations <- list(tags$li(
          "Insufficient positivity data to generate demographic recommendations.",
          " Continue broad surveillance until patterns emerge."
        ))
      }

      tags$ul(class = "mb-0", recommendations)
    })

    # ========================================================================
    # TEMPORAL TAB OUTPUTS
    # ========================================================================

    output$trend_forecast_plot <- plotly::renderPlotly({
      tp <- temporal_predictions()
      if (is.null(tp$positivity_trend) || nrow(tp$positivity_trend) == 0) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No trend data available"))
      }

      p <- plotly::plot_ly() %>%
        plotly::add_lines(
          data = tp$positivity_trend,
          x = ~sample_month,
          y = ~positivity_rate,
          name = "Actual",
          line = list(color = "#007bff", width = 2),
          hovertemplate = paste(
            "<b>%{x|%b %Y}</b><br>",
            "Positivity: %{y:.2f}%<br>",
            "<extra></extra>"
          )
        )

      # Add moving average if available
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

      # Add forecast if available
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

      p %>%
        plotly::layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Positivity Rate (%)"),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$trend_summary_box <- renderUI({
      tp <- temporal_predictions()

      if (is.null(tp$trend_summary)) {
        return(div(
          class = "text-center p-4",
          icon("info-circle", class = "fa-2x text-muted mb-3"),
          p("Insufficient data for trend analysis")
        ))
      }

      trend_icon <- switch(
        tp$trend_summary$trend_direction,
        "Increasing" = icon("arrow-up", class = "text-danger"),
        "Decreasing" = icon("arrow-down", class = "text-success"),
        icon("minus", class = "text-warning")
      )

      tagList(
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
              class = paste0("badge ",
                             ifelse(tp$trend_summary$confidence == "High", "bg-success", "bg-warning")),
              tp$trend_summary$confidence
            )
        ),
        hr(),
        p(class = "text-muted small",
          "Based on ", input$forecast_months, " month forecast using historical patterns.")
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
        marker = list(color = "#17a2b8"),
        hovertemplate = paste(
          "<b>%{x|%b %Y}</b><br>",
          "Samples: %{y}<br>",
          "<extra></extra>"
        )
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
        marker = list(color = "#28a745"),
        hovertemplate = paste(
          "<b>%{x}</b><br>",
          "Avg Samples: %{y}<br>",
          "<extra></extra>"
        )
      ) %>%
        plotly::layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Average Samples")
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

          # Watchlist sheet
          wl <- watchlist()
          if (!is.null(wl$priority_zones)) {
            openxlsx::addWorksheet(wb, "Priority Zones")
            openxlsx::writeData(wb, "Priority Zones", wl$priority_zones)
          }

          if (!is.null(wl$priority_structures)) {
            openxlsx::addWorksheet(wb, "Priority Structures")
            openxlsx::writeData(wb, "Priority Structures", wl$priority_structures)
          }

          # Zone risk
          zr <- zone_risk()
          if (!is.null(zr) && nrow(zr) > 0) {
            openxlsx::addWorksheet(wb, "Zone Risk Analysis")
            openxlsx::writeData(wb, "Zone Risk Analysis", zr)
          }

          # Structure risk
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
