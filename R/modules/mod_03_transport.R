# R/modules/mod_03_transport.R
# Transport & Processing Command Center - Redesigned Transport Module
# ============================================================================
# A comprehensive transport and lab processing monitoring dashboard with:
# - Hero KPI section with trends
# - Sankey flow diagram for pipeline visualization
# - Gauge charts with SLA targets
# - Lab processing time indicators (MIC, ELISA-PE, ELISA-VSG, iELISA)
# - Tabbed attention dashboard
# - Week-over-week comparisons
# - Enhanced temperature monitoring
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Transport Module UI
#' @param id Module namespace ID
#' @export
mod_transport_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Transport & Processing",
    icon = icon("truck"),

    div(class = "container-fluid transport-panel",

        # Custom CSS for transport module
        tags$style(HTML("
          .transport-panel {
            max-height: calc(100vh - 120px);
            overflow-y: auto;
            padding-bottom: 2rem;
          }

          .hero-kpi-card {
            border-radius: 12px;
            border: none;
            box-shadow: 0 2px 8px rgba(0,0,0,0.08);
            transition: transform 0.2s, box-shadow 0.2s;
          }

          .hero-kpi-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 16px rgba(0,0,0,0.12);
          }

          .kpi-value {
            font-size: 2.5rem;
            font-weight: 700;
            line-height: 1.2;
          }

          .kpi-label {
            font-size: 0.85rem;
            color: #6c757d;
            text-transform: uppercase;
            letter-spacing: 0.5px;
          }

          .kpi-trend {
            font-size: 0.9rem;
            font-weight: 500;
          }

          .kpi-trend.positive {
            color: #10B981;
          }

          .kpi-trend.negative {
            color: #EF4444;
          }

          .kpi-trend.neutral {
            color: #6c757d;
          }

          .attention-badge {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
          }

          .attention-badge.warning {
            background-color: #FEF3C7;
            color: #D97706;
          }

          .attention-badge.danger {
            background-color: #FEE2E2;
            color: #DC2626;
          }

          .attention-badge.success {
            background-color: #D1FAE5;
            color: #059669;
          }

          .section-title {
            font-size: 1.1rem;
            font-weight: 600;
            color: #1F2937;
            margin-bottom: 1rem;
            padding-bottom: 0.5rem;
            border-bottom: 2px solid #E5E7EB;
          }

          .gauge-container {
            text-align: center;
          }

          .gauge-label {
            font-size: 0.9rem;
            color: #4B5563;
            margin-top: 0.5rem;
          }

          .transport-card {
            border-radius: 12px;
            border: 1px solid #E5E7EB;
            margin-bottom: 1rem;
          }

          .transport-card .card-header {
            background: linear-gradient(135deg, #F9FAFB 0%, #F3F4F6 100%);
            border-bottom: 1px solid #E5E7EB;
            font-weight: 600;
          }

          .processing-metric {
            text-align: center;
            padding: 1rem;
            border-radius: 8px;
            background: linear-gradient(135deg, #F8FAFC 0%, #F1F5F9 100%);
            transition: transform 0.2s;
          }

          .processing-metric:hover {
            transform: translateY(-2px);
          }

          .processing-metric .metric-value {
            font-size: 1.75rem;
            font-weight: 700;
            line-height: 1.2;
          }

          .processing-metric .metric-label {
            font-size: 0.8rem;
            color: #64748B;
            margin-top: 0.25rem;
          }

          .processing-metric .metric-samples {
            font-size: 0.75rem;
            color: #94A3B8;
            margin-top: 0.5rem;
          }

          .processing-metric.mic { border-left: 4px solid #8B5CF6; }
          .processing-metric.elisa-pe { border-left: 4px solid #EC4899; }
          .processing-metric.elisa-vsg { border-left: 4px solid #F97316; }
          .processing-metric.ielisa { border-left: 4px solid #06B6D4; }
        ")),

        # ==== HERO KPI SECTION ================================================
        h5(class = "section-title", icon("gauge-high"), " Transport Command Center"),

        layout_columns(
          col_widths = c(4, 4, 4), gap = "16px",

          # Card 1: Overall Performance
          card(
            class = "hero-kpi-card",
            card_body(
              div(style = "text-align: center; padding: 1rem;",
                  div(class = "kpi-label", "Overall Performance"),
                  div(class = "kpi-value", style = "color: #4F46E5;",
                      textOutput(ns("hero_total_days"), inline = TRUE)),
                  div(style = "font-size: 0.9rem; color: #6c757d;", "Median Total Transport"),
                  hr(style = "margin: 1rem 0;"),
                  layout_columns(
                    col_widths = c(6, 6),
                    div(style = "text-align: center;",
                        div(style = "font-size: 1.5rem; font-weight: 600; color: #3B82F6;",
                            textOutput(ns("hero_in_transit"), inline = TRUE)),
                        div(style = "font-size: 0.75rem; color: #6c757d;", "In Transit")
                    ),
                    div(style = "text-align: center;",
                        div(style = "font-size: 1.5rem; font-weight: 600; color: #10B981;",
                            textOutput(ns("hero_completion_rate"), inline = TRUE)),
                        div(style = "font-size: 0.75rem; color: #6c757d;", "Completion Rate")
                    )
                  )
              )
            )
          ),

          # Card 2: SLA Compliance
          card(
            class = "hero-kpi-card",
            card_body(
              div(style = "text-align: center; padding: 1rem;",
                  div(class = "kpi-label", "SLA Compliance"),
                  div(class = "kpi-value", style = "color: #10B981;",
                      textOutput(ns("hero_ontime_rate"), inline = TRUE)),
                  div(style = "font-size: 0.9rem; color: #6c757d;", "On-Time Delivery"),
                  hr(style = "margin: 1rem 0;"),
                  layout_columns(
                    col_widths = c(6, 6),
                    div(style = "text-align: center;",
                        div(style = "font-size: 1.5rem; font-weight: 600;",
                            textOutput(ns("hero_temp_compliance"), inline = TRUE)),
                        div(style = "font-size: 0.75rem; color: #6c757d;", "Temp Compliance")
                    ),
                    div(style = "text-align: center;",
                        div(class = "kpi-trend",
                            textOutput(ns("hero_wow_change"), inline = TRUE)),
                        div(style = "font-size: 0.75rem; color: #6c757d;", "vs Last Week")
                    )
                  )
              )
            )
          ),

          # Card 3: Attention Required
          card(
            class = "hero-kpi-card",
            card_body(
              div(style = "text-align: center; padding: 1rem;",
                  div(class = "kpi-label", "Attention Required"),
                  div(style = "margin: 1rem 0;",
                       uiOutput(ns("hero_attention_badges"))
                  ),
                  hr(style = "margin: 1rem 0;"),
                  div(style = "font-size: 0.85rem; color: #6c757d;",
                      icon("clock"), " Last updated: ",
                      textOutput(ns("hero_last_update"), inline = TRUE)
                  )
              )
            )
          )
        ),

        # ==== SANKEY FLOW DIAGRAM =============================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("route"), " Sample Flow Pipeline"),
            card(
              class = "transport-card",
              card_body_fill(
                plotly::plotlyOutput(ns("sankey_flow"), height = "350px")
              )
            )
        ),

        # ==== GAUGE CHARTS + TIMELINE =========================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("bullseye"), " Transport Leg Performance vs Targets"),
            layout_columns(
              col_widths = c(8, 4), gap = "16px",

              # Gauge charts row
              card(
                class = "transport-card",
                card_body(
                  layout_columns(
                    col_widths = c(4, 4, 4),
                    div(class = "gauge-container",
                        plotly::plotlyOutput(ns("gauge_field_cpltha"), height = "200px"),
                        div(class = "gauge-label", "Field to CPLTHA")
                    ),
                    div(class = "gauge-container",
                        plotly::plotlyOutput(ns("gauge_cpltha_receipt"), height = "200px"),
                        div(class = "gauge-label", "CPLTHA Processing")
                    ),
                    div(class = "gauge-container",
                        plotly::plotlyOutput(ns("gauge_cpltha_inrb"), height = "200px"),
                        div(class = "gauge-label", "CPLTHA to INRB")
                    )
                  )
                )
              ),

              # Week-over-week comparison
              card(
                class = "transport-card",
                card_header(
                  class = "d-flex align-items-center",
                  icon("calendar-week"), span(style = "margin-left: 8px;", "Week Comparison")
                ),
                card_body(
                  uiOutput(ns("wow_comparison"))
                )
              )
            )
        ),

        # ==== LAB PROCESSING TIMES =============================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("flask"), " Lab Processing Times"),
            p(class = "text-muted small", "Time from sample reception at CPLTHA to test completion"),
            layout_columns(
              col_widths = c(3, 3, 3, 3), gap = "16px",

              # MIC qPCR
              div(class = "processing-metric mic",
                  div(class = "metric-value", style = "color: #8B5CF6;",
                      textOutput(ns("proc_mic_days"), inline = TRUE)),
                  div(class = "metric-label", "MIC qPCR"),
                  div(class = "metric-samples",
                      textOutput(ns("proc_mic_samples"), inline = TRUE))
              ),

              # ELISA-PE
              div(class = "processing-metric elisa-pe",
                  div(class = "metric-value", style = "color: #EC4899;",
                      textOutput(ns("proc_elisa_pe_days"), inline = TRUE)),
                  div(class = "metric-label", "ELISA-PE"),
                  div(class = "metric-samples",
                      textOutput(ns("proc_elisa_pe_samples"), inline = TRUE))
              ),

              # ELISA-VSG
              div(class = "processing-metric elisa-vsg",
                  div(class = "metric-value", style = "color: #F97316;",
                      textOutput(ns("proc_elisa_vsg_days"), inline = TRUE)),
                  div(class = "metric-label", "ELISA-VSG"),
                  div(class = "metric-samples",
                      textOutput(ns("proc_elisa_vsg_samples"), inline = TRUE))
              ),

              # iELISA
              div(class = "processing-metric ielisa",
                  div(class = "metric-value", style = "color: #06B6D4;",
                      textOutput(ns("proc_ielisa_days"), inline = TRUE)),
                  div(class = "metric-label", "iELISA"),
                  div(class = "metric-samples",
                      textOutput(ns("proc_ielisa_samples"), inline = TRUE))
              )
            ),

            # Processing timeline chart
            card(
              class = "transport-card",
              style = "margin-top: 1rem;",
              card_header("Processing Time Distribution by Test Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("processing_boxplot"), height = "280px")
              )
            )
        ),

        # ==== TEMPERATURE MONITORING ==========================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("temperature-half"), " Temperature Monitoring"),
            layout_columns(
              col_widths = c(8, 4), gap = "16px",
              card(
                class = "transport-card",
                card_header("Temperature Timeline with Compliance Band"),
                card_body_fill(
                  plotly::plotlyOutput(ns("temp_timeline"), height = "280px")
                )
              ),
              card(
                class = "transport-card",
                card_header("Temperature Distribution"),
                card_body_fill(
                  plotly::plotlyOutput(ns("temp_distribution"), height = "280px")
                )
              )
            )
        ),

        # ==== TRANSPORT TRENDS ================================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("chart-line"), " Transport Trends"),
            layout_columns(
              col_widths = c(6, 6), gap = "16px",
              card(
                class = "transport-card",
                card_header("Transport Duration by Week"),
                card_body_fill(
                  plotly::plotlyOutput(ns("transport_timeline_plot"), height = "320px")
                )
              ),
              card(
                class = "transport-card",
                card_header("Transport Duration by Province"),
                card_body_fill(
                  plotly::plotlyOutput(ns("transport_province_plot"), height = "320px")
                )
              )
            )
        ),

        # ==== ATTENTION DASHBOARD (TABBED) ====================================
        div(style = "margin-top: 1.5rem;",
            h5(class = "section-title", icon("exclamation-triangle"), " Samples Requiring Attention"),
            card(
              class = "transport-card",
              navset_card_tab(
                nav_panel(
                  title = span(icon("hourglass-half"), " In Transit"),
                  div(style = "padding: 1rem;",
                      div(class = "d-flex justify-content-between align-items-center mb-3",
                          span(class = "text-muted", "Samples currently moving through the pipeline"),
                          downloadButton(ns("download_in_transit"), "Export CSV", class = "btn-sm btn-outline-primary")
                      ),
                      DT::DTOutput(ns("table_in_transit"))
                  )
                ),
                nav_panel(
                  title = span(icon("clock"), " Delayed"),
                  div(style = "padding: 1rem;",
                      div(class = "d-flex justify-content-between align-items-center mb-3",
                          span(class = "text-muted", sprintf("Shipments exceeding %d days", config$qc$max_transport_days)),
                          downloadButton(ns("download_delayed"), "Export CSV", class = "btn-sm btn-outline-primary")
                      ),
                      DT::DTOutput(ns("table_delayed"))
                  )
                ),
                nav_panel(
                  title = span(icon("thermometer-empty"), " Temp Alerts"),
                  div(style = "padding: 1rem;",
                      div(class = "d-flex justify-content-between align-items-center mb-3",
                          span(class = "text-muted", "Samples with temperature outside 2-8°C range"),
                          downloadButton(ns("download_temp_alerts"), "Export CSV", class = "btn-sm btn-outline-primary")
                      ),
                      DT::DTOutput(ns("table_temp_alerts"))
                  )
                ),
                nav_panel(
                  title = span(icon("question-circle"), " Missing Data"),
                  div(style = "padding: 1rem;",
                      div(class = "d-flex justify-content-between align-items-center mb-3",
                          span(class = "text-muted", "Records with incomplete transport information"),
                          downloadButton(ns("download_missing"), "Export CSV", class = "btn-sm btn-outline-primary")
                      ),
                      DT::DTOutput(ns("table_missing_dates"))
                  )
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

#' Transport Module Server
#' @param id Module namespace ID
#' @param filtered_data Reactive expression containing filtered biobank data
#' @param mic_data Reactive expression containing MIC qPCR data (optional)
#' @param elisa_data Reactive expression containing ELISA PE/VSG data (optional)
#' @param ielisa_data Reactive expression containing iELISA data (optional)
#' @export
mod_transport_server <- function(id, filtered_data, mic_data = NULL, elisa_data = NULL, ielisa_data = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # HELPER FUNCTIONS
    # ========================================================================

    safe_median <- function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(NA_real_)
      stats::median(x, na.rm = TRUE)
    }

    safe_mean <- function(x) {
      x <- x[is.finite(x)]
      if (length(x) == 0) return(NA_real_)
      base::mean(x, na.rm = TRUE)
    }

    fmt_days <- function(x) {
      if (is.na(x) || !is.finite(x)) return("N/A")
      paste0(scales::number(x, accuracy = 0.1), "d")
    }

    fmt_percent <- function(x) {
      if (is.na(x) || !is.finite(x)) return("N/A")
      scales::percent(x / 100, accuracy = 0.1)
    }

    fmt_number <- function(x) {
      if (is.na(x) || !is.finite(x)) return("0")
      scales::comma(x, accuracy = 1)
    }

    # ========================================================================
    # REACTIVE DATA CALCULATIONS
    # ========================================================================

    # Core transport metrics
    transport_metrics <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(NULL)

      tibble::tibble(
        median_field_to_cpltha = safe_median(data$transport_field_to_cpltha),
        median_cpltha_receipt = safe_median(data$transport_received_at_cpltha),
        median_cpltha_to_inrb = safe_median(data$transport_cpltha_to_inrb),
        median_total_transport = safe_median(data$total_transport_days),
        pct_shipped_cpltha = safe_mean(as.numeric(data$shipped_to_cpltha) * 100),
        pct_received_cpltha = safe_mean(as.numeric(data$received_at_cpltha) * 100),
        pct_shipped_inrb = safe_mean(as.numeric(data$shipped_to_inrb) * 100),
        avg_transport_temp = safe_mean(data$transport_temperature),
        total_samples = nrow(data)
      )
    })

    # Week-over-week comparison data
    wow_metrics <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data) || !"date_sample" %in% names(data)) return(NULL)

      data <- data %>% dplyr::filter(!is.na(date_sample))
      if (nrow(data) == 0) return(NULL)

      # Get current week and last week
      today <- Sys.Date()
      current_week_start <- lubridate::floor_date(today, "week")
      last_week_start <- current_week_start - lubridate::weeks(1)

      this_week <- data %>%
        dplyr::filter(date_sample >= current_week_start)

      last_week <- data %>%
        dplyr::filter(date_sample >= last_week_start, date_sample < current_week_start)

      list(
        this_week_samples = nrow(this_week),
        last_week_samples = nrow(last_week),
        this_week_median = safe_median(this_week$total_transport_days),
        last_week_median = safe_median(last_week$total_transport_days),
        this_week_shipped = sum(this_week$shipped_to_cpltha, na.rm = TRUE),
        last_week_shipped = sum(last_week$shipped_to_cpltha, na.rm = TRUE)
      )
    })

    # Sankey flow data
    sankey_data <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(NULL)

      total <- nrow(data)
      shipped_cpltha <- sum(data$shipped_to_cpltha, na.rm = TRUE)
      not_shipped_cpltha <- total - shipped_cpltha
      received_cpltha <- sum(data$received_at_cpltha, na.rm = TRUE)
      in_transit_cpltha <- shipped_cpltha - received_cpltha
      shipped_inrb <- sum(data$shipped_to_inrb, na.rm = TRUE)
      pending_inrb <- received_cpltha - shipped_inrb

      list(
        total = total,
        shipped_cpltha = shipped_cpltha,
        not_shipped_cpltha = max(0, not_shipped_cpltha),
        received_cpltha = received_cpltha,
        in_transit_cpltha = max(0, in_transit_cpltha),
        shipped_inrb = shipped_inrb,
        pending_inrb = max(0, pending_inrb)
      )
    })

    # Attention counts
    attention_counts <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(list(delayed = 0, missing = 0, temp_alerts = 0, in_transit = 0))

      delayed <- sum(data$transport_field_to_cpltha > config$qc$max_transport_days, na.rm = TRUE)
      missing <- sum(is.na(data$date_sent_cpltha) | is.na(data$date_received_cpltha) | is.na(data$date_sent_inrb))

      # Temperature alerts (outside 2-8°C)
      temp_alerts <- sum(data$transport_temperature < 2 | data$transport_temperature > 8, na.rm = TRUE)

      # In transit (shipped but not received)
      in_transit <- sum(data$shipped_to_cpltha & !data$received_at_cpltha, na.rm = TRUE)

      list(
        delayed = delayed,
        missing = missing,
        temp_alerts = temp_alerts,
        in_transit = in_transit
      )
    })

    # Temperature timeline data
    temp_timeline_data <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data) || !"date_sample" %in% names(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(!is.na(date_sample), !is.na(transport_temperature)) %>%
        dplyr::mutate(sample_week = lubridate::floor_date(date_sample, "week")) %>%
        dplyr::group_by(sample_week) %>%
        dplyr::summarise(
          avg_temp = mean(transport_temperature, na.rm = TRUE),
          min_temp = min(transport_temperature, na.rm = TRUE),
          max_temp = max(transport_temperature, na.rm = TRUE),
          samples = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(sample_week)
    })

    # Transport timeline data
    transport_timeline <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(!is.na(date_sample)) %>%
        dplyr::mutate(sample_week = lubridate::floor_date(date_sample, "week")) %>%
        dplyr::filter(!is.na(sample_week)) %>%
        dplyr::group_by(sample_week) %>%
        dplyr::summarise(
          field_to_cpltha = safe_median(transport_field_to_cpltha),
          cpltha_to_inrb = safe_median(transport_cpltha_to_inrb),
          total_transport = safe_median(total_transport_days),
          samples = dplyr::n(),
          .groups = "drop"
        )
    })

    # ========================================================================
    # LAB PROCESSING TIME CALCULATIONS
    # ========================================================================

    # Helper to normalize sample identifiers for matching
    normalize_sample_id <- function(x) {
      if (is.null(x)) return(NA_character_)
      x %>%
        as.character() %>%
        stringr::str_trim() %>%
        stringr::str_to_upper() %>%
        stringr::str_remove_all("^KPS[_-]?") %>%
        stringr::str_remove_all("[^A-Z0-9]")
    }

    # Calculate processing times for MIC data
    mic_processing_times <- reactive({
      tryCatch({
        biobank <- filtered_data()
        mic <- if (!is.null(mic_data)) mic_data() else NULL

        if (is.null(biobank) || !nrow(biobank)) return(NULL)
        if (is.null(mic) || !is.list(mic)) return(NULL)

        # MIC data has samples in $samples
        mic_samples <- mic$samples
        if (is.null(mic_samples) || !nrow(mic_samples)) return(NULL)

        # Check for required columns
        if (!"RunDate" %in% names(mic_samples) && !"RunDateTime" %in% names(mic_samples)) return(NULL)

        # Get run date
        mic_samples <- mic_samples %>%
          dplyr::mutate(
            test_date = if ("RunDate" %in% names(.)) {
              suppressWarnings(as.Date(RunDate))
            } else if ("RunDateTime" %in% names(.)) {
              suppressWarnings(as.Date(RunDateTime))
            } else {
              NA
            }
          )

        # Normalize sample IDs for matching
        mic_samples <- mic_samples %>%
          dplyr::mutate(
            norm_id = normalize_sample_id(if ("SampleName" %in% names(.)) SampleName else SampleID)
          )

        biobank <- biobank %>%
          dplyr::mutate(
            norm_barcode = normalize_sample_id(barcode),
            norm_labid = normalize_sample_id(lab_id)
          )

        # Join on normalized IDs
        matched <- mic_samples %>%
          dplyr::left_join(
            biobank %>% dplyr::select(norm_barcode, norm_labid, date_received_cpltha),
            by = c("norm_id" = "norm_barcode")
          ) %>%
          dplyr::filter(!is.na(date_received_cpltha), !is.na(test_date)) %>%
          dplyr::mutate(
            processing_days = as.numeric(test_date - date_received_cpltha)
          ) %>%
          dplyr::filter(processing_days >= 0, processing_days <= 365)

        if (nrow(matched) == 0) return(NULL)

        list(
          median_days = safe_median(matched$processing_days),
          n_samples = nrow(matched),
          data = matched
        )
      }, error = function(e) {
        message("Warning: Error calculating MIC processing times: ", e$message)
        NULL
      })
    })

    # Calculate processing times for ELISA data
    elisa_processing_times <- reactive({
      biobank <- filtered_data()
      elisa <- if (!is.null(elisa_data)) elisa_data() else NULL

      if (is.null(biobank) || !nrow(biobank)) return(list(pe = NULL, vsg = NULL))
      if (is.null(elisa) || !nrow(elisa)) return(list(pe = NULL, vsg = NULL))

      # Check for plate_date column
      if (!"plate_date" %in% names(elisa)) return(list(pe = NULL, vsg = NULL))

      # Normalize sample IDs
      elisa <- elisa %>%
        dplyr::mutate(
          test_date = suppressWarnings(as.Date(plate_date)),
          norm_id = normalize_sample_id(if ("code_barres_kps" %in% names(.)) code_barres_kps else barcode)
        )

      biobank <- biobank %>%
        dplyr::mutate(
          norm_barcode = normalize_sample_id(barcode)
        )

      # Join and calculate
      matched <- elisa %>%
        dplyr::left_join(
          biobank %>% dplyr::select(norm_barcode, date_received_cpltha),
          by = c("norm_id" = "norm_barcode")
        ) %>%
        dplyr::filter(!is.na(date_received_cpltha), !is.na(test_date)) %>%
        dplyr::mutate(
          processing_days = as.numeric(test_date - date_received_cpltha)
        ) %>%
        dplyr::filter(processing_days >= 0, processing_days <= 365)

      # Split by test type if available
      pe_data <- if ("test_type" %in% names(matched)) {
        matched %>% dplyr::filter(stringr::str_detect(tolower(test_type), "pe|plasmodium"))
      } else if ("plate_id" %in% names(matched)) {
        matched %>% dplyr::filter(stringr::str_detect(tolower(plate_id), "pe"))
      } else {
        matched
      }

      vsg_data <- if ("test_type" %in% names(matched)) {
        matched %>% dplyr::filter(stringr::str_detect(tolower(test_type), "vsg"))
      } else if ("plate_id" %in% names(matched)) {
        matched %>% dplyr::filter(stringr::str_detect(tolower(plate_id), "vsg"))
      } else {
        tibble::tibble()
      }

      list(
        pe = if (nrow(pe_data) > 0) list(
          median_days = safe_median(pe_data$processing_days),
          n_samples = nrow(pe_data),
          data = pe_data
        ) else NULL,
        vsg = if (nrow(vsg_data) > 0) list(
          median_days = safe_median(vsg_data$processing_days),
          n_samples = nrow(vsg_data),
          data = vsg_data
        ) else NULL
      )
    })

    # Calculate processing times for iELISA data
    ielisa_processing_times <- reactive({
      tryCatch({
        biobank <- filtered_data()
        ielisa <- if (!is.null(ielisa_data)) ielisa_data() else NULL

        if (is.null(biobank) || !nrow(biobank)) return(NULL)
        if (is.null(ielisa) || !nrow(ielisa)) return(NULL)

        # Check for plate_date column
        if (!"plate_date" %in% names(ielisa)) return(NULL)

        # Normalize sample IDs
        ielisa <- ielisa %>%
          dplyr::mutate(
            test_date = suppressWarnings(as.Date(plate_date)),
            norm_id = normalize_sample_id(if ("code_barres_kps" %in% names(.)) code_barres_kps else barcode)
          )

        biobank <- biobank %>%
          dplyr::mutate(
            norm_barcode = normalize_sample_id(barcode)
          )

        # Join and calculate
        matched <- ielisa %>%
          dplyr::left_join(
            biobank %>% dplyr::select(norm_barcode, date_received_cpltha),
            by = c("norm_id" = "norm_barcode")
          ) %>%
          dplyr::filter(!is.na(date_received_cpltha), !is.na(test_date)) %>%
          dplyr::mutate(
            processing_days = as.numeric(test_date - date_received_cpltha)
          ) %>%
          dplyr::filter(processing_days >= 0, processing_days <= 365)

        if (nrow(matched) == 0) return(NULL)

        list(
          median_days = safe_median(matched$processing_days),
          n_samples = nrow(matched),
          data = matched
        )
      }, error = function(e) {
        message("Warning: Error calculating iELISA processing times: ", e$message)
        NULL
      })
    })

    # Table data reactives
    delayed_shipments <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(!is.na(transport_field_to_cpltha)) %>%
        dplyr::filter(transport_field_to_cpltha > config$qc$max_transport_days) %>%
        dplyr::select(
          Barcode = barcode,
          `Lab ID` = lab_id,
          Province = province,
          `Sample Date` = date_sample,
          `Sent to CPLTHA` = date_sent_cpltha,
          `Days Delayed` = transport_field_to_cpltha
        ) %>%
        dplyr::arrange(dplyr::desc(`Days Delayed`))
    })

    in_transit_samples <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(shipped_to_cpltha & !received_at_cpltha) %>%
        dplyr::mutate(
          days_in_transit = as.numeric(Sys.Date() - date_sent_cpltha)
        ) %>%
        dplyr::select(
          Barcode = barcode,
          `Lab ID` = lab_id,
          Province = province,
          `Sample Date` = date_sample,
          `Sent to CPLTHA` = date_sent_cpltha,
          `Days in Transit` = days_in_transit
        ) %>%
        dplyr::arrange(dplyr::desc(`Days in Transit`))
    })

    temp_alert_samples <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(!is.na(transport_temperature)) %>%
        dplyr::filter(transport_temperature < 2 | transport_temperature > 8) %>%
        dplyr::select(
          Barcode = barcode,
          `Lab ID` = lab_id,
          Province = province,
          `Sample Date` = date_sample,
          `Temperature (°C)` = transport_temperature
        ) %>%
        dplyr::arrange(`Temperature (°C)`)
    })

    missing_dates <- reactive({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return(tibble::tibble())

      data %>%
        dplyr::filter(is.na(date_sent_cpltha) | is.na(date_received_cpltha) | is.na(date_sent_inrb)) %>%
        dplyr::mutate(
          missing_fields = paste0(
            ifelse(is.na(date_sent_cpltha), "Sent CPLTHA, ", ""),
            ifelse(is.na(date_received_cpltha), "Rcvd CPLTHA, ", ""),
            ifelse(is.na(date_sent_inrb), "Sent INRB", "")
          ) %>% stringr::str_remove(", $")
        ) %>%
        dplyr::select(
          Barcode = barcode,
          `Lab ID` = lab_id,
          `Sample Date` = date_sample,
          `Missing Fields` = missing_fields
        )
    })

    # ========================================================================
    # HERO KPI OUTPUTS
    # ========================================================================

    output$hero_total_days <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_days(metrics$median_total_transport)
    })

    output$hero_in_transit <- renderText({
      counts <- attention_counts()
      fmt_number(counts$in_transit)
    })

    output$hero_completion_rate <- renderText({
      metrics <- transport_metrics()
      if (is.null(metrics)) return("N/A")
      fmt_percent(metrics$pct_shipped_inrb)
    })

    output$hero_ontime_rate <- renderText({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return("N/A")

      with_transport <- data %>% dplyr::filter(!is.na(transport_field_to_cpltha))
      if (nrow(with_transport) == 0) return("N/A")

      on_time <- sum(with_transport$transport_field_to_cpltha <= config$qc$max_transport_days, na.rm = TRUE)
      rate <- (on_time / nrow(with_transport)) * 100
      fmt_percent(rate)
    })

    output$hero_temp_compliance <- renderText({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data)) return("N/A")

      with_temp <- data %>% dplyr::filter(!is.na(transport_temperature))
      if (nrow(with_temp) == 0) return("N/A")

      compliant <- sum(with_temp$transport_temperature >= 2 & with_temp$transport_temperature <= 8, na.rm = TRUE)
      rate <- (compliant / nrow(with_temp)) * 100
      fmt_percent(rate)
    })

    output$hero_wow_change <- renderText({
      wow <- wow_metrics()
      if (is.null(wow)) return("N/A")
      if (is.na(wow$this_week_median) || is.na(wow$last_week_median)) return("N/A")
      if (wow$last_week_median == 0) return("N/A")

      change <- ((wow$this_week_median - wow$last_week_median) / wow$last_week_median) * 100
      if (change < 0) {
        paste0(scales::number(abs(change), accuracy = 0.1), "% faster")
      } else if (change > 0) {
        paste0("+", scales::number(change, accuracy = 0.1), "% slower")
      } else {
        "No change"
      }
    })

    output$hero_attention_badges <- renderUI({
      counts <- attention_counts()

      badges <- list()

      if (counts$delayed > 0) {
        badges[[length(badges) + 1]] <- span(
          class = "attention-badge danger",
          icon("clock"), paste(counts$delayed, "Delayed")
        )
      }

      if (counts$in_transit > 0) {
        badges[[length(badges) + 1]] <- span(
          class = "attention-badge warning",
          icon("truck"), paste(counts$in_transit, "In Transit")
        )
      }

      if (counts$temp_alerts > 0) {
        badges[[length(badges) + 1]] <- span(
          class = "attention-badge danger",
          icon("thermometer-full"), paste(counts$temp_alerts, "Temp Alerts")
        )
      }

      if (counts$missing > 0) {
        badges[[length(badges) + 1]] <- span(
          class = "attention-badge warning",
          icon("question"), paste(counts$missing, "Missing Data")
        )
      }

      if (length(badges) == 0) {
        badges[[1]] <- span(
          class = "attention-badge success",
          icon("check-circle"), "All Clear"
        )
      }

      div(style = "display: flex; flex-wrap: wrap; gap: 8px; justify-content: center;",
          badges)
    })

    output$hero_last_update <- renderText({
      format(Sys.time(), "%H:%M")
    })

    # ========================================================================
    # SANKEY FLOW DIAGRAM
    # ========================================================================

    output$sankey_flow <- plotly::renderPlotly({
      data <- sankey_data()
      if (is.null(data)) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No data available")))
      }

      # Node labels
      labels <- c(
        paste0("Collected\n(", scales::comma(data$total), ")"),
        paste0("Shipped CPLTHA\n(", scales::comma(data$shipped_cpltha), ")"),
        paste0("Not Yet Shipped\n(", scales::comma(data$not_shipped_cpltha), ")"),
        paste0("Received CPLTHA\n(", scales::comma(data$received_cpltha), ")"),
        paste0("In Transit\n(", scales::comma(data$in_transit_cpltha), ")"),
        paste0("Shipped INRB\n(", scales::comma(data$shipped_inrb), ")"),
        paste0("Pending INRB\n(", scales::comma(data$pending_inrb), ")")
      )

      # Node colors
      node_colors <- c(
        "#4F46E5",  # Collected - indigo
        "#3B82F6",  # Shipped CPLTHA - blue
        "#9CA3AF",  # Not shipped - gray
        "#10B981",  # Received CPLTHA - green
        "#F59E0B",  # In transit - amber
        "#059669",  # Shipped INRB - emerald
        "#FBBF24"   # Pending INRB - yellow
      )

      # Source -> Target connections
      # 0: Collected
      # 1: Shipped CPLTHA
      # 2: Not Yet Shipped
      # 3: Received CPLTHA
      # 4: In Transit
      # 5: Shipped INRB
      # 6: Pending INRB

      sources <- c(0, 0, 1, 1, 3, 3)
      targets <- c(1, 2, 3, 4, 5, 6)
      values <- c(
        data$shipped_cpltha,
        data$not_shipped_cpltha,
        data$received_cpltha,
        data$in_transit_cpltha,
        data$shipped_inrb,
        data$pending_inrb
      )

      # Link colors (matching source)
      link_colors <- c(
        "rgba(79, 70, 229, 0.4)",  # Collected -> Shipped
        "rgba(156, 163, 175, 0.4)", # Collected -> Not shipped
        "rgba(59, 130, 246, 0.4)",  # Shipped -> Received
        "rgba(245, 158, 11, 0.4)",  # Shipped -> In transit
        "rgba(16, 185, 129, 0.4)",  # Received -> Shipped INRB
        "rgba(251, 191, 36, 0.4)"   # Received -> Pending
      )

      plotly::plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(
          label = labels,
          color = node_colors,
          pad = 20,
          thickness = 25,
          line = list(color = "white", width = 1)
        ),
        link = list(
          source = sources,
          target = targets,
          value = values,
          color = link_colors
        )
      ) %>%
        plotly::layout(
          font = list(size = 12, family = "Inter"),
          margin = list(t = 20, b = 20, l = 20, r = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # GAUGE CHARTS
    # ========================================================================

    create_gauge <- function(value, target, title, color) {
      if (is.na(value)) value <- 0

      # Determine color based on performance
      if (value <= target) {
        gauge_color <- "#10B981"  # Green - good
      } else if (value <= target * 1.5) {
        gauge_color <- "#F59E0B"  # Yellow - warning
      } else {
        gauge_color <- "#EF4444"  # Red - bad
      }

      plotly::plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = value,
        number = list(suffix = "d", font = list(size = 24)),
        gauge = list(
          axis = list(
            range = list(0, max(target * 2, value * 1.2)),
            tickwidth = 1,
            tickcolor = "#E5E7EB"
          ),
          bar = list(color = gauge_color, thickness = 0.75),
          bgcolor = "#F3F4F6",
          borderwidth = 0,
          steps = list(
            list(range = c(0, target), color = "rgba(16, 185, 129, 0.2)"),
            list(range = c(target, target * 1.5), color = "rgba(245, 158, 11, 0.2)"),
            list(range = c(target * 1.5, target * 2), color = "rgba(239, 68, 68, 0.2)")
          ),
          threshold = list(
            line = list(color = "#1F2937", width = 3),
            thickness = 0.8,
            value = target
          )
        )
      ) %>%
        plotly::layout(
          margin = list(t = 30, b = 0, l = 30, r = 30),
          font = list(family = "Inter")
        ) %>%
        plotly::config(displayModeBar = FALSE)
    }

    output$gauge_field_cpltha <- plotly::renderPlotly({
      metrics <- transport_metrics()
      value <- if (is.null(metrics)) 0 else metrics$median_field_to_cpltha
      create_gauge(value, target = 7, title = "Field → CPLTHA", color = "#3B82F6")
    })

    output$gauge_cpltha_receipt <- plotly::renderPlotly({
      metrics <- transport_metrics()
      value <- if (is.null(metrics)) 0 else metrics$median_cpltha_receipt
      create_gauge(value, target = 3, title = "CPLTHA Processing", color = "#8B5CF6")
    })

    output$gauge_cpltha_inrb <- plotly::renderPlotly({
      metrics <- transport_metrics()
      value <- if (is.null(metrics)) 0 else metrics$median_cpltha_to_inrb
      create_gauge(value, target = 7, title = "CPLTHA → INRB", color = "#10B981")
    })

    # ========================================================================
    # WEEK-OVER-WEEK COMPARISON
    # ========================================================================

    output$wow_comparison <- renderUI({
      wow <- wow_metrics()
      if (is.null(wow)) {
        return(div(class = "text-muted text-center p-3", "Insufficient data for comparison"))
      }

      # Calculate changes
      sample_change <- if (wow$last_week_samples > 0) {
        ((wow$this_week_samples - wow$last_week_samples) / wow$last_week_samples) * 100
      } else NA

      time_change <- if (!is.na(wow$last_week_median) && wow$last_week_median > 0) {
        ((wow$this_week_median - wow$last_week_median) / wow$last_week_median) * 100
      } else NA

      div(
        style = "padding: 0.5rem;",

        # Samples collected
        div(
          style = "margin-bottom: 1rem; padding: 0.75rem; background: #F9FAFB; border-radius: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(style = "font-weight: 500;", "Samples Collected"),
              if (!is.na(sample_change)) {
                span(
                  style = paste0("color: ", if (sample_change >= 0) "#10B981" else "#EF4444", ";"),
                  if (sample_change >= 0) icon("arrow-up") else icon("arrow-down"),
                  scales::number(abs(sample_change), accuracy = 0.1), "%"
                )
              }
          ),
          div(style = "display: flex; justify-content: space-between; margin-top: 0.5rem;",
              div(
                span(style = "font-size: 1.25rem; font-weight: 600;", wow$this_week_samples),
                span(style = "font-size: 0.75rem; color: #6c757d; margin-left: 4px;", "this week")
              ),
              div(style = "color: #6c757d;",
                  span(wow$last_week_samples),
                  span(style = "font-size: 0.75rem; margin-left: 4px;", "last week")
              )
          )
        ),

        # Transport time
        div(
          style = "margin-bottom: 1rem; padding: 0.75rem; background: #F9FAFB; border-radius: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(style = "font-weight: 500;", "Avg Transport Time"),
              if (!is.na(time_change)) {
                span(
                  style = paste0("color: ", if (time_change <= 0) "#10B981" else "#EF4444", ";"),
                  if (time_change <= 0) icon("arrow-down") else icon("arrow-up"),
                  scales::number(abs(time_change), accuracy = 0.1), "%"
                )
              }
          ),
          div(style = "display: flex; justify-content: space-between; margin-top: 0.5rem;",
              div(
                span(style = "font-size: 1.25rem; font-weight: 600;",
                     if (!is.na(wow$this_week_median)) paste0(round(wow$this_week_median, 1), "d") else "N/A"),
                span(style = "font-size: 0.75rem; color: #6c757d; margin-left: 4px;", "this week")
              ),
              div(style = "color: #6c757d;",
                  span(if (!is.na(wow$last_week_median)) paste0(round(wow$last_week_median, 1), "d") else "N/A"),
                  span(style = "font-size: 0.75rem; margin-left: 4px;", "last week")
              )
          )
        ),

        # Shipments
        div(
          style = "padding: 0.75rem; background: #F9FAFB; border-radius: 8px;",
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              span(style = "font-weight: 500;", "Samples Shipped")
          ),
          div(style = "display: flex; justify-content: space-between; margin-top: 0.5rem;",
              div(
                span(style = "font-size: 1.25rem; font-weight: 600;", wow$this_week_shipped),
                span(style = "font-size: 0.75rem; color: #6c757d; margin-left: 4px;", "this week")
              ),
              div(style = "color: #6c757d;",
                  span(wow$last_week_shipped),
                  span(style = "font-size: 0.75rem; margin-left: 4px;", "last week")
              )
          )
        )
      )
    })

    # ========================================================================
    # TEMPERATURE VISUALIZATIONS
    # ========================================================================

    output$temp_timeline <- plotly::renderPlotly({
      data <- temp_timeline_data()
      if (nrow(data) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No temperature data available")))
      }

      plotly::plot_ly(data, x = ~sample_week) %>%
        # Compliance band (2-8°C)
        plotly::add_ribbons(
          ymin = 2, ymax = 8,
          fillcolor = "rgba(16, 185, 129, 0.15)",
          line = list(color = "transparent"),
          name = "Target Range (2-8°C)",
          showlegend = TRUE
        ) %>%
        # Min-max range
        plotly::add_ribbons(
          ymin = ~min_temp, ymax = ~max_temp,
          fillcolor = "rgba(59, 130, 246, 0.2)",
          line = list(color = "transparent"),
          name = "Min-Max Range",
          showlegend = TRUE
        ) %>%
        # Average line
        plotly::add_lines(
          y = ~avg_temp,
          line = list(color = "#3B82F6", width = 3),
          name = "Avg Temperature",
          hovertemplate = "Week: %{x}<br>Avg: %{y:.1f}°C<extra></extra>"
        ) %>%
        # Reference lines
        plotly::add_lines(
          y = rep(2, nrow(data)),
          line = list(color = "#10B981", width = 1, dash = "dash"),
          name = "Min Target",
          showlegend = FALSE
        ) %>%
        plotly::add_lines(
          y = rep(8, nrow(data)),
          line = list(color = "#10B981", width = 1, dash = "dash"),
          name = "Max Target",
          showlegend = FALSE
        ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Temperature (°C)"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(t = 20, b = 60)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    output$temp_distribution <- plotly::renderPlotly({
      req(filtered_data())
      temps <- filtered_data()$transport_temperature

      # Ensure temps is numeric and remove NA/non-finite values
      if (!is.numeric(temps)) {
        temps <- suppressWarnings(as.numeric(temps))
      }
      temps <- temps[!is.na(temps) & is.finite(temps)]

      if (length(temps) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No temperature data")))
      }

      # Pre-calculate histogram for reference lines
      hist_data <- hist(temps, breaks = 25, plot = FALSE)
      y_max <- max(hist_data$counts) * 1.1

      plotly::plot_ly(x = temps, type = "histogram", nbinsx = 25,
                      marker = list(
                        color = ifelse(temps >= 2 & temps <= 8, "#10B981", "#EF4444"),
                        line = list(color = "white", width = 1)
                      )) %>%
        plotly::add_segments(
          x = 2, xend = 2, y = 0, yend = y_max,
          line = list(color = "#10B981", width = 2, dash = "dash"),
          name = "Min (2°C)"
        ) %>%
        plotly::add_segments(
          x = 8, xend = 8, y = 0, yend = y_max,
          line = list(color = "#10B981", width = 2, dash = "dash"),
          name = "Max (8°C)"
        ) %>%
        plotly::layout(
          xaxis = list(title = "Temperature (°C)"),
          yaxis = list(title = "Samples"),
          showlegend = FALSE,
          bargap = 0.05,
          margin = list(t = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # TRANSPORT TREND CHARTS
    # ========================================================================

    output$transport_timeline_plot <- plotly::renderPlotly({
      data <- transport_timeline()
      if (nrow(data) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No data available")))
      }

      plotly::plot_ly(data, x = ~sample_week) %>%
        plotly::add_trace(
          y = ~field_to_cpltha,
          type = "scatter",
          mode = "lines+markers",
          name = "Field → CPLTHA",
          line = list(color = "#3B82F6", width = 2),
          marker = list(size = 6)
        ) %>%
        plotly::add_trace(
          y = ~cpltha_to_inrb,
          type = "scatter",
          mode = "lines+markers",
          name = "CPLTHA → INRB",
          line = list(color = "#10B981", width = 2),
          marker = list(size = 6)
        ) %>%
        plotly::add_trace(
          y = ~total_transport,
          type = "scatter",
          mode = "lines+markers",
          name = "Total",
          line = list(color = "#8B5CF6", width = 2, dash = "dot"),
          marker = list(size = 6)
        ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Median Days"),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.15),
          margin = list(t = 20, b = 60)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    output$transport_province_plot <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      if (!nrow(data) || !"province" %in% names(data)) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No province data")))
      }

      summary <- data %>%
        dplyr::filter(!is.na(province)) %>%
        dplyr::group_by(province) %>%
        dplyr::summarise(
          median_total = safe_median(total_transport_days),
          samples = dplyr::n(),
          on_time_pct = mean(total_transport_days <= config$qc$max_transport_days, na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        dplyr::arrange(median_total) %>%
        dplyr::mutate(
          bar_color = dplyr::case_when(
            median_total <= 7 ~ "#10B981",
            median_total <= 14 ~ "#F59E0B",
            TRUE ~ "#EF4444"
          )
        )

      if (nrow(summary) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No province data")))
      }

      plotly::plot_ly(
        summary,
        y = ~reorder(province, median_total),
        x = ~median_total,
        type = "bar",
        orientation = "h",
        marker = list(color = ~bar_color),
        text = ~paste0(round(median_total, 1), "d | ", samples, " samples | ", round(on_time_pct, 0), "% on-time"),
        textposition = "auto",
        hovertemplate = "<b>%{y}</b><br>Median: %{x:.1f} days<br>Samples: %{text}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Median Transport Days"),
          yaxis = list(title = ""),
          margin = list(l = 120, t = 20)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # DATA TABLES
    # ========================================================================

    output$table_in_transit <- DT::renderDT({
      data <- in_transit_samples()
      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble::tibble(Message = "No samples currently in transit"),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(5, 'desc'))
        )
      ) %>%
        DT::formatStyle(
          'Days in Transit',
          backgroundColor = DT::styleInterval(
            c(7, 14),
            c('#D1FAE5', '#FEF3C7', '#FEE2E2')
          )
        )
    })

    output$table_delayed <- DT::renderDT({
      data <- delayed_shipments()
      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble::tibble(Message = "No delayed shipments - great job!"),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(5, 'desc'))
        )
      ) %>%
        DT::formatStyle(
          'Days Delayed',
          backgroundColor = DT::styleInterval(
            c(45, 60),
            c('#FEF3C7', '#FED7AA', '#FEE2E2')
          )
        )
    })

    output$table_temp_alerts <- DT::renderDT({
      data <- temp_alert_samples()
      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble::tibble(Message = "All samples within temperature range"),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        )
      ) %>%
        DT::formatStyle(
          'Temperature (°C)',
          backgroundColor = DT::styleInterval(
            c(2, 8),
            c('#FEE2E2', '#D1FAE5', '#FEE2E2')
          )
        )
    })

    output$table_missing_dates <- DT::renderDT({
      data <- missing_dates()
      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble::tibble(Message = "All transport dates recorded"),
          rownames = FALSE, options = list(dom = 't')
        ))
      }

      DT::datatable(
        data,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip'
        )
      )
    })

    # ========================================================================
    # LAB PROCESSING TIME OUTPUTS
    # ========================================================================

    output$proc_mic_days <- renderText({
      mic <- mic_processing_times()
      if (is.null(mic)) return("N/A")
      fmt_days(mic$median_days)
    })

    output$proc_mic_samples <- renderText({
      mic <- mic_processing_times()
      if (is.null(mic)) return("No data")
      paste(scales::comma(mic$n_samples), "samples")
    })

    output$proc_elisa_pe_days <- renderText({
      elisa <- elisa_processing_times()
      if (is.null(elisa$pe)) return("N/A")
      fmt_days(elisa$pe$median_days)
    })

    output$proc_elisa_pe_samples <- renderText({
      elisa <- elisa_processing_times()
      if (is.null(elisa$pe)) return("No data")
      paste(scales::comma(elisa$pe$n_samples), "samples")
    })

    output$proc_elisa_vsg_days <- renderText({
      elisa <- elisa_processing_times()
      if (is.null(elisa$vsg)) return("N/A")
      fmt_days(elisa$vsg$median_days)
    })

    output$proc_elisa_vsg_samples <- renderText({
      elisa <- elisa_processing_times()
      if (is.null(elisa$vsg)) return("No data")
      paste(scales::comma(elisa$vsg$n_samples), "samples")
    })

    output$proc_ielisa_days <- renderText({
      ielisa <- ielisa_processing_times()
      if (is.null(ielisa)) return("N/A")
      fmt_days(ielisa$median_days)
    })

    output$proc_ielisa_samples <- renderText({
      ielisa <- ielisa_processing_times()
      if (is.null(ielisa)) return("No data")
      paste(scales::comma(ielisa$n_samples), "samples")
    })

    # Processing boxplot
    output$processing_boxplot <- plotly::renderPlotly({
      # Collect all processing data
      mic <- mic_processing_times()
      elisa <- elisa_processing_times()
      ielisa <- ielisa_processing_times()

      all_data <- list()

      if (!is.null(mic) && !is.null(mic$data) && nrow(mic$data) > 0) {
        all_data$MIC <- mic$data %>%
          dplyr::mutate(test_type = "MIC qPCR")
      }

      if (!is.null(elisa$pe) && !is.null(elisa$pe$data) && nrow(elisa$pe$data) > 0) {
        all_data$PE <- elisa$pe$data %>%
          dplyr::mutate(test_type = "ELISA-PE")
      }

      if (!is.null(elisa$vsg) && !is.null(elisa$vsg$data) && nrow(elisa$vsg$data) > 0) {
        all_data$VSG <- elisa$vsg$data %>%
          dplyr::mutate(test_type = "ELISA-VSG")
      }

      if (!is.null(ielisa) && !is.null(ielisa$data) && nrow(ielisa$data) > 0) {
        all_data$iELISA <- ielisa$data %>%
          dplyr::mutate(test_type = "iELISA")
      }

      if (length(all_data) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(title = list(text = "No processing data available")))
      }

      # Combine all data
      combined <- dplyr::bind_rows(all_data)

      # Define colors for each test type
      colors <- c(
        "MIC qPCR" = "#8B5CF6",
        "ELISA-PE" = "#EC4899",
        "ELISA-VSG" = "#F97316",
        "iELISA" = "#06B6D4"
      )

      plotly::plot_ly(
        combined,
        x = ~test_type,
        y = ~processing_days,
        type = "box",
        color = ~test_type,
        colors = colors,
        boxpoints = "outliers",
        hoverinfo = "y"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Days from Reception to Test"),
          showlegend = FALSE,
          margin = list(t = 20, b = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    output$download_in_transit <- downloadHandler(
      filename = function() {
        paste0("in_transit_samples_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(in_transit_samples(), file)
      }
    )

    output$download_delayed <- downloadHandler(
      filename = function() {
        paste0("delayed_shipments_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(delayed_shipments(), file)
      }
    )

    output$download_temp_alerts <- downloadHandler(
      filename = function() {
        paste0("temperature_alerts_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(temp_alert_samples(), file)
      }
    )

    output$download_missing <- downloadHandler(
      filename = function() {
        paste0("missing_transport_dates_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(missing_dates(), file)
      }
    )
  })
}
