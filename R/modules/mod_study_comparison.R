# R/modules/mod_study_comparison.R
# Study Results Comparison Module - Compare DA (Active) vs DP (Passive) Screening
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Study Comparison Module UI
#' @param id Module namespace ID
#' @export
mod_study_comparison_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Study Comparison",
    icon = icon("balance-scale"),

    page_fluid(
      # Header
      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex align-items-center gap-2",
            bsicons::bs_icon("bar-chart-line", size = "1.5rem"),
            "Study Results Comparison: DA vs DP",
            span(class = "badge bg-info ms-2", "Statistical Analysis")
          )
        ),
        card_body(
          p("Compare results between Active Screening (DA - Dépistage Actif) and Passive Screening (DP - Dépistage Passif) across demographics, geography, and all test types."),
          p(class = "text-muted mb-0 small", "Includes chi-squared tests for categorical variables and Mann-Whitney U tests for continuous variables.")
        )
      ),

      # KPI Summary Row
      layout_column_wrap(
        width = 1/5, fixed_width = TRUE, heights_equal = "row", gap = "12px",

        value_box(
          title = "DA Samples",
          value = textOutput(ns("kpi_da_count")),
          showcase = icon("users"),
          theme = "info"
        ),
        value_box(
          title = "DP Samples",
          value = textOutput(ns("kpi_dp_count")),
          showcase = icon("user"),
          theme = "success"
        ),
        value_box(
          title = "DA Positivity",
          value = textOutput(ns("kpi_da_positivity")),
          showcase = icon("vial"),
          theme = "warning"
        ),
        value_box(
          title = "DP Positivity",
          value = textOutput(ns("kpi_dp_positivity")),
          showcase = icon("vial"),
          theme = "danger"
        ),
        value_box(
          title = "Significance",
          value = textOutput(ns("kpi_overall_pvalue")),
          showcase = icon("calculator"),
          theme = "secondary"
        )
      ),

      # Main tabbed content
      navset_card_tab(
        id = ns("comparison_tabs"),

        # ==== TAB 1: Demographics ====
        nav_panel(
          title = "Demographics",
          icon = icon("users"),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("Age Distribution by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("age_comparison_plot"), height = "400px")
              )
            ),
            card(
              card_header("Sex Distribution by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("sex_comparison_plot"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("Age Groups by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("age_group_comparison_plot"), height = "400px")
              )
            ),
            card(
              card_header("Case Category by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("case_category_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Demographics Statistical Summary"),
            card_body(
              DT::DTOutput(ns("demographics_stats_table"))
            )
          )
        ),

        # ==== TAB 2: Geographic ====
        nav_panel(
          title = "Geographic",
          icon = icon("map-marker-alt"),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("Province Distribution by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("province_comparison_plot"), height = "450px")
              )
            ),
            card(
              card_header("Health Zone Distribution by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("zone_comparison_plot"), height = "450px")
              )
            )
          ),

          card(
            card_header("Geographic Distribution Summary"),
            card_body(
              DT::DTOutput(ns("geographic_stats_table"))
            )
          )
        ),

        # ==== TAB 3: Test Results ====
        nav_panel(
          title = "Test Results",
          icon = icon("vials"),

          # Positivity rates comparison
          card(
            card_header("Positivity Rates by Study Type"),
            card_body_fill(
              plotly::plotlyOutput(ns("positivity_comparison_plot"), height = "400px")
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("MIC qPCR Results by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("mic_comparison_plot"), height = "400px")
              )
            ),
            card(
              card_header("ELISA-PE Results by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_pe_comparison_plot"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("ELISA-VSG Results by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_vsg_comparison_plot"), height = "400px")
              )
            ),
            card(
              card_header("iELISA Results by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("ielisa_comparison_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Test Results Statistical Summary"),
            card_body(
              DT::DTOutput(ns("test_stats_table"))
            )
          )
        ),

        # ==== TAB 4: Transport & Extraction ====
        nav_panel(
          title = "Transport & Extraction",
          icon = icon("truck"),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("Transport Time by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("transport_time_plot"), height = "400px")
              )
            ),
            card(
              card_header("DRS Volume by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("drs_volume_plot"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("DRS State by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("drs_state_plot"), height = "400px")
              )
            ),
            card(
              card_header("Extract Quality by Study Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("extract_quality_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Transport & Extraction Statistical Summary"),
            card_body(
              DT::DTOutput(ns("transport_stats_table"))
            )
          )
        ),

        # ==== TAB 5: Summary Statistics ====
        nav_panel(
          title = "Summary",
          icon = icon("table"),

          card(
            card_header("Comprehensive Comparison Summary"),
            card_body(
              DT::DTOutput(ns("full_summary_table"))
            )
          ),

          card(
            card_header("Statistical Tests Summary"),
            card_body(
              DT::DTOutput(ns("statistical_tests_table"))
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

#' Study Comparison Module Server
#' @param id Module namespace ID
#' @param biobank_df Reactive containing biobank data
#' @param extraction_df Reactive containing extraction data
#' @param mic_df Reactive containing MIC qPCR data
#' @param elisa_pe_df Reactive containing ELISA-PE data
#' @param elisa_vsg_df Reactive containing ELISA-VSG data
#' @param ielisa_df Reactive containing iELISA data
#' @param filters Reactive containing filter settings
#' @export
mod_study_comparison_server <- function(id,
                                        biobank_df = reactive(NULL),
                                        extraction_df = reactive(NULL),
                                        mic_df = reactive(NULL),
                                        elisa_pe_df = reactive(NULL),
                                        elisa_vsg_df = reactive(NULL),
                                        ielisa_df = reactive(NULL),
                                        filters = reactive(NULL)) {

  moduleServer(id, function(input, output, session) {

    # Color palette for DA vs DP
    study_colors <- c("DA" = "#3498DB", "DP" = "#E74C3C")

    # ========================================================================
    # REACTIVE DATA PREPARATION
    # ========================================================================

    # Split biobank data by study type
    study_data <- reactive({
      req(biobank_df())
      data <- biobank_df()

      if (!"study" %in% names(data) || nrow(data) == 0) {
        return(list(da = data.frame(), dp = data.frame(), all = data))
      }

      list(
        da = data %>% dplyr::filter(study == "DA"),
        dp = data %>% dplyr::filter(study == "DP"),
        all = data %>% dplyr::filter(study %in% c("DA", "DP"))
      )
    })

    # Link test data with study type from biobank
    linked_mic_data <- reactive({
      mic <- mic_df()
      biobank <- biobank_df()

      if (is.null(mic) || nrow(mic) == 0) return(NULL)
      if (is.null(biobank) || nrow(biobank) == 0) return(mic)

      # Try to link via barcode
      if ("barcode" %in% names(mic) && "barcode" %in% names(biobank)) {
        mic %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = "barcode"
          )
      } else if ("sample_id" %in% names(mic) && "barcode" %in% names(biobank)) {
        mic %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = c("sample_id" = "barcode")
          )
      } else {
        mic
      }
    })

    linked_elisa_pe_data <- reactive({
      elisa <- elisa_pe_df()
      biobank <- biobank_df()

      if (is.null(elisa) || nrow(elisa) == 0) return(NULL)
      if (is.null(biobank) || nrow(biobank) == 0) return(elisa)

      # Check for study column already present
      if ("study" %in% names(elisa)) return(elisa)

      # Link via barcode
      barcode_col <- intersect(c("barcode", "code_barres_kps", "sample_id"), names(elisa))[1]
      if (!is.na(barcode_col) && "barcode" %in% names(biobank)) {
        elisa %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = stats::setNames("barcode", barcode_col)
          )
      } else {
        elisa
      }
    })

    linked_elisa_vsg_data <- reactive({
      elisa <- elisa_vsg_df()
      biobank <- biobank_df()

      if (is.null(elisa) || nrow(elisa) == 0) return(NULL)
      if (is.null(biobank) || nrow(biobank) == 0) return(elisa)

      if ("study" %in% names(elisa)) return(elisa)

      barcode_col <- intersect(c("barcode", "code_barres_kps", "sample_id"), names(elisa))[1]
      if (!is.na(barcode_col) && "barcode" %in% names(biobank)) {
        elisa %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = stats::setNames("barcode", barcode_col)
          )
      } else {
        elisa
      }
    })

    linked_ielisa_data <- reactive({
      ielisa <- ielisa_df()
      biobank <- biobank_df()

      if (is.null(ielisa) || nrow(ielisa) == 0) return(NULL)
      if (is.null(biobank) || nrow(biobank) == 0) return(ielisa)

      if ("study" %in% names(ielisa)) return(ielisa)

      barcode_col <- intersect(c("barcode", "code_barres_kps", "sample_id"), names(ielisa))[1]
      if (!is.na(barcode_col) && "barcode" %in% names(biobank)) {
        ielisa %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = stats::setNames("barcode", barcode_col)
          )
      } else {
        ielisa
      }
    })

    linked_extraction_data <- reactive({
      extract <- extraction_df()
      biobank <- biobank_df()

      if (is.null(extract) || nrow(extract) == 0) return(NULL)
      if (is.null(biobank) || nrow(biobank) == 0) return(extract)

      if ("study" %in% names(extract)) return(extract)

      barcode_col <- intersect(c("barcode", "sample_id", "code_barres_kps"), names(extract))[1]
      if (!is.na(barcode_col) && "barcode" %in% names(biobank)) {
        extract %>%
          dplyr::left_join(
            biobank %>% dplyr::select(barcode, study) %>% dplyr::distinct(),
            by = stats::setNames("barcode", barcode_col)
          )
      } else {
        extract
      }
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_da_count <- renderText({
      data <- study_data()
      scales::comma(nrow(data$da))
    })

    output$kpi_dp_count <- renderText({
      data <- study_data()
      scales::comma(nrow(data$dp))
    })

    output$kpi_da_positivity <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"study" %in% names(mic)) return("N/A")

      da_mic <- mic %>% dplyr::filter(study == "DA")
      if (nrow(da_mic) == 0) return("N/A")

      # Find positivity column
      pos_col <- intersect(c("positive", "is_positive", "overall_positive", "PipelineCategory"), names(da_mic))[1]
      if (is.na(pos_col)) return("N/A")

      if (pos_col == "PipelineCategory") {
        pos_rate <- mean(da_mic[[pos_col]] %in% c("positive", "detected"), na.rm = TRUE) * 100
      } else {
        pos_rate <- mean(da_mic[[pos_col]] == TRUE, na.rm = TRUE) * 100
      }
      sprintf("%.1f%%", pos_rate)
    })

    output$kpi_dp_positivity <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"study" %in% names(mic)) return("N/A")

      dp_mic <- mic %>% dplyr::filter(study == "DP")
      if (nrow(dp_mic) == 0) return("N/A")

      pos_col <- intersect(c("positive", "is_positive", "overall_positive", "PipelineCategory"), names(dp_mic))[1]
      if (is.na(pos_col)) return("N/A")

      if (pos_col == "PipelineCategory") {
        pos_rate <- mean(dp_mic[[pos_col]] %in% c("positive", "detected"), na.rm = TRUE) * 100
      } else {
        pos_rate <- mean(dp_mic[[pos_col]] == TRUE, na.rm = TRUE) * 100
      }
      sprintf("%.1f%%", pos_rate)
    })

    output$kpi_overall_pvalue <- renderText({
      data <- study_data()
      if (nrow(data$all) == 0 || !"sex" %in% names(data$all)) return("N/A")

      # Quick chi-squared on sex distribution
      tryCatch({
        tbl <- table(data$all$study, data$all$sex)
        if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
          test <- chisq.test(tbl)
          if (test$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", test$p.value)
        } else {
          "N/A"
        }
      }, error = function(e) "N/A")
    })

    # ========================================================================
    # DEMOGRAPHICS PLOTS
    # ========================================================================

    output$age_comparison_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0 || !"age" %in% names(data)) return(plotly::plotly_empty())

      data <- data %>% dplyr::filter(!is.na(age), !is.na(study))

      plotly::plot_ly(data, x = ~age, color = ~study, colors = study_colors,
                      type = "histogram", alpha = 0.7,
                      hovertemplate = "Age: %{x}<br>Count: %{y}<extra></extra>") %>%
        plotly::layout(
          barmode = "overlay",
          xaxis = list(title = "Age (years)"),
          yaxis = list(title = "Count"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$sex_comparison_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0 || !"sex" %in% names(data)) return(plotly::plotly_empty())

      sex_data <- data %>%
        dplyr::filter(!is.na(sex), !is.na(study), sex %in% c("M", "F")) %>%
        dplyr::count(study, sex) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      plotly::plot_ly(sex_data, x = ~study, y = ~pct, color = ~sex,
                      colors = c("M" = "#3498DB", "F" = "#E91E63"),
                      type = "bar", text = ~sprintf("%.1f%%", pct), textposition = "auto",
                      hovertemplate = "Study: %{x}<br>Sex: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "Percentage (%)", range = c(0, 100)),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$age_group_comparison_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0 || !"age" %in% names(data)) return(plotly::plotly_empty())

      age_data <- data %>%
        dplyr::filter(!is.na(age), !is.na(study)) %>%
        dplyr::mutate(
          age_group = cut(age,
            breaks = c(0, 5, 15, 25, 35, 45, 55, 65, Inf),
            labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
            right = FALSE
          )
        ) %>%
        dplyr::count(study, age_group) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      plotly::plot_ly(age_data, x = ~age_group, y = ~pct, color = ~study,
                      colors = study_colors, type = "bar",
                      hovertemplate = "Age: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Percentage (%)"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$case_category_plot <- plotly::renderPlotly({
      data <- study_data()$all

      # Create case_category if not present
      if (!"case_category" %in% names(data)) {
        if ("previous_case" %in% names(data) || "treated" %in% names(data)) {
          data <- data %>%
            dplyr::mutate(
              case_category = dplyr::case_when(
                previous_case == "Oui" | treated == "Oui" ~ "Previous/Treated",
                previous_case == "Non" & treated == "Non" ~ "New case",
                previous_case == "Incertain" | treated == "Incertain" ~ "Uncertain",
                TRUE ~ "Unknown"
              )
            )
        } else {
          return(plotly::plotly_empty())
        }
      }

      case_data <- data %>%
        dplyr::filter(!is.na(study)) %>%
        dplyr::count(study, case_category) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      plotly::plot_ly(case_data, x = ~case_category, y = ~pct, color = ~study,
                      colors = study_colors, type = "bar",
                      hovertemplate = "Category: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Case Category"),
          yaxis = list(title = "Percentage (%)"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    # Demographics statistics table
    output$demographics_stats_table <- DT::renderDT({
      data <- study_data()$all
      if (nrow(data) == 0) return(DT::datatable(data.frame(Message = "No data")))

      # Calculate statistics
      stats_list <- list()

      # Age comparison
      if ("age" %in% names(data)) {
        da_ages <- data$age[data$study == "DA" & !is.na(data$age)]
        dp_ages <- data$age[data$study == "DP" & !is.na(data$age)]

        if (length(da_ages) > 0 && length(dp_ages) > 0) {
          wilcox_test <- tryCatch(wilcox.test(da_ages, dp_ages), error = function(e) NULL)
          stats_list[[length(stats_list) + 1]] <- data.frame(
            Variable = "Age",
            DA_Value = sprintf("%.1f (%.1f-%.1f)", median(da_ages), quantile(da_ages, 0.25), quantile(da_ages, 0.75)),
            DP_Value = sprintf("%.1f (%.1f-%.1f)", median(dp_ages), quantile(dp_ages, 0.25), quantile(dp_ages, 0.75)),
            Test = "Mann-Whitney U",
            P_Value = if (!is.null(wilcox_test)) sprintf("%.4f", wilcox_test$p.value) else "N/A",
            stringsAsFactors = FALSE
          )
        }
      }

      # Sex comparison
      if ("sex" %in% names(data)) {
        sex_tbl <- table(data$study, data$sex)
        if (all(dim(sex_tbl) >= 2)) {
          chi_test <- tryCatch(chisq.test(sex_tbl), error = function(e) NULL)
          da_male_pct <- if ("DA" %in% rownames(sex_tbl) && "M" %in% colnames(sex_tbl))
            sex_tbl["DA", "M"] / sum(sex_tbl["DA", ]) * 100 else NA
          dp_male_pct <- if ("DP" %in% rownames(sex_tbl) && "M" %in% colnames(sex_tbl))
            sex_tbl["DP", "M"] / sum(sex_tbl["DP", ]) * 100 else NA

          stats_list[[length(stats_list) + 1]] <- data.frame(
            Variable = "Sex (% Male)",
            DA_Value = sprintf("%.1f%%", da_male_pct),
            DP_Value = sprintf("%.1f%%", dp_male_pct),
            Test = "Chi-squared",
            P_Value = if (!is.null(chi_test)) sprintf("%.4f", chi_test$p.value) else "N/A",
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(stats_list) == 0) {
        return(DT::datatable(data.frame(Message = "Insufficient data for statistics")))
      }

      stats_df <- do.call(rbind, stats_list)

      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', scrollX = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatStyle("P_Value",
          backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa"))
        )
    })

    # ========================================================================
    # GEOGRAPHIC PLOTS
    # ========================================================================

    output$province_comparison_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0 || !"province" %in% names(data)) return(plotly::plotly_empty())

      prov_data <- data %>%
        dplyr::filter(!is.na(province), !is.na(study)) %>%
        dplyr::count(study, province) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      plotly::plot_ly(prov_data, x = ~province, y = ~pct, color = ~study,
                      colors = study_colors, type = "bar",
                      hovertemplate = "Province: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "Percentage (%)"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$zone_comparison_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0 || !"health_zone" %in% names(data)) return(plotly::plotly_empty())

      # Get top 10 zones by total count
      top_zones <- data %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::count(health_zone, sort = TRUE) %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::pull(health_zone)

      zone_data <- data %>%
        dplyr::filter(!is.na(health_zone), !is.na(study), health_zone %in% top_zones) %>%
        dplyr::count(study, health_zone) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      plotly::plot_ly(zone_data, y = ~health_zone, x = ~pct, color = ~study,
                      colors = study_colors, type = "bar", orientation = "h",
                      hovertemplate = "Zone: %{y}<br>Study: %{fullData.name}<br>%{x:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Percentage (%)"),
          yaxis = list(title = "", categoryorder = "total ascending"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$geographic_stats_table <- DT::renderDT({
      data <- study_data()$all
      if (nrow(data) == 0 || !"health_zone" %in% names(data)) {
        return(DT::datatable(data.frame(Message = "No geographic data")))
      }

      # Summary by health zone
      zone_summary <- data %>%
        dplyr::filter(!is.na(health_zone), !is.na(study)) %>%
        dplyr::group_by(health_zone) %>%
        dplyr::summarise(
          Total = dplyr::n(),
          DA = sum(study == "DA"),
          DP = sum(study == "DP"),
          DA_Pct = round(DA / Total * 100, 1),
          DP_Pct = round(DP / Total * 100, 1),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(Total))

      DT::datatable(
        zone_summary,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE),
        class = "table-sm"
      )
    })

    # ========================================================================
    # TEST RESULTS PLOTS
    # ========================================================================

    output$positivity_comparison_plot <- plotly::renderPlotly({
      # Collect positivity rates from all tests
      results <- list()

      # MIC
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "study" %in% names(mic)) {
        pos_col <- intersect(c("positive", "is_positive", "overall_positive"), names(mic))[1]
        if (!is.na(pos_col)) {
          mic_summary <- mic %>%
            dplyr::filter(!is.na(study), study %in% c("DA", "DP")) %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(positivity = mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100, .groups = "drop") %>%
            dplyr::mutate(test = "MIC")
          results[[length(results) + 1]] <- mic_summary
        }
      }

      # ELISA-PE
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0 && "study" %in% names(pe)) {
        pos_col <- intersect(c("positive", "is_positive", "status_final"), names(pe))[1]
        if (!is.na(pos_col)) {
          pe_summary <- pe %>%
            dplyr::filter(!is.na(study), study %in% c("DA", "DP")) %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = if (pos_col == "status_final") {
                mean(.data[[pos_col]] == "Positive", na.rm = TRUE) * 100
              } else {
                mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100
              },
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "ELISA-PE")
          results[[length(results) + 1]] <- pe_summary
        }
      }

      # ELISA-VSG
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0 && "study" %in% names(vsg)) {
        pos_col <- intersect(c("positive", "is_positive", "status_final"), names(vsg))[1]
        if (!is.na(pos_col)) {
          vsg_summary <- vsg %>%
            dplyr::filter(!is.na(study), study %in% c("DA", "DP")) %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = if (pos_col == "status_final") {
                mean(.data[[pos_col]] == "Positive", na.rm = TRUE) * 100
              } else {
                mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100
              },
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "ELISA-VSG")
          results[[length(results) + 1]] <- vsg_summary
        }
      }

      # iELISA
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0 && "study" %in% names(ielisa)) {
        pos_col <- intersect(c("positive_L13", "positive", "is_positive"), names(ielisa))[1]
        if (!is.na(pos_col)) {
          ielisa_summary <- ielisa %>%
            dplyr::filter(!is.na(study), study %in% c("DA", "DP")) %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(positivity = mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100, .groups = "drop") %>%
            dplyr::mutate(test = "iELISA")
          results[[length(results) + 1]] <- ielisa_summary
        }
      }

      if (length(results) == 0) return(plotly::plotly_empty())

      all_results <- do.call(rbind, results)

      plotly::plot_ly(all_results, x = ~test, y = ~positivity, color = ~study,
                      colors = study_colors, type = "bar",
                      text = ~sprintf("%.1f%%", positivity), textposition = "auto",
                      hovertemplate = "Test: %{x}<br>Study: %{fullData.name}<br>Positivity: %{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Test Type"),
          yaxis = list(title = "Positivity Rate (%)", range = c(0, max(all_results$positivity, na.rm = TRUE) * 1.2)),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$mic_comparison_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"study" %in% names(mic)) return(plotly::plotly_empty())

      # Find Cq column
      cq_col <- intersect(c("cq_177t", "Cq_177T", "cq"), names(mic))[1]
      if (is.na(cq_col)) return(plotly::plotly_empty())

      mic_filtered <- mic %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[cq_col]]))

      if (nrow(mic_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(mic_filtered, x = ~study, y = ~.data[[cq_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>Cq: %{y:.1f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "Cq Value (177T)"),
          showlegend = FALSE
        )
    })

    output$elisa_pe_comparison_plot <- plotly::renderPlotly({
      pe <- linked_elisa_pe_data()
      if (is.null(pe) || nrow(pe) == 0 || !"study" %in% names(pe)) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(pe))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      pe_filtered <- pe %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[pp_col]]))

      if (nrow(pe_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(pe_filtered, x = ~study, y = ~.data[[pp_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>PP%%: %{y:.1f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "PP% (ELISA-PE)"),
          showlegend = FALSE
        )
    })

    output$elisa_vsg_comparison_plot <- plotly::renderPlotly({
      vsg <- linked_elisa_vsg_data()
      if (is.null(vsg) || nrow(vsg) == 0 || !"study" %in% names(vsg)) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(vsg))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      vsg_filtered <- vsg %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[pp_col]]))

      if (nrow(vsg_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(vsg_filtered, x = ~study, y = ~.data[[pp_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>PP%%: %{y:.1f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "PP% (ELISA-VSG)"),
          showlegend = FALSE
        )
    })

    output$ielisa_comparison_plot <- plotly::renderPlotly({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0 || !"study" %in% names(ielisa)) return(plotly::plotly_empty())

      inh_col <- intersect(c("pct_inh_f2_13", "pct_inhibition", "inhibition"), names(ielisa))[1]
      if (is.na(inh_col)) return(plotly::plotly_empty())

      ielisa_filtered <- ielisa %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[inh_col]]))

      if (nrow(ielisa_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(ielisa_filtered, x = ~study, y = ~.data[[inh_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>%% Inhibition: %{y:.1f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "% Inhibition (iELISA L13)"),
          showlegend = FALSE
        )
    })

    # Test results statistics table
    output$test_stats_table <- DT::renderDT({
      stats_list <- list()

      # MIC statistics
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "study" %in% names(mic)) {
        pos_col <- intersect(c("positive", "is_positive", "overall_positive"), names(mic))[1]
        if (!is.na(pos_col)) {
          da_pos <- sum(mic$study == "DA" & mic[[pos_col]] == TRUE, na.rm = TRUE)
          da_total <- sum(mic$study == "DA", na.rm = TRUE)
          dp_pos <- sum(mic$study == "DP" & mic[[pos_col]] == TRUE, na.rm = TRUE)
          dp_total <- sum(mic$study == "DP", na.rm = TRUE)

          if (da_total > 0 && dp_total > 0) {
            tbl <- matrix(c(da_pos, da_total - da_pos, dp_pos, dp_total - dp_pos), nrow = 2)
            chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)

            stats_list[[length(stats_list) + 1]] <- data.frame(
              Test = "MIC qPCR",
              DA_N = da_total,
              DA_Positive = da_pos,
              DA_Rate = sprintf("%.1f%%", da_pos / da_total * 100),
              DP_N = dp_total,
              DP_Positive = dp_pos,
              DP_Rate = sprintf("%.1f%%", dp_pos / dp_total * 100),
              P_Value = if (!is.null(chi_test)) sprintf("%.4f", chi_test$p.value) else "N/A",
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Similar for ELISA-PE, ELISA-VSG, iELISA...
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0 && "study" %in% names(pe)) {
        pos_col <- intersect(c("positive", "is_positive", "status_final"), names(pe))[1]
        if (!is.na(pos_col)) {
          is_positive <- if (pos_col == "status_final") pe[[pos_col]] == "Positive" else pe[[pos_col]] == TRUE
          da_pos <- sum(pe$study == "DA" & is_positive, na.rm = TRUE)
          da_total <- sum(pe$study == "DA", na.rm = TRUE)
          dp_pos <- sum(pe$study == "DP" & is_positive, na.rm = TRUE)
          dp_total <- sum(pe$study == "DP", na.rm = TRUE)

          if (da_total > 0 && dp_total > 0) {
            tbl <- matrix(c(da_pos, da_total - da_pos, dp_pos, dp_total - dp_pos), nrow = 2)
            chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)

            stats_list[[length(stats_list) + 1]] <- data.frame(
              Test = "ELISA-PE",
              DA_N = da_total,
              DA_Positive = da_pos,
              DA_Rate = sprintf("%.1f%%", da_pos / da_total * 100),
              DP_N = dp_total,
              DP_Positive = dp_pos,
              DP_Rate = sprintf("%.1f%%", dp_pos / dp_total * 100),
              P_Value = if (!is.null(chi_test)) sprintf("%.4f", chi_test$p.value) else "N/A",
              stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(stats_list) == 0) {
        return(DT::datatable(data.frame(Message = "No test data available")))
      }

      stats_df <- do.call(rbind, stats_list)

      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', scrollX = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatStyle("P_Value",
          backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa"))
        )
    })

    # ========================================================================
    # TRANSPORT & EXTRACTION PLOTS
    # ========================================================================

    output$transport_time_plot <- plotly::renderPlotly({
      data <- study_data()$all
      if (nrow(data) == 0) return(plotly::plotly_empty())

      time_col <- intersect(c("transport_time", "transport_days", "days_to_extraction"), names(data))[1]
      if (is.na(time_col)) return(plotly::plotly_empty())

      data_filtered <- data %>%
        dplyr::filter(!is.na(study), !is.na(.data[[time_col]]))

      if (nrow(data_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(data_filtered, x = ~study, y = ~.data[[time_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>Days: %{y:.1f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "Transport Time (days)"),
          showlegend = FALSE
        )
    })

    output$drs_volume_plot <- plotly::renderPlotly({
      extract <- linked_extraction_data()
      if (is.null(extract) || nrow(extract) == 0 || !"study" %in% names(extract)) return(plotly::plotly_empty())

      vol_col <- intersect(c("drs_volume_ml", "volume_ml", "volume"), names(extract))[1]
      if (is.na(vol_col)) return(plotly::plotly_empty())

      extract_filtered <- extract %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[vol_col]]))

      if (nrow(extract_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(extract_filtered, x = ~study, y = ~.data[[vol_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>Volume: %{y:.2f} mL<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "DRS Volume (mL)"),
          showlegend = FALSE
        )
    })

    output$drs_state_plot <- plotly::renderPlotly({
      extract <- linked_extraction_data()
      if (is.null(extract) || nrow(extract) == 0 || !"study" %in% names(extract)) return(plotly::plotly_empty())

      state_col <- intersect(c("drs_state", "state", "etat_dbs"), names(extract))[1]
      if (is.na(state_col)) return(plotly::plotly_empty())

      state_data <- extract %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[state_col]])) %>%
        dplyr::count(study, state = .data[[state_col]]) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      if (nrow(state_data) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(state_data, x = ~state, y = ~pct, color = ~study,
                      colors = study_colors, type = "bar",
                      hovertemplate = "State: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "DRS State"),
          yaxis = list(title = "Percentage (%)"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$extract_quality_plot <- plotly::renderPlotly({
      extract <- linked_extraction_data()
      if (is.null(extract) || nrow(extract) == 0 || !"study" %in% names(extract)) return(plotly::plotly_empty())

      qual_col <- intersect(c("extract_quality", "quality", "qualite"), names(extract))[1]
      if (is.na(qual_col)) return(plotly::plotly_empty())

      qual_data <- extract %>%
        dplyr::filter(!is.na(study), study %in% c("DA", "DP"), !is.na(.data[[qual_col]])) %>%
        dplyr::count(study, quality = .data[[qual_col]]) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      if (nrow(qual_data) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(qual_data, x = ~quality, y = ~pct, color = ~study,
                      colors = study_colors, type = "bar",
                      hovertemplate = "Quality: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Extract Quality"),
          yaxis = list(title = "Percentage (%)"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$transport_stats_table <- DT::renderDT({
      stats_list <- list()

      # Transport time
      data <- study_data()$all
      time_col <- intersect(c("transport_time", "transport_days", "days_to_extraction"), names(data))[1]
      if (!is.na(time_col)) {
        da_times <- data[[time_col]][data$study == "DA" & !is.na(data[[time_col]])]
        dp_times <- data[[time_col]][data$study == "DP" & !is.na(data[[time_col]])]

        if (length(da_times) > 0 && length(dp_times) > 0) {
          wilcox_test <- tryCatch(wilcox.test(da_times, dp_times), error = function(e) NULL)
          stats_list[[length(stats_list) + 1]] <- data.frame(
            Variable = "Transport Time (days)",
            DA_Value = sprintf("%.1f (%.1f-%.1f)", median(da_times), quantile(da_times, 0.25), quantile(da_times, 0.75)),
            DP_Value = sprintf("%.1f (%.1f-%.1f)", median(dp_times), quantile(dp_times, 0.25), quantile(dp_times, 0.75)),
            Test = "Mann-Whitney U",
            P_Value = if (!is.null(wilcox_test)) sprintf("%.4f", wilcox_test$p.value) else "N/A",
            stringsAsFactors = FALSE
          )
        }
      }

      # DRS Volume
      extract <- linked_extraction_data()
      if (!is.null(extract) && "study" %in% names(extract)) {
        vol_col <- intersect(c("drs_volume_ml", "volume_ml", "volume"), names(extract))[1]
        if (!is.na(vol_col)) {
          da_vols <- extract[[vol_col]][extract$study == "DA" & !is.na(extract[[vol_col]])]
          dp_vols <- extract[[vol_col]][extract$study == "DP" & !is.na(extract[[vol_col]])]

          if (length(da_vols) > 0 && length(dp_vols) > 0) {
            wilcox_test <- tryCatch(wilcox.test(da_vols, dp_vols), error = function(e) NULL)
            stats_list[[length(stats_list) + 1]] <- data.frame(
              Variable = "DRS Volume (mL)",
              DA_Value = sprintf("%.2f (%.2f-%.2f)", median(da_vols), quantile(da_vols, 0.25), quantile(da_vols, 0.75)),
              DP_Value = sprintf("%.2f (%.2f-%.2f)", median(dp_vols), quantile(dp_vols, 0.25), quantile(dp_vols, 0.75)),
              Test = "Mann-Whitney U",
              P_Value = if (!is.null(wilcox_test)) sprintf("%.4f", wilcox_test$p.value) else "N/A",
              stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(stats_list) == 0) {
        return(DT::datatable(data.frame(Message = "No transport/extraction data")))
      }

      stats_df <- do.call(rbind, stats_list)

      DT::datatable(
        stats_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', scrollX = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatStyle("P_Value",
          backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa"))
        )
    })

    # ========================================================================
    # SUMMARY TABLES
    # ========================================================================

    output$full_summary_table <- DT::renderDT({
      data <- study_data()

      summary_df <- data.frame(
        Category = c("Total Samples", "Active Screening (DA)", "Passive Screening (DP)"),
        Count = c(nrow(data$all), nrow(data$da), nrow(data$dp)),
        Percentage = c("100%",
                      sprintf("%.1f%%", nrow(data$da) / max(nrow(data$all), 1) * 100),
                      sprintf("%.1f%%", nrow(data$dp) / max(nrow(data$all), 1) * 100)),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        summary_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't'),
        class = "table-sm"
      )
    })

    output$statistical_tests_table <- DT::renderDT({
      # Compile all statistical tests
      all_tests <- list()

      data <- study_data()$all

      # Demographics tests
      if ("age" %in% names(data)) {
        da_ages <- data$age[data$study == "DA" & !is.na(data$age)]
        dp_ages <- data$age[data$study == "DP" & !is.na(data$age)]
        if (length(da_ages) > 0 && length(dp_ages) > 0) {
          test <- tryCatch(wilcox.test(da_ages, dp_ages), error = function(e) NULL)
          all_tests[[length(all_tests) + 1]] <- data.frame(
            Category = "Demographics",
            Variable = "Age",
            Test_Type = "Mann-Whitney U",
            Statistic = if (!is.null(test)) sprintf("W = %.0f", test$statistic) else "N/A",
            P_Value = if (!is.null(test)) test$p.value else NA,
            Significant = if (!is.null(test) && test$p.value < 0.05) "Yes" else "No",
            stringsAsFactors = FALSE
          )
        }
      }

      if ("sex" %in% names(data)) {
        tbl <- table(data$study, data$sex)
        if (all(dim(tbl) >= 2)) {
          test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          all_tests[[length(all_tests) + 1]] <- data.frame(
            Category = "Demographics",
            Variable = "Sex",
            Test_Type = "Chi-squared",
            Statistic = if (!is.null(test)) sprintf("X² = %.2f", test$statistic) else "N/A",
            P_Value = if (!is.null(test)) test$p.value else NA,
            Significant = if (!is.null(test) && test$p.value < 0.05) "Yes" else "No",
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(all_tests) == 0) {
        return(DT::datatable(data.frame(Message = "Insufficient data for statistical tests")))
      }

      tests_df <- do.call(rbind, all_tests)
      tests_df$P_Value <- sprintf("%.4f", tests_df$P_Value)

      DT::datatable(
        tests_df,
        rownames = FALSE,
        options = list(pageLength = 20, scrollX = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatStyle("Significant",
          backgroundColor = DT::styleEqual(c("Yes", "No"), c("#d4edda", "#f8f9fa"))
        )
    })

  })
}
