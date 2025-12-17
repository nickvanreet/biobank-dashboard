# R/modules/mod_study_comparison.R
# Study Results Comparison Module - Compare DA (Active) vs DP (Passive) Screening
# ============================================================================
# Enhanced version with:
# - Filter controls for invalid/borderline results
# - Test Methods documentation tab
# - Detailed MIC Cq values (177T, 18S2, RNAseP-DNA, RNAseP-RNA)
# - ELISA OD values alongside PP%
# - iELISA LiTat 1.3 and 1.5 separate analysis
# - Enhanced geographic statistical tests
# - Clarified significance metrics with effect sizes
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
      # Header with Filter Controls
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

          # Filter controls row
          layout_columns(
            col_widths = c(4, 4, 4), gap = "12px",

            div(
              class = "d-flex align-items-center gap-2",
              tags$label("Result Filters:", class = "fw-bold"),
              checkboxInput(ns("exclude_invalid"), "Exclude Invalid Results", value = TRUE)
            ),
            div(
              class = "d-flex align-items-center gap-2",
              checkboxInput(ns("include_borderline"), "Include Borderline as Positive", value = FALSE)
            ),
            div(
              class = "text-muted small",
              textOutput(ns("filter_status"))
            )
          ),

          p(class = "text-muted mb-0 small mt-2",
            "Statistical tests: Chi-squared for categorical, Mann-Whitney U for continuous, Fisher's exact for small samples. Effect sizes: Cramér's V, Odds Ratio with 95% CI.")
        )
      ),

      # KPI Summary Row 1 - Sample counts and MIC
      layout_column_wrap(
        width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "8px",

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
          title = "DA MIC+",
          value = textOutput(ns("kpi_da_positivity")),
          showcase = icon("dna"),
          theme = "warning",
          p(class = "small text-muted mb-0", textOutput(ns("kpi_da_mic_detail"), inline = TRUE))
        ),
        value_box(
          title = "DP MIC+",
          value = textOutput(ns("kpi_dp_positivity")),
          showcase = icon("dna"),
          theme = "danger",
          p(class = "small text-muted mb-0", textOutput(ns("kpi_dp_mic_detail"), inline = TRUE))
        )
      ),

      # KPI Summary Row 2 - ELISA and iELISA
      layout_column_wrap(
        width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "8px",

        value_box(
          title = "DA ELISA+",
          value = textOutput(ns("kpi_da_elisa")),
          showcase = icon("flask"),
          theme = "primary",
          p(class = "small text-muted mb-0", "PE/VSG positive")
        ),
        value_box(
          title = "DP ELISA+",
          value = textOutput(ns("kpi_dp_elisa")),
          showcase = icon("flask"),
          theme = "secondary",
          p(class = "small text-muted mb-0", "PE/VSG positive")
        ),
        value_box(
          title = "DA iELISA+",
          value = textOutput(ns("kpi_da_ielisa")),
          showcase = icon("vial"),
          theme = "info",
          p(class = "small text-muted mb-0", "L13/L15 positive")
        ),
        value_box(
          title = "DP iELISA+",
          value = textOutput(ns("kpi_dp_ielisa")),
          showcase = icon("vial"),
          theme = "light",
          p(class = "small text-muted mb-0", "L13/L15 positive")
        )
      ),

      # Main tabbed content
      navset_card_tab(
        id = ns("comparison_tabs"),

        # ==== TAB 0: Overview ====
        nav_panel(
          title = "Overview",
          icon = icon("home"),

          # Quick summary cards
          layout_column_wrap(
            width = 1/3, gap = "12px",

            card(
              card_header(class = "bg-primary text-white", "Sample Overview"),
              card_body(
                DT::DTOutput(ns("overview_samples_table"))
              )
            ),
            card(
              card_header(class = "bg-success text-white", "Test Positivity Overview"),
              card_body(
                DT::DTOutput(ns("overview_positivity_table"))
              )
            ),
            card(
              card_header(class = "bg-info text-white", "Key Findings"),
              card_body(
                uiOutput(ns("overview_key_findings"))
              )
            )
          ),

          # Detailed breakdown
          card(
            card_header("Detailed Test Results by Study Type"),
            card_body(
              DT::DTOutput(ns("overview_detailed_table"))
            )
          )
        ),

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

          # NEW: Geographic positivity comparison
          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("Positivity Rate by Province"),
              card_body_fill(
                plotly::plotlyOutput(ns("province_positivity_plot"), height = "400px")
              )
            ),
            card(
              card_header("Positivity Rate by Health Zone (Top 10)"),
              card_body_fill(
                plotly::plotlyOutput(ns("zone_positivity_plot"), height = "400px")
              )
            )
          ),

          card(
            card_header("Geographic Distribution Summary"),
            card_body(
              DT::DTOutput(ns("geographic_stats_table"))
            )
          ),

          # NEW: Geographic statistical tests
          card(
            card_header("Geographic Statistical Tests"),
            card_body(
              DT::DTOutput(ns("geographic_tests_table"))
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

        # ==== TAB 3b: MIC Detailed ====
        nav_panel(
          title = "MIC Details",
          icon = icon("dna"),

          card(
            card_header("MIC qPCR Target Analysis: Cq Values by Study Type"),
            card_body(
              p(class = "text-muted", "Comparison of Cq (quantification cycle) values for each MIC target between DA and DP samples. Lower Cq = higher parasite load.")
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("MIC-177T (Trypanozoon DNA)"),
              card_body_fill(
                plotly::plotlyOutput(ns("mic_177t_plot"), height = "350px")
              )
            ),
            card(
              card_header("MIC-18S2 (18S rRNA gene)"),
              card_body_fill(
                plotly::plotlyOutput(ns("mic_18s2_plot"), height = "350px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("MIC-RNAseP-DNA (Human DNA control)"),
              card_body_fill(
                plotly::plotlyOutput(ns("mic_rnasep_dna_plot"), height = "350px")
              )
            ),
            card(
              card_header("MIC-RNAseP-RNA (Human RNA control)"),
              card_body_fill(
                plotly::plotlyOutput(ns("mic_rnasep_rna_plot"), height = "350px")
              )
            )
          ),

          card(
            card_header("MIC Cq Values Statistical Summary"),
            card_body(
              DT::DTOutput(ns("mic_cq_stats_table"))
            )
          )
        ),

        # ==== TAB 3c: ELISA Detailed ====
        nav_panel(
          title = "ELISA Details",
          icon = icon("flask"),

          card(
            card_header("ELISA Detailed Analysis: PP% and OD Values"),
            card_body(
              p(class = "text-muted", "Comparison of PP% (Percent Positivity) and raw OD (Optical Density) values between DA and DP samples.")
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("ELISA-PE: PP% Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_pe_pp_plot"), height = "350px")
              )
            ),
            card(
              card_header("ELISA-PE: DOD (Delta OD) Values"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_pe_od_plot"), height = "350px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("ELISA-VSG: PP% Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_vsg_pp_plot"), height = "350px")
              )
            ),
            card(
              card_header("ELISA-VSG: DOD (Delta OD) Values"),
              card_body_fill(
                plotly::plotlyOutput(ns("elisa_vsg_od_plot"), height = "350px")
              )
            )
          ),

          card(
            card_header("ELISA Detailed Statistics"),
            card_body(
              DT::DTOutput(ns("elisa_detailed_stats_table"))
            )
          )
        ),

        # ==== TAB 3d: iELISA Detailed ====
        nav_panel(
          title = "iELISA Details",
          icon = icon("vial"),

          card(
            card_header("iELISA Antigen-Specific Analysis"),
            card_body(
              p(class = "text-muted", "Comparison of LiTat 1.3 and LiTat 1.5 antigen results between DA and DP samples.")
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("LiTat 1.3 Positivity"),
              card_body_fill(
                plotly::plotlyOutput(ns("ielisa_l13_positivity_plot"), height = "350px")
              )
            ),
            card(
              card_header("LiTat 1.5 Positivity"),
              card_body_fill(
                plotly::plotlyOutput(ns("ielisa_l15_positivity_plot"), height = "350px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              card_header("LiTat 1.3 % Inhibition"),
              card_body_fill(
                plotly::plotlyOutput(ns("ielisa_l13_inhibition_plot"), height = "350px")
              )
            ),
            card(
              card_header("LiTat 1.5 % Inhibition"),
              card_body_fill(
                plotly::plotlyOutput(ns("ielisa_l15_inhibition_plot"), height = "350px")
              )
            )
          ),

          card(
            card_header("iELISA Antigen-Specific Statistics"),
            card_body(
              DT::DTOutput(ns("ielisa_detailed_stats_table"))
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
            card_header("Statistical Tests Summary with Effect Sizes"),
            card_body(
              DT::DTOutput(ns("statistical_tests_table"))
            )
          )
        ),

        # ==== TAB 6: Test Methods ====
        nav_panel(
          title = "Test Methods",
          icon = icon("book"),

          card(
            card_header(class = "bg-dark text-white", "How Tests Are Calculated"),
            card_body(
              h4("Overview"),
              p("This module compares results between Active Screening (DA) and Passive Screening (DP) using appropriate statistical tests based on data type and sample size."),
              hr(),

              h4("Data Filtering"),
              tags$ul(
                tags$li(tags$strong("Invalid Results:"), " Results with FinalCall = 'Invalid', 'Failed', 'RunInvalid' are excluded by default. Toggle 'Exclude Invalid Results' to include them."),
                tags$li(tags$strong("Borderline Results:"), " Results with FinalCall = 'Indeterminate', 'Inconclusive', 'Review', 'Retest' are treated as negative by default. Toggle 'Include Borderline as Positive' to count them as positive.")
              ),
              hr(),

              h4("Statistical Tests Used"),

              h5("1. Chi-squared Test (χ²)"),
              p("Used for: Comparing categorical variables (sex, province, health zone, test positivity)"),
              tags$ul(
                tags$li("Tests whether the distribution of a categorical variable differs between DA and DP"),
                tags$li("Requires expected cell counts ≥ 5"),
                tags$li("Reports: χ² statistic, degrees of freedom, p-value")
              ),

              h5("2. Fisher's Exact Test"),
              p("Used for: Categorical comparisons when sample sizes are small (expected counts < 5)"),
              tags$ul(
                tags$li("More accurate than chi-squared for small samples"),
                tags$li("Reports: Odds Ratio, 95% CI, p-value")
              ),

              h5("3. Mann-Whitney U Test (Wilcoxon rank-sum)"),
              p("Used for: Comparing continuous variables (age, Cq values, PP%, % inhibition, transport time)"),
              tags$ul(
                tags$li("Non-parametric test - does not assume normal distribution"),
                tags$li("Compares medians between groups"),
                tags$li("Reports: U statistic, p-value, median (IQR) for each group")
              ),
              hr(),

              h4("Effect Size Measures"),

              h5("1. Cramér's V"),
              p("For categorical variables:"),
              tags$ul(
                tags$li("Range: 0 to 1"),
                tags$li("0.1 = small effect, 0.3 = medium effect, 0.5 = large effect")
              ),

              h5("2. Odds Ratio (OR)"),
              p("For binary outcomes (positive/negative):"),
              tags$ul(
                tags$li("OR = 1: No difference between DA and DP"),
                tags$li("OR > 1: Higher odds in DA"),
                tags$li("OR < 1: Higher odds in DP"),
                tags$li("95% CI not crossing 1 indicates statistical significance")
              ),

              h5("3. Cliff's Delta"),
              p("For continuous variables:"),
              tags$ul(
                tags$li("Range: -1 to 1"),
                tags$li("|d| < 0.147 = negligible, |d| < 0.33 = small, |d| < 0.474 = medium, |d| ≥ 0.474 = large")
              ),
              hr(),

              h4("Test-Specific Calculations"),

              h5("MIC qPCR"),
              tags$ul(
                tags$li(tags$strong("Positive:"), " FinalCall in ('Positive', 'Positive_DNA', 'Positive_RNA', 'LatePositive')"),
                tags$li(tags$strong("Targets:"), " 177T (Trypanozoon DNA), 18S2 (18S rRNA), RNAseP-DNA (human DNA control), RNAseP-RNA (human RNA control)"),
                tags$li(tags$strong("Cq values:"), " Lower Cq = higher parasite/template concentration")
              ),

              h5("ELISA (PE and VSG)"),
              tags$ul(
                tags$li(tags$strong("PP%:"), " (Sample OD / Positive Control OD) × 100"),
                tags$li(tags$strong("Positive:"), " PP% ≥ cutoff threshold (typically 15-20%)"),
                tags$li(tags$strong("OD:"), " Raw optical density at 450nm")
              ),

              h5("iELISA (Inhibition ELISA)"),
              tags$ul(
                tags$li(tags$strong("Antigens:"), " LiTat 1.3 and LiTat 1.5 (T.b. gambiense VSGs)"),
                tags$li(tags$strong("% Inhibition:"), " 100 × (1 - Sample OD / Negative Control OD)"),
                tags$li(tags$strong("Positive:"), " % Inhibition ≥ threshold (typically 50%)"),
                tags$li(tags$strong("Borderline:"), " % Inhibition between borderline and positive threshold")
              ),
              hr(),

              h4("P-value Interpretation"),
              tags$ul(
                tags$li(tags$span(class = "badge bg-success", "p < 0.001"), " Highly significant difference"),
                tags$li(tags$span(class = "badge bg-warning text-dark", "p < 0.05"), " Significant difference"),
                tags$li(tags$span(class = "badge bg-secondary", "p ≥ 0.05"), " No significant difference")
              ),
              p(class = "text-muted small", "Note: Multiple comparisons may inflate Type I error. Consider Bonferroni correction for formal analysis.")
            )
          )
        )
      )
    )
  )
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Normalize barcode for matching (local to this module)
.normalize_barcode_for_study <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  x <- trimws(tolower(as.character(x)))
  x <- gsub("^kps[-_]?", "", x)
  x <- gsub("[^a-z0-9]", "", x)
  if (x == "") NA_character_ else x
}

#' Calculate Cramér's V effect size for chi-squared test
.cramers_v <- function(chi_result, n) {
  if (is.null(chi_result) || is.na(n) || n == 0) return(NA_real_)
  k <- min(chi_result$parameter + 1, 2)  # For 2x2 or 2xk tables
  v <- sqrt(chi_result$statistic / (n * (k - 1)))
  as.numeric(v)
}

#' Calculate Odds Ratio with 95% CI
.calc_odds_ratio <- function(a, b, c, d) {
  # a = DA positive, b = DA negative, c = DP positive, d = DP negative
  # Add 0.5 continuity correction if any cell is 0
  if (any(c(a, b, c, d) == 0)) {
    a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5
  }
  or <- (a * d) / (b * c)
  se_log_or <- sqrt(1/a + 1/b + 1/c + 1/d)
  ci_lower <- exp(log(or) - 1.96 * se_log_or)
  ci_upper <- exp(log(or) + 1.96 * se_log_or)
  list(or = or, ci_lower = ci_lower, ci_upper = ci_upper)
}

#' Calculate Cliff's Delta effect size for continuous variables
.cliffs_delta <- function(x, y) {
  if (length(x) == 0 || length(y) == 0) return(NA_real_)
  n_x <- length(x)
  n_y <- length(y)
  # Count dominance
  d <- 0
  for (xi in x) {
    d <- d + sum(xi > y) - sum(xi < y)
  }
  d / (n_x * n_y)
}

#' Interpret effect size magnitude
.interpret_effect <- function(value, type = "cramers_v") {
  if (is.na(value)) return("N/A")
  if (type == "cramers_v") {
    if (value < 0.1) return("Negligible")
    if (value < 0.3) return("Small")
    if (value < 0.5) return("Medium")
    return("Large")
  } else if (type == "cliffs_delta") {
    value <- abs(value)
    if (value < 0.147) return("Negligible")
    if (value < 0.33) return("Small")
    if (value < 0.474) return("Medium")
    return("Large")
  }
  "N/A"
}

#' Determine if a MIC result is positive based on filter settings
.is_mic_positive <- function(final_call, include_borderline = FALSE) {
  positive_values <- c("Positive", "Positive_DNA", "Positive_RNA", "LatePositive")
  borderline_values <- c("Indeterminate", "Inconclusive", "Review", "Retest")

  if (include_borderline) {
    return(final_call %in% c(positive_values, borderline_values))
  }
  final_call %in% positive_values
}

#' Determine if an ELISA result is positive based on filter settings
#' @param sample_positive Logical vector of positivity
#' @param sample_borderline Logical vector of borderline status (optional)
#' @param include_borderline Whether to treat borderline as positive
.is_elisa_positive <- function(sample_positive, sample_borderline = NULL, include_borderline = FALSE) {
  result <- sample_positive == TRUE
  if (include_borderline && !is.null(sample_borderline)) {
    result <- result | (sample_borderline == TRUE)
  }
  result
}

#' Determine if an iELISA L13 result is positive based on filter settings
#' @param positive_L13 Logical vector of L13 positivity
#' @param status_L13 Character vector of L13 status (optional)
#' @param include_borderline Whether to treat borderline as positive
.is_ielisa_L13_positive <- function(positive_L13, status_L13 = NULL, include_borderline = FALSE) {
  result <- positive_L13 == TRUE
  if (include_borderline && !is.null(status_L13)) {
    result <- result | (status_L13 == "Borderline")
  }
  result
}

#' Determine if an iELISA L15 result is positive based on filter settings
#' @param positive_L15 Logical vector of L15 positivity
#' @param status_L15 Character vector of L15 status (optional)
#' @param include_borderline Whether to treat borderline as positive
.is_ielisa_L15_positive <- function(positive_L15, status_L15 = NULL, include_borderline = FALSE) {
  result <- positive_L15 == TRUE
  if (include_borderline && !is.null(status_L15)) {
    result <- result | (status_L15 == "Borderline")
  }
  result
}

#' Filter out invalid results
.filter_invalid <- function(data, exclude_invalid = TRUE, final_call_col = "FinalCall") {

  if (!exclude_invalid || !final_call_col %in% names(data)) return(data)
  invalid_values <- c("Invalid", "Failed", "RunInvalid")
  data %>% dplyr::filter(!.data[[final_call_col]] %in% invalid_values)
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
    # FILTER STATUS OUTPUT
    # ========================================================================

    output$filter_status <- renderText({
      invalid_status <- if (input$exclude_invalid) "Invalid excluded" else "Invalid included"
      borderline_status <- if (input$include_borderline) "Borderline = Positive" else "Borderline = Negative"
      paste(invalid_status, "|", borderline_status)
    })

    # ========================================================================
    # REACTIVE DATA PREPARATION
    # ========================================================================

    # Get biobank data with study info
    biobank_data <- reactive({
      data <- biobank_df()
      if (is.null(data) || nrow(data) == 0) return(NULL)
      if (!"study" %in% names(data)) return(NULL)
      data %>% dplyr::filter(study %in% c("DA", "DP"))
    })

    # Create lookup table: lab_number -> study
    lab_number_lookup <- reactive({
      biobank <- biobank_data()
      if (is.null(biobank)) return(NULL)

      # Find lab number column
      lab_col <- NULL
      for (col in c("numero_labo", "lab_id", "numero")) {
        if (col %in% names(biobank)) {
          lab_col <- col
          break
        }
      }
      if (is.null(lab_col)) return(NULL)

      biobank %>%
        dplyr::select(lab_number = !!rlang::sym(lab_col), study) %>%
        dplyr::filter(!is.na(lab_number), lab_number != "") %>%
        dplyr::mutate(lab_number_norm = sapply(lab_number, function(x) trimws(tolower(as.character(x))))) %>%
        dplyr::select(lab_number_norm, study) %>%
        dplyr::distinct(lab_number_norm, .keep_all = TRUE)
    })

    # Create lookup table: barcode -> study
    barcode_lookup <- reactive({
      biobank <- biobank_data()
      if (is.null(biobank)) return(NULL)

      # Find barcode column
      barcode_col <- NULL
      for (col in c("barcode", "code_barres_kps")) {
        if (col %in% names(biobank)) {
          barcode_col <- col
          break
        }
      }
      if (is.null(barcode_col)) return(NULL)

      biobank %>%
        dplyr::select(barcode = !!rlang::sym(barcode_col), study) %>%
        dplyr::filter(!is.na(barcode), barcode != "") %>%
        dplyr::mutate(barcode_norm = sapply(barcode, .normalize_barcode_for_study)) %>%
        dplyr::select(barcode_norm, study) %>%
        dplyr::distinct(barcode_norm, .keep_all = TRUE)
    })

    # Link MIC data with study type (MIC uses lab numbers, not barcodes)
    # Apply invalid/borderline filters
    linked_mic_data <- reactive({
      mic <- mic_df()
      lookup <- lab_number_lookup()

      if (is.null(mic) || nrow(mic) == 0) return(NULL)
      if (is.null(lookup) || nrow(lookup) == 0) return(NULL)

      # Find sample ID column in MIC data
      sample_col <- NULL
      for (col in c("SampleID", "SampleName", "sample_id", "Name")) {
        if (col %in% names(mic)) {
          sample_col <- col
          break
        }
      }
      if (is.null(sample_col)) return(NULL)

      result <- mic %>%
        dplyr::mutate(lab_number_norm = sapply(.data[[sample_col]], function(x) trimws(tolower(as.character(x))))) %>%
        dplyr::left_join(lookup, by = "lab_number_norm") %>%
        dplyr::filter(!is.na(study))

      # Apply invalid filter
      if (input$exclude_invalid && "FinalCall" %in% names(result)) {
        invalid_values <- c("Invalid", "Failed", "RunInvalid")
        result <- result %>% dplyr::filter(!FinalCall %in% invalid_values)
      }

      result
    })

    # Link ELISA-PE data with study type
    linked_elisa_pe_data <- reactive({
      elisa <- elisa_pe_df()
      lookup <- barcode_lookup()

      if (is.null(elisa) || nrow(elisa) == 0) return(NULL)

      # Check if study column already exists
      if ("study" %in% names(elisa) && any(!is.na(elisa$study))) {
        return(elisa %>% dplyr::filter(study %in% c("DA", "DP")))
      }

      if (is.null(lookup) || nrow(lookup) == 0) return(NULL)

      # Find barcode column
      barcode_col <- NULL
      for (col in c("code_barres_kps", "barcode", "sample_id")) {
        if (col %in% names(elisa)) {
          barcode_col <- col
          break
        }
      }
      if (is.null(barcode_col)) return(NULL)

      elisa %>%
        dplyr::mutate(barcode_norm = sapply(.data[[barcode_col]], .normalize_barcode_for_study)) %>%
        dplyr::left_join(lookup, by = "barcode_norm") %>%
        dplyr::filter(!is.na(study))
    })

    # Link ELISA-VSG data with study type
    linked_elisa_vsg_data <- reactive({
      elisa <- elisa_vsg_df()
      lookup <- barcode_lookup()

      if (is.null(elisa) || nrow(elisa) == 0) return(NULL)

      if ("study" %in% names(elisa) && any(!is.na(elisa$study))) {
        return(elisa %>% dplyr::filter(study %in% c("DA", "DP")))
      }

      if (is.null(lookup) || nrow(lookup) == 0) return(NULL)

      barcode_col <- NULL
      for (col in c("code_barres_kps", "barcode", "sample_id")) {
        if (col %in% names(elisa)) {
          barcode_col <- col
          break
        }
      }
      if (is.null(barcode_col)) return(NULL)

      elisa %>%
        dplyr::mutate(barcode_norm = sapply(.data[[barcode_col]], .normalize_barcode_for_study)) %>%
        dplyr::left_join(lookup, by = "barcode_norm") %>%
        dplyr::filter(!is.na(study))
    })

    # Link iELISA data with study type
    linked_ielisa_data <- reactive({
      ielisa <- ielisa_df()
      lookup <- barcode_lookup()

      if (is.null(ielisa) || nrow(ielisa) == 0) return(NULL)

      if ("study" %in% names(ielisa) && any(!is.na(ielisa$study))) {
        return(ielisa %>% dplyr::filter(study %in% c("DA", "DP")))
      }

      if (is.null(lookup) || nrow(lookup) == 0) return(NULL)

      barcode_col <- NULL
      for (col in c("code_barres_kps", "barcode", "sample_id")) {
        if (col %in% names(ielisa)) {
          barcode_col <- col
          break
        }
      }
      if (is.null(barcode_col)) return(NULL)

      ielisa %>%
        dplyr::mutate(barcode_norm = sapply(.data[[barcode_col]], .normalize_barcode_for_study)) %>%
        dplyr::left_join(lookup, by = "barcode_norm") %>%
        dplyr::filter(!is.na(study))
    })

    # Link extraction data with study type
    linked_extraction_data <- reactive({
      extract <- extraction_df()
      lookup <- barcode_lookup()

      if (is.null(extract) || nrow(extract) == 0) return(NULL)

      if ("study" %in% names(extract) && any(!is.na(extract$study))) {
        return(extract %>% dplyr::filter(study %in% c("DA", "DP")))
      }

      if (is.null(lookup) || nrow(lookup) == 0) return(NULL)

      barcode_col <- NULL
      for (col in c("barcode", "sample_id", "code_barres_kps")) {
        if (col %in% names(extract)) {
          barcode_col <- col
          break
        }
      }
      if (is.null(barcode_col)) return(NULL)

      extract %>%
        dplyr::mutate(barcode_norm = sapply(.data[[barcode_col]], .normalize_barcode_for_study)) %>%
        dplyr::left_join(lookup, by = "barcode_norm") %>%
        dplyr::filter(!is.na(study))
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_da_count <- renderText({
      data <- biobank_data()
      if (is.null(data)) return("0")
      scales::comma(sum(data$study == "DA", na.rm = TRUE))
    })

    output$kpi_dp_count <- renderText({
      data <- biobank_data()
      if (is.null(data)) return("0")
      scales::comma(sum(data$study == "DP", na.rm = TRUE))
    })

    output$kpi_da_positivity <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0) return("N/A")

      da_mic <- mic %>% dplyr::filter(study == "DA")
      if (nrow(da_mic) == 0) return("N/A")

      # Find positivity - check FinalCall column first, respecting borderline setting
      if ("FinalCall" %in% names(da_mic)) {
        pos_rate <- mean(.is_mic_positive(da_mic$FinalCall, input$include_borderline), na.rm = TRUE) * 100
      } else {
        pos_col <- intersect(c("positive", "is_positive", "overall_positive"), names(da_mic))[1]
        if (is.na(pos_col)) return("N/A")
        pos_rate <- mean(da_mic[[pos_col]] == TRUE, na.rm = TRUE) * 100
      }
      sprintf("%.1f%%", pos_rate)
    })

    output$kpi_dp_positivity <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0) return("N/A")

      dp_mic <- mic %>% dplyr::filter(study == "DP")
      if (nrow(dp_mic) == 0) return("N/A")

      if ("FinalCall" %in% names(dp_mic)) {
        pos_rate <- mean(.is_mic_positive(dp_mic$FinalCall, input$include_borderline), na.rm = TRUE) * 100
      } else {
        pos_col <- intersect(c("positive", "is_positive", "overall_positive"), names(dp_mic))[1]
        if (is.na(pos_col)) return("N/A")
        pos_rate <- mean(dp_mic[[pos_col]] == TRUE, na.rm = TRUE) * 100
      }
      sprintf("%.1f%%", pos_rate)
    })

    # Significance KPI - Now for MIC positivity comparison (not sex)
    output$kpi_overall_pvalue <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"FinalCall" %in% names(mic)) return("N/A")

      tryCatch({
        is_positive <- .is_mic_positive(mic$FinalCall, input$include_borderline)
        tbl <- table(mic$study, is_positive)
        if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
          test <- chisq.test(tbl)
          if (test$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", test$p.value)
        } else {
          "N/A"
        }
      }, error = function(e) "N/A")
    })

    # MIC detail - DNA vs RNA breakdown for DA
    output$kpi_da_mic_detail <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"FinalCall" %in% names(mic)) return("")

      da_mic <- mic %>% dplyr::filter(study == "DA")
      if (nrow(da_mic) == 0) return("")

      dna_pos <- sum(da_mic$FinalCall %in% c("Positive_DNA", "Positive"), na.rm = TRUE)
      rna_pos <- sum(da_mic$FinalCall %in% c("Positive_RNA", "LatePositive"), na.rm = TRUE)
      sprintf("DNA:%d RNA:%d", dna_pos, rna_pos)
    })

    # MIC detail - DNA vs RNA breakdown for DP
    output$kpi_dp_mic_detail <- renderText({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0 || !"FinalCall" %in% names(mic)) return("")

      dp_mic <- mic %>% dplyr::filter(study == "DP")
      if (nrow(dp_mic) == 0) return("")

      dna_pos <- sum(dp_mic$FinalCall %in% c("Positive_DNA", "Positive"), na.rm = TRUE)
      rna_pos <- sum(dp_mic$FinalCall %in% c("Positive_RNA", "LatePositive"), na.rm = TRUE)
      sprintf("DNA:%d RNA:%d", dna_pos, rna_pos)
    })

    # ELISA KPIs (combined PE and VSG) - with borderline handling
    output$kpi_da_elisa <- renderText({
      pe <- linked_elisa_pe_data()
      vsg <- linked_elisa_vsg_data()

      da_pos <- 0
      da_total <- 0

      if (!is.null(pe) && nrow(pe) > 0) {
        da_pe <- pe %>% dplyr::filter(study == "DA")
        if (nrow(da_pe) > 0 && "sample_positive" %in% names(da_pe)) {
          borderline_col <- if ("sample_borderline" %in% names(da_pe)) da_pe$sample_borderline else NULL
          is_pos <- .is_elisa_positive(da_pe$sample_positive, borderline_col, input$include_borderline)
          da_pos <- da_pos + sum(is_pos, na.rm = TRUE)
          da_total <- da_total + nrow(da_pe)
        }
      }

      if (!is.null(vsg) && nrow(vsg) > 0) {
        da_vsg <- vsg %>% dplyr::filter(study == "DA")
        if (nrow(da_vsg) > 0 && "sample_positive" %in% names(da_vsg)) {
          borderline_col <- if ("sample_borderline" %in% names(da_vsg)) da_vsg$sample_borderline else NULL
          is_pos <- .is_elisa_positive(da_vsg$sample_positive, borderline_col, input$include_borderline)
          da_pos <- da_pos + sum(is_pos, na.rm = TRUE)
          da_total <- da_total + nrow(da_vsg)
        }
      }

      if (da_total == 0) return("N/A")
      sprintf("%.1f%%", da_pos / da_total * 100)
    })

    output$kpi_dp_elisa <- renderText({
      pe <- linked_elisa_pe_data()
      vsg <- linked_elisa_vsg_data()

      dp_pos <- 0
      dp_total <- 0

      if (!is.null(pe) && nrow(pe) > 0) {
        dp_pe <- pe %>% dplyr::filter(study == "DP")
        if (nrow(dp_pe) > 0 && "sample_positive" %in% names(dp_pe)) {
          borderline_col <- if ("sample_borderline" %in% names(dp_pe)) dp_pe$sample_borderline else NULL
          is_pos <- .is_elisa_positive(dp_pe$sample_positive, borderline_col, input$include_borderline)
          dp_pos <- dp_pos + sum(is_pos, na.rm = TRUE)
          dp_total <- dp_total + nrow(dp_pe)
        }
      }

      if (!is.null(vsg) && nrow(vsg) > 0) {
        dp_vsg <- vsg %>% dplyr::filter(study == "DP")
        if (nrow(dp_vsg) > 0 && "sample_positive" %in% names(dp_vsg)) {
          borderline_col <- if ("sample_borderline" %in% names(dp_vsg)) dp_vsg$sample_borderline else NULL
          is_pos <- .is_elisa_positive(dp_vsg$sample_positive, borderline_col, input$include_borderline)
          dp_pos <- dp_pos + sum(is_pos, na.rm = TRUE)
          dp_total <- dp_total + nrow(dp_vsg)
        }
      }

      if (dp_total == 0) return("N/A")
      sprintf("%.1f%%", dp_pos / dp_total * 100)
    })

    # iELISA KPIs - with borderline handling
    output$kpi_da_ielisa <- renderText({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0) return("N/A")

      da_ielisa <- ielisa %>% dplyr::filter(study == "DA")
      if (nrow(da_ielisa) == 0) return("N/A")

      # Use L13 or L15 with borderline handling
      da_pos <- 0
      da_n <- nrow(da_ielisa)
      if ("positive_L13" %in% names(da_ielisa)) {
        status_col <- if ("status_L13" %in% names(da_ielisa)) da_ielisa$status_L13 else NULL
        is_pos <- .is_ielisa_L13_positive(da_ielisa$positive_L13, status_col, input$include_borderline)
        da_pos <- max(da_pos, sum(is_pos, na.rm = TRUE))
      }
      if ("positive_L15" %in% names(da_ielisa)) {
        status_col <- if ("status_L15" %in% names(da_ielisa)) da_ielisa$status_L15 else NULL
        is_pos <- .is_ielisa_L15_positive(da_ielisa$positive_L15, status_col, input$include_borderline)
        da_pos <- max(da_pos, sum(is_pos, na.rm = TRUE))
      }

      if (da_n == 0) return("N/A")
      sprintf("%.1f%%", da_pos / da_n * 100)
    })

    output$kpi_dp_ielisa <- renderText({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0) return("N/A")

      dp_ielisa <- ielisa %>% dplyr::filter(study == "DP")
      if (nrow(dp_ielisa) == 0) return("N/A")

      # Use L13 or L15 with borderline handling
      dp_pos <- 0
      dp_n <- nrow(dp_ielisa)
      if ("positive_L13" %in% names(dp_ielisa)) {
        status_col <- if ("status_L13" %in% names(dp_ielisa)) dp_ielisa$status_L13 else NULL
        is_pos <- .is_ielisa_L13_positive(dp_ielisa$positive_L13, status_col, input$include_borderline)
        dp_pos <- max(dp_pos, sum(is_pos, na.rm = TRUE))
      }
      if ("positive_L15" %in% names(dp_ielisa)) {
        status_col <- if ("status_L15" %in% names(dp_ielisa)) dp_ielisa$status_L15 else NULL
        is_pos <- .is_ielisa_L15_positive(dp_ielisa$positive_L15, status_col, input$include_borderline)
        dp_pos <- max(dp_pos, sum(is_pos, na.rm = TRUE))
      }

      if (dp_n == 0) return("N/A")
      sprintf("%.1f%%", dp_pos / dp_n * 100)
    })

    # ========================================================================
    # OVERVIEW TAB OUTPUTS
    # ========================================================================

    # Sample overview table
    output$overview_samples_table <- DT::renderDT({
      data <- biobank_data()
      if (is.null(data)) return(DT::datatable(data.frame(Message = "No data")))

      da_n <- sum(data$study == "DA", na.rm = TRUE)
      dp_n <- sum(data$study == "DP", na.rm = TRUE)
      total <- da_n + dp_n

      df <- data.frame(
        Metric = c("Total Samples", "DA (Active)", "DP (Passive)", "DA %", "DP %"),
        Value = c(
          scales::comma(total),
          scales::comma(da_n),
          scales::comma(dp_n),
          sprintf("%.1f%%", da_n / max(total, 1) * 100),
          sprintf("%.1f%%", dp_n / max(total, 1) * 100)
        ),
        stringsAsFactors = FALSE
      )

      DT::datatable(df, rownames = FALSE, options = list(dom = 't', pageLength = 10), class = "table-sm")
    })

    # Positivity overview table
    output$overview_positivity_table <- DT::renderDT({
      results <- list()

      # MIC
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        is_pos <- .is_mic_positive(mic$FinalCall, input$include_borderline)
        da_rate <- mean(is_pos[mic$study == "DA"], na.rm = TRUE) * 100
        dp_rate <- mean(is_pos[mic$study == "DP"], na.rm = TRUE) * 100
        results[[length(results) + 1]] <- data.frame(
          Test = "MIC qPCR",
          DA_Rate = sprintf("%.1f%%", da_rate),
          DP_Rate = sprintf("%.1f%%", dp_rate),
          stringsAsFactors = FALSE
        )
      }

      # ELISA-PE
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0) {
        pos_col <- if ("sample_positive" %in% names(pe)) "sample_positive" else intersect(c("positive", "is_positive"), names(pe))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          da_rate <- mean(pe[[pos_col]][pe$study == "DA"] == TRUE, na.rm = TRUE) * 100
          dp_rate <- mean(pe[[pos_col]][pe$study == "DP"] == TRUE, na.rm = TRUE) * 100
          results[[length(results) + 1]] <- data.frame(
            Test = "ELISA-PE",
            DA_Rate = sprintf("%.1f%%", da_rate),
            DP_Rate = sprintf("%.1f%%", dp_rate),
            stringsAsFactors = FALSE
          )
        }
      }

      # ELISA-VSG
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0) {
        pos_col <- if ("sample_positive" %in% names(vsg)) "sample_positive" else intersect(c("positive", "is_positive"), names(vsg))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          da_rate <- mean(vsg[[pos_col]][vsg$study == "DA"] == TRUE, na.rm = TRUE) * 100
          dp_rate <- mean(vsg[[pos_col]][vsg$study == "DP"] == TRUE, na.rm = TRUE) * 100
          results[[length(results) + 1]] <- data.frame(
            Test = "ELISA-VSG",
            DA_Rate = sprintf("%.1f%%", da_rate),
            DP_Rate = sprintf("%.1f%%", dp_rate),
            stringsAsFactors = FALSE
          )
        }
      }

      # iELISA
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0) {
        pos_col <- intersect(c("positive_L13", "sample_positive", "positive"), names(ielisa))[1]
        if (!is.na(pos_col)) {
          da_rate <- mean(ielisa[[pos_col]][ielisa$study == "DA"] == TRUE, na.rm = TRUE) * 100
          dp_rate <- mean(ielisa[[pos_col]][ielisa$study == "DP"] == TRUE, na.rm = TRUE) * 100
          results[[length(results) + 1]] <- data.frame(
            Test = "iELISA",
            DA_Rate = sprintf("%.1f%%", da_rate),
            DP_Rate = sprintf("%.1f%%", dp_rate),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(results) == 0) {
        return(DT::datatable(data.frame(Message = "No test data")))
      }

      df <- do.call(rbind, results)
      DT::datatable(df, rownames = FALSE, options = list(dom = 't', pageLength = 10), class = "table-sm")
    })

    # Key findings
    output$overview_key_findings <- renderUI({
      findings <- list()

      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        is_pos <- .is_mic_positive(mic$FinalCall, input$include_borderline)
        da_rate <- mean(is_pos[mic$study == "DA"], na.rm = TRUE) * 100
        dp_rate <- mean(is_pos[mic$study == "DP"], na.rm = TRUE) * 100

        if (!is.na(da_rate) && !is.na(dp_rate)) {
          diff <- da_rate - dp_rate
          direction <- if (diff > 0) "higher" else "lower"
          findings[[length(findings) + 1]] <- tags$li(
            sprintf("MIC positivity is %.1f%% %s in DA vs DP (%.1f%% vs %.1f%%)",
                    abs(diff), direction, da_rate, dp_rate)
          )

          # Calculate OR
          da_pos <- sum(mic$study == "DA" & is_pos, na.rm = TRUE)
          da_neg <- sum(mic$study == "DA" & !is_pos, na.rm = TRUE)
          dp_pos <- sum(mic$study == "DP" & is_pos, na.rm = TRUE)
          dp_neg <- sum(mic$study == "DP" & !is_pos, na.rm = TRUE)

          if (da_neg > 0 && dp_neg > 0) {
            or_result <- .calc_odds_ratio(da_pos, da_neg, dp_pos, dp_neg)
            findings[[length(findings) + 1]] <- tags$li(
              sprintf("Odds Ratio (DA vs DP): %.2f (95%% CI: %.2f-%.2f)",
                      or_result$or, or_result$ci_lower, or_result$ci_upper)
            )
          }
        }
      }

      data <- biobank_data()
      if (!is.null(data) && "age" %in% names(data)) {
        da_median <- median(data$age[data$study == "DA"], na.rm = TRUE)
        dp_median <- median(data$age[data$study == "DP"], na.rm = TRUE)
        if (!is.na(da_median) && !is.na(dp_median)) {
          findings[[length(findings) + 1]] <- tags$li(
            sprintf("Median age: DA = %.0f years, DP = %.0f years", da_median, dp_median)
          )
        }
      }

      if (length(findings) == 0) {
        return(p("No significant findings available"))
      }

      tags$ul(findings)
    })

    # Detailed overview table
    output$overview_detailed_table <- DT::renderDT({
      results <- list()

      # MIC detailed
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        is_pos <- .is_mic_positive(mic$FinalCall, input$include_borderline)
        da_n <- sum(mic$study == "DA", na.rm = TRUE)
        dp_n <- sum(mic$study == "DP", na.rm = TRUE)
        da_pos <- sum(mic$study == "DA" & is_pos, na.rm = TRUE)
        dp_pos <- sum(mic$study == "DP" & is_pos, na.rm = TRUE)

        results[[length(results) + 1]] <- data.frame(
          Test = "MIC qPCR",
          DA_N = da_n,
          DA_Pos = da_pos,
          DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
          DP_N = dp_n,
          DP_Pos = dp_pos,
          DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
          stringsAsFactors = FALSE
        )

        # Add borderline counts if applicable
        if ("FinalCall" %in% names(mic)) {
          borderline_values <- c("Indeterminate", "Inconclusive", "Review", "Retest")
          da_borderline <- sum(mic$study == "DA" & mic$FinalCall %in% borderline_values, na.rm = TRUE)
          dp_borderline <- sum(mic$study == "DP" & mic$FinalCall %in% borderline_values, na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Test = "  - Borderline",
            DA_N = "",
            DA_Pos = da_borderline,
            DA_Rate = sprintf("%.1f%%", da_borderline / max(da_n, 1) * 100),
            DP_N = "",
            DP_Pos = dp_borderline,
            DP_Rate = sprintf("%.1f%%", dp_borderline / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }
      }

      # ELISA-PE
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0) {
        pos_col <- if ("sample_positive" %in% names(pe)) "sample_positive" else intersect(c("positive", "is_positive"), names(pe))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          da_n <- sum(pe$study == "DA", na.rm = TRUE)
          dp_n <- sum(pe$study == "DP", na.rm = TRUE)
          da_pos <- sum(pe$study == "DA" & pe[[pos_col]] == TRUE, na.rm = TRUE)
          dp_pos <- sum(pe$study == "DP" & pe[[pos_col]] == TRUE, na.rm = TRUE)

          results[[length(results) + 1]] <- data.frame(
            Test = "ELISA-PE",
            DA_N = da_n,
            DA_Pos = da_pos,
            DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
            DP_N = dp_n,
            DP_Pos = dp_pos,
            DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }
      }

      # ELISA-VSG
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0) {
        pos_col <- if ("sample_positive" %in% names(vsg)) "sample_positive" else intersect(c("positive", "is_positive"), names(vsg))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          da_n <- sum(vsg$study == "DA", na.rm = TRUE)
          dp_n <- sum(vsg$study == "DP", na.rm = TRUE)
          da_pos <- sum(vsg$study == "DA" & vsg[[pos_col]] == TRUE, na.rm = TRUE)
          dp_pos <- sum(vsg$study == "DP" & vsg[[pos_col]] == TRUE, na.rm = TRUE)

          results[[length(results) + 1]] <- data.frame(
            Test = "ELISA-VSG",
            DA_N = da_n,
            DA_Pos = da_pos,
            DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
            DP_N = dp_n,
            DP_Pos = dp_pos,
            DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }
      }

      # iELISA with L13 and L15
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0) {
        da_n <- sum(ielisa$study == "DA", na.rm = TRUE)
        dp_n <- sum(ielisa$study == "DP", na.rm = TRUE)

        # Overall
        pos_col <- intersect(c("sample_positive", "positive_L13", "positive"), names(ielisa))[1]
        if (!is.na(pos_col)) {
          da_pos <- sum(ielisa$study == "DA" & ielisa[[pos_col]] == TRUE, na.rm = TRUE)
          dp_pos <- sum(ielisa$study == "DP" & ielisa[[pos_col]] == TRUE, na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Test = "iELISA (Overall)",
            DA_N = da_n,
            DA_Pos = da_pos,
            DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
            DP_N = dp_n,
            DP_Pos = dp_pos,
            DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }

        # L13 - with borderline handling
        if ("positive_L13" %in% names(ielisa)) {
          status_L13 <- if ("status_L13" %in% names(ielisa)) ielisa$status_L13 else NULL
          da_is_pos <- .is_ielisa_L13_positive(ielisa$positive_L13, status_L13, input$include_borderline) & ielisa$study == "DA"
          dp_is_pos <- .is_ielisa_L13_positive(ielisa$positive_L13, status_L13, input$include_borderline) & ielisa$study == "DP"
          da_pos <- sum(da_is_pos, na.rm = TRUE)
          dp_pos <- sum(dp_is_pos, na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Test = "  - LiTat 1.3",
            DA_N = "",
            DA_Pos = da_pos,
            DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
            DP_N = "",
            DP_Pos = dp_pos,
            DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }

        # L15 - with borderline handling
        if ("positive_L15" %in% names(ielisa)) {
          status_L15 <- if ("status_L15" %in% names(ielisa)) ielisa$status_L15 else NULL
          da_is_pos <- .is_ielisa_L15_positive(ielisa$positive_L15, status_L15, input$include_borderline) & ielisa$study == "DA"
          dp_is_pos <- .is_ielisa_L15_positive(ielisa$positive_L15, status_L15, input$include_borderline) & ielisa$study == "DP"
          da_pos <- sum(da_is_pos, na.rm = TRUE)
          dp_pos <- sum(dp_is_pos, na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Test = "  - LiTat 1.5",
            DA_N = "",
            DA_Pos = da_pos,
            DA_Rate = sprintf("%.1f%%", da_pos / max(da_n, 1) * 100),
            DP_N = "",
            DP_Pos = dp_pos,
            DP_Rate = sprintf("%.1f%%", dp_pos / max(dp_n, 1) * 100),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(results) == 0) {
        return(DT::datatable(data.frame(Message = "No test data available")))
      }

      df <- do.call(rbind, results)
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE), class = "table-sm")
    })

    # ========================================================================
    # DEMOGRAPHICS PLOTS
    # ========================================================================

    output$age_comparison_plot <- plotly::renderPlotly({
      data <- biobank_data()
      if (is.null(data) || !"age" %in% names(data)) return(plotly::plotly_empty())

      data <- data %>% dplyr::filter(!is.na(age), !is.na(study))
      if (nrow(data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data) || !"sex" %in% names(data)) return(plotly::plotly_empty())

      sex_data <- data %>%
        dplyr::filter(!is.na(sex), !is.na(study), sex %in% c("M", "F")) %>%
        dplyr::count(study, sex) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      if (nrow(sex_data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data) || !"age" %in% names(data)) return(plotly::plotly_empty())

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

      if (nrow(age_data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data)) return(plotly::plotly_empty())

      # Create case_category if not present
      if (!"case_category" %in% names(data)) {
        if ("previous_case" %in% names(data) || "treated" %in% names(data)) {
          prev_col <- if ("previous_case" %in% names(data)) data$previous_case else rep(NA, nrow(data))
          treat_col <- if ("treated" %in% names(data)) data$treated else rep(NA, nrow(data))
          data <- data %>%
            dplyr::mutate(
              case_category = dplyr::case_when(
                prev_col == "Oui" | treat_col == "Oui" ~ "Previous/Treated",
                prev_col == "Non" & treat_col == "Non" ~ "New case",
                prev_col == "Incertain" | treat_col == "Incertain" ~ "Uncertain",
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

      if (nrow(case_data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data)) return(DT::datatable(data.frame(Message = "No data available")))

      stats_list <- list()

      # Age comparison
      if ("age" %in% names(data)) {
        da_ages <- data$age[data$study == "DA" & !is.na(data$age)]
        dp_ages <- data$age[data$study == "DP" & !is.na(data$age)]

        if (length(da_ages) > 0 && length(dp_ages) > 0) {
          wilcox_test <- tryCatch(wilcox.test(da_ages, dp_ages), error = function(e) NULL)
          stats_list[[length(stats_list) + 1]] <- data.frame(
            Variable = "Age (median, IQR)",
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
      data <- biobank_data()
      if (is.null(data) || !"province" %in% names(data)) return(plotly::plotly_empty())

      prov_data <- data %>%
        dplyr::filter(!is.na(province), !is.na(study)) %>%
        dplyr::count(study, province) %>%
        dplyr::group_by(study) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::ungroup()

      if (nrow(prov_data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data) || !"health_zone" %in% names(data)) return(plotly::plotly_empty())

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

      if (nrow(zone_data) == 0) return(plotly::plotly_empty())

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
      data <- biobank_data()
      if (is.null(data) || !"health_zone" %in% names(data)) {
        return(DT::datatable(data.frame(Message = "No geographic data available")))
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

      if (nrow(zone_summary) == 0) {
        return(DT::datatable(data.frame(Message = "No geographic data available")))
      }

      DT::datatable(
        zone_summary,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE),
        class = "table-sm"
      )
    })

    # NEW: Province positivity plot
    output$province_positivity_plot <- plotly::renderPlotly({
      data <- biobank_data()
      mic <- linked_mic_data()

      if (is.null(data) || is.null(mic) || !"province" %in% names(data)) return(plotly::plotly_empty())
      if (!"FinalCall" %in% names(mic)) return(plotly::plotly_empty())

      # Get lab number column for joining
      lab_col <- intersect(c("numero_labo", "lab_id", "numero"), names(data))[1]
      if (is.na(lab_col)) return(plotly::plotly_empty())

      # Create combined dataset
      is_positive <- .is_mic_positive(mic$FinalCall, input$include_borderline)
      mic_summary <- mic %>%
        dplyr::mutate(mic_positive = is_positive) %>%
        dplyr::select(lab_number_norm, mic_positive)

      combined <- data %>%
        dplyr::mutate(lab_number_norm = sapply(.data[[lab_col]], function(x) trimws(tolower(as.character(x))))) %>%
        dplyr::left_join(mic_summary, by = "lab_number_norm") %>%
        dplyr::filter(!is.na(province), !is.na(mic_positive))

      if (nrow(combined) == 0) return(plotly::plotly_empty())

      prov_positivity <- combined %>%
        dplyr::group_by(study, province) %>%
        dplyr::summarise(
          n = dplyr::n(),
          pos = sum(mic_positive, na.rm = TRUE),
          rate = pos / n * 100,
          .groups = "drop"
        )

      plotly::plot_ly(prov_positivity, x = ~province, y = ~rate, color = ~study,
                      colors = study_colors, type = "bar",
                      text = ~sprintf("%.1f%% (%d/%d)", rate, pos, n), textposition = "auto",
                      hovertemplate = "Province: %{x}<br>Study: %{fullData.name}<br>Rate: %{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = ""),
          yaxis = list(title = "MIC Positivity Rate (%)"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    # NEW: Zone positivity plot
    output$zone_positivity_plot <- plotly::renderPlotly({
      data <- biobank_data()
      mic <- linked_mic_data()

      if (is.null(data) || is.null(mic) || !"health_zone" %in% names(data)) return(plotly::plotly_empty())
      if (!"FinalCall" %in% names(mic)) return(plotly::plotly_empty())

      lab_col <- intersect(c("numero_labo", "lab_id", "numero"), names(data))[1]
      if (is.na(lab_col)) return(plotly::plotly_empty())

      is_positive <- .is_mic_positive(mic$FinalCall, input$include_borderline)
      mic_summary <- mic %>%
        dplyr::mutate(mic_positive = is_positive) %>%
        dplyr::select(lab_number_norm, mic_positive)

      combined <- data %>%
        dplyr::mutate(lab_number_norm = sapply(.data[[lab_col]], function(x) trimws(tolower(as.character(x))))) %>%
        dplyr::left_join(mic_summary, by = "lab_number_norm") %>%
        dplyr::filter(!is.na(health_zone), !is.na(mic_positive))

      if (nrow(combined) == 0) return(plotly::plotly_empty())

      # Get top 10 zones
      top_zones <- combined %>%
        dplyr::count(health_zone, sort = TRUE) %>%
        dplyr::slice_head(n = 10) %>%
        dplyr::pull(health_zone)

      zone_positivity <- combined %>%
        dplyr::filter(health_zone %in% top_zones) %>%
        dplyr::group_by(study, health_zone) %>%
        dplyr::summarise(
          n = dplyr::n(),
          pos = sum(mic_positive, na.rm = TRUE),
          rate = pos / n * 100,
          .groups = "drop"
        )

      plotly::plot_ly(zone_positivity, y = ~health_zone, x = ~rate, color = ~study,
                      colors = study_colors, type = "bar", orientation = "h",
                      text = ~sprintf("%.1f%%", rate), textposition = "auto",
                      hovertemplate = "Zone: %{y}<br>Study: %{fullData.name}<br>Rate: %{x:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "MIC Positivity Rate (%)"),
          yaxis = list(title = "", categoryorder = "total ascending"),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    # NEW: Geographic statistical tests table
    output$geographic_tests_table <- DT::renderDT({
      data <- biobank_data()
      if (is.null(data)) return(DT::datatable(data.frame(Message = "No data")))

      tests <- list()

      # Province distribution test
      if ("province" %in% names(data)) {
        tbl <- table(data$study, data$province)
        if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
          chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          if (!is.null(chi_test)) {
            cramers <- .cramers_v(chi_test, sum(tbl))
            tests[[length(tests) + 1]] <- data.frame(
              Variable = "Province Distribution",
              Test = "Chi-squared",
              Statistic = sprintf("χ² = %.2f (df=%d)", chi_test$statistic, chi_test$parameter),
              P_Value = chi_test$p.value,
              Effect_Size = sprintf("V = %.3f (%s)", cramers, .interpret_effect(cramers, "cramers_v")),
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Health zone distribution test
      if ("health_zone" %in% names(data)) {
        tbl <- table(data$study, data$health_zone)
        if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
          chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          if (!is.null(chi_test)) {
            cramers <- .cramers_v(chi_test, sum(tbl))
            tests[[length(tests) + 1]] <- data.frame(
              Variable = "Health Zone Distribution",
              Test = "Chi-squared",
              Statistic = sprintf("χ² = %.2f (df=%d)", chi_test$statistic, chi_test$parameter),
              P_Value = chi_test$p.value,
              Effect_Size = sprintf("V = %.3f (%s)", cramers, .interpret_effect(cramers, "cramers_v")),
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # Geographic positivity test (if MIC data available)
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic) && "province" %in% names(data)) {
        lab_col <- intersect(c("numero_labo", "lab_id", "numero"), names(data))[1]
        if (!is.na(lab_col)) {
          is_positive <- .is_mic_positive(mic$FinalCall, input$include_borderline)
          mic_summary <- mic %>%
            dplyr::mutate(mic_positive = is_positive) %>%
            dplyr::select(lab_number_norm, mic_positive)

          combined <- data %>%
            dplyr::mutate(lab_number_norm = sapply(.data[[lab_col]], function(x) trimws(tolower(as.character(x))))) %>%
            dplyr::left_join(mic_summary, by = "lab_number_norm") %>%
            dplyr::filter(!is.na(province), !is.na(mic_positive))

          if (nrow(combined) > 0) {
            # Test: Province vs MIC positivity
            tbl <- table(combined$province, combined$mic_positive)
            if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
              chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
              if (!is.null(chi_test)) {
                cramers <- .cramers_v(chi_test, sum(tbl))
                tests[[length(tests) + 1]] <- data.frame(
                  Variable = "Province vs MIC Positivity",
                  Test = "Chi-squared",
                  Statistic = sprintf("χ² = %.2f (df=%d)", chi_test$statistic, chi_test$parameter),
                  P_Value = chi_test$p.value,
                  Effect_Size = sprintf("V = %.3f (%s)", cramers, .interpret_effect(cramers, "cramers_v")),
                  stringsAsFactors = FALSE
                )
              }
            }

            # Test: Health zone vs MIC positivity
            if ("health_zone" %in% names(combined)) {
              combined_hz <- combined %>% dplyr::filter(!is.na(health_zone))
              if (nrow(combined_hz) > 0) {
                tbl <- table(combined_hz$health_zone, combined_hz$mic_positive)
                if (all(dim(tbl) >= 2) && sum(tbl) > 0) {
                  chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
                  if (!is.null(chi_test)) {
                    cramers <- .cramers_v(chi_test, sum(tbl))
                    tests[[length(tests) + 1]] <- data.frame(
                      Variable = "Health Zone vs MIC Positivity",
                      Test = "Chi-squared",
                      Statistic = sprintf("χ² = %.2f (df=%d)", chi_test$statistic, chi_test$parameter),
                      P_Value = chi_test$p.value,
                      Effect_Size = sprintf("V = %.3f (%s)", cramers, .interpret_effect(cramers, "cramers_v")),
                      stringsAsFactors = FALSE
                    )
                  }
                }
              }
            }
          }
        }
      }

      if (length(tests) == 0) {
        return(DT::datatable(data.frame(Message = "Insufficient geographic data for tests")))
      }

      tests_df <- do.call(rbind, tests)
      tests_df$P_Value <- sprintf("%.4f", tests_df$P_Value)

      DT::datatable(
        tests_df,
        rownames = FALSE,
        options = list(pageLength = 10, dom = 't', scrollX = TRUE),
        class = "table-sm"
      )
    })

    # ========================================================================
    # TEST RESULTS PLOTS
    # ========================================================================

    output$positivity_comparison_plot <- plotly::renderPlotly({
      results <- list()

      # MIC
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0) {
        if ("FinalCall" %in% names(mic)) {
          mic_summary <- mic %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = mean(FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA", "LatePositive"), na.rm = TRUE) * 100,
              n = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "MIC")
          results[[length(results) + 1]] <- mic_summary
        }
      }

      # ELISA-PE
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0) {
        pos_col <- if ("sample_positive" %in% names(pe)) "sample_positive" else NULL
        if (is.null(pos_col)) pos_col <- intersect(c("positive", "is_positive"), names(pe))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          pe_summary <- pe %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100,
              n = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "ELISA-PE")
          results[[length(results) + 1]] <- pe_summary
        }
      }

      # ELISA-VSG
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0) {
        pos_col <- if ("sample_positive" %in% names(vsg)) "sample_positive" else NULL
        if (is.null(pos_col)) pos_col <- intersect(c("positive", "is_positive"), names(vsg))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          vsg_summary <- vsg %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100,
              n = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "ELISA-VSG")
          results[[length(results) + 1]] <- vsg_summary
        }
      }

      # iELISA
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0) {
        pos_col <- intersect(c("positive_L13", "positive", "is_positive"), names(ielisa))[1]
        if (!is.na(pos_col)) {
          ielisa_summary <- ielisa %>%
            dplyr::group_by(study) %>%
            dplyr::summarise(
              positivity = mean(.data[[pos_col]] == TRUE, na.rm = TRUE) * 100,
              n = dplyr::n(),
              .groups = "drop"
            ) %>%
            dplyr::mutate(test = "iELISA")
          results[[length(results) + 1]] <- ielisa_summary
        }
      }

      if (length(results) == 0) return(plotly::plotly_empty())

      all_results <- do.call(rbind, results)

      plotly::plot_ly(all_results, x = ~test, y = ~positivity, color = ~study,
                      colors = study_colors, type = "bar",
                      text = ~sprintf("%.1f%% (n=%d)", positivity, n), textposition = "auto",
                      hovertemplate = "Test: %{x}<br>Study: %{fullData.name}<br>Positivity: %{y:.1f}%<extra></extra>") %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Test Type"),
          yaxis = list(title = "Positivity Rate (%)", range = c(0, max(all_results$positivity, na.rm = TRUE) * 1.3)),
          legend = list(orientation = "h", y = -0.15)
        )
    })

    output$mic_comparison_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0) return(plotly::plotly_empty())

      # Create summary for MIC results
      if ("FinalCall" %in% names(mic)) {
        mic_summary <- mic %>%
          dplyr::count(study, FinalCall) %>%
          dplyr::group_by(study) %>%
          dplyr::mutate(pct = n / sum(n) * 100) %>%
          dplyr::ungroup()

        if (nrow(mic_summary) == 0) return(plotly::plotly_empty())

        plotly::plot_ly(mic_summary, x = ~FinalCall, y = ~pct, color = ~study,
                        colors = study_colors, type = "bar",
                        hovertemplate = "Result: %{x}<br>Study: %{fullData.name}<br>%{y:.1f}%<extra></extra>") %>%
          plotly::layout(
            barmode = "group",
            xaxis = list(title = "MIC Result"),
            yaxis = list(title = "Percentage (%)"),
            legend = list(orientation = "h", y = -0.15)
          )
      } else {
        return(plotly::plotly_empty())
      }
    })

    output$elisa_pe_comparison_plot <- plotly::renderPlotly({
      pe <- linked_elisa_pe_data()
      if (is.null(pe) || nrow(pe) == 0) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(pe))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      pe_filtered <- pe %>% dplyr::filter(!is.na(.data[[pp_col]]))
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
      if (is.null(vsg) || nrow(vsg) == 0) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(vsg))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      vsg_filtered <- vsg %>% dplyr::filter(!is.na(.data[[pp_col]]))
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
      if (is.null(ielisa) || nrow(ielisa) == 0) return(plotly::plotly_empty())

      inh_col <- intersect(c("pct_inh_f2_13", "pct_inhibition", "inhibition"), names(ielisa))[1]
      if (is.na(inh_col)) return(plotly::plotly_empty())

      ielisa_filtered <- ielisa %>% dplyr::filter(!is.na(.data[[inh_col]]))
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
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        is_positive <- mic$FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA", "LatePositive")
        da_pos <- sum(mic$study == "DA" & is_positive, na.rm = TRUE)
        da_total <- sum(mic$study == "DA", na.rm = TRUE)
        dp_pos <- sum(mic$study == "DP" & is_positive, na.rm = TRUE)
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

      # ELISA-PE statistics
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0) {
        pos_col <- if ("sample_positive" %in% names(pe)) "sample_positive" else intersect(c("positive", "is_positive"), names(pe))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          is_positive <- pe[[pos_col]] == TRUE
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

      # ELISA-VSG statistics
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0) {
        pos_col <- if ("sample_positive" %in% names(vsg)) "sample_positive" else intersect(c("positive", "is_positive"), names(vsg))[1]
        if (!is.na(pos_col) && !is.null(pos_col)) {
          is_positive <- vsg[[pos_col]] == TRUE
          da_pos <- sum(vsg$study == "DA" & is_positive, na.rm = TRUE)
          da_total <- sum(vsg$study == "DA", na.rm = TRUE)
          dp_pos <- sum(vsg$study == "DP" & is_positive, na.rm = TRUE)
          dp_total <- sum(vsg$study == "DP", na.rm = TRUE)

          if (da_total > 0 && dp_total > 0) {
            tbl <- matrix(c(da_pos, da_total - da_pos, dp_pos, dp_total - dp_pos), nrow = 2)
            chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)

            stats_list[[length(stats_list) + 1]] <- data.frame(
              Test = "ELISA-VSG",
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

      # iELISA statistics
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0) {
        pos_col <- intersect(c("positive_L13", "positive", "is_positive"), names(ielisa))[1]
        if (!is.na(pos_col)) {
          is_positive <- ielisa[[pos_col]] == TRUE
          da_pos <- sum(ielisa$study == "DA" & is_positive, na.rm = TRUE)
          da_total <- sum(ielisa$study == "DA", na.rm = TRUE)
          dp_pos <- sum(ielisa$study == "DP" & is_positive, na.rm = TRUE)
          dp_total <- sum(ielisa$study == "DP", na.rm = TRUE)

          if (da_total > 0 && dp_total > 0) {
            tbl <- matrix(c(da_pos, da_total - da_pos, dp_pos, dp_total - dp_pos), nrow = 2)
            chi_test <- tryCatch(chisq.test(tbl), error = function(e) NULL)

            stats_list[[length(stats_list) + 1]] <- data.frame(
              Test = "iELISA",
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
    # MIC DETAILED PLOTS (Cq values)
    # ========================================================================

    # Helper to create MIC Cq boxplot
    .create_mic_cq_plot <- function(mic_data, cq_col, title) {
      if (is.null(mic_data) || nrow(mic_data) == 0) return(plotly::plotly_empty())
      if (!cq_col %in% names(mic_data)) return(plotly::plotly_empty())

      data_filtered <- mic_data %>% dplyr::filter(!is.na(.data[[cq_col]]))
      if (nrow(data_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(data_filtered, x = ~study, y = ~.data[[cq_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>Cq: %{y:.2f}<extra></extra>") %>%
        plotly::layout(
          xaxis = list(title = "Study Type"),
          yaxis = list(title = paste0("Cq Value (", title, ")")),
          showlegend = FALSE
        )
    }

    output$mic_177t_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      cq_col <- intersect(c("Cq_177T", "cq_177t", "177T_Cq", "Cq.177T"), names(mic))[1]
      if (is.na(cq_col)) cq_col <- "Cq_177T"  # Default
      .create_mic_cq_plot(mic, cq_col, "177T")
    })

    output$mic_18s2_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      cq_col <- intersect(c("Cq_18S2", "cq_18s2", "18S2_Cq", "Cq.18S2"), names(mic))[1]
      if (is.na(cq_col)) cq_col <- "Cq_18S2"
      .create_mic_cq_plot(mic, cq_col, "18S2")
    })

    output$mic_rnasep_dna_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      cq_col <- intersect(c("Cq_RNAseP_DNA", "cq_rnasep_dna", "RNAseP_DNA_Cq", "Cq.RNAseP.DNA"), names(mic))[1]
      if (is.na(cq_col)) cq_col <- "Cq_RNAseP_DNA"
      .create_mic_cq_plot(mic, cq_col, "RNAseP-DNA")
    })

    output$mic_rnasep_rna_plot <- plotly::renderPlotly({
      mic <- linked_mic_data()
      cq_col <- intersect(c("Cq_RNAseP_RNA", "cq_rnasep_rna", "RNAseP_RNA_Cq", "Cq.RNAseP.RNA"), names(mic))[1]
      if (is.na(cq_col)) cq_col <- "Cq_RNAseP_RNA"
      .create_mic_cq_plot(mic, cq_col, "RNAseP-RNA")
    })

    # MIC Cq statistics table
    output$mic_cq_stats_table <- DT::renderDT({
      mic <- linked_mic_data()
      if (is.null(mic) || nrow(mic) == 0) {
        return(DT::datatable(data.frame(Message = "No MIC data available")))
      }

      stats <- list()
      cq_cols <- list(
        "177T" = c("Cq_177T", "cq_177t", "177T_Cq"),
        "18S2" = c("Cq_18S2", "cq_18s2", "18S2_Cq"),
        "RNAseP-DNA" = c("Cq_RNAseP_DNA", "cq_rnasep_dna", "RNAseP_DNA_Cq"),
        "RNAseP-RNA" = c("Cq_RNAseP_RNA", "cq_rnasep_rna", "RNAseP_RNA_Cq")
      )

      for (target in names(cq_cols)) {
        cq_col <- intersect(cq_cols[[target]], names(mic))[1]
        if (!is.na(cq_col)) {
          da_vals <- mic[[cq_col]][mic$study == "DA" & !is.na(mic[[cq_col]])]
          dp_vals <- mic[[cq_col]][mic$study == "DP" & !is.na(mic[[cq_col]])]

          if (length(da_vals) > 0 && length(dp_vals) > 0) {
            wilcox_test <- tryCatch(wilcox.test(da_vals, dp_vals), error = function(e) NULL)
            stats[[length(stats) + 1]] <- data.frame(
              Target = target,
              DA_N = length(da_vals),
              DA_Median_IQR = sprintf("%.1f (%.1f-%.1f)", median(da_vals), quantile(da_vals, 0.25), quantile(da_vals, 0.75)),
              DP_N = length(dp_vals),
              DP_Median_IQR = sprintf("%.1f (%.1f-%.1f)", median(dp_vals), quantile(dp_vals, 0.25), quantile(dp_vals, 0.75)),
              P_Value = if (!is.null(wilcox_test)) sprintf("%.4f", wilcox_test$p.value) else "N/A",
              stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(stats) == 0) {
        return(DT::datatable(data.frame(Message = "No Cq data available")))
      }

      DT::datatable(do.call(rbind, stats), rownames = FALSE,
                    options = list(dom = 't', pageLength = 10), class = "table-sm") %>%
        DT::formatStyle("P_Value",
          backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa")))
    })

    # ========================================================================
    # ELISA DETAILED PLOTS (PP% and OD)
    # ========================================================================

    output$elisa_pe_pp_plot <- plotly::renderPlotly({
      pe <- linked_elisa_pe_data()
      if (is.null(pe) || nrow(pe) == 0) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(pe))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      pe_filtered <- pe %>% dplyr::filter(!is.na(.data[[pp_col]]))
      if (nrow(pe_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(pe_filtered, x = ~study, y = ~.data[[pp_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>PP%%: %{y:.1f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "PP%"), showlegend = FALSE)
    })

    output$elisa_pe_od_plot <- plotly::renderPlotly({
      pe <- linked_elisa_pe_data()
      if (is.null(pe) || nrow(pe) == 0) return(plotly::plotly_empty())

      # DOD = Difference of OD (sample OD - blank OD)
      od_col <- intersect(c("DOD", "dod", "delta_od", "OD", "od"), names(pe))[1]
      if (is.na(od_col)) return(plotly::plotly_empty())

      pe_filtered <- pe %>% dplyr::filter(!is.na(.data[[od_col]]))
      if (nrow(pe_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(pe_filtered, x = ~study, y = ~.data[[od_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>DOD: %{y:.3f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "DOD (Delta OD)"), showlegend = FALSE)
    })

    output$elisa_vsg_pp_plot <- plotly::renderPlotly({
      vsg <- linked_elisa_vsg_data()
      if (is.null(vsg) || nrow(vsg) == 0) return(plotly::plotly_empty())

      pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(vsg))[1]
      if (is.na(pp_col)) return(plotly::plotly_empty())

      vsg_filtered <- vsg %>% dplyr::filter(!is.na(.data[[pp_col]]))
      if (nrow(vsg_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(vsg_filtered, x = ~study, y = ~.data[[pp_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>PP%%: %{y:.1f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "PP%"), showlegend = FALSE)
    })

    output$elisa_vsg_od_plot <- plotly::renderPlotly({
      vsg <- linked_elisa_vsg_data()
      if (is.null(vsg) || nrow(vsg) == 0) return(plotly::plotly_empty())

      # DOD = Difference of OD (sample OD - blank OD)
      od_col <- intersect(c("DOD", "dod", "delta_od", "OD", "od"), names(vsg))[1]
      if (is.na(od_col)) return(plotly::plotly_empty())

      vsg_filtered <- vsg %>% dplyr::filter(!is.na(.data[[od_col]]))
      if (nrow(vsg_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(vsg_filtered, x = ~study, y = ~.data[[od_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>DOD: %{y:.3f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "DOD (Delta OD)"), showlegend = FALSE)
    })

    # ELISA detailed statistics
    output$elisa_detailed_stats_table <- DT::renderDT({
      stats <- list()

      # ELISA-PE PP%
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0) {
        pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(pe))[1]
        if (!is.na(pp_col)) {
          da_vals <- pe[[pp_col]][pe$study == "DA" & !is.na(pe[[pp_col]])]
          dp_vals <- pe[[pp_col]][pe$study == "DP" & !is.na(pe[[pp_col]])]
          if (length(da_vals) > 0 && length(dp_vals) > 0) {
            wilcox <- tryCatch(wilcox.test(da_vals, dp_vals), error = function(e) NULL)
            stats[[length(stats) + 1]] <- data.frame(
              Assay = "ELISA-PE", Measure = "PP%",
              DA_Median = sprintf("%.1f (%.1f-%.1f)", median(da_vals), quantile(da_vals, 0.25), quantile(da_vals, 0.75)),
              DP_Median = sprintf("%.1f (%.1f-%.1f)", median(dp_vals), quantile(dp_vals, 0.25), quantile(dp_vals, 0.75)),
              P_Value = if (!is.null(wilcox)) sprintf("%.4f", wilcox$p.value) else "N/A",
              stringsAsFactors = FALSE)
          }
        }
      }

      # ELISA-VSG PP%
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0) {
        pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(vsg))[1]
        if (!is.na(pp_col)) {
          da_vals <- vsg[[pp_col]][vsg$study == "DA" & !is.na(vsg[[pp_col]])]
          dp_vals <- vsg[[pp_col]][vsg$study == "DP" & !is.na(vsg[[pp_col]])]
          if (length(da_vals) > 0 && length(dp_vals) > 0) {
            wilcox <- tryCatch(wilcox.test(da_vals, dp_vals), error = function(e) NULL)
            stats[[length(stats) + 1]] <- data.frame(
              Assay = "ELISA-VSG", Measure = "PP%",
              DA_Median = sprintf("%.1f (%.1f-%.1f)", median(da_vals), quantile(da_vals, 0.25), quantile(da_vals, 0.75)),
              DP_Median = sprintf("%.1f (%.1f-%.1f)", median(dp_vals), quantile(dp_vals, 0.25), quantile(dp_vals, 0.75)),
              P_Value = if (!is.null(wilcox)) sprintf("%.4f", wilcox$p.value) else "N/A",
              stringsAsFactors = FALSE)
          }
        }
      }

      if (length(stats) == 0) return(DT::datatable(data.frame(Message = "No ELISA data")))
      DT::datatable(do.call(rbind, stats), rownames = FALSE, options = list(dom = 't'), class = "table-sm") %>%
        DT::formatStyle("P_Value", backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa")))
    })

    # ========================================================================
    # iELISA DETAILED PLOTS (L13 and L15)
    # ========================================================================

    output$ielisa_l13_positivity_plot <- plotly::renderPlotly({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0 || !"positive_L13" %in% names(ielisa)) return(plotly::plotly_empty())

      # Apply borderline handling
      status_L13 <- if ("status_L13" %in% names(ielisa)) ielisa$status_L13 else NULL
      ielisa$is_pos_L13 <- .is_ielisa_L13_positive(ielisa$positive_L13, status_L13, input$include_borderline)

      pos_data <- ielisa %>%
        dplyr::group_by(study) %>%
        dplyr::summarise(
          n = dplyr::n(),
          pos = sum(is_pos_L13, na.rm = TRUE),
          rate = pos / n * 100,
          .groups = "drop"
        )

      plotly::plot_ly(pos_data, x = ~study, y = ~rate, color = ~study, colors = study_colors, type = "bar",
                      text = ~sprintf("%.1f%% (%d/%d)", rate, pos, n), textposition = "auto",
                      hovertemplate = "Study: %{x}<br>Rate: %{y:.1f}%<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "LiTat 1.3 Positivity (%)"), showlegend = FALSE)
    })

    output$ielisa_l15_positivity_plot <- plotly::renderPlotly({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0 || !"positive_L15" %in% names(ielisa)) return(plotly::plotly_empty())

      # Apply borderline handling
      status_L15 <- if ("status_L15" %in% names(ielisa)) ielisa$status_L15 else NULL
      ielisa$is_pos_L15 <- .is_ielisa_L15_positive(ielisa$positive_L15, status_L15, input$include_borderline)

      pos_data <- ielisa %>%
        dplyr::group_by(study) %>%
        dplyr::summarise(
          n = dplyr::n(),
          pos = sum(is_pos_L15, na.rm = TRUE),
          rate = pos / n * 100,
          .groups = "drop"
        )

      plotly::plot_ly(pos_data, x = ~study, y = ~rate, color = ~study, colors = study_colors, type = "bar",
                      text = ~sprintf("%.1f%% (%d/%d)", rate, pos, n), textposition = "auto",
                      hovertemplate = "Study: %{x}<br>Rate: %{y:.1f}%<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "LiTat 1.5 Positivity (%)"), showlegend = FALSE)
    })

    output$ielisa_l13_inhibition_plot <- plotly::renderPlotly({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0) return(plotly::plotly_empty())

      inh_col <- intersect(c("pct_inh_f2_13", "pct_inh_f1_13", "pct_inhibition_L13"), names(ielisa))[1]
      if (is.na(inh_col)) return(plotly::plotly_empty())

      ielisa_filtered <- ielisa %>% dplyr::filter(!is.na(.data[[inh_col]]))
      if (nrow(ielisa_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(ielisa_filtered, x = ~study, y = ~.data[[inh_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>%% Inh: %{y:.1f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "% Inhibition (L13)"), showlegend = FALSE)
    })

    output$ielisa_l15_inhibition_plot <- plotly::renderPlotly({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0) return(plotly::plotly_empty())

      inh_col <- intersect(c("pct_inh_f2_15", "pct_inh_f1_15", "pct_inhibition_L15"), names(ielisa))[1]
      if (is.na(inh_col)) return(plotly::plotly_empty())

      ielisa_filtered <- ielisa %>% dplyr::filter(!is.na(.data[[inh_col]]))
      if (nrow(ielisa_filtered) == 0) return(plotly::plotly_empty())

      plotly::plot_ly(ielisa_filtered, x = ~study, y = ~.data[[inh_col]], color = ~study,
                      colors = study_colors, type = "box",
                      hovertemplate = "Study: %{x}<br>%% Inh: %{y:.1f}<extra></extra>") %>%
        plotly::layout(xaxis = list(title = "Study Type"), yaxis = list(title = "% Inhibition (L15)"), showlegend = FALSE)
    })

    # iELISA detailed statistics
    output$ielisa_detailed_stats_table <- DT::renderDT({
      ielisa <- linked_ielisa_data()
      if (is.null(ielisa) || nrow(ielisa) == 0) return(DT::datatable(data.frame(Message = "No iELISA data")))

      stats <- list()
      da_n <- sum(ielisa$study == "DA", na.rm = TRUE)
      dp_n <- sum(ielisa$study == "DP", na.rm = TRUE)

      # L13 positivity - with borderline handling
      if ("positive_L13" %in% names(ielisa)) {
        status_L13 <- if ("status_L13" %in% names(ielisa)) ielisa$status_L13 else NULL
        is_pos_L13 <- .is_ielisa_L13_positive(ielisa$positive_L13, status_L13, input$include_borderline)
        da_pos <- sum(ielisa$study == "DA" & is_pos_L13, na.rm = TRUE)
        dp_pos <- sum(ielisa$study == "DP" & is_pos_L13, na.rm = TRUE)
        if (da_n > 0 && dp_n > 0) {
          tbl <- matrix(c(da_pos, da_n - da_pos, dp_pos, dp_n - dp_pos), nrow = 2)
          chi <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          or <- .calc_odds_ratio(da_pos, da_n - da_pos, dp_pos, dp_n - dp_pos)
          stats[[length(stats) + 1]] <- data.frame(
            Antigen = "LiTat 1.3", Measure = "Positivity",
            DA = sprintf("%d/%d (%.1f%%)", da_pos, da_n, da_pos/da_n*100),
            DP = sprintf("%d/%d (%.1f%%)", dp_pos, dp_n, dp_pos/dp_n*100),
            OR_95CI = sprintf("%.2f (%.2f-%.2f)", or$or, or$ci_lower, or$ci_upper),
            P_Value = if (!is.null(chi)) sprintf("%.4f", chi$p.value) else "N/A",
            stringsAsFactors = FALSE)
        }
      }

      # L15 positivity - with borderline handling
      if ("positive_L15" %in% names(ielisa)) {
        status_L15 <- if ("status_L15" %in% names(ielisa)) ielisa$status_L15 else NULL
        is_pos_L15 <- .is_ielisa_L15_positive(ielisa$positive_L15, status_L15, input$include_borderline)
        da_pos <- sum(ielisa$study == "DA" & is_pos_L15, na.rm = TRUE)
        dp_pos <- sum(ielisa$study == "DP" & is_pos_L15, na.rm = TRUE)
        if (da_n > 0 && dp_n > 0) {
          tbl <- matrix(c(da_pos, da_n - da_pos, dp_pos, dp_n - dp_pos), nrow = 2)
          chi <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          or <- .calc_odds_ratio(da_pos, da_n - da_pos, dp_pos, dp_n - dp_pos)
          stats[[length(stats) + 1]] <- data.frame(
            Antigen = "LiTat 1.5", Measure = "Positivity",
            DA = sprintf("%d/%d (%.1f%%)", da_pos, da_n, da_pos/da_n*100),
            DP = sprintf("%d/%d (%.1f%%)", dp_pos, dp_n, dp_pos/dp_n*100),
            OR_95CI = sprintf("%.2f (%.2f-%.2f)", or$or, or$ci_lower, or$ci_upper),
            P_Value = if (!is.null(chi)) sprintf("%.4f", chi$p.value) else "N/A",
            stringsAsFactors = FALSE)
        }
      }

      # L13 inhibition
      inh_col_13 <- intersect(c("pct_inh_f2_13", "pct_inh_f1_13"), names(ielisa))[1]
      if (!is.na(inh_col_13)) {
        da_vals <- ielisa[[inh_col_13]][ielisa$study == "DA" & !is.na(ielisa[[inh_col_13]])]
        dp_vals <- ielisa[[inh_col_13]][ielisa$study == "DP" & !is.na(ielisa[[inh_col_13]])]
        if (length(da_vals) > 0 && length(dp_vals) > 0) {
          wilcox <- tryCatch(wilcox.test(da_vals, dp_vals), error = function(e) NULL)
          stats[[length(stats) + 1]] <- data.frame(
            Antigen = "LiTat 1.3", Measure = "% Inhibition",
            DA = sprintf("%.1f (%.1f-%.1f)", median(da_vals), quantile(da_vals, 0.25), quantile(da_vals, 0.75)),
            DP = sprintf("%.1f (%.1f-%.1f)", median(dp_vals), quantile(dp_vals, 0.25), quantile(dp_vals, 0.75)),
            OR_95CI = "-",
            P_Value = if (!is.null(wilcox)) sprintf("%.4f", wilcox$p.value) else "N/A",
            stringsAsFactors = FALSE)
        }
      }

      # L15 inhibition
      inh_col_15 <- intersect(c("pct_inh_f2_15", "pct_inh_f1_15"), names(ielisa))[1]
      if (!is.na(inh_col_15)) {
        da_vals <- ielisa[[inh_col_15]][ielisa$study == "DA" & !is.na(ielisa[[inh_col_15]])]
        dp_vals <- ielisa[[inh_col_15]][ielisa$study == "DP" & !is.na(ielisa[[inh_col_15]])]
        if (length(da_vals) > 0 && length(dp_vals) > 0) {
          wilcox <- tryCatch(wilcox.test(da_vals, dp_vals), error = function(e) NULL)
          stats[[length(stats) + 1]] <- data.frame(
            Antigen = "LiTat 1.5", Measure = "% Inhibition",
            DA = sprintf("%.1f (%.1f-%.1f)", median(da_vals), quantile(da_vals, 0.25), quantile(da_vals, 0.75)),
            DP = sprintf("%.1f (%.1f-%.1f)", median(dp_vals), quantile(dp_vals, 0.25), quantile(dp_vals, 0.75)),
            OR_95CI = "-",
            P_Value = if (!is.null(wilcox)) sprintf("%.4f", wilcox$p.value) else "N/A",
            stringsAsFactors = FALSE)
        }
      }

      if (length(stats) == 0) return(DT::datatable(data.frame(Message = "No iELISA data")))
      DT::datatable(do.call(rbind, stats), rownames = FALSE, options = list(dom = 't', pageLength = 10), class = "table-sm") %>%
        DT::formatStyle("P_Value", backgroundColor = DT::styleInterval(c(0.001, 0.05), c("#d4edda", "#fff3cd", "#f8f9fa")))
    })

    # ========================================================================
    # TRANSPORT & EXTRACTION PLOTS
    # ========================================================================

    output$transport_time_plot <- plotly::renderPlotly({
      data <- biobank_data()
      if (is.null(data)) return(plotly::plotly_empty())

      time_col <- intersect(c("transport_time", "transport_days", "days_to_extraction", "delai_transport"), names(data))[1]
      if (is.na(time_col)) return(plotly::plotly_empty())

      data_filtered <- data %>% dplyr::filter(!is.na(.data[[time_col]]))
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
      if (is.null(extract) || nrow(extract) == 0) return(plotly::plotly_empty())

      vol_col <- intersect(c("drs_volume_ml", "volume_ml", "volume", "vol_dbs"), names(extract))[1]
      if (is.na(vol_col)) return(plotly::plotly_empty())

      extract_filtered <- extract %>% dplyr::filter(!is.na(.data[[vol_col]]))
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
      if (is.null(extract) || nrow(extract) == 0) return(plotly::plotly_empty())

      state_col <- intersect(c("drs_state", "state", "etat_dbs", "etat"), names(extract))[1]
      if (is.na(state_col)) return(plotly::plotly_empty())

      state_data <- extract %>%
        dplyr::filter(!is.na(.data[[state_col]])) %>%
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
      if (is.null(extract) || nrow(extract) == 0) return(plotly::plotly_empty())

      qual_col <- intersect(c("extract_quality", "quality", "qualite", "qualite_extrait"), names(extract))[1]
      if (is.na(qual_col)) return(plotly::plotly_empty())

      qual_data <- extract %>%
        dplyr::filter(!is.na(.data[[qual_col]])) %>%
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

      # Transport time from biobank
      data <- biobank_data()
      if (!is.null(data)) {
        time_col <- intersect(c("transport_time", "transport_days", "days_to_extraction", "delai_transport"), names(data))[1]
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
      }

      # DRS Volume from extraction
      extract <- linked_extraction_data()
      if (!is.null(extract) && nrow(extract) > 0) {
        vol_col <- intersect(c("drs_volume_ml", "volume_ml", "volume", "vol_dbs"), names(extract))[1]
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
        return(DT::datatable(data.frame(Message = "No transport/extraction data available")))
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
      data <- biobank_data()
      if (is.null(data)) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      results <- list()

      # Sample counts
      da_n <- sum(data$study == "DA", na.rm = TRUE)
      dp_n <- sum(data$study == "DP", na.rm = TRUE)
      total_n <- da_n + dp_n

      results[[length(results) + 1]] <- data.frame(
        Category = "Samples", Metric = "Total", DA = scales::comma(da_n), DP = scales::comma(dp_n), stringsAsFactors = FALSE)

      # Demographics
      if ("age" %in% names(data)) {
        da_age <- median(data$age[data$study == "DA"], na.rm = TRUE)
        dp_age <- median(data$age[data$study == "DP"], na.rm = TRUE)
        results[[length(results) + 1]] <- data.frame(
          Category = "Demographics", Metric = "Median Age", DA = sprintf("%.0f", da_age), DP = sprintf("%.0f", dp_age), stringsAsFactors = FALSE)
      }
      if ("sex" %in% names(data)) {
        da_male <- mean(data$sex[data$study == "DA"] == "M", na.rm = TRUE) * 100
        dp_male <- mean(data$sex[data$study == "DP"] == "M", na.rm = TRUE) * 100
        results[[length(results) + 1]] <- data.frame(
          Category = "Demographics", Metric = "% Male", DA = sprintf("%.1f%%", da_male), DP = sprintf("%.1f%%", dp_male), stringsAsFactors = FALSE)
      }

      # MIC Results with DNA/RNA breakdown
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        # Overall positivity
        is_pos <- .is_mic_positive(mic$FinalCall, input$include_borderline)
        da_pos <- mean(is_pos[mic$study == "DA"], na.rm = TRUE) * 100
        dp_pos <- mean(is_pos[mic$study == "DP"], na.rm = TRUE) * 100
        results[[length(results) + 1]] <- data.frame(
          Category = "MIC qPCR", Metric = "Overall Positivity", DA = sprintf("%.1f%%", da_pos), DP = sprintf("%.1f%%", dp_pos), stringsAsFactors = FALSE)

        # DNA positive
        dna_pos_da <- sum(mic$FinalCall[mic$study == "DA"] %in% c("Positive_DNA", "Positive"), na.rm = TRUE)
        dna_pos_dp <- sum(mic$FinalCall[mic$study == "DP"] %in% c("Positive_DNA", "Positive"), na.rm = TRUE)
        results[[length(results) + 1]] <- data.frame(
          Category = "MIC qPCR", Metric = "DNA Positive (n)", DA = as.character(dna_pos_da), DP = as.character(dna_pos_dp), stringsAsFactors = FALSE)

        # RNA positive
        rna_pos_da <- sum(mic$FinalCall[mic$study == "DA"] %in% c("Positive_RNA", "LatePositive"), na.rm = TRUE)
        rna_pos_dp <- sum(mic$FinalCall[mic$study == "DP"] %in% c("Positive_RNA", "LatePositive"), na.rm = TRUE)
        results[[length(results) + 1]] <- data.frame(
          Category = "MIC qPCR", Metric = "RNA Positive (n)", DA = as.character(rna_pos_da), DP = as.character(rna_pos_dp), stringsAsFactors = FALSE)

        # Borderline
        borderline_da <- sum(mic$FinalCall[mic$study == "DA"] %in% c("Indeterminate", "Inconclusive", "Review", "Retest"), na.rm = TRUE)
        borderline_dp <- sum(mic$FinalCall[mic$study == "DP"] %in% c("Indeterminate", "Inconclusive", "Review", "Retest"), na.rm = TRUE)
        results[[length(results) + 1]] <- data.frame(
          Category = "MIC qPCR", Metric = "Borderline (n)", DA = as.character(borderline_da), DP = as.character(borderline_dp), stringsAsFactors = FALSE)
      }

      # ELISA-PE - with borderline handling
      pe <- linked_elisa_pe_data()
      if (!is.null(pe) && nrow(pe) > 0 && "sample_positive" %in% names(pe)) {
        borderline_col <- if ("sample_borderline" %in% names(pe)) pe$sample_borderline else NULL
        is_pos_pe <- .is_elisa_positive(pe$sample_positive, borderline_col, input$include_borderline)
        da_pos <- mean(is_pos_pe[pe$study == "DA"], na.rm = TRUE) * 100
        dp_pos <- mean(is_pos_pe[pe$study == "DP"], na.rm = TRUE) * 100
        results[[length(results) + 1]] <- data.frame(
          Category = "ELISA-PE", Metric = "Positivity", DA = sprintf("%.1f%%", da_pos), DP = sprintf("%.1f%%", dp_pos), stringsAsFactors = FALSE)
        pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(pe))[1]
        if (!is.na(pp_col)) {
          da_pp <- median(pe[[pp_col]][pe$study == "DA"], na.rm = TRUE)
          dp_pp <- median(pe[[pp_col]][pe$study == "DP"], na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Category = "ELISA-PE", Metric = "Median PP%", DA = sprintf("%.1f", da_pp), DP = sprintf("%.1f", dp_pp), stringsAsFactors = FALSE)
        }
      }

      # ELISA-VSG - with borderline handling
      vsg <- linked_elisa_vsg_data()
      if (!is.null(vsg) && nrow(vsg) > 0 && "sample_positive" %in% names(vsg)) {
        borderline_col <- if ("sample_borderline" %in% names(vsg)) vsg$sample_borderline else NULL
        is_pos_vsg <- .is_elisa_positive(vsg$sample_positive, borderline_col, input$include_borderline)
        da_pos <- mean(is_pos_vsg[vsg$study == "DA"], na.rm = TRUE) * 100
        dp_pos <- mean(is_pos_vsg[vsg$study == "DP"], na.rm = TRUE) * 100
        results[[length(results) + 1]] <- data.frame(
          Category = "ELISA-VSG", Metric = "Positivity", DA = sprintf("%.1f%%", da_pos), DP = sprintf("%.1f%%", dp_pos), stringsAsFactors = FALSE)
        pp_col <- intersect(c("PP_percent", "pp_percent", "PP"), names(vsg))[1]
        if (!is.na(pp_col)) {
          da_pp <- median(vsg[[pp_col]][vsg$study == "DA"], na.rm = TRUE)
          dp_pp <- median(vsg[[pp_col]][vsg$study == "DP"], na.rm = TRUE)
          results[[length(results) + 1]] <- data.frame(
            Category = "ELISA-VSG", Metric = "Median PP%", DA = sprintf("%.1f", da_pp), DP = sprintf("%.1f", dp_pp), stringsAsFactors = FALSE)
        }
      }

      # iELISA - with borderline handling
      ielisa <- linked_ielisa_data()
      if (!is.null(ielisa) && nrow(ielisa) > 0) {
        if ("positive_L13" %in% names(ielisa)) {
          status_L13 <- if ("status_L13" %in% names(ielisa)) ielisa$status_L13 else NULL
          is_pos_L13 <- .is_ielisa_L13_positive(ielisa$positive_L13, status_L13, input$include_borderline)
          da_pos <- mean(is_pos_L13[ielisa$study == "DA"], na.rm = TRUE) * 100
          dp_pos <- mean(is_pos_L13[ielisa$study == "DP"], na.rm = TRUE) * 100
          results[[length(results) + 1]] <- data.frame(
            Category = "iELISA", Metric = "LiTat 1.3 Positivity", DA = sprintf("%.1f%%", da_pos), DP = sprintf("%.1f%%", dp_pos), stringsAsFactors = FALSE)
        }
        if ("positive_L15" %in% names(ielisa)) {
          status_L15 <- if ("status_L15" %in% names(ielisa)) ielisa$status_L15 else NULL
          is_pos_L15 <- .is_ielisa_L15_positive(ielisa$positive_L15, status_L15, input$include_borderline)
          da_pos <- mean(is_pos_L15[ielisa$study == "DA"], na.rm = TRUE) * 100
          dp_pos <- mean(is_pos_L15[ielisa$study == "DP"], na.rm = TRUE) * 100
          results[[length(results) + 1]] <- data.frame(
            Category = "iELISA", Metric = "LiTat 1.5 Positivity", DA = sprintf("%.1f%%", da_pos), DP = sprintf("%.1f%%", dp_pos), stringsAsFactors = FALSE)
        }
      }

      if (length(results) == 0) {
        return(DT::datatable(data.frame(Message = "No data available")))
      }

      summary_df <- do.call(rbind, results)

      DT::datatable(
        summary_df,
        rownames = FALSE,
        options = list(pageLength = 25, dom = 'ft', scrollX = TRUE),
        class = "table-sm"
      ) %>%
        DT::formatStyle("Category", fontWeight = "bold")
    })

    output$statistical_tests_table <- DT::renderDT({
      all_tests <- list()
      data <- biobank_data()

      if (!is.null(data)) {
        # Age test
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

        # Sex test
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

        # Health zone test
        if ("health_zone" %in% names(data)) {
          tbl <- table(data$study, data$health_zone)
          if (all(dim(tbl) >= 2)) {
            test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
            all_tests[[length(all_tests) + 1]] <- data.frame(
              Category = "Geographic",
              Variable = "Health Zone",
              Test_Type = "Chi-squared",
              Statistic = if (!is.null(test)) sprintf("X² = %.2f", test$statistic) else "N/A",
              P_Value = if (!is.null(test)) test$p.value else NA,
              Significant = if (!is.null(test) && test$p.value < 0.05) "Yes" else "No",
              stringsAsFactors = FALSE
            )
          }
        }
      }

      # MIC positivity test
      mic <- linked_mic_data()
      if (!is.null(mic) && nrow(mic) > 0 && "FinalCall" %in% names(mic)) {
        is_positive <- mic$FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA", "LatePositive")
        tbl <- table(mic$study, is_positive)
        if (all(dim(tbl) >= 2)) {
          test <- tryCatch(chisq.test(tbl), error = function(e) NULL)
          all_tests[[length(all_tests) + 1]] <- data.frame(
            Category = "Test Results",
            Variable = "MIC Positivity",
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
