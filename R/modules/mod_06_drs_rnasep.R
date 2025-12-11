# R/modules/mod_06_drs_rnasep.R
# Pre-Analytical Factors & RNAseP Quality Analysis Module
# ============================================================================
# Analyzes the impact of transport time, temperature conditions, and sample
# volume on RNAseP DNA/RNA Cq values (sample quality indicators)

# ============================================================================
# MODULE UI
# ============================================================================

mod_drs_rnasep_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "RNAseP & Pre-Analytics",
    icon = icon("thermometer-half"),

    div(
      class = "container-fluid",

      # ==== HEADER SECTION ===================================================
      h4(class = "mb-3", icon("flask-vial"), " Pre-Analytical Factors & RNAseP Quality"),
      p(class = "text-muted mb-4",
        "Analyze how transport time, storage temperature, and sample volume affect RNAseP Cq values (sample quality indicators)."
      ),

      # ==== KPI SECTION ======================================================
      layout_column_wrap(
        width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "12px",

        value_box(
          title = "Samples Analyzed",
          value = textOutput(ns("kpi_total_samples")),
          showcase = icon("vials"),
          theme = "primary"
        ),
        value_box(
          title = "Mean Pre-Analytical Time",
          value = textOutput(ns("kpi_mean_preanalytical_days")),
          showcase = icon("clock"),
          theme = "info"
        ),
        value_box(
          title = "Optimal Temperature",
          value = textOutput(ns("kpi_optimal_temp_pct")),
          showcase = icon("temperature-low"),
          theme = "success"
        ),
        value_box(
          title = "Mean RNAseP DNA Cq",
          value = textOutput(ns("kpi_mean_rnasep_dna")),
          showcase = icon("dna"),
          theme = "warning"
        ),
        value_box(
          title = "Mean RNAseP RNA Cq",
          value = textOutput(ns("kpi_mean_rnasep_rna")),
          showcase = icon("bacteria"),
          theme = "danger"
        ),
        value_box(
          title = "Mean Delta RP",
          value = textOutput(ns("kpi_mean_delta_rp")),
          showcase = icon("arrows-left-right"),
          theme = "secondary"
        )
      ),

      # ==== PRE-ANALYTICAL TIME IMPACT =======================================
      h5(class = "mt-4 mb-3", icon("clock"), " Pre-Analytical Time Impact"),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          full_screen = TRUE,
          card_header("Pre-Analytical Time vs RNAseP DNA Cq"),
          card_body_fill(
            plotly::plotlyOutput(ns("time_vs_rnasep_dna"), height = "450px")
          ),
          card_footer(
            class = "text-muted small",
            "Time from sample collection to CPLTHA reception. Lower Cq = better quality."
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Pre-Analytical Time vs RNAseP RNA Cq"),
          card_body_fill(
            plotly::plotlyOutput(ns("time_vs_rnasep_rna"), height = "450px")
          ),
          card_footer(
            class = "text-muted small",
            "RNA is more sensitive to degradation than DNA - expect steeper correlation."
          )
        )
      ),

      # ==== TEMPERATURE CONDITIONS IMPACT ====================================
      h5(class = "mt-4 mb-3", icon("thermometer-half"), " Temperature Conditions Impact"),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          full_screen = TRUE,
          card_header("RNAseP Cq by Storage/Transport Temperature"),
          card_body_fill(
            plotly::plotlyOutput(ns("temp_boxplot"), height = "450px")
          ),
          card_footer(
            class = "text-muted small",
            "Comparison of Cq values across temperature conditions (Ambiante = suboptimal)."
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Delta RP (RNA-DNA) by Temperature"),
          card_body_fill(
            plotly::plotlyOutput(ns("delta_rp_by_temp"), height = "450px")
          ),
          card_footer(
            class = "text-muted small",
            "Higher Delta RP indicates RNA degradation relative to DNA."
          )
        )
      ),

      # ==== COMBINED FACTORS HEATMAP =========================================
      h5(class = "mt-4 mb-3", icon("table-cells"), " Combined Factors Analysis"),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          full_screen = TRUE,
          card_header("Mean RNAseP DNA Cq: Time × Temperature"),
          card_body_fill(
            plotly::plotlyOutput(ns("heatmap_dna"), height = "400px")
          ),
          card_footer(
            class = "text-muted small",
            "Heatmap showing combined effect of pre-analytical time and temperature on DNA quality."
          )
        ),
        card(
          full_screen = TRUE,
          card_header("Mean RNAseP RNA Cq: Time × Temperature"),
          card_body_fill(
            plotly::plotlyOutput(ns("heatmap_rna"), height = "400px")
          ),
          card_footer(
            class = "text-muted small",
            "Heatmap showing combined effect of pre-analytical time and temperature on RNA quality."
          )
        )
      ),

      # ==== VOLUME ANALYSIS ==================================================
      h5(class = "mt-4 mb-3", icon("droplet"), " Sample Volume Impact"),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          full_screen = TRUE,
          card_header("DRS Volume vs RNAseP DNA Cq (colored by time category)"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_vs_dna"), height = "450px")
          )
        ),
        card(
          full_screen = TRUE,
          card_header("DRS Volume vs RNAseP RNA Cq (colored by temperature)"),
          card_body_fill(
            plotly::plotlyOutput(ns("volume_vs_rna"), height = "450px")
          )
        )
      ),

      # ==== MULTI-FACTOR SCATTER =============================================
      h5(class = "mt-4 mb-3", icon("chart-scatter"), " Multi-Factor Visualization"),

      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          full_screen = TRUE,
          card_header("RNAseP DNA vs RNA: Volume, Time & Temperature"),
          card_body_fill(
            plotly::plotlyOutput(ns("multifactor_scatter"), height = "550px")
          ),
          card_footer(
            class = "text-muted small",
            "Point size = transport time (days), color = temperature condition, position = Cq values."
          )
        )
      ),

      # ==== SUMMARY TABLES ===================================================
      h5(class = "mt-4 mb-3", icon("table"), " Summary Statistics"),

      layout_columns(
        col_widths = c(6, 6), gap = "16px",

        card(
          card_header("Statistics by Pre-Analytical Time Category"),
          card_body(
            DT::DTOutput(ns("summary_by_time"))
          )
        ),
        card(
          card_header("Statistics by Temperature Condition"),
          card_body(
            DT::DTOutput(ns("summary_by_temp"))
          )
        )
      ),

      # ==== DETAILED DATA TABLE ==============================================
      layout_columns(
        col_widths = c(12), gap = "16px",
        card(
          card_header(
            "Detailed Sample Data",
            class = "d-flex justify-content-between align-items-center"
          ),
          card_body(
            DT::DTOutput(ns("detail_table"))
          )
        )
      )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

mod_drs_rnasep_server <- function(id, extractions_df, qpcr_data, biobank_df, filters) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # HELPER: Robust barcode normalization for matching
    # ========================================================================
    normalize_barcode_for_match <- function(x) {
      if (is.null(x)) return(character(0))

      # Convert to character and trim
      bc <- trimws(as.character(x))

      # Normalize to lowercase
      bc <- tolower(bc)

      # Remove common prefixes (KPS, kps, etc.)
      bc <- gsub("^kps[- _]*", "", bc)

      # Remove all non-alphanumeric characters except hyphens
      bc <- gsub("[^a-z0-9-]", "", bc)

      # Remove leading zeros (but keep if it's the only character)
      bc <- gsub("^0+(?=.)", "", bc, perl = TRUE)

      # Replace empty strings with NA
      bc[bc == "" | bc == "na" | bc == "n/a"] <- NA_character_

      bc
    }

    # ========================================================================
    # REACTIVE DATA PREPARATION
    # ========================================================================

    # Combine extraction, qPCR, and biobank transport data
    combined_data <- reactive({
      req(extractions_df())

      ext_data <- extractions_df()

      if (is.null(ext_data) || nrow(ext_data) == 0) {
        return(tibble::tibble())
      }

      # Normalize sample ID for matching - use robust normalization
      ext_data$barcode_norm <- normalize_barcode_for_match(ext_data$sample_id)

      # Also try matching on barcode column if different from sample_id
      if ("barcode" %in% names(ext_data)) {
        ext_data$barcode_alt_norm <- normalize_barcode_for_match(ext_data$barcode)
      } else {
        ext_data$barcode_alt_norm <- ext_data$barcode_norm
      }

      # And try numero if available
      if ("numero" %in% names(ext_data)) {
        ext_data$numero_norm <- normalize_barcode_for_match(ext_data$numero)
      } else {
        ext_data$numero_norm <- NA_character_
      }

      # ----- Join qPCR data -----
      if (!is.null(qpcr_data) && !is.null(qpcr_data()) && nrow(qpcr_data()) > 0) {
        qpcr <- qpcr_data()

        # Normalize the SampleID for matching
        qpcr$barcode_norm <- normalize_barcode_for_match(qpcr$SampleID)

        # Check which RNAseP columns are available
        has_dna <- "Cq_median_RNAseP_DNA" %in% names(qpcr)
        has_rna <- "Cq_median_RNAseP_RNA" %in% names(qpcr)

        if (has_dna || has_rna) {
          # Build the summarise expression based on available columns
          qpcr_summary <- qpcr %>%
            group_by(barcode_norm)

          if (has_dna && has_rna) {
            qpcr_summary <- qpcr_summary %>%
              summarise(
                rnasep_dna_cq = mean(Cq_median_RNAseP_DNA, na.rm = TRUE),
                rnasep_rna_cq = mean(Cq_median_RNAseP_RNA, na.rm = TRUE),
                .groups = "drop"
              )
          } else if (has_dna) {
            qpcr_summary <- qpcr_summary %>%
              summarise(
                rnasep_dna_cq = mean(Cq_median_RNAseP_DNA, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(rnasep_rna_cq = NA_real_)
          } else {
            qpcr_summary <- qpcr_summary %>%
              summarise(
                rnasep_rna_cq = mean(Cq_median_RNAseP_RNA, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              mutate(rnasep_dna_cq = NA_real_)
          }

          # Filter out infinite values
          qpcr_summary <- qpcr_summary %>%
            mutate(
              rnasep_dna_cq = ifelse(is.infinite(rnasep_dna_cq), NA_real_, rnasep_dna_cq),
              rnasep_rna_cq = ifelse(is.infinite(rnasep_rna_cq), NA_real_, rnasep_rna_cq)
            )

          ext_data <- ext_data %>%
            left_join(qpcr_summary, by = "barcode_norm")
        }
      }

      # ----- Join biobank transport data -----
      if (!is.null(biobank_df) && !is.null(biobank_df()) && nrow(biobank_df()) > 0) {
        biobank <- biobank_df()

        # Find the barcode column in biobank data
        biobank_barcode_col <- intersect(
          c("barcode", "code_barres_kps"),
          names(biobank)
        )[1]

        # Also check for lab_id/numero columns for alternative matching
        biobank_numero_col <- intersect(
          c("lab_id", "numero"),
          names(biobank)
        )[1]

        if (!is.na(biobank_barcode_col) || !is.na(biobank_numero_col)) {
          # Prepare biobank transport data
          biobank_transport <- biobank

          # Add normalized barcode column
          if (!is.na(biobank_barcode_col)) {
            biobank_transport$barcode_norm <- normalize_barcode_for_match(biobank_transport[[biobank_barcode_col]])
          } else {
            biobank_transport$barcode_norm <- NA_character_
          }

          # Add normalized numero/lab_id column
          if (!is.na(biobank_numero_col)) {
            biobank_transport$numero_norm <- normalize_barcode_for_match(as.character(biobank_transport[[biobank_numero_col]]))
          } else {
            biobank_transport$numero_norm <- NA_character_
          }

          biobank_transport <- biobank_transport %>%
            select(
              barcode_norm,
              numero_norm,
              any_of(c(
                "date_sample",
                "date_sent_cpltha",
                "date_received_cpltha",
                "storage_before_cpltha",
                "transport_temperature"
              ))
            ) %>%
            distinct(barcode_norm, numero_norm, .keep_all = TRUE)

          # First try to join by barcode
          ext_data <- ext_data %>%
            left_join(
              biobank_transport %>%
                filter(!is.na(barcode_norm)) %>%
                select(-numero_norm) %>%
                distinct(barcode_norm, .keep_all = TRUE),
              by = "barcode_norm",
              suffix = c("", "_biobank")
            )

          # For rows that didn't match by barcode, try matching by numero
          unmatched_idx <- is.na(ext_data$date_sample) & !is.na(ext_data$numero_norm)
          if (any(unmatched_idx)) {
            numero_lookup <- biobank_transport %>%
              filter(!is.na(numero_norm)) %>%
              select(-barcode_norm) %>%
              distinct(numero_norm, .keep_all = TRUE)

            if (nrow(numero_lookup) > 0) {
              # Get the fields we need to fill
              fill_cols <- intersect(
                c("date_sample", "date_sent_cpltha", "date_received_cpltha",
                  "storage_before_cpltha", "transport_temperature"),
                names(numero_lookup)
              )

              for (i in which(unmatched_idx)) {
                match_row <- numero_lookup[numero_lookup$numero_norm == ext_data$numero_norm[i], ]
                if (nrow(match_row) > 0) {
                  for (col in fill_cols) {
                    if (col %in% names(ext_data) && col %in% names(match_row)) {
                      if (is.na(ext_data[[col]][i])) {
                        ext_data[[col]][i] <- match_row[[col]][1]
                      }
                    }
                  }
                }
              }
            }
          }

          # Also try matching by alternative barcode (barcode column vs sample_id)
          if ("barcode_alt_norm" %in% names(ext_data)) {
            still_unmatched_idx <- is.na(ext_data$date_sample) & !is.na(ext_data$barcode_alt_norm) &
                                   ext_data$barcode_alt_norm != ext_data$barcode_norm
            if (any(still_unmatched_idx)) {
              alt_lookup <- biobank_transport %>%
                filter(!is.na(barcode_norm)) %>%
                select(-numero_norm) %>%
                distinct(barcode_norm, .keep_all = TRUE)

              if (nrow(alt_lookup) > 0) {
                fill_cols <- intersect(
                  c("date_sample", "date_sent_cpltha", "date_received_cpltha",
                    "storage_before_cpltha", "transport_temperature"),
                  names(alt_lookup)
                )

                for (i in which(still_unmatched_idx)) {
                  match_row <- alt_lookup[alt_lookup$barcode_norm == ext_data$barcode_alt_norm[i], ]
                  if (nrow(match_row) > 0) {
                    for (col in fill_cols) {
                      if (col %in% names(ext_data) && col %in% names(match_row)) {
                        if (is.na(ext_data[[col]][i])) {
                          ext_data[[col]][i] <- match_row[[col]][1]
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

      # ----- Ensure required columns exist before calculating metrics -----
      # These columns might not exist if no biobank join occurred
      if (!"date_sample" %in% names(ext_data)) {
        ext_data$date_sample <- as.Date(NA)
      }
      if (!"date_received_cpltha" %in% names(ext_data)) {
        ext_data$date_received_cpltha <- as.Date(NA)
      }
      if (!"transport_temperature" %in% names(ext_data)) {
        ext_data$transport_temperature <- NA_character_
      }
      if (!"storage_before_cpltha" %in% names(ext_data)) {
        ext_data$storage_before_cpltha <- NA_character_
      }
      if (!"rnasep_dna_cq" %in% names(ext_data)) {
        ext_data$rnasep_dna_cq <- NA_real_
      }
      if (!"rnasep_rna_cq" %in% names(ext_data)) {
        ext_data$rnasep_rna_cq <- NA_real_
      }

      # ----- Calculate pre-analytical metrics -----
      ext_data <- ext_data %>%
        mutate(
          # Calculate pre-analytical time (collection to reception)
          preanalytical_days = as.numeric(difftime(
            coalesce(date_received_cpltha, extraction_date),
            date_sample,
            units = "days"
          )),

          # Cap unreasonable values
          preanalytical_days = ifelse(
            preanalytical_days < 0 | preanalytical_days > 365,
            NA_real_,
            preanalytical_days
          ),

          # Determine effective temperature (transport temp, or storage temp if transport not specified)
          effective_temperature = coalesce(transport_temperature, storage_before_cpltha),

          # Categorize temperature: Frigo/Congelateur = Optimal, Ambiante = Suboptimal
          temp_category = case_when(
            effective_temperature %in% c("Frigo", "Congelateur") ~ "Optimal (Frigo/Congel.)",
            effective_temperature == "Ambiante" ~ "Suboptimal (Ambiante)",
            TRUE ~ "Unknown"
          ),

          # Time categories based on user specification
          time_category = case_when(
            preanalytical_days <= 7 ~ "Good (≤7 days)",
            preanalytical_days <= 14 ~ "Acceptable (8-14 days)",
            preanalytical_days <= 30 ~ "Caution (15-30 days)",
            preanalytical_days > 30 ~ "Problematic (>30 days)",
            TRUE ~ "Unknown"
          ),
          time_category = factor(
            time_category,
            levels = c("Good (≤7 days)", "Acceptable (8-14 days)", "Caution (15-30 days)", "Problematic (>30 days)", "Unknown")
          ),

          # Calculate Delta RP (RNA degradation indicator)
          delta_rp = rnasep_rna_cq - rnasep_dna_cq,

          # Volume categories
          volume_category = case_when(
            is.na(drs_volume_ml) ~ "No volume",
            drs_volume_ml < 1 ~ "< 1000 μL",
            drs_volume_ml >= 1 & drs_volume_ml <= 1.5 ~ "1000-1500 μL",
            drs_volume_ml > 1.5 & drs_volume_ml <= 2 ~ "1501-2000 μL",
            drs_volume_ml > 2 & drs_volume_ml <= 2.5 ~ "2000-2500 μL",
            drs_volume_ml > 2.5 ~ "> 2500 μL",
            TRUE ~ "Unknown"
          ),
          volume_category = factor(
            volume_category,
            levels = c("< 1000 μL", "1000-1500 μL", "1501-2000 μL", "2000-2500 μL", "> 2500 μL", "No volume", "Unknown")
          ),

          # Convert volume to μL for display
          volume_ul = drs_volume_ml * 1000
        )

      ext_data
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_total_samples <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("0")

      # Count samples with at least some RNAseP data
      n <- sum(
        !is.na(data$rnasep_dna_cq) | !is.na(data$rnasep_rna_cq),
        na.rm = TRUE
      )
      scales::comma(n)
    })

    output$kpi_mean_preanalytical_days <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("—")

      mean_days <- mean(data$preanalytical_days, na.rm = TRUE)
      if (is.na(mean_days)) return("—")

      sprintf("%.1f days", mean_days)
    })

    output$kpi_optimal_temp_pct <- renderText({
      data <- combined_data()
      if (nrow(data) == 0) return("—")

      valid_temp <- data %>%
        filter(temp_category != "Unknown")

      if (nrow(valid_temp) == 0) return("—")

      pct_optimal <- sum(valid_temp$temp_category == "Optimal (Frigo/Congel.)") / nrow(valid_temp) * 100
      sprintf("%.0f%%", pct_optimal)
    })

    output$kpi_mean_rnasep_dna <- renderText({
      data <- combined_data()
      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) return("—")

      mean_cq <- mean(data$rnasep_dna_cq, na.rm = TRUE)
      if (is.na(mean_cq)) return("—")

      sprintf("%.1f", mean_cq)
    })

    output$kpi_mean_rnasep_rna <- renderText({
      data <- combined_data()
      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) return("—")

      mean_cq <- mean(data$rnasep_rna_cq, na.rm = TRUE)
      if (is.na(mean_cq)) return("—")

      sprintf("%.1f", mean_cq)
    })

    output$kpi_mean_delta_rp <- renderText({
      data <- combined_data()
      if (nrow(data) == 0 || !"delta_rp" %in% names(data)) return("—")

      mean_delta <- mean(data$delta_rp, na.rm = TRUE)
      if (is.na(mean_delta)) return("—")

      sprintf("%.1f", mean_delta)
    })

    # ========================================================================
    # TIME IMPACT VISUALIZATIONS
    # ========================================================================

    # Time vs RNAseP DNA
    output$time_vs_rnasep_dna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      plot_data <- data %>%
        filter(
          !is.na(preanalytical_days),
          !is.na(rnasep_dna_cq),
          !is.infinite(rnasep_dna_cq)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No linked data available"))
      }

      # Color palette for time categories
      time_colors <- c(
        "Good (≤7 days)" = "#27AE60",
        "Acceptable (8-14 days)" = "#F39C12",
        "Caution (15-30 days)" = "#E67E22",
        "Problematic (>30 days)" = "#E74C3C",
        "Unknown" = "#95A5A6"
      )

      plotly::plot_ly(
        plot_data,
        x = ~preanalytical_days,
        y = ~rnasep_dna_cq,
        color = ~time_category,
        colors = time_colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, opacity = 0.7),
        text = ~paste0(
          "Sample: ", sample_id, "<br>",
          "Days: ", round(preanalytical_days, 1), "<br>",
          "RNAseP DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
          "Temperature: ", effective_temperature
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Pre-Analytical Time (days)"),
          yaxis = list(title = "RNAseP DNA Cq (lower = better)"),
          hovermode = "closest",
          legend = list(title = list(text = "Time Category"))
        )
    })

    # Time vs RNAseP RNA
    output$time_vs_rnasep_rna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      plot_data <- data %>%
        filter(
          !is.na(preanalytical_days),
          !is.na(rnasep_rna_cq),
          !is.infinite(rnasep_rna_cq)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No linked data available"))
      }

      time_colors <- c(
        "Good (≤7 days)" = "#27AE60",
        "Acceptable (8-14 days)" = "#F39C12",
        "Caution (15-30 days)" = "#E67E22",
        "Problematic (>30 days)" = "#E74C3C",
        "Unknown" = "#95A5A6"
      )

      plotly::plot_ly(
        plot_data,
        x = ~preanalytical_days,
        y = ~rnasep_rna_cq,
        color = ~time_category,
        colors = time_colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, opacity = 0.7),
        text = ~paste0(
          "Sample: ", sample_id, "<br>",
          "Days: ", round(preanalytical_days, 1), "<br>",
          "RNAseP RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
          "Temperature: ", effective_temperature
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Pre-Analytical Time (days)"),
          yaxis = list(title = "RNAseP RNA Cq (lower = better)"),
          hovermode = "closest",
          legend = list(title = list(text = "Time Category"))
        )
    })

    # ========================================================================
    # TEMPERATURE IMPACT VISUALIZATIONS
    # ========================================================================

    # Temperature boxplot
    output$temp_boxplot <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      # Prepare data for both DNA and RNA
      plot_data <- data %>%
        filter(temp_category != "Unknown") %>%
        select(temp_category, rnasep_dna_cq, rnasep_rna_cq) %>%
        tidyr::pivot_longer(
          cols = c(rnasep_dna_cq, rnasep_rna_cq),
          names_to = "assay",
          values_to = "cq"
        ) %>%
        filter(!is.na(cq), !is.infinite(cq)) %>%
        mutate(
          assay = ifelse(assay == "rnasep_dna_cq", "RNAseP DNA", "RNAseP RNA")
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No temperature data available"))
      }

      temp_colors <- c(
        "Optimal (Frigo/Congel.)" = "#27AE60",
        "Suboptimal (Ambiante)" = "#E74C3C"
      )

      plotly::plot_ly(
        plot_data,
        x = ~assay,
        y = ~cq,
        color = ~temp_category,
        colors = temp_colors,
        type = "box",
        boxpoints = "outliers"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Cq Value (lower = better)"),
          boxmode = "group",
          legend = list(title = list(text = "Temperature"))
        )
    })

    # Delta RP by temperature
    output$delta_rp_by_temp <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"delta_rp" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      plot_data <- data %>%
        filter(
          temp_category != "Unknown",
          !is.na(delta_rp),
          !is.infinite(delta_rp)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No Delta RP data available"))
      }

      temp_colors <- c(
        "Optimal (Frigo/Congel.)" = "#27AE60",
        "Suboptimal (Ambiante)" = "#E74C3C"
      )

      plotly::plot_ly(
        plot_data,
        x = ~temp_category,
        y = ~delta_rp,
        color = ~temp_category,
        colors = temp_colors,
        type = "box",
        boxpoints = "all",
        jitter = 0.3,
        pointpos = 0
      ) %>%
        plotly::layout(
          xaxis = list(title = "Temperature Condition"),
          yaxis = list(title = "Delta RP (RNA Cq - DNA Cq)"),
          showlegend = FALSE
        ) %>%
        plotly::add_annotations(
          text = "Higher = more RNA degradation",
          x = 0.5,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          font = list(size = 11, color = "#7f8c8d")
        )
    })

    # ========================================================================
    # HEATMAP VISUALIZATIONS
    # ========================================================================

    # Heatmap DNA
    output$heatmap_dna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      heatmap_data <- data %>%
        filter(
          time_category != "Unknown",
          temp_category != "Unknown",
          !is.na(rnasep_dna_cq),
          !is.infinite(rnasep_dna_cq)
        ) %>%
        group_by(time_category, temp_category) %>%
        summarise(
          mean_cq = mean(rnasep_dna_cq, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        )

      if (nrow(heatmap_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "Insufficient data for heatmap"))
      }

      # Pivot to matrix form
      heatmap_matrix <- heatmap_data %>%
        tidyr::pivot_wider(
          names_from = temp_category,
          values_from = mean_cq,
          values_fill = NA_real_
        )

      # Get time categories in correct order
      time_levels <- levels(data$time_category)
      time_levels <- time_levels[time_levels != "Unknown"]

      plotly::plot_ly(
        x = c("Optimal (Frigo/Congel.)", "Suboptimal (Ambiante)"),
        y = time_levels,
        z = as.matrix(heatmap_matrix[match(time_levels, heatmap_matrix$time_category), -1]),
        type = "heatmap",
        colorscale = list(c(0, "#27AE60"), c(0.5, "#F39C12"), c(1, "#E74C3C")),
        colorbar = list(title = "Mean Cq"),
        text = as.matrix(heatmap_data %>%
          tidyr::pivot_wider(names_from = temp_category, values_from = n, values_fill = 0) %>%
          mutate(across(-time_category, ~paste0("n=", .))) %>%
          select(-time_category)),
        hovertemplate = "Time: %{y}<br>Temp: %{x}<br>Mean Cq: %{z:.1f}<br>%{text}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Temperature Condition"),
          yaxis = list(title = "Pre-Analytical Time", categoryorder = "array", categoryarray = rev(time_levels))
        )
    })

    # Heatmap RNA
    output$heatmap_rna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      heatmap_data <- data %>%
        filter(
          time_category != "Unknown",
          temp_category != "Unknown",
          !is.na(rnasep_rna_cq),
          !is.infinite(rnasep_rna_cq)
        ) %>%
        group_by(time_category, temp_category) %>%
        summarise(
          mean_cq = mean(rnasep_rna_cq, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        )

      if (nrow(heatmap_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "Insufficient data for heatmap"))
      }

      heatmap_matrix <- heatmap_data %>%
        tidyr::pivot_wider(
          names_from = temp_category,
          values_from = mean_cq,
          values_fill = NA_real_
        )

      time_levels <- levels(data$time_category)
      time_levels <- time_levels[time_levels != "Unknown"]

      plotly::plot_ly(
        x = c("Optimal (Frigo/Congel.)", "Suboptimal (Ambiante)"),
        y = time_levels,
        z = as.matrix(heatmap_matrix[match(time_levels, heatmap_matrix$time_category), -1]),
        type = "heatmap",
        colorscale = list(c(0, "#27AE60"), c(0.5, "#F39C12"), c(1, "#E74C3C")),
        colorbar = list(title = "Mean Cq"),
        text = as.matrix(heatmap_data %>%
          tidyr::pivot_wider(names_from = temp_category, values_from = n, values_fill = 0) %>%
          mutate(across(-time_category, ~paste0("n=", .))) %>%
          select(-time_category)),
        hovertemplate = "Time: %{y}<br>Temp: %{x}<br>Mean Cq: %{z:.1f}<br>%{text}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Temperature Condition"),
          yaxis = list(title = "Pre-Analytical Time", categoryorder = "array", categoryarray = rev(time_levels))
        )
    })

    # ========================================================================
    # VOLUME ANALYSIS VISUALIZATIONS
    # ========================================================================

    # Volume vs DNA (colored by time)
    output$volume_vs_dna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_dna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      plot_data <- data %>%
        filter(
          !is.na(drs_volume_ml),
          !is.na(rnasep_dna_cq),
          !is.infinite(rnasep_dna_cq)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No linked volume/RNAseP data"))
      }

      time_colors <- c(
        "Good (≤7 days)" = "#27AE60",
        "Acceptable (8-14 days)" = "#F39C12",
        "Caution (15-30 days)" = "#E67E22",
        "Problematic (>30 days)" = "#E74C3C",
        "Unknown" = "#95A5A6"
      )

      plotly::plot_ly(
        plot_data,
        x = ~volume_ul,
        y = ~rnasep_dna_cq,
        color = ~time_category,
        colors = time_colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, opacity = 0.7),
        text = ~paste0(
          "Sample: ", sample_id, "<br>",
          "Volume: ", round(volume_ul, 0), " μL<br>",
          "RNAseP DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
          "Days: ", round(preanalytical_days, 1)
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "DRS Volume (μL)"),
          yaxis = list(title = "RNAseP DNA Cq"),
          hovermode = "closest",
          legend = list(title = list(text = "Time Category"))
        )
    })

    # Volume vs RNA (colored by temperature)
    output$volume_vs_rna <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 || !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No data available"))
      }

      plot_data <- data %>%
        filter(
          !is.na(drs_volume_ml),
          !is.na(rnasep_rna_cq),
          !is.infinite(rnasep_rna_cq)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No linked volume/RNAseP data"))
      }

      temp_colors <- c(
        "Optimal (Frigo/Congel.)" = "#27AE60",
        "Suboptimal (Ambiante)" = "#E74C3C",
        "Unknown" = "#95A5A6"
      )

      plotly::plot_ly(
        plot_data,
        x = ~volume_ul,
        y = ~rnasep_rna_cq,
        color = ~temp_category,
        colors = temp_colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 10, opacity = 0.7),
        text = ~paste0(
          "Sample: ", sample_id, "<br>",
          "Volume: ", round(volume_ul, 0), " μL<br>",
          "RNAseP RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
          "Temperature: ", effective_temperature
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "DRS Volume (μL)"),
          yaxis = list(title = "RNAseP RNA Cq"),
          hovermode = "closest",
          legend = list(title = list(text = "Temperature"))
        )
    })

    # ========================================================================
    # MULTI-FACTOR SCATTER
    # ========================================================================

    output$multifactor_scatter <- plotly::renderPlotly({
      data <- combined_data()

      if (nrow(data) == 0 ||
          !"rnasep_dna_cq" %in% names(data) ||
          !"rnasep_rna_cq" %in% names(data)) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No RNAseP data available"))
      }

      plot_data <- data %>%
        filter(
          !is.na(rnasep_dna_cq), !is.infinite(rnasep_dna_cq),
          !is.na(rnasep_rna_cq), !is.infinite(rnasep_rna_cq)
        ) %>%
        mutate(
          # Size based on transport time (scaled)
          point_size = pmin(preanalytical_days, 60) / 2 + 5,
          point_size = ifelse(is.na(point_size), 8, point_size)
        )

      if (nrow(plot_data) == 0) {
        return(plotly::plot_ly() %>%
          plotly::layout(title = "No linked RNAseP data"))
      }

      temp_colors <- c(
        "Optimal (Frigo/Congel.)" = "#27AE60",
        "Suboptimal (Ambiante)" = "#E74C3C",
        "Unknown" = "#95A5A6"
      )

      plotly::plot_ly(
        plot_data,
        x = ~rnasep_dna_cq,
        y = ~rnasep_rna_cq,
        color = ~temp_category,
        colors = temp_colors,
        size = ~point_size,
        sizes = c(5, 30),
        type = "scatter",
        mode = "markers",
        marker = list(opacity = 0.7, sizemode = "diameter"),
        text = ~paste0(
          "Sample: ", sample_id, "<br>",
          "RNAseP DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
          "RNAseP RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
          "Delta RP: ", round(delta_rp, 2), "<br>",
          "Volume: ", round(volume_ul, 0), " μL<br>",
          "Days: ", round(preanalytical_days, 1), "<br>",
          "Temperature: ", effective_temperature
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "RNAseP DNA Cq"),
          yaxis = list(title = "RNAseP RNA Cq"),
          hovermode = "closest",
          legend = list(title = list(text = "Temperature"))
        ) %>%
        plotly::add_annotations(
          text = "Point size = pre-analytical time",
          x = 1,
          y = 0,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "right",
          font = list(size = 11, color = "#7f8c8d")
        )
    })

    # ========================================================================
    # SUMMARY TABLES
    # ========================================================================

    # Summary by time category
    output$summary_by_time <- DT::renderDT({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      summary <- data %>%
        filter(time_category != "Unknown") %>%
        group_by(time_category) %>%
        summarise(
          n_samples = n(),
          mean_days = mean(preanalytical_days, na.rm = TRUE),
          mean_dna_cq = mean(rnasep_dna_cq, na.rm = TRUE),
          sd_dna_cq = sd(rnasep_dna_cq, na.rm = TRUE),
          mean_rna_cq = mean(rnasep_rna_cq, na.rm = TRUE),
          sd_rna_cq = sd(rnasep_rna_cq, na.rm = TRUE),
          mean_delta_rp = mean(delta_rp, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          across(where(is.numeric), ~round(., 2)),
          time_category = as.character(time_category)
        )

      DT::datatable(
        summary,
        colnames = c(
          "Time Category", "N", "Mean Days",
          "Mean DNA Cq", "SD DNA", "Mean RNA Cq", "SD RNA", "Mean ΔRP"
        ),
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "table table-striped table-hover table-sm"
      )
    })

    # Summary by temperature
    output$summary_by_temp <- DT::renderDT({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      summary <- data %>%
        filter(temp_category != "Unknown") %>%
        group_by(temp_category) %>%
        summarise(
          n_samples = n(),
          pct = n() / nrow(data) * 100,
          mean_dna_cq = mean(rnasep_dna_cq, na.rm = TRUE),
          sd_dna_cq = sd(rnasep_dna_cq, na.rm = TRUE),
          mean_rna_cq = mean(rnasep_rna_cq, na.rm = TRUE),
          sd_rna_cq = sd(rnasep_rna_cq, na.rm = TRUE),
          mean_delta_rp = mean(delta_rp, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          across(where(is.numeric), ~round(., 2)),
          temp_category = as.character(temp_category)
        )

      DT::datatable(
        summary,
        colnames = c(
          "Temperature", "N", "% Total",
          "Mean DNA Cq", "SD DNA", "Mean RNA Cq", "SD RNA", "Mean ΔRP"
        ),
        options = list(
          pageLength = 5,
          dom = 't',
          ordering = FALSE
        ),
        rownames = FALSE,
        class = "table table-striped table-hover table-sm"
      )
    })

    # ========================================================================
    # DETAILED DATA TABLE
    # ========================================================================

    output$detail_table <- DT::renderDT({
      data <- combined_data()

      if (nrow(data) == 0) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      detail_data <- data %>%
        select(
          sample_id,
          any_of(c(
            "date_sample", "date_received_cpltha",
            "preanalytical_days", "time_category",
            "effective_temperature", "temp_category",
            "volume_ul", "volume_category",
            "rnasep_dna_cq", "rnasep_rna_cq", "delta_rp"
          ))
        ) %>%
        filter(
          !is.na(rnasep_dna_cq) | !is.na(rnasep_rna_cq)
        ) %>%
        mutate(
          across(where(is.factor), as.character),
          across(where(is.Date), as.character),
          sample_id = as.character(sample_id)
        )

      # Rename columns for display
      col_names <- c(
        "Sample ID",
        "Collection Date", "Reception Date",
        "Days", "Time Category",
        "Temperature", "Temp Category",
        "Volume (μL)", "Volume Category",
        "DNA Cq", "RNA Cq", "Delta RP"
      )
      col_names <- col_names[1:ncol(detail_data)]

      DT::datatable(
        detail_data,
        colnames = col_names,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        rownames = FALSE,
        class = "table table-striped table-hover table-sm",
        filter = "top"
      ) %>%
        DT::formatRound(
          columns = intersect(
            c("preanalytical_days", "volume_ul", "rnasep_dna_cq", "rnasep_rna_cq", "delta_rp"),
            names(detail_data)
          ),
          digits = 2
        )
    })
  })
}
