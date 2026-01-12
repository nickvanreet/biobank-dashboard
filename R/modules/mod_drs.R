# R/modules/mod_drs.R
# Unified DRS (Diagnostic Research Specimen) Module
# ============================================================================
# Combines extraction quality monitoring, RNAseP analysis, and QC warnings
# into a single comprehensive module with structure sanitaire statistics

# ============================================================================
# MODULE UI
# ============================================================================

mod_drs_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "DRS",
    icon = icon("vials"),

    div(
      class = "container-fluid",

      # ==== HEADER ============================================================
      h4(class = "mb-3", icon("vials"), " DRS Quality & Pre-Analytics"),
      p(class = "text-muted mb-4",
        "Comprehensive DRS quality monitoring: extraction volumes, RNAseP integrity, and structure sanitaire statistics."
      ),

      # ==== QC ALERT BANNER ===================================================
      uiOutput(ns("qc_alert_banner")),

      # ==== KPI SECTION =======================================================
      h5("Overview", class = "mt-3 mb-2"),
      layout_column_wrap(
        width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "12px",

        value_box(
          title = "Total Samples",
          value = textOutput(ns("kpi_total")),
          showcase = icon("vial"),
          theme = "primary"
        ),
        value_box(
          title = "Volume OK",
          value = textOutput(ns("kpi_volume_ok")),
          showcase = icon("check-circle"),
          theme = "success"
        ),
        value_box(
          title = "Volume Warnings",
          value = textOutput(ns("kpi_volume_warnings")),
          showcase = icon("exclamation-triangle"),
          theme = "warning"
        ),
        value_box(
          title = "Mean RNAseP DNA Cq",
          value = textOutput(ns("kpi_rnasep_dna")),
          showcase = icon("dna"),
          theme = "info"
        ),
        value_box(
          title = "Mean RNAseP RNA Cq",
          value = textOutput(ns("kpi_rnasep_rna")),
          showcase = icon("bacteria"),
          theme = "secondary"
        ),
        value_box(
          title = "Mean Pre-Analytical",
          value = textOutput(ns("kpi_preanalytical")),
          showcase = icon("clock"),
          theme = "info"
        )
      ),

      # ==== TABBED CONTENT ====================================================
      navset_card_tab(
        id = ns("drs_tabs"),

        # ---- TAB 1: Volume Analysis ----------------------------------------
        nav_panel(
          title = "Volume Analysis",
          icon = icon("droplet"),

          layout_columns(
            col_widths = c(12), gap = "16px",

            # Volume threshold info card
            card(
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(icon("info-circle"), " Volume Thresholds"),
                span(class = "text-muted small", "Based on barcode numbering")
              ),
              card_body(
                div(class = "alert alert-info",
                  HTML("<strong>Barcode < 2502200:</strong> Expected volume ~2 mL (range: 1.5-2.5 mL)<br>"),
                  HTML("<strong>Barcode >= 2502200:</strong> Expected volume ~4 mL (range: 3.5-4.5 mL)")
                )
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("Volume Distribution by Barcode Type"),
              card_body_fill(
                plotly::plotlyOutput(ns("volume_by_barcode_type"), height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("Volume Status Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("volume_status_plot"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",
            card(
              full_screen = TRUE,
              card_header("Volume vs Expected by Sample"),
              card_body_fill(
                plotly::plotlyOutput(ns("volume_deviation_plot"), height = "450px")
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",
            card(
              full_screen = TRUE,
              card_header("Volume Trend Over Time"),
              card_body_fill(
                plotly::plotlyOutput(ns("volume_timeseries"), height = "400px")
              )
            )
          )
        ),

        # ---- TAB 2: RNAseP & Pre-Analytics ---------------------------------
        nav_panel(
          title = "RNAseP Quality",
          icon = icon("dna"),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("Pre-Analytical Time vs RNAseP DNA Cq"),
              card_body_fill(
                plotly::plotlyOutput(ns("time_vs_dna"), height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("Pre-Analytical Time vs RNAseP RNA Cq"),
              card_body_fill(
                plotly::plotlyOutput(ns("time_vs_rna"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("RNAseP Cq by Temperature Condition"),
              card_body_fill(
                plotly::plotlyOutput(ns("temp_boxplot"), height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("Delta RP (RNA-DNA) Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("delta_rp_hist"), height = "400px")
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("RNAseP DNA vs RNA Cq (Quality Matrix)"),
              card_body_fill(
                plotly::plotlyOutput(ns("rnasep_scatter"), height = "500px")
              )
            )
          )
        ),

        # ---- TAB 3: Structure Sanitaire Statistics -------------------------
        nav_panel(
          title = "Structure Sanitaire",
          icon = icon("hospital"),

          layout_columns(
            col_widths = c(12), gap = "16px",

            card(
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span(icon("chart-bar"), " QC Statistics by Health Structure"),
                downloadButton(ns("download_structure_stats"), "Export CSV", class = "btn-sm btn-outline-primary")
              ),
              card_body(
                DT::DTOutput(ns("structure_stats_table"))
              )
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("Volume Quality by Structure"),
              card_body_fill(
                plotly::plotlyOutput(ns("structure_volume_chart"), height = "450px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("QC Score by Structure"),
              card_body_fill(
                plotly::plotlyOutput(ns("structure_qc_score"), height = "450px")
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",

            card(
              card_header(
                span(icon("exclamation-triangle"), " Structures with QC Issues")
              ),
              card_body(
                DT::DTOutput(ns("structures_with_issues"))
              )
            )
          )
        ),

        # ---- TAB 4: Extraction Details -------------------------------------
        nav_panel(
          title = "Sample Details",
          icon = icon("table"),

          layout_columns(
            col_widths = c(12), gap = "16px",

            # Freezer lookup
            card(
              card_header("Freezer Sample Lookup"),
              card_body(
                layout_columns(
                  col_widths = c(4, 8),
                  div(
                    textInput(ns("freezer_search"), "Search Sample:", placeholder = "Enter barcode or numero..."),
                    actionButton(ns("freezer_search_btn"), "Search", class = "btn-primary")
                  ),
                  div(
                    DT::DTOutput(ns("freezer_results"))
                  )
                )
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",

            card(
              card_header(
                class = "d-flex justify-content-between align-items-center",
                span("Full DRS Sample Data"),
                downloadButton(ns("download_sample_data"), "Export CSV", class = "btn-sm btn-outline-primary")
              ),
              card_body(
                DT::DTOutput(ns("sample_table"))
              )
            )
          )
        ),

        # ---- TAB 5: QC Warnings --------------------------------------------
        nav_panel(
          title = "QC Warnings",
          icon = icon("exclamation-triangle"),

          layout_columns(
            col_widths = c(4, 4, 4), gap = "16px",

            value_box(
              title = "Critical Issues",
              value = textOutput(ns("kpi_critical_count")),
              showcase = icon("times-circle"),
              theme = "danger"
            ),
            value_box(
              title = "Warnings",
              value = textOutput(ns("kpi_warning_count")),
              showcase = icon("exclamation-triangle"),
              theme = "warning"
            ),
            value_box(
              title = "OK Samples",
              value = textOutput(ns("kpi_ok_count")),
              showcase = icon("check-circle"),
              theme = "success"
            )
          ),

          layout_columns(
            col_widths = c(6, 6), gap = "16px",

            card(
              full_screen = TRUE,
              card_header("Warning Types Distribution"),
              card_body_fill(
                plotly::plotlyOutput(ns("warning_types_chart"), height = "350px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header("QC Status by Week"),
              card_body_fill(
                plotly::plotlyOutput(ns("qc_status_timeline"), height = "350px")
              )
            )
          ),

          layout_columns(
            col_widths = c(12), gap = "16px",

            card(
              card_header("Samples with QC Issues"),
              card_body(
                DT::DTOutput(ns("samples_with_warnings"))
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

mod_drs_server <- function(id, extractions_df, qpcr_data = NULL, biobank_df = NULL, filters = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # HELPER FUNCTIONS
    # ========================================================================

    safe_mean <- function(x) {
      x <- x[!is.na(x) & !is.infinite(x)]
      if (!length(x)) return(NA_real_)
      mean(x)
    }

    normalize_barcode <- function(x) {
      if (is.null(x)) return(character(0))
      bc <- trimws(as.character(x))
      bc <- tolower(bc)
      bc <- gsub("^kps[- _]*", "", bc)
      bc <- gsub("[^a-z0-9-]", "", bc)
      bc <- gsub("^0+(?=.)", "", bc, perl = TRUE)
      bc[bc == "" | bc == "na" | bc == "n/a"] <- NA_character_
      bc
    }

    # Extract numeric part of barcode for threshold comparison
    extract_barcode_num <- function(x) {
      if (is.null(x)) return(NA_real_)
      bc <- as.character(x)
      bc <- gsub("^[Kk][Pp][Ss][-_ ]*", "", bc)
      bc <- gsub("[^0-9]", "", bc)
      num <- suppressWarnings(as.numeric(bc))
      num[is.na(num) | bc == ""] <- NA_real_
      num
    }

    # ========================================================================
    # COMBINED DATA REACTIVE
    # ========================================================================

    combined_data <- reactive({
      req(extractions_df())

      ext_data <- extractions_df()
      if (is.null(ext_data) || nrow(ext_data) == 0) {
        return(tibble::tibble())
      }

      # Keep a display barcode (string) for charts
      ext_data$barcode_display <- dplyr::coalesce(
        ext_data$barcode,
        ext_data$sample_id,
        ext_data$numero
      )

      # Normalize barcodes for matching - multiple columns
      ext_data$barcode_norm <- normalize_barcode(ext_data$sample_id)
      if ("barcode" %in% names(ext_data)) {
        ext_data$barcode_alt_norm <- normalize_barcode(ext_data$barcode)
      } else {
        ext_data$barcode_alt_norm <- ext_data$barcode_norm
      }
      if ("numero" %in% names(ext_data)) {
        ext_data$numero_norm <- normalize_barcode(as.character(ext_data$numero))
      } else {
        ext_data$numero_norm <- NA_character_
      }

      # Extract numeric barcode for volume thresholds
      ext_data$barcode_number <- extract_barcode_num(ext_data$barcode_display)

      # Determine expected volume based on barcode number
      ext_data$expected_volume_ml <- dplyr::case_when(
        is.na(ext_data$barcode_number) ~ NA_real_,
        ext_data$barcode_number < 2502200 ~ 2.0,
        TRUE ~ 4.0
      )

      # Classify volume status
      ext_data$volume_status <- dplyr::case_when(
        is.na(ext_data$drs_volume_ml) ~ "Missing",
        is.na(ext_data$expected_volume_ml) ~ "Unknown",
        ext_data$expected_volume_ml == 2.0 & ext_data$drs_volume_ml < 1.0 ~ "Critical Low",
        ext_data$expected_volume_ml == 2.0 & ext_data$drs_volume_ml < 1.5 ~ "Low",
        ext_data$expected_volume_ml == 2.0 & ext_data$drs_volume_ml > 3.0 ~ "Critical High",
        ext_data$expected_volume_ml == 2.0 & ext_data$drs_volume_ml > 2.5 ~ "High",
        ext_data$expected_volume_ml == 4.0 & ext_data$drs_volume_ml < 2.5 ~ "Critical Low",
        ext_data$expected_volume_ml == 4.0 & ext_data$drs_volume_ml < 3.5 ~ "Low",
        ext_data$expected_volume_ml == 4.0 & ext_data$drs_volume_ml > 5.0 ~ "Critical High",
        ext_data$expected_volume_ml == 4.0 & ext_data$drs_volume_ml > 4.5 ~ "High",
        TRUE ~ "Normal"
      )

      # Calculate volume deviation
      ext_data$volume_deviation_ml <- ext_data$drs_volume_ml - ext_data$expected_volume_ml

      # Add barcode type category
      ext_data$barcode_type <- dplyr::case_when(
        is.na(ext_data$barcode_number) ~ "Unknown",
        ext_data$barcode_number < 2502200 ~ "Old (<2502200, 2mL)",
        TRUE ~ "New (>=2502200, 4mL)"
      )

      # ---- Join qPCR data if available ----
      # Initialize RNAseP columns
      ext_data$rnasep_dna_cq <- NA_real_
      ext_data$rnasep_rna_cq <- NA_real_

      if (!is.null(qpcr_data)) {
        qpcr_df <- tryCatch(qpcr_data(), error = function(e) NULL)
        if (!is.null(qpcr_df) && nrow(qpcr_df) > 0) {
          # Normalize qPCR sample IDs
          qpcr_df$barcode_norm <- normalize_barcode(qpcr_df$SampleID)

          has_dna <- "Cq_median_RNAseP_DNA" %in% names(qpcr_df)
          has_rna <- "Cq_median_RNAseP_RNA" %in% names(qpcr_df)

          if (has_dna || has_rna) {
            # Summarize qPCR data by barcode
            qpcr_summary <- qpcr_df %>%
              dplyr::filter(!is.na(barcode_norm)) %>%
              dplyr::group_by(barcode_norm) %>%
              dplyr::summarise(
                rnasep_dna_cq = if (has_dna) mean(Cq_median_RNAseP_DNA, na.rm = TRUE) else NA_real_,
                rnasep_rna_cq = if (has_rna) mean(Cq_median_RNAseP_RNA, na.rm = TRUE) else NA_real_,
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                rnasep_dna_cq = dplyr::if_else(is.infinite(rnasep_dna_cq), NA_real_, rnasep_dna_cq),
                rnasep_rna_cq = dplyr::if_else(is.infinite(rnasep_rna_cq), NA_real_, rnasep_rna_cq)
              )

            # Match by barcode_norm first
            for (i in seq_len(nrow(ext_data))) {
              bc <- ext_data$barcode_norm[i]
              bc_alt <- ext_data$barcode_alt_norm[i]
              num <- ext_data$numero_norm[i]

              # Try barcode_norm
              if (!is.na(bc)) {
                match_row <- qpcr_summary[qpcr_summary$barcode_norm == bc, ]
                if (nrow(match_row) > 0) {
                  ext_data$rnasep_dna_cq[i] <- match_row$rnasep_dna_cq[1]
                  ext_data$rnasep_rna_cq[i] <- match_row$rnasep_rna_cq[1]
                  next
                }
              }

              # Try barcode_alt_norm
              if (!is.na(bc_alt) && bc_alt != bc) {
                match_row <- qpcr_summary[qpcr_summary$barcode_norm == bc_alt, ]
                if (nrow(match_row) > 0) {
                  ext_data$rnasep_dna_cq[i] <- match_row$rnasep_dna_cq[1]
                  ext_data$rnasep_rna_cq[i] <- match_row$rnasep_rna_cq[1]
                  next
                }
              }

              # Try numero_norm
              if (!is.na(num)) {
                match_row <- qpcr_summary[qpcr_summary$barcode_norm == num, ]
                if (nrow(match_row) > 0) {
                  ext_data$rnasep_dna_cq[i] <- match_row$rnasep_dna_cq[1]
                  ext_data$rnasep_rna_cq[i] <- match_row$rnasep_rna_cq[1]
                }
              }
            }
          }
        }
      }

      # ---- Join biobank data for transport temperature ----
      # The extraction data doesn't have transport_temperature - need to get it from biobank
      if (!is.null(biobank_df)) {
        biobank <- tryCatch(biobank_df(), error = function(e) NULL)
        if (!is.null(biobank) && nrow(biobank) > 0) {
          # Normalize biobank barcodes and lab_ids
          biobank_bc_col <- intersect(c("barcode", "code_barres_kps"), names(biobank))[1]
          biobank_num_col <- intersect(c("lab_id", "numero"), names(biobank))[1]

          if (!is.na(biobank_bc_col) || !is.na(biobank_num_col)) {
            biobank_transport <- biobank

            if (!is.na(biobank_bc_col)) {
              biobank_transport$barcode_norm <- normalize_barcode(biobank_transport[[biobank_bc_col]])
            } else {
              biobank_transport$barcode_norm <- NA_character_
            }

            if (!is.na(biobank_num_col)) {
              biobank_transport$numero_norm <- normalize_barcode(as.character(biobank_transport[[biobank_num_col]]))
            } else {
              biobank_transport$numero_norm <- NA_character_
            }

            # Select transport columns we need
            biobank_transport <- biobank_transport %>%
              dplyr::select(
                barcode_norm, numero_norm,
                dplyr::any_of(c("date_sample", "date_received_cpltha", "transport_temperature", "storage_temp_cpltha"))
              ) %>%
              dplyr::distinct(barcode_norm, numero_norm, .keep_all = TRUE)

            # Initialize columns if they don't exist
            if (!"date_sample" %in% names(ext_data)) ext_data$date_sample <- as.Date(NA)
            if (!"date_received_cpltha" %in% names(ext_data)) ext_data$date_received_cpltha <- as.Date(NA)
            if (!"transport_temperature" %in% names(ext_data)) ext_data$transport_temperature <- NA_character_
            if (!"storage_temp_cpltha" %in% names(ext_data)) ext_data$storage_temp_cpltha <- NA_character_

            # Match biobank transport data to extraction data
            for (i in seq_len(nrow(ext_data))) {
              # Skip if already has transport data
              if (!is.na(ext_data$transport_temperature[i])) next

              bc <- ext_data$barcode_norm[i]
              bc_alt <- ext_data$barcode_alt_norm[i]
              num <- ext_data$numero_norm[i]

              match_row <- NULL

              # Try barcode_norm first
              if (!is.na(bc)) {
                match_row <- biobank_transport[biobank_transport$barcode_norm == bc & !is.na(biobank_transport$barcode_norm), ]
                if (nrow(match_row) > 0) match_row <- match_row[1, ]
              }

              # Try barcode_alt_norm
              if ((is.null(match_row) || nrow(match_row) == 0) && !is.na(bc_alt) && bc_alt != bc) {
                match_row <- biobank_transport[biobank_transport$barcode_norm == bc_alt & !is.na(biobank_transport$barcode_norm), ]
                if (nrow(match_row) > 0) match_row <- match_row[1, ]
              }

              # Try numero_norm
              if ((is.null(match_row) || nrow(match_row) == 0) && !is.na(num)) {
                match_row <- biobank_transport[biobank_transport$numero_norm == num & !is.na(biobank_transport$numero_norm), ]
                if (nrow(match_row) > 0) match_row <- match_row[1, ]
              }

              # Fill in transport data if found
              if (!is.null(match_row) && nrow(match_row) > 0) {
                if (is.na(ext_data$date_sample[i]) && "date_sample" %in% names(match_row)) {
                  ext_data$date_sample[i] <- match_row$date_sample[1]
                }
                if (is.na(ext_data$date_received_cpltha[i]) && "date_received_cpltha" %in% names(match_row)) {
                  ext_data$date_received_cpltha[i] <- match_row$date_received_cpltha[1]
                }
                if (is.na(ext_data$transport_temperature[i]) && "transport_temperature" %in% names(match_row)) {
                  ext_data$transport_temperature[i] <- match_row$transport_temperature[1]
                }
                if (is.na(ext_data$storage_temp_cpltha[i]) && "storage_temp_cpltha" %in% names(match_row)) {
                  ext_data$storage_temp_cpltha[i] <- match_row$storage_temp_cpltha[1]
                }
              }
            }
          }
        }
      }

      # ---- Use existing biobank-linked columns for transport data ----
      # The extraction data already has biobank columns from data_manager linking
      # Use biobank_date_sample if date_sample doesn't exist
      if (!"date_sample" %in% names(ext_data) || all(is.na(ext_data$date_sample))) {
        if ("biobank_date_sample" %in% names(ext_data)) {
          ext_data$date_sample <- ext_data$biobank_date_sample
        } else {
          ext_data$date_sample <- as.Date(NA)
        }
      }

      # Ensure other columns exist
      if (!"date_received_cpltha" %in% names(ext_data)) ext_data$date_received_cpltha <- as.Date(NA)
      if (!"transport_temperature" %in% names(ext_data)) ext_data$transport_temperature <- NA_character_
      if (!"storage_temp_cpltha" %in% names(ext_data)) ext_data$storage_temp_cpltha <- NA_character_

      # Get health_structure - use existing columns from extraction data
      # The data_manager already linked biobank data, so use those columns
      if (!"health_structure" %in% names(ext_data) || all(is.na(ext_data$health_structure)) ||
          all(ext_data$health_structure == "Unspecified", na.rm = TRUE)) {
        # Try fallback columns
        fallback_cols <- c("biobank_health_facility", "biobank_structure_sanitaire",
                          "structure_sanitaire", "health_facility")
        for (col in fallback_cols) {
          if (col %in% names(ext_data)) {
            valid_vals <- !is.na(ext_data[[col]]) & ext_data[[col]] != "" & ext_data[[col]] != "Unspecified"
            if (any(valid_vals)) {
              ext_data$health_structure <- ext_data[[col]]
              break
            }
          }
        }
      }
      if (!"health_structure" %in% names(ext_data)) ext_data$health_structure <- NA_character_

      # ---- Calculate derived metrics ----
      ext_data <- ext_data %>%
        dplyr::mutate(
          # Pre-analytical time
          preanalytical_days = as.numeric(difftime(
            dplyr::coalesce(date_received_cpltha, extraction_date),
            date_sample,
            units = "days"
          )),
          preanalytical_days = dplyr::if_else(
            preanalytical_days < 0 | preanalytical_days > 365,
            NA_real_,
            preanalytical_days
          ),

          # Temperature category
          effective_temperature = dplyr::coalesce(transport_temperature, storage_temp_cpltha),
          temp_category = dplyr::case_when(
            effective_temperature %in% c("Frigo", "Congelateur") ~ "Optimal",
            effective_temperature == "Ambiante" ~ "Suboptimal",
            TRUE ~ "Unknown"
          ),

          # Time category
          time_category = dplyr::case_when(
            is.na(preanalytical_days) ~ "Unknown",
            preanalytical_days <= 7 ~ "Optimal",
            preanalytical_days <= 14 ~ "Acceptable",
            preanalytical_days <= 30 ~ "Caution",
            preanalytical_days <= 60 ~ "Problematic",
            TRUE ~ "Critical"
          ),

          # Delta RP
          delta_rp = rnasep_rna_cq - rnasep_dna_cq,

          # RNAseP status classifications
          rnasep_dna_status = dplyr::case_when(
            is.na(rnasep_dna_cq) | is.infinite(rnasep_dna_cq) ~ "No Data",
            rnasep_dna_cq <= 25 ~ "Optimal",
            rnasep_dna_cq <= 30 ~ "Acceptable",
            rnasep_dna_cq <= 35 ~ "Degraded",
            TRUE ~ "Critical"
          ),
          rnasep_rna_status = dplyr::case_when(
            is.na(rnasep_rna_cq) | is.infinite(rnasep_rna_cq) ~ "No Data",
            rnasep_rna_cq <= 28 ~ "Optimal",
            rnasep_rna_cq <= 33 ~ "Acceptable",
            rnasep_rna_cq <= 38 ~ "Degraded",
            TRUE ~ "Critical"
          ),

          # Overall QC status
          qc_status = dplyr::case_when(
            volume_status %in% c("Critical Low", "Critical High") ~ "Critical",
            rnasep_dna_status == "Critical" | rnasep_rna_status == "Critical" ~ "Critical",
            volume_status %in% c("Low", "High") ~ "Warning",
            rnasep_dna_status == "Degraded" | rnasep_rna_status == "Degraded" ~ "Warning",
            time_category %in% c("Problematic", "Critical") ~ "Warning",
            TRUE ~ "OK"
          ),

          # Warning flags
          has_volume_warning = volume_status %in% c("Low", "High", "Critical Low", "Critical High"),
          has_time_warning = time_category %in% c("Caution", "Problematic", "Critical"),
          has_rnasep_warning = rnasep_dna_status %in% c("Degraded", "Critical") |
                              rnasep_rna_status %in% c("Degraded", "Critical")
        )

      ext_data
    })

    # ========================================================================
    # STRUCTURE SANITAIRE STATISTICS
    # ========================================================================

    structure_stats <- reactive({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return(tibble::tibble())

      # Filter to valid structures
      df_valid <- df %>%
        dplyr::filter(!is.na(health_structure) & health_structure != "" & health_structure != "Unspecified")

      if (!nrow(df_valid)) return(tibble::tibble())

      df_valid %>%
        dplyr::group_by(health_structure) %>%
        dplyr::summarise(
          n_samples = dplyr::n(),
          mean_volume = safe_mean(drs_volume_ml),
          expected_volume = safe_mean(expected_volume_ml),
          volume_deviation = safe_mean(volume_deviation_ml),
          pct_volume_ok = mean(volume_status == "Normal", na.rm = TRUE) * 100,
          pct_volume_warning = mean(volume_status %in% c("Low", "High"), na.rm = TRUE) * 100,
          pct_volume_critical = mean(volume_status %in% c("Critical Low", "Critical High"), na.rm = TRUE) * 100,
          mean_preanalytical = safe_mean(preanalytical_days),
          pct_time_optimal = mean(time_category == "Optimal", na.rm = TRUE) * 100,
          mean_rnasep_dna = safe_mean(rnasep_dna_cq),
          mean_rnasep_rna = safe_mean(rnasep_rna_cq),
          mean_delta_rp = safe_mean(delta_rp),
          pct_qc_ok = mean(qc_status == "OK", na.rm = TRUE) * 100,
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          qc_score = pct_volume_ok * 0.4 +
                     dplyr::coalesce(pct_time_optimal, 100) * 0.3 +
                     dplyr::if_else(is.na(mean_rnasep_dna), 100, pmax(0, 100 - (mean_rnasep_dna - 25) * 3)) * 0.3
        ) %>%
        dplyr::arrange(dplyr::desc(n_samples))
    })

    # ========================================================================
    # QC ALERT BANNER
    # ========================================================================

    output$qc_alert_banner <- renderUI({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return(NULL)

      critical_count <- sum(df$qc_status == "Critical", na.rm = TRUE)
      warning_count <- sum(df$qc_status == "Warning", na.rm = TRUE)

      if (critical_count > 0) {
        div(
          class = "alert alert-danger d-flex align-items-center mb-3",
          icon("exclamation-triangle", class = "me-2"),
          HTML(sprintf(
            "<strong>%d critical issues</strong> detected (%d warnings). Review QC Warnings tab for details.",
            critical_count, warning_count
          ))
        )
      } else if (warning_count > 0) {
        div(
          class = "alert alert-warning d-flex align-items-center mb-3",
          icon("exclamation-triangle", class = "me-2"),
          HTML(sprintf(
            "<strong>%d warnings</strong> detected. Review QC Warnings tab for details.",
            warning_count
          ))
        )
      } else {
        NULL
      }
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$kpi_total <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(nrow(df))
    })

    output$kpi_volume_ok <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("--")
      n_ok <- sum(df$volume_status == "Normal", na.rm = TRUE)
      pct <- n_ok / nrow(df) * 100
      sprintf("%s (%.0f%%)", scales::comma(n_ok), pct)
    })

    output$kpi_volume_warnings <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("--")
      n_warn <- sum(df$has_volume_warning, na.rm = TRUE)
      pct <- n_warn / nrow(df) * 100
      sprintf("%s (%.0f%%)", scales::comma(n_warn), pct)
    })

    output$kpi_rnasep_dna <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("--")
      mean_cq <- safe_mean(df$rnasep_dna_cq)
      if (is.na(mean_cq)) return("--")
      sprintf("%.1f", mean_cq)
    })

    output$kpi_rnasep_rna <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("--")
      mean_cq <- safe_mean(df$rnasep_rna_cq)
      if (is.na(mean_cq)) return("--")
      sprintf("%.1f", mean_cq)
    })

    output$kpi_preanalytical <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("--")
      mean_days <- safe_mean(df$preanalytical_days)
      if (is.na(mean_days)) return("--")
      sprintf("%.1f days", mean_days)
    })

    output$kpi_critical_count <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(df$qc_status == "Critical", na.rm = TRUE))
    })

    output$kpi_warning_count <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("0")
      scales::comma(sum(df$qc_status == "Warning", na.rm = TRUE))
    })

    output$kpi_ok_count <- renderText({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) return("0")
      n_ok <- sum(df$qc_status == "OK", na.rm = TRUE)
      pct <- n_ok / nrow(df) * 100
      sprintf("%s (%.0f%%)", scales::comma(n_ok), pct)
    })

    # ========================================================================
    # VOLUME ANALYSIS PLOTS
    # ========================================================================

    output$volume_by_barcode_type <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(!is.na(drs_volume_ml), barcode_type != "Unknown")

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No volume data"))
      }

      plotly::plot_ly(
        plot_data,
        x = ~barcode_type,
        y = ~drs_volume_ml,
        type = "box",
        color = ~barcode_type,
        colors = c("Old (<2502200, 2mL)" = "#3498DB", "New (>=2502200, 4mL)" = "#E74C3C")
      ) %>%
        plotly::layout(
          xaxis = list(title = "Barcode Type"),
          yaxis = list(title = "DRS Volume (mL)"),
          showlegend = FALSE,
          shapes = list(
            list(type = "line", x0 = -0.5, x1 = 0.5, y0 = 2, y1 = 2,
                 line = list(color = "#3498DB", dash = "dash", width = 2)),
            list(type = "line", x0 = 0.5, x1 = 1.5, y0 = 4, y1 = 4,
                 line = list(color = "#E74C3C", dash = "dash", width = 2))
          )
        )
    })

    output$volume_status_plot <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::count(volume_status) %>%
        dplyr::mutate(
          volume_status = factor(volume_status,
            levels = c("Normal", "Low", "High", "Critical Low", "Critical High", "Missing", "Unknown"))
        )

      status_colors <- c(
        "Normal" = "#27AE60",
        "Low" = "#F39C12",
        "High" = "#E67E22",
        "Critical Low" = "#E74C3C",
        "Critical High" = "#C0392B",
        "Missing" = "#95A5A6",
        "Unknown" = "#BDC3C7"
      )

      plotly::plot_ly(
        plot_data,
        x = ~volume_status,
        y = ~n,
        type = "bar",
        color = ~volume_status,
        colors = status_colors
      ) %>%
        plotly::layout(
          xaxis = list(title = "Volume Status"),
          yaxis = list(title = "Count"),
          showlegend = FALSE
        )
    })

    output$volume_deviation_plot <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(!is.na(drs_volume_ml), !is.na(expected_volume_ml)) %>%
        dplyr::arrange(extraction_date) %>%
        dplyr::mutate(row_idx = dplyr::row_number())

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No volume data"))
      }

      status_colors <- c(
        "Normal" = "#27AE60",
        "Low" = "#F39C12",
        "High" = "#E67E22",
        "Critical Low" = "#E74C3C",
        "Critical High" = "#C0392B",
        "Unknown" = "#BDC3C7"
      )

      plotly::plot_ly(
        plot_data,
        x = ~row_idx,
        y = ~drs_volume_ml,
        type = "scatter",
        mode = "markers",
        color = ~volume_status,
        colors = status_colors,
        marker = list(size = 6, opacity = 0.7),
        text = ~paste0(
          "Barcode: ", barcode_display, "<br>",
          "Volume: ", round(drs_volume_ml, 2), " mL<br>",
          "Expected: ", expected_volume_ml, " mL<br>",
          "Deviation: ", round(volume_deviation_ml, 2), " mL<br>",
          "Date: ", extraction_date
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Sample (ordered by date)"),
          yaxis = list(title = "DRS Volume (mL)"),
          shapes = list(
            # 2 mL reference line
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 2, y1 = 2,
                 line = list(color = "#3498DB", dash = "dash", width = 1)),
            # 4 mL reference line
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 4, y1 = 4,
                 line = list(color = "#E74C3C", dash = "dash", width = 1))
          ),
          annotations = list(
            list(x = 1, xref = "paper", y = 2, text = "2 mL (old)", showarrow = FALSE,
                 xanchor = "left", font = list(size = 10, color = "#3498DB")),
            list(x = 1, xref = "paper", y = 4, text = "4 mL (new)", showarrow = FALSE,
                 xanchor = "left", font = list(size = 10, color = "#E74C3C"))
          )
        )
    })

    output$volume_timeseries <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      ts_data <- df %>%
        dplyr::filter(!is.na(extraction_date), !is.na(drs_volume_ml)) %>%
        dplyr::mutate(week = lubridate::floor_date(extraction_date, "week")) %>%
        dplyr::group_by(week, barcode_type) %>%
        dplyr::summarise(
          mean_volume = safe_mean(drs_volume_ml),
          n = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::filter(barcode_type != "Unknown")

      if (!nrow(ts_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No dated volume data"))
      }

      plotly::plot_ly(
        ts_data,
        x = ~week,
        y = ~mean_volume,
        color = ~barcode_type,
        colors = c("Old (<2502200, 2mL)" = "#3498DB", "New (>=2502200, 4mL)" = "#E74C3C"),
        type = "scatter",
        mode = "lines+markers",
        text = ~paste0("Week: ", week, "<br>Mean Volume: ", round(mean_volume, 2), " mL<br>N: ", n),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Week"),
          yaxis = list(title = "Mean DRS Volume (mL)"),
          shapes = list(
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 2, y1 = 2,
                 line = list(color = "#3498DB", dash = "dash", width = 1)),
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 4, y1 = 4,
                 line = list(color = "#E74C3C", dash = "dash", width = 1))
          )
        )
    })

    # ========================================================================
    # RNASEP ANALYSIS PLOTS
    # ========================================================================

    output$time_vs_dna <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(!is.na(preanalytical_days), !is.na(rnasep_dna_cq), !is.infinite(rnasep_dna_cq))

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No linked pre-analytical/RNAseP data"))
      }

      time_colors <- c(
        "Optimal" = "#27AE60",
        "Acceptable" = "#F39C12",
        "Caution" = "#E67E22",
        "Problematic" = "#E74C3C",
        "Critical" = "#C0392B",
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
        marker = list(size = 8, opacity = 0.7),
        text = ~paste0("Days: ", round(preanalytical_days, 1), "<br>DNA Cq: ", round(rnasep_dna_cq, 2)),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Pre-Analytical Time (days)"),
          yaxis = list(title = "RNAseP DNA Cq (lower = better)")
        )
    })

    output$time_vs_rna <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(!is.na(preanalytical_days), !is.na(rnasep_rna_cq), !is.infinite(rnasep_rna_cq))

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No linked pre-analytical/RNAseP data"))
      }

      time_colors <- c(
        "Optimal" = "#27AE60",
        "Acceptable" = "#F39C12",
        "Caution" = "#E67E22",
        "Problematic" = "#E74C3C",
        "Critical" = "#C0392B",
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
        marker = list(size = 8, opacity = 0.7),
        text = ~paste0("Days: ", round(preanalytical_days, 1), "<br>RNA Cq: ", round(rnasep_rna_cq, 2)),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Pre-Analytical Time (days)"),
          yaxis = list(title = "RNAseP RNA Cq (lower = better)")
        )
    })

    output$temp_boxplot <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(temp_category != "Unknown") %>%
        dplyr::select(temp_category, rnasep_dna_cq, rnasep_rna_cq) %>%
        tidyr::pivot_longer(
          cols = c(rnasep_dna_cq, rnasep_rna_cq),
          names_to = "assay",
          values_to = "cq"
        ) %>%
        dplyr::filter(!is.na(cq), !is.infinite(cq)) %>%
        dplyr::mutate(assay = dplyr::if_else(assay == "rnasep_dna_cq", "DNA", "RNA"))

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No temperature/RNAseP data"))
      }

      temp_colors <- c("Optimal" = "#27AE60", "Suboptimal" = "#E74C3C")

      plotly::plot_ly(
        plot_data,
        x = ~assay,
        y = ~cq,
        color = ~temp_category,
        colors = temp_colors,
        type = "box"
      ) %>%
        plotly::layout(
          xaxis = list(title = ""),
          yaxis = list(title = "Cq Value (lower = better)"),
          boxmode = "group"
        )
    })

    output$delta_rp_hist <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(!is.na(delta_rp), !is.infinite(delta_rp))

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No Delta RP data"))
      }

      plotly::plot_ly(
        plot_data,
        x = ~delta_rp,
        type = "histogram",
        marker = list(color = "#9B59B6", line = list(color = "#8E44AD", width = 1))
      ) %>%
        plotly::layout(
          xaxis = list(title = "Delta RP (RNA Cq - DNA Cq)"),
          yaxis = list(title = "Count"),
          shapes = list(
            list(type = "line", x0 = 3, x1 = 3, y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#F39C12", dash = "dash", width = 2)),
            list(type = "line", x0 = 5, x1 = 5, y0 = 0, y1 = 1, yref = "paper",
                 line = list(color = "#E74C3C", dash = "dash", width = 2))
          )
        )
    })

    output$rnasep_scatter <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      plot_data <- df %>%
        dplyr::filter(
          !is.na(rnasep_dna_cq), !is.infinite(rnasep_dna_cq),
          !is.na(rnasep_rna_cq), !is.infinite(rnasep_rna_cq)
        )

      if (!nrow(plot_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No RNAseP data"))
      }

      status_colors <- c("OK" = "#27AE60", "Warning" = "#F39C12", "Critical" = "#E74C3C")

      plotly::plot_ly(
        plot_data,
        x = ~rnasep_dna_cq,
        y = ~rnasep_rna_cq,
        color = ~qc_status,
        colors = status_colors,
        type = "scatter",
        mode = "markers",
        marker = list(size = 8, opacity = 0.7),
        text = ~paste0(
          "Sample: ", barcode_display, "<br>",
          "DNA Cq: ", round(rnasep_dna_cq, 2), "<br>",
          "RNA Cq: ", round(rnasep_rna_cq, 2), "<br>",
          "Delta RP: ", round(delta_rp, 2), "<br>",
          "Status: ", qc_status
        ),
        hoverinfo = "text"
      ) %>%
        plotly::layout(
          xaxis = list(title = "RNAseP DNA Cq"),
          yaxis = list(title = "RNAseP RNA Cq"),
          shapes = list(
            list(type = "line", x0 = 20, x1 = 40, y0 = 20, y1 = 40,
                 line = list(color = "#95A5A6", dash = "dot", width = 1))
          )
        )
    })

    # ========================================================================
    # STRUCTURE SANITAIRE OUTPUTS
    # ========================================================================

    output$structure_stats_table <- DT::renderDT({
      stats <- structure_stats()
      if (is.null(stats) || !nrow(stats)) {
        return(DT::datatable(tibble::tibble(Message = "No structure data available")))
      }

      display_df <- stats %>%
        dplyr::transmute(
          `Structure` = health_structure,
          `N` = n_samples,
          `Mean Vol (mL)` = round(mean_volume, 2),
          `Expected (mL)` = round(expected_volume, 1),
          `Vol Deviation` = round(volume_deviation, 2),
          `% Vol OK` = sprintf("%.0f%%", pct_volume_ok),
          `% Vol Warn` = sprintf("%.0f%%", pct_volume_warning + pct_volume_critical),
          `Mean Days` = dplyr::if_else(is.na(mean_preanalytical), "--", sprintf("%.1f", mean_preanalytical)),
          `Mean DNA Cq` = dplyr::if_else(is.na(mean_rnasep_dna), "--", sprintf("%.1f", mean_rnasep_dna)),
          `Mean RNA Cq` = dplyr::if_else(is.na(mean_rnasep_rna), "--", sprintf("%.1f", mean_rnasep_rna)),
          `QC Score` = sprintf("%.0f", qc_score)
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 15, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        filter = "top"
      )
    })

    output$structure_volume_chart <- plotly::renderPlotly({
      stats <- structure_stats()
      if (is.null(stats) || !nrow(stats)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No structure data"))
      }

      plot_data <- stats %>%
        dplyr::mutate(health_structure = factor(health_structure, levels = rev(health_structure)))

      # Dynamic height based on number of structures
      chart_height <- max(400, nrow(plot_data) * 25)

      plotly::plot_ly(plot_data, height = chart_height) %>%
        plotly::add_bars(
          x = ~pct_volume_ok, y = ~health_structure, name = "OK",
          marker = list(color = "#27AE60"), orientation = "h"
        ) %>%
        plotly::add_bars(
          x = ~pct_volume_warning, y = ~health_structure, name = "Warning",
          marker = list(color = "#F39C12"), orientation = "h"
        ) %>%
        plotly::add_bars(
          x = ~pct_volume_critical, y = ~health_structure, name = "Critical",
          marker = list(color = "#E74C3C"), orientation = "h"
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Percentage", range = c(0, 100)),
          yaxis = list(title = ""),
          margin = list(l = 150)
        )
    })

    output$structure_qc_score <- plotly::renderPlotly({
      stats <- structure_stats()
      if (is.null(stats) || !nrow(stats)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No structure data"))
      }

      plot_data <- stats %>%
        dplyr::arrange(qc_score) %>%
        dplyr::mutate(
          health_structure = factor(health_structure, levels = health_structure),
          color = dplyr::case_when(
            qc_score >= 80 ~ "#27AE60",
            qc_score >= 60 ~ "#F39C12",
            TRUE ~ "#E74C3C"
          )
        )

      # Dynamic height based on number of structures
      chart_height <- max(400, nrow(plot_data) * 25)

      plotly::plot_ly(
        plot_data,
        x = ~qc_score,
        y = ~health_structure,
        type = "bar",
        orientation = "h",
        marker = list(color = ~color),
        text = ~sprintf("%.0f", qc_score),
        textposition = "outside",
        height = chart_height
      ) %>%
        plotly::layout(
          xaxis = list(title = "QC Score (0-100)", range = c(0, 110)),
          yaxis = list(title = ""),
          margin = list(l = 150)
        )
    })

    output$structures_with_issues <- DT::renderDT({
      stats <- structure_stats()
      if (is.null(stats) || !nrow(stats)) {
        return(DT::datatable(tibble::tibble(Message = "No structure data")))
      }

      issues_df <- stats %>%
        dplyr::filter(
          pct_volume_warning + pct_volume_critical > 20 |
          (!is.na(mean_preanalytical) & mean_preanalytical > 14) |
          (!is.na(mean_rnasep_dna) & mean_rnasep_dna > 30)
        ) %>%
        dplyr::mutate(
          issue_type = dplyr::case_when(
            pct_volume_critical > 10 ~ "Critical volume issues",
            pct_volume_warning > 20 ~ "Volume warnings",
            !is.na(mean_preanalytical) & mean_preanalytical > 14 ~ "Pre-analytical time",
            !is.na(mean_rnasep_dna) & mean_rnasep_dna > 30 ~ "RNAseP quality",
            TRUE ~ "Multiple issues"
          )
        ) %>%
        dplyr::transmute(
          `Structure` = health_structure,
          `N` = n_samples,
          `Issue` = issue_type,
          `% Vol OK` = sprintf("%.0f%%", pct_volume_ok),
          `Mean Days` = dplyr::if_else(is.na(mean_preanalytical), "--", sprintf("%.1f", mean_preanalytical)),
          `Mean DNA Cq` = dplyr::if_else(is.na(mean_rnasep_dna), "--", sprintf("%.1f", mean_rnasep_dna)),
          `QC Score` = sprintf("%.0f", qc_score)
        )

      if (!nrow(issues_df)) {
        return(DT::datatable(tibble::tibble(Message = "No structures with significant issues")))
      }

      DT::datatable(
        issues_df,
        options = list(pageLength = 10, dom = "tip"),
        rownames = FALSE
      )
    })

    # ========================================================================
    # QC WARNINGS OUTPUTS
    # ========================================================================

    output$warning_types_chart <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      warning_counts <- tibble::tibble(
        type = c("Volume", "Time", "RNAseP"),
        count = c(
          sum(df$has_volume_warning, na.rm = TRUE),
          sum(df$has_time_warning, na.rm = TRUE),
          sum(df$has_rnasep_warning, na.rm = TRUE)
        )
      )

      plotly::plot_ly(
        warning_counts,
        x = ~type,
        y = ~count,
        type = "bar",
        marker = list(color = c("#E67E22", "#3498DB", "#9B59B6"))
      ) %>%
        plotly::layout(
          xaxis = list(title = "Warning Type"),
          yaxis = list(title = "Count")
        )
    })

    output$qc_status_timeline <- plotly::renderPlotly({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No data available"))
      }

      ts_data <- df %>%
        dplyr::filter(!is.na(extraction_date)) %>%
        dplyr::mutate(week = lubridate::floor_date(extraction_date, "week")) %>%
        dplyr::group_by(week, qc_status) %>%
        dplyr::summarise(n = dplyr::n(), .groups = "drop")

      if (!nrow(ts_data)) {
        return(plotly::plot_ly() %>% plotly::layout(title = "No dated data"))
      }

      status_colors <- c("OK" = "#27AE60", "Warning" = "#F39C12", "Critical" = "#E74C3C")

      plotly::plot_ly(
        ts_data,
        x = ~week,
        y = ~n,
        color = ~qc_status,
        colors = status_colors,
        type = "bar"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Week"),
          yaxis = list(title = "Samples")
        )
    })

    output$samples_with_warnings <- DT::renderDT({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      warnings_df <- df %>%
        dplyr::filter(qc_status != "OK") %>%
        dplyr::arrange(dplyr::desc(qc_status == "Critical"), dplyr::desc(extraction_date)) %>%
        dplyr::transmute(
          `Status` = qc_status,
          `Barcode` = barcode_display,
          `Date` = format(extraction_date, "%Y-%m-%d"),
          `Volume (mL)` = round(drs_volume_ml, 2),
          `Expected (mL)` = expected_volume_ml,
          `Vol Status` = volume_status,
          `Days` = round(preanalytical_days, 1),
          `DNA Cq` = round(rnasep_dna_cq, 1),
          `RNA Cq` = round(rnasep_rna_cq, 1),
          `Structure` = dplyr::coalesce(health_structure, "Unspecified")
        )

      if (!nrow(warnings_df)) {
        return(DT::datatable(tibble::tibble(Message = "No samples with QC warnings")))
      }

      DT::datatable(
        warnings_df,
        options = list(pageLength = 20, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        filter = "top"
      )
    })

    # ========================================================================
    # SAMPLE DETAILS OUTPUTS
    # ========================================================================

    freezer_results_val <- reactiveVal(tibble::tibble())

    observeEvent(input$freezer_search_btn, {
      df <- combined_data()
      search_term <- trimws(input$freezer_search)

      if (is.null(df) || !nrow(df) || search_term == "") {
        freezer_results_val(tibble::tibble())
        return()
      }

      search_norm <- tolower(gsub("[^a-z0-9]", "", search_term))

      results <- df %>%
        dplyr::mutate(
          .search_col = tolower(gsub("[^a-z0-9]", "", dplyr::coalesce(barcode_display, numero, "")))
        ) %>%
        dplyr::filter(grepl(search_norm, .search_col, fixed = TRUE)) %>%
        dplyr::select(-.search_col)

      freezer_results_val(results)
    })

    output$freezer_results <- DT::renderDT({
      results <- freezer_results_val()
      if (!nrow(results)) {
        return(DT::datatable(tibble::tibble(Message = "No results. Enter barcode and click Search.")))
      }

      display_df <- results %>%
        dplyr::transmute(
          `Barcode` = barcode_display,
          `Numero` = numero,
          `Date` = format(extraction_date, "%Y-%m-%d"),
          `Volume (mL)` = round(drs_volume_ml, 2),
          `Status` = volume_status,
          `Rack` = rack,
          `Row` = rack_row,
          `Position` = rack_column
        )

      DT::datatable(display_df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
    })

    output$sample_table <- DT::renderDT({
      df <- combined_data()
      if (is.null(df) || !nrow(df)) {
        return(DT::datatable(tibble::tibble(Message = "No data available")))
      }

      display_df <- df %>%
        dplyr::arrange(dplyr::desc(extraction_date)) %>%
        dplyr::transmute(
          `QC` = qc_status,
          `Barcode` = barcode_display,
          `Numero` = numero,
          `Date` = format(extraction_date, "%Y-%m-%d"),
          `Volume (mL)` = round(drs_volume_ml, 2),
          `Expected (mL)` = expected_volume_ml,
          `Vol Status` = volume_status,
          `Days` = round(preanalytical_days, 1),
          `DNA Cq` = round(rnasep_dna_cq, 1),
          `RNA Cq` = round(rnasep_rna_cq, 1),
          `Delta RP` = round(delta_rp, 1),
          `Structure` = health_structure,
          `Technician` = technician,
          `Extract Quality` = extract_quality,
          `DRS State` = drs_state
        )

      DT::datatable(
        display_df,
        options = list(pageLength = 25, scrollX = TRUE, dom = "frtip"),
        rownames = FALSE,
        filter = "top"
      )
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    output$download_structure_stats <- downloadHandler(
      filename = function() {
        paste0("drs_structure_stats_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(structure_stats(), file)
      }
    )

    output$download_sample_data <- downloadHandler(
      filename = function() {
        paste0("drs_sample_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        readr::write_csv(combined_data(), file)
      }
    )
  })
}
