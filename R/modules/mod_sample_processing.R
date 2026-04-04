# ==============================================================================
# MODULE: SAMPLE PROCESSING - Comprehensive sample tracking across all tests
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(DT)
  library(plotly)
})

#' Safely get column from data frame (returns NULL if not present)
#' @param df Data frame
#' @param col_name Column name
#' @return Column values or NULL if column doesn't exist
safe_get_col <- function(df, col_name) {
  if (col_name %in% names(df)) df[[col_name]] else NULL
}

#' Normalize sample ID for consistent matching across datasets
#'
#' Less aggressive normalization to prevent sample ID collisions.
#' Preserves dashes, underscores, and dots to maintain uniqueness.
#'
#' @param barcode Barcode value
#' @param lab_id Lab ID value
#' @return Normalized ID string
normalize_sample_id <- function(barcode = NULL, lab_id = NULL) {
  ids <- c(barcode, lab_id)
  ids <- ids[!is.na(ids) & ids != ""]
  if (!length(ids)) return(NA_character_)

  # Convert to string and trim whitespace
  ids <- trimws(as.character(ids))

  # Remove KPS prefix (case-insensitive)
  ids <- gsub("^[Kk][Pp][Ss][-_]?", "", ids)

  # Convert to lowercase for case-insensitive matching
  ids <- tolower(ids)

  # Only remove spaces and special punctuation, but keep dashes, underscores, dots
  ids <- gsub("[^a-z0-9._-]", "", ids)

  # Remove any resulting empty strings
  ids <- ids[ids != ""]
  if (!length(ids)) return(NA_character_)

  ids[1]
}

#' Convert a character vector to numeric if ALL non-NA values are valid numbers.
#' Returns the original vector unchanged if any non-NA value is non-numeric.
#' This ensures Excel writes the column as a number (sortable 1→10000)
#' rather than as text (which sorts 1, 10, 100, …).
#' @param x Character (or any) vector
#' @return Numeric vector, or original vector if conversion would lose data
try_numeric <- function(x) {
  nums <- suppressWarnings(as.numeric(x))
  # Only convert if no NEW NAs were introduced (i.e. every non-NA became a number)
  if (all(is.na(nums) == is.na(x))) nums else x
}

#' Safely format the most recent date in a vector to dd/mm/yyyy
#' Returns NA_character_ if no valid dates are present
#' @param dates Vector of dates (Date or POSIXct)
#' @return Character string "dd/mm/yyyy" or NA_character_
safe_latest_date_dmy <- function(dates) {
  if (is.null(dates) || length(dates) == 0) return(NA_character_)
  valid <- tryCatch(as.Date(dates[!is.na(dates)]), error = function(e) as.Date(character(0)))
  if (length(valid) == 0) return(NA_character_)
  tryCatch(format(max(valid), "%d/%m/%Y"), error = function(e) NA_character_)
}

#' Sample Processing UI
#' @param id Module namespace ID
#' @export
mod_sample_processing_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Sample Processing",
    icon = icon("list-check"),

    tagList(
      tags$style(HTML("
        .sample-processing-panel {
          max-height: calc(100vh - 160px);
          overflow-y: auto;
          padding-right: 0.5rem;
          padding-bottom: 1rem;
        }
      ")),

      div(
        class = "sample-processing-panel",

        # Summary KPIs
        card(
          class = "mb-3",
          card_header("Sample Processing Overview"),
          card_body(
            uiOutput(ns("summary_kpis"))
          )
        ),

        # Filters Card
        card(
          class = "mb-3",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              span(icon("filter"), " Filters"),
              actionButton(ns("reset_filters"), "Reset", class = "btn-sm btn-outline-secondary")
            )
          ),
          card_body(
            # First row - 2 main filters
            layout_column_wrap(
              width = 1/2,
              selectInput(
                ns("filter_positivity"),
                "Positivity Status",
                choices = c(
                  "All Samples" = "all",
                  "Any Positive Result" = "any_positive",
                  "All Negative" = "all_negative",
                  "MIC Positive (TNA)" = "mic_positive",
                  "MIC Positive DNA" = "mic_positive_dna",
                  "MIC Positive RNA" = "mic_positive_rna",
                  "ELISA Positive" = "elisa_positive",
                  "iELISA Positive" = "ielisa_positive"
                ),
                selected = "all"
              ),
              selectInput(
                ns("filter_completeness"),
                "Data Completeness",
                choices = c(
                  "All Samples" = "all",
                  "Fully Tested (All 3)" = "fully_tested",
                  "Partially Tested" = "partially_tested",
                  "Biobank Only" = "biobank_only"
                ),
                selected = "all"
              )
            ),
            # Second row - 2 additional filters
            layout_column_wrap(
              width = 1/2,
              selectInput(
                ns("filter_processing"),
                "Processing Status",
                choices = c(
                  "All Samples" = "all",
                  "Extracted" = "has_extraction",
                  "Not Extracted" = "no_extraction",
                  "Has MIC Test" = "has_mic",
                  "Has ELISA-PE Test" = "has_elisa_pe",
                  "Has ELISA-VSG Test" = "has_elisa_vsg",
                  "Has Any ELISA Test" = "has_elisa",
                  "Has iELISA Test" = "has_ielisa",
                  "Complete Testing" = "complete"
                ),
                selected = "all"
              ),
              selectInput(
                ns("filter_qc"),
                "QC Status",
                choices = c(
                  "All Samples" = "all",
                  "QC Pass Only" = "qc_pass",
                  "QC Fail Only" = "qc_fail"
                ),
                selected = "all"
              )
            )
          )
        ),

        # Processing stages visualization
        card(
          class = "mb-3",
          card_header("Sample Flow Through Processing Stages"),
          card_body(
            plotlyOutput(ns("processing_flow"), height = "300px")
          )
        ),

        # Sample Details (wide format)
        card(
          class = "mb-3",
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              span("Sample Details"),
              div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("download_csv"), "CSV", class = "btn-sm"),
                downloadButton(ns("download_excel"), "Excel (Wide)", class = "btn-sm"),
                downloadButton(ns("download_long_excel"), "Excel (Long)", class = "btn-sm")
              )
            )
          ),
          card_body(
            DTOutput(ns("samples_table"))
          )
        ),

        # Test History (long format)
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              span("Test History (Long Format)"),
              downloadButton(ns("download_long_excel2"), "Download Excel", class = "btn-sm")
            )
          ),
          card_body(
            DTOutput(ns("long_format_table"))
          )
        )
      )
    )
  )
}

#' Sample Processing Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data
#' @param extraction_df Reactive returning extraction data
#' @param mic_df Reactive returning MIC data
#' @param elisa_pe_df Reactive returning ELISA PE data
#' @param elisa_vsg_df Reactive returning ELISA VSG data
#' @param ielisa_df Reactive returning iELISA data
#' @param filters Reactive filters from data manager
#' @export
mod_sample_processing_server <- function(id, biobank_df, extraction_df, mic_df,
                                          elisa_pe_df, elisa_vsg_df, ielisa_df, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # REACTIVE DATA PROCESSING
    # ========================================================================

    # Comprehensive sample data combining all sources
    comprehensive_samples <- reactive({
      req(biobank_df())

      biobank <- biobank_df()

      # Get extraction data
      extractions <- tryCatch(extraction_df(), error = function(e) tibble())

      # Get test data
      mic_data <- tryCatch(mic_df(), error = function(e) tibble())
      elisa_pe_data <- tryCatch(elisa_pe_df(), error = function(e) tibble())
      elisa_vsg_data <- tryCatch(elisa_vsg_df(), error = function(e) tibble())
      ielisa_data <- tryCatch(ielisa_df(), error = function(e) tibble())

      # Start with biobank data (already cleaned with standardized column names)
      samples <- biobank %>%
        mutate(
          sample_id = map_chr(barcode, ~normalize_sample_id(barcode = .x))
        ) %>%
        filter(!is.na(sample_id) & sample_id != "") %>%
        select(
          sample_id, barcode, lab_id,
          province, health_zone,
          starts_with("date_"),
          everything()
        )

      # Add extraction info
      if (nrow(extractions) > 0) {
        extraction_summary <- extractions %>%
          mutate(
            sample_id = map_chr(barcode, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_extraction = TRUE,
            n_extractions = n(),
            latest_extraction_date = max(extraction_date, na.rm = TRUE),
            extraction_date_display = safe_latest_date_dmy(extraction_date),
            extraction_qc_results = paste(
              ifelse(coalesce(extract_quality, "Clear") != "Échec", "PASS", "FAIL"),
              collapse = ", "
            ),
            drs_volume = last(drs_volume_ml, order_by = extraction_date),
            .groups = "drop"
          )

        samples <- samples %>%
          left_join(extraction_summary, by = "sample_id") %>%
          mutate(has_extraction = replace_na(has_extraction, FALSE))
      } else {
        samples <- samples %>%
          mutate(
            has_extraction = FALSE,
            n_extractions = 0,
            latest_extraction_date = as.Date(NA),
            extraction_date_display = NA_character_,
            extraction_qc_results = NA_character_,
            drs_volume = NA_real_
          )
      }

      # Build biobank lookup for MIC lab-number matching
      biobank_lookup <- NULL
      if (nrow(biobank) > 0) {
        col_numero_labo <- safe_get_col(biobank, "numero_labo")
        col_lab_id <- safe_get_col(biobank, "lab_id")
        col_numero <- safe_get_col(biobank, "numero")
        cols_to_coalesce <- Filter(Negate(is.null), list(col_numero_labo, col_lab_id, col_numero))
        biobank_lab_numbers_raw <- if (length(cols_to_coalesce) > 0) {
          do.call(coalesce, cols_to_coalesce)
        } else {
          rep(NA_character_, nrow(biobank))
        }
        biobank_lab_numbers_norm <- sapply(biobank_lab_numbers_raw, function(x) {
          if (is.na(x) || x == "") return(NA_character_)
          trimws(tolower(as.character(x)))
        })
        biobank_lookup <- tibble(
          lab_number_norm = biobank_lab_numbers_norm,
          sample_id = samples$sample_id
        ) %>%
          filter(!is.na(lab_number_norm) & lab_number_norm != "" &
                 !is.na(sample_id) & sample_id != "") %>%
          distinct(lab_number_norm, .keep_all = TRUE)
      }

      # Add MIC test info
      if (nrow(mic_data) > 0) {
        mic_col_sampleid <- safe_get_col(mic_data, "SampleID")
        mic_col_samplename <- safe_get_col(mic_data, "SampleName")
        mic_col_name <- safe_get_col(mic_data, "Name")
        mic_cols_to_coalesce <- Filter(Negate(is.null), list(mic_col_sampleid, mic_col_samplename, mic_col_name))
        mic_lab_numbers_raw <- if (length(mic_cols_to_coalesce) > 0) {
          do.call(coalesce, mic_cols_to_coalesce)
        } else {
          rep(NA_character_, nrow(mic_data))
        }
        mic_lab_numbers_norm <- sapply(mic_lab_numbers_raw, function(x) {
          if (is.na(x) || x == "") return(NA_character_)
          trimws(tolower(as.character(x)))
        })

        mic_with_lab_numbers <- mic_data %>%
          mutate(
            lab_number_norm = mic_lab_numbers_norm,
            QC_Pass_Count = coalesce(ReplicatesTotal, 4) - coalesce(Replicates_Failed, 0)
          )

        if (!is.null(biobank_lookup) && nrow(biobank_lookup) > 0) {
          mic_summary <- mic_with_lab_numbers %>%
            left_join(biobank_lookup, by = "lab_number_norm") %>%
            filter(!is.na(sample_id) & sample_id != "") %>%
            group_by(sample_id) %>%
            summarise(
              has_mic = TRUE,
              n_mic_tests = n(),
              mic_results = paste(
                case_when(
                  FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA", "LatePositive") ~ "+",
                  FinalCall == "Negative" ~ "-",
                  FinalCall %in% c("Indeterminate", "Invalid_NoDNA") ~ "?",
                  TRUE ~ FinalCall
                ),
                collapse = ", "
              ),
              mic_positive = any(FinalCall == "Positive", na.rm = TRUE),
              mic_positive_dna = any(FinalCall == "Positive_DNA", na.rm = TRUE),
              mic_positive_rna = any(FinalCall == "Positive_RNA", na.rm = TRUE),
              mic_result = first(FinalCall),
              mic_qc_pass = all(QC_Pass_Count > 0, na.rm = TRUE),
              mic_date_display = safe_latest_date_dmy(as.Date(RunDate)),
              mic_qc_results = paste(ifelse(QC_Pass_Count > 0, "PASS", "FAIL"), collapse = ", "),
              # Replicate information
              mic_wells_total = first(coalesce(ReplicatesTotal, 4)),
              mic_wells_tna_positive = first(coalesce(Wells_TNA_Positive, 0)),
              mic_wells_dna_positive = first(coalesce(Wells_DNA_Positive, 0)),
              mic_wells_rna_positive = first(coalesce(Wells_RNA_Positive, 0)),
              .groups = "drop"
            )
        } else {
          mic_summary <- tibble()
        }

        if (nrow(mic_summary) > 0) {
          samples <- samples %>%
            left_join(mic_summary, by = "sample_id") %>%
            mutate(
              has_mic = replace_na(has_mic, FALSE),
              mic_results = replace_na(mic_results, "✗"),
              mic_positive = replace_na(mic_positive, FALSE),
              mic_positive_dna = replace_na(mic_positive_dna, FALSE),
              mic_positive_rna = replace_na(mic_positive_rna, FALSE),
              mic_qc_pass = replace_na(mic_qc_pass, TRUE),
              mic_wells_total = replace_na(mic_wells_total, 4),
              mic_wells_tna_positive = replace_na(mic_wells_tna_positive, 0),
              mic_wells_dna_positive = replace_na(mic_wells_dna_positive, 0),
              mic_wells_rna_positive = replace_na(mic_wells_rna_positive, 0)
            )
        } else {
          samples <- samples %>%
            mutate(
              has_mic = FALSE,
              n_mic_tests = 0,
              mic_results = "✗",
              mic_positive = FALSE,
              mic_positive_dna = FALSE,
              mic_positive_rna = FALSE,
              mic_result = NA_character_,
              mic_qc_pass = TRUE,
              mic_date_display = NA_character_,
              mic_qc_results = NA_character_,
              mic_wells_total = 4,
              mic_wells_tna_positive = 0,
              mic_wells_dna_positive = 0,
              mic_wells_rna_positive = 0
            )
        }
      } else {
        samples <- samples %>%
          mutate(
            has_mic = FALSE,
            n_mic_tests = 0,
            mic_results = "✗",
            mic_positive = FALSE,
            mic_positive_dna = FALSE,
            mic_positive_rna = FALSE,
            mic_result = NA_character_,
            mic_qc_pass = TRUE,
            mic_date_display = NA_character_,
            mic_qc_results = NA_character_,
            mic_wells_total = 4,
            mic_wells_tna_positive = 0,
            mic_wells_dna_positive = 0,
            mic_wells_rna_positive = 0
          )
      }

      # Add ELISA PE test info
      if (nrow(elisa_pe_data) > 0) {
        elisa_pe_summary <- elisa_pe_data %>%
          filter(sample_type == "sample") %>%
          mutate(
            sample_id = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_elisa_pe = TRUE,
            n_elisa_pe_tests = n(),
            elisa_pe_results = paste(ifelse(sample_positive, "+", "-"), collapse = ", "),
            elisa_pe_positive = any(sample_positive, na.rm = TRUE),
            elisa_pe_qc_pass = all(qc_overall, na.rm = TRUE),
            elisa_pe_dod = mean(DOD, na.rm = TRUE),
            elisa_pe_date_display = safe_latest_date_dmy(plate_date),
            elisa_pe_qc_results = paste(ifelse(qc_overall, "PASS", "FAIL"), collapse = ", "),
            .groups = "drop"
          )

        samples <- samples %>%
          left_join(elisa_pe_summary, by = "sample_id") %>%
          mutate(
            has_elisa_pe = replace_na(has_elisa_pe, FALSE),
            elisa_pe_results = replace_na(elisa_pe_results, "✗"),
            elisa_pe_positive = replace_na(elisa_pe_positive, FALSE),
            elisa_pe_qc_pass = replace_na(elisa_pe_qc_pass, TRUE)
          )
      } else {
        samples <- samples %>%
          mutate(
            has_elisa_pe = FALSE,
            n_elisa_pe_tests = 0,
            elisa_pe_results = "✗",
            elisa_pe_positive = FALSE,
            elisa_pe_qc_pass = TRUE,
            elisa_pe_dod = NA_real_,
            elisa_pe_date_display = NA_character_,
            elisa_pe_qc_results = NA_character_
          )
      }

      # Add ELISA VSG test info
      if (nrow(elisa_vsg_data) > 0) {
        elisa_vsg_summary <- elisa_vsg_data %>%
          filter(sample_type == "sample") %>%
          mutate(
            sample_id = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_elisa_vsg = TRUE,
            n_elisa_vsg_tests = n(),
            elisa_vsg_results = paste(ifelse(sample_positive, "+", "-"), collapse = ", "),
            elisa_vsg_positive = any(sample_positive, na.rm = TRUE),
            elisa_vsg_qc_pass = all(qc_overall, na.rm = TRUE),
            elisa_vsg_dod = mean(DOD, na.rm = TRUE),
            elisa_vsg_date_display = safe_latest_date_dmy(plate_date),
            elisa_vsg_qc_results = paste(ifelse(qc_overall, "PASS", "FAIL"), collapse = ", "),
            .groups = "drop"
          )

        samples <- samples %>%
          left_join(elisa_vsg_summary, by = "sample_id") %>%
          mutate(
            has_elisa_vsg = replace_na(has_elisa_vsg, FALSE),
            elisa_vsg_results = replace_na(elisa_vsg_results, "✗"),
            elisa_vsg_positive = replace_na(elisa_vsg_positive, FALSE),
            elisa_vsg_qc_pass = replace_na(elisa_vsg_qc_pass, TRUE)
          )
      } else {
        samples <- samples %>%
          mutate(
            has_elisa_vsg = FALSE,
            n_elisa_vsg_tests = 0,
            elisa_vsg_results = "✗",
            elisa_vsg_positive = FALSE,
            elisa_vsg_qc_pass = TRUE,
            elisa_vsg_dod = NA_real_,
            elisa_vsg_date_display = NA_character_,
            elisa_vsg_qc_results = NA_character_
          )
      }

      # Add iELISA test info
      if (nrow(ielisa_data) > 0) {
        ielisa_summary <- ielisa_data %>%
          mutate(
            sample_id = map_chr(Barcode, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_ielisa = TRUE,
            n_ielisa_tests = n(),
            ielisa_results = paste(
              ifelse(positive_L13 | positive_L15, "+", "-"),
              collapse = ", "
            ),
            ielisa_positive = any(positive_L13 | positive_L15, na.rm = TRUE),
            ielisa_l13_positive = any(positive_L13, na.rm = TRUE),
            ielisa_l15_positive = any(positive_L15, na.rm = TRUE),
            ielisa_date_display = safe_latest_date_dmy(plate_date),
            ielisa_qc_results = paste(
              ifelse(coalesce(status_final, "Invalid") != "Invalid", "PASS", "FAIL"),
              collapse = ", "
            ),
            .groups = "drop"
          )

        samples <- samples %>%
          left_join(ielisa_summary, by = "sample_id") %>%
          mutate(
            has_ielisa = replace_na(has_ielisa, FALSE),
            ielisa_results = replace_na(ielisa_results, "✗"),
            ielisa_positive = replace_na(ielisa_positive, FALSE)
          )
      } else {
        samples <- samples %>%
          mutate(
            has_ielisa = FALSE,
            n_ielisa_tests = 0,
            ielisa_results = "✗",
            ielisa_positive = FALSE,
            ielisa_l13_positive = FALSE,
            ielisa_l15_positive = FALSE,
            ielisa_date_display = NA_character_,
            ielisa_qc_results = NA_character_
          )
      }

      # Calculate composite fields
      samples <- samples %>%
        mutate(
          # Any positive result across all tests (including DNA-only and RNA-only)
          any_positive = mic_positive | mic_positive_dna | mic_positive_rna | elisa_pe_positive | elisa_vsg_positive | ielisa_positive,

          # Aggregate ELISA results
          elisa_positive = elisa_pe_positive | elisa_vsg_positive,
          has_elisa = has_elisa_pe | has_elisa_vsg,

          # Distinguish molecular vs serological positivity
          molecular_positive = mic_positive | mic_positive_dna | mic_positive_rna,
          serological_positive = elisa_pe_positive | elisa_vsg_positive | ielisa_positive,

          # Overall QC status (all tests that were done must pass)
          overall_qc_pass = case_when(
            has_mic & !mic_qc_pass ~ FALSE,
            has_elisa_pe & !elisa_pe_qc_pass ~ FALSE,
            has_elisa_vsg & !elisa_vsg_qc_pass ~ FALSE,
            TRUE ~ TRUE
          ),

          # Processing completeness
          n_test_types = (has_mic + has_elisa + has_ielisa),
          processing_stage = case_when(
            n_test_types == 3 ~ "Fully Tested",
            n_test_types > 0 ~ "Partially Tested",
            has_extraction ~ "Extracted Only",
            TRUE ~ "Biobank Only"
          ),

          # Processing status: distinguish molecular vs serological positivity
          processing_status = case_when(
            molecular_positive & serological_positive ~ "Positive",
            molecular_positive ~ "Molecular Positive",
            serological_positive ~ "Serological Positive",
            n_test_types >= 2 & !any_positive ~ "Tested - Negative",
            n_test_types == 1 ~ "Limited Testing",
            has_extraction ~ "Awaiting Testing",
            TRUE ~ "Not Processed"
          )
        )

      samples
    })

    # ========================================================================
    # LONG FORMAT DATA: one row per test per sample
    # ========================================================================

    long_format_data <- reactive({
      req(biobank_df())

      biobank <- biobank_df()
      extractions <- tryCatch(extraction_df(), error = function(e) tibble())
      mic_data <- tryCatch(mic_df(), error = function(e) tibble())
      elisa_pe_data <- tryCatch(elisa_pe_df(), error = function(e) tibble())
      elisa_vsg_data <- tryCatch(elisa_vsg_df(), error = function(e) tibble())
      ielisa_data <- tryCatch(ielisa_df(), error = function(e) tibble())

      # Build sample lookup (rename to avoid clashes in joins)
      sample_lookup <- biobank %>%
        mutate(.sid = map_chr(barcode, ~normalize_sample_id(barcode = .x))) %>%
        filter(!is.na(.sid)) %>%
        select(
          .sid,
          .bc  = barcode,
          .lid = lab_id,
          .prov = province,
          .hz   = health_zone
        ) %>%
        distinct(.sid, .keep_all = TRUE)

      # Build biobank lookup for MIC lab-number matching
      biobank_lookup_long <- NULL
      if (nrow(biobank) > 0) {
        col_numero_labo <- safe_get_col(biobank, "numero_labo")
        col_lab_id      <- safe_get_col(biobank, "lab_id")
        col_numero      <- safe_get_col(biobank, "numero")
        cols_to_coalesce <- Filter(Negate(is.null), list(col_numero_labo, col_lab_id, col_numero))
        lab_numbers_raw  <- if (length(cols_to_coalesce) > 0) do.call(coalesce, cols_to_coalesce) else rep(NA_character_, nrow(biobank))
        lab_numbers_norm <- sapply(lab_numbers_raw, function(x) {
          if (is.na(x) || x == "") return(NA_character_)
          trimws(tolower(as.character(x)))
        })
        bc_norm <- map_chr(biobank$barcode, ~normalize_sample_id(barcode = .x))
        biobank_lookup_long <- tibble(lab_number_norm = lab_numbers_norm, .sid = bc_norm) %>%
          filter(!is.na(lab_number_norm) & lab_number_norm != "" & !is.na(.sid)) %>%
          distinct(lab_number_norm, .keep_all = TRUE)
      }

      rows <- list()

      # --- Extraction rows ---
      if (nrow(extractions) > 0 && "extraction_date" %in% names(extractions)) {
        ext_rows <- extractions %>%
          mutate(.sid = map_chr(barcode, ~normalize_sample_id(barcode = .x))) %>%
          filter(!is.na(.sid)) %>%
          left_join(sample_lookup, by = ".sid") %>%
          transmute(
            Barcode      = coalesce(.bc, barcode),
            Lab_ID       = .lid,
            Province     = .prov,
            Health_Zone  = .hz,
            Test_Type    = "Extraction",
            Test_Date    = tryCatch(format(as.Date(extraction_date), "%d/%m/%Y"), error = function(e) NA_character_),
            Result       = coalesce(extract_quality, NA_character_),
            QC_Pass      = coalesce(extract_quality, "Clear") != "Échec"
          )
        rows[["extraction"]] <- ext_rows
      }

      # --- MIC rows ---
      if (nrow(mic_data) > 0 && !is.null(biobank_lookup_long) && nrow(biobank_lookup_long) > 0) {
        mic_col_sampleid   <- safe_get_col(mic_data, "SampleID")
        mic_col_samplename <- safe_get_col(mic_data, "SampleName")
        mic_col_name       <- safe_get_col(mic_data, "Name")
        mic_cols_to_coalesce <- Filter(Negate(is.null), list(mic_col_sampleid, mic_col_samplename, mic_col_name))
        mic_lab_numbers_raw  <- if (length(mic_cols_to_coalesce) > 0) do.call(coalesce, mic_cols_to_coalesce) else rep(NA_character_, nrow(mic_data))
        mic_lab_numbers_norm <- sapply(mic_lab_numbers_raw, function(x) {
          if (is.na(x) || x == "") return(NA_character_)
          trimws(tolower(as.character(x)))
        })

        mic_rows <- mic_data %>%
          mutate(
            lab_number_norm  = mic_lab_numbers_norm,
            .qc_pass_count   = coalesce(ReplicatesTotal, 4) - coalesce(Replicates_Failed, 0)
          ) %>%
          left_join(biobank_lookup_long, by = "lab_number_norm") %>%
          filter(!is.na(.sid)) %>%
          left_join(sample_lookup, by = ".sid") %>%
          transmute(
            Barcode      = .bc,
            Lab_ID       = .lid,
            Province     = .prov,
            Health_Zone  = .hz,
            Test_Type    = "MIC",
            Test_Date    = tryCatch(format(as.Date(RunDate), "%d/%m/%Y"), error = function(e) NA_character_),
            Result       = FinalCall,
            QC_Pass      = .qc_pass_count > 0
          )
        rows[["mic"]] <- mic_rows
      }

      # --- ELISA PE rows ---
      if (nrow(elisa_pe_data) > 0) {
        pe_rows <- elisa_pe_data %>%
          filter(sample_type == "sample") %>%
          mutate(.sid = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))) %>%
          filter(!is.na(.sid)) %>%
          left_join(sample_lookup, by = ".sid") %>%
          transmute(
            Barcode      = coalesce(.bc, code_barres_kps),
            Lab_ID       = coalesce(.lid, numero_labo),
            Province     = .prov,
            Health_Zone  = .hz,
            Test_Type    = "ELISA-PE",
            Test_Date    = tryCatch(format(as.Date(plate_date), "%d/%m/%Y"), error = function(e) NA_character_),
            Result       = ifelse(sample_positive, "Positive", "Negative"),
            QC_Pass      = qc_overall
          )
        rows[["elisa_pe"]] <- pe_rows
      }

      # --- ELISA VSG rows ---
      if (nrow(elisa_vsg_data) > 0) {
        vsg_rows <- elisa_vsg_data %>%
          filter(sample_type == "sample") %>%
          mutate(.sid = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))) %>%
          filter(!is.na(.sid)) %>%
          left_join(sample_lookup, by = ".sid") %>%
          transmute(
            Barcode      = coalesce(.bc, code_barres_kps),
            Lab_ID       = coalesce(.lid, numero_labo),
            Province     = .prov,
            Health_Zone  = .hz,
            Test_Type    = "ELISA-VSG",
            Test_Date    = tryCatch(format(as.Date(plate_date), "%d/%m/%Y"), error = function(e) NA_character_),
            Result       = ifelse(sample_positive, "Positive", "Negative"),
            QC_Pass      = qc_overall
          )
        rows[["elisa_vsg"]] <- vsg_rows
      }

      # --- iELISA rows ---
      if (nrow(ielisa_data) > 0) {
        ielisa_rows <- ielisa_data %>%
          mutate(.sid = map_chr(Barcode, ~normalize_sample_id(barcode = .x))) %>%
          filter(!is.na(.sid)) %>%
          left_join(sample_lookup, by = ".sid") %>%
          transmute(
            Barcode      = coalesce(.bc, Barcode),
            Lab_ID       = coalesce(.lid, LabID),
            Province     = .prov,
            Health_Zone  = .hz,
            Test_Type    = "iELISA",
            Test_Date    = tryCatch(format(as.Date(plate_date), "%d/%m/%Y"), error = function(e) NA_character_),
            Result       = coalesce(status_final, NA_character_),
            QC_Pass      = coalesce(status_final, "Invalid") != "Invalid"
          )
        rows[["ielisa"]] <- ielisa_rows
      }

      if (length(rows) == 0) {
        return(tibble(
          Barcode = character(), Lab_ID = character(), Province = character(),
          Health_Zone = character(), Test_Type = character(), Test_Date = character(),
          Result = character(), QC_Pass = logical()
        ))
      }

      bind_rows(rows) %>%
        mutate(QC = ifelse(QC_Pass, "PASS", "FAIL")) %>%
        select(Barcode, Lab_ID, Province, Health_Zone, Test_Type, Test_Date, Result, QC) %>%
        arrange(Barcode, Lab_ID, Test_Type, Test_Date)
    })

    # Filtered samples based on user selections
    filtered_samples <- reactive({
      samples <- comprehensive_samples()

      # Apply positivity filter
      if (input$filter_positivity != "all") {
        samples <- switch(
          input$filter_positivity,
          "any_positive" = samples %>% filter(any_positive),
          "all_negative" = samples %>% filter(!any_positive),
          "mic_positive" = samples %>% filter(mic_positive),
          "mic_positive_dna" = samples %>% filter(mic_positive_dna),
          "mic_positive_rna" = samples %>% filter(mic_positive_rna),
          "elisa_positive" = samples %>% filter(elisa_positive),
          "ielisa_positive" = samples %>% filter(ielisa_positive),
          samples
        )
      }

      # Apply QC filter
      if (input$filter_qc != "all") {
        samples <- switch(
          input$filter_qc,
          "qc_pass" = samples %>% filter(overall_qc_pass),
          "qc_fail" = samples %>% filter(!overall_qc_pass),
          samples
        )
      }

      # Apply processing filter
      if (input$filter_processing != "all") {
        samples <- switch(
          input$filter_processing,
          "has_extraction" = samples %>% filter(has_extraction),
          "no_extraction" = samples %>% filter(!has_extraction),
          "has_mic" = samples %>% filter(has_mic),
          "has_elisa_pe" = samples %>% filter(has_elisa_pe),
          "has_elisa_vsg" = samples %>% filter(has_elisa_vsg),
          "has_elisa" = samples %>% filter(has_elisa),
          "has_ielisa" = samples %>% filter(has_ielisa),
          "complete" = samples %>% filter(has_extraction & has_mic & has_elisa),
          samples
        )
      }

      # Apply completeness filter
      if (input$filter_completeness != "all") {
        samples <- switch(
          input$filter_completeness,
          "fully_tested" = samples %>% filter(n_test_types == 3),
          "partially_tested" = samples %>% filter(n_test_types > 0 & n_test_types < 3),
          "biobank_only" = samples %>% filter(n_test_types == 0),
          samples
        )
      }

      samples
    })

    # Reset filters
    observeEvent(input$reset_filters, {
      updateSelectInput(session, "filter_positivity", selected = "all")
      updateSelectInput(session, "filter_qc", selected = "all")
      updateSelectInput(session, "filter_processing", selected = "all")
      updateSelectInput(session, "filter_completeness", selected = "all")
    })

    # ========================================================================
    # SUMMARY KPIs
    # ========================================================================

    output$summary_kpis <- renderUI({
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      all_samples <- comprehensive_samples()
      samples <- filtered_samples()

      if (is.null(all_samples) || nrow(all_samples) == 0) {
        return(div(class = "alert alert-info", "No samples available"))
      }

      n_total_all <- nrow(all_samples)
      n_total <- nrow(samples)
      n_extracted <- sum(samples$has_extraction, na.rm = TRUE)
      n_mic <- sum(samples$has_mic, na.rm = TRUE)
      n_elisa <- sum(samples$has_elisa, na.rm = TRUE)
      n_ielisa <- sum(samples$has_ielisa, na.rm = TRUE)
      n_fully_tested <- sum(samples$n_test_types == 3, na.rm = TRUE)
      n_positive <- sum(samples$any_positive, na.rm = TRUE)
      n_qc_fail <- sum(!samples$overall_qc_pass, na.rm = TRUE)

      layout_column_wrap(
        width = 1/4,
        heights_equal = "row",

        value_box(
          title = "Total Samples",
          value = scales::comma(n_total_all),
          showcase = icon("vial"),
          theme = "primary"
        ),

        value_box(
          title = "Samples Shown",
          value = sprintf("%d (%.1f%%)", n_total, 100 * n_total / max(n_total_all, 1)),
          showcase = icon("filter"),
          theme = if (n_total < n_total_all) "warning" else "info"
        ),

        value_box(
          title = "Extracted",
          value = sprintf("%d (%.1f%%)", n_extracted, 100 * n_extracted / max(n_total, 1)),
          showcase = icon("flask"),
          theme = "info"
        ),

        value_box(
          title = "Fully Tested",
          value = sprintf("%d (%.1f%%)", n_fully_tested, 100 * n_fully_tested / max(n_total, 1)),
          showcase = icon("check-double"),
          theme = "success"
        ),

        value_box(
          title = "Any Positive",
          value = sprintf("%d (%.1f%%)", n_positive, 100 * n_positive / max(n_total, 1)),
          showcase = icon("plus-circle"),
          theme = if (n_positive > 0) "danger" else "secondary"
        ),

        value_box(
          title = "Has MIC Test",
          value = sprintf("%d (%.1f%%)", n_mic, 100 * n_mic / max(n_total, 1)),
          showcase = icon("dna"),
          theme = "info"
        ),

        value_box(
          title = "Has ELISA-PE Test",
          value = sprintf("%d (%.1f%%)", sum(samples$has_elisa_pe, na.rm = TRUE), 100 * sum(samples$has_elisa_pe, na.rm = TRUE) / max(n_total, 1)),
          showcase = icon("vial-circle-check"),
          theme = "info"
        ),

        value_box(
          title = "Has ELISA-VSG Test",
          value = sprintf("%d (%.1f%%)", sum(samples$has_elisa_vsg, na.rm = TRUE), 100 * sum(samples$has_elisa_vsg, na.rm = TRUE) / max(n_total, 1)),
          showcase = icon("vial-circle-check"),
          theme = "info"
        ),

        value_box(
          title = "Has iELISA Test",
          value = sprintf("%d (%.1f%%)", n_ielisa, 100 * n_ielisa / max(n_total, 1)),
          showcase = icon("microscope"),
          theme = "info"
        ),

        value_box(
          title = "QC Failures",
          value = sprintf("%d (%.1f%%)", n_qc_fail, 100 * n_qc_fail / max(n_total, 1)),
          showcase = icon("triangle-exclamation"),
          theme = if (n_qc_fail > 0) "warning" else "success"
        )
      )
    })

    # ========================================================================
    # PROCESSING FLOW VISUALIZATION
    # ========================================================================

    output$processing_flow <- renderPlotly({
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      samples <- filtered_samples()

      if (is.null(samples) || nrow(samples) == 0) {
        return(plotly_empty() %>%
          layout(title = list(text = "No sample data available")))
      }

      n_biobank <- as.integer(nrow(samples))
      n_extracted <- as.integer(sum(samples$has_extraction, na.rm = TRUE))
      n_mic <- as.integer(sum(samples$has_mic, na.rm = TRUE))
      n_elisa_pe <- as.integer(sum(samples$has_elisa_pe, na.rm = TRUE))
      n_elisa_vsg <- as.integer(sum(samples$has_elisa_vsg, na.rm = TRUE))
      n_ielisa <- as.integer(sum(samples$has_ielisa, na.rm = TRUE))
      n_fully_tested <- as.integer(sum(samples$n_test_types == 3, na.rm = TRUE))

      force(n_biobank); force(n_extracted); force(n_mic)
      force(n_elisa_pe); force(n_elisa_vsg); force(n_ielisa); force(n_fully_tested)

      count_vector <- c(n_biobank, n_extracted, n_mic, n_elisa_pe, n_elisa_vsg, n_ielisa, n_fully_tested)

      flow_data <- tibble(
        stage = c("Biobank", "Extracted", "MIC Tested", "ELISA-PE", "ELISA-VSG", "iELISA Tested", "Fully Tested"),
        count = count_vector,
        percentage = round(100 * count_vector / max(n_biobank, 1), 1)
      ) %>%
        mutate(
          stage = factor(stage, levels = stage),
          label = sprintf("%s\n%d samples (%.1f%%)", stage, count, percentage)
        )

      plot_ly(
        data = flow_data,
        x = ~stage,
        y = ~count,
        type = "bar",
        text = ~label,
        textposition = "auto",
        marker = list(
          color = c("#4F46E5", "#10B981", "#F59E0B", "#06B6D4", "#EC4899", "#8B5CF6", "#14B8A6"),
          line = list(color = "white", width = 2)
        ),
        hovertemplate = "%{text}<extra></extra>"
      ) %>%
        layout(
          xaxis = list(title = "Processing Stage"),
          yaxis = list(title = "Number of Samples"),
          showlegend = FALSE,
          margin = list(t = 20, b = 80)
        )
    })

    # ========================================================================
    # SAMPLES TABLE (wide format)
    # ========================================================================

    output$samples_table <- renderDT({
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      samples <- filtered_samples()

      if (nrow(samples) == 0) {
        return(datatable(
          tibble(Message = "No samples match the current filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Select and format columns for display (wide format with dates and QC)
      display_data <- samples %>%
        select(
          barcode, lab_id,
          province, health_zone,
          processing_status, processing_stage,
          has_extraction, extraction_date_display,
          n_mic_tests, mic_date_display, mic_results, mic_qc_results,
          n_elisa_pe_tests, elisa_pe_date_display, elisa_pe_results, elisa_pe_qc_results,
          n_elisa_vsg_tests, elisa_vsg_date_display, elisa_vsg_results, elisa_vsg_qc_results,
          n_ielisa_tests, ielisa_date_display, ielisa_results, ielisa_qc_results,
          any_positive, overall_qc_pass
        ) %>%
        mutate(
          has_extraction = ifelse(has_extraction, "✓", "✗"),
          n_mic_tests = ifelse(is.na(n_mic_tests) | n_mic_tests == 0, "✗", as.character(n_mic_tests)),
          n_elisa_pe_tests = ifelse(is.na(n_elisa_pe_tests) | n_elisa_pe_tests == 0, "✗", as.character(n_elisa_pe_tests)),
          n_elisa_vsg_tests = ifelse(is.na(n_elisa_vsg_tests) | n_elisa_vsg_tests == 0, "✗", as.character(n_elisa_vsg_tests)),
          n_ielisa_tests = ifelse(is.na(n_ielisa_tests) | n_ielisa_tests == 0, "✗", as.character(n_ielisa_tests)),
          any_positive = ifelse(any_positive, "YES", "NO"),
          overall_qc_pass = ifelse(overall_qc_pass, "PASS", "FAIL")
        )

      names(display_data) <- c(
        "Barcode", "Lab ID", "Province", "Health Zone",
        "Status", "Stage",
        "Extracted", "Extraction Date",
        "MIC Tests", "MIC Date", "MIC Results", "MIC QC",
        "ELISA PE Tests", "ELISA PE Date", "ELISA PE Results", "ELISA PE QC",
        "ELISA VSG Tests", "ELISA VSG Date", "ELISA VSG Results", "ELISA VSG QC",
        "iELISA Tests", "iELISA Date", "iELISA Results", "iELISA QC",
        "Any Positive", "Overall QC"
      )

      datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "1200px",
          scrollCollapse = TRUE,
          dom = 'frtip',
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          )
        ),
        rownames = FALSE,
        filter = 'top',
        class = "table table-striped table-hover table-sm"
      ) %>%
        formatStyle(
          "Any Positive",
          backgroundColor = styleEqual(c("YES", "NO"), c('#f8d7da', '#d4edda')),
          fontWeight = "bold"
        ) %>%
        formatStyle(
          "Overall QC",
          backgroundColor = styleEqual(c("PASS", "FAIL"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          "Status",
          backgroundColor = styleEqual(
            c("Positive", "Molecular Positive", "Serological Positive",
              "Tested - Negative", "Limited Testing", "Awaiting Testing", "Not Processed"),
            c('#f8d7da', '#f5c2c7', '#fde8f0',
              '#d4edda', '#fff3cd', '#cfe2ff', '#e9ecef')
          )
        ) %>%
        formatStyle(
          "MIC Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';
              if (value === '\u2717') return '#e9ecef';
              var hasPositive = value.includes('+');
              var hasNegative = value.includes('-');
              var hasIndeterminate = value.includes('?');
              if (hasIndeterminate) return '#fff3cd';
              if (hasPositive && hasNegative) return '#fff3cd';
              if (hasPositive) return '#f8d7da';
              if (hasNegative) return '#d4edda';
              return '';
            }"
          )
        ) %>%
        formatStyle(
          "ELISA PE Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';
              if (value === '\u2717') return '#e9ecef';
              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');
              if (hasPos && hasNeg) return '#fff3cd';
              if (hasPos) return '#f8d7da';
              if (hasNeg) return '#d4edda';
              return '';
            }"
          )
        ) %>%
        formatStyle(
          "ELISA VSG Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';
              if (value === '\u2717') return '#e9ecef';
              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');
              if (hasPos && hasNeg) return '#fff3cd';
              if (hasPos) return '#f8d7da';
              if (hasNeg) return '#d4edda';
              return '';
            }"
          )
        ) %>%
        formatStyle(
          "iELISA Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';
              if (value === '\u2717') return '#e9ecef';
              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');
              if (hasPos && hasNeg) return '#fff3cd';
              if (hasPos) return '#f8d7da';
              if (hasNeg) return '#d4edda';
              return '';
            }"
          )
        ) %>%
        formatStyle(
          c("MIC QC", "ELISA PE QC", "ELISA VSG QC", "iELISA QC"),
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';
              if (value.includes('FAIL')) return '#f8d7da';
              if (value.includes('PASS')) return '#d4edda';
              return '';
            }"
          )
        )
    })

    # ========================================================================
    # LONG FORMAT TABLE
    # ========================================================================

    output$long_format_table <- renderDT({
      lf <- long_format_data()

      if (nrow(lf) == 0) {
        return(datatable(
          tibble(Message = "No test data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      datatable(
        lf,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "600px",
          scrollCollapse = TRUE,
          dom = 'frtip',
          columnDefs = list(list(className = 'dt-center', targets = "_all"))
        ),
        rownames = FALSE,
        filter = 'top',
        class = "table table-striped table-hover table-sm"
      ) %>%
        formatStyle(
          "Result",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value) return '';
              var v = value.toLowerCase();
              if (v === 'positive' || v === '+') return '#f8d7da';
              if (v === 'negative' || v === '-') return '#d4edda';
              if (v === 'borderline') return '#fff3cd';
              if (v === 'invalid') return '#e9ecef';
              if (v.includes('positive')) return '#f8d7da';
              if (v.includes('negative')) return '#d4edda';
              return '';
            }"
          )
        ) %>%
        formatStyle(
          "QC",
          backgroundColor = styleEqual(c("PASS", "FAIL"), c('#d4edda', '#f8d7da'))
        ) %>%
        formatStyle(
          "Test_Type",
          backgroundColor = styleEqual(
            c("Extraction", "MIC", "ELISA-PE", "ELISA-VSG", "iELISA"),
            c('#e2e3e5', '#cff4fc', '#d1ecf1', '#d6eaf8', '#e8daef')
          )
        )
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    # Helper to build the wide export data frame
    build_wide_export <- function(samples) {
      samples %>%
        select(
          barcode, lab_id,
          province, health_zone,
          processing_status, processing_stage,
          has_extraction, extraction_date_display, extraction_qc_results,
          n_mic_tests, mic_date_display, mic_results, mic_qc_results,
          n_elisa_pe_tests, elisa_pe_date_display, elisa_pe_results, elisa_pe_qc_results,
          n_elisa_vsg_tests, elisa_vsg_date_display, elisa_vsg_results, elisa_vsg_qc_results,
          n_ielisa_tests, ielisa_date_display, ielisa_results, ielisa_qc_results,
          any_positive, overall_qc_pass
        ) %>%
        mutate(
          # Numeric lab_id → Excel number column so rows sort 1 → 10000
          lab_id        = try_numeric(lab_id),
          has_extraction = ifelse(has_extraction, "Yes", "No"),
          n_mic_tests = ifelse(is.na(n_mic_tests) | n_mic_tests == 0, "-", as.character(n_mic_tests)),
          n_elisa_pe_tests = ifelse(is.na(n_elisa_pe_tests) | n_elisa_pe_tests == 0, "-", as.character(n_elisa_pe_tests)),
          n_elisa_vsg_tests = ifelse(is.na(n_elisa_vsg_tests) | n_elisa_vsg_tests == 0, "-", as.character(n_elisa_vsg_tests)),
          n_ielisa_tests = ifelse(is.na(n_ielisa_tests) | n_ielisa_tests == 0, "-", as.character(n_ielisa_tests)),
          any_positive = ifelse(any_positive, "YES", "NO"),
          overall_qc_pass = ifelse(overall_qc_pass, "PASS", "FAIL")
        ) %>%
        setNames(c(
          "Barcode", "Lab ID", "Province", "Health Zone",
          "Status", "Stage",
          "Extracted", "Extraction Date", "Extraction QC",
          "MIC Tests", "MIC Date", "MIC Results", "MIC QC",
          "ELISA PE Tests", "ELISA PE Date", "ELISA PE Results", "ELISA PE QC",
          "ELISA VSG Tests", "ELISA VSG Date", "ELISA VSG Results", "ELISA VSG QC",
          "iELISA Tests", "iELISA Date", "iELISA Results", "iELISA QC",
          "Any Positive", "Overall QC"
        ))
    }

    # Download CSV (wide format)
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("sample_processing_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write_csv(build_wide_export(filtered_samples()), file)
      }
    )

    # Download Excel (wide format)
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("sample_processing_wide_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(build_wide_export(filtered_samples()), file)
      }
    )

    # Helper: prepare long-format data for Excel export
    # Converts Lab_ID to numeric so Excel sorts 1 → 10000 instead of lexicographically
    long_export_data <- function() {
      long_format_data() %>%
        mutate(Lab_ID = try_numeric(Lab_ID))
    }

    # Download Excel (long format) - button in Sample Details card
    output$download_long_excel <- downloadHandler(
      filename = function() {
        paste0("sample_processing_long_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(long_export_data(), file)
      }
    )

    # Download Excel (long format) - button in Test History card
    output$download_long_excel2 <- downloadHandler(
      filename = function() {
        paste0("sample_processing_long_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(long_export_data(), file)
      }
    )
  })
}
