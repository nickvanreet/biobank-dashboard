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
  # This prevents "001-A" and "001-B" from collapsing to the same ID
  ids <- gsub("[^a-z0-9._-]", "", ids)

  # Remove any resulting empty strings
  ids <- ids[ids != ""]
  if (!length(ids)) return(NA_character_)

  ids[1]
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

        # Samples table
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              span("Sample Details"),
              div(
                class = "btn-group btn-group-sm",
                downloadButton(ns("download_csv"), "CSV", class = "btn-sm"),
                downloadButton(ns("download_excel"), "Excel", class = "btn-sm")
              )
            )
          ),
          card_body(
            DTOutput(ns("samples_table"))
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
            drs_volume = NA_real_
          )
      }

      # Add MIC test info
      if (nrow(mic_data) > 0) {
        # MIC uses lab numbers (SampleID = "1", "2", "3"), not barcodes
        # We need to create a lookup from biobank data to match lab numbers to barcodes
        biobank_lookup <- NULL
        if (nrow(biobank) > 0) {
          # Extract lab numbers from biobank
          biobank_lab_numbers_raw <- coalesce(
            biobank$numero_labo,
            biobank$lab_id,
            biobank$numero
          )
          # Normalize lab numbers (just trim and lowercase: "1" → "1")
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

        # Extract lab numbers from MIC data
        mic_lab_numbers_raw <- coalesce(
          mic_data$SampleID,
          mic_data$SampleName,
          mic_data$Name
        )
        mic_lab_numbers_norm <- sapply(mic_lab_numbers_raw, function(x) {
          if (is.na(x) || x == "") return(NA_character_)
          trimws(tolower(as.character(x)))
        })

        # Create MIC data frame with lab numbers
        mic_with_lab_numbers <- mic_data %>%
          mutate(
            lab_number_norm = mic_lab_numbers_norm,
            # Calculate QC_Pass_Count from available columns
            QC_Pass_Count = coalesce(ReplicatesTotal, 4) - coalesce(Replicates_Failed, 0)
          )

        # Join with biobank lookup to convert lab numbers → barcodes
        if (!is.null(biobank_lookup) && nrow(biobank_lookup) > 0) {
          mic_summary <- mic_with_lab_numbers %>%
            left_join(biobank_lookup, by = "lab_number_norm") %>%
            filter(!is.na(sample_id) & sample_id != "") %>%
            group_by(sample_id) %>%
            summarise(
              has_mic = TRUE,
              n_mic_tests = n(),
              # FinalCall contains: "Positive" (TNA), "Positive_DNA", "Positive_RNA", "LatePositive", "Negative", "Invalid_NoDNA", "Indeterminate"
              # Convert to simplified symbols: + for positive, - for negative, ? for indeterminate
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
              # Replicate information
              mic_wells_total = first(coalesce(ReplicatesTotal, 4)),
              mic_wells_tna_positive = first(coalesce(Wells_TNA_Positive, 0)),
              mic_wells_dna_positive = first(coalesce(Wells_DNA_Positive, 0)),
              mic_wells_rna_positive = first(coalesce(Wells_RNA_Positive, 0)),
              .groups = "drop"
            )
        } else {
          # No biobank lookup available - can't match MIC samples
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
            # ELISA uses code_barres_kps
            sample_id = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_elisa_pe = TRUE,
            n_elisa_pe_tests = n(),
            # Convert to simplified symbols: + for positive, - for negative
            elisa_pe_results = paste(ifelse(sample_positive, "+", "-"), collapse = ", "),
            elisa_pe_positive = any(sample_positive, na.rm = TRUE),
            elisa_pe_qc_pass = all(qc_overall, na.rm = TRUE),
            elisa_pe_dod = mean(DOD, na.rm = TRUE),
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
            elisa_pe_dod = NA_real_
          )
      }

      # Add ELISA VSG test info
      if (nrow(elisa_vsg_data) > 0) {
        elisa_vsg_summary <- elisa_vsg_data %>%
          filter(sample_type == "sample") %>%
          mutate(
            # ELISA uses code_barres_kps
            sample_id = map_chr(code_barres_kps, ~normalize_sample_id(barcode = .x))
          ) %>%
          filter(!is.na(sample_id) & sample_id != "") %>%
          group_by(sample_id) %>%
          summarise(
            has_elisa_vsg = TRUE,
            n_elisa_vsg_tests = n(),
            # Convert to simplified symbols: + for positive, - for negative
            elisa_vsg_results = paste(ifelse(sample_positive, "+", "-"), collapse = ", "),
            elisa_vsg_positive = any(sample_positive, na.rm = TRUE),
            elisa_vsg_qc_pass = all(qc_overall, na.rm = TRUE),
            elisa_vsg_dod = mean(DOD, na.rm = TRUE),
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
            elisa_vsg_dod = NA_real_
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
            # Convert to simplified symbols: + for positive, - for negative
            ielisa_results = paste(
              ifelse(positive_L13 | positive_L15, "+", "-"),
              collapse = ", "
            ),
            ielisa_positive = any(positive_L13 | positive_L15, na.rm = TRUE),
            ielisa_l13_positive = any(positive_L13, na.rm = TRUE),
            ielisa_l15_positive = any(positive_L15, na.rm = TRUE),
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
            ielisa_l15_positive = FALSE
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

          # Create a processing status indicator
          processing_status = case_when(
            any_positive ~ "Positive",
            n_test_types >= 2 & !any_positive ~ "Tested - Negative",
            n_test_types == 1 ~ "Limited Testing",
            has_extraction ~ "Awaiting Testing",
            TRUE ~ "Not Processed"
          )
        )

      samples
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
      # Explicitly depend on all filter inputs to ensure reactivity
      # This forces the KPIs to re-render when any filter changes
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      # Get comprehensive and filtered samples
      all_samples <- comprehensive_samples()
      samples <- filtered_samples()

      # Safety check
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
      # Explicitly depend on all filter inputs to ensure reactivity
      # This forces the chart to re-render when any filter changes
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      # Now get filtered samples
      samples <- filtered_samples()

      # Safety check
      if (is.null(samples) || nrow(samples) == 0) {
        return(plotly_empty() %>%
          layout(title = list(text = "No sample data available")))
      }

      # Calculate numbers for each stage from filtered samples
      # All counts are based on the filtered dataset
      n_biobank <- as.integer(nrow(samples))
      n_extracted <- as.integer(sum(samples$has_extraction, na.rm = TRUE))
      n_mic <- as.integer(sum(samples$has_mic, na.rm = TRUE))
      n_elisa_pe <- as.integer(sum(samples$has_elisa_pe, na.rm = TRUE))
      n_elisa_vsg <- as.integer(sum(samples$has_elisa_vsg, na.rm = TRUE))
      n_ielisa <- as.integer(sum(samples$has_ielisa, na.rm = TRUE))
      n_fully_tested <- as.integer(sum(samples$n_test_types == 3, na.rm = TRUE))

      # Force evaluation of counts before creating vector
      # This ensures counts are calculated before being used in the visualization
      force(n_biobank)
      force(n_extracted)
      force(n_mic)
      force(n_elisa_pe)
      force(n_elisa_vsg)
      force(n_ielisa)
      force(n_fully_tested)

      # Create count vector
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
    # SAMPLES TABLE
    # ========================================================================

    output$samples_table <- renderDT({
      # Explicitly depend on all filter inputs to ensure reactivity
      # This forces the table to re-render when any filter changes
      filter_positivity <- input$filter_positivity
      filter_completeness <- input$filter_completeness
      filter_processing <- input$filter_processing
      filter_qc <- input$filter_qc

      # Now get filtered samples
      samples <- filtered_samples()

      if (nrow(samples) == 0) {
        return(datatable(
          tibble(Message = "No samples match the current filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Select and format columns for display
      display_data <- samples %>%
        select(
          barcode,
          lab_id,
          province,
          health_zone,
          processing_status,
          processing_stage,
          has_extraction,
          n_mic_tests,
          mic_results,
          n_elisa_pe_tests,
          elisa_pe_results,
          n_elisa_vsg_tests,
          elisa_vsg_results,
          n_ielisa_tests,
          ielisa_results,
          any_positive,
          overall_qc_pass
        ) %>%
        mutate(
          # Convert extraction to ✓/✗ (keep this one as is)
          has_extraction = ifelse(has_extraction, "✓", "✗"),
          # Show test counts (number of tests performed, or "✗" if none)
          n_mic_tests = ifelse(is.na(n_mic_tests) | n_mic_tests == 0, "✗", as.character(n_mic_tests)),
          n_elisa_pe_tests = ifelse(is.na(n_elisa_pe_tests) | n_elisa_pe_tests == 0, "✗", as.character(n_elisa_pe_tests)),
          n_elisa_vsg_tests = ifelse(is.na(n_elisa_vsg_tests) | n_elisa_vsg_tests == 0, "✗", as.character(n_elisa_vsg_tests)),
          n_ielisa_tests = ifelse(is.na(n_ielisa_tests) | n_ielisa_tests == 0, "✗", as.character(n_ielisa_tests)),
          # Results show all test outcomes using simplified symbols (+/-/?)
          # Multiple results per sample are comma-separated (e.g., "+, -, +" for 3 tests)
          any_positive = ifelse(any_positive, "YES", "NO"),
          overall_qc_pass = ifelse(overall_qc_pass, "PASS", "FAIL")
        )

      # Rename columns
      names(display_data) <- c(
        "Barcode", "Lab ID", "Province", "Health Zone",
        "Status", "Stage", "Extracted",
        "MIC Tests", "MIC Results",
        "ELISA PE Tests", "ELISA PE Results",
        "ELISA VSG Tests", "ELISA VSG Results",
        "iELISA Tests", "iELISA Results",
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
            c("Positive", "Tested - Negative", "Limited Testing", "Awaiting Testing", "Not Processed"),
            c('#f8d7da', '#d4edda', '#fff3cd', '#cfe2ff', '#e9ecef')
          )
        ) %>%
        # Color code MIC Results
        formatStyle(
          "MIC Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';

              // Not done
              if (value === '\u2717') return '#e9ecef'; // Gray for not done

              var hasPositive = value.includes('+');
              var hasNegative = value.includes('-');
              var hasIndeterminate = value.includes('?');

              if (hasIndeterminate) {
                return '#fff3cd'; // Yellow for indeterminate
              } else if (hasPositive && hasNegative) {
                return '#fff3cd'; // Yellow for conflicting results
              } else if (hasPositive) {
                return '#f8d7da'; // Red for positive
              } else if (hasNegative) {
                return '#d4edda'; // Green for negative
              }
              return '';
            }"
          )
        ) %>%
        # Color code ELISA PE Results
        formatStyle(
          "ELISA PE Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';

              // Not done
              if (value === '\u2717') return '#e9ecef'; // Gray for not done

              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');

              if (hasPos && hasNeg) {
                return '#fff3cd'; // Yellow for conflicting results
              } else if (hasPos) {
                return '#f8d7da'; // Red for positive
              } else if (hasNeg) {
                return '#d4edda'; // Green for negative
              }
              return '';
            }"
          )
        ) %>%
        # Color code ELISA VSG Results
        formatStyle(
          "ELISA VSG Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';

              // Not done
              if (value === '\u2717') return '#e9ecef'; // Gray for not done

              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');

              if (hasPos && hasNeg) {
                return '#fff3cd'; // Yellow for conflicting results
              } else if (hasPos) {
                return '#f8d7da'; // Red for positive
              } else if (hasNeg) {
                return '#d4edda'; // Green for negative
              }
              return '';
            }"
          )
        ) %>%
        # Color code iELISA Results
        formatStyle(
          "iELISA Results",
          backgroundColor = JS(
            "function(value, type, row, meta) {
              if (!value || value === '') return '';

              // Not done
              if (value === '\u2717') return '#e9ecef'; // Gray for not done

              var hasPos = value.includes('+');
              var hasNeg = value.includes('-');

              if (hasPos && hasNeg) {
                return '#fff3cd'; // Yellow for conflicting results
              } else if (hasPos) {
                return '#f8d7da'; // Red for positive
              } else if (hasNeg) {
                return '#d4edda'; // Green for negative
              }
              return '';
            }"
          )
        )
    })

    # ========================================================================
    # DOWNLOAD HANDLERS
    # ========================================================================

    # Download CSV
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("sample_processing_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        # Get the filtered samples data
        samples <- filtered_samples()

        # Format for export (same as display)
        export_data <- samples %>%
          select(
            barcode,
            lab_id,
            province,
            health_zone,
            processing_status,
            processing_stage,
            has_extraction,
            n_mic_tests,
            mic_results,
            n_elisa_pe_tests,
            elisa_pe_results,
            n_elisa_vsg_tests,
            elisa_vsg_results,
            n_ielisa_tests,
            ielisa_results,
            any_positive,
            overall_qc_pass
          ) %>%
          mutate(
            has_extraction = ifelse(has_extraction, "Yes", "No"),
            n_mic_tests = ifelse(is.na(n_mic_tests) | n_mic_tests == 0, "-", as.character(n_mic_tests)),
            n_elisa_pe_tests = ifelse(is.na(n_elisa_pe_tests) | n_elisa_pe_tests == 0, "-", as.character(n_elisa_pe_tests)),
            n_elisa_vsg_tests = ifelse(is.na(n_elisa_vsg_tests) | n_elisa_vsg_tests == 0, "-", as.character(n_elisa_vsg_tests)),
            n_ielisa_tests = ifelse(is.na(n_ielisa_tests) | n_ielisa_tests == 0, "-", as.character(n_ielisa_tests)),
            any_positive = ifelse(any_positive, "YES", "NO"),
            overall_qc_pass = ifelse(overall_qc_pass, "PASS", "FAIL")
          )

        # Rename columns for export
        names(export_data) <- c(
          "Barcode", "Lab ID", "Province", "Health Zone",
          "Status", "Stage", "Extracted",
          "MIC Tests", "MIC Results",
          "ELISA PE Tests", "ELISA PE Results",
          "ELISA VSG Tests", "ELISA VSG Results",
          "iELISA Tests", "iELISA Results",
          "Any Positive", "Overall QC"
        )

        write_csv(export_data, file)
      }
    )

    # Download Excel
    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("sample_processing_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        # Get the filtered samples data
        samples <- filtered_samples()

        # Format for export (same as display)
        export_data <- samples %>%
          select(
            barcode,
            lab_id,
            province,
            health_zone,
            processing_status,
            processing_stage,
            has_extraction,
            n_mic_tests,
            mic_results,
            n_elisa_pe_tests,
            elisa_pe_results,
            n_elisa_vsg_tests,
            elisa_vsg_results,
            n_ielisa_tests,
            ielisa_results,
            any_positive,
            overall_qc_pass
          ) %>%
          mutate(
            has_extraction = ifelse(has_extraction, "Yes", "No"),
            n_mic_tests = ifelse(is.na(n_mic_tests) | n_mic_tests == 0, "-", as.character(n_mic_tests)),
            n_elisa_pe_tests = ifelse(is.na(n_elisa_pe_tests) | n_elisa_pe_tests == 0, "-", as.character(n_elisa_pe_tests)),
            n_elisa_vsg_tests = ifelse(is.na(n_elisa_vsg_tests) | n_elisa_vsg_tests == 0, "-", as.character(n_elisa_vsg_tests)),
            n_ielisa_tests = ifelse(is.na(n_ielisa_tests) | n_ielisa_tests == 0, "-", as.character(n_ielisa_tests)),
            any_positive = ifelse(any_positive, "YES", "NO"),
            overall_qc_pass = ifelse(overall_qc_pass, "PASS", "FAIL")
          )

        # Rename columns for export
        names(export_data) <- c(
          "Barcode", "Lab ID", "Province", "Health Zone",
          "Status", "Stage", "Extracted",
          "MIC Tests", "MIC Results",
          "ELISA PE Tests", "ELISA PE Results",
          "ELISA VSG Tests", "ELISA VSG Results",
          "iELISA Tests", "iELISA Results",
          "Any Positive", "Overall QC"
        )

        writexl::write_xlsx(export_data, file)
      }
    )
  })
}
