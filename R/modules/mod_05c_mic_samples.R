# ==============================================================================
# MODULE 2: SAMPLES - Main results table with filters
# ==============================================================================

mod_mic_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    card(
      class = "mb-3",
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Sample KPIs"),
        div(
          class = "d-flex align-items-center gap-2",
          checkboxInput(ns("secondary_only"), NULL, value = FALSE),
          tags$span(
            "Use secondary result when available",
            class = "text-muted small"
          )
        )
      ),
      card_body(
        layout_column_wrap(
          width = 1/4,
          heights_equal = "row",
          gap = "12px",

          value_box(
            title = "Total Samples",
            value = textOutput(ns("kpi_samples_total")),
            showcase = icon("vial"),
            theme = "primary"
          ),

          value_box(
            title = "Unique Samples",
            value = textOutput(ns("kpi_samples_unique")),
            showcase = icon("id-card"),
            theme = "info"
          ),

          value_box(
            title = "Tested Once",
            value = textOutput(ns("kpi_samples_once")),
            showcase = icon("circle-check"),
            theme = "success"
          ),

          value_box(
            title = "Retested",
            value = textOutput(ns("kpi_samples_multiple")),
            showcase = icon("redo"),
            theme = "warning"
          )
        ),

        layout_column_wrap(
          width = 1/4,
          heights_equal = "row",
          gap = "12px",

          value_box(
            title = "TNA Positive",
            value = textOutput(ns("kpi_samples_tna")),
            showcase = icon("dna"),
            theme = "success"
          ),

          value_box(
            title = "DNA Positive",
            value = textOutput(ns("kpi_samples_dna")),
            showcase = icon("circle-plus"),
            theme = "info"
          ),

          value_box(
            title = "RNA Positive",
            value = textOutput(ns("kpi_samples_rna")),
            showcase = icon("wave-square"),
            theme = "info"
          ),

          value_box(
            title = "Indeterminate",
            value = textOutput(ns("kpi_samples_indeterminate")),
            showcase = icon("question-circle"),
            theme = "warning"
          ),

          value_box(
            title = "Negative",
            value = textOutput(ns("kpi_samples_negative")),
            showcase = icon("circle-minus"),
            theme = "secondary"
          ),

          value_box(
            title = "TNA Prevalence",
            value = textOutput(ns("kpi_samples_tna_prev")),
            showcase = icon("percent"),
            theme = "primary"
          ),

          value_box(
            title = "Any Positive Prevalence",
            value = textOutput(ns("kpi_samples_any_prev")),
            showcase = icon("chart-pie"),
            theme = "primary"
          )
        )
      )
    ),

    # Results table - filters are applied from sidebar
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Sample Results"),
        downloadButton(ns("dl_filtered"), "Download", class = "btn-sm btn-outline-primary")
      ),
      card_body(
        DTOutput(ns("tbl_samples")),
        class = "p-3"
      )
    )
  )
}

mod_mic_samples_server <- function(id, filtered_base, processed_data) {
  moduleServer(id, function(input, output, session) {

    ordinal_label <- function(n) {
      n <- as.integer(n)
      suffix <- ifelse(
        n %% 100L %in% c(11L, 12L, 13L), "th",
        ifelse(
          n %% 10L == 1L, "st",
          ifelse(n %% 10L == 2L, "nd",
                 ifelse(n %% 10L == 3L, "rd", "th"))
        )
      )
      paste0(n, suffix)
    }

    append_test_order <- function(df, drop_helper_cols = TRUE) {
      if (!nrow(df)) return(df)

      has_id <- "SampleID" %in% names(df)
      has_name <- "SampleName" %in% names(df)

      if (!has_id && !has_name) return(df)

      result <- df %>%
        mutate(
          SampleKey = dplyr::coalesce(
            if (has_id) as.character(SampleID) else NA_character_,
            if (has_name) as.character(SampleName) else NA_character_,
            paste0("row_", dplyr::row_number())
          ),
          RunKey = dplyr::coalesce(
            if ("RunID" %in% names(.)) as.character(RunID) else NA_character_,
            paste0("run_", dplyr::row_number())
          ),
          RunDateTimeParsed = if ("RunDateTime" %in% names(.)) {
            suppressWarnings(lubridate::ymd_hms(as.character(RunDateTime), tz = "UTC"))
          } else {
            as.POSIXct(NA)
          },
          RunDateParsed = if ("RunDate" %in% names(.)) {
            suppressWarnings(lubridate::ymd(as.character(RunDate)))
          } else {
            as.Date(NA)
          }
        ) %>%
        arrange(SampleKey, RunDateTimeParsed, RunDateParsed, RunKey) %>%
        group_by(SampleKey) %>%
        mutate(
          TestNumber = dplyr::row_number(),
          TestOrderLabel = dplyr::case_when(
            TestNumber == 1L ~ "Primary",
            TestNumber == 2L ~ "Secondary",
            TestNumber == 3L ~ "Tertiary",
            TRUE ~ paste(ordinal_label(TestNumber), "Test")
          )
        ) %>%
        ungroup()

      if (isTRUE(drop_helper_cols)) {
        result <- result %>% select(-SampleKey, -RunKey, -RunDateTimeParsed, -RunDateParsed)
      }

      result
    }

    sample_metrics <- reactive({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return(NULL)

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return(NULL)

      total_instances <- nrow(df)
      has_id <- "SampleID" %in% names(df)
      has_name <- "SampleName" %in% names(df)

      df <- append_test_order(df, drop_helper_cols = FALSE)

      sample_counts <- df %>% count(SampleKey, name = "TimesTested")

      deduped <- df %>%
        arrange(SampleKey, RunDateTimeParsed, RunDateParsed, RunKey) %>%
        group_by(SampleKey) %>%
        mutate(
          TestNumber = dplyr::row_number(),
          TargetTest = if_else(
            isTRUE(input$secondary_only) & max(TestNumber) >= 2,
            2L,
            max(TestNumber)
          )
        ) %>%
        filter(TestNumber == TargetTest) %>%
        slice_head(n = 1) %>%
        ungroup()

      unique_samples <- nrow(sample_counts)
      tested_once <- sum(sample_counts$TimesTested == 1)
      tested_multiple <- sum(sample_counts$TimesTested > 1)
      deduped_total <- nrow(deduped)

      final_calls <- if ("FinalCall" %in% names(deduped)) {
        deduped$FinalCall
      } else {
        rep(NA_character_, deduped_total)
      }

      tna <- sum(final_calls == "Positive", na.rm = TRUE)
      dna <- sum(final_calls == "Positive_DNA", na.rm = TRUE)
      rna <- sum(final_calls == "Positive_RNA", na.rm = TRUE)
      indeterminate <- sum(final_calls == "Indeterminate", na.rm = TRUE)
      negative <- sum(final_calls == "Negative", na.rm = TRUE)
      any_positive <- sum(final_calls %in% c("Positive", "Positive_DNA", "Positive_RNA"), na.rm = TRUE)

      tna_prev <- if (deduped_total) round(100 * tna / deduped_total, 1) else NA_real_
      any_prev <- if (deduped_total) round(100 * any_positive / deduped_total, 1) else NA_real_

      list(
        total_instances = total_instances,
        unique_samples = unique_samples,
        tested_once = tested_once,
        tested_multiple = tested_multiple,
        deduped_total = deduped_total,
        tna = tna,
        dna = dna,
        rna = rna,
        indeterminate = indeterminate,
        negative = negative,
        tna_prev = tna_prev,
        any_prev = any_prev
      )
    })

    output$kpi_samples_total <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$total_instances)
    })

    output$kpi_samples_unique <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$unique_samples)
    })

    output$kpi_samples_once <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$tested_once)
    })

    output$kpi_samples_multiple <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      suffix <- if (isTRUE(input$secondary_only) && metrics$tested_multiple > 0) " (secondary applied)" else ""
      paste0(scales::comma(metrics$tested_multiple), suffix)
    })

    output$kpi_samples_tna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$tna)
    })

    output$kpi_samples_dna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$dna)
    })

    output$kpi_samples_rna <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$rna)
    })

    output$kpi_samples_indeterminate <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$indeterminate)
    })

    output$kpi_samples_negative <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics)) return("0")
      scales::comma(metrics$negative)
    })

    output$kpi_samples_tna_prev <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$tna_prev)) return("0%")
      sprintf("%.1f%%", metrics$tna_prev)
    })

    output$kpi_samples_any_prev <- renderText({
      metrics <- sample_metrics()
      if (is.null(metrics) || is.na(metrics$any_prev)) return("0%")
      sprintf("%.1f%%", metrics$any_prev)
    })

    # Main samples table
    output$tbl_samples <- renderDT({
      df <- filtered_base()
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples match filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Only show samples (not controls)
      if ("ControlType" %in% names(df)) {
        df <- df %>% filter(ControlType == "Sample")
      }

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found (only controls)"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      df <- append_test_order(df)

      # Round numeric columns
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA",
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(df)
      )
      
      if (length(numeric_cols) > 0) {
        df <- df %>% mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }
      
      # Select columns to display - including Barcode and replicate counts
      available_cols <- intersect(
        c("RunID", "SampleName", "Barcode", "TestOrderLabel", "TestNumber", "FinalCall",
          "Wells_TNA_Positive", "Wells_DNA_Positive", "Wells_RNA_Positive",
          "ReplicatesTotal", "Replicates_Positive", "Replicates_Negative", "Replicates_Failed",
          "Cq_median_177T", "Cq_median_18S2",
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "Province", "Structure", "HealthZone",
          "BiobankMatched", "ExtractionMatched", "Flags"),
        names(df)
      )
      
      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = c('FinalCall', 'BiobankMatched', 'ExtractionMatched'))
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        filter = 'top'
      ) %>%
        formatStyle('FinalCall',
                    backgroundColor = styleEqual(
                      c('Positive', 'Positive_DNA', 'Positive_RNA', 'LatePositive', 'Negative', 'Indeterminate', 'Invalid_NoDNA', 'Control', 'Control_Fail'),
                      c('#d4edda', '#b3e0f2', '#d4b3f2', '#ffe8a1', '#f8f9fa', '#fff3cd', '#f8d7da', '#dbe9ff', '#f5c6cb')
                    ))
    })

    # Download filtered
    output$dl_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>% filter(ControlType == "Sample") %>% append_test_order()
        write_csv(df, file)
      }
    )
    
  })
}
