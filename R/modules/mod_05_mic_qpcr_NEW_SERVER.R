# =============================================================================
# NEW SERVER IMPLEMENTATION - PASTE INTO mod_05_mic_qpcr.R starting at line 1135
# =============================================================================

mod_mic_qpcr_server <- function(id, biobank_df, extractions_df, filters) {
  moduleServer(id, function(input, output, session) {

    # =========================================================================
    # REACTIVE VALUES & SETTINGS
    # =========================================================================

    cache_state <- reactiveVal(list())

    # Settings inputs - initialize with defaults in case modal hasn't been opened yet
    observeEvent(session$userData, once = TRUE, {
      # Initialize threshold values if they don't exist
      defaults <- list(
        th_177t_pos = 35, th_177t_neg = 40,
        th_18s2_pos = 35, th_18s2_neg = 40,
        th_rnp_dna_pos = 28, th_rnp_dna_neg = 40,
        th_rnp_rna_pos = 35, th_rnp_rna_neg = 40,
        late_min = 35, late_max = 40,
        delta_rp_limit = 8,
        allow_review = FALSE
      )

      for (name in names(defaults)) {
        if (is.null(input[[name]])) {
          updateNumericInput(session, name, value = defaults[[name]])
        }
      }
    })

    settings <- reactive({
      list(
        thresholds = list(
          `177T` = list(positive = input$th_177t_pos %||% 35, negative = input$th_177t_neg %||% 40),
          `18S2` = list(positive = input$th_18s2_pos %||% 35, negative = input$th_18s2_neg %||% 40),
          RNAseP_DNA = list(positive = input$th_rnp_dna_pos %||% 28, negative = input$th_rnp_dna_neg %||% 40),
          RNAseP_RNA = list(positive = input$th_rnp_rna_pos %||% 35, negative = input$th_rnp_rna_neg %||% 40)
        ),
        late_window = c(input$late_min %||% 35, input$late_max %||% 40),
        delta_rp_limit = input$delta_rp_limit %||% 8,
        allow_review_controls = isTRUE(input$allow_review),
        pc_aliases = c("PC", "POS", "POSITIVE", "CP"),
        nc_aliases = c("NC", "NEG", "NTC", "CN")
      )
    })

    # =========================================================================
    # SETTINGS MODAL
    # =========================================================================

    observeEvent(input$settings, {
      showModal(modalDialog(
        title = "qPCR Settings & Thresholds",
        size = "xl",
        easyClose = TRUE,

        layout_columns(
          col_widths = c(6, 6),

          # Left column: Trypanozoon targets
          card(
            card_header("Trypanozoon Targets (DNA & RNA)", class = "bg-primary text-white"),
            card_body(
              class = "p-3",

              h6("177T (DNA Target)", class = "mt-2 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_177t_pos"),
                  "Positive ≤",
                  value = isolate(input$th_177t_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_177t_neg"),
                  "Negative >",
                  value = isolate(input$th_177t_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              ),

              h6("18S2 (RNA Target)", class = "mt-3 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_18s2_pos"),
                  "Positive ≤",
                  value = isolate(input$th_18s2_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_18s2_neg"),
                  "Negative >",
                  value = isolate(input$th_18s2_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              )
            )
          ),

          # Right column: RNAseP targets
          card(
            card_header("RNAseP Targets (Quality Control)", class = "bg-info text-white"),
            card_body(
              class = "p-3",

              h6("RNAseP-DNA", class = "mt-2 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_rnp_dna_pos"),
                  "Positive ≤",
                  value = isolate(input$th_rnp_dna_pos) %||% 28,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_rnp_dna_neg"),
                  "Negative >",
                  value = isolate(input$th_rnp_dna_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              ),

              h6("RNAseP-RNA", class = "mt-3 mb-3"),
              layout_columns(
                col_widths = c(6, 6),
                numericInput(
                  session$ns("th_rnp_rna_pos"),
                  "Positive ≤",
                  value = isolate(input$th_rnp_rna_pos) %||% 35,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                ),
                numericInput(
                  session$ns("th_rnp_rna_neg"),
                  "Negative >",
                  value = isolate(input$th_rnp_rna_neg) %||% 40,
                  min = 0,
                  max = 50,
                  step = 0.5,
                  width = "100%"
                )
              )
            )
          )
        ),

        # QC parameters at bottom
        card(
          card_header("Quality Control Parameters", class = "bg-warning"),
          card_body(
            class = "p-3",
            layout_columns(
              col_widths = c(4, 4, 4),

              div(
                h6("Late Positive Window"),
                p(class = "small text-muted mb-2", "Cq values inside this range are labelled LatePositive during classification."),
                layout_columns(
                  col_widths = c(6, 6),
                  numericInput(
                    session$ns("late_min"),
                    "Min",
                    value = isolate(input$late_min) %||% 35,
                    min = 0,
                    max = 50,
                    step = 0.5,
                    width = "100%"
                  ),
                  numericInput(
                    session$ns("late_max"),
                    "Max",
                    value = isolate(input$late_max) %||% 40,
                    min = 0,
                    max = 50,
                    step = 0.5,
                    width = "100%"
                  )
                )
              ),

              div(
                h6("RNA Preservation"),
                numericInput(
                  session$ns("delta_rp_limit"),
                  "ΔCq Limit",
                  value = isolate(input$delta_rp_limit) %||% 8,
                  min = 0,
                  max = 20,
                  step = 0.5,
                  width = "100%"
                ),
                p(class = "small text-muted mt-1", "Max acceptable difference between RNA and DNA")
              ),

              div(
                h6("Control Handling"),
                checkboxInput(
                  session$ns("allow_review"),
                  "Allow review despite control failures",
                  value = isolate(input$allow_review) %||% FALSE
                ),
                p(class = "small text-muted mt-1", "Process samples even when controls fail")
              )
            )
          )
        ),

        footer = div(
          actionButton(session$ns("apply_settings"), "Apply Settings", class = "btn-primary"),
          modalButton("Cancel", class = "btn-secondary")
        )
      ))
    })

    observeEvent(input$apply_settings, {
      removeModal()
      showNotification("Settings applied. Refresh data to reprocess.", type = "message")
    })

    # =========================================================================
    # DATA LOADING
    # =========================================================================

    raw_data <- reactive({
      req(input$mic_dir)

      withProgress(message = "Loading MIC files...", value = 0.3, {
        parsed <- parse_mic_directory(input$mic_dir, settings(), cache_state())
        cache_state(parsed$cache)
        parsed
      })
    })

    # Force refresh
    observeEvent(input$refresh, {
      cache_state(list())
      raw_data()
    })

    # =========================================================================
    # PROCESSED DATA
    # =========================================================================

    processed_data <- reactive({
      rd <- raw_data()

      if (!nrow(rd$samples)) {
        return(list(
          runs = tibble(),
          samples = tibble(),
          replicates = tibble(),
          control_status = tibble(),
          lj_stats = list()
        ))
      }

      # Validate controls
      control_status <- validate_controls(rd$samples, settings())

      # Create run summary
      runs_summary <- create_run_summary(rd$samples, rd$runs, control_status)

      # Link to biobank and extractions
      samples_linked <- rd$samples %>%
        link_to_biobank(if (is.null(biobank_df)) NULL else biobank_df()) %>%
        link_to_extractions(if (is.null(extractions_df)) NULL else extractions_df())

      # Apply control flags
      samples_linked <- samples_linked %>%
        left_join(
          control_status %>% select(RunID, SampleID, ControlFlag),
          by = c("RunID", "SampleID")
        ) %>%
        mutate(
          Flags = if_else(
            !is.na(ControlFlag),
            if_else(is.na(Flags), ControlFlag, paste(Flags, ControlFlag, sep = ";")),
            Flags
          ),
          AnyFlag = AnyFlag | !is.na(ControlFlag)
        )

      # Mark invalid runs
      if (!settings()$allow_review_controls) {
        invalid_runs <- runs_summary %>%
          filter(!RunValid) %>%
          pull(RunID)

        if (length(invalid_runs) > 0) {
          samples_linked <- samples_linked %>%
            mutate(
              FinalCall = if_else(RunID %in% invalid_runs, "RunInvalid", FinalCall),
              Flags = if_else(RunID %in% invalid_runs,
                              paste(Flags, "RunInvalid", sep = ";"),
                              Flags),
              AnyFlag = if_else(RunID %in% invalid_runs, TRUE, AnyFlag)
            )
        }
      }

      # Compute Levey-Jennings stats
      lj_stats <- list(
        `177T` = compute_levey_jennings(rd$replicates, "177T"),
        `18S2` = compute_levey_jennings(rd$replicates, "18S2"),
        RNAseP_DNA = compute_levey_jennings(rd$replicates, "RNAseP_DNA"),
        RNAseP_RNA = compute_levey_jennings(rd$replicates, "RNAseP_RNA")
      )

      list(
        runs = runs_summary,
        samples = samples_linked,
        replicates = rd$replicates,
        control_status = control_status,
        lj_stats = lj_stats
      )
    })

    # =========================================================================
    # FILTERED SAMPLES (with UI filters)
    # =========================================================================

    filtered_samples <- reactive({
      pd <- processed_data()
      df <- pd$samples

      if (!nrow(df)) return(df)

      # Apply global filters first
      df <- apply_global_filters(df, if (is.null(filters)) NULL else filters())

      # Apply UI filters
      if (!is.null(input$filter_call) && input$filter_call != "all") {
        df <- df %>% filter(FinalCall == input$filter_call)
      }

      if (!is.null(input$filter_province) && input$filter_province != "all") {
        df <- df %>% filter(Province == input$filter_province)
      }

      if (!is.null(input$filter_structure) && input$filter_structure != "all") {
        df <- df %>% filter(Structure == input$filter_structure)
      }

      if (isTRUE(input$filter_flagged_only)) {
        df <- df %>% filter(AnyFlag == TRUE)
      }

      df
    })

    # Update filter dropdowns dynamically
    observe({
      df <- processed_data()$samples
      if (nrow(df) > 0 && "Province" %in% names(df)) {
        provinces <- sort(unique(na.omit(df$Province)))
        if (length(provinces) > 0) {
          updateSelectInput(session, "filter_province",
                            choices = c("All" = "all", provinces))
        }
      }

      if (nrow(df) > 0 && "Structure" %in% names(df)) {
        structures <- sort(unique(na.omit(df$Structure)))
        if (length(structures) > 0) {
          updateSelectInput(session, "filter_structure",
                            choices = c("All" = "all", structures))
        }
      }
    })

    # =========================================================================
    # KPIs - ALL 10 METRICS
    # =========================================================================

    output$kpi_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0")
      nrow(runs) %>% scales::comma()
    })

    output$kpi_samples <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")

      df %>%
        filter(ControlType == "Sample") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_positives <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", FinalCall == "Positive") %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_prevalence <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0%")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")

      n_pos <- sum(df$FinalCall == "Positive", na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_pos / total, 1) else 0

      paste0(pct, "%")
    })

    output$kpi_flagged <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return("0")
      }

      df %>%
        filter(ControlType == "Sample", AnyFlag == TRUE) %>%
        nrow() %>%
        scales::comma()
    })

    output$kpi_biobank <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"BiobankMatched" %in% names(df)) {
        return("0%")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")

      n_linked <- sum(df$BiobankMatched, na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_linked / total) else 0

      paste0(pct, "%")
    })

    output$kpi_extractions <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"ExtractionMatched" %in% names(df)) {
        return("0%")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")

      n_linked <- sum(df$ExtractionMatched, na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_linked / total) else 0

      paste0(pct, "%")
    })

    output$kpi_dna_good <- renderText({
      df <- processed_data()$samples
      if (!nrow(df) || !"Call_RNAseP_DNA" %in% names(df)) return("N/A")

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      good <- sum(df$Call_RNAseP_DNA %in% c("Positive", "LatePositive"), na.rm = TRUE)
      total <- sum(!is.na(df$Call_RNAseP_DNA))

      if (total == 0) return("N/A")
      pct <- round(100 * good / total)
      paste0(pct, "%")
    })

    output$kpi_rna_good <- renderText({
      df <- processed_data()$samples
      if (!nrow(df) || !"Call_RNAseP_RNA" %in% names(df) || !"Delta_RP" %in% names(df)) return("N/A")

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      # Good RNA = detected AND ΔRP < limit
      limit <- settings()$delta_rp_limit
      good <- sum(
        df$Call_RNAseP_RNA %in% c("Positive", "LatePositive") &
          !is.na(df$Delta_RP) &
          df$Delta_RP <= limit,
        na.rm = TRUE
      )
      total <- sum(!is.na(df$Call_RNAseP_RNA))

      if (total == 0) return("N/A")
      pct <- round(100 * good / total)
      paste0(pct, "%")
    })

    output$kpi_valid_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs) || !"RunValid" %in% names(runs)) return("0/0")

      valid <- sum(runs$RunValid, na.rm = TRUE)
      total <- nrow(runs)

      glue("{valid}/{total}")
    })

    # =========================================================================
    # TABLES - WITH IMPROVED STYLING
    # =========================================================================

    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs

      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No MIC runs found. Check that files exist in the MIC directory."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      display_runs <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗")
        )

      available_cols <- intersect(
        c("RunID", "FileName", "RunDateTime", "WellCount", "TotalSamples",
          "Positives", "Negatives", "LatePositives", "Flagged", "RunValid"),
        names(display_runs)
      )

      datatable(
        display_runs %>% select(all_of(available_cols)),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 15, 25, 50, -1), c('10', '15', '25', '50', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        formatStyle('RunValid',
                    color = styleEqual(c('✓', '✗'), c('green', 'red')))
    })

    output$tbl_samples <- renderDT({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(datatable(
          tibble(),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      if (!"ControlType" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: ControlType column missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- df %>% filter(ControlType == "Sample")

      if (!nrow(df)) {
        return(datatable(
          tibble(),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Round numeric columns
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA",
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(df)
      )

      if (length(numeric_cols) > 0) {
        df <- df %>%
          mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }

      # Select columns including replicate counts
      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall",
          "Wells_TNA_Positive", "Wells_DNA_Positive", "Wells_RNA_Positive",
          "ReplicatesTotal", "Replicates_Positive", "Replicates_Negative", "Replicates_Failed",
          "Cq_median_177T", "Cq_median_18S2",
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "Province", "Structure",
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
            list(className = 'dt-center', targets = which(available_cols %in% c('FinalCall', 'BiobankMatched', 'ExtractionMatched')) - 1)
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

    output$tbl_controls <- renderDT({
      ctrl <- processed_data()$control_status

      if (!nrow(ctrl)) {
        return(datatable(
          tibble(Message = "No control data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      available_cols <- intersect(
        c("RunID", "SampleName", "ControlType", "ControlPass", "ControlFlag"),
        names(ctrl)
      )

      ctrl <- ctrl %>%
        mutate(ControlPass = if_else(ControlPass, "✓ Pass", "✗ Fail"))

      datatable(
        ctrl %>% select(all_of(available_cols)),
        options = list(
          dom = 'tp',
          paging = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe"
      ) %>%
        formatStyle('ControlPass',
                    color = styleEqual(c('✓ Pass', '✗ Fail'), c('green', 'red')))
    })

    output$tbl_flags <- renderDT({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      if (!"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: required columns missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          AnyFlag == TRUE | (if ("FinalCall" %in% names(df)) FinalCall == "Invalid_NoDNA" else FALSE)
        )

      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "✓ No flagged samples - all QC passed!"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", "Flags", "Delta_RP",
          "BiobankMatched", "ExtractionMatched"),
        names(df)
      )

      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })

    # =========================================================================
    # LEVEY-JENNINGS PLOTS (with improved styling)
    # =========================================================================

    render_lj_plot <- function(target_name, output_name) {
      output[[output_name]] <- renderPlotly({
        lj <- processed_data()$lj_stats[[target_name]]

        if (!nrow(lj$data)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        plot_ly(lj$data, x = ~RunID, y = ~Cq_mean,
                type = 'scatter', mode = 'markers+lines',
                name = 'Run Mean',
                marker = list(size = 12, color = '#2c3e50'),
                line = list(width = 3, color = '#2c3e50')) %>%
          add_lines(y = ~Mean, name = 'Mean',
                    line = list(color = 'black', width = 2)) %>%
          add_lines(y = ~plus1, name = '+1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1)) %>%
          add_lines(y = ~minus1, name = '-1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1)) %>%
          add_lines(y = ~plus2, name = '+2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2)) %>%
          add_lines(y = ~minus2, name = '-2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2)) %>%
          add_lines(y = ~plus3, name = '+3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2)) %>%
          add_lines(y = ~minus3, name = '-3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2)) %>%
          layout(
            xaxis = list(title = "Run ID", tickangle = -45, automargin = TRUE),
            yaxis = list(title = "Cq Value", automargin = TRUE),
            legend = list(orientation = 'h', y = -0.25, x = 0),
            margin = list(t = 40, r = 40, b = 120, l = 60)
          )
      })
    }

    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")

    # =========================================================================
    # QC SCATTER PLOTS (with improved styling)
    # =========================================================================

    output$scatter_tryp <- renderPlotly({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      required_cols <- c("ControlType", "Cq_median_177T", "Cq_median_18S2", "FinalCall", "SampleName")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        return(plotly_empty() %>%
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_177T),
          !is.na(Cq_median_18S2)
        )

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with both 177T and 18S2 data"))
      }

      plot_ly(df, x = ~Cq_median_177T, y = ~Cq_median_18S2,
              color = ~FinalCall,
              colors = c(
                "Positive" = "#27ae60",
                "Positive_DNA" = "#3498db",
                "Positive_RNA" = "#9b59b6",
                "LatePositive" = "#f39c12",
                "Negative" = "#95a5a6",
                "Indeterminate" = "#f1c40f",
                "Invalid_NoDNA" = "#e74c3c"
              ),
              type = 'scatter', mode = 'markers',
              text = ~SampleName,
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "177T Cq: %{x:.2f}<br>",
                "18S2 Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          title = list(text = "Trypanozoon Detection: 18S2 vs 177T", font = list(size = 16)),
          xaxis = list(title = "177T Cq (DNA)"),
          yaxis = list(title = "18S2 Cq (RNA)"),
          legend = list(title = list(text = "Call")),
          hovermode = 'closest'
        )
    })

    output$scatter_rnp <- renderPlotly({
      df <- filtered_samples()

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }

      required_cols <- c("ControlType", "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA", "SampleName", "Delta_RP")
      missing_cols <- setdiff(required_cols, names(df))

      if (length(missing_cols) > 0) {
        return(plotly_empty() %>%
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }

      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_RNAseP_DNA),
          !is.na(Cq_median_RNAseP_RNA)
        ) %>%
        mutate(
          Quality = case_when(
            is.na(Delta_RP) ~ "Unknown",
            Delta_RP <= 5 ~ "Good",
            Delta_RP <= 8 ~ "Moderate",
            TRUE ~ "Poor"
          )
        )

      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with RNAseP data"))
      }

      plot_ly(df, x = ~Cq_median_RNAseP_DNA, y = ~Cq_median_RNAseP_RNA,
              color = ~Quality,
              colors = c("Good" = "#27ae60", "Moderate" = "#f39c12",
                         "Poor" = "#e74c3c", "Unknown" = "#95a5a6"),
              type = 'scatter', mode = 'markers',
              text = ~paste0(SampleName, "<br>ΔCq: ", round(Delta_RP, 2)),
              hovertemplate = paste0(
                "<b>%{text}</b><br>",
                "DNA Cq: %{x:.2f}<br>",
                "RNA Cq: %{y:.2f}<br>",
                "<extra></extra>"
              ),
              marker = list(size = 10, opacity = 0.7)) %>%
        layout(
          title = list(text = "RNA Preservation Quality (ΔCq Analysis)", font = list(size = 16)),
          xaxis = list(title = "RNAseP-DNA Cq"),
          yaxis = list(title = "RNAseP-RNA Cq"),
          legend = list(title = list(text = "RNA Quality")),
          hovermode = 'closest'
        )
    })

    # =========================================================================
    # DOWNLOADS - COMPREHENSIVE
    # =========================================================================

    output$dl_runs <- downloadHandler(
      filename = function() sprintf("mic_runs_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$runs, file)
    )

    output$dl_samples <- downloadHandler(
      filename = function() sprintf("mic_samples_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )

    output$dl_samples_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )

    output$dl_positives <- downloadHandler(
      filename = function() sprintf("mic_positives_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          filter(ControlType == "Sample", FinalCall == "Positive")
        write_csv(df, file)
      }
    )

    output$dl_deltas <- downloadHandler(
      filename = function() sprintf("mic_deltas_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          select(RunID, SampleID, SampleName, Delta_18S2_177T, Delta_RP, any_of("Flags"))
        write_csv(df, file)
      }
    )

    output$dl_flags <- downloadHandler(
      filename = function() sprintf("mic_flags_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          filter(ControlType == "Sample", AnyFlag | FinalCall == "Invalid_NoDNA")
        write_csv(df, file)
      }
    )

    output$dl_lj <- downloadHandler(
      filename = function() sprintf("mic_lj_stats_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        lj <- processed_data()$lj_stats
        stats <- map_dfr(lj, "summary")
        write_csv(stats, file)
      }
    )

    output$dl_controls <- downloadHandler(
      filename = function() sprintf("mic_controls_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$control_status, file)
    )

    output$dl_complete <- downloadHandler(
      filename = function() sprintf("mic_complete_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        if (!requireNamespace("writexl", quietly = TRUE)) {
          showNotification("writexl package required for Excel export", type = "error")
          return()
        }

        pd <- processed_data()
        sheets <- list(
          "Samples" = filtered_samples(),
          "Runs" = pd$runs,
          "Controls" = pd$control_status,
          "LJ_Stats" = map_dfr(pd$lj_stats, "summary"),
          "Replicates" = pd$replicates
        )
        writexl::write_xlsx(sheets, file)
      }
    )

    # Quick export button
    observeEvent(input$export_qc, {
      pd <- processed_data()
      if (!nrow(pd$samples)) {
        showNotification("No data to export", type = "warning")
        return()
      }

      dir.create("outputs", showWarnings = FALSE)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      path <- file.path("outputs", glue("MIC_QC_{timestamp}.csv"))
      write_csv(pd$samples, path)

      showNotification(
        glue("QC export saved: {path}"),
        type = "message",
        duration = 5
      )
    })

  })
}

# Define %||% operator if not already defined (NULL coalescing)
`%||%` <- function(x, y) if (is.null(x)) y else x
