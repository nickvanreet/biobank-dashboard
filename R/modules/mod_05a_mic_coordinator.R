# ==============================================================================
# MIC qPCR - PARENT COORDINATOR MODULE (ULTRAMODERN ARCHITECTURE)
# ==============================================================================
# Modular 4-step pipeline integration with clean, modern UI design
# ==============================================================================

mod_mic_qpcr_coordinator_ui <- function(id) {
  ns <- NS(id)

  # Return single navigation panel with three sub-tabs (like ELISA)
  nav_panel(
    title = "MIC",
    icon = icon("microscope"),
    # Add controls above the tabs
    card(
      card_body(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex flex-column justify-content-start gap-2",
            div(
              class = "d-flex align-items-center",
              checkboxInput(
                ns("exclude_invalid_runs"),
                "Exclude invalid runs",
                value = TRUE
              ),
              tags$small(
                class = "text-muted ms-2",
                "Removes runs with failed control QC"
              )
            ),
            div(
              class = "d-flex align-items-center",
              checkboxInput(
                ns("show_retests_only"),
                "Show retested samples only",
                value = FALSE
              ),
              tags$small(
                class = "text-muted ms-2",
                "Filter samples with multiple test dates"
              )
            )
          ),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(
              ns("refresh"),
              "Refresh Data",
              icon = icon("sync"),
              class = "btn-sm btn-outline-secondary"
            ),
            actionButton(
              ns("settings"),
              "QC Settings",
              icon = icon("sliders"),
              class = "btn-sm btn-outline-secondary"
            )
          )
        )
      )
    ),
    navset_card_tab(
      id = ns("mic_tabs"),
      # Tab 1: Runs (overview)
      nav_panel(
        title = "MIC - Runs",
        value = "runs",
        mod_mic_overview_ui(ns("overview"))
      ),
      # Tab 2: Samples
      nav_panel(
        title = "MIC - Samples",
        value = "samples",
        mod_mic_samples_ui(ns("samples"))
      ),
      # Tab 3: Analysis
      nav_panel(
        title = "MIC - Analysis",
        value = "analysis",
        mod_mic_analysis_ui(ns("analysis"))
      )
    )
  )
}

mod_mic_qpcr_coordinator_server <- function(id, biobank_df, extractions_df, filters) {
  moduleServer(id, function(input, output, session) {
    
    # =========================================================================
    # SHARED DATA LAYER - Computed ONCE, used by all child modules
    # =========================================================================
    
    cache_state <- reactiveVal(list())
    
    settings <- reactiveVal(list(
      thresholds = list(
        `177T` = list(positive = 35, negative = 40),
        `18S2` = list(positive = 35, negative = 40),
        RNAseP_DNA = list(positive = 32, negative = 45),
        RNAseP_RNA = list(positive = 30, negative = 45)
      ),
      late_window = c(38, 40),
      delta_rp_limit = 8,
      allow_review_controls = FALSE,
      min_positive_reps = 2  # Default: require 2/4 replicates for positive call
    ))
    
    # Settings modal
    observeEvent(input$settings, {
      showModal(modalDialog(
        id = session$ns("settings_modal"),
        title = "qPCR Settings",
        size = "xl",
        easyClose = TRUE,
        mod_mic_settings_ui(session$ns("settings_content"))
      ))
    })
    
    # Core data loading - shared by all modules
    # Use default path from config
    mic_dir <- reactive({
      if (!is.null(config$site_paths) && !is.null(config$site_paths$mic_dir)) {
        config$site_paths$mic_dir
      } else {
        "data/MIC"
      }
    })

    raw_data <- reactive({
      dir_path <- mic_dir()

      withProgress(message = "Loading MIC files...", value = 0.3, {
        parsed <- parse_mic_directory(dir_path, settings(), cache_state())
        cache_state(parsed$cache)
        parsed
      })
    }) %>% bindCache(mic_dir(), settings())
    
    # Force refresh
    observeEvent(input$refresh, {
      cache_state(list())
    })
    
    # Processed data - shared by all modules
    processed_data <- reactive({
      rd <- raw_data()

      if (!nrow(rd$samples)) {
        return(list(
          runs = tibble(),
          samples = tibble(),
          replicates = tibble(),
          control_status = tibble(),
          lj_stats = list(),
          files = rd$files
        ))
      }

      withProgress(message = "Processing data...", value = 0.5, {
        # Validate controls
        control_status <- validate_controls(rd$samples, settings())

        # Create run summary
        runs_summary <- create_run_summary(rd$samples, rd$runs, control_status)

        invalid_runs <- if (nrow(runs_summary)) {
          runs_summary %>%
            filter(!RunValid) %>%
            pull(RunID)
        } else {
          character()
        }

        # Link to biobank and extractions
        samples_linked <- rd$samples %>%
          link_to_biobank(if (is.null(biobank_df)) NULL else biobank_df()) %>%
          link_to_extractions(if (is.null(extractions_df)) NULL else extractions_df())
        
        # Apply control flags (deduplicate control_status first to prevent duplicate rows)
        samples_linked <- samples_linked %>%
          left_join(
            control_status %>%
              select(RunID, SampleID, ControlFlag) %>%
              distinct(RunID, SampleID, .keep_all = TRUE),
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
        if (!settings()$allow_review_controls && length(invalid_runs)) {
          samples_linked <- samples_linked %>%
            mutate(
              FinalCall = if_else(RunID %in% invalid_runs, "RunInvalid", FinalCall),
              Flags = if_else(RunID %in% invalid_runs,
                              paste(Flags, "RunInvalid", sep = ";"),
                              Flags),
              AnyFlag = if_else(RunID %in% invalid_runs, TRUE, AnyFlag)
            )
        }

        replicates_df <- rd$replicates
        if (is.null(replicates_df)) {
          replicates_df <- tibble()
        }

        if (isTRUE(input$exclude_invalid_runs) && length(invalid_runs)) {
          samples_linked <- samples_linked %>%
            filter(!RunID %in% invalid_runs)

          if (!is.null(replicates_df) && nrow(replicates_df)) {
            replicates_df <- replicates_df %>% filter(!RunID %in% invalid_runs)
          }
        }

        # Compute Levey-Jennings stats
        lj_stats <- list(
          `177T` = compute_levey_jennings(replicates_df, "177T"),
          `18S2` = compute_levey_jennings(replicates_df, "18S2"),
          RNAseP_DNA = compute_levey_jennings(replicates_df, "RNAseP_DNA"),
          RNAseP_RNA = compute_levey_jennings(replicates_df, "RNAseP_RNA")
        )

        # Final deduplication safety check - remove any duplicate rows
        # Keep the first occurrence of each unique RunID+SampleID combination
        samples_linked <- samples_linked %>%
          distinct(RunID, SampleID, .keep_all = TRUE)

        # =======================================================================
        # RETEST CONSOLIDATION
        # =======================================================================
        # Apply consolidation to resolve retested samples with potentially
        # discordant results. Uses conservative logic: any positive = positive.
        # Adds columns: mic_status_final, mic_n_tests, mic_is_discordant, etc.

        samples_consolidated <- tryCatch({
          consolidate_mic_results(
            samples_linked,
            sample_id_col = "SampleID",
            status_col = "FinalCall",
            include_all_runs = TRUE
          )
        }, error = function(e) {
          message("Warning: MIC consolidation failed: ", e$message)
          samples_linked
        })

        # Get discordance summary for reporting
        discordance_summary <- tryCatch({
          get_mic_discordance_report(samples_consolidated, "SampleID")
        }, error = function(e) {
          tibble()
        })

        # Get one-row-per-sample summary
        sample_summary <- tryCatch({
          get_mic_sample_summary(samples_consolidated, "SampleID")
        }, error = function(e) {
          tibble()
        })

        list(
          runs = runs_summary,
          samples = samples_consolidated,
          replicates = replicates_df,
          control_status = control_status,
          lj_stats = lj_stats,
          files = rd$files,
          # New consolidation outputs
          discordance_summary = discordance_summary,
          sample_summary = sample_summary
        )
      })
    }) %>% bindCache(raw_data(), settings(), input$exclude_invalid_runs)
    
    # Apply global filters once
    filtered_base <- reactive({
      df <- processed_data()$samples
      if (!nrow(df)) return(df)

      df <- apply_global_filters(df, if (is.null(filters)) NULL else filters())

      # Apply retest filter if enabled
      if (isTRUE(input$show_retests_only)) {
        # Identify samples that have been tested multiple times
        if (all(c("SampleID", "RunDate") %in% names(df))) {
          retested_samples <- df %>%
            group_by(SampleID) %>%
            filter(n_distinct(RunDate) > 1) %>%
            ungroup()

          message("DEBUG [mod_mic_coordinator]: Showing only retested samples: ",
                  n_distinct(retested_samples$SampleID), " samples")

          df <- retested_samples
        }
      }

      df
    })

    filtered_replicates <- reactive({
      base_df <- filtered_base()
      reps <- processed_data()$replicates

      if (is.null(reps) || !nrow(reps) || is.null(base_df) || !nrow(base_df)) {
        return(tibble())
      }

      reps %>%
        semi_join(
          base_df %>% select(RunID, SampleID) %>% distinct(),
          by = c("RunID", "SampleID")
        )
    })
    
    # =========================================================================
    # CALL CHILD MODULES - Pass shared reactive data
    # =========================================================================
    
    # Settings module
    updated_settings <- mod_mic_settings_server("settings_content", settings)
    observeEvent(updated_settings(), {
      settings(updated_settings())
    })
    
    # Overview module - KPIs and summary
    mod_mic_overview_server("overview", processed_data, filtered_base)

    # Samples module - Main results table
    mod_mic_samples_server("samples", filtered_base, processed_data)

    # Analysis module - Scatter plots
    mod_mic_analysis_server("analysis", filtered_base, filtered_replicates)

    # =========================================================================
    # RETURN DATA - For downstream modules (e.g., DRS/RNAseP)
    # =========================================================================

    return(list(
      qpcr_samples = filtered_base,       # Filtered samples for DRS module
      processed_data = processed_data,    # Full processed data
      raw_data = raw_data                 # Raw data if needed
    ))
  })
}
