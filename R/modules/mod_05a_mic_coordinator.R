# ==============================================================================
# MIC qPCR - PARENT COORDINATOR MODULE
# Handles data loading and coordinates child modules
# ==============================================================================

mod_mic_qpcr_coordinator_ui <- function(id) {
  ns <- NS(id)

  # Return a list of nav_panels for top-level navigation
  # Note: Action bar removed to avoid navigation container errors
  list(
    # Module 1: Overview
    nav_panel(
      title = "MIC Overview",
      icon = icon("dashboard"),
      value = "mic_overview",
      mod_mic_overview_ui(ns("overview"))
    ),

    # Module 2: Sample Results
    nav_panel(
      title = "MIC - Samples",
      icon = icon("vials"),
      value = "mic_samples",
      mod_mic_samples_ui(ns("samples"))
    ),

    # Module 3: Quality Control
    nav_panel(
      title = "MIC - QC & Controls",
      icon = icon("chart-line"),
      value = "mic_qc",
      mod_mic_qc_ui(ns("qc"))
    ),

    # Module 4: Analysis
    nav_panel(
      title = "MIC - Analysis",
      icon = icon("chart-scatter"),
      value = "mic_analysis",
      mod_mic_analysis_ui(ns("analysis"))
    ),

    # Module 5: Export
    nav_panel(
      title = "MIC - Export",
      icon = icon("download"),
      value = "mic_export",
      mod_mic_export_ui(ns("export"))
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
      allow_review_controls = FALSE
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
    raw_data <- reactive({
      req(input$mic_dir)
      
      withProgress(message = "Loading MIC files...", value = 0.3, {
        parsed <- parse_mic_directory(input$mic_dir, settings(), cache_state())
        cache_state(parsed$cache)
        parsed
      })
    }) %>% bindCache(input$mic_dir, settings())
    
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
          lj_stats = list()
        ))
      }
      
      withProgress(message = "Processing data...", value = 0.5, {
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
          
          samples_linked <- samples_linked %>%
            mutate(
              FinalCall = if_else(RunID %in% invalid_runs, "RunInvalid", FinalCall),
              Flags = if_else(RunID %in% invalid_runs, 
                              paste(Flags, "RunInvalid", sep = ";"), 
                              Flags),
              AnyFlag = if_else(RunID %in% invalid_runs, TRUE, AnyFlag)
            )
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
    }) %>% bindCache(raw_data(), settings())
    
    # Apply global filters once
    filtered_base <- reactive({
      df <- processed_data()$samples
      if (!nrow(df)) return(df)
      apply_global_filters(df, if (is.null(filters)) NULL else filters())
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
    
    # QC module - Controls and Levey-Jennings
    mod_mic_qc_server("qc", processed_data)
    
    # Analysis module - Scatter plots
    mod_mic_analysis_server("analysis", filtered_base)
    
    # Export module - All downloads
    mod_mic_export_server("export", processed_data, filtered_base)

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
