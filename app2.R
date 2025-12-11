# app.R - Mbuji-Mayi Biobank Dashboard v3.1
# ============================================================================

# Load global configuration and libraries
source("global.R")

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- do.call(
  page_navbar,
  c(
    list(
      title = config$app$title,
      theme = app_theme,

      # Sidebar with data loading and filters
      sidebar = mod_data_manager_ui("data_manager")
    ),

    # Navigation panels
    list(
      mod_data_quality_ui("data_quality"),
      mod_overview_assays_ui("overview_assays"),
      mod_overview_demographics_ui("overview_demographics"),
      mod_geographic_ui("geographic"),
      mod_transport_ui("transport"),
      mod_extractions_ui("extractions"),
      mod_mic_qpcr_coordinator_ui("mic"),
      mod_ielisa_coordinator_ui("ielisa"),
      mod_elisa_pe_ui("elisa_pe"),
      mod_elisa_vsg_ui("elisa_vsg"),
      mod_elisa_concordance_ui("concordance"),
      mod_drs_rnasep_ui("drs_rnasep"),
      # New comprehensive analysis modules
      mod_sample_journey_ui("sample_journey"),
      mod_sample_processing_ui("sample_processing"),
      mod_concordance_ui("concordance_analysis")
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================
server <- function(input, output, session) {
  
  # Core data management module - returns reactive data
  data <- mod_data_manager_server("data_manager")
  
  # Pass data to data quality module
  mod_data_quality_server(
    "data_quality",
    raw_data = data$raw_data,
    clean_data = data$clean_data,
    quality_report = data$quality_report
  )

  mod_overview_assays_server(
    "overview_assays",
    biobank_df = data$filtered_data,
    elisa_df = data$elisa_data,
    ielisa_df = data$ielisa_data,
    mic_df = data$mic_data,
    filters = data$filters
  )

  # Pass data to overview & demographics module
  mod_overview_demographics_server(
    "overview_demographics",
    filtered_data = data$filtered_data
  )

  # Extraction quality module (uses shared data manager reactives)
  mod_extractions_server(
    "extractions",
    filtered_data = data$filtered_extractions,
    biobank_data = data$clean_data
  )

  # MIC qPCR module - FIXED to use new coordinator architecture
  mic_data <- mod_mic_qpcr_coordinator_server(
    "mic",
    biobank_df = data$clean_data,              # ← Biobank data from data manager
    extractions_df = data$filtered_extractions, # ← Extractions data from data manager
    filters = data$filters                      # ← Filters from data manager (FIXED)
  )

  # iELISA module - Inhibition ELISA for LiTat 1.3 and 1.5
  ielisa_data <- mod_ielisa_coordinator_server(
    "ielisa",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # ELISA modules - using new coordinator architecture (capture return values for downstream modules)
  elisa_pe_data <- mod_elisa_pe_server(
    "elisa_pe",
    biobank_df = data$clean_data,
    filters = data$filters
  )
  elisa_vsg_data <- mod_elisa_vsg_server(
    "elisa_vsg",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # Wrap MIC data for transport module (expects $samples structure)
  mic_data_for_transport <- reactive({
    tryCatch({
      if (is.null(mic_data) || is.null(mic_data$qpcr_samples)) return(NULL)
      samples <- mic_data$qpcr_samples()
      if (is.null(samples) || !nrow(samples)) return(NULL)
      list(samples = samples)
    }, error = function(e) {
      message("Warning: Could not get MIC data for transport: ", e$message)
      NULL
    })
  })

  # Wrap iELISA data for transport module (expects dataframe, not list)
  ielisa_data_for_transport <- reactive({
    tryCatch({
      if (is.null(ielisa_data) || is.null(ielisa_data$samples)) return(NULL)
      samples <- ielisa_data$samples()
      if (is.null(samples) || !nrow(samples)) return(NULL)
      samples
    }, error = function(e) {
      message("Warning: Could not get iELISA data for transport: ", e$message)
      NULL
    })
  })

  # Combine ELISA data (PE + VSG) for transport module
  combined_elisa_data <- reactive({
    pe_data <- elisa_pe_data$samples()
    vsg_data <- elisa_vsg_data$samples()

    if (is.null(pe_data)) pe_data <- tibble::tibble()
    if (is.null(vsg_data)) vsg_data <- tibble::tibble()

    if (nrow(pe_data) == 0 && nrow(vsg_data) == 0) return(NULL)

    dplyr::bind_rows(pe_data, vsg_data)
  })

  # Pass filtered data to transport module so visuals respect dashboard filters
  mod_transport_server(
    "transport",
    filtered_data = data$filtered_data,
    mic_data = mic_data_for_transport,
    elisa_data = combined_elisa_data,
    ielisa_data = ielisa_data_for_transport
  )

  # Geographic visualization module (map with test results)
  mod_geographic_server(
    "geographic",
    filtered_data = data$filtered_data,
    mic_data = mic_data$qpcr_samples,
    elisa_pe_data = elisa_pe_data$samples,
    elisa_vsg_data = elisa_vsg_data$samples,
    ielisa_data = ielisa_data$samples
  )

  # ELISA Concordance module (PE vs VSG comparison)
  mod_elisa_concordance_server(
    "concordance",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # Pre-Analytical Factors & RNAseP Quality module
  mod_drs_rnasep_server(
    "drs_rnasep",
    extractions_df = data$filtered_extractions, # ← Extractions data from data manager
    qpcr_data = mic_data$qpcr_samples,          # ← qPCR data from MIC module
    biobank_df = data$filtered_data,            # ← Biobank data for transport fields
    filters = data$filters                      # ← Filters from data manager
  )

  # Sample Journey module (comprehensive sample tracking)
  mod_sample_journey_server(
    "sample_journey",
    biobank_data = data$clean_data,
    extraction_data = data$filtered_extractions,
    mic_data = mic_data$qpcr_samples,
    elisa_pe_data = elisa_pe_data$samples,
    elisa_vsg_data = elisa_vsg_data$samples,
    ielisa_data = ielisa_data$samples
  )

  # Sample Processing module (comprehensive sample processing overview)
  mod_sample_processing_server(
    "sample_processing",
    biobank_df = data$filtered_data,  # Use filtered_data to respect global filters
    extraction_df = data$filtered_extractions,
    mic_df = mic_data$qpcr_samples,
    elisa_pe_df = elisa_pe_data$samples,
    elisa_vsg_df = elisa_vsg_data$samples,
    ielisa_df = ielisa_data$samples,
    filters = data$filters
  )
  
  # Concordance Analysis module (comprehensive statistical analysis)
  mod_concordance_server(
    "concordance_analysis",
    biobank_df = data$clean_data,
    mic_df = mic_data$qpcr_samples,
    elisa_pe_df = elisa_pe_data$samples,
    elisa_vsg_df = elisa_vsg_data$samples,
    ielisa_df = ielisa_data$samples,
    filters = data$filters
  )

  # Session management
  session$onSessionEnded(function() {
    message("Session ended")
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui, server)
