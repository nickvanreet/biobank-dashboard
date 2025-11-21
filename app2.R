# app.R - Mbuji-Mayi Biobank Dashboard v3.1
# ============================================================================

# Load global configuration and libraries
source("global.R")

# ============================================================================
# USER INTERFACE
# ============================================================================

mic_panels <- mod_mic_qpcr_coordinator_ui("mic")

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
      mod_overview_demographics_ui("overview_demographics"),
      mod_transport_ui("transport"),
      mod_extractions_ui("extractions")
    ),

    # Add MIC panels dynamically
    mic_panels,

    # Remaining panels
    list(
      mod_ielisa_coordinator_ui("ielisa"),
      mod_elisa_pe_ui("elisa_pe"),
      mod_elisa_vsg_ui("elisa_vsg"),
      mod_elisa_concordance_ui("concordance"),
      mod_drs_rnasep_ui("drs_rnasep")
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
  
  # Pass data to overview & demographics module
  mod_overview_demographics_server(
    "overview_demographics",
    filtered_data = data$filtered_data
  )
  
  # Pass filtered data to transport module so visuals respect dashboard filters
  mod_transport_server(
    "transport",
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

  # ELISA modules - using new coordinator architecture
  mod_elisa_pe_server(
    "elisa_pe",
    biobank_df = data$clean_data,
    filters = data$filters
  )
  mod_elisa_vsg_server(
    "elisa_vsg",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # ELISA Concordance module (PE vs VSG comparison)
  mod_elisa_concordance_server(
    "concordance",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # DRS vs RNAseP module
  mod_drs_rnasep_server(
    "drs_rnasep",
    extractions_df = data$filtered_extractions, # ← Extractions data from data manager
    qpcr_data = mic_data$qpcr_samples,          # ← qPCR data from MIC module
    filters = data$filters                      # ← Filters from data manager
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
