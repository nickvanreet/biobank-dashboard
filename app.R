# app.R - Mbuji-Mayi Biobank Dashboard v3.1
# ============================================================================

# Load global configuration and libraries
source("global.R")

# ============================================================================
# USER INTERFACE
# ============================================================================

# Build UI with dynamic MIC panels
mic_panels <- mod_mic_qpcr_ui("mic_qpcr")  # Returns list of nav_panels

# Create compact header with site information
current_site_info <- config$sites[[config$current_site]]
app_header <- tags$div(
  style = "display: flex; align-items: center; gap: 8px;",
  tags$span(
    icon("dna"),
    style = "font-size: 1.1rem; color: #4F46E5;"
  ),
  tags$span(
    paste0(current_site_info$short_name, " - ", current_site_info$location),
    style = "font-weight: 600; font-size: 1rem; letter-spacing: -0.01em;"
  )
)

ui <- do.call(
  page_navbar,
  c(
    list(
      title = app_header,
      theme = app_theme,
      sidebar = mod_data_manager_ui("data_manager"),
      fillable = TRUE,
      bg = "#ffffff",
      position = "fixed-top"
    ),
    # Navigation panels
    list(
      mod_data_quality_ui("data_quality"),
      mod_overview_assays_ui("overview_assays"),
      mod_overview_demographics_ui("overview_demographics"),
      mod_transport_ui("transport"),
      mod_extractions_ui("extractions")
    ),
    # Add MIC panels dynamically
    mic_panels,
    # Remaining panels
    list(
      mod_elisa_pe_ui("elisa_pe"),
      mod_elisa_vsg_ui("elisa_vsg"),
      mod_ielisa_ui("ielisa"),
      mod_elisa_concordance_ui("elisa_concordance"),
      mod_drs_rnasep_ui("drs_rnasep"),
      # New comprehensive analysis modules
      mod_sample_journey_ui("sample_journey"),
      mod_concordance_ui("concordance")
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

  mod_overview_assays_server(
    "overview_assays",
    biobank_df = data$filtered_data,
    elisa_df = data$elisa_data,
    ielisa_df = data$ielisa_data,
    mic_df = data$mic_data,
    filters = data$filters
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

  # MIC qPCR module (uses coordinator pattern)
  mic_data <- mod_mic_qpcr_server(
    "mic_qpcr",
    biobank_df = data$clean_data,
    extractions_df = data$filtered_extractions,
    filters = data$filters
  )

  # ELISA modules (rebuilt with coordinator pattern)
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

  # iELISA module (inhibition ELISA for LiTat 1.3 and 1.5)
  ielisa_data <- mod_ielisa_server(
    "ielisa",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # ELISA Concordance module (compares PE and VSG results)
  elisa_concordance_data <- mod_elisa_concordance_server(
    "elisa_concordance",
    biobank_df = data$clean_data,
    filters = data$filters
  )

  # DRS volume vs RNAseP analysis module
  mod_drs_rnasep_server(
    "drs_rnasep",
    extractions_df = data$filtered_extractions,
    qpcr_data = mic_data$qpcr_samples,  # Now linked to qPCR data from MIC module
    filters = data$filters
  )

  # Sample Journey module (comprehensive sample tracking)
  mod_sample_journey_server(
    "sample_journey",
    biobank_df = data$clean_data,
    extraction_df = data$filtered_extractions,
    mic_df = mic_data$qpcr_samples,
    elisa_pe_df = elisa_pe_data$samples,
    elisa_vsg_df = elisa_vsg_data$samples,
    ielisa_df = ielisa_data$samples,
    filters = data$filters
  )

  # Concordance Analysis module (comprehensive statistical analysis)
  mod_concordance_server(
    "concordance",
    biobank_df = data$clean_data,
    mic_df = mic_data$qpcr_samples,
    elisa_pe_df = elisa_pe_data$samples,
    elisa_vsg_df = elisa_vsg_data$samples,
    ielisa_df = ielisa_data$samples,
    filters = data$filters
  )

  # Session management
  session$onSessionEnded(function() {
    # Clean up temporary files or connections if needed
    message("Session ended")
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui, server)
