# app.R - Mbuji-Mayi Biobank Dashboard v3.1
# ============================================================================

# Load global configuration and libraries
source("global.R")

# ============================================================================
# USER INTERFACE
# ============================================================================

# Build UI with dynamic MIC panels
mic_panels <- mod_mic_qpcr_ui("mic_qpcr")  # Returns list of nav_panels

# Create elegant header
app_header <- tags$div(
  style = "display: flex; align-items: center; gap: 12px;",
  tags$span(
    icon("dna"),
    style = "font-size: 1.5rem; color: #4F46E5;"
  ),
  tags$span(
    config$app$title,
    style = "font-weight: 600; font-size: 1.25rem; letter-spacing: -0.02em;"
  ),
  tags$span(
    "Analytics Platform",
    style = "font-size: 0.875rem; color: #64748b; font-weight: 500; margin-left: 8px;"
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
      mod_overview_demographics_ui("overview_demographics"),
      mod_transport_ui("transport"),
      mod_extractions_ui("extractions")
    ),
    # Add MIC panels dynamically
    mic_panels,
    # Remaining panels
    list(
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
  
  mic_data <- mod_mic_qpcr_server(
    "mic_qpcr",
    biobank_df = data$clean_data,
    extractions_df = data$filtered_extractions,
    filters = data$filters
  )

  # DRS volume vs RNAseP analysis module
  mod_drs_rnasep_server(
    "drs_rnasep",
    extractions_df = data$filtered_extractions,
    qpcr_data = mic_data$qpcr_samples,  # Now linked to qPCR data from MIC module
    filters = data$filters
  )
  
  # Add other module servers here:
  # mod_transport_server("transport", filtered_data = data$filtered_data)
  # mod_lab_results_server("lab_results", biobank_data = data$clean_data)
  # etc.
  
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
