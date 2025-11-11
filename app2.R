# app.R - Mbuji-Mayi Biobank Dashboard v3.1
# ============================================================================

# Load global configuration and libraries
source("global.R")

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- page_navbar(
  title = config$app$title,
  theme = app_theme,
  
  # Sidebar with data loading and filters
  sidebar = mod_data_manager_ui("data_manager"),
  
  # Navigation panels
  mod_data_quality_ui("data_quality"),
  mod_overview_demographics_ui("overview_demographics"),
  mod_transport_ui("transport"),
  mod_extractions_ui("extractions"),
#  mod_pcr_ui("pcr"),
  mod_mic_pcr_ui("mic05")
  
  
  # Add other modules here as they're developed:
  # mod_transport_ui("transport"),
  # mod_lab_results_ui("lab_results"),
  # mod_data_export_ui("data_export"),
  # etc.
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
  
#  mod_pcr_server(
#    "pcr",
#    pcr_dir = "data/PCR",
#    biobank_df = data$clean_data,
#    extractions_df = data$filtered_extractions,
#    filters_reactive = NULL
#  )
  
  mod_mic_pcr_server(
    "mic05", 
    default_dir = "data/MIC"
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
