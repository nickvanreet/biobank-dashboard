# app.R - Minimal, clean main application file
# Mbuji-Mayi Biobank Dashboard v3.0
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
  
  # Navigation panels - each module provides its own UI
  mod_data_quality_ui("data_quality"),
  mod_overview_ui("overview"),
  mod_transport_ui("transport"),
  mod_demographics_ui("demographics"),
  mod_geography_ui("geography"),
  mod_extraction_qc_ui("extraction_qc"),
  mod_lab_results_ui("lab_results"),
  mod_data_export_ui("data_export"),
  
  # Optional debug panel (only in development)
  if (config$app$debug_mode) mod_debug_ui("debug")
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Core data management module
  data <- mod_data_manager_server("data_manager")
  
  # Pass data to each module
  mod_data_quality_server("data_quality", 
                          raw_data = data$raw_data,
                          clean_data = data$clean_data,
                          quality_report = data$quality_report)
  
  mod_overview_server("overview", 
                      filtered_data = data$filtered_data)
  
  mod_transport_server("transport", 
                       filtered_data = data$filtered_data)
  
  mod_demographics_server("demographics", 
                          filtered_data = data$filtered_data,
                          full_data = data$clean_data)
  
  mod_geography_server("geography", 
                       filtered_data = data$filtered_data,
                       lab_data = data$lab_data)
  
  mod_extraction_qc_server("extraction_qc", 
                           biobank_data = data$clean_data)
  
  lab_results <- mod_lab_results_server("lab_results", 
                                        biobank_data = data$clean_data)
  
  mod_data_export_server("data_export", 
                         filtered_data = data$filtered_data,
                         lab_data = lab_results$combined_data)
  
  # Debug module (development only)
  if (config$app$debug_mode) {
    mod_debug_server("debug", data = data, config = config)
  }
  
  # Session management
  session$onSessionEnded(function() {
    # Clean up temporary files or connections if needed
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui, server)
