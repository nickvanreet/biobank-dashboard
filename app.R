# app.R - Mbuji-Mayi Biobank Dashboard v3.0
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
  mod_data_quality_ui("data_quality")
  
  # Add other modules here as they're developed:
  # mod_overview_ui("overview"),
  # mod_transport_ui("transport"),
  # mod_demographics_ui("demographics"),
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
  
  # Add other module servers here:
  # mod_overview_server("overview", filtered_data = data$filtered_data)
  # mod_transport_server("transport", filtered_data = data$filtered_data)
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
