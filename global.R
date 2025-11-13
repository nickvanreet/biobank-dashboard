# global.R - Updated for rebuilt MIC qPCR module
# ============================================================================

# ============================================================================
# CONFIGURATION & SETUP
# ============================================================================

# Install/load packages
required_packages <- c(
  "shiny", "bslib", "tidyverse", "readxl", "writexl", "janitor",
  "DT", "plotly", "lubridate", "scales", "stringr", "stringi",
  "purrr", "dplyr", "tidyr", "ggplot2", "jsonlite", "digest", "glue"
)

for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    install.packages(pkg, quietly = TRUE)
    require(pkg, quietly = TRUE, character.only = TRUE)
  }
}

# ============================================================================
# CONFIGURATION
# ============================================================================

config <- list(
  app = list(
    title = "Mbuji-Mayi Biobank Dashboard"
  ),
  paths = list(
    biobank_dir = "data/biobank",
    extractions_dir = "data/extractions",
    pcr_dir = "data/PCR",
    mic_dir = "data/MIC"  # Note: uppercase MIC to match your directory
  ),
  ui = list(
    theme_primary = "#3498DB",
    theme_success = "#27AE60",
    theme_info = "#2980B9",
    theme_warning = "#F39C12"
  ),
  qc = list(
    max_transport_days = 30
  )
)

# ============================================================================
# LOAD ALL CORE UTILITIES
# ============================================================================

# Data utilities
source("R/core/data_loader_utils.R")       # Must source BEFORE data_linking
source("R/core/data_linking_utils.R")      # Defines normalize_barcode()
source("R/core/extraction_data_utils.R")   # Extraction utilities

# CRITICAL: Source the qPCR analysis pipeline
# This file defines analyze_qpcr(), extract_cq_values(), and related functions
# Make sure this file exists and is named correctly
if (file.exists("R/core/qpcr_analysis.R")) {
  source("R/core/qpcr_analysis.R")
  message("✅ Loaded qpcr_analysis.R")
} else if (file.exists("R/core/mic_qpcr_pipeline.R")) {
  source("R/core/mic_qpcr_pipeline.R")
  message("✅ Loaded mic_qpcr_pipeline.R")
} else {
  warning("⚠️  qPCR analysis file not found! MIC module will not work.")
  warning("    Expected: R/core/qpcr_analysis.R or R/core/mic_qpcr_pipeline.R")
}

# Data cleaner
if (file.exists("R/data/data_cleaner_improved.R")) {
  source("R/data/data_cleaner_improved.R")
}

# ============================================================================
# LOAD ALL MODULES
# ============================================================================

source("R/modules/mod_data_manager.R")
source("R/modules/mod_01_data_quality.R")
source("R/modules/mod_02_overview_demographics.R")
source("R/modules/mod_03_transport.R")
source("R/modules/mod_04_extractions.R")
source("R/modules/mod_05_mic_qpcr.R")  # The rebuilt module
source("R/modules/mod_06_drs_rnasep.R")  # DRS volume vs RNAseP analysis

# ============================================================================
# SOURCE MODULES - ADD THIS SECTION HERE
# ============================================================================
source("R/modules/mod_05a_mic_coordinator.R")
source("R/modules/mod_05b_mic_overview.R")
source("R/modules/mod_05c_mic_samples.R")
source("R/modules/mod_05d_mic_qc.R")
source("R/modules/mod_05e_mic_analysis.R")
source("R/modules/mod_05f_mic_export.R")
source("R/modules/mod_05g_mic_settings.R")

# Reassign mod_mic_qpcr_ui to use the coordinator (returns list of nav_panels)
# This must be done AFTER mod_05a_mic_coordinator.R is loaded
mod_mic_qpcr_ui <- mod_mic_qpcr_coordinator_ui
mod_mic_qpcr_server <- mod_mic_qpcr_coordinator_server

cat("✓ MIC modules loaded\n")
# ============================================================================


# ============================================================================
# UI THEME
# ============================================================================

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2563EB",   # Lighter cobalt blue for primary actions
  success = "#16A34A",   # Functional green for success KPI states
  danger = "#DC2626",    # Accessible red for alerts and errors
  warning = "#F59E0B",   # Warm amber for warning KPI states
  info = "#0EA5E9",      # Bright cyan for informational states
  heading_font = "'Helvetica Neue', Helvetica, Arial, sans-serif",
  base_font = "'Source Sans Pro', 'Segoe UI', system-ui, sans-serif"
)

# Add custom CSS for scrolling and elegant UI
app_theme <- bslib::bs_add_rules(
  app_theme,
  "
  /* Enable smooth scrolling */
  html {
    scroll-behavior: smooth;
  }

  body {
    background-color: #f5f7fb;
    color: #0f172a;
  }

  /* Make main content area scrollable */
  .container-fluid {
    overflow-y: auto;
    max-height: 100vh;
    padding-bottom: 2rem;
  }

  /* Lighter card styling */
  .card {
    border-radius: 12px;
    box-shadow: 0 10px 30px rgba(15, 23, 42, 0.08);
    transition: box-shadow 0.3s ease;
    background-color: #ffffff;
    border: 1px solid #e2e8f0;
  }

  .card:hover {
    box-shadow: 0 18px 38px rgba(15, 23, 42, 0.12);
  }

  /* Value box enhancements */
  .bslib-value-box {
    border-radius: 12px;
    transition: transform 0.2s ease, box-shadow 0.2s ease;
    box-shadow: 0 12px 20px rgba(15, 23, 42, 0.08);
    background: linear-gradient(135deg, rgba(255, 255, 255, 0.96), rgba(241, 245, 249, 0.96));
    border: 1px solid rgba(226, 232, 240, 0.8);
  }

  .bslib-value-box:hover {
    transform: translateY(-4px);
    box-shadow: 0 20px 28px rgba(15, 23, 42, 0.12);
  }

  .bslib-value-box .value-box-title {
    color: #64748b;
    font-weight: 600;
    letter-spacing: 0.02em;
  }

  .bslib-value-box .value-box-value {
    color: #0f172a;
    font-weight: 700;
  }

  /* Use functional accent rings to highlight KPI state */
  .bslib-value-box[data-theme='primary'],
  .bslib-value-box.bg-primary { border-left: 6px solid #2563EB; }
  .bslib-value-box[data-theme='success'],
  .bslib-value-box.bg-success { border-left: 6px solid #16A34A; }
  .bslib-value-box[data-theme='danger'],
  .bslib-value-box.bg-danger { border-left: 6px solid #DC2626; }
  .bslib-value-box[data-theme='warning'],
  .bslib-value-box.bg-warning { border-left: 6px solid #F59E0B; }
  .bslib-value-box[data-theme='info'],
  .bslib-value-box.bg-info { border-left: 6px solid #0EA5E9; }

  /* Table styling enhancements */
  .dataTables_wrapper {
    font-family: 'Source Sans Pro', 'Segoe UI', system-ui, sans-serif;
    color: #1e293b;
  }

  .table {
    font-size: 13px;
    color: #0f172a;
    background-color: #ffffff;
  }

  .table thead th {
    background-color: #e2e8f0;
    font-weight: 600;
    border-bottom: 2px solid #cbd5f5;
    color: #1e293b;
  }

  .table-striped tbody tr:nth-of-type(odd) {
    background-color: rgba(226, 232, 240, 0.4);
  }

  .table-hover tbody tr:hover {
    background-color: rgba(37, 99, 235, 0.08);
  }

  /* Plotly chart containers */
  .plotly {
    border-radius: 8px;
  }

  /* Card headers */
  .card-header {
    font-weight: 600;
    background-color: #f1f5f9;
    border-bottom: 1px solid #e2e8f0;
    color: #0f172a;
  }

  .card-body,
  .card-footer {
    background-color: #ffffff;
  }
  "
)

# ============================================================================
# APP CONSTANTS
# ============================================================================

APP_CONSTANTS <- list(
  DT_OPTIONS = list(
    pageLength = 20,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  )
)

# ============================================================================
# ELEGANT TABLE STYLING HELPER
# ============================================================================

#' Create elegant styled DataTable
#' @param data Data frame to display
#' @param ... Additional DT::datatable arguments
#' @export
create_elegant_table <- function(data,
                                 pageLength = 25,
                                 buttons = c('copy', 'csv', 'excel'),
                                 filter_position = "top",
                                 striped = TRUE,
                                 hover = TRUE,
                                 compact = TRUE,
                                 bordered = FALSE,
                                 ...) {

  # Build class string
  classes <- c("table")
  if (striped) classes <- c(classes, "table-striped")
  if (hover) classes <- c(classes, "table-hover")
  if (compact) classes <- c(classes, "table-sm")
  if (bordered) classes <- c(classes, "table-bordered")

  class_str <- paste(classes, collapse = " ")

  # Create datatable with elegant styling
  dt <- DT::datatable(
    data,
    options = list(
      pageLength = pageLength,
      scrollX = TRUE,
      scrollY = "500px",
      scrollCollapse = TRUE,
      dom = 'Bfrtip',
      buttons = buttons,
      columnDefs = list(
        list(className = 'dt-center', targets = "_all")
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'font-weight': 'bold'});",
        "}"
      )
    ),
    extensions = 'Buttons',
    rownames = FALSE,
    class = class_str,
    filter = filter_position,
    ...
  ) %>%
    DT::formatStyle(
      columns = 1:ncol(data),
      fontSize = '13px',
      fontFamily = "'Source Sans Pro', 'Segoe UI', system-ui, sans-serif"
    )

  return(dt)
}

# ============================================================================
# HELPER FUNCTIONS (if not in other files)
# ============================================================================

# These might be needed by modules but not defined elsewhere
if (!exists("normalize_id")) {
  normalize_id <- function(x) {
    if (is.null(x)) return(NA_character_)
    x %>% as.character() %>% stringr::str_trim() %>% toupper()
  }
}

if (!exists("safe_coalesce")) {
  safe_coalesce <- function(...) {
    args <- list(...)
    for (arg in args) {
      if (!is.null(arg) && length(arg) > 0 && !all(is.na(arg))) {
        return(arg)
      }
    }
    return(NA)
  }
}

# ============================================================================
# ENVIRONMENT VERIFICATION
# ============================================================================

# Check that all critical functions are available
critical_functions <- c(
  "normalize_barcode",
  "analyze_qpcr",             # From qpcr_analysis.R
  "extract_cq_values",        # From qpcr_analysis.R
  "link_extraction_to_biobank",
  "apply_filters",
  "mod_data_manager_ui",
  "mod_mic_qpcr_ui",          # The new module
  "mod_mic_qpcr_server"       # The new module
)

missing_functions <- critical_functions[!sapply(critical_functions, exists)]

if (length(missing_functions) > 0) {
  warning("⚠️  Missing functions in global environment:")
  for (fn in missing_functions) {
    warning(sprintf("   - %s", fn))
  }
  warning("\n⚠️  Some modules may not work correctly!")
} else {
  message("✅ All critical functions loaded successfully")
}

# Create necessary directories
dirs_to_create <- c(
  "outputs",
  config$paths$biobank_dir,
  config$paths$extractions_dir,
  config$paths$pcr_dir,
  config$paths$mic_dir
)

for (dir in dirs_to_create) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# ============================================================================
# STARTUP MESSAGE
# ============================================================================

message("╔═══════════════════════════════════════════════════════════════╗")
message("║  Mbuji-Mayi Biobank Dashboard - Global Configuration         ║")
message("╚═══════════════════════════════════════════════════════════════╝")
message("")
message(sprintf("📁 Biobank directory:    %s", config$paths$biobank_dir))
message(sprintf("📁 Extractions directory: %s", config$paths$extractions_dir))
message(sprintf("📁 PCR directory:        %s", config$paths$pcr_dir))
message(sprintf("📁 MIC directory:        %s", config$paths$mic_dir))
message("")
message(sprintf("✅ %d packages loaded", length(required_packages)))
message(sprintf("✅ %d critical functions verified", 
                length(critical_functions) - length(missing_functions)))
message("")

if (length(missing_functions) > 0) {
  message("⚠️  WARNING: Some functions are missing. Check the output above.")
} else {
  message("🚀 Ready to launch application!")
}
message("")