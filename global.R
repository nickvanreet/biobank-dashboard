# global.R - Updated for rebuilt MIC qPCR module (Simplified UI)
# ============================================================================

# ============================================================================
# CONFIGURATION & SETUP
# ============================================================================

# Install/load packages
required_packages <- c(
  "shiny", "bslib", "tidyverse", "readxl", "writexl", "janitor",
  "DT", "plotly", "lubridate", "scales", "stringr", "stringi",
  "purrr", "dplyr", "tidyr", "ggplot2", "jsonlite", "digest", "glue",
  # New packages for Sample Journey and Concordance modules
  "irr", "pROC", "randomForest", "xgboost", "rmarkdown", "openxlsx", "officer",
  # Packages for PDF export functionality
  "knitr", "kableExtra",
  # Packages for Geographic visualization module
  "sf", "leaflet", "viridisLite", "htmltools"
)

for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    install.packages(pkg, quietly = TRUE)
    require(pkg, quietly = TRUE, character.only = TRUE)
  }
}

# Check for TinyTeX (required for PDF export)
if (!require("tinytex", quietly = TRUE)) {
  message("Installing tinytex package for PDF export functionality...")
  install.packages("tinytex", quietly = TRUE)
  require("tinytex", quietly = TRUE)
}

# Check if TinyTeX is installed
if (!tinytex::is_tinytex()) {
  message("Note: TinyTeX (LaTeX) is not installed. PDF export will not work.")
  message("To enable PDF export, run: tinytex::install_tinytex()")
  message("Or use: source('R/install_pdf_dependencies.R')")
}

# ============================================================================
# CONFIGURATION
# ============================================================================

# Load configuration from config.yml if available
if (!require("yaml", quietly = TRUE)) {
  install.packages("yaml", quietly = TRUE)
  library(yaml)
}

config_yml <- if (file.exists("config.yml")) {
  yaml::read_yaml("config.yml")
} else {
  list()
}

# Build config with site support
config <- list(
  app = list(
    title = config_yml$app$title %||% "Multi-Site Biobank Dashboard",
    version = config_yml$app$version %||% "3.2.0",
    institution = config_yml$app$institution %||% "Institute of Tropical Medicine, Antwerp",
    debug_mode = config_yml$app$debug_mode %||% TRUE,
    default_site = config_yml$app$default_site %||% "lsd"
  ),
  sites = config_yml$sites %||% list(
    lsd = list(
      name = "Laboratoire de Santé de Dipumba (LSD)",
      short_name = "LSD",
      location = "Mbuji-Mayi, DRC",
      institution = "Dipumba Hospital",
      folder = "lsd"
    )
  ),
  paths = config_yml$paths %||% list(
    biobank_dir = "data/biobank",
    extractions_dir = "data/extractions",
    pcr_dir = "data/PCR",
    mic_dir = "data/MIC",
    ielisa_dir = "data/lsd/ielisa"
  ),
  ui = config_yml$ui %||% list(
    theme_primary = "#3498DB",
    theme_success = "#27AE60",
    theme_info = "#2980B9",
    theme_warning = "#F39C12"
  ),
  qc = config_yml$qc %||% list(
    max_transport_days = 30
  )
)

# Helper function to get site-specific paths
get_site_paths <- function(site_id) {
  if (is.null(config$sites[[site_id]])) {
    stop(sprintf("Unknown site: %s", site_id))
  }

  site_folder <- config$sites[[site_id]]$folder
  base_path <- file.path("data", site_folder)

  list(
    biobank_dir = file.path(base_path, "biobank"),
    extractions_dir = file.path(base_path, "extractions"),
    elisa_pe_dir = file.path(base_path, "elisa_pe"),
    elisa_vsg_dir = file.path(base_path, "elisa_vsg"),
    ielisa_dir = file.path(base_path, "ielisa"),
    mic_dir = file.path(base_path, "mic"),
    pcr_dir = file.path(base_path, "pcr"),
    cache_dir = file.path(base_path, "cache")
  )
}

# Set default site paths
config$current_site <- config$app$default_site
config$site_paths <- get_site_paths(config$current_site)

# ============================================================================
# LOAD ALL CORE UTILITIES
# ============================================================================

# Data utilities
source("R/core/data_loader_utils.R")       # Must source BEFORE data_linking
source("R/core/data_linking_utils.R")      # Defines normalize_barcode()
source("R/core/extraction_data_utils.R")   # Extraction utilities
source("R/core/cache_manager.R")           # Unified cache management
source("R/utils_elisa.R")                  # ELISA loader utilities
source("R/data/dashboard_data_utils.R")    # Shared assay prep utilities

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
# RETEST CONSOLIDATION UTILITIES
# ============================================================================
# Generic utilities for resolving retested samples across all test types
source("R/utils_thresholds.R")        # Threshold configuration loader
source("R/utils_consolidate.R")       # Generic retest resolution functions

# Test-specific consolidation modules
source("R/modules/mic/consolidate_mic.R")       # MIC consolidation
source("R/modules/elisa_pe/consolidate_elisa.R") # ELISA consolidation
source("R/modules/ielisa/consolidate_ielisa.R") # iELISA consolidation

cat("✓ Consolidation utilities loaded\n")

# ============================================================================
# LOAD ALL MODULES
# ============================================================================

source("R/modules/mod_data_manager.R")
source("R/modules/mod_01_data_quality.R")
source("R/modules/mod_02_overview_demographics.R")
source("R/modules/mod_03_transport.R")
source("R/modules/mod_overview_assays.R")
source("R/modules/mod_04_extractions.R")
source("R/modules/mod_05_mic_qpcr.R")       # The rebuilt module
source("R/modules/mod_06_drs_rnasep.R")     # DRS volume vs RNAseP analysis

# ELISA modules (rebuilt architecture)
source("R/modules/mod_elisa_coordinator.R")
source("R/modules/mod_elisa_runs.R")
source("R/modules/mod_elisa_samples.R")
source("R/modules/mod_elisa_analysis.R")
source("R/modules/mod_06_elisa_pe.R")
source("R/modules/mod_07_elisa_vsg.R")

# ELISA Concordance module
source("R/utils_elisa_concordance.R")
source("R/modules/mod_elisa_concordance_summary.R")
source("R/modules/mod_elisa_concordance_table.R")
source("R/modules/mod_elisa_concordance_analysis.R")
source("R/modules/mod_08_elisa_concordance.R")

# iELISA modules (inhibition ELISA for LiTat 1.3 and 1.5)
source("R/utils_ielisa.R")
source("R/modules/mod_ielisa_coordinator.R")
source("R/modules/mod_ielisa_runs.R")
source("R/modules/mod_ielisa_samples.R")
source("R/modules/mod_ielisa_analysis.R")
source("R/modules/mod_09_ielisa.R")

# Sample Journey module (individual sample tracking)
source("R/sampleJourneyHelpers.R")
source("R/sampleJourneyVisualizations.R")
source("R/modules/mod_10_sample_journey.R")

# Sample Processing module (comprehensive sample processing overview)
source("R/modules/mod_sample_processing.R")

# Concordance Analysis module (comprehensive statistical analysis)
source("R/concordanceStatistics.R")
source("R/concordanceVisualizations.R")
source("R/concordancePredictive.R")
source("R/modules/mod_11_concordance.R")

# Geographic visualization module
source("R/modules/mod_12_geographic.R")
cat("✓ Geographic module loaded\n")

# ============================================================================
# SOURCE MIC SUB-MODULES
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
# UI THEME — SIMPLE & CLEAN
# ============================================================================

app_theme <- bslib::bs_theme(
  version      = 5,
  bootswatch   = "flatly",
  primary      = "#4F46E5",
  success      = "#10B981",
  danger       = "#EF4444",
  warning      = "#F59E0B",
  info         = "#06B6D4",
  base_font    = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Inter")
)

# Optional: small amount of extra polish, no layout hacking
app_theme <- bslib::bs_add_rules(
  app_theme,
  "
  body {
    background-color: #f5f5f5;
  }

  .card {
    border-radius: 8px;
  }

  .bslib-value-box {
    border-radius: 10px;
  }

  .table {
    font-size: 14px;
  }

  .bslib-sidebar-layout {
    min-height: 100vh;
  }

  .bslib-sidebar-layout > .bslib-sidebar {
    overflow-y: auto;
    max-height: 100vh;
  }

  .bslib-sidebar-layout > .bslib-main {
    overflow: visible !important;
  }

  /* Let the navbar fill the viewport but keep each nav panel independently scrollable */
  .navs-page-container {
    min-height: 100vh;
    display: flex;
    flex-direction: column;
  }

  .navs-page-container > .bslib-nav-content {
    flex: 1 1 auto;
    min-height: 0;
    height: calc(100vh - 64px);
    overflow-y: auto;
    padding-bottom: 1rem;
  }

  @media (max-width: 991.98px) {
    .navs-page-container > .bslib-nav-content {
      height: auto;
    }
  }

  .mic-overview-panel {
    max-height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 0.5rem;
    padding-bottom: 1rem;
  }

  .mic-overview-panel::-webkit-scrollbar {
    width: 8px;
  }

  .mic-overview-panel::-webkit-scrollbar-thumb {
    background-color: rgba(79, 70, 229, 0.4);
    border-radius: 4px;
  }

  .mic-overview-panel::-webkit-scrollbar-track {
    background: transparent;
  }

  @media (max-width: 991.98px) {
    .mic-overview-panel {
      max-height: none;
      padding-right: 0;
    }
  }

  /* ELISA Module Styling - Enhanced figure visibility with scrolling */
  .elisa-panel {
    max-height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 0.5rem;
    padding-bottom: 1rem;
  }

  .elisa-panel::-webkit-scrollbar {
    width: 8px;
  }

  .elisa-panel::-webkit-scrollbar-thumb {
    background-color: rgba(79, 70, 229, 0.4);
    border-radius: 4px;
  }

  .elisa-panel::-webkit-scrollbar-track {
    background: transparent;
  }

  @media (max-width: 991.98px) {
    .elisa-panel {
      max-height: none;
      padding-right: 0;
    }
  }

  /* Ensure ELISA cards have proper spacing and visibility */
  [id*='elisa'] .card {
    margin-bottom: 1rem;
  }

  [id*='elisa'] .plotly {
    width: 100% !important;
  }

  /* iELISA Module Styling - Enhanced figure visibility with scrolling */
  .ielisa-panel {
    max-height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 0.5rem;
    padding-bottom: 1rem;
  }

  .ielisa-panel::-webkit-scrollbar {
    width: 8px;
  }

  .ielisa-panel::-webkit-scrollbar-thumb {
    background-color: rgba(79, 70, 229, 0.4);
    border-radius: 4px;
  }

  .ielisa-panel::-webkit-scrollbar-track {
    background: transparent;
  }

  @media (max-width: 991.98px) {
    .ielisa-panel {
      max-height: none;
      padding-right: 0;
    }
  }

  /* Ensure iELISA cards have proper spacing and visibility */
  [id*='ielisa'] .card {
    margin-bottom: 1rem;
  }

  [id*='ielisa'] .plotly {
    width: 100% !important;
  }

  /* Geographic Module Styling - Interactive map with scrolling */
  .geo-panel {
    max-height: calc(100vh - 160px);
    overflow-y: auto;
    padding-right: 0.5rem;
    padding-bottom: 1rem;
  }

  .geo-panel::-webkit-scrollbar {
    width: 8px;
  }

  .geo-panel::-webkit-scrollbar-thumb {
    background-color: rgba(79, 70, 229, 0.4);
    border-radius: 4px;
  }

  .geo-panel::-webkit-scrollbar-track {
    background: transparent;
  }

  @media (max-width: 991.98px) {
    .geo-panel {
      max-height: none;
      padding-right: 0;
    }
  }

  /* Leaflet map styling */
  .leaflet-container {
    border-radius: 8px;
  }

  [id*='geographic'] .card {
    margin-bottom: 1rem;
  }

  [id*='geographic'] .plotly {
    width: 100% !important;
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
      fontSize = '14px',
      fontFamily = "'Inter', 'SF Pro Text', system-ui, sans-serif"
    )
  
  return(dt)
}

# ============================================================================
# HELPER FUNCTIONS (if not in other files)
# ============================================================================

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
  "analyze_qpcr",             # From qpcr_analysis.R or mic_qpcr_pipeline.R
  "extract_cq_values",        # From qpcr_analysis.R or mic_qpcr_pipeline.R
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
# Create output directory
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# Create site-specific directories for all configured sites
for (site_id in names(config$sites)) {
  site_paths <- get_site_paths(site_id)
  for (path in site_paths) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }
}

# ============================================================================
# STARTUP MESSAGE
# ============================================================================

message("╔═══════════════════════════════════════════════════════════════╗")
message("║  Multi-Site Biobank Dashboard - Global Configuration         ║")
message("╚═══════════════════════════════════════════════════════════════╝")
message("")
message(sprintf("🏥 Current site:         %s (%s)",
                config$sites[[config$current_site]]$name,
                config$sites[[config$current_site]]$short_name))
message(sprintf("📍 Location:             %s", config$sites[[config$current_site]]$location))
message(sprintf("🏛️  Institution:          %s", config$sites[[config$current_site]]$institution))
message("")
message(sprintf("📁 Site data folder:     data/%s/", config$sites[[config$current_site]]$folder))
message(sprintf("   - Biobank:            %s", config$site_paths$biobank_dir))
message(sprintf("   - Extractions:        %s", config$site_paths$extractions_dir))
message(sprintf("   - MIC qPCR:           %s", config$site_paths$mic_dir))
message(sprintf("   - ELISA PE:           %s", config$site_paths$elisa_pe_dir))
message(sprintf("   - ELISA VSG:          %s", config$site_paths$elisa_vsg_dir))
message(sprintf("   - iELISA:             %s", config$site_paths$ielisa_dir))
message(sprintf("   - Cache:              %s", config$site_paths$cache_dir))
message("")
message(sprintf("🌍 Configured sites:     %s", paste(names(config$sites), collapse = ", ")))
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
