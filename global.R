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

# ============================================================================
# UI THEME
# ============================================================================

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#3498DB",
  success = "#27AE60",
  danger = "#E74C3C",
  warning = "#F39C12",
  info = "#2980B9",
  heading_font = "'Helvetica Neue', Helvetica, Arial, sans-serif",
  base_font = "'Source Sans Pro', 'Segoe UI', system-ui, sans-serif"
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