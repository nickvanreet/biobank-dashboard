# TEMPLATE: global.R - Complete correct version
# ============================================================================
# This is what your global.R should contain to make mod_05_mic.R work

# ============================================================================
# CONFIGURATION & SETUP
# ============================================================================

# Install/load packages
required_packages <- c(
  "shiny", "bslib", "tidyverse", "readxl", "writexl", "janitor",
  "DT", "plotly", "lubridate", "scales", "stringr", "stringi",
  "purrr", "dplyr", "tidyr", "ggplot2"
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
    mic_dir = "data/mic"
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
# LOAD ALL CORE UTILITIES (CRITICAL FOR MOD_05_MIC)
# ============================================================================

# Data utilities
source("R/core/data_loader_utils.R")       # Must source BEFORE data_linking
source("R/core/data_linking_utils.R")      # Defines normalize_barcode()
source("R/core/extraction_data_utils.R")   # Extraction utilities
source("R/core/mic_qpcr_pipeline.R")       # CRITICAL: Defines analyze_qpcr()
source("R/data/data_cleaner_improved.R")

# ============================================================================
# LOAD ALL MODULES
# ============================================================================

source("R/modules/mod_data_manager.R")
source("R/modules/mod_01_data_quality.R")
source("R/modules/mod_02_overview_demographics.R")
source("R/modules/mod_03_transport.R")
source("R/modules/mod_04_extractions.R")
source("R/modules/mod_05_mic_qpcr.R")

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
# ENVIRONMENT VERIFICATION
# ============================================================================

# Quick check that all critical functions are available
critical_functions <- c(
  "normalize_barcode",
  "analyze_qpcr",
  "link_extraction_to_biobank",
  "apply_filters",
  "mod_data_manager_ui"
)

missing_functions <- critical_functions[!sapply(critical_functions, exists)]

if (length(missing_functions) > 0) {
  warning("⚠️  Missing functions in global environment:")
  warning(paste("  -", missing_functions, collapse="\n"))
}

# Create output directory if it doesn't exist
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

message("✅ Global configuration loaded successfully")
message(sprintf("   Config paths: biobank=%s, extractions=%s, pcr=%s, mic=%s",
                config$paths$biobank_dir,
                config$paths$extractions_dir,
                config$paths$pcr_dir,
                config$paths$mic_dir))
