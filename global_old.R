# global.R - Global configuration and setup
# Mbuji-Mayi Biobank Dashboard v3.0
# ============================================================================

message("Loading Biobank Dashboard...")

# ============================================================================
# PACKAGE LOADING
# ============================================================================

suppressPackageStartupMessages({
  # Shiny ecosystem
  library(shiny)
  library(bslib)
  library(bsicons)
  
  # Data manipulation
  library(tidyverse)
  library(data.table)
  library(janitor)
  
  # File handling
  library(readxl)
  library(jsonlite)
  library(yaml)
  
  # Date/time
  library(lubridate)
  
  # Visualization
  library(plotly)
  library(DT)
  
  # Text processing
  library(stringi)
  
  # Utilities
  library(scales)
})

# ============================================================================
# CONFIGURATION
# ============================================================================

#' Load application configuration
load_app_config <- function(config_path = "config.yml") {
  
  # Default configuration
  default_config <- list(
    app = list(
      title = "Mbuji-Mayi Biobank Dashboard",
      version = "3.0.0",
      institution = "Institute of Tropical Medicine, Antwerp",
      debug_mode = FALSE
    ),
    
    paths = list(
      biobank_dir = "data/biobank",
      extractions_dir = "data/extractions",
      pcr_dir = "data/pcr",
      elisa_pe_dir = "data/elisa_pe",
      elisa_vsg_dir = "data/elisa_vsg",
      ielisa_dir = "data/ielisa"
    ),
    
    ui = list(
      theme_primary = "#2C3E50",
      theme_success = "#27AE60",
      theme_info = "#3498DB",
      theme_warning = "#F39C12",
      theme_danger = "#E74C3C",
      sidebar_width = 280,
      default_date_range_days = 180
    ),
    
    qc = list(
      max_transport_days = 30,
      max_conservation_days = 365,
      min_age = 0,
      max_age = 120
    )
  )
  
  # Load user configuration if exists
  if (file.exists(config_path)) {
    tryCatch({
      user_config <- yaml::read_yaml(config_path)
      config <- modifyList(default_config, user_config)
      message("Loaded config from ", config_path)
    }, error = function(e) {
      warning("Failed to load config: ", e$message)
      config <- default_config
    })
  } else {
    message("No config.yml found, using defaults")
    config <- default_config
  }
  
  config
}

# Load configuration
config <- load_app_config()

# ============================================================================
# THEME SETUP
# ============================================================================

app_theme <- bs_theme(
  version = 5,
  preset = "bootstrap",
  primary = config$ui$theme_primary,
  success = config$ui$theme_success,
  info = config$ui$theme_info,
  warning = config$ui$theme_warning,
  danger = config$ui$theme_danger,
  base_font = font_google("Inter"),
  heading_font = font_google("Montserrat")
)

# ============================================================================
# SOURCE APPLICATION FILES
# ============================================================================

#' Source all R files in a directory
source_directory <- function(path, recursive = FALSE) {
  if (!dir.exists(path)) {
    message("Directory not found: ", path)
    return(invisible(NULL))
  }
  
  files <- list.files(
    path, 
    pattern = "\\.R$", 
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  
  for (file in files) {
    tryCatch({
      source(file, local = FALSE)
      message("  ✓ ", basename(file))
    }, error = function(e) {
      warning(sprintf("Failed to source %s: %s", basename(file), e$message))
    })
  }
}

# Load application components in order
message("Loading application components:")
source_directory("R/ui/")        # UI utilities
source_directory("R/core/")      # Core utilities
source_directory("R/data/")      # Data handling
source_directory("R/modules/")   # Application modules

# ============================================================================
# GLOBAL UTILITIES
# ============================================================================

#' Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Not in operator
`%nin%` <- function(x, y) !(x %in% y)

# ============================================================================
# CONSTANTS
# ============================================================================

APP_CONSTANTS <- list(
  # File size limits
  MAX_UPLOAD_SIZE = 50 * 1024^2,  # 50 MB
  
  # Data table options
  DT_OPTIONS = list(
    pageLength = 25,
    scrollX = TRUE,
    dom = 'Bfrtip'
  ),
  
  # Plot defaults
  PLOT_HEIGHT = 400,
  
  # Color schemes
  COLORS = list(
    study = c(DA = "#3498DB", DP = "#27AE60"),
    sex = c(M = "#3498DB", F = "#E91E63"),
    quality = c(
      good = "#27AE60",
      warning = "#F39C12", 
      error = "#E74C3C"
    )
  )
)

# ============================================================================
# SESSION OPTIONS
# ============================================================================

options(shiny.maxRequestSize = APP_CONSTANTS$MAX_UPLOAD_SIZE)

if (config$app$debug_mode) {
  options(shiny.fullstacktrace = TRUE)
  options(shiny.reactlog = TRUE)
}

# ============================================================================
# VALIDATION
# ============================================================================

# Create data directories if they don't exist
for (path_name in names(config$paths)) {
  path <- config$paths[[path_name]]
  if (!is.null(path) && !dir.exists(path)) {
    message("Creating directory: ", path)
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

message(sprintf("\n✓ %s v%s initialized\n", config$app$title, config$app$version))
