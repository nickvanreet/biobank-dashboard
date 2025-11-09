# global.R - Global configuration and setup
# ============================================================================

# ============================================================================
# PACKAGE LOADING
# ============================================================================

# Core packages
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
  library(leaflet)
  library(sf)
  
  # Text processing
  library(stringi)
  
  # Statistics
  library(binom)
  
  # Performance
  library(memoise)
  library(cachem)
  
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
      ielisa_dir = "data/ielisa",
      column_mappings = "data/column_mappings.json"
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
    ),
    
    cache = list(
      enabled = TRUE,
      ttl_minutes = 10
    )
  )
  
  # Load user configuration if exists
  if (file.exists(config_path)) {
    user_config <- yaml::read_yaml(config_path)
    # Merge configurations (user config overrides defaults)
    config <- modifyList(default_config, user_config)
  } else {
    config <- default_config
    warning("Config file not found, using defaults")
  }
  
  # Normalize paths
  if (!is.null(config$paths)) {
    config$paths <- lapply(config$paths, function(p) {
      if (!is.null(p) && nzchar(p)) {
        normalizePath(p, winslash = "/", mustWork = FALSE)
      } else {
        p
      }
    })
  }
  
  config
}

# Load configuration
config <- load_app_config()

# ============================================================================
# THEME SETUP
# ============================================================================

#' Create application theme
create_app_theme <- function(theme_config = config$ui) {
  bs_theme(
    version = 5,
    preset = "bootstrap",
    primary = theme_config$theme_primary,
    success = theme_config$theme_success,
    info = theme_config$theme_info,
    warning = theme_config$theme_warning,
    danger = theme_config$theme_danger,
    base_font = font_google("Inter"),
    heading_font = font_google("Montserrat"),
    code_font = font_google("JetBrains Mono")
  )
}

app_theme <- create_app_theme()

# ============================================================================
# MODULE LOADING
# ============================================================================

#' Source all R files in a directory
source_directory <- function(path, recursive = FALSE) {
  if (!dir.exists(path)) {
    warning(sprintf("Directory not found: %s", path))
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
    tryCatch(
      source(file, local = FALSE),
      error = function(e) {
        warning(sprintf("Failed to source %s: %s", file, e$message))
      }
    )
  }
}

# Load all modules in order
source_directory("R/core/")      # Core utilities first
source_directory("R/data/")      # Data handling
source_directory("R/ui/")        # UI components
source_directory("R/modules/")   # Application modules

# ============================================================================
# GLOBAL UTILITIES
# ============================================================================

#' Null coalescing operator
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

#' Not in operator
`%nin%` <- function(x, y) !(x %in% y)

#' Safe icon wrapper with fallback
safe_icon <- function(name, ...) {
  tryCatch(
    bs_icon(name, ...),
    error = function(e) {
      # Fallback to Font Awesome
      tryCatch(
        icon(name),
        error = function(e2) {
          # Ultimate fallback
          icon("question-circle")
        }
      )
    }
  )
}

# ============================================================================
# CACHING SETUP
# ============================================================================

if (config$cache$enabled) {
  # Create cache with TTL
  app_cache <- cachem::cache_mem(
    max_size = 100 * 1024^2,  # 100 MB
    max_age = config$cache$ttl_minutes * 60
  )
} else {
  app_cache <- NULL
}

#' Create memoised function with app cache
memoise_with_cache <- function(f) {
  if (!is.null(app_cache)) {
    memoise::memoise(f, cache = app_cache)
  } else {
    f
  }
}

# ============================================================================
# ERROR HANDLING
# ============================================================================

#' Safe error handler for reactive expressions
safe_reactive <- function(expr, default = NULL) {
  reactive({
    tryCatch(
      expr,
      error = function(e) {
        showNotification(
          sprintf("Error: %s", e$message),
          type = "error",
          duration = 5
        )
        default
      }
    )
  })
}

# ============================================================================
# LOGGING SETUP (Optional)
# ============================================================================

if (config$app$debug_mode) {
  # Enable detailed error messages
  options(shiny.fullstacktrace = TRUE)
  options(shiny.reactlog = TRUE)
  
  # Create log function
  log_message <- function(msg, level = "INFO") {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    cat(sprintf("[%s] %s: %s\n", timestamp, level, msg))
  }
} else {
  # Production mode - minimal logging
  options(shiny.fullstacktrace = FALSE)
  log_message <- function(msg, level = "INFO") invisible(NULL)
}

# ============================================================================
# LOCALE SETTINGS
# ============================================================================

# Set locale for proper character encoding
if (.Platform$OS.type == "windows") {
  tryCatch(
    Sys.setlocale("LC_CTYPE", "English_United States.utf8"),
    error = function(e) {
      # Fallback to system default
      Sys.setlocale("LC_CTYPE", "")
    }
  )
}

# ============================================================================
# CONSTANTS
# ============================================================================

# Application constants
APP_CONSTANTS <- list(
  # File size limits
  MAX_UPLOAD_SIZE = 50 * 1024^2,  # 50 MB
  
  # Data table options
  DT_OPTIONS = list(
    pageLength = 25,
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ),
  
  # Plot defaults
  PLOT_HEIGHT = 400,
  PLOT_BASE_SIZE = 13,
  
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

# Set max request size for file uploads
options(shiny.maxRequestSize = APP_CONSTANTS$MAX_UPLOAD_SIZE)

# ============================================================================
# VALIDATION
# ============================================================================

# Check critical paths exist
validate_paths <- function() {
  missing_paths <- character()
  
  for (path_name in names(config$paths)) {
    path <- config$paths[[path_name]]
    if (!is.null(path) && !file.exists(path) && !dir.exists(path)) {
      missing_paths <- c(missing_paths, sprintf("%s: %s", path_name, path))
    }
  }
  
  if (length(missing_paths) > 0) {
    warning(sprintf(
      "Missing paths:\n%s", 
      paste(missing_paths, collapse = "\n")
    ))
  }
}

validate_paths()

# ============================================================================
# EXPORT GLOBAL OBJECTS
# ============================================================================

# Make key objects available globally
.GlobalEnv$config <- config
.GlobalEnv$app_theme <- app_theme
.GlobalEnv$APP_CONSTANTS <- APP_CONSTANTS

log_message(sprintf("Biobank Dashboard v%s initialized", config$app$version))
