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
  primary = "#4F46E5",   # Elegant indigo for primary actions
  success = "#10B981",   # Fresh emerald for success states
  danger = "#EF4444",    # Vibrant red for alerts and errors
  warning = "#F59E0B",   # Warm amber for warning states
  info = "#06B6D4",      # Cyan for informational states
  secondary = "#64748B", # Slate for secondary elements
  heading_font = "'Inter', 'SF Pro Display', -apple-system, BlinkMacSystemFont, sans-serif",
  base_font = "'Inter', 'SF Pro Text', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif"
)

# Add custom CSS for elegant, modern UI
app_theme <- bslib::bs_add_rules(
  app_theme,
  "
  /* ========================================== */
  /* ELEGANT MODERN UI - Core Foundations      */
  /* ========================================== */

  /* Smooth scrolling and refined animations */
  html {
    scroll-behavior: smooth;
  }

  body {
    background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
    color: #0f172a;
    font-size: 15px;
    line-height: 1.6;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
  }

  /* Main content area */
  .container-fluid {
    overflow-y: auto;
    max-height: 100vh;
    padding: 2rem 1.5rem 3rem;
  }

  /* ========================================== */
  /* ELEGANT CARDS - Modern depth & shadows    */
  /* ========================================== */

  .card {
    border-radius: 16px;
    box-shadow:
      0 1px 3px rgba(15, 23, 42, 0.08),
      0 10px 30px rgba(15, 23, 42, 0.06);
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    background: rgba(255, 255, 255, 0.98);
    backdrop-filter: blur(10px);
    border: 1px solid rgba(226, 232, 240, 0.8);
    overflow: hidden;
  }

  .card:hover {
    box-shadow:
      0 4px 6px rgba(15, 23, 42, 0.1),
      0 20px 40px rgba(15, 23, 42, 0.1);
    transform: translateY(-2px);
    border-color: rgba(79, 70, 229, 0.2);
  }

  /* ========================================== */
  /* VALUE BOXES - Sophisticated KPI Cards     */
  /* ========================================== */

  .bslib-value-box {
    border-radius: 16px;
    transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
    box-shadow:
      0 2px 8px rgba(15, 23, 42, 0.06),
      0 12px 24px rgba(15, 23, 42, 0.04);
    background: linear-gradient(135deg,
      rgba(255, 255, 255, 0.98) 0%,
      rgba(248, 250, 252, 0.98) 100%);
    backdrop-filter: blur(10px);
    border: 1px solid rgba(226, 232, 240, 0.6);
    position: relative;
    overflow: hidden;
  }

  /* Elegant hover effect */
  .bslib-value-box:hover {
    transform: translateY(-6px) scale(1.02);
    box-shadow:
      0 6px 16px rgba(15, 23, 42, 0.1),
      0 24px 48px rgba(15, 23, 42, 0.08);
    border-color: rgba(79, 70, 229, 0.3);
  }

  /* Subtle gradient overlay on hover */
  .bslib-value-box::before {
    content: '';
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    height: 4px;
    background: linear-gradient(90deg,
      rgba(79, 70, 229, 0) 0%,
      rgba(79, 70, 229, 0.6) 50%,
      rgba(79, 70, 229, 0) 100%);
    opacity: 0;
    transition: opacity 0.3s ease;
  }

  .bslib-value-box:hover::before {
    opacity: 1;
  }

  /* Refined typography */
  .bslib-value-box .value-box-title {
    color: #64748b;
    font-weight: 600;
    font-size: 0.875rem;
    letter-spacing: 0.03em;
    text-transform: uppercase;
    margin-bottom: 0.5rem;
  }

  .bslib-value-box .value-box-value {
    color: #0f172a;
    font-weight: 700;
    font-size: 2rem;
    letter-spacing: -0.02em;
  }

  /* Full color KPI backgrounds */
  .bslib-value-box[data-theme='primary'],
  .bslib-value-box.bg-primary {
    background: linear-gradient(135deg, #4F46E5 0%, #6366F1 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='primary'] .value-box-title,
  .bslib-value-box.bg-primary .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='primary'] .value-box-value,
  .bslib-value-box.bg-primary .value-box-value {
    color: white;
  }

  .bslib-value-box[data-theme='success'],
  .bslib-value-box.bg-success {
    background: linear-gradient(135deg, #10B981 0%, #34D399 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='success'] .value-box-title,
  .bslib-value-box.bg-success .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='success'] .value-box-value,
  .bslib-value-box.bg-success .value-box-value {
    color: white;
  }

  .bslib-value-box[data-theme='danger'],
  .bslib-value-box.bg-danger {
    background: linear-gradient(135deg, #EF4444 0%, #F87171 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='danger'] .value-box-title,
  .bslib-value-box.bg-danger .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='danger'] .value-box-value,
  .bslib-value-box.bg-danger .value-box-value {
    color: white;
  }

  .bslib-value-box[data-theme='warning'],
  .bslib-value-box.bg-warning {
    background: linear-gradient(135deg, #F59E0B 0%, #FBBF24 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='warning'] .value-box-title,
  .bslib-value-box.bg-warning .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='warning'] .value-box-value,
  .bslib-value-box.bg-warning .value-box-value {
    color: white;
  }

  .bslib-value-box[data-theme='info'],
  .bslib-value-box.bg-info {
    background: linear-gradient(135deg, #06B6D4 0%, #22D3EE 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='info'] .value-box-title,
  .bslib-value-box.bg-info .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='info'] .value-box-value,
  .bslib-value-box.bg-info .value-box-value {
    color: white;
  }

  .bslib-value-box[data-theme='secondary'],
  .bslib-value-box.bg-secondary {
    background: linear-gradient(135deg, #64748B 0%, #94A3B8 100%);
    color: white;
    border: none;
  }

  .bslib-value-box[data-theme='secondary'] .value-box-title,
  .bslib-value-box.bg-secondary .value-box-title {
    color: rgba(255, 255, 255, 0.9);
  }

  .bslib-value-box[data-theme='secondary'] .value-box-value,
  .bslib-value-box.bg-secondary .value-box-value {
    color: white;
  }

  /* Icon styling for value boxes */
  .bslib-value-box .value-box-showcase {
    color: rgba(255, 255, 255, 0.95);
    font-size: 2.5rem;
  }

  .bslib-value-box .bslib-value-box-icon {
    color: rgba(255, 255, 255, 0.95);
  }

  /* ========================================== */
  /* NAVIGATION - Polished & Modern            */
  /* ========================================== */

  .navbar {
    background: rgba(255, 255, 255, 0.98) !important;
    backdrop-filter: blur(20px) saturate(180%);
    box-shadow: 0 1px 0 rgba(15, 23, 42, 0.08),
                0 4px 16px rgba(15, 23, 42, 0.04);
    border-bottom: 1px solid rgba(226, 232, 240, 0.6);
    padding: 0.75rem 1.5rem;
  }

  /* Account for fixed navbar */
  body.bslib-page-navbar {
    padding-top: 72px;
  }

  .navbar-brand {
    margin-right: 2rem;
  }

  .nav-link {
    color: #475569 !important;
    font-weight: 500;
    font-size: 0.9375rem;
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    border-radius: 10px;
    margin: 0 2px;
    padding: 0.625rem 1.125rem !important;
    position: relative;
  }

  .nav-link::after {
    content: '';
    position: absolute;
    bottom: 0;
    left: 50%;
    transform: translateX(-50%) scaleX(0);
    width: 80%;
    height: 2px;
    background: linear-gradient(90deg, #4F46E5, #6366F1);
    border-radius: 2px 2px 0 0;
    transition: transform 0.3s ease;
  }

  .nav-link:hover {
    color: #4F46E5 !important;
    background-color: rgba(79, 70, 229, 0.06);
  }

  .nav-link.active {
    color: #4F46E5 !important;
    background-color: rgba(79, 70, 229, 0.1);
    font-weight: 600;
  }

  .nav-link.active::after {
    transform: translateX(-50%) scaleX(1);
  }

  /* ========================================== */
  /* TABLES - Clean & Professional             */
  /* ========================================== */

  .dataTables_wrapper {
    font-family: 'Inter', 'SF Pro Text', system-ui, sans-serif;
    color: #1e293b;
  }

  .table {
    font-size: 14px;
    color: #0f172a;
    background-color: #ffffff;
  }

  .table thead th {
    background: linear-gradient(180deg, #f8fafc 0%, #f1f5f9 100%);
    font-weight: 600;
    font-size: 0.8125rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    border-bottom: 2px solid #cbd5e1;
    color: #475569;
    padding: 1rem 0.75rem;
  }

  .table-striped tbody tr:nth-of-type(odd) {
    background-color: rgba(248, 250, 252, 0.6);
  }

  .table-hover tbody tr {
    transition: all 0.2s ease;
  }

  .table-hover tbody tr:hover {
    background-color: rgba(79, 70, 229, 0.06);
    transform: scale(1.002);
    box-shadow: 0 2px 8px rgba(79, 70, 229, 0.1);
  }

  .table td {
    padding: 0.875rem 0.75rem;
    border-top: 1px solid #f1f5f9;
  }

  /* ========================================== */
  /* CHARTS - Elegant Plotly Containers        */
  /* ========================================== */

  .plotly {
    border-radius: 12px;
    padding: 1rem;
    background: #ffffff;
  }

  /* Enhanced Plotly modebar */
  .modebar {
    padding: 8px;
    border-radius: 8px;
    background: rgba(255, 255, 255, 0.9);
    backdrop-filter: blur(8px);
  }

  .modebar-btn {
    transition: all 0.2s ease;
  }

  .modebar-btn:hover {
    background-color: rgba(79, 70, 229, 0.1);
  }

  /* ========================================== */
  /* CARD HEADERS & SECTIONS                   */
  /* ========================================== */

  .card-header {
    font-weight: 600;
    font-size: 1.125rem;
    background: linear-gradient(135deg, #fafbfc 0%, #f8fafc 100%);
    border-bottom: 1px solid #e2e8f0;
    color: #0f172a;
    padding: 1.25rem 1.5rem;
    letter-spacing: -0.01em;
  }

  .card-body {
    background-color: #ffffff;
    padding: 1.5rem;
  }

  .card-footer {
    background-color: #fafbfc;
    border-top: 1px solid #f1f5f9;
    padding: 1rem 1.5rem;
  }

  /* ========================================== */
  /* BUTTONS - Modern & Inviting               */
  /* ========================================== */

  .btn {
    border-radius: 10px;
    font-weight: 600;
    padding: 0.625rem 1.25rem;
    transition: all 0.2s cubic-bezier(0.4, 0, 0.2, 1);
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.1);
    border: none;
  }

  .btn:hover {
    transform: translateY(-1px);
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
  }

  .btn:active {
    transform: translateY(0);
  }

  .btn-primary {
    background: linear-gradient(135deg, #4F46E5 0%, #6366F1 100%);
  }

  .btn-primary:hover {
    background: linear-gradient(135deg, #4338CA 0%, #4F46E5 100%);
  }

  /* ========================================== */
  /* FORM INPUTS - Clean & Accessible          */
  /* ========================================== */

  .form-control, .form-select {
    border-radius: 10px;
    border: 1.5px solid #e2e8f0;
    padding: 0.625rem 1rem;
    transition: all 0.2s ease;
    background-color: #ffffff;
  }

  .form-control:focus, .form-select:focus {
    border-color: #4F46E5;
    box-shadow: 0 0 0 3px rgba(79, 70, 229, 0.1);
    outline: none;
  }

  /* ========================================== */
  /* SIDEBAR - Elegant Panel                   */
  /* ========================================== */

  .bslib-sidebar-layout > .sidebar {
    background: rgba(255, 255, 255, 0.95);
    backdrop-filter: blur(10px);
    border-right: 1px solid rgba(226, 232, 240, 0.8);
    box-shadow: 2px 0 12px rgba(15, 23, 42, 0.04);
  }

  /* ========================================== */
  /* UTILITY CLASSES                           */
  /* ========================================== */

  /* Elegant spacing */
  .mb-elegant { margin-bottom: 2rem; }
  .mt-elegant { margin-top: 2rem; }

  /* Smooth fade-in animation */
  @keyframes fadeIn {
    from {
      opacity: 0;
      transform: translateY(10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .card, .bslib-value-box {
    animation: fadeIn 0.4s ease-out;
  }

  /* ========================================== */
  /* SCROLLBAR - Minimal & Modern              */
  /* ========================================== */

  ::-webkit-scrollbar {
    width: 10px;
    height: 10px;
  }

  ::-webkit-scrollbar-track {
    background: #f1f5f9;
  }

  ::-webkit-scrollbar-thumb {
    background: #cbd5e1;
    border-radius: 5px;
  }

  ::-webkit-scrollbar-thumb:hover {
    background: #94a3b8;
  }

  /* ========================================== */
  /* ADDITIONAL UI POLISH                      */
  /* ========================================== */

  /* Loading spinner */
  .shiny-spinner-output-container {
    position: relative;
  }

  /* Alerts and notifications */
  .alert {
    border-radius: 12px;
    border: 1px solid;
    padding: 1rem 1.25rem;
    margin-bottom: 1rem;
  }

  .alert-info {
    background-color: rgba(6, 182, 212, 0.08);
    border-color: rgba(6, 182, 212, 0.3);
    color: #0e7490;
  }

  .alert-success {
    background-color: rgba(16, 185, 129, 0.08);
    border-color: rgba(16, 185, 129, 0.3);
    color: #047857;
  }

  .alert-warning {
    background-color: rgba(245, 158, 11, 0.08);
    border-color: rgba(245, 158, 11, 0.3);
    color: #b45309;
  }

  .alert-danger {
    background-color: rgba(239, 68, 68, 0.08);
    border-color: rgba(239, 68, 68, 0.3);
    color: #b91c1c;
  }

  /* Badges */
  .badge {
    border-radius: 6px;
    padding: 0.35rem 0.65rem;
    font-weight: 600;
    font-size: 0.75rem;
    letter-spacing: 0.025em;
  }

  /* Modal dialogs */
  .modal-content {
    border-radius: 16px;
    border: none;
    box-shadow: 0 20px 60px rgba(15, 23, 42, 0.3);
  }

  .modal-header {
    border-bottom: 1px solid #e2e8f0;
    padding: 1.5rem;
    background: linear-gradient(135deg, #fafbfc 0%, #f8fafc 100%);
    border-radius: 16px 16px 0 0;
  }

  .modal-body {
    padding: 1.5rem;
  }

  .modal-footer {
    border-top: 1px solid #e2e8f0;
    padding: 1rem 1.5rem;
    background-color: #fafbfc;
    border-radius: 0 0 16px 16px;
  }

  /* Tooltips */
  .tooltip {
    font-size: 0.875rem;
  }

  .tooltip-inner {
    background-color: #0f172a;
    border-radius: 8px;
    padding: 0.5rem 0.75rem;
    box-shadow: 0 4px 12px rgba(15, 23, 42, 0.3);
  }

  /* Progress bars */
  .progress {
    height: 8px;
    border-radius: 4px;
    background-color: #e2e8f0;
    overflow: hidden;
  }

  .progress-bar {
    background: linear-gradient(90deg, #4F46E5, #6366F1);
    border-radius: 4px;
  }

  /* Tabs */
  .nav-tabs {
    border-bottom: 2px solid #e2e8f0;
  }

  .nav-tabs .nav-link {
    border: none;
    color: #64748b;
    border-bottom: 3px solid transparent;
    margin-bottom: -2px;
  }

  .nav-tabs .nav-link:hover {
    border-bottom-color: rgba(79, 70, 229, 0.3);
    background-color: rgba(79, 70, 229, 0.04);
  }

  .nav-tabs .nav-link.active {
    color: #4F46E5;
    border-bottom-color: #4F46E5;
    background-color: transparent;
  }

  /* Select2 dropdowns (if used) */
  .select2-container--default .select2-selection--single {
    border-radius: 10px;
    border: 1.5px solid #e2e8f0;
    height: 42px;
    padding: 0.5rem;
  }

  .select2-container--default .select2-selection--single:focus {
    border-color: #4F46E5;
    outline: none;
  }
  "
)
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