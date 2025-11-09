# test_structure.R
# Verification script for Biobank Dashboard structure
# ============================================================================

cat("\n🔍 Verifying Biobank Dashboard Structure\n")
cat("==========================================\n\n")

# Track status
all_ok <- TRUE

# ============================================================================
# CHECK FILES
# ============================================================================

cat("📄 Checking core files...\n")

required_files <- c(
  "app.R",
  "global.R",
  "config.yml",
  "R/core/data_loader_utils.R",
  "R/data/data_cleaner_improved.R",
  "R/ui/ui_utils.R",
  "R/modules/mod_data_manager.R",
  "R/modules/mod_01_data_quality.R"
)

for (file in required_files) {
  if (file.exists(file)) {
    cat(sprintf("  ✓ %s\n", file))
  } else {
    cat(sprintf("  ✗ %s NOT FOUND\n", file))
    all_ok <- FALSE
  }
}

# ============================================================================
# CHECK DIRECTORIES
# ============================================================================

cat("\n📁 Checking data directories...\n")

required_dirs <- c(
  "data",
  "data/biobank",
  "data/extractions",
  "data/pcr",
  "data/elisa_pe",
  "data/elisa_vsg",
  "data/ielisa",
  "R/core",
  "R/data",
  "R/ui",
  "R/modules"
)

for (dir in required_dirs) {
  if (dir.exists(dir)) {
    cat(sprintf("  ✓ %s/\n", dir))
  } else {
    cat(sprintf("  ✗ %s/ NOT FOUND\n", dir))
    all_ok <- FALSE
  }
}

# ============================================================================
# CHECK PACKAGES
# ============================================================================

cat("\n📦 Checking required packages...\n")

required_packages <- c(
  "shiny", "bslib", "bsicons",
  "tidyverse", "data.table", "janitor",
  "readxl", "jsonlite", "yaml",
  "lubridate", "plotly", "DT",
  "stringi", "scales"
)

missing_packages <- character()

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  ✓ %s\n", pkg))
  } else {
    cat(sprintf("  ✗ %s NOT INSTALLED\n", pkg))
    missing_packages <- c(missing_packages, pkg)
    all_ok <- FALSE
  }
}

# ============================================================================
# TEST SOURCING
# ============================================================================

cat("\n🔧 Testing file sourcing...\n")

tryCatch({
  source("global.R", local = TRUE)
  cat("  ✓ global.R sourced successfully\n")
}, error = function(e) {
  cat(sprintf("  ✗ Error sourcing global.R: %s\n", e$message))
  all_ok <- FALSE
})

# ============================================================================
# TEST FUNCTIONS
# ============================================================================

cat("\n⚙️  Testing core functions...\n")

test_functions <- c(
  "load_biobank_file",
  "list_biobank_files",
  "analyze_data_quality",
  "apply_filters",
  "clean_biobank_data_improved",
  "safe_icon"
)

for (func in test_functions) {
  if (exists(func)) {
    cat(sprintf("  ✓ %s() exists\n", func))
  } else {
    cat(sprintf("  ✗ %s() NOT FOUND\n", func))
    all_ok <- FALSE
  }
}

# ============================================================================
# CHECK DATA FILES
# ============================================================================

cat("\n📊 Checking for data files...\n")

biobank_files <- list.files("data/biobank", pattern = "\\.(xlsx|xls)$", full.names = TRUE)

if (length(biobank_files) > 0) {
  cat(sprintf("  ✓ Found %d Excel file(s):\n", length(biobank_files)))
  for (f in biobank_files) {
    cat(sprintf("    - %s\n", basename(f)))
  }
} else {
  cat("  ⚠ No Excel files found in data/biobank/\n")
  cat("    Place your biobank Excel files there to test with real data\n")
}

# ============================================================================
# FINAL REPORT
# ============================================================================

cat("\n")
cat("==========================================\n")

if (all_ok && length(missing_packages) == 0) {
  cat("✅ ALL CHECKS PASSED!\n")
  cat("\nYou can now run the application:\n")
  cat("  shiny::runApp()\n\n")
} else {
  cat("⚠️  SOME ISSUES FOUND\n\n")
  
  if (length(missing_packages) > 0) {
    cat("📦 Install missing packages:\n")
    cat(sprintf('  install.packages(c(%s))\n\n',
                paste(sprintf('"%s"', missing_packages), collapse = ", ")))
  }
  
  if (!all_ok) {
    cat("🔧 Fix the issues listed above before running the app\n\n")
  }
}

cat("==========================================\n\n")
