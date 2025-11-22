# ==============================================================================
# PDF EXPORT DEPENDENCIES INSTALLER
# ==============================================================================
#
# This script installs all required packages for the Sample Journey PDF export feature.
# Run this once to set up your environment.
#
# Usage:
#   source("R/install_pdf_dependencies.R")

cat("Installing PDF export dependencies...\n\n")

# List of required packages
required_packages <- c(
  "rmarkdown",      # For rendering R Markdown to PDF
  "knitr",          # For dynamic report generation
  "kableExtra",     # For beautiful tables in PDF
  "ggplot2",        # For static plot generation
  "tidyverse",      # Data manipulation (usually already installed)
  "plotly",         # Interactive plots (usually already installed)
  "tinytex"         # LaTeX distribution for PDF generation
)

# Optional packages for enhanced functionality
optional_packages <- c(
  "webshot2"        # For advanced plotly exports
)

# Function to install a package if it's not already installed
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    cat(sprintf("Installing %s...\n", package_name))
    install.packages(package_name, dependencies = TRUE)
    cat(sprintf("✓ %s installed\n\n", package_name))
  } else {
    cat(sprintf("✓ %s already installed\n", package_name))
  }
}

# Install required packages
cat("=== Installing Required Packages ===\n")
for (pkg in required_packages) {
  install_if_missing(pkg)
}

# Install TinyTeX if needed
cat("\n=== Checking TinyTeX (LaTeX distribution) ===\n")
if (requireNamespace("tinytex", quietly = TRUE)) {
  if (!tinytex::is_tinytex()) {
    cat("TinyTeX not found. Installing...\n")
    cat("This may take a few minutes...\n")
    tryCatch({
      tinytex::install_tinytex()
      cat("✓ TinyTeX installed successfully\n\n")
    }, error = function(e) {
      cat("⚠ Warning: Could not install TinyTeX automatically\n")
      cat("Please install manually:\n")
      cat("  tinytex::install_tinytex()\n\n")
    })
  } else {
    cat("✓ TinyTeX already installed\n\n")
  }

  # Install required LaTeX packages for kableExtra
  if (tinytex::is_tinytex()) {
    cat("=== Installing Required LaTeX Packages ===\n")
    latex_packages <- c(
      "booktabs",
      "longtable",
      "array",
      "multirow",
      "wrapfig",
      "float",
      "colortbl",
      "pdflscape",
      "tabu",
      "threeparttable",
      "threeparttablex",
      "ulem",
      "makecell",
      "xcolor",
      "fancyhdr"
    )

    for (pkg in latex_packages) {
      cat(sprintf("Installing LaTeX package: %s... ", pkg))
      tryCatch({
        tinytex::tlmgr_install(pkg)
        cat("✓\n")
      }, error = function(e) {
        cat("(may already be installed)\n")
      })
    }
    cat("\n")
  }
} else {
  cat("⚠ Warning: tinytex package not found\n")
  cat("Please install: install.packages('tinytex')\n\n")
}

# Install optional packages
cat("\n=== Installing Optional Packages ===\n")
for (pkg in optional_packages) {
  tryCatch({
    install_if_missing(pkg)
  }, error = function(e) {
    cat(sprintf("⚠ Warning: Could not install %s (not critical)\n", pkg))
  })
}

# Verify installation
cat("\n=== Verifying Installation ===\n")
all_good <- TRUE

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("✓ %s: OK\n", pkg))
  } else {
    cat(sprintf("✗ %s: NOT FOUND\n", pkg))
    all_good <- FALSE
  }
}

# Final message
cat("\n")
if (all_good) {
  cat("========================================\n")
  cat("✓ All dependencies installed successfully!\n")
  cat("✓ PDF export is ready to use.\n")
  cat("========================================\n\n")
  cat("You can now use the Sample Journey PDF export feature.\n")
  cat("See R/PDF_EXPORT_GUIDE.md for usage instructions.\n")
} else {
  cat("========================================\n")
  cat("⚠ Some packages failed to install.\n")
  cat("========================================\n\n")
  cat("Please install missing packages manually:\n")
  cat("install.packages(c('rmarkdown', 'knitr', 'kableExtra', 'ggplot2'))\n")
  cat("tinytex::install_tinytex()\n")
}
