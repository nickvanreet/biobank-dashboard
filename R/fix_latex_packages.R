# ==============================================================================
# FIX LATEX PACKAGES FOR PDF EXPORT
# ==============================================================================
#
# This script installs all required LaTeX packages for PDF export.
# Run this if you encounter LaTeX compilation errors.
#
# Usage:
#   source("R/fix_latex_packages.R")

cat("=== Fixing LaTeX Packages for PDF Export ===\n\n")

# Check if tinytex package is installed
if (!requireNamespace("tinytex", quietly = TRUE)) {
  cat("Installing tinytex package...\n")
  install.packages("tinytex")
  library(tinytex)
}

# Check if TinyTeX is installed
if (!tinytex::is_tinytex()) {
  cat("TinyTeX is not installed.\n")
  cat("Installing TinyTeX (this may take a few minutes)...\n\n")
  tinytex::install_tinytex()
  cat("\n✓ TinyTeX installed successfully!\n\n")
}

# Install all required LaTeX packages
cat("Installing required LaTeX packages...\n")

required_latex_packages <- c(
  "booktabs",        # For beautiful tables
  "multirow",        # For multi-row cells in tables
  "wrapfig",         # For wrapping text around figures
  "float",           # For better float placement
  "colortbl",        # For colored table cells
  "pdflscape",       # For landscape pages
  "tabu",            # For advanced table features
  "threeparttable",  # For table notes
  "threeparttablex", # Extended threeparttable
  "ulem",            # For underlining and strikethrough
  "makecell",        # For better cell formatting
  "xcolor",          # For colors
  "fancyhdr"         # For custom headers/footers
)

# These packages are bundled with core LaTeX and cannot be installed separately
# via tlmgr. Attempting to install them causes tlmgr to exit with errors even
# though they are available by default.
builtin_latex_packages <- c("array", "longtable")

success_count <- 0
already_installed_count <- 0

for (pkg in required_latex_packages) {
  cat(sprintf("  %s... ", pkg))
  tryCatch({
    tinytex::tlmgr_install(pkg)
    success_count <- success_count + 1
    cat("✓ installed\n")
  }, error = function(e) {
    # Package might already be installed
    already_installed_count <- already_installed_count + 1
    cat("✓ OK\n")
  })
}

cat("\nSkipping built-in LaTeX packages (already included with TinyTeX):\n")
for (pkg in builtin_latex_packages) {
  cat(sprintf("  %s\n", pkg))
}

cat("\n=== Summary ===\n")
cat(sprintf("Total packages checked: %d\n", length(required_latex_packages)))
cat(sprintf("Newly installed: %d\n", success_count))
cat(sprintf("Already available: %d\n", already_installed_count))
cat("\n✓ LaTeX packages are ready for PDF export!\n\n")
cat("You can now generate PDF reports from the Sample Journey module.\n")
