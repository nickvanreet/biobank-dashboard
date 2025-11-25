# ==============================================================================
# SAMPLE JOURNEY MODULE - PDF EXPORT FUNCTIONS
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(plotly)
  library(rmarkdown)
  # webshot2 is optional - only loaded if available
})

#' Check whether pdflatex is available in PATH (and try to fix it for TinyTeX)
#' @return TRUE if pdflatex can be located, otherwise FALSE
pdflatex_available <- function() {
  # First try a simple PATH lookup to avoid system2 warnings when missing
  pdflatex_path <- Sys.which("pdflatex")

  if (nzchar(pdflatex_path)) {
    return(TRUE)
  }

  # If TinyTeX is installed, try adding its bin directory to PATH
  if (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex()) {
    # TinyTeX installs into different bin subdirectories depending on platform.
    # We first try the expected directory, then fall back to whatever exists.
    platform_bin <- if (.Platform$OS.type == "windows") {
      "windows"
    } else if (Sys.info()["sysname"] == "Darwin") {
      "x86_64-darwin"
    } else {
      "x86_64-linux"
    }

    default_bin <- file.path(tinytex::tinytex_root(), "bin", platform_bin)
    candidate_bins <- c(default_bin, list.dirs(file.path(tinytex::tinytex_root(), "bin"), full.names = TRUE, recursive = FALSE))
    tinytex_bin <- candidate_bins[file.exists(candidate_bins)][1]

    if (!is.na(tinytex_bin) && dir.exists(tinytex_bin)) {
      current_path <- Sys.getenv("PATH")

      if (!grepl(tinytex_bin, current_path, fixed = TRUE)) {
        Sys.setenv(PATH = paste(tinytex_bin, current_path, sep = .Platform$path.sep))
      }

      return(nzchar(Sys.which("pdflatex")))
    }
  }

  FALSE
}

#' Ensure required LaTeX packages are installed for PDF generation
#' @return TRUE if successful, FALSE otherwise
ensure_latex_packages <- function() {
  # Check if tinytex is available
  if (!requireNamespace("tinytex", quietly = TRUE)) {
    message("tinytex package not found. Installing...")
    tryCatch({
      install.packages("tinytex")
      library(tinytex)
    }, error = function(e) {
      warning("Could not install tinytex package: ", e$message)
      return(FALSE)
    })
  }

  # Check if TinyTeX is installed
  if (!tinytex::is_tinytex()) {
    message("TinyTeX not installed. Attempting to install...")
    tryCatch({
      tinytex::install_tinytex()
      message("TinyTeX installed successfully. You may need to restart R for PATH changes to take effect.")
    }, error = function(e) {
      warning("Could not install TinyTeX: ", e$message)
      warning("Please install manually: tinytex::install_tinytex()")
      warning("If TinyTeX is already installed, try restarting R or running: Sys.setenv(PATH = paste(tinytex::tinytex_root(), 'bin', Sys.info()['sysname'], sep = .Platform$file.sep, ':', Sys.getenv('PATH')))")
      return(FALSE)
    })
  }

  # Verify pdflatex is available
  if (!pdflatex_available()) {
    warning(
      paste(
        "pdflatex not found in PATH.",
        "If TinyTeX was just installed, try restarting R.",
        "If the problem persists, follow the setup steps in R/PDF_EXPORT_GUIDE.md",
        sep = "\n"
      )
    )
    return(FALSE)
  }

  # Install required LaTeX packages for kableExtra and the template
  required_latex_packages <- c(
    "booktabs",
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

  builtin_latex_packages <- c("array", "longtable")

  message("Checking LaTeX packages...")
  message(
    "Skipping built-in LaTeX packages (already included with TinyTeX): ",
    paste(builtin_latex_packages, collapse = ", ")
  )
  if (tinytex::tinytex_root() != "") {
    for (pkg in required_latex_packages) {
      tryCatch({
        tinytex::tlmgr_install(pkg)
      }, error = function(e) {
        # Ignore errors - package might already be installed
      })
    }
  }

  return(TRUE)
}

#' Render the modern Sample Journey report using Quarto
#' @param sample_id The sample ID being reported
#' @param journey_data The journey data object from gather_sample_journey
#' @param output_dir Optional directory to write the PDF into (defaults to a temp folder)
#' @return Path to the generated PDF file
#' @export
render_sample_journey_report <- function(sample_id, journey_data, output_dir = NULL) {
  if (!ensure_latex_packages()) {
    stop(paste(
      "LaTeX dependencies are not available.",
      "Install TinyTeX (tinytex::install_tinytex()) and retry.",
      sep = "\n"
    ))
  }

  if (!pdflatex_available()) {
    stop(paste(
      "pdflatex not found in system PATH.",
      "Install TinyTeX and restart R to refresh PATH, then try again.",
      sep = "\n"
    ))
  }

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' package is required to render the PDF. Install it via install.packages('quarto').")
  }

  template_path <- file.path("reports", "sample_report.qmd")
  if (!file.exists(template_path)) {
    stop("Template file not found at: ", template_path)
  }

  out_dir <- if (!is.null(output_dir)) output_dir else file.path(tempdir(), "sample_journey_report")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  output_file <- file.path(
    out_dir,
    sprintf(
      "sample_journey_%s_%s.pdf",
      gsub("[^A-Za-z0-9]", "_", sample_id),
      format(Sys.time(), "%Y%m%d_%H%M%S")
    )
  )

  logo_dir <- file.path(getwd(), "logo")
  if (!dir.exists(logo_dir)) {
    logo_dir <- "logo"
  }

  tryCatch({
    quarto::quarto_render(
      input = template_path,
      execute_params = list(
        sample_id = sample_id,
        journey_data = journey_data,
        logo_dir = logo_dir
      ),
      output_file = output_file,
      quiet = TRUE
    )

    return(output_file)
  }, error = function(e) {
    error_msg <- paste0(
      "Error generating PDF report: ", e$message,
      "\n\nEnsure Quarto CLI is installed and available in PATH. For guidance, see R/PDF_EXPORT_GUIDE.md."
    )
    stop(error_msg)
  })
}

#' Generate PDF report for sample journey (legacy entry point)
#' @inheritParams render_sample_journey_report
#' @export
generate_sample_journey_pdf <- function(sample_id, journey_data) {
  render_sample_journey_report(sample_id, journey_data)
}

#' Alternative function retained for backward compatibility
#' @inheritParams render_sample_journey_report
#' @export
generate_sample_journey_pdf_simple <- function(sample_id, journey_data) {
  render_sample_journey_report(sample_id, journey_data)
}
