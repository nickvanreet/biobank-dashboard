# ==============================================================================
# SAMPLE JOURNEY MODULE - PDF EXPORT FUNCTIONS
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(plotly)
  library(rmarkdown)
  # webshot2 is optional - only loaded if available
})

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
    }, error = function(e) {
      warning("Could not install TinyTeX: ", e$message)
      warning("Please install manually: tinytex::install_tinytex()")
      return(FALSE)
    })
  }

  # Install required LaTeX packages for kableExtra and the template
  required_latex_packages <- c(
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

  message("Checking LaTeX packages...")
  for (pkg in required_latex_packages) {
    if (!tinytex::tinytex_root() == "") {
      tryCatch({
        tinytex::tlmgr_install(pkg)
      }, error = function(e) {
        # Ignore errors - package might already be installed
      })
    }
  }

  return(TRUE)
}

#' Generate PDF report for sample journey
#' @param sample_id The sample ID being reported
#' @param journey_data The journey data object from gather_sample_journey
#' @return Path to the generated PDF file
#' @export
generate_sample_journey_pdf <- function(sample_id, journey_data) {
  # Ensure LaTeX packages are installed
  ensure_latex_packages()

  # Create temporary directory for plots
  temp_dir <- tempdir()

  # Generate timeline plot as static image
  timeline_plot_path <- NULL
  if (!is.null(journey_data$timeline) && nrow(journey_data$timeline) > 0) {
    timeline_plot_path <- file.path(temp_dir, "timeline_plot.png")

    tryCatch({
      # Create the plotly timeline
      p <- plot_sample_timeline(journey_data$timeline)

      # Save as static image using plotly's export
      # Note: This requires the plotly and webshot2 packages
      # If webshot2 is not available, this will use kaleido
      if (requireNamespace("plotly", quietly = TRUE)) {
        plotly::save_image(p, timeline_plot_path, width = 1200, height = 600)
      }
    }, error = function(e) {
      message("Warning: Could not save timeline plot: ", e$message)
      timeline_plot_path <<- NULL
    })
  }

  # Generate DRS gauge plot if available
  drs_gauge_path <- NULL
  if (nrow(journey_data$extraction_data) > 0) {
    extraction <- journey_data$extraction_data[nrow(journey_data$extraction_data), ]

    if ("drs_volume_ml" %in% names(extraction) && !is.na(extraction$drs_volume_ml)) {
      drs_gauge_path <- file.path(temp_dir, "drs_gauge.png")

      tryCatch({
        # Create the DRS gauge plot
        p <- plot_drs_gauge(extraction$drs_volume_ml)

        # Save as static image
        if (requireNamespace("plotly", quietly = TRUE)) {
          plotly::save_image(p, drs_gauge_path, width = 600, height = 400)
        }
      }, error = function(e) {
        message("Warning: Could not save DRS gauge plot: ", e$message)
        drs_gauge_path <<- NULL
      })
    }
  }

  # Find the R Markdown template
  template_path <- system.file("R", "sample_journey_report_template.Rmd", package = ".")

  # If not found in package, look in the R directory
  if (!file.exists(template_path) || template_path == "") {
    template_path <- "R/sample_journey_report_template.Rmd"
  }

  # Generate output file path
  output_file <- file.path(temp_dir, sprintf("sample_journey_%s_%s.pdf",
                                              gsub("[^A-Za-z0-9]", "_", sample_id),
                                              format(Sys.time(), "%Y%m%d_%H%M%S")))

  # Render the R Markdown document
  tryCatch({
    # Set options for better error reporting
    old_opts <- options(tinytex.verbose = TRUE)
    on.exit(options(old_opts), add = TRUE)

    rmarkdown::render(
      input = template_path,
      output_file = basename(output_file),
      output_dir = temp_dir,
      params = list(
        sample_id = sample_id,
        journey_data = journey_data,
        timeline_plot_path = timeline_plot_path,
        drs_gauge_path = drs_gauge_path
      ),
      envir = new.env(),
      quiet = FALSE  # Changed to FALSE for better debugging
    )

    return(output_file)
  }, error = function(e) {
    # Provide more helpful error message
    error_msg <- paste0(
      "Error generating PDF report: ", e$message,
      "\n\nTroubleshooting steps:",
      "\n1. Ensure TinyTeX is installed: tinytex::install_tinytex()",
      "\n2. Install required LaTeX packages: source('R/install_pdf_dependencies.R')",
      "\n3. Check the LaTeX log file for details",
      "\n\nFor more help, see: https://yihui.org/tinytex/r/#debugging"
    )
    stop(error_msg)
  })
}

#' Alternative function to generate PDF without plotly static exports
#' Uses ggplot2 instead for better compatibility
#' @param sample_id The sample ID being reported
#' @param journey_data The journey data object from gather_sample_journey
#' @return Path to the generated PDF file
#' @export
generate_sample_journey_pdf_simple <- function(sample_id, journey_data) {
  # Ensure LaTeX packages are installed
  ensure_latex_packages()

  # Create temporary directory for plots
  temp_dir <- tempdir()

  # Generate timeline plot as static image using ggplot2
  timeline_plot_path <- NULL
  if (!is.null(journey_data$timeline) && nrow(journey_data$timeline) > 0) {
    timeline_plot_path <- file.path(temp_dir, "timeline_plot.png")

    tryCatch({
      # Create a ggplot2 version of the timeline
      timeline_data <- journey_data$timeline %>%
        mutate(
          color = case_when(
            category == "Biobank" ~ "#4F46E5",
            category == "Extraction" ~ "#10B981",
            category == "MIC" ~ "#F59E0B",
            category == "ELISA" ~ "#06B6D4",
            category == "iELISA" ~ "#8B5CF6",
            TRUE ~ "#999999"
          )
        )

      p <- ggplot(timeline_data, aes(x = date, y = event_id, color = category)) +
        geom_point(size = 4) +
        scale_y_reverse(breaks = timeline_data$event_id,
                       labels = timeline_data$event) +
        scale_color_manual(values = c(
          "Biobank" = "#4F46E5",
          "Extraction" = "#10B981",
          "MIC" = "#F59E0B",
          "ELISA" = "#06B6D4",
          "iELISA" = "#8B5CF6"
        )) +
        labs(
          title = "Sample Journey Timeline",
          x = "Date",
          y = "",
          color = "Category"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          axis.text.y = element_text(hjust = 1),
          panel.grid.major.x = element_line(color = "gray90"),
          panel.grid.major.y = element_line(color = "gray90")
        )

      ggsave(timeline_plot_path, plot = p, width = 10, height = 6, dpi = 150)
    }, error = function(e) {
      message("Warning: Could not save timeline plot: ", e$message)
      timeline_plot_path <<- NULL
    })
  }

  # Generate DRS gauge plot as a simple bar chart
  drs_gauge_path <- NULL
  if (nrow(journey_data$extraction_data) > 0) {
    extraction <- journey_data$extraction_data[nrow(journey_data$extraction_data), ]

    if ("drs_volume_ml" %in% names(extraction) && !is.na(extraction$drs_volume_ml)) {
      drs_gauge_path <- file.path(temp_dir, "drs_gauge.png")

      tryCatch({
        volume_ul <- extraction$drs_volume_ml * 1000

        # Determine color based on thresholds
        gauge_color <- if (volume_ul >= 30) {
          "#4daf4a"  # Green for good
        } else if (volume_ul >= 20) {
          "#ff7f00"  # Orange for warning
        } else {
          "#e41a1c"  # Red for critical
        }

        # Create a simple bar chart representation
        df <- data.frame(
          x = c("DRS Volume"),
          value = volume_ul,
          threshold = 30
        )

        p <- ggplot(df, aes(x = x, y = value)) +
          geom_bar(stat = "identity", fill = gauge_color, width = 0.5) +
          geom_hline(yintercept = 30, linetype = "dashed", color = "black", size = 1) +
          geom_text(aes(label = sprintf("%.0f µL", value)), vjust = -0.5, size = 6) +
          annotate("text", x = 1, y = 30, label = "Threshold: 30 µL",
                   vjust = -0.5, hjust = 0.5, size = 4) +
          ylim(0, max(200, volume_ul * 1.2)) +
          labs(
            title = "DRS Volume",
            x = "",
            y = "Volume (µL)"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            panel.grid.major.x = element_blank()
          )

        ggsave(drs_gauge_path, plot = p, width = 6, height = 4, dpi = 150)
      }, error = function(e) {
        message("Warning: Could not save DRS gauge plot: ", e$message)
        drs_gauge_path <<- NULL
      })
    }
  }

  # Find the R Markdown template
  template_path <- "R/sample_journey_report_template.Rmd"

  if (!file.exists(template_path)) {
    stop("Template file not found at: ", template_path)
  }

  # Generate output file path
  output_file <- file.path(temp_dir, sprintf("sample_journey_%s_%s.pdf",
                                              gsub("[^A-Za-z0-9]", "_", sample_id),
                                              format(Sys.time(), "%Y%m%d_%H%M%S")))

  # Render the R Markdown document
  tryCatch({
    # Set options for better error reporting
    old_opts <- options(tinytex.verbose = TRUE)
    on.exit(options(old_opts), add = TRUE)

    rmarkdown::render(
      input = template_path,
      output_file = basename(output_file),
      output_dir = temp_dir,
      params = list(
        sample_id = sample_id,
        journey_data = journey_data,
        timeline_plot_path = timeline_plot_path,
        drs_gauge_path = drs_gauge_path
      ),
      envir = new.env(),
      quiet = FALSE  # Changed to FALSE for better debugging
    )

    return(output_file)
  }, error = function(e) {
    # Provide more helpful error message
    error_msg <- paste0(
      "Error generating PDF report: ", e$message,
      "\n\nTroubleshooting steps:",
      "\n1. Ensure TinyTeX is installed: tinytex::install_tinytex()",
      "\n2. Install required LaTeX packages: source('R/install_pdf_dependencies.R')",
      "\n3. Check the LaTeX log file for details",
      "\n\nFor more help, see: https://yihui.org/tinytex/r/#debugging"
    )
    stop(error_msg)
  })
}
