# R/modules/mod_elisa_coordinator.R
# Generic ELISA coordinator module (handles both ELISA-PE and ELISA-VSG)
# Modeled after mod_05a_mic_coordinator.R

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
})

#' ELISA Coordinator UI (generic for PE or VSG)
#' @param id Module namespace ID
#' @param label Display label ("ELISA-PE" or "ELISA-VSG")
#' @param elisa_type ELISA type filter ("ELISA_pe" or "ELISA_vsg")
#' @export
mod_elisa_coordinator_ui <- function(id, label = "ELISA", elisa_type = "ELISA_pe") {
  ns <- NS(id)

  # Return navigation panel with three sub-tabs (like MIC)
  nav_panel(
    title = label,
    icon = icon("vial-virus"),
    navset_card_tab(
      id = ns("elisa_tabs"),
      # Tab 1: Runs (overview)
      nav_panel(
        title = paste0(label, " - Runs"),
        value = "runs",
        mod_elisa_runs_ui(ns("runs"))
      ),
      # Tab 2: Samples
      nav_panel(
        title = paste0(label, " - Samples"),
        value = "samples",
        mod_elisa_samples_ui(ns("samples"))
      ),
      # Tab 3: Analysis
      nav_panel(
        title = paste0(label, " - Analysis"),
        value = "analysis",
        mod_elisa_analysis_ui(ns("analysis"))
      )
    )
  )
}

#' ELISA Coordinator Server (generic for PE or VSG)
#' @param id Module namespace ID
#' @param elisa_type ELISA type filter ("ELISA_pe" or "ELISA_vsg")
#' @param biobank_df Reactive returning biobank data frame
#' @param filters Reactive returning global filters (optional)
#' @export
mod_elisa_coordinator_server <- function(id, elisa_type = "ELISA_pe", biobank_df = reactive(NULL), filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # DATA LOADING & CACHING
    # ========================================================================

    # Raw ELISA data (all types, cached)
    raw_elisa_data <- reactive({
      req(biobank_df())

      # Load all ELISA data with biobank linkage
      data <- tryCatch({
        get_elisa_data(biobank_df = biobank_df())
      }, error = function(e) {
        message("Error loading ELISA data: ", e$message)
        NULL
      })

      data
    })

    # Filter to specific ELISA type (PE or VSG)
    elisa_data_typed <- reactive({
      data <- raw_elisa_data()
      if (is.null(data) || !nrow(data)) return(tibble())

      # Filter by elisa_type
      if ("elisa_type" %in% names(data)) {
        data %>% filter(elisa_type == !!elisa_type)
      } else {
        message("Warning: elisa_type column not found in data")
        message("Available columns: ", paste(names(data), collapse = ", "))
        tibble()
      }
    })

    # Apply global filters (if provided)
    filtered_base <- reactive({
      data <- elisa_data_typed()
      if (!nrow(data)) return(data)

      # Apply filters if available
      flt <- filters()
      if (is.null(flt)) return(data)

      # Apply province filter
      if (!is.null(flt$province) && flt$province != "all" && flt$province != "") {
        if ("Province" %in% names(data)) {
          data <- data %>% filter(Province == !!flt$province)
        }
      }

      # Apply health zone filter
      if (!is.null(flt$zone) && flt$zone != "all" && flt$zone != "") {
        if ("HealthZone" %in% names(data)) {
          data <- data %>% filter(HealthZone == !!flt$zone)
        }
      }

      # Apply structure filter
      if (!is.null(flt$structure) && flt$structure != "all" && flt$structure != "") {
        if ("Structure" %in% names(data)) {
          data <- data %>% filter(Structure == !!flt$structure)
        }
      }

      # Apply date range filter
      if (!is.null(flt$date_range) && length(flt$date_range) == 2) {
        if ("plate_date" %in% names(data)) {
          data <- data %>%
            filter(
              as.Date(plate_date) >= flt$date_range[1],
              as.Date(plate_date) <= flt$date_range[2]
            )
        }
      }

      data
    })

    # Separate samples and controls
    samples_data <- reactive({
      data <- filtered_base()
      if (!nrow(data)) return(data)

      if ("sample_type" %in% names(data)) {
        data %>% filter(sample_type == "sample")
      } else {
        data
      }
    })

    controls_data <- reactive({
      data <- filtered_base()
      if (!nrow(data)) return(data)

      if ("sample_type" %in% names(data)) {
        data %>% filter(sample_type == "control")
      } else {
        tibble()
      }
    })

    # ========================================================================
    # CALL SUB-MODULES
    # ========================================================================

    # Runs module (plate-level overview)
    mod_elisa_runs_server(
      "runs",
      elisa_data = filtered_base
    )

    # Samples module (sample-level details with filters)
    mod_elisa_samples_server(
      "samples",
      elisa_data = samples_data
    )

    # Analysis module (QC plots and distributions)
    mod_elisa_analysis_server(
      "analysis",
      elisa_data = filtered_base,
      samples_data = samples_data,
      controls_data = controls_data
    )

    # ========================================================================
    # RETURN DATA FOR OTHER MODULES
    # ========================================================================

    return(list(
      all_data = filtered_base,
      samples = samples_data,
      controls = controls_data,
      raw_data = raw_elisa_data
    ))
  })
}
