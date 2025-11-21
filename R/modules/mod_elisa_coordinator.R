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
    # Add controls above the tabs
    card(
      card_body(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex flex-column justify-content-end",
            checkboxInput(
              ns("exclude_invalid_plates"),
              "Exclude invalid plates",
              value = TRUE
            ),
            tags$small(
              class = "text-muted",
              "Removes plates with failed control QC from all analyses"
            )
          ),
          div(
            class = "d-flex align-items-center gap-2",
            actionButton(
              ns("settings_btn"),
              "Settings",
              icon = icon("sliders"),
              class = "btn-sm btn-outline-secondary"
            )
          )
        )
      )
    ),
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
    # THRESHOLD SETTINGS
    # ========================================================================

    # Store threshold settings
    thresholds <- reactiveValues(
      pos_control_min = 0.5,
      pos_control_max = 1.5,
      neg_control_max = 0.5,
      sample_pp_cutoff = 20,
      sample_od_cutoff = 0.3
    )

    # Show settings modal when button is clicked
    observeEvent(input$settings_btn, {
      showModal(modalDialog(
        title = "ELISA QC Threshold Settings",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("settings_reset"), "Reset to Defaults", class = "btn-secondary"),
          modalButton("Cancel"),
          actionButton(ns("settings_apply"), "Apply", class = "btn-primary")
        ),

        h5("Control Validation Thresholds"),
        p(class = "text-muted", "Set OD ranges for positive and negative controls"),

        # Positive control range
        div(
          class = "row mb-3",
          div(
            class = "col-6",
            numericInput(
              ns("pos_control_min_input"),
              "Positive Control - Min OD",
              value = thresholds$pos_control_min,
              min = 0,
              max = 2,
              step = 0.1,
              width = "100%"
            )
          ),
          div(
            class = "col-6",
            numericInput(
              ns("pos_control_max_input"),
              "Positive Control - Max OD",
              value = thresholds$pos_control_max,
              min = 0,
              max = 2,
              step = 0.1,
              width = "100%"
            )
          )
        ),

        # Negative control threshold
        numericInput(
          ns("neg_control_max_input"),
          "Negative Control - Max OD",
          value = thresholds$neg_control_max,
          min = 0,
          max = 2,
          step = 0.1,
          width = "100%"
        ),

        hr(),

        h5("Sample Positivity Thresholds"),
        p(class = "text-muted", "Set cutoffs for classifying samples as positive"),

        # Sample PP% cutoff
        numericInput(
          ns("sample_pp_cutoff_input"),
          "Percent Positivity (PP%) Cutoff",
          value = thresholds$sample_pp_cutoff,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),

        # Sample OD cutoff
        numericInput(
          ns("sample_od_cutoff_input"),
          "OD Cutoff",
          value = thresholds$sample_od_cutoff,
          min = 0,
          max = 3,
          step = 0.05,
          width = "100%"
        )
      ))
    })

    # Reset to defaults
    observeEvent(input$settings_reset, {
      updateNumericInput(session, "pos_control_min_input", value = 0.5)
      updateNumericInput(session, "pos_control_max_input", value = 1.5)
      updateNumericInput(session, "neg_control_max_input", value = 0.5)
      updateNumericInput(session, "sample_pp_cutoff_input", value = 20)
      updateNumericInput(session, "sample_od_cutoff_input", value = 0.3)
    })

    # Apply settings
    observeEvent(input$settings_apply, {
      thresholds$pos_control_min <- input$pos_control_min_input
      thresholds$pos_control_max <- input$pos_control_max_input
      thresholds$neg_control_max <- input$neg_control_max_input
      thresholds$sample_pp_cutoff <- input$sample_pp_cutoff_input
      thresholds$sample_od_cutoff <- input$sample_od_cutoff_input

      removeModal()

      showNotification(
        "Settings updated successfully. Plate validation will be recalculated.",
        type = "message",
        duration = 3
      )
    })

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

    # Filter to specific ELISA type (PE or VSG) and recalculate validation
    elisa_data_typed <- reactive({
      # Trigger recalculation when thresholds change
      pos_min <- thresholds$pos_control_min
      pos_max <- thresholds$pos_control_max
      neg_max <- thresholds$neg_control_max
      pp_cutoff <- thresholds$sample_pp_cutoff
      od_cutoff <- thresholds$sample_od_cutoff

      data <- raw_elisa_data()
      if (is.null(data) || !nrow(data)) {
        message("DEBUG: raw_elisa_data is NULL or empty")
        return(tibble())
      }

      message("DEBUG: raw_elisa_data has ", nrow(data), " rows")
      message("DEBUG: Looking for elisa_type = ", elisa_type)

      # Filter by elisa_type
      if ("elisa_type" %in% names(data)) {
        # Show unique values of elisa_type
        unique_types <- unique(data$elisa_type)
        message("DEBUG: Unique elisa_type values: ", paste(unique_types, collapse = ", "))

        filtered <- data %>% filter(elisa_type == !!elisa_type)
        message("DEBUG: After filtering for '", elisa_type, "': ", nrow(filtered), " rows")

        # Recalculate plate validation based on current threshold settings
        if (all(c("positive_control_od", "negative_control_od") %in% names(filtered))) {
          filtered <- filtered %>%
            mutate(
              plate_positive_control_valid = ifelse(
                is.na(positive_control_od),
                NA,
                positive_control_od >= pos_min & positive_control_od <= pos_max
              ),
              plate_negative_control_valid = ifelse(
                is.na(negative_control_od),
                NA,
                negative_control_od < neg_max
              ),
              plate_valid = plate_positive_control_valid & plate_negative_control_valid
            )
          message("DEBUG: Recalculated plate validation with custom thresholds")
        }

        # Add sample positivity classification based on thresholds
        if ("PP_percent" %in% names(filtered) && "DOD" %in% names(filtered)) {
          filtered <- filtered %>%
            mutate(
              sample_positive_by_pp = PP_percent >= pp_cutoff,
              sample_positive_by_od = DOD >= od_cutoff,
              # Sample is positive if either criterion is met
              sample_positive = sample_positive_by_pp | sample_positive_by_od
            )
          message("DEBUG: Added sample positivity classification")
        }

        return(filtered)
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
      if (!is.null(flt)) {
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
      }

      # Apply plate validity filter
      if (isTRUE(input$exclude_invalid_plates)) {
        if ("plate_valid" %in% names(data)) {
          # Get list of invalid plate combinations (plate_id + plate_num)
          # This ensures that in multi-plate tests (e.g., 4-plate VSG), only
          # the specific failing plate is excluded, not all plates in the test
          invalid_plate_combos <- data %>%
            filter(plate_valid == FALSE) %>%
            distinct(plate_id, plate_num)

          # Filter out rows from invalid plate combinations
          if (nrow(invalid_plate_combos) > 0) {
            message("DEBUG [mod_elisa_coordinator]: Excluding ", nrow(invalid_plate_combos), " invalid plate(s)")
            data <- data %>%
              anti_join(invalid_plate_combos, by = c("plate_id", "plate_num"))
          }
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
