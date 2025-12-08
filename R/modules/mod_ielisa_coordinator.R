# R/modules/mod_ielisa_coordinator.R
# iELISA Coordinator module - Modernized with clean UI (matching PE/VSG pattern)

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
})

#' iELISA Coordinator UI
#' @param id Module namespace ID
#' @param label Display label (default: "iELISA")
#' @export
mod_ielisa_coordinator_ui <- function(id, label = "iELISA") {
  ns <- NS(id)

  # Return navigation panel with three sub-tabs
  nav_panel(
    title = label,
    icon = icon("vial-circle-check"),

    # Clean control card (like PE/VSG)
    card(
      card_body(
        class = "p-2",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            class = "d-flex flex-column justify-content-start gap-2",
            div(
              class = "d-flex align-items-center",
              checkboxInput(
                ns("exclude_invalid_plates"),
                "Exclude invalid plates",
                value = TRUE
              ),
              tags$small(
                class = "text-muted ms-2",
                "Removes plates with failed control QC"
              )
            ),
            div(
              class = "d-flex align-items-center",
              checkboxInput(
                ns("show_retests_only"),
                "Show retested samples only",
                value = FALSE
              ),
              tags$small(
                class = "text-muted ms-2",
                "Filter samples with multiple test dates"
              )
            )
          ),
          div(
            class = "d-flex align-items-center gap-3",
            # Formula selection (compact dropdown)
            div(
              class = "d-flex flex-column",
              tags$label(
                class = "form-label mb-1",
                "Inhibition Formula"
              ),
              selectInput(
                ns("formula_choice"),
                label = NULL,
                choices = c(
                  "F1: NEG-based (Spec)" = "f1",
                  "F2: NEG-POS normalized" = "f2"
                ),
                selected = "f1",
                width = "200px"
              )
            ),
            # Positivity threshold (compact)
            div(
              class = "d-flex flex-column",
              tags$label(
                class = "form-label mb-1",
                "Positivity Threshold"
              ),
              numericInput(
                ns("positivity_threshold"),
                label = NULL,
                value = 30,
                min = 0,
                max = 100,
                step = 5,
                width = "100px"
              ),
              tags$small(
                class = "text-muted",
                "% inhibition cutoff"
              )
            ),
            # QC Settings button
            actionButton(
              ns("settings_btn"),
              "QC Thresholds",
              icon = icon("sliders"),
              class = "btn-sm btn-outline-secondary",
              style = "margin-top: 8px;"
            )
          )
        )
      )
    ),
    navset_card_tab(
      id = ns("ielisa_tabs"),
      # Tab 1: Runs (overview with KPIs)
      nav_panel(
        title = paste0(label, " - Runs"),
        value = "runs",
        mod_ielisa_runs_ui(ns("runs"))
      ),
      # Tab 2: Samples
      nav_panel(
        title = paste0(label, " - Samples"),
        value = "samples",
        mod_ielisa_samples_ui(ns("samples"))
      ),
      # Tab 3: Analysis (controls, duplicates, multiple testing)
      nav_panel(
        title = paste0(label, " - Analysis"),
        value = "analysis",
        mod_ielisa_analysis_ui(ns("analysis"))
      )
    )
  )
}

#' iELISA Coordinator Server
#' @param id Module namespace ID
#' @param biobank_df Reactive returning biobank data frame (optional, for future integration)
#' @param filters Reactive returning global filters (optional, for future integration)
#' @export
mod_ielisa_coordinator_server <- function(id, biobank_df = reactive(NULL), filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========================================================================
    # THRESHOLD SETTINGS (stored in reactiveValues)
    # ========================================================================

    # Store threshold settings
    thresholds <- reactiveValues(
      neg_od_min = 1.0,
      neg_od_max = 3.1,
      pos_od_min = 0.3,
      pos_od_max = 0.7,
      ctrl_inh_min = 30,
      ctrl_inh_max = 100,
      ctrl_cv_max = 20
    )

    # Show settings modal when button is clicked
    observeEvent(input$settings_btn, {
      showModal(modalDialog(
        title = "iELISA QC Threshold Settings",
        size = "m",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("settings_reset"), "Reset to Defaults", class = "btn-secondary"),
          modalButton("Cancel"),
          actionButton(ns("settings_apply"), "Apply", class = "btn-primary")
        ),

        h5("Control Validation Thresholds"),
        p(class = "text-muted", "Set acceptable ranges for control performance"),

        # Negative control OD range
        div(
          class = "mb-3",
          tags$label(class = "form-label", "Negative Control OD Range"),
          div(
            class = "row",
            div(
              class = "col-6",
              numericInput(
                ns("neg_od_min_input"),
                "Min OD",
                value = thresholds$neg_od_min,
                min = 0,
                max = 5,
                step = 0.1,
                width = "100%"
              )
            ),
            div(
              class = "col-6",
              numericInput(
                ns("neg_od_max_input"),
                "Max OD",
                value = thresholds$neg_od_max,
                min = 0,
                max = 5,
                step = 0.1,
                width = "100%"
              )
            )
          ),
          tags$small(class = "text-muted", "Spec: 1.000 < OD < 3.100")
        ),

        # Positive control inhibition
        numericInput(
          ns("ctrl_inh_min_input"),
          "Positive Control - Minimum Inhibition %",
          value = thresholds$ctrl_inh_min,
          min = 0,
          max = 100,
          step = 5,
          width = "100%"
        ),
        tags$small(class = "text-muted mb-3 d-block", "Spec: ≥ 30%"),

        # Control CV
        numericInput(
          ns("ctrl_cv_max_input"),
          "Control CV - Maximum %",
          value = thresholds$ctrl_cv_max,
          min = 0,
          max = 100,
          step = 5,
          width = "100%"
        ),
        tags$small(class = "text-muted", "Maximum acceptable coefficient of variation")
      ))
    })

    # Reset to defaults
    observeEvent(input$settings_reset, {
      updateNumericInput(session, "neg_od_min_input", value = 1.0)
      updateNumericInput(session, "neg_od_max_input", value = 3.1)
      updateNumericInput(session, "ctrl_inh_min_input", value = 30)
      updateNumericInput(session, "ctrl_cv_max_input", value = 20)
    })

    # Apply settings
    observeEvent(input$settings_apply, {
      thresholds$neg_od_min <- input$neg_od_min_input
      thresholds$neg_od_max <- input$neg_od_max_input
      thresholds$ctrl_inh_min <- input$ctrl_inh_min_input
      thresholds$ctrl_cv_max <- input$ctrl_cv_max_input

      removeModal()

      showNotification(
        "QC thresholds updated. Data will be reloaded with new settings.",
        type = "message",
        duration = 3
      )
    })

    # ========================================================================
    # LOAD iELISA DATA (auto-refresh on threshold changes)
    # ========================================================================

    # Raw data loading
    raw_ielisa_data <- reactive({
      # Trigger refresh when thresholds change
      neg_min <- thresholds$neg_od_min
      neg_max <- thresholds$neg_od_max
      ctrl_inh_min <- thresholds$ctrl_inh_min
      ctrl_cv_max <- thresholds$ctrl_cv_max

      message("Loading iELISA data with QC settings...")

      tryCatch({
        data <- load_ielisa_data(
          # Uses site-aware paths by default (ielisa_dir and cache_dir)
          # Control QC parameters
          neg_od_min = neg_min,
          neg_od_max = neg_max,
          ctrl_inh_min = ctrl_inh_min,
          ctrl_cv_max = ctrl_cv_max,
          # Use F1 for loading (can be changed in UI later)
          formula = "f1"
        )

        if (nrow(data) == 0) {
          showNotification(
            "No iELISA data found. Please check the data/ielisa directory.",
            type = "warning",
            duration = 5
          )
        } else {
          message("Successfully loaded ", nrow(data), " iELISA samples")
        }

        data
      }, error = function(e) {
        showNotification(
          paste("Error loading iELISA data:", e$message),
          type = "error",
          duration = 10
        )
        tibble()
      })
    })

    # Filtered data (apply plate validity filter, custom QC settings, and global filters)
    filtered_ielisa_data <- reactive({
      data <- raw_ielisa_data()

      if (!nrow(data)) {
        return(tibble())
      }

      # Apply exclude invalid plates filter
      if (input$exclude_invalid_plates) {
        data <- data %>%
          filter(plate_valid_L13 == TRUE | plate_valid_L15 == TRUE)
      }

      # Apply retest filter if enabled
      if (isTRUE(input$show_retests_only)) {
        # Identify samples that have been tested multiple times
        if (all(c("code_barres_kps", "PlateDate") %in% names(data))) {
          retested_samples <- data %>%
            group_by(code_barres_kps) %>%
            filter(n_distinct(PlateDate) > 1) %>%
            ungroup()

          message("DEBUG [mod_ielisa_coordinator]: Showing only retested samples: ",
                  n_distinct(retested_samples$code_barres_kps), " samples")

          data <- retested_samples
        }
      }

      # Apply custom QC settings (threshold and formula)
      data <- apply_custom_qc(
        data,
        threshold = input$positivity_threshold,
        formula = input$formula_choice
      )

      # Apply global filters (Study, Province, Health Zone, Structure)
      # Join with biobank to get demographic info, then filter
      if (!is.null(biobank_df) && !is.null(filters)) {
        biobank_data <- tryCatch(biobank_df(), error = function(e) NULL)
        filter_list <- tryCatch(filters(), error = function(e) NULL)

        if (!is.null(biobank_data) && nrow(biobank_data) > 0 &&
            !is.null(filter_list) && is.list(filter_list)) {

          # Create a filtered barcode list from biobank
          filtered_barcodes <- biobank_data %>%
            select(barcode, any_of(c("study", "province", "health_zone",
                                     "health_structure", "health_facility",
                                     "structure_sanitaire")))

          # Apply filters to biobank to get valid barcodes
          if (!is.null(filter_list$cohort) && !identical(filter_list$cohort, "all") &&
              "study" %in% names(filtered_barcodes)) {
            filtered_barcodes <- filtered_barcodes %>%
              filter(is.na(study) | study %in% filter_list$cohort)
          }

          if (!is.null(filter_list$province) && !identical(filter_list$province, "all") &&
              "province" %in% names(filtered_barcodes)) {
            filtered_barcodes <- filtered_barcodes %>%
              filter(is.na(province) | province %in% filter_list$province)
          }

          if (!is.null(filter_list$zone) && !identical(filter_list$zone, "all") &&
              "health_zone" %in% names(filtered_barcodes)) {
            filtered_barcodes <- filtered_barcodes %>%
              filter(is.na(health_zone) | health_zone %in% filter_list$zone)
          }

          # Filter iELISA data to only include samples from filtered biobank
          if ("code_barres_kps" %in% names(data) && "barcode" %in% names(filtered_barcodes)) {
            valid_barcodes <- filtered_barcodes$barcode
            data <- data %>%
              filter(code_barres_kps %in% valid_barcodes)
          }
        }
      }

      data
    })

    # ========================================================================
    # CALL SUB-MODULES
    # ========================================================================

    # Runs (overview) module
    mod_ielisa_runs_server(
      "runs",
      ielisa_data = filtered_ielisa_data
    )

    # Samples module
    mod_ielisa_samples_server(
      "samples",
      ielisa_data = filtered_ielisa_data
    )

    # Analysis module (duplicates, multiple testing)
    mod_ielisa_analysis_server(
      "analysis",
      ielisa_data = filtered_ielisa_data
    )

    # ========================================================================
    # RETURN DATA FOR OTHER MODULES (if needed)
    # ========================================================================

    return(list(
      samples = filtered_ielisa_data,
      raw_data = raw_ielisa_data
    ))

  })
}
