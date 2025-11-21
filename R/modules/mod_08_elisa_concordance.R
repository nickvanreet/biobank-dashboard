# ELISA Concordance Analysis Module
# Compares PE and VSG ELISA results for the same samples

# UI ----
mod_elisa_concordance_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "ELISA Concordance",
    icon = icon("diagram-3"),
    page_fluid(
      # Header with settings button
      layout_columns(
        col_widths = c(9, 3),
        card(
          card_header(
            class = "d-flex justify-content-between align-items-center",
            div(
              class = "d-flex align-items-center gap-2",
              bsicons::bs_icon("diagram-3", size = "1.5rem"),
              "ELISA Concordance Analysis",
              span(
                class = "badge bg-info ms-2",
                "PE vs VSG"
              )
            )
          ),
          card_body(
            p("Compare PE-PGRS and VSG ELISA results to assess concordance, co-positivity, and discordance patterns."),
            p(class = "text-muted mb-0 small",
              "Samples are matched by barcode or lab number. Only samples tested in both assays are included.")
          )
        ),
        card(
          card_body(
            class = "d-flex align-items-center justify-content-end gap-2",
            actionButton(
              ns("settings_btn"),
              "Thresholds",
              icon = icon("sliders"),
              class = "btn-outline-primary"
            ),
            checkboxInput(
              ns("exclude_invalid_qc"),
              "Exclude invalid QC",
              value = FALSE
            )
          )
        )
      ),

      # Main content tabs
      navset_card_tab(
        id = ns("tabs"),
        nav_panel(
          "Summary",
          icon = icon("chart-pie"),
          mod_elisa_concordance_summary_ui(ns("summary"))
        ),
        nav_panel(
          "Table",
          icon = icon("table"),
          mod_elisa_concordance_table_ui(ns("table"))
        ),
        nav_panel(
          "Analysis",
          icon = icon("chart-line"),
          mod_elisa_concordance_analysis_ui(ns("analysis"))
        )
      )
    )
  )
}

# Server ----
mod_elisa_concordance_server <- function(id,
                                         biobank_df = reactive(NULL),
                                         filters = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for thresholds
    thresholds <- reactiveValues(
      pe_pp_cutoff = 20,
      pe_dod_cutoff = 0.3,
      vsg_pp_cutoff = 20,
      vsg_dod_cutoff = 0.3
    )

    # Settings modal
    observeEvent(input$settings_btn, {
      showModal(
        modalDialog(
          title = "Concordance Thresholds",
          size = "m",
          easyClose = TRUE,

          h5("PE-PGRS ELISA Positivity Thresholds"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("pe_pp_cutoff"),
              "PP% Cutoff (%)",
              value = thresholds$pe_pp_cutoff,
              min = 0,
              max = 100,
              step = 1
            ),
            numericInput(
              ns("pe_dod_cutoff"),
              "DOD Cutoff",
              value = thresholds$pe_dod_cutoff,
              min = 0,
              max = 3,
              step = 0.1
            )
          ),

          hr(),

          h5("VSG ELISA Positivity Thresholds"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("vsg_pp_cutoff"),
              "PP% Cutoff (%)",
              value = thresholds$vsg_pp_cutoff,
              min = 0,
              max = 100,
              step = 1
            ),
            numericInput(
              ns("vsg_dod_cutoff"),
              "DOD Cutoff",
              value = thresholds$vsg_dod_cutoff,
              min = 0,
              max = 3,
              step = 0.1
            )
          ),

          p(class = "text-muted small mt-3",
            "A sample is considered POSITIVE if it meets EITHER the PP% OR DOD threshold."),

          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("apply_thresholds"), "Apply", class = "btn-primary")
          )
        )
      )
    })

    # Apply threshold changes
    observeEvent(input$apply_thresholds, {
      thresholds$pe_pp_cutoff <- input$pe_pp_cutoff
      thresholds$pe_dod_cutoff <- input$pe_dod_cutoff
      thresholds$vsg_pp_cutoff <- input$vsg_pp_cutoff
      thresholds$vsg_dod_cutoff <- input$vsg_dod_cutoff
      removeModal()
    })

    # Load all ELISA data
    all_elisa_data <- reactive({
      data <- load_elisa_data(
        dirs = c("data/elisa_pe", "data/elisa_vsg"),
        biobank_df = biobank_df()
      )
      message("DEBUG: Loaded ", nrow(data), " total ELISA records")
      message("DEBUG: elisa_type values: ", paste(unique(data$elisa_type), collapse = ", "))
      message("DEBUG: sample_type values: ", paste(unique(data$sample_type), collapse = ", "))
      data
    })

    # Separate PE and VSG data
    pe_data <- reactive({
      data <- all_elisa_data() %>%
        filter(elisa_type == "ELISA_pe")
      message("DEBUG: PE records after filter: ", nrow(data))
      data
    })

    vsg_data <- reactive({
      data <- all_elisa_data() %>%
        filter(elisa_type == "ELISA_vsg")
      message("DEBUG: VSG records after filter: ", nrow(data))
      data
    })

    # Apply global filters to PE data
    pe_filtered <- reactive({
      data <- pe_data()
      message("DEBUG: PE data before filters: ", nrow(data))

      # Apply QC filter
      if (input$exclude_invalid_qc) {
        data <- data %>%
          filter(qc_overall == TRUE, plate_valid == TRUE)
        message("DEBUG: PE data after QC filter: ", nrow(data))
      }

      # Apply global filters if provided
      # NOTE: We DON'T apply date filters here because plate_date (ELISA plate run date)
      # is different from SampleDate (biobank sample collection date)
      if (!is.null(filters())) {
        f <- filters()
        message("DEBUG: PE Filters object: province=", f$province,
                ", health_zone=", f$health_zone,
                ", structure=", f$structure)

        # Province filter (only for biobank-matched samples)
        if (!is.null(f$province) && length(f$province) > 0 &&
            !tolower(f$province) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(Province) | Province %in% f$province)
          message("DEBUG: PE Province filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # Health Zone filter (only for biobank-matched samples)
        if (!is.null(f$health_zone) && length(f$health_zone) > 0 &&
            !tolower(f$health_zone) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(HealthZone) | HealthZone %in% f$health_zone)
          message("DEBUG: PE HealthZone filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # Structure filter (only for biobank-matched samples)
        if (!is.null(f$structure) && length(f$structure) > 0 &&
            !tolower(f$structure) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(Structure) | Structure %in% f$structure)
          message("DEBUG: PE Structure filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # NOTE: Date filters intentionally NOT applied - plate_date != SampleDate
      }

      message("DEBUG: PE data after all filters: ", nrow(data))
      message("DEBUG: PE samples (sample_type='sample'): ", sum(data$sample_type == "sample", na.rm = TRUE))
      data
    })

    # Apply global filters to VSG data
    vsg_filtered <- reactive({
      data <- vsg_data()
      message("DEBUG: VSG data before filters: ", nrow(data))

      # Apply QC filter
      if (input$exclude_invalid_qc) {
        data <- data %>%
          filter(qc_overall == TRUE, plate_valid == TRUE)
        message("DEBUG: VSG data after QC filter: ", nrow(data))
      }

      # Apply global filters if provided
      # NOTE: We DON'T apply date filters here because plate_date (ELISA plate run date)
      # is different from SampleDate (biobank sample collection date)
      if (!is.null(filters())) {
        f <- filters()

        # Province filter (only for biobank-matched samples)
        if (!is.null(f$province) && length(f$province) > 0 &&
            !tolower(f$province) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(Province) | Province %in% f$province)
          message("DEBUG: VSG Province filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # Health Zone filter (only for biobank-matched samples)
        if (!is.null(f$health_zone) && length(f$health_zone) > 0 &&
            !tolower(f$health_zone) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(HealthZone) | HealthZone %in% f$health_zone)
          message("DEBUG: VSG HealthZone filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # Structure filter (only for biobank-matched samples)
        if (!is.null(f$structure) && length(f$structure) > 0 &&
            !tolower(f$structure) %in% c("all", "")) {
          before <- nrow(data)
          data <- data %>% filter(is.na(Structure) | Structure %in% f$structure)
          message("DEBUG: VSG Structure filter applied - removed ", before - nrow(data), " rows (", nrow(data), " remaining)")
        }

        # NOTE: Date filters intentionally NOT applied - plate_date != SampleDate
      }

      message("DEBUG: VSG data after all filters: ", nrow(data))
      message("DEBUG: VSG samples (sample_type='sample'): ", sum(data$sample_type == "sample", na.rm = TRUE))
      data
    })

    # Match samples between PE and VSG
    matched_data <- reactive({
      pe <- pe_filtered()
      vsg <- vsg_filtered()
      message("DEBUG: Matching - PE filtered: ", nrow(pe), ", VSG filtered: ", nrow(vsg))

      matched <- match_elisa_samples(pe, vsg)
      message("DEBUG: Matched samples: ", nrow(matched))
      matched
    })

    # Calculate concordance with current thresholds
    concordance_results <- reactive({
      calculate_concordance(
        matched_data(),
        pe_pp_cutoff = thresholds$pe_pp_cutoff,
        pe_dod_cutoff = thresholds$pe_dod_cutoff,
        vsg_pp_cutoff = thresholds$vsg_pp_cutoff,
        vsg_dod_cutoff = thresholds$vsg_dod_cutoff
      )
    })

    # Call sub-modules
    mod_elisa_concordance_summary_server("summary", concordance_results)
    mod_elisa_concordance_table_server("table", concordance_results)
    mod_elisa_concordance_analysis_server("analysis", concordance_results)

    # Return concordance data for potential use by parent module
    return(concordance_results)
  })
}
