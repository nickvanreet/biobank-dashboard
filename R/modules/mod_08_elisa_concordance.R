# ELISA Concordance Analysis Module
# Compares PE and VSG ELISA results for the same samples

# UI ----
mod_elisa_concordance_ui <- function(id) {
  ns <- NS(id)

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
      load_elisa_data(
        dirs = c("data/elisa_pe", "data/elisa_vsg"),
        biobank_df = biobank_df()
      )
    })

    # Separate PE and VSG data
    pe_data <- reactive({
      all_elisa_data() %>%
        filter(elisa_type == "ELISA_pe")
    })

    vsg_data <- reactive({
      all_elisa_data() %>%
        filter(elisa_type == "ELISA_vsg")
    })

    # Apply global filters to PE data
    pe_filtered <- reactive({
      data <- pe_data()

      # Apply QC filter
      if (input$exclude_invalid_qc) {
        data <- data %>%
          filter(qc_overall == TRUE, plate_valid == TRUE)
      }

      # Apply global filters if provided
      if (!is.null(filters())) {
        f <- filters()

        # Province filter
        if (!is.null(f$province) && length(f$province) > 0 && f$province != "All") {
          data <- data %>% filter(Province %in% f$province)
        }

        # Health Zone filter
        if (!is.null(f$health_zone) && length(f$health_zone) > 0 && f$health_zone != "All") {
          data <- data %>% filter(HealthZone %in% f$health_zone)
        }

        # Structure filter
        if (!is.null(f$structure) && length(f$structure) > 0 && f$structure != "All") {
          data <- data %>% filter(Structure %in% f$structure)
        }

        # Date range filter
        if (!is.null(f$date_from) && !is.na(f$date_from)) {
          data <- data %>% filter(plate_date >= f$date_from)
        }
        if (!is.null(f$date_to) && !is.na(f$date_to)) {
          data <- data %>% filter(plate_date <= f$date_to)
        }
      }

      data
    })

    # Apply global filters to VSG data
    vsg_filtered <- reactive({
      data <- vsg_data()

      # Apply QC filter
      if (input$exclude_invalid_qc) {
        data <- data %>%
          filter(qc_overall == TRUE, plate_valid == TRUE)
      }

      # Apply global filters if provided
      if (!is.null(filters())) {
        f <- filters()

        # Province filter
        if (!is.null(f$province) && length(f$province) > 0 && f$province != "All") {
          data <- data %>% filter(Province %in% f$province)
        }

        # Health Zone filter
        if (!is.null(f$health_zone) && length(f$health_zone) > 0 && f$health_zone != "All") {
          data <- data %>% filter(HealthZone %in% f$health_zone)
        }

        # Structure filter
        if (!is.null(f$structure) && length(f$structure) > 0 && f$structure != "All") {
          data <- data %>% filter(Structure %in% f$structure)
        }

        # Date range filter
        if (!is.null(f$date_from) && !is.na(f$date_from)) {
          data <- data %>% filter(plate_date >= f$date_from)
        }
        if (!is.null(f$date_to) && !is.na(f$date_to)) {
          data <- data %>% filter(plate_date <= f$date_to)
        }
      }

      data
    })

    # Match samples between PE and VSG
    matched_data <- reactive({
      match_elisa_samples(pe_filtered(), vsg_filtered())
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
