# Assay Overview Module
# Comprehensive prevalence and concordance analysis across MIC qPCR, ELISA, and iELISA assays

mod_overview_assays_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Overview",
    icon = icon("dashboard"),
    page_fluid(
      layout_columns(
        col_widths = c(3, 9),
        # Redesigned Quality Controls Panel
        div(
          # Data Quality Summary Card
          card(
            class = "mb-3",
            card_header(
              class = "bg-primary text-white",
              div(class = "d-flex align-items-center",
                  icon("chart-pie", class = "me-2"),
                  tags$strong("Data Quality Summary")
              )
            ),
            card_body(
              class = "p-3",
              uiOutput(ns("quality_summary_stats"))
            )
          ),

          # Filter Controls Card
          card(
            class = "mb-3",
            card_header(
              class = "bg-secondary text-white",
              div(class = "d-flex align-items-center",
                  icon("filter", class = "me-2"),
                  tags$strong("Filter Controls")
              )
            ),
            card_body(
              class = "p-3",
              div(class = "mb-3",
                  tags$label(class = "form-label fw-bold", icon("triangle-exclamation", class = "text-warning me-1"), "Borderline Results"),
                  checkboxInput(
                    ns("include_borderline"),
                    label = "Include in analysis",
                    value = TRUE
                  )
              ),
              div(class = "mb-3",
                  tags$label(class = "form-label fw-bold", icon("ban", class = "text-danger me-1"), "Invalid Results"),
                  checkboxInput(
                    ns("include_invalid"),
                    label = "Include in analysis",
                    value = FALSE
                  )
              ),
              tags$hr(),
              tags$small(class = "text-muted",
                         icon("info-circle"), " Toggle filters to include/exclude results from all analyses")
            )
          ),

          # Classification Thresholds Card
          card(
            card_header(
              class = "bg-light",
              div(class = "d-flex align-items-center",
                  icon("sliders", class = "me-2"),
                  tags$strong("Classification Thresholds")
              )
            ),
            card_body(
              class = "p-2",
              # ELISA Section
              tags$div(
                class = "mb-3 p-2 border rounded bg-light",
                tags$div(class = "fw-bold text-primary mb-2", icon("vial", class = "me-1"), "ELISA (PE/VSG)"),
                tags$table(
                  class = "table table-sm table-borderless mb-0",
                  style = "font-size: 0.85em;",
                  tags$tr(
                    tags$td(class = "text-success fw-bold", "Positive"),
                    tags$td("PP% ≥20 or DOD ≥0.3")
                  ),
                  tags$tr(
                    tags$td(class = "text-warning fw-bold", "Borderline"),
                    tags$td("PP% 15-20 or DOD 0.2-0.3")
                  ),
                  tags$tr(
                    tags$td(class = "text-secondary fw-bold", "Negative"),
                    tags$td("Below thresholds")
                  )
                )
              ),
              # iELISA Section
              tags$div(
                class = "mb-3 p-2 border rounded bg-light",
                tags$div(class = "fw-bold text-success mb-2", icon("flask", class = "me-1"), "iELISA (LiTat 1.3/1.5)"),
                tags$table(
                  class = "table table-sm table-borderless mb-0",
                  style = "font-size: 0.85em;",
                  tags$tr(
                    tags$td(class = "text-success fw-bold", "Positive"),
                    tags$td("≥30% inhibition")
                  ),
                  tags$tr(
                    tags$td(class = "text-warning fw-bold", "Borderline"),
                    tags$td("25-30% inhibition")
                  ),
                  tags$tr(
                    tags$td(class = "text-secondary fw-bold", "Negative"),
                    tags$td("<25% inhibition")
                  )
                )
              ),
              # MIC Section
              tags$div(
                class = "p-2 border rounded bg-light",
                tags$div(class = "fw-bold text-info mb-2", icon("dna", class = "me-1"), "MIC qPCR (177T/18S2)"),
                tags$table(
                  class = "table table-sm table-borderless mb-0",
                  style = "font-size: 0.85em;",
                  tags$tr(
                    tags$td(class = "text-success fw-bold", "Positive"),
                    tags$td("Cq ≤35 (strong signal)")
                  ),
                  tags$tr(
                    tags$td(class = "text-warning fw-bold", "Borderline"),
                    tags$td("Cq 35-40 (late positive)")
                  ),
                  tags$tr(
                    tags$td(class = "text-secondary fw-bold", "Negative"),
                    tags$td("Cq >40 or undetermined")
                  )
                )
              )
            )
          )
        ),
        div(
          # Summary KPIs
          card(
            card_header(icon("chart-line"), "Summary Statistics"),
            card_body(
              layout_column_wrap(
                width = 1/3,
                value_box(title = "Total Samples", value = uiOutput(ns("kpi_total_samples")), showcase = icon("users"), theme = "primary"),
                value_box(title = "Total Tests Completed", value = uiOutput(ns("kpi_total_tests")), showcase = icon("vials"), theme = "info"),
                value_box(title = "Samples with Any Positive", value = uiOutput(ns("kpi_any_positive")), showcase = icon("check"), theme = "success")
              )
            )
          ),

          # Test Prevalence Section - Clickable KPIs
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(icon("chart-pie"), "Test Prevalence & Overlaps"),
              tags$span(
                class = "badge bg-info",
                icon("mouse-pointer", class = "me-1"),
                "Click any KPI to filter sample table"
              )
            ),
            card_body(
              # Custom CSS for clickable KPIs
              tags$style(HTML("
                .clickable-kpi {
                  cursor: pointer;
                  transition: transform 0.2s, box-shadow 0.2s;
                }
                .clickable-kpi:hover {
                  transform: translateY(-2px);
                  box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                }
                .clickable-kpi.active {
                  border: 3px solid #0d6efd !important;
                  box-shadow: 0 0 0 3px rgba(13, 110, 253, 0.25);
                }
              ")),

              h5(icon("dna", class = "text-info me-2"), "MIC qPCR"),
              layout_column_wrap(
                width = 1/3,
                # MIC-177T (DNA target)
                div(
                  class = "clickable-kpi",
                  id = ns("kpi_click_mic_177t"),
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-177T', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "MIC-177T Positive",
                    value = uiOutput(ns("kpi_mic_177t_positive")),
                    showcase = icon("dna"),
                    theme = "primary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-177T_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "177T Exclusive",
                    value = uiOutput(ns("kpi_mic_177t_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-177T_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "177T Shared",
                    value = uiOutput(ns("kpi_mic_177t_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              ),

              layout_column_wrap(
                width = 1/3,
                # MIC-18S2 (RNA target)
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-18S2', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "MIC-18S2 Positive",
                    value = uiOutput(ns("kpi_mic_18s2_positive")),
                    showcase = icon("dna"),
                    theme = "info",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-18S2_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "18S2 Exclusive",
                    value = uiOutput(ns("kpi_mic_18s2_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'MIC-18S2_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "18S2 Shared",
                    value = uiOutput(ns("kpi_mic_18s2_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              ),

              tags$hr(),
              h5(icon("vial", class = "text-primary me-2"), "ELISA Tests"),
              layout_column_wrap(
                width = 1/3,
                # ELISA PE
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA PE', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "ELISA PE Positive",
                    value = uiOutput(ns("kpi_pe_positive")),
                    showcase = icon("vial"),
                    theme = "info",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA PE_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "PE Exclusive",
                    value = uiOutput(ns("kpi_pe_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA PE_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "PE Shared",
                    value = uiOutput(ns("kpi_pe_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              ),

              layout_column_wrap(
                width = 1/3,
                # ELISA VSG
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA VSG', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "ELISA VSG Positive",
                    value = uiOutput(ns("kpi_vsg_positive")),
                    showcase = icon("vials"),
                    theme = "warning",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA VSG_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "VSG Exclusive",
                    value = uiOutput(ns("kpi_vsg_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'ELISA VSG_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "VSG Shared",
                    value = uiOutput(ns("kpi_vsg_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              ),

              tags$hr(),
              h5(icon("flask", class = "text-success me-2"), "iELISA Tests"),
              layout_column_wrap(
                width = 1/3,
                # iELISA L13
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.3', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "iELISA LiTat 1.3 Positive",
                    value = uiOutput(ns("kpi_l13_positive")),
                    showcase = icon("flask"),
                    theme = "success",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.3_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "L13 Exclusive",
                    value = uiOutput(ns("kpi_l13_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.3_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "L13 Shared",
                    value = uiOutput(ns("kpi_l13_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              ),

              layout_column_wrap(
                width = 1/3,
                # iELISA L15
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.5', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "iELISA LiTat 1.5 Positive",
                    value = uiOutput(ns("kpi_l15_positive")),
                    showcase = icon("flask-vial"),
                    theme = "success",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.5_exclusive', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "L15 Exclusive",
                    value = uiOutput(ns("kpi_l15_exclusive")),
                    showcase = icon("circle"),
                    theme = "secondary",
                    showcase_layout = "left center"
                  )
                ),
                div(
                  class = "clickable-kpi",
                  onclick = sprintf("Shiny.setInputValue('%s', 'iELISA LiTat 1.5_shared', {priority: 'event'})", ns("kpi_clicked")),
                  value_box(
                    title = "L15 Shared",
                    value = uiOutput(ns("kpi_l15_shared")),
                    showcase = icon("share-nodes"),
                    theme = "dark",
                    showcase_layout = "left center"
                  )
                )
              )
            )
          ),

          # Overlap Summary KPIs
          card(
            card_header(icon("diagram-project"), "Test Overlap Summary"),
            card_body(
              layout_column_wrap(
                width = 1/3,
                value_box(
                  title = "Positive on ALL Tests",
                  value = uiOutput(ns("kpi_all_tests_positive")),
                  showcase = icon("check-double"),
                  theme = "success"
                ),
                value_box(
                  title = "Positive on ALL Serology",
                  value = uiOutput(ns("kpi_all_serology_positive")),
                  showcase = icon("layer-group"),
                  theme = "info"
                ),
                value_box(
                  title = "MIC + Any Serology",
                  value = uiOutput(ns("kpi_mic_and_serology")),
                  showcase = icon("handshake"),
                  theme = "warning"
                )
              )
            )
          )
        )
      ),

      # Detailed Tables Section
      layout_columns(
        col_widths = c(12),
        card(
          full_screen = TRUE,
          card_header(icon("table"), "Detailed Test Prevalence Table"),
          card_body(
            DT::DTOutput(ns("prevalence_table"))
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(icon("th"), "Pairwise Test Overlaps"),
          card_body(
            tags$p(class = "text-muted small", "Number of samples positive on both tests"),
            DT::DTOutput(ns("overlap_table"))
          )
        ),
        card(
          full_screen = TRUE,
          card_header(icon("heat"), "Overlap Heatmap"),
          card_body_fill(
            plotlyOutput(ns("overlap_heatmap"), height = "400px"),
            tags$small(class = "text-muted", "Darker colors indicate more samples positive on both tests")
          )
        )
      ),

      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header(icon("bars"), "Status Distribution by Test"),
          card_body_fill(plotlyOutput(ns("assay_bars"), height = "350px"))
        ),
        card(
          card_header(icon("project-diagram"), "Positive Sample Overlaps"),
          card_body_fill(
            plotlyOutput(ns("upset_plot"), height = "350px"),
            tags$small(class = "text-muted", "Red bars = single test positive, Blue bars = multiple tests positive")
          )
        )
      ),

      card(
        card_header(
          class = "d-flex justify-content-between align-items-center",
          div(icon("table"), "Sample Drilldown"),
          div(
            uiOutput(ns("filter_status"), inline = TRUE),
            actionButton(ns("clear_filter"), "Clear Filter", class = "btn-sm btn-secondary ms-2"),
            downloadButton(ns("export_table"), "Export CSV", class = "btn-sm btn-primary ms-2")
          )
        ),
        card_body(DT::DTOutput(ns("sample_table")))
      )
    )
  )
}

mod_overview_assays_server <- function(id, biobank_df, elisa_df, ielisa_df, mic_df, filters) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Enhanced drill state with exclusive/shared filtering support
    drill_state <- reactiveValues(
      status = NULL,
      assay = NULL,
      filter_type = NULL  # "all", "exclusive", or "shared"
    )

    assay_palette <- reactive({
      assay_cutoffs()$shared_palette
    })

    prepared <- reactive({
      tryCatch({
        result <- prepare_assay_dashboard_data(
          biobank_df = if (is.null(biobank_df)) NULL else biobank_df(),
          elisa_df = if (is.null(elisa_df)) NULL else elisa_df(),
          ielisa_df = if (is.null(ielisa_df)) NULL else ielisa_df(),
          mic_data = if (is.null(mic_df)) NULL else mic_df(),
          filters = if (is.null(filters)) NULL else filters(),
          include_borderline = input$include_borderline,
          include_invalid = input$include_invalid
        )

        # Validate result structure
        if (is.null(result) || !is.list(result)) {
          warning("prepare_assay_dashboard_data returned NULL or invalid structure")
          return(NULL)
        }

        return(result)
      }, error = function(e) {
        warning(sprintf("Error in prepare_assay_dashboard_data: %s", e$message))
        return(NULL)
      })
    })

    # Enhanced filtered_tidy with exclusive/shared support
    filtered_tidy <- reactive({
      req(prepared())
      df <- prepared()$tidy_assays

      # Apply status filter
      if (!is.null(drill_state$status)) {
        df <- df %>% filter(status == drill_state$status)
      }

      # Apply assay filter with exclusive/shared support
      if (!is.null(drill_state$assay)) {
        target_assay <- drill_state$assay
        filter_type <- drill_state$filter_type

        if (!is.null(filter_type) && filter_type %in% c("exclusive", "shared")) {
          # Get samples positive on this assay
          positive_samples <- df %>%
            filter(as.character(assay) == target_assay, status == "Positive") %>%
            pull(sample_id)

          # Calculate how many positive tests each sample has
          sample_positive_counts <- df %>%
            filter(status == "Positive", sample_id %in% positive_samples) %>%
            group_by(sample_id) %>%
            summarise(n_positive = n(), .groups = "drop")

          if (filter_type == "exclusive") {
            # Exclusive: only positive on this test
            exclusive_samples <- sample_positive_counts %>%
              filter(n_positive == 1) %>%
              pull(sample_id)
            df <- df %>%
              filter(sample_id %in% exclusive_samples, as.character(assay) == target_assay)
          } else {
            # Shared: positive on this test AND at least one other
            shared_samples <- sample_positive_counts %>%
              filter(n_positive > 1) %>%
              pull(sample_id)
            df <- df %>%
              filter(sample_id %in% shared_samples, as.character(assay) == target_assay)
          }
        } else {
          # Standard assay filter (all positive for this test)
          df <- df %>% filter(as.character(assay) %in% target_assay)
        }
      }

      df
    })

    # Data Quality Summary Stats (for the redesigned Quality Controls panel)
    output$quality_summary_stats <- renderUI({
      req(prepared())

      tidy <- prepared()$tidy_assays
      if (is.null(tidy) || nrow(tidy) == 0) {
        return(tags$div(class = "text-muted text-center p-3", "No data available"))
      }

      # Calculate quality metrics
      total_tests <- nrow(tidy)
      total_samples <- n_distinct(tidy$sample_id)
      n_positive <- sum(tidy$status == "Positive", na.rm = TRUE)
      n_negative <- sum(tidy$status == "Negative", na.rm = TRUE)
      n_borderline <- sum(tidy$status == "Borderline", na.rm = TRUE)
      n_invalid <- sum(tidy$status == "Invalid", na.rm = TRUE)
      n_missing <- sum(tidy$status == "Missing", na.rm = TRUE)

      pct_positive <- round((n_positive / total_tests) * 100, 1)
      pct_negative <- round((n_negative / total_tests) * 100, 1)
      pct_borderline <- round((n_borderline / total_tests) * 100, 1)

      # Count unique assays
      assay_counts <- tidy %>% count(assay)
      n_assays <- nrow(assay_counts)

      # Create visual summary
      tagList(
        # Total counts row
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$span(class = "fw-bold", icon("users", class = "me-1"), "Samples"),
          tags$span(class = "badge bg-primary", total_samples)
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-2",
          tags$span(class = "fw-bold", icon("vials", class = "me-1"), "Tests"),
          tags$span(class = "badge bg-info", total_tests)
        ),
        tags$div(
          class = "d-flex justify-content-between align-items-center mb-3",
          tags$span(class = "fw-bold", icon("list-check", class = "me-1"), "Assay Types"),
          tags$span(class = "badge bg-secondary", n_assays)
        ),

        tags$hr(class = "my-2"),

        # Status breakdown with progress bars
        tags$div(class = "mb-2 small fw-bold", "Status Distribution"),

        # Positive bar
        tags$div(
          class = "d-flex align-items-center mb-1",
          tags$span(class = "me-2", style = "width: 70px;", sprintf("%d (%.0f%%)", n_positive, pct_positive)),
          tags$div(
            class = "progress flex-grow-1",
            style = "height: 8px;",
            tags$div(
              class = "progress-bar bg-primary",
              style = sprintf("width: %.0f%%;", pct_positive),
              role = "progressbar"
            )
          ),
          tags$span(class = "ms-2 text-primary small", icon("check"))
        ),

        # Negative bar
        tags$div(
          class = "d-flex align-items-center mb-1",
          tags$span(class = "me-2", style = "width: 70px;", sprintf("%d (%.0f%%)", n_negative, pct_negative)),
          tags$div(
            class = "progress flex-grow-1",
            style = "height: 8px;",
            tags$div(
              class = "progress-bar bg-secondary",
              style = sprintf("width: %.0f%%;", pct_negative),
              role = "progressbar"
            )
          ),
          tags$span(class = "ms-2 text-secondary small", icon("minus"))
        ),

        # Borderline bar (if any)
        if (n_borderline > 0) {
          tags$div(
            class = "d-flex align-items-center mb-1",
            tags$span(class = "me-2", style = "width: 70px;", sprintf("%d (%.0f%%)", n_borderline, pct_borderline)),
            tags$div(
              class = "progress flex-grow-1",
              style = "height: 8px;",
              tags$div(
                class = "progress-bar bg-warning",
                style = sprintf("width: %.0f%%;", pct_borderline),
                role = "progressbar"
              )
            ),
            tags$span(class = "ms-2 text-warning small", icon("triangle-exclamation"))
          )
        } else NULL,

        # Invalid/Missing warning
        if (n_invalid > 0 || n_missing > 0) {
          tags$div(
            class = "mt-2 p-2 bg-light rounded small",
            if (n_invalid > 0) tags$div(class = "text-danger", icon("ban", class = "me-1"), sprintf("%d invalid", n_invalid)),
            if (n_missing > 0) tags$div(class = "text-muted", icon("question", class = "me-1"), sprintf("%d missing", n_missing))
          )
        } else NULL
      )
    })

    # KPI Click Handler
    observeEvent(input$kpi_clicked, {
      clicked_value <- input$kpi_clicked

      # Parse the clicked value (format: "AssayName" or "AssayName_exclusive" or "AssayName_shared")
      if (grepl("_exclusive$", clicked_value)) {
        assay_name <- sub("_exclusive$", "", clicked_value)
        drill_state$assay <- assay_name
        drill_state$status <- "Positive"
        drill_state$filter_type <- "exclusive"
      } else if (grepl("_shared$", clicked_value)) {
        assay_name <- sub("_shared$", "", clicked_value)
        drill_state$assay <- assay_name
        drill_state$status <- "Positive"
        drill_state$filter_type <- "shared"
      } else {
        # Standard click - show all results for this assay
        drill_state$assay <- clicked_value
        drill_state$status <- NULL
        drill_state$filter_type <- "all"
      }
    })

    # Helper function to get stats for a specific test
    get_test_stats <- function(test_name) {
      tryCatch({
        prep_data <- prepared()
        if (is.null(prep_data)) {
          return(list(
            n_positive = 0,
            pct_positive = 0,
            total_tests = 0,
            n_exclusive = 0,
            n_shared = 0,
            n_with_serology = 0
          ))
        }

        prevalence <- prep_data$test_prevalence %>% filter(assay == test_name)
        overlaps <- prep_data$test_specific_overlaps %>% filter(assay == test_name)

        if (nrow(prevalence) == 0) {
          return(list(
            n_positive = 0,
            pct_positive = 0,
            total_tests = 0,
            n_exclusive = 0,
            n_shared = 0,
            n_with_serology = 0
          ))
        }

        n_with_serology <- if (nrow(overlaps) > 0) {
          overlaps$n_with_any_serology
        } else {
          0
        }

        list(
          n_positive = prevalence$n_positive,
          pct_positive = prevalence$pct_positive,
          total_tests = prevalence$total_tests,
          n_exclusive = prevalence$n_exclusive,
          n_shared = prevalence$n_shared,
          n_with_serology = n_with_serology
        )
      }, error = function(e) {
        warning(sprintf("Error in get_test_stats for %s: %s", test_name, e$message))
        return(list(
          n_positive = 0,
          pct_positive = 0,
          total_tests = 0,
          n_exclusive = 0,
          n_shared = 0,
          n_with_serology = 0
        ))
      })
    }

    # Summary KPIs
    output$kpi_total_samples <- renderUI({
      req(prepared())
      summary <- prepared()$overlap_summary
      if (is.null(summary)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$total_samples %||% 0),
        tags$br(),
        tags$span(class = "text-muted", "Unique samples")
      )
    })

    output$kpi_total_tests <- renderUI({
      req(prepared())
      tidy <- prepared()$tidy_assays
      if (is.null(tidy)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      n_tests <- nrow(tidy)
      tags$div(
        tags$strong(style = "font-size: 2em;", n_tests),
        tags$br(),
        tags$span(class = "text-muted", "Total test results")
      )
    })

    output$kpi_any_positive <- renderUI({
      req(prepared())
      summary <- prepared()$overlap_summary
      if (is.null(summary)) {
        return(tags$div(tags$strong("0"), tags$br(), tags$span(class = "text-muted", "No data")))
      }
      pct <- if (summary$total_samples > 0) {
        (summary$n_any_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_any_positive %||% 0),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    # MIC-177T KPIs
    output$kpi_mic_177t_positive <- renderUI({
      stats <- get_test_stats("MIC-177T")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_mic_177t_exclusive <- renderUI({
      stats <- get_test_stats("MIC-177T")
      pct <- if (stats$n_positive > 0) {
        (stats$n_exclusive / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of positives", pct))
      )
    })

    output$kpi_mic_177t_shared <- renderUI({
      stats <- get_test_stats("MIC-177T")
      pct <- if (stats$n_positive > 0) {
        (stats$n_shared / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of positives", pct))
      )
    })

    # MIC-18S2 KPIs
    output$kpi_mic_18s2_positive <- renderUI({
      stats <- get_test_stats("MIC-18S2")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_mic_18s2_exclusive <- renderUI({
      stats <- get_test_stats("MIC-18S2")
      pct <- if (stats$n_positive > 0) {
        (stats$n_exclusive / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of positives", pct))
      )
    })

    output$kpi_mic_18s2_shared <- renderUI({
      stats <- get_test_stats("MIC-18S2")
      pct <- if (stats$n_positive > 0) {
        (stats$n_shared / stats$n_positive) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of positives", pct))
      )
    })

    # ELISA PE KPIs
    output$kpi_pe_positive <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_pe_exclusive <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only PE positive")
      )
    })

    output$kpi_pe_shared <- renderUI({
      stats <- get_test_stats("ELISA PE")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # ELISA VSG KPIs
    output$kpi_vsg_positive <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_vsg_exclusive <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only VSG positive")
      )
    })

    output$kpi_vsg_shared <- renderUI({
      stats <- get_test_stats("ELISA VSG")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # iELISA L13 KPIs
    output$kpi_l13_positive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_l13_exclusive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only L13 positive")
      )
    })

    output$kpi_l13_shared <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.3")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # iELISA L15 KPIs
    output$kpi_l15_positive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of %d tests", stats$pct_positive, stats$total_tests))
      )
    })

    output$kpi_l15_exclusive <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_exclusive),
        tags$br(),
        tags$span(class = "text-muted", "Only L15 positive")
      )
    })

    output$kpi_l15_shared <- renderUI({
      stats <- get_test_stats("iELISA LiTat 1.5")
      tags$div(
        tags$strong(style = "font-size: 1.5em;", stats$n_shared),
        tags$br(),
        tags$span(class = "text-muted", "Also positive on others")
      )
    })

    # Overlap Summary KPIs
    output$kpi_all_tests_positive <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_all_tests_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_all_tests_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    output$kpi_all_serology_positive <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_all_serology_positive / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_all_serology_positive),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    output$kpi_mic_and_serology <- renderUI({
      summary <- prepared()$overlap_summary
      pct <- if (summary$total_samples > 0) {
        (summary$n_mic_and_any_serology / summary$total_samples) * 100
      } else {
        0
      }
      tags$div(
        tags$strong(style = "font-size: 2em;", summary$n_mic_and_any_serology),
        tags$br(),
        tags$span(class = "text-muted", sprintf("%.1f%% of samples", pct))
      )
    })

    # Prevalence Table
    output$prevalence_table <- DT::renderDT({
      req(prepared())

      tryCatch({
        prevalence <- prepared()$test_prevalence
        overlaps <- prepared()$test_specific_overlaps

        if (is.null(prevalence) || nrow(prevalence) == 0) {
          return(DT::datatable(
            data.frame(Message = "No test data available"),
            options = list(dom = 't', paging = FALSE),
            rownames = FALSE
          ))
        }

        # Join prevalence with detailed overlaps
        table_data <- prevalence %>%
          left_join(overlaps, by = "assay", suffix = c("", "_overlap")) %>%
          select(
            Test = assay,
            `Total Tests` = total_tests,
            Positive = n_positive,
            `% Positive` = pct_positive,
            `Exclusive` = n_exclusive,
            `Shared` = n_shared,
            Negative = n_negative,
            Borderline = n_borderline,
            Invalid = n_invalid
          )

        DT::datatable(
          table_data,
          options = list(
            pageLength = 10,
            scrollX = TRUE,
            dom = 'Bfrtip'
          ),
          class = "table-sm table-striped",
          rownames = FALSE
        ) %>%
          DT::formatPercentage("% Positive", digits = 1) %>%
          DT::formatStyle(
            "Positive",
            background = DT::styleColorBar(range(0, max(table_data$Positive, na.rm = TRUE)), "#2563EB"),
            backgroundSize = '100% 90%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
          )
      }, error = function(e) {
        message(sprintf("Error rendering prevalence table: %s", e$message))
        DT::datatable(
          data.frame(Error = paste("Error:", e$message)),
          options = list(dom = 't', paging = FALSE),
          rownames = FALSE
        )
      })
    })

    # Overlap Table
    output$overlap_table <- DT::renderDT({
      overlaps <- prepared()$pairwise_overlaps

      if (nrow(overlaps) == 0) {
        return(DT::datatable(
          data.frame(Message = "No overlap data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }

      # Create a symmetric matrix for display
      overlap_matrix <- overlaps %>%
        select(`Test 1` = assay1, `Test 2` = assay2, `Both Positive` = n_both_positive)

      DT::datatable(
        overlap_matrix,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        class = "table-sm table-striped",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          "Both Positive",
          background = DT::styleColorBar(range(overlap_matrix$`Both Positive`, na.rm = TRUE), "#10B981"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Overlap Heatmap
    output$overlap_heatmap <- renderPlotly({
      overlaps <- prepared()$pairwise_overlaps

      if (nrow(overlaps) == 0) {
        return(plotly_empty() %>%
          layout(annotations = list(
            text = "No overlap data available",
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5, showarrow = FALSE,
            font = list(size = 16)
          ))
        )
      }

      p <- plot_ly(
        data = overlaps,
        x = ~assay2,
        y = ~assay1,
        z = ~n_both_positive,
        type = "heatmap",
        text = ~paste0(assay1, " ∩ ", assay2, "<br>", n_both_positive, " samples"),
        hoverinfo = "text",
        colors = colorRamp(c("#FFFFFF", "#10B981", "#047857"))
      ) %>%
        layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          margin = list(l = 100, r = 20, t = 20, b = 100)
        )

      p
    })

    # Status bars
    output$assay_bars <- renderPlotly({
      df <- filtered_tidy() %>%
        count(assay, status) %>%
        mutate(status = factor(status, levels = names(assay_palette())))
      if (!nrow(df)) return(NULL)
      p <- ggplot(df, aes(x = assay, y = n, fill = status, text = paste("Assay:", assay, "<br>Status:", status, "<br>n:", n))) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = assay_palette()) +
        theme_minimal() + labs(x = NULL, y = "Samples", title = "Click a bar to filter samples") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text", source = "assay_bars")
    })

    # UpSet plot
    output$upset_plot <- renderPlotly({
      inter <- prepared()$intersections
      if (!nrow(inter)) return(NULL)

      # Add a column to indicate if it's a single test or multiple tests
      inter <- inter %>%
        mutate(
          n_assays = str_count(combo, "\\+") + 1,
          is_single = n_assays == 1,
          fill_color = if_else(is_single, "#EF4444", "#2563EB")  # Red for single, blue for multiple
        )

      p <- ggplot(inter, aes(x = reorder(combo, n), y = n,
                            text = paste(combo, "<br>n=", n, "<br>",
                                        if_else(is_single, "Single test positive", "Multiple tests positive")),
                            fill = fill_color)) +
        geom_col() +
        scale_fill_identity() +
        coord_flip() +
        theme_minimal() +
        labs(x = "Test combination", y = "Positive samples") +
        theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    })

    # Sample table
    output$sample_table <- DT::renderDT({
      df <- filtered_tidy()

      # Show count message
      total_count <- nrow(prepared()$tidy_assays)
      filtered_count <- nrow(df)

      DT::datatable(
        df %>% select(sample_id, assay, status, metric, quantitative, assay_date),
        options = list(
          pageLength = 25,
          order = list(list(0, 'asc')),
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        caption = if (filtered_count < total_count) {
          sprintf("Showing %d of %d total tests", filtered_count, total_count)
        } else {
          sprintf("Showing all %d tests", total_count)
        },
        extensions = 'Buttons',
        selection = "single",
        class = "table-sm table-striped",
        rownames = FALSE
      )
    })

    # Filter status display with enhanced filter type support
    output$filter_status <- renderUI({
      if (!is.null(drill_state$status) || !is.null(drill_state$assay)) {
        filters_active <- c()
        if (!is.null(drill_state$status)) {
          filters_active <- c(filters_active, sprintf("Status: %s", drill_state$status))
        }
        if (!is.null(drill_state$assay)) {
          assay_label <- paste(drill_state$assay, collapse = ", ")
          if (!is.null(drill_state$filter_type) && drill_state$filter_type != "all") {
            filter_label <- switch(drill_state$filter_type,
              "exclusive" = "Exclusive only",
              "shared" = "Shared only",
              ""
            )
            assay_label <- sprintf("%s (%s)", assay_label, filter_label)
          }
          filters_active <- c(filters_active, sprintf("Test: %s", assay_label))
        }
        tags$span(
          class = "badge bg-info me-2",
          icon("filter"),
          " ",
          paste(filters_active, collapse = " | ")
        )
      } else {
        NULL
      }
    })

    # Clear filter button
    observeEvent(input$clear_filter, {
      drill_state$status <- NULL
      drill_state$assay <- NULL
      drill_state$filter_type <- NULL
    })

    # Make KPIs clickable by detecting table row clicks
    observeEvent(input$prevalence_table_rows_selected, {
      req(input$prevalence_table_rows_selected)
      selected_row <- input$prevalence_table_rows_selected

      prevalence <- prepared()$test_prevalence
      if (!is.null(prevalence) && nrow(prevalence) >= selected_row) {
        selected_test <- as.character(prevalence$assay[selected_row])
        drill_state$assay <- selected_test
        drill_state$status <- NULL  # Clear status filter when selecting test
      }
    })

    # Click on plots to filter
    observeEvent(event_data("plotly_click", source = "assay_bars"), {
      click_data <- event_data("plotly_click", source = "assay_bars")
      if (!is.null(click_data)) {
        drill_state$assay <- click_data$x
        drill_state$status <- NULL
      }
    })

    output$export_table <- downloadHandler(
      filename = function() paste0("assay_drilldown_", Sys.Date(), ".csv"),
      content = function(file) {
        readr::write_csv(filtered_tidy(), file)
      }
    )
  })
}
