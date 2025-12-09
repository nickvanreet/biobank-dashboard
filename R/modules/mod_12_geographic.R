# R/modules/mod_12_geographic.R
# Geographic Visualization Module - Interactive Map with Test Results
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Geographic Visualization Module UI
#' @param id Module namespace ID
#' @export
mod_geographic_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    title = "Geographic",
    icon = icon("map-location-dot"),

    div(class = "container-fluid geo-panel",

        # ==== HEADER SECTION ================================================
        h4(class = "mb-3", icon("globe-africa"), " Geographic Distribution"),

        # KPI Cards (Top Row)
        layout_column_wrap(
          width = 1/5, fixed_width = TRUE, heights_equal = "row", gap = "12px",

          value_box(
            title = "Health Zones",
            value = textOutput(ns("n_zones")),
            showcase = icon("map-marked-alt"),
            theme = "primary"
          ),
          value_box(
            title = "Total Samples",
            value = textOutput(ns("total_samples")),
            showcase = icon("vial"),
            theme = "info"
          ),
          value_box(
            title = "MIC Positive",
            value = textOutput(ns("mic_positive")),
            showcase = icon("dna"),
            theme = "danger"
          ),
          value_box(
            title = "ELISA Positive",
            value = textOutput(ns("elisa_positive")),
            showcase = icon("flask"),
            theme = "warning"
          ),
          value_box(
            title = "iELISA Positive",
            value = textOutput(ns("ielisa_positive")),
            showcase = icon("vials"),
            theme = "success"
          )
        ),

        # ==== MAP CONTROLS ==================================================
        card(
          class = "mb-3",
          card_header(
            class = "d-flex justify-content-between align-items-center",
            span(icon("sliders-h"), " Map Display Options"),
            div(
              class = "d-flex gap-2",
              selectInput(
                ns("map_metric"),
                label = NULL,
                choices = c(
                  "Total Samples" = "total",
                  "MIC qPCR Results" = "mic",
                  "ELISA Results" = "elisa",
                  "iELISA Results" = "ielisa",
                  "Positivity Rate" = "positivity"
                ),
                selected = "total",
                width = "200px"
              ),
              selectInput(
                ns("color_scheme"),
                label = NULL,
                choices = c(
                  "Blue Gradient" = "Blues",
                  "Red Gradient" = "Reds",
                  "Green Gradient" = "Greens",
                  "Heat (Yellow-Red)" = "YlOrRd",
                  "Viridis" = "viridis"
                ),
                selected = "YlOrRd",
                width = "150px"
              )
            )
          ),
          card_body(
            div(
              class = "row",
              div(class = "col-md-8",
                  leaflet::leafletOutput(ns("main_map"), height = "550px")
              ),
              div(class = "col-md-4",
                  h6(class = "mb-2", icon("chart-bar"), " Zone Statistics"),
                  plotly::plotlyOutput(ns("zone_bar_chart"), height = "530px")
              )
            )
          )
        ),

        # ==== TEST RESULTS BY ZONE ==========================================
        h4(class = "mt-4 mb-3", icon("microscope"), " Test Results by Health Zone"),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("dna"), " MIC qPCR Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("mic_zone_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("flask"), " ELISA Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("elisa_zone_plot"), height = "400px")
            )
          )
        ),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("vials"), " iELISA Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("ielisa_zone_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("chart-pie"), " Combined Positivity by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("combined_positivity_plot"), height = "400px")
            )
          )
        ),

        # ==== DEMOGRAPHICS BY ZONE ==========================================
        h4(class = "mt-4 mb-3", icon("users"), " Demographics by Health Zone"),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("venus-mars"), " Sex Distribution by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("sex_zone_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("cake-candles"), " Age Distribution by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("age_zone_plot"), height = "400px")
            )
          )
        ),

        # ==== SUMMARY TABLE =================================================
        card(
          class = "mt-3",
          card_header(icon("table"), " Complete Geographic Summary"),
          card_body(
            DT::DTOutput(ns("geo_summary_table"))
          )
        )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' Geographic Visualization Module Server
#' @param id Module namespace ID
#' @param filtered_data Reactive biobank data (filtered by global filters)
#' @param mic_data Reactive MIC qPCR data
#' @param elisa_pe_data Reactive ELISA PE data
#' @param elisa_vsg_data Reactive ELISA VSG data
#' @param ielisa_data Reactive iELISA data
#' @export
mod_geographic_server <- function(id, filtered_data, mic_data = NULL,
                                   elisa_pe_data = NULL, elisa_vsg_data = NULL,
                                   ielisa_data = NULL) {

  moduleServer(id, function(input, output, session) {

    # ========================================================================
    # LOAD MAP DATA
    # ========================================================================

    map_data <- reactive({
      # Try to load the GeoJSON map
      map_path <- "data/lsd/map/cod_kasai_lomami_health_zones.geojson"

      if (!file.exists(map_path)) {
        return(NULL)
      }

      tryCatch({
        sf::st_read(map_path, quiet = TRUE)
      }, error = function(e) {
        message("Error loading map: ", e$message)
        NULL
      })
    })

    # ========================================================================
    # PREPARE GEOGRAPHIC DATA
    # ========================================================================

    # Normalize health zone names for matching
    normalize_zone_name <- function(x) {
      if (is.null(x)) return(NA_character_)
      x <- as.character(x)
      x <- stringr::str_trim(x)
      x <- stringr::str_to_title(x)
      # Remove accents
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
      x
    }

    # Aggregate biobank data by health zone
    zone_summary <- reactive({
      req(filtered_data())
      data <- filtered_data()

      if (nrow(data) == 0 || !"health_zone" %in% names(data)) {
        return(tibble::tibble(
          health_zone = character(),
          n_samples = integer(),
          n_male = integer(),
          n_female = integer(),
          median_age = numeric()
        ))
      }

      data %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::mutate(health_zone_norm = normalize_zone_name(health_zone)) %>%
        dplyr::group_by(health_zone, health_zone_norm) %>%
        dplyr::summarise(
          n_samples = dplyr::n(),
          n_male = sum(sex == "M", na.rm = TRUE),
          n_female = sum(sex == "F", na.rm = TRUE),
          median_age = median(age, na.rm = TRUE),
          .groups = "drop"
        )
    })

    # Get MIC results by zone
    mic_by_zone <- reactive({
      mic <- if (is.reactive(mic_data)) mic_data() else mic_data
      biobank <- filtered_data()

      if (is.null(mic) || !is.data.frame(mic) || nrow(mic) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          mic_positive = integer(),
          mic_negative = integer(),
          mic_inconclusive = integer(),
          mic_total = integer()
        ))
      }

      if (is.null(biobank) || nrow(biobank) == 0 || !"health_zone" %in% names(biobank)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          mic_positive = integer(),
          mic_negative = integer(),
          mic_inconclusive = integer(),
          mic_total = integer()
        ))
      }

      # Determine the status column
      status_col <- intersect(c("final_status", "status_final", "result", "interpretation"), names(mic))
      if (length(status_col) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          mic_positive = integer(),
          mic_negative = integer(),
          mic_inconclusive = integer(),
          mic_total = integer()
        ))
      }
      status_col <- status_col[1]

      # Join MIC with biobank to get health zone
      mic_joined <- mic %>%
        dplyr::left_join(
          biobank %>%
            dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
            dplyr::distinct(),
          by = intersect(names(mic), c("barcode", "lab_id", "numero_labo"))
        )

      if (!"health_zone" %in% names(mic_joined)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          mic_positive = integer(),
          mic_negative = integer(),
          mic_inconclusive = integer(),
          mic_total = integer()
        ))
      }

      mic_joined %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          status = toupper(.data[[status_col]])
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          mic_positive = sum(grepl("POS", status), na.rm = TRUE),
          mic_negative = sum(grepl("NEG", status), na.rm = TRUE),
          mic_inconclusive = sum(grepl("INC|INDO|BORDER", status), na.rm = TRUE),
          mic_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # Get ELISA results by zone (combine PE and VSG)
    elisa_by_zone <- reactive({
      pe <- if (is.reactive(elisa_pe_data)) elisa_pe_data() else elisa_pe_data
      vsg <- if (is.reactive(elisa_vsg_data)) elisa_vsg_data() else elisa_vsg_data
      biobank <- filtered_data()

      # Combine PE and VSG
      elisa <- dplyr::bind_rows(
        if (!is.null(pe) && is.data.frame(pe)) pe else tibble::tibble(),
        if (!is.null(vsg) && is.data.frame(vsg)) vsg else tibble::tibble()
      )

      if (nrow(elisa) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          elisa_positive = integer(),
          elisa_negative = integer(),
          elisa_borderline = integer(),
          elisa_total = integer()
        ))
      }

      if (is.null(biobank) || nrow(biobank) == 0 || !"health_zone" %in% names(biobank)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          elisa_positive = integer(),
          elisa_negative = integer(),
          elisa_borderline = integer(),
          elisa_total = integer()
        ))
      }

      # Determine the status column
      status_col <- intersect(c("status_final", "final_status", "result", "interpretation"), names(elisa))
      if (length(status_col) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          elisa_positive = integer(),
          elisa_negative = integer(),
          elisa_borderline = integer(),
          elisa_total = integer()
        ))
      }
      status_col <- status_col[1]

      # Join ELISA with biobank
      barcode_col <- intersect(c("code_barres_kps", "barcode", "numero_labo", "lab_id"), names(elisa))
      if (length(barcode_col) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          elisa_positive = integer(),
          elisa_negative = integer(),
          elisa_borderline = integer(),
          elisa_total = integer()
        ))
      }

      elisa_joined <- elisa %>%
        dplyr::rename_with(~ "join_key", .cols = dplyr::all_of(barcode_col[1])) %>%
        dplyr::left_join(
          biobank %>%
            dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
            dplyr::mutate(join_key = dplyr::coalesce(barcode, as.character(lab_id))) %>%
            dplyr::distinct(join_key, .keep_all = TRUE),
          by = "join_key"
        )

      if (!"health_zone" %in% names(elisa_joined)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          elisa_positive = integer(),
          elisa_negative = integer(),
          elisa_borderline = integer(),
          elisa_total = integer()
        ))
      }

      elisa_joined %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          status = toupper(.data[[status_col]])
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          elisa_positive = sum(grepl("POS", status), na.rm = TRUE),
          elisa_negative = sum(grepl("NEG", status), na.rm = TRUE),
          elisa_borderline = sum(grepl("BORDER|IND", status), na.rm = TRUE),
          elisa_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # Get iELISA results by zone
    ielisa_by_zone <- reactive({
      ielisa <- if (is.reactive(ielisa_data)) ielisa_data() else ielisa_data
      biobank <- filtered_data()

      if (is.null(ielisa) || !is.data.frame(ielisa) || nrow(ielisa) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          ielisa_positive = integer(),
          ielisa_negative = integer(),
          ielisa_indeterminate = integer(),
          ielisa_total = integer()
        ))
      }

      if (is.null(biobank) || nrow(biobank) == 0 || !"health_zone" %in% names(biobank)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          ielisa_positive = integer(),
          ielisa_negative = integer(),
          ielisa_indeterminate = integer(),
          ielisa_total = integer()
        ))
      }

      # Determine the status column
      status_col <- intersect(c("status_final", "final_status", "result", "interpretation"), names(ielisa))
      if (length(status_col) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          ielisa_positive = integer(),
          ielisa_negative = integer(),
          ielisa_indeterminate = integer(),
          ielisa_total = integer()
        ))
      }
      status_col <- status_col[1]

      # Join iELISA with biobank
      barcode_col <- intersect(c("code_barres_kps", "barcode", "numero_labo", "lab_id"), names(ielisa))
      if (length(barcode_col) == 0) {
        return(tibble::tibble(
          health_zone_norm = character(),
          ielisa_positive = integer(),
          ielisa_negative = integer(),
          ielisa_indeterminate = integer(),
          ielisa_total = integer()
        ))
      }

      ielisa_joined <- ielisa %>%
        dplyr::rename_with(~ "join_key", .cols = dplyr::all_of(barcode_col[1])) %>%
        dplyr::left_join(
          biobank %>%
            dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
            dplyr::mutate(join_key = dplyr::coalesce(barcode, as.character(lab_id))) %>%
            dplyr::distinct(join_key, .keep_all = TRUE),
          by = "join_key"
        )

      if (!"health_zone" %in% names(ielisa_joined)) {
        return(tibble::tibble(
          health_zone_norm = character(),
          ielisa_positive = integer(),
          ielisa_negative = integer(),
          ielisa_indeterminate = integer(),
          ielisa_total = integer()
        ))
      }

      ielisa_joined %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          status = toupper(.data[[status_col]])
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          ielisa_positive = sum(grepl("POS", status), na.rm = TRUE),
          ielisa_negative = sum(grepl("NEG", status), na.rm = TRUE),
          ielisa_indeterminate = sum(grepl("IND|BORDER", status), na.rm = TRUE),
          ielisa_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # Combined data for map
    combined_geo_data <- reactive({
      zones <- zone_summary()
      mic <- mic_by_zone()
      elisa <- elisa_by_zone()
      ielisa <- ielisa_by_zone()

      if (nrow(zones) == 0) {
        return(tibble::tibble())
      }

      result <- zones %>%
        dplyr::left_join(mic, by = "health_zone_norm") %>%
        dplyr::left_join(elisa, by = "health_zone_norm") %>%
        dplyr::left_join(ielisa, by = "health_zone_norm") %>%
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(.x, 0)),
          # Calculate overall positivity
          total_tested = mic_total + elisa_total + ielisa_total,
          total_positive = mic_positive + elisa_positive + ielisa_positive,
          positivity_rate = dplyr::if_else(
            total_tested > 0,
            round(total_positive / total_tested * 100, 1),
            0
          )
        )

      result
    })

    # Join map with data
    map_with_data <- reactive({
      map <- map_data()
      data <- combined_geo_data()

      if (is.null(map) || nrow(data) == 0) {
        return(map)
      }

      # Normalize map zone names
      map$zone_norm <- normalize_zone_name(map$zonesante)

      # Join data to map
      map_joined <- map %>%
        dplyr::left_join(
          data,
          by = c("zone_norm" = "health_zone_norm")
        ) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::any_of(c("n_samples", "mic_positive", "mic_negative", "mic_total",
                           "elisa_positive", "elisa_negative", "elisa_total",
                           "ielisa_positive", "ielisa_negative", "ielisa_total",
                           "total_positive", "positivity_rate")),
            ~ tidyr::replace_na(.x, 0)
          )
        )

      map_joined
    })

    # ========================================================================
    # KPI OUTPUTS
    # ========================================================================

    output$n_zones <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      as.character(nrow(data))
    })

    output$total_samples <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      scales::comma(sum(data$n_samples, na.rm = TRUE))
    })

    output$mic_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$mic_total, na.rm = TRUE)
      pos <- sum(data$mic_positive, na.rm = TRUE)
      if (total > 0) {
        sprintf("%s (%.1f%%)", scales::comma(pos), pos/total*100)
      } else {
        "0"
      }
    })

    output$elisa_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$elisa_total, na.rm = TRUE)
      pos <- sum(data$elisa_positive, na.rm = TRUE)
      if (total > 0) {
        sprintf("%s (%.1f%%)", scales::comma(pos), pos/total*100)
      } else {
        "0"
      }
    })

    output$ielisa_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$ielisa_total, na.rm = TRUE)
      pos <- sum(data$ielisa_positive, na.rm = TRUE)
      if (total > 0) {
        sprintf("%s (%.1f%%)", scales::comma(pos), pos/total*100)
      } else {
        "0"
      }
    })

    # ========================================================================
    # MAIN MAP OUTPUT
    # ========================================================================

    output$main_map <- leaflet::renderLeaflet({
      map <- map_with_data()

      if (is.null(map)) {
        # Return empty map centered on DRC
        return(
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            leaflet::setView(lng = 23.5, lat = -5.5, zoom = 7) %>%
            leaflet::addControl(
              html = "<div style='background: white; padding: 10px; border-radius: 5px;'>
                       <strong>No map data available</strong><br/>
                       Please ensure map files are in data/lsd/map/
                     </div>",
              position = "topright"
            )
        )
      }

      # Determine which metric to display
      metric <- input$map_metric %||% "total"

      map$display_value <- switch(
        metric,
        "total" = map$n_samples,
        "mic" = map$mic_positive,
        "elisa" = map$elisa_positive,
        "ielisa" = map$ielisa_positive,
        "positivity" = map$positivity_rate,
        map$n_samples
      )

      # Replace NAs with 0 for display
      map$display_value[is.na(map$display_value)] <- 0

      # Create color palette
      pal_name <- input$color_scheme %||% "YlOrRd"

      # Handle viridis separately
      if (pal_name == "viridis") {
        pal <- leaflet::colorNumeric(
          palette = viridisLite::viridis(9),
          domain = map$display_value,
          na.color = "#CCCCCC"
        )
      } else {
        pal <- leaflet::colorNumeric(
          palette = pal_name,
          domain = map$display_value,
          na.color = "#CCCCCC"
        )
      }

      # Create labels
      labels <- sprintf(
        "<strong>%s</strong><br/>
        Province: %s<br/>
        <hr style='margin: 5px 0;'>
        Samples: %s<br/>
        <b>MIC:</b> %d pos / %d total<br/>
        <b>ELISA:</b> %d pos / %d total<br/>
        <b>iELISA:</b> %d pos / %d total<br/>
        <b>Positivity:</b> %.1f%%",
        map$zonesante,
        map$province,
        scales::comma(map$n_samples),
        map$mic_positive, map$mic_total,
        map$elisa_positive, map$elisa_total,
        map$ielisa_positive, map$ielisa_total,
        map$positivity_rate
      ) %>%
        lapply(htmltools::HTML)

      # Create metric title for legend
      legend_title <- switch(
        metric,
        "total" = "Samples",
        "mic" = "MIC+",
        "elisa" = "ELISA+",
        "ielisa" = "iELISA+",
        "positivity" = "Positivity %",
        "Samples"
      )

      # Build the map
      leaflet::leaflet(map) %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(maxZoom = 18)
        ) %>%
        leaflet::addPolygons(
          fillColor = ~pal(display_value),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            weight = 4,
            color = "#4F46E5",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-family" = "Inter, sans-serif",
              "font-size" = "13px",
              "padding" = "8px 12px",
              "border-radius" = "6px"
            ),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~display_value,
          opacity = 0.8,
          title = legend_title,
          position = "bottomright"
        ) %>%
        leaflet::setView(lng = 23.8, lat = -6.0, zoom = 8)
    })

    # ========================================================================
    # ZONE BAR CHART (SIDEBAR)
    # ========================================================================

    output$zone_bar_chart <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(plotly::plotly_empty())
      }

      metric <- input$map_metric %||% "total"

      # Select the appropriate column
      data$value <- switch(
        metric,
        "total" = data$n_samples,
        "mic" = data$mic_positive,
        "elisa" = data$elisa_positive,
        "ielisa" = data$ielisa_positive,
        "positivity" = data$positivity_rate,
        data$n_samples
      )

      y_title <- switch(
        metric,
        "total" = "Samples",
        "mic" = "MIC Positive",
        "elisa" = "ELISA Positive",
        "ielisa" = "iELISA Positive",
        "positivity" = "Positivity Rate (%)",
        "Samples"
      )

      # Sort and limit to top 15
      data <- data %>%
        dplyr::arrange(dplyr::desc(value)) %>%
        dplyr::slice_head(n = 15)

      # Color based on metric
      bar_color <- switch(
        metric,
        "total" = "#4F46E5",
        "mic" = "#EF4444",
        "elisa" = "#F59E0B",
        "ielisa" = "#10B981",
        "positivity" = "#8B5CF6",
        "#4F46E5"
      )

      plotly::plot_ly(
        data,
        x = ~value,
        y = ~reorder(health_zone, value),
        type = "bar",
        orientation = "h",
        marker = list(
          color = bar_color,
          line = list(color = "white", width = 1)
        ),
        hovertemplate = paste0(
          "<b>%{y}</b><br>",
          y_title, ": %{x}<extra></extra>"
        )
      ) %>%
        plotly::layout(
          xaxis = list(title = y_title, gridcolor = "#E5E7EB"),
          yaxis = list(title = "", tickfont = list(size = 11)),
          margin = list(l = 120, r = 20, t = 20, b = 40),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )
    })

    # ========================================================================
    # TEST DISTRIBUTION PLOTS
    # ========================================================================

    # MIC distribution by zone
    output$mic_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0 || sum(data$mic_total) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(
                   annotations = list(
                     text = "No MIC data available",
                     showarrow = FALSE,
                     font = list(size = 16, color = "#6B7280")
                   )
                 ))
      }

      # Filter to zones with MIC data and sort
      mic_data <- data %>%
        dplyr::filter(mic_total > 0) %>%
        dplyr::arrange(dplyr::desc(mic_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(mic_data, x = ~reorder(health_zone, mic_total)) %>%
        plotly::add_bars(
          y = ~mic_positive,
          name = "Positive",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~mic_inconclusive,
          name = "Inconclusive",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~mic_negative,
          name = "Negative",
          marker = list(color = "#10B981")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Tests"),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    # ELISA distribution by zone
    output$elisa_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0 || sum(data$elisa_total) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(
                   annotations = list(
                     text = "No ELISA data available",
                     showarrow = FALSE,
                     font = list(size = 16, color = "#6B7280")
                   )
                 ))
      }

      elisa_data <- data %>%
        dplyr::filter(elisa_total > 0) %>%
        dplyr::arrange(dplyr::desc(elisa_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(elisa_data, x = ~reorder(health_zone, elisa_total)) %>%
        plotly::add_bars(
          y = ~elisa_positive,
          name = "Positive",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~elisa_borderline,
          name = "Borderline",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~elisa_negative,
          name = "Negative",
          marker = list(color = "#10B981")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Tests"),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    # iELISA distribution by zone
    output$ielisa_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0 || sum(data$ielisa_total) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(
                   annotations = list(
                     text = "No iELISA data available",
                     showarrow = FALSE,
                     font = list(size = 16, color = "#6B7280")
                   )
                 ))
      }

      ielisa_data <- data %>%
        dplyr::filter(ielisa_total > 0) %>%
        dplyr::arrange(dplyr::desc(ielisa_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(ielisa_data, x = ~reorder(health_zone, ielisa_total)) %>%
        plotly::add_bars(
          y = ~ielisa_positive,
          name = "Positive",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~ielisa_indeterminate,
          name = "Indeterminate",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~ielisa_negative,
          name = "Negative",
          marker = list(color = "#10B981")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Tests"),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    # Combined positivity plot
    output$combined_positivity_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(plotly::plotly_empty())
      }

      # Calculate positivity rates per test
      pos_data <- data %>%
        dplyr::mutate(
          mic_rate = dplyr::if_else(mic_total > 0, mic_positive / mic_total * 100, NA_real_),
          elisa_rate = dplyr::if_else(elisa_total > 0, elisa_positive / elisa_total * 100, NA_real_),
          ielisa_rate = dplyr::if_else(ielisa_total > 0, ielisa_positive / ielisa_total * 100, NA_real_)
        ) %>%
        dplyr::filter(!is.na(mic_rate) | !is.na(elisa_rate) | !is.na(ielisa_rate)) %>%
        dplyr::arrange(dplyr::desc(positivity_rate)) %>%
        dplyr::slice_head(n = 12)

      if (nrow(pos_data) == 0) {
        return(plotly::plotly_empty())
      }

      plotly::plot_ly(pos_data, x = ~reorder(health_zone, positivity_rate)) %>%
        plotly::add_bars(
          y = ~mic_rate,
          name = "MIC",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~elisa_rate,
          name = "ELISA",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~ielisa_rate,
          name = "iELISA",
          marker = list(color = "#10B981")
        ) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Positivity Rate (%)", range = c(0, 100)),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    # ========================================================================
    # DEMOGRAPHICS PLOTS
    # ========================================================================

    output$sex_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(plotly::plotly_empty())
      }

      sex_data <- data %>%
        dplyr::filter(n_samples > 0) %>%
        dplyr::arrange(dplyr::desc(n_samples)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(sex_data, x = ~reorder(health_zone, n_samples)) %>%
        plotly::add_bars(
          y = ~n_male,
          name = "Male",
          marker = list(color = "#3B82F6")
        ) %>%
        plotly::add_bars(
          y = ~n_female,
          name = "Female",
          marker = list(color = "#EC4899")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Samples"),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    output$age_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(plotly::plotly_empty())
      }

      age_data <- data %>%
        dplyr::filter(!is.na(median_age), n_samples >= 5) %>%
        dplyr::arrange(dplyr::desc(n_samples)) %>%
        dplyr::slice_head(n = 12)

      if (nrow(age_data) == 0) {
        return(plotly::plotly_empty())
      }

      plotly::plot_ly(
        age_data,
        x = ~reorder(health_zone, median_age),
        y = ~median_age,
        type = "bar",
        marker = list(
          color = ~median_age,
          colorscale = "Viridis",
          showscale = TRUE,
          colorbar = list(title = "Median Age")
        ),
        hovertemplate = "<b>%{x}</b><br>Median Age: %{y:.1f} years<br>Samples: %{customdata}<extra></extra>",
        customdata = ~n_samples
      ) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Median Age (years)"),
          margin = list(b = 100)
        )
    })

    # ========================================================================
    # SUMMARY TABLE
    # ========================================================================

    output$geo_summary_table <- DT::renderDT({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(DT::datatable(
          data.frame(Message = "No geographic data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }

      table_data <- data %>%
        dplyr::select(
          `Health Zone` = health_zone,
          `Samples` = n_samples,
          `Male` = n_male,
          `Female` = n_female,
          `Median Age` = median_age,
          `MIC+` = mic_positive,
          `MIC Total` = mic_total,
          `ELISA+` = elisa_positive,
          `ELISA Total` = elisa_total,
          `iELISA+` = ielisa_positive,
          `iELISA Total` = ielisa_total,
          `Positivity %` = positivity_rate
        ) %>%
        dplyr::arrange(dplyr::desc(Samples))

      DT::datatable(
        table_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        extensions = 'Buttons',
        class = "table table-striped table-hover table-sm",
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Samples',
          background = DT::styleColorBar(range(table_data$Samples), '#4F46E5'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        DT::formatStyle(
          'Positivity %',
          background = DT::styleColorBar(c(0, max(table_data$`Positivity %`, na.rm = TRUE)), '#EF4444'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        DT::formatRound('Median Age', digits = 1) %>%
        DT::formatRound('Positivity %', digits = 1)
    })

  })
}
