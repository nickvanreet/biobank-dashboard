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

        # KPI Cards (Top Row) - Molecular vs Serological
        layout_column_wrap(
          width = 1/6, fixed_width = TRUE, heights_equal = "row", gap = "10px",

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
            title = "MIC DNA+",
            value = textOutput(ns("mic_dna_positive")),
            showcase = icon("dna"),
            theme = "danger"
          ),
          value_box(
            title = "MIC RNA+",
            value = textOutput(ns("mic_rna_positive")),
            showcase = icon("disease"),
            theme = "warning"
          ),
          value_box(
            title = "Sero+ (ELISA)",
            value = textOutput(ns("elisa_positive")),
            showcase = icon("flask"),
            theme = "success"
          ),
          value_box(
            title = "Sero+ (iELISA)",
            value = textOutput(ns("ielisa_positive")),
            showcase = icon("vials"),
            theme = "purple"
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
                  "MIC DNA+ (177T)" = "mic_dna",
                  "MIC RNA+ (18S2)" = "mic_rna",
                  "MIC TNA+ (DNA+RNA)" = "mic_tna",
                  "MIC Any Positive" = "mic_any",
                  "RNAseP-DNA+" = "rnasep_dna",
                  "RNAseP-RNA+" = "rnasep_rna",
                  "ELISA-PE Positive" = "elisa_pe",
                  "ELISA-VSG Positive" = "elisa_vsg",
                  "iELISA LiTat 1.3+" = "ielisa_l13",
                  "iELISA LiTat 1.5+" = "ielisa_l15",
                  "Molecular Positivity %" = "molecular_rate",
                  "Serological Positivity %" = "sero_rate"
                ),
                selected = "total",
                width = "220px"
              ),
              selectInput(
                ns("color_scheme"),
                label = NULL,
                choices = c(
                  "Heat (Yellow-Red)" = "YlOrRd",
                  "Blue Gradient" = "Blues",
                  "Red Gradient" = "Reds",
                  "Green Gradient" = "Greens",
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

        # ==== MOLECULAR VS SEROLOGICAL ======================================
        h4(class = "mt-4 mb-3", icon("microscope"), " Molecular vs Serological Results"),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("dna"), " MIC qPCR by Zone (DNA vs RNA)"),
            card_body_fill(
              plotly::plotlyOutput(ns("mic_zone_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("chart-pie"), " Molecular vs Serological by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("mol_sero_plot"), height = "400px")
            )
          )
        ),

        # ==== ELISA RESULTS =================================================
        h4(class = "mt-4 mb-3", icon("flask"), " ELISA Results by Health Zone"),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("flask"), " ELISA-PE Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("elisa_pe_zone_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("flask-vial"), " ELISA-VSG Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("elisa_vsg_zone_plot"), height = "400px")
            )
          )
        ),

        # ==== iELISA LiTat RESULTS ==========================================
        h4(class = "mt-4 mb-3", icon("vials"), " iELISA Results: LiTat 1.3 vs LiTat 1.5"),

        layout_columns(
          col_widths = c(6, 6), gap = "16px",

          card(
            full_screen = TRUE,
            card_header(icon("vial"), " iELISA LiTat 1.3 by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("ielisa_l13_plot"), height = "400px")
            )
          ),
          card(
            full_screen = TRUE,
            card_header(icon("vial"), " iELISA LiTat 1.5 by Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("ielisa_l15_plot"), height = "400px")
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
    # HELPER FUNCTIONS
    # ========================================================================

    # Normalize health zone names for matching
    normalize_zone_name <- function(x) {
      if (is.null(x)) return(NA_character_)
      x <- as.character(x)
      x <- stringr::str_trim(x)
      x <- stringr::str_to_title(x)
      x <- stringi::stri_trans_general(x, "Latin-ASCII")
      x
    }

    # Safe data extraction from reactive
    safe_get_data <- function(data_reactive) {
      tryCatch({
        if (is.reactive(data_reactive)) {
          data_reactive()
        } else if (is.function(data_reactive)) {
          data_reactive()
        } else {
          data_reactive
        }
      }, error = function(e) NULL)
    }

    # ========================================================================
    # AGGREGATE BIOBANK DATA BY HEALTH ZONE
    # ========================================================================

    zone_summary <- reactive({
      req(filtered_data())
      data <- filtered_data()

      if (nrow(data) == 0 || !"health_zone" %in% names(data)) {
        return(tibble::tibble(
          health_zone = character(),
          health_zone_norm = character(),
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

    # ========================================================================
    # MIC DATA BY ZONE (DNA vs RNA differentiation)
    # ========================================================================

    mic_by_zone <- reactive({
      mic <- safe_get_data(mic_data)
      biobank <- filtered_data()

      empty_result <- tibble::tibble(
        health_zone_norm = character(),
        mic_dna_pos = integer(),
        mic_rna_pos = integer(),
        mic_tna_pos = integer(),
        mic_any_pos = integer(),
        mic_negative = integer(),
        mic_rnasep_dna_pos = integer(),
        mic_rnasep_rna_pos = integer(),
        mic_total = integer()
      )

      if (is.null(mic) || !is.data.frame(mic) || nrow(mic) == 0) {
        return(empty_result)
      }

      if (is.null(biobank) || nrow(biobank) == 0) {
        return(empty_result)
      }

      # Get health zone from biobank via sample identifiers
      biobank_geo <- biobank %>%
        dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::distinct()

      if (nrow(biobank_geo) == 0) {
        return(empty_result)
      }

      # Create a copy and add join keys safely
      mic_work <- mic

      # Add barcode join key
      if ("code_barres_kps" %in% names(mic_work)) {
        mic_work$join_barcode <- as.character(mic_work$code_barres_kps)
      } else if ("barcode" %in% names(mic_work)) {
        mic_work$join_barcode <- as.character(mic_work$barcode)
      } else if ("SampleID" %in% names(mic_work)) {
        mic_work$join_barcode <- as.character(mic_work$SampleID)
      } else {
        mic_work$join_barcode <- NA_character_
      }

      # Add lab_id join key
      if ("numero_labo" %in% names(mic_work)) {
        mic_work$join_labid <- as.character(mic_work$numero_labo)
      } else if ("lab_id" %in% names(mic_work)) {
        mic_work$join_labid <- as.character(mic_work$lab_id)
      } else {
        mic_work$join_labid <- NA_character_
      }

      # Try joining by barcode first
      mic_joined <- NULL
      if ("barcode" %in% names(biobank_geo) && any(!is.na(mic_work$join_barcode))) {
        biobank_bc <- biobank_geo %>%
          dplyr::filter(!is.na(barcode)) %>%
          dplyr::mutate(join_barcode = as.character(barcode)) %>%
          dplyr::select(join_barcode, health_zone) %>%
          dplyr::distinct(join_barcode, .keep_all = TRUE)

        mic_joined <- mic_work %>%
          dplyr::left_join(biobank_bc, by = "join_barcode")
      }

      # If no health_zone yet, try lab_id join
      if (is.null(mic_joined) || !"health_zone" %in% names(mic_joined) || all(is.na(mic_joined$health_zone))) {
        if ("lab_id" %in% names(biobank_geo) && any(!is.na(mic_work$join_labid))) {
          biobank_lab <- biobank_geo %>%
            dplyr::filter(!is.na(lab_id)) %>%
            dplyr::mutate(join_labid = as.character(lab_id)) %>%
            dplyr::select(join_labid, health_zone) %>%
            dplyr::distinct(join_labid, .keep_all = TRUE)

          mic_joined <- mic_work %>%
            dplyr::left_join(biobank_lab, by = "join_labid")
        }
      }

      if (is.null(mic_joined) || !"health_zone" %in% names(mic_joined)) {
        return(empty_result)
      }

      # Filter to rows with health zone
      mic_with_zone <- mic_joined %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(mic_with_zone) == 0) {
        return(empty_result)
      }

      # Determine DNA/RNA/TNA/RNAseP positivity
      mic_results <- mic_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone)
        )

      # DNA positive
      if ("marker_177T" %in% names(mic_results)) {
        mic_results$is_dna_pos <- grepl("Pos", mic_results$marker_177T, ignore.case = TRUE)
      } else if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_dna_pos <- grepl("DNA|177T", mic_results$FinalCall, ignore.case = TRUE) &
          grepl("Pos|detect", mic_results$FinalCall, ignore.case = TRUE)
      } else {
        mic_results$is_dna_pos <- FALSE
      }

      # RNA positive
      if ("marker_18S2" %in% names(mic_results)) {
        mic_results$is_rna_pos <- grepl("Pos", mic_results$marker_18S2, ignore.case = TRUE)
      } else if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_rna_pos <- grepl("RNA|18S", mic_results$FinalCall, ignore.case = TRUE) &
          grepl("Pos|detect", mic_results$FinalCall, ignore.case = TRUE)
      } else {
        mic_results$is_rna_pos <- FALSE
      }

      # TNA positive (both DNA and RNA)
      mic_results$is_tna_pos <- mic_results$is_dna_pos & mic_results$is_rna_pos

      # Any positive
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_any_pos <- grepl("Pos|detect", mic_results$FinalCall, ignore.case = TRUE)
      } else if ("category" %in% names(mic_results)) {
        mic_results$is_any_pos <- mic_results$category == "positive"
      } else {
        mic_results$is_any_pos <- mic_results$is_dna_pos | mic_results$is_rna_pos
      }

      # Negative
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_negative <- grepl("Neg", mic_results$FinalCall, ignore.case = TRUE)
      } else if ("category" %in% names(mic_results)) {
        mic_results$is_negative <- mic_results$category == "negative"
      } else {
        mic_results$is_negative <- !mic_results$is_any_pos
      }

      # RNAseP-DNA positive
      if ("marker_RNAseP_DNA" %in% names(mic_results)) {
        mic_results$is_rnasep_dna_pos <- grepl("Pos", mic_results$marker_RNAseP_DNA, ignore.case = TRUE)
      } else if ("RNAseP_DNA" %in% names(mic_results)) {
        mic_results$is_rnasep_dna_pos <- grepl("Pos|Good", mic_results$RNAseP_DNA, ignore.case = TRUE)
      } else {
        mic_results$is_rnasep_dna_pos <- FALSE
      }

      # RNAseP-RNA positive
      if ("marker_RNAseP_RNA" %in% names(mic_results)) {
        mic_results$is_rnasep_rna_pos <- grepl("Pos", mic_results$marker_RNAseP_RNA, ignore.case = TRUE)
      } else if ("RNAseP_RNA" %in% names(mic_results)) {
        mic_results$is_rnasep_rna_pos <- grepl("Pos|Good", mic_results$RNAseP_RNA, ignore.case = TRUE)
      } else {
        mic_results$is_rnasep_rna_pos <- FALSE
      }

      mic_results %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          mic_dna_pos = sum(is_dna_pos, na.rm = TRUE),
          mic_rna_pos = sum(is_rna_pos, na.rm = TRUE),
          mic_tna_pos = sum(is_tna_pos, na.rm = TRUE),
          mic_any_pos = sum(is_any_pos, na.rm = TRUE),
          mic_negative = sum(is_negative, na.rm = TRUE),
          mic_rnasep_dna_pos = sum(is_rnasep_dna_pos, na.rm = TRUE),
          mic_rnasep_rna_pos = sum(is_rnasep_rna_pos, na.rm = TRUE),
          mic_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # ========================================================================
    # ELISA PE DATA BY ZONE
    # ========================================================================

    elisa_pe_by_zone <- reactive({
      elisa <- safe_get_data(elisa_pe_data)
      biobank <- filtered_data()

      empty_result <- tibble::tibble(
        health_zone_norm = character(),
        elisa_pe_pos = integer(),
        elisa_pe_neg = integer(),
        elisa_pe_border = integer(),
        elisa_pe_total = integer()
      )

      if (is.null(elisa) || !is.data.frame(elisa) || nrow(elisa) == 0) {
        return(empty_result)
      }

      if (is.null(biobank) || nrow(biobank) == 0) {
        return(empty_result)
      }

      # Get health zone from biobank
      biobank_geo <- biobank %>%
        dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::distinct()

      if (nrow(biobank_geo) == 0) {
        return(empty_result)
      }

      # Create a copy and add join keys safely (dual-key strategy like MIC)
      elisa_work <- elisa

      # Add barcode join key
      if ("code_barres_kps" %in% names(elisa_work)) {
        elisa_work$join_barcode <- as.character(elisa_work$code_barres_kps)
      } else if ("barcode" %in% names(elisa_work)) {
        elisa_work$join_barcode <- as.character(elisa_work$barcode)
      } else {
        elisa_work$join_barcode <- NA_character_
      }

      # Add lab_id join key
      if ("numero_labo" %in% names(elisa_work)) {
        elisa_work$join_labid <- as.character(elisa_work$numero_labo)
      } else if ("lab_id" %in% names(elisa_work)) {
        elisa_work$join_labid <- as.character(elisa_work$lab_id)
      } else {
        elisa_work$join_labid <- NA_character_
      }

      # Try joining by barcode first
      elisa_joined <- NULL
      if ("barcode" %in% names(biobank_geo) && any(!is.na(elisa_work$join_barcode))) {
        biobank_bc <- biobank_geo %>%
          dplyr::filter(!is.na(barcode)) %>%
          dplyr::mutate(join_barcode = as.character(barcode)) %>%
          dplyr::select(join_barcode, health_zone) %>%
          dplyr::distinct(join_barcode, .keep_all = TRUE)

        elisa_joined <- elisa_work %>%
          dplyr::left_join(biobank_bc, by = "join_barcode")
      }

      # If no health_zone yet, try lab_id join
      if (is.null(elisa_joined) || !"health_zone" %in% names(elisa_joined) || all(is.na(elisa_joined$health_zone))) {
        if ("lab_id" %in% names(biobank_geo) && any(!is.na(elisa_work$join_labid))) {
          biobank_lab <- biobank_geo %>%
            dplyr::filter(!is.na(lab_id)) %>%
            dplyr::mutate(join_labid = as.character(lab_id)) %>%
            dplyr::select(join_labid, health_zone) %>%
            dplyr::distinct(join_labid, .keep_all = TRUE)

          elisa_joined <- elisa_work %>%
            dplyr::left_join(biobank_lab, by = "join_labid")
        }
      }

      if (is.null(elisa_joined) || !"health_zone" %in% names(elisa_joined)) {
        return(empty_result)
      }

      # Filter to rows with health zone
      elisa_with_zone <- elisa_joined %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(elisa_with_zone) == 0) {
        return(empty_result)
      }

      # Get status column
      status_col <- intersect(c("status_final", "status_raw", "result"), names(elisa_with_zone))
      if (length(status_col) == 0) {
        return(empty_result)
      }

      elisa_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          status = toupper(.data[[status_col[1]]])
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          elisa_pe_pos = sum(grepl("POS", status), na.rm = TRUE),
          elisa_pe_neg = sum(grepl("NEG", status), na.rm = TRUE),
          elisa_pe_border = sum(grepl("BORDER|IND", status), na.rm = TRUE),
          elisa_pe_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # ========================================================================
    # ELISA VSG DATA BY ZONE
    # ========================================================================

    elisa_vsg_by_zone <- reactive({
      elisa <- safe_get_data(elisa_vsg_data)
      biobank <- filtered_data()

      empty_result <- tibble::tibble(
        health_zone_norm = character(),
        elisa_vsg_pos = integer(),
        elisa_vsg_neg = integer(),
        elisa_vsg_border = integer(),
        elisa_vsg_total = integer()
      )

      if (is.null(elisa) || !is.data.frame(elisa) || nrow(elisa) == 0) {
        return(empty_result)
      }

      if (is.null(biobank) || nrow(biobank) == 0) {
        return(empty_result)
      }

      biobank_geo <- biobank %>%
        dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::distinct()

      if (nrow(biobank_geo) == 0) {
        return(empty_result)
      }

      # Create a copy and add join keys safely (dual-key strategy like MIC)
      elisa_work <- elisa

      # Add barcode join key
      if ("code_barres_kps" %in% names(elisa_work)) {
        elisa_work$join_barcode <- as.character(elisa_work$code_barres_kps)
      } else if ("barcode" %in% names(elisa_work)) {
        elisa_work$join_barcode <- as.character(elisa_work$barcode)
      } else {
        elisa_work$join_barcode <- NA_character_
      }

      # Add lab_id join key
      if ("numero_labo" %in% names(elisa_work)) {
        elisa_work$join_labid <- as.character(elisa_work$numero_labo)
      } else if ("lab_id" %in% names(elisa_work)) {
        elisa_work$join_labid <- as.character(elisa_work$lab_id)
      } else {
        elisa_work$join_labid <- NA_character_
      }

      # Try joining by barcode first
      elisa_joined <- NULL
      if ("barcode" %in% names(biobank_geo) && any(!is.na(elisa_work$join_barcode))) {
        biobank_bc <- biobank_geo %>%
          dplyr::filter(!is.na(barcode)) %>%
          dplyr::mutate(join_barcode = as.character(barcode)) %>%
          dplyr::select(join_barcode, health_zone) %>%
          dplyr::distinct(join_barcode, .keep_all = TRUE)

        elisa_joined <- elisa_work %>%
          dplyr::left_join(biobank_bc, by = "join_barcode")
      }

      # If no health_zone yet, try lab_id join
      if (is.null(elisa_joined) || !"health_zone" %in% names(elisa_joined) || all(is.na(elisa_joined$health_zone))) {
        if ("lab_id" %in% names(biobank_geo) && any(!is.na(elisa_work$join_labid))) {
          biobank_lab <- biobank_geo %>%
            dplyr::filter(!is.na(lab_id)) %>%
            dplyr::mutate(join_labid = as.character(lab_id)) %>%
            dplyr::select(join_labid, health_zone) %>%
            dplyr::distinct(join_labid, .keep_all = TRUE)

          elisa_joined <- elisa_work %>%
            dplyr::left_join(biobank_lab, by = "join_labid")
        }
      }

      if (is.null(elisa_joined) || !"health_zone" %in% names(elisa_joined)) {
        return(empty_result)
      }

      # Filter to rows with health zone
      elisa_with_zone <- elisa_joined %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(elisa_with_zone) == 0) {
        return(empty_result)
      }

      status_col <- intersect(c("status_final", "status_raw", "result"), names(elisa_with_zone))
      if (length(status_col) == 0) {
        return(empty_result)
      }

      elisa_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          status = toupper(.data[[status_col[1]]])
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          elisa_vsg_pos = sum(grepl("POS", status), na.rm = TRUE),
          elisa_vsg_neg = sum(grepl("NEG", status), na.rm = TRUE),
          elisa_vsg_border = sum(grepl("BORDER|IND", status), na.rm = TRUE),
          elisa_vsg_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # ========================================================================
    # iELISA DATA BY ZONE (LiTat 1.3 and 1.5)
    # ========================================================================

    ielisa_by_zone <- reactive({
      ielisa <- safe_get_data(ielisa_data)
      biobank <- filtered_data()

      empty_result <- tibble::tibble(
        health_zone_norm = character(),
        ielisa_l13_pos = integer(),
        ielisa_l13_neg = integer(),
        ielisa_l15_pos = integer(),
        ielisa_l15_neg = integer(),
        ielisa_any_pos = integer(),
        ielisa_total = integer()
      )

      if (is.null(ielisa) || !is.data.frame(ielisa) || nrow(ielisa) == 0) {
        return(empty_result)
      }

      if (is.null(biobank) || nrow(biobank) == 0) {
        return(empty_result)
      }

      # Get health zone from biobank
      biobank_geo <- biobank %>%
        dplyr::select(dplyr::any_of(c("barcode", "lab_id", "health_zone"))) %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::distinct()

      if (nrow(biobank_geo) == 0) {
        return(empty_result)
      }

      # Create a copy and add join keys safely (dual-key strategy like MIC)
      ielisa_work <- ielisa

      # Add barcode join key
      if ("code_barres_kps" %in% names(ielisa_work)) {
        ielisa_work$join_barcode <- as.character(ielisa_work$code_barres_kps)
      } else if ("barcode" %in% names(ielisa_work)) {
        ielisa_work$join_barcode <- as.character(ielisa_work$barcode)
      } else {
        ielisa_work$join_barcode <- NA_character_
      }

      # Add lab_id join key
      if ("numero_labo" %in% names(ielisa_work)) {
        ielisa_work$join_labid <- as.character(ielisa_work$numero_labo)
      } else if ("lab_id" %in% names(ielisa_work)) {
        ielisa_work$join_labid <- as.character(ielisa_work$lab_id)
      } else {
        ielisa_work$join_labid <- NA_character_
      }

      # Try joining by barcode first
      ielisa_joined <- NULL
      if ("barcode" %in% names(biobank_geo) && any(!is.na(ielisa_work$join_barcode))) {
        biobank_bc <- biobank_geo %>%
          dplyr::filter(!is.na(barcode)) %>%
          dplyr::mutate(join_barcode = as.character(barcode)) %>%
          dplyr::select(join_barcode, health_zone) %>%
          dplyr::distinct(join_barcode, .keep_all = TRUE)

        ielisa_joined <- ielisa_work %>%
          dplyr::left_join(biobank_bc, by = "join_barcode")
      }

      # If no health_zone yet, try lab_id join
      if (is.null(ielisa_joined) || !"health_zone" %in% names(ielisa_joined) || all(is.na(ielisa_joined$health_zone))) {
        if ("lab_id" %in% names(biobank_geo) && any(!is.na(ielisa_work$join_labid))) {
          biobank_lab <- biobank_geo %>%
            dplyr::filter(!is.na(lab_id)) %>%
            dplyr::mutate(join_labid = as.character(lab_id)) %>%
            dplyr::select(join_labid, health_zone) %>%
            dplyr::distinct(join_labid, .keep_all = TRUE)

          ielisa_joined <- ielisa_work %>%
            dplyr::left_join(biobank_lab, by = "join_labid")
        }
      }

      if (is.null(ielisa_joined) || !"health_zone" %in% names(ielisa_joined)) {
        return(empty_result)
      }

      # Filter to rows with health zone
      ielisa_with_zone <- ielisa_joined %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(ielisa_with_zone) == 0) {
        return(empty_result)
      }

      # Check for LiTat 1.3 and 1.5 columns and compute positivity
      ielisa_results <- ielisa_with_zone %>%
        dplyr::mutate(health_zone_norm = normalize_zone_name(health_zone))

      # L1.3 positivity
      if ("positive_L13" %in% names(ielisa_results)) {
        ielisa_results$l13_pos <- as.logical(ielisa_results$positive_L13)
      } else {
        ielisa_results$l13_pos <- FALSE
      }

      # L1.5 positivity
      if ("positive_L15" %in% names(ielisa_results)) {
        ielisa_results$l15_pos <- as.logical(ielisa_results$positive_L15)
      } else {
        ielisa_results$l15_pos <- FALSE
      }

      # Replace NA with FALSE for logical operations
      ielisa_results$l13_pos[is.na(ielisa_results$l13_pos)] <- FALSE
      ielisa_results$l15_pos[is.na(ielisa_results$l15_pos)] <- FALSE
      ielisa_results$any_pos <- ielisa_results$l13_pos | ielisa_results$l15_pos

      ielisa_results %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          ielisa_l13_pos = sum(l13_pos, na.rm = TRUE),
          ielisa_l13_neg = sum(!l13_pos, na.rm = TRUE),
          ielisa_l15_pos = sum(l15_pos, na.rm = TRUE),
          ielisa_l15_neg = sum(!l15_pos, na.rm = TRUE),
          ielisa_any_pos = sum(any_pos, na.rm = TRUE),
          ielisa_total = dplyr::n(),
          .groups = "drop"
        )
    })

    # ========================================================================
    # COMBINED DATA FOR MAP
    # ========================================================================

    combined_geo_data <- reactive({
      zones <- zone_summary()
      mic <- mic_by_zone()
      elisa_pe <- elisa_pe_by_zone()
      elisa_vsg <- elisa_vsg_by_zone()
      ielisa <- ielisa_by_zone()

      if (nrow(zones) == 0) {
        return(tibble::tibble())
      }

      result <- zones %>%
        dplyr::left_join(mic, by = "health_zone_norm") %>%
        dplyr::left_join(elisa_pe, by = "health_zone_norm") %>%
        dplyr::left_join(elisa_vsg, by = "health_zone_norm") %>%
        dplyr::left_join(ielisa, by = "health_zone_norm") %>%
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(.x, 0)),
          # Combined ELISA
          elisa_total = elisa_pe_total + elisa_vsg_total,
          elisa_pos = elisa_pe_pos + elisa_vsg_pos,
          # Molecular = MIC
          molecular_tested = mic_total,
          molecular_pos = mic_any_pos,
          molecular_rate = dplyr::if_else(molecular_tested > 0,
                                          round(molecular_pos / molecular_tested * 100, 1), 0),
          # Serological = ELISA + iELISA
          sero_tested = elisa_total + ielisa_total,
          sero_pos = elisa_pos + ielisa_any_pos,
          sero_rate = dplyr::if_else(sero_tested > 0,
                                     round(sero_pos / sero_tested * 100, 1), 0)
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

      map$zone_norm <- normalize_zone_name(map$zonesante)

      map_joined <- map %>%
        dplyr::left_join(data, by = c("zone_norm" = "health_zone_norm")) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::where(is.numeric),
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
      as.character(sum(data$n_samples > 0))
    })

    output$total_samples <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      scales::comma(sum(data$n_samples, na.rm = TRUE))
    })

    output$mic_dna_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$mic_total, na.rm = TRUE)
      pos <- sum(data$mic_dna_pos, na.rm = TRUE)
      if (total > 0) {
        sprintf("%s (%.1f%%)", scales::comma(pos), pos/total*100)
      } else {
        "0"
      }
    })

    output$mic_rna_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$mic_total, na.rm = TRUE)
      pos <- sum(data$mic_rna_pos, na.rm = TRUE)
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
      pos <- sum(data$elisa_pos, na.rm = TRUE)
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
      pos <- sum(data$ielisa_any_pos, na.rm = TRUE)
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

      metric <- input$map_metric %||% "total"

      map$display_value <- switch(
        metric,
        "total" = map$n_samples,
        "mic_dna" = map$mic_dna_pos,
        "mic_rna" = map$mic_rna_pos,
        "mic_tna" = map$mic_tna_pos,
        "mic_any" = map$mic_any_pos,
        "rnasep_dna" = map$mic_rnasep_dna_pos,
        "rnasep_rna" = map$mic_rnasep_rna_pos,
        "elisa_pe" = map$elisa_pe_pos,
        "elisa_vsg" = map$elisa_vsg_pos,
        "ielisa_l13" = map$ielisa_l13_pos,
        "ielisa_l15" = map$ielisa_l15_pos,
        "molecular_rate" = map$molecular_rate,
        "sero_rate" = map$sero_rate,
        map$n_samples
      )

      map$display_value[is.na(map$display_value)] <- 0

      pal_name <- input$color_scheme %||% "YlOrRd"

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

      labels <- sprintf(
        "<strong>%s</strong><br/>
        Province: %s<br/>
        <hr style='margin: 5px 0;'>
        <b>Samples:</b> %s<br/>
        <hr style='margin: 5px 0;'>
        <b>MOLECULAR (MIC):</b><br/>
        &nbsp;&nbsp;DNA+ (177T): %d<br/>
        &nbsp;&nbsp;RNA+ (18S2): %d<br/>
        &nbsp;&nbsp;TNA+ (DNA+RNA): %d<br/>
        &nbsp;&nbsp;Any positive: %d<br/>
        &nbsp;&nbsp;Total tested: %d<br/>
        <hr style='margin: 5px 0;'>
        <b>EXTRACTION QC:</b><br/>
        &nbsp;&nbsp;RNAseP-DNA+: %d<br/>
        &nbsp;&nbsp;RNAseP-RNA+: %d<br/>
        <hr style='margin: 5px 0;'>
        <b>SEROLOGICAL:</b><br/>
        &nbsp;&nbsp;ELISA-PE+: %d / %d<br/>
        &nbsp;&nbsp;ELISA-VSG+: %d / %d<br/>
        &nbsp;&nbsp;iELISA L1.3+: %d<br/>
        &nbsp;&nbsp;iELISA L1.5+: %d<br/>
        &nbsp;&nbsp;iELISA total: %d<br/>
        <hr style='margin: 5px 0;'>
        <b>Molecular rate:</b> %.1f%%<br/>
        <b>Sero rate:</b> %.1f%%",
        map$zonesante,
        map$province,
        scales::comma(map$n_samples),
        map$mic_dna_pos, map$mic_rna_pos, map$mic_tna_pos, map$mic_any_pos, map$mic_total,
        map$mic_rnasep_dna_pos, map$mic_rnasep_rna_pos,
        map$elisa_pe_pos, map$elisa_pe_total,
        map$elisa_vsg_pos, map$elisa_vsg_total,
        map$ielisa_l13_pos, map$ielisa_l15_pos, map$ielisa_total,
        map$molecular_rate, map$sero_rate
      ) %>%
        lapply(htmltools::HTML)

      legend_title <- switch(
        metric,
        "total" = "Samples",
        "mic_dna" = "MIC DNA+",
        "mic_rna" = "MIC RNA+",
        "mic_tna" = "MIC TNA+",
        "mic_any" = "MIC Any+",
        "rnasep_dna" = "RNAseP-DNA+",
        "rnasep_rna" = "RNAseP-RNA+",
        "elisa_pe" = "ELISA-PE+",
        "elisa_vsg" = "ELISA-VSG+",
        "ielisa_l13" = "iELISA L1.3+",
        "ielisa_l15" = "iELISA L1.5+",
        "molecular_rate" = "Molecular %",
        "sero_rate" = "Sero %",
        "Samples"
      )

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
              "font-size" = "12px",
              "padding" = "8px 12px",
              "border-radius" = "6px"
            ),
            textsize = "12px",
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

      data$value <- switch(
        metric,
        "total" = data$n_samples,
        "mic_dna" = data$mic_dna_pos,
        "mic_rna" = data$mic_rna_pos,
        "mic_tna" = data$mic_tna_pos,
        "mic_any" = data$mic_any_pos,
        "rnasep_dna" = data$mic_rnasep_dna_pos,
        "rnasep_rna" = data$mic_rnasep_rna_pos,
        "elisa_pe" = data$elisa_pe_pos,
        "elisa_vsg" = data$elisa_vsg_pos,
        "ielisa_l13" = data$ielisa_l13_pos,
        "ielisa_l15" = data$ielisa_l15_pos,
        "molecular_rate" = data$molecular_rate,
        "sero_rate" = data$sero_rate,
        data$n_samples
      )

      y_title <- switch(
        metric,
        "total" = "Samples",
        "mic_dna" = "MIC DNA+",
        "mic_rna" = "MIC RNA+",
        "mic_tna" = "MIC TNA+",
        "mic_any" = "MIC Any+",
        "rnasep_dna" = "RNAseP-DNA+",
        "rnasep_rna" = "RNAseP-RNA+",
        "elisa_pe" = "ELISA-PE+",
        "elisa_vsg" = "ELISA-VSG+",
        "ielisa_l13" = "iELISA L1.3+",
        "ielisa_l15" = "iELISA L1.5+",
        "molecular_rate" = "Molecular %",
        "sero_rate" = "Sero %",
        "Samples"
      )

      data <- data %>%
        dplyr::arrange(dplyr::desc(value)) %>%
        dplyr::slice_head(n = 15)

      bar_color <- switch(
        metric,
        "total" = "#4F46E5",
        "mic_dna" = "#DC2626",
        "mic_rna" = "#F59E0B",
        "mic_tna" = "#B91C1C",
        "mic_any" = "#EF4444",
        "rnasep_dna" = "#0891B2",
        "rnasep_rna" = "#06B6D4",
        "elisa_pe" = "#10B981",
        "elisa_vsg" = "#059669",
        "ielisa_l13" = "#8B5CF6",
        "ielisa_l15" = "#7C3AED",
        "molecular_rate" = "#EF4444",
        "sero_rate" = "#10B981",
        "#4F46E5"
      )

      plotly::plot_ly(
        data,
        x = ~value,
        y = ~reorder(health_zone, value),
        type = "bar",
        orientation = "h",
        marker = list(color = bar_color, line = list(color = "white", width = 1)),
        hovertemplate = paste0("<b>%{y}</b><br>", y_title, ": %{x}<extra></extra>")
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
    # MIC PLOT (DNA vs RNA)
    # ========================================================================

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

      mic_data <- data %>%
        dplyr::filter(mic_total > 0) %>%
        dplyr::arrange(dplyr::desc(mic_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(mic_data, x = ~reorder(health_zone, mic_total)) %>%
        plotly::add_bars(
          y = ~mic_tna_pos,
          name = "TNA+ (DNA+RNA)",
          marker = list(color = "#B91C1C")
        ) %>%
        plotly::add_bars(
          y = ~pmax(0, mic_dna_pos - mic_tna_pos),
          name = "DNA only (177T)",
          marker = list(color = "#DC2626")
        ) %>%
        plotly::add_bars(
          y = ~pmax(0, mic_rna_pos - mic_tna_pos),
          name = "RNA only (18S2)",
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

    # ========================================================================
    # MOLECULAR VS SEROLOGICAL PLOT
    # ========================================================================

    output$mol_sero_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0) {
        return(plotly::plotly_empty())
      }

      plot_data <- data %>%
        dplyr::filter(molecular_tested > 0 | sero_tested > 0) %>%
        dplyr::arrange(dplyr::desc(n_samples)) %>%
        dplyr::slice_head(n = 12)

      if (nrow(plot_data) == 0) {
        return(plotly::plotly_empty())
      }

      plotly::plot_ly(plot_data, x = ~reorder(health_zone, n_samples)) %>%
        plotly::add_bars(
          y = ~molecular_rate,
          name = "Molecular (MIC) %",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~sero_rate,
          name = "Serological (ELISA+iELISA) %",
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
    # ELISA PE PLOT
    # ========================================================================

    output$elisa_pe_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0 || sum(data$elisa_pe_total) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(
                   annotations = list(
                     text = "No ELISA-PE data available",
                     showarrow = FALSE,
                     font = list(size = 16, color = "#6B7280")
                   )
                 ))
      }

      elisa_data <- data %>%
        dplyr::filter(elisa_pe_total > 0) %>%
        dplyr::arrange(dplyr::desc(elisa_pe_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(elisa_data, x = ~reorder(health_zone, elisa_pe_total)) %>%
        plotly::add_bars(
          y = ~elisa_pe_pos,
          name = "Positive",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~elisa_pe_border,
          name = "Borderline",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~elisa_pe_neg,
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

    # ========================================================================
    # ELISA VSG PLOT
    # ========================================================================

    output$elisa_vsg_zone_plot <- plotly::renderPlotly({
      data <- combined_geo_data()

      if (nrow(data) == 0 || sum(data$elisa_vsg_total) == 0) {
        return(plotly::plotly_empty() %>%
                 plotly::layout(
                   annotations = list(
                     text = "No ELISA-VSG data available",
                     showarrow = FALSE,
                     font = list(size = 16, color = "#6B7280")
                   )
                 ))
      }

      elisa_data <- data %>%
        dplyr::filter(elisa_vsg_total > 0) %>%
        dplyr::arrange(dplyr::desc(elisa_vsg_total)) %>%
        dplyr::slice_head(n = 12)

      plotly::plot_ly(elisa_data, x = ~reorder(health_zone, elisa_vsg_total)) %>%
        plotly::add_bars(
          y = ~elisa_vsg_pos,
          name = "Positive",
          marker = list(color = "#EF4444")
        ) %>%
        plotly::add_bars(
          y = ~elisa_vsg_border,
          name = "Borderline",
          marker = list(color = "#F59E0B")
        ) %>%
        plotly::add_bars(
          y = ~elisa_vsg_neg,
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

    # ========================================================================
    # iELISA L13 PLOT
    # ========================================================================

    output$ielisa_l13_plot <- plotly::renderPlotly({
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
          y = ~ielisa_l13_pos,
          name = "LiTat 1.3 Positive",
          marker = list(color = "#8B5CF6")
        ) %>%
        plotly::add_bars(
          y = ~ielisa_l13_neg,
          name = "LiTat 1.3 Negative",
          marker = list(color = "#C4B5FD")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Tests"),
          legend = list(orientation = "h", y = -0.25),
          margin = list(b = 100)
        )
    })

    # ========================================================================
    # iELISA L15 PLOT
    # ========================================================================

    output$ielisa_l15_plot <- plotly::renderPlotly({
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
          y = ~ielisa_l15_pos,
          name = "LiTat 1.5 Positive",
          marker = list(color = "#7C3AED")
        ) %>%
        plotly::add_bars(
          y = ~ielisa_l15_neg,
          name = "LiTat 1.5 Negative",
          marker = list(color = "#DDD6FE")
        ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "", tickangle = -45),
          yaxis = list(title = "Number of Tests"),
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
          `MIC DNA+` = mic_dna_pos,
          `MIC RNA+` = mic_rna_pos,
          `MIC TNA+` = mic_tna_pos,
          `MIC Total` = mic_total,
          `RNAseP-DNA+` = mic_rnasep_dna_pos,
          `RNAseP-RNA+` = mic_rnasep_rna_pos,
          `ELISA-PE+` = elisa_pe_pos,
          `ELISA-PE Tot` = elisa_pe_total,
          `ELISA-VSG+` = elisa_vsg_pos,
          `ELISA-VSG Tot` = elisa_vsg_total,
          `iELISA L1.3+` = ielisa_l13_pos,
          `iELISA L1.5+` = ielisa_l15_pos,
          `iELISA Tot` = ielisa_total,
          `Molecular %` = molecular_rate,
          `Sero %` = sero_rate
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
          'Molecular %',
          background = DT::styleColorBar(c(0, 100), '#EF4444'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        DT::formatStyle(
          'Sero %',
          background = DT::styleColorBar(c(0, 100), '#10B981'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

  })
}
