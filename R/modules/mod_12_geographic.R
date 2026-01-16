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
              ),
              checkboxInput(
                ns("show_structures"),
                label = span(icon("hospital"), " Show Structures"),
                value = TRUE,
                width = "160px"
              ),
              checkboxInput(
                ns("show_mobile_units"),
                label = span(icon("truck-medical"), " Mobile Units"),
                value = TRUE,
                width = "160px"
              )
            )
          ),
          card_body(
            div(
              class = "row",
              div(class = "col-12",
                  leaflet::leafletOutput(ns("main_map"), height = "650px")
              )
            ),
            # Details panel below map - shows info on click/hover
            div(
              class = "mt-3",
              card(
                class = "bg-light",
                card_header(
                  class = "py-2",
                  span(icon("info-circle"), " Zone Details"),
                  span(class = "text-muted small ms-2", "(Click on a health zone or structure for details)")
                ),
                card_body(
                  class = "py-2",
                  uiOutput(ns("zone_details_panel"))
                )
              )
            )
          )
        ),

        # Zone Statistics Chart (moved to separate card)
        card(
          class = "mb-3",
          card_header(icon("chart-bar"), " Zone Statistics"),
          card_body(
            plotly::plotlyOutput(ns("zone_bar_chart"), height = "350px")
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
    # REACTIVE VALUES FOR SELECTION STATE
    # ========================================================================

    selected_zone <- reactiveVal(NULL)
    selected_structure <- reactiveVal(NULL)
    selected_mobile_unit <- reactiveVal(NULL)

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
    # Health zone name mapping (biobank variants -> geojson names)
    zone_name_mapping <- c(
      # Citenge variants
      "TSHITENGE" = "Citenge",
      "CITENGE" = "Citenge",
      # Kabeya Kamwanga variants
      "KABEYA KAMUANGA" = "Kabeya Kamwanga",
      "KABEYA KAMWANGA" = "Kabeya Kamwanga",
      "KABEYA-KAMWANGA" = "Kabeya Kamwanga",
      # Mweneditu variants
      "MWENE-DITU" = "Mweneditu",
      "MWENE DITU" = "Mweneditu",
      "MUENE DITU" = "Mweneditu",
      "MUENE-DITU" = "Mweneditu",
      "MWENA-DITU" = "Mweneditu",
      "MWENA DITU" = "Mweneditu",
      "MWENEDITU" = "Mweneditu",
      # Kalambayi Kabanga variants
      "KALAMBAYI" = "Kalambayi Kabanga",
      "KALAMBAYI KABANGA" = "Kalambayi Kabanga",
      "KALAMBAYI-KABANGA" = "Kalambayi Kabanga",
      # Standard zone names (for case normalization)
      "BIBANGA" = "Bibanga",
      "BIPEMBA" = "Bipemba",
      "BONZOLA" = "Bonzola",
      "CILUNDU" = "Cilundu",
      "DIBINDI" = "Dibindi",
      "DIULU" = "Diulu",
      "KABINDA" = "Kabinda",
      "KALENDA" = "Kalenda",
      "KALONDA EST" = "Kalonda Est",
      "KAMANA" = "Kamana",
      "KAMIJI" = "Kamiji",
      "KANDA KANDA" = "Kanda Kanda",
      "KANDA-KANDA" = "Kanda Kanda",
      "KANSELE" = "Kansele",
      "KASANSA" = "Kasansa",
      "LUBAO" = "Lubao",
      "LUBILANJI" = "Lubilanji",
      "LUDIMBI LUKULA" = "Ludimbi Lukula",
      "LUDIMBI-LUKULA" = "Ludimbi Lukula",
      "LUKELENGE" = "Lukelenge",
      "LUPUTA" = "Luputa",
      "MAKOTA" = "Makota",
      "MIABI" = "Miabi",
      "MPOKOLO" = "Mpokolo",
      "MUKUMBI" = "Mukumbi",
      "MULUMBA" = "Mulumba",
      "MUYA" = "Muya",
      "NGANDAJIKA" = "Ngandajika",
      "NZABA" = "Nzaba",
      "TSHILENGE" = "Tshilenge",
      "TSHISHIMBI" = "Tshishimbi",
      "TSHOFA" = "Tshofa",
      "WIKONG" = "Wikong"
    )

    normalize_zone_name <- function(x) {
      if (is.null(x)) return(NA_character_)
      x <- as.character(x)
      x <- stringr::str_trim(x)

      # Try exact match in mapping (uppercase)
      x_upper <- toupper(x)
      matched <- zone_name_mapping[x_upper]

      # Use matched value if found, otherwise apply title case
      result <- ifelse(!is.na(matched), matched, stringr::str_to_title(x))
      result <- stringi::stri_trans_general(result, "Latin-ASCII")
      result
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
    # HEALTH STRUCTURES DATA (with coordinates and endemicity)
    # ========================================================================

    health_structures_data <- tibble::tibble(
      province = c(
        "Lomami", "Lomami", "Lomami", "Lomami",
        "Kasai-Oriental", "Kasai-Oriental", "Kasai-Oriental", "Kasai-Oriental",
        "Kasai-Oriental", "Kasai-Oriental", "Kasai-Oriental", "Kasai-Oriental",
        "Kasai-Oriental", "Kasai-Oriental", "Kasai-Oriental"
      ),
      zone_sante = c(
        "ZS Kalambayi Kabanga", "ZS Mulumba", "ZS Mweneditu", "ZS Kabinda",
        "ZS Bibanga", "ZS Bibanga", "ZS Mukumbi", "ZS Miabi",
        "ZS Citenge", "ZS Tshishimbi", "ZS Lukelenge", "ZS Dibindi",
        "ZS Bipemba", "ZS Muya", "ZS Bonzola"
      ),
      structure = c(
        "HGR Kalambayi Kabanga", "HGR de Mulumba", "HGR Mweneditu", "CS Nkumba",
        "HS Katanda", "CSR Tshibila", "HGR Mukumbi", "HGR Miabi",
        "HGR Citenge", "HGR Tshishimbi", "HGR Lukelenge", "HGR Dibindi",
        "HGR Bipemba", "HGR Muya", "HGR Dipumba"
      ),
      endemicite = c(
        "A", "B", "B", "B",
        "A", "A", "A", "B",
        "B", "B", "B", "B",
        "B", "C", "A"
      ),
      latitude = c(
        -6.498208333, -6.528583333, -7.0171585, -6.211511667,
        -6.333333333, -6.0627, -6.023643586, -6.214743333,
        -6.13261, -6.1906, -6.110713333, -6.12615835,
        -6.108013333, -6.105933762, -6.13627845
      ),
      longitude = c(
        23.9965, 23.86480167, 23.45565555, 24.03191833,
        23.901715, 23.80390333, 23.70578596, 23.39243167,
        23.66920833, 23.563735, 23.65144833, 23.61680671,
        23.58201333, 23.62184745, 23.57444875
      )
    )

    # ========================================================================
    # MOBILE UNITS DATA (extracted from biobank - structures starting with UM)
    # ========================================================================

    mobile_units_data <- reactive({
      data <- filtered_data()
      if (is.null(data) || nrow(data) == 0) {
        return(tibble::tibble(
          structure_name = character(),
          health_zone = character(),
          health_zone_norm = character(),
          province = character(),
          n_samples = integer(),
          latitude = numeric(),
          longitude = numeric()
        ))
      }

      # Find structure column
      structure_col <- NULL
      candidate_cols <- c("structure_sanitaire", "health_structure", "health_facility")
      for (col in candidate_cols) {
        if (col %in% names(data)) {
          structure_col <- col
          break
        }
      }

      if (is.null(structure_col)) {
        return(tibble::tibble(
          structure_name = character(),
          health_zone = character(),
          health_zone_norm = character(),
          province = character(),
          n_samples = integer(),
          latitude = numeric(),
          longitude = numeric()
        ))
      }

      # Extract mobile units (structures starting with "UM")
      mobile_data <- data %>%
        dplyr::filter(!is.na(.data[[structure_col]]) &
                        grepl("^UM", .data[[structure_col]], ignore.case = FALSE)) %>%
        dplyr::mutate(
          structure_name = .data[[structure_col]],
          health_zone_norm = normalize_zone_name(health_zone)
        )

      if (nrow(mobile_data) == 0) {
        return(tibble::tibble(
          structure_name = character(),
          health_zone = character(),
          health_zone_norm = character(),
          province = character(),
          n_samples = integer(),
          latitude = numeric(),
          longitude = numeric()
        ))
      }

      # Aggregate by structure and zone
      mobile_summary <- mobile_data %>%
        dplyr::group_by(structure_name, health_zone, health_zone_norm) %>%
        dplyr::summarise(
          n_samples = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::distinct()

      # Add approximate coordinates and province based on health zone from map
      map <- map_data()
      if (!is.null(map) && nrow(map) > 0) {
        # Get centroids and province for each zone
        zone_info <- map %>%
          dplyr::mutate(
            centroid = sf::st_centroid(geometry),
            longitude = sf::st_coordinates(centroid)[, 1],
            latitude = sf::st_coordinates(centroid)[, 2],
            zone_norm = normalize_zone_name(zonesante)
          ) %>%
          sf::st_drop_geometry()

        # Add province column if it exists in map, otherwise set to NA
        if (!"province" %in% names(zone_info)) {
          zone_info$province <- NA_character_
        }
        zone_info <- zone_info %>%
          dplyr::select(zone_norm, province, latitude, longitude)

        # Join zone info to mobile units, with slight offset for multiple units
        mobile_summary <- mobile_summary %>%
          dplyr::left_join(zone_info, by = c("health_zone_norm" = "zone_norm")) %>%
          dplyr::group_by(health_zone_norm) %>%
          dplyr::mutate(
            # Add small offset for multiple mobile units in same zone
            row_num = dplyr::row_number(),
            latitude = latitude + (row_num - 1) * 0.02,
            longitude = longitude + (row_num - 1) * 0.015
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-row_num)
      } else {
        mobile_summary <- mobile_summary %>%
          dplyr::mutate(province = NA_character_, latitude = NA_real_, longitude = NA_real_)
      }

      # Ensure consistent column order
      mobile_summary %>%
        dplyr::select(structure_name, health_zone, health_zone_norm, province, n_samples, latitude, longitude)
    })

    # ========================================================================
    # BIOBANK STRUCTURES BY ZONE (all structures from biobank data)
    # ========================================================================

    biobank_structures_by_zone <- reactive({
      data <- filtered_data()
      if (is.null(data) || nrow(data) == 0) {
        return(tibble::tibble(
          structure_name = character(),
          health_zone = character(),
          health_zone_norm = character(),
          n_samples = integer(),
          is_mobile_unit = logical()
        ))
      }

      # Find structure column
      structure_col <- NULL
      candidate_cols <- c("structure_sanitaire", "health_structure", "health_facility")
      for (col in candidate_cols) {
        if (col %in% names(data)) {
          structure_col <- col
          break
        }
      }

      if (is.null(structure_col) || !"health_zone" %in% names(data)) {
        return(tibble::tibble(
          structure_name = character(),
          health_zone = character(),
          health_zone_norm = character(),
          n_samples = integer(),
          is_mobile_unit = logical()
        ))
      }

      # Get all structures with their zones
      data %>%
        dplyr::filter(!is.na(.data[[structure_col]]) & !is.na(health_zone)) %>%
        dplyr::mutate(
          structure_name = .data[[structure_col]],
          health_zone_norm = normalize_zone_name(health_zone),
          is_mobile_unit = grepl("^UM", .data[[structure_col]], ignore.case = FALSE)
        ) %>%
        dplyr::group_by(structure_name, health_zone, health_zone_norm, is_mobile_unit) %>%
        dplyr::summarise(n_samples = dplyr::n(), .groups = "drop") %>%
        dplyr::arrange(health_zone_norm, dplyr::desc(n_samples))
    })

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

      # Check if MIC data already has health zone from coordinator linkage
      mic_work <- mic
      has_health_zone <- FALSE

      # Check for HealthZone (from MIC coordinator linkage) or health_zone
      if ("HealthZone" %in% names(mic_work) && any(!is.na(mic_work$HealthZone))) {
        mic_work$health_zone <- mic_work$HealthZone
        has_health_zone <- TRUE
      } else if ("health_zone" %in% names(mic_work) && any(!is.na(mic_work$health_zone))) {
        has_health_zone <- TRUE
      }

      # If no health zone in MIC data, try to join with biobank
      if (!has_health_zone) {
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

        mic_work <- mic_joined
      }

      # Filter to rows with health zone
      mic_with_zone <- mic_work %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(mic_with_zone) == 0) {
        return(empty_result)
      }

      # ========================================================================
      # STANDARDIZED MIC COUNTING (matches Sample Overview logic)
      # Uses utils_standardized_counting.R for consistent classification
      # ========================================================================

      # First, deduplicate samples to get one result per sample
      # This matches the Sample Overview module's deduplication logic
      sample_id_col <- if ("SampleID" %in% names(mic_with_zone)) "SampleID"
                       else if ("sample_id" %in% names(mic_with_zone)) "sample_id"
                       else if ("numero_labo" %in% names(mic_with_zone)) "numero_labo"
                       else NULL

      if (!is.null(sample_id_col)) {
        mic_dedup <- deduplicate_mic_samples(mic_with_zone, sample_id_col)
      } else {
        mic_dedup <- mic_with_zone
      }

      # Normalize health zone and apply standardized classification
      mic_results <- mic_dedup %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone)
        )

      # Use standardized classification for DNA (177T) and RNA (18S2)
      has_marker_177t <- "marker_177T" %in% names(mic_results) || "Call_177T" %in% names(mic_results)
      has_marker_18s2 <- "marker_18S2" %in% names(mic_results) || "Call_18S2" %in% names(mic_results)

      # ========================================================================
      # IMPORTANT: LatePositive (Cq 35-40) is NOT counted as positive!
      # It's tracked separately as a borderline/suspect category.
      # ========================================================================

      # DNA positive - using standardized classification (LatePositive excluded)
      if (has_marker_177t) {
        marker_col <- if ("marker_177T" %in% names(mic_results)) mic_results$marker_177T else mic_results$Call_177T
        mic_results$is_dna_pos <- is_mic_dna_positive(marker_col, include_latepositive = FALSE)
        mic_results$is_dna_late <- classify_mic_marker(marker_col) == "LatePositive"
      } else if ("FinalCall" %in% names(mic_results)) {
        # FinalCall: Positive (TNA), Positive_DNA, Positive_RNA, LatePositive
        mic_results$is_dna_pos <- mic_results$FinalCall %in% c("Positive", "Positive_DNA")
        mic_results$is_dna_late <- mic_results$FinalCall == "LatePositive"
      } else {
        mic_results$is_dna_pos <- FALSE
        mic_results$is_dna_late <- FALSE
      }

      # RNA positive - using standardized classification (LatePositive excluded)
      if (has_marker_18s2) {
        marker_col <- if ("marker_18S2" %in% names(mic_results)) mic_results$marker_18S2 else mic_results$Call_18S2
        mic_results$is_rna_pos <- is_mic_rna_positive(marker_col, include_latepositive = FALSE)
        mic_results$is_rna_late <- classify_mic_marker(marker_col) == "LatePositive"
      } else if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_rna_pos <- mic_results$FinalCall %in% c("Positive", "Positive_RNA")
        mic_results$is_rna_late <- mic_results$FinalCall == "LatePositive"
      } else {
        mic_results$is_rna_pos <- FALSE
        mic_results$is_rna_late <- FALSE
      }

      # TNA positive (both DNA and RNA) - sample has BOTH targets positive
      mic_results$is_tna_pos <- mic_results$is_dna_pos & mic_results$is_rna_pos

      # LatePositive - tracked separately (Cq 35-40 range, borderline/suspect)
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_late_pos <- mic_results$FinalCall == "LatePositive"
      } else {
        mic_results$is_late_pos <- mic_results$is_dna_late | mic_results$is_rna_late
      }

      # Any positive - does NOT include LatePositive (that's borderline)
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_any_pos <- is_mic_positive(mic_results$FinalCall,
                                                   include_latepositive = FALSE,
                                                   include_borderline = FALSE)
      } else {
        mic_results$is_any_pos <- mic_results$is_dna_pos | mic_results$is_rna_pos
      }

      # Negative - standardized classification
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_negative <- mic_results$FinalCall == "Negative"
      } else {
        mic_results$is_negative <- !mic_results$is_any_pos & !mic_results$is_late_pos
      }

      # Invalid - standardized classification
      if ("FinalCall" %in% names(mic_results)) {
        mic_results$is_invalid <- mic_results$FinalCall %in% standardized_cutoffs()$mic_invalid_calls
      } else {
        mic_results$is_invalid <- FALSE
      }

      # RNAseP-DNA positive (using standardized marker classification)
      if ("marker_RNAseP_DNA" %in% names(mic_results)) {
        mic_results$is_rnasep_dna_pos <- classify_mic_marker(mic_results$marker_RNAseP_DNA) == "Positive"
      } else if ("RNAseP_DNA" %in% names(mic_results)) {
        mic_results$is_rnasep_dna_pos <- classify_mic_marker(mic_results$RNAseP_DNA) == "Positive"
      } else {
        mic_results$is_rnasep_dna_pos <- FALSE
      }

      # RNAseP-RNA positive (using standardized marker classification)
      if ("marker_RNAseP_RNA" %in% names(mic_results)) {
        mic_results$is_rnasep_rna_pos <- classify_mic_marker(mic_results$marker_RNAseP_RNA) == "Positive"
      } else if ("RNAseP_RNA" %in% names(mic_results)) {
        mic_results$is_rnasep_rna_pos <- classify_mic_marker(mic_results$RNAseP_RNA) == "Positive"
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
          mic_late_pos = sum(is_late_pos, na.rm = TRUE),  # LatePositive (borderline/suspect)
          mic_negative = sum(is_negative, na.rm = TRUE),
          mic_invalid = sum(is_invalid, na.rm = TRUE),
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

      # Check if ELISA data already has health zone from coordinator linkage
      elisa_work <- elisa
      has_health_zone <- FALSE

      # Check for HealthZone (from coordinator linkage) or health_zone
      if ("HealthZone" %in% names(elisa_work) && any(!is.na(elisa_work$HealthZone))) {
        elisa_work$health_zone <- elisa_work$HealthZone
        has_health_zone <- TRUE
      } else if ("health_zone" %in% names(elisa_work) && any(!is.na(elisa_work$health_zone))) {
        has_health_zone <- TRUE
      }

      # If no health zone in ELISA data, try to join with biobank
      if (!has_health_zone) {
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

        elisa_work <- elisa_joined
      }

      # Filter to rows with health zone
      elisa_with_zone <- elisa_work %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(elisa_with_zone) == 0) {
        return(empty_result)
      }

      # ========================================================================
      # STANDARDIZED ELISA PE COUNTING (matches Sample Overview logic)
      # Uses utils_standardized_counting.R for consistent classification
      # ========================================================================

      # First, deduplicate samples to get one result per sample
      sample_id_col <- if ("code_barres_kps" %in% names(elisa_with_zone)) "code_barres_kps"
                       else if ("barcode" %in% names(elisa_with_zone)) "barcode"
                       else if ("sample_id" %in% names(elisa_with_zone)) "sample_id"
                       else NULL

      # Classify using standardized function
      status_col <- intersect(c("status_final", "status_raw", "result"), names(elisa_with_zone))
      has_sample_positive <- "sample_positive" %in% names(elisa_with_zone)

      if (length(status_col) > 0) {
        elisa_with_zone$std_status <- std_classify_elisa(status_final = elisa_with_zone[[status_col[1]]])
      } else if ("PP_percent" %in% names(elisa_with_zone) || "DOD" %in% names(elisa_with_zone)) {
        elisa_with_zone$std_status <- std_classify_elisa(
          pp_percent = if ("PP_percent" %in% names(elisa_with_zone)) elisa_with_zone$PP_percent else NULL,
          dod = if ("DOD" %in% names(elisa_with_zone)) elisa_with_zone$DOD else NULL
        )
      } else if (has_sample_positive) {
        elisa_with_zone$std_status <- dplyr::if_else(
          as.logical(elisa_with_zone$sample_positive), "Positive", "Negative"
        )
      } else {
        return(empty_result)
      }

      # Deduplicate if sample ID is available
      if (!is.null(sample_id_col)) {
        elisa_with_zone <- deduplicate_test_results(elisa_with_zone, sample_id_col, "std_status")
      }

      # Aggregate by health zone
      elisa_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone)
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          elisa_pe_pos = sum(std_status == "Positive", na.rm = TRUE),
          elisa_pe_neg = sum(std_status == "Negative", na.rm = TRUE),
          elisa_pe_border = sum(std_status == "Borderline", na.rm = TRUE),
          elisa_pe_invalid = sum(std_status == "Invalid", na.rm = TRUE),
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

      # Check if ELISA data already has health zone from coordinator linkage
      elisa_work <- elisa
      has_health_zone <- FALSE

      # Check for HealthZone (from coordinator linkage) or health_zone
      if ("HealthZone" %in% names(elisa_work) && any(!is.na(elisa_work$HealthZone))) {
        elisa_work$health_zone <- elisa_work$HealthZone
        has_health_zone <- TRUE
      } else if ("health_zone" %in% names(elisa_work) && any(!is.na(elisa_work$health_zone))) {
        has_health_zone <- TRUE
      }

      # If no health zone in ELISA data, try to join with biobank
      if (!has_health_zone) {
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

        elisa_work <- elisa_joined
      }

      # Filter to rows with health zone
      elisa_with_zone <- elisa_work %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(elisa_with_zone) == 0) {
        return(empty_result)
      }

      # ========================================================================
      # STANDARDIZED ELISA VSG COUNTING (matches Sample Overview logic)
      # Uses utils_standardized_counting.R for consistent classification
      # ========================================================================

      # First, deduplicate samples to get one result per sample
      sample_id_col <- if ("code_barres_kps" %in% names(elisa_with_zone)) "code_barres_kps"
                       else if ("barcode" %in% names(elisa_with_zone)) "barcode"
                       else if ("sample_id" %in% names(elisa_with_zone)) "sample_id"
                       else NULL

      # Classify using standardized function
      status_col <- intersect(c("status_final", "status_raw", "result"), names(elisa_with_zone))
      has_sample_positive <- "sample_positive" %in% names(elisa_with_zone)

      if (length(status_col) > 0) {
        elisa_with_zone$std_status <- std_classify_elisa(status_final = elisa_with_zone[[status_col[1]]])
      } else if ("PP_percent" %in% names(elisa_with_zone) || "DOD" %in% names(elisa_with_zone)) {
        elisa_with_zone$std_status <- std_classify_elisa(
          pp_percent = if ("PP_percent" %in% names(elisa_with_zone)) elisa_with_zone$PP_percent else NULL,
          dod = if ("DOD" %in% names(elisa_with_zone)) elisa_with_zone$DOD else NULL
        )
      } else if (has_sample_positive) {
        elisa_with_zone$std_status <- dplyr::if_else(
          as.logical(elisa_with_zone$sample_positive), "Positive", "Negative"
        )
      } else {
        return(empty_result)
      }

      # Deduplicate if sample ID is available
      if (!is.null(sample_id_col)) {
        elisa_with_zone <- deduplicate_test_results(elisa_with_zone, sample_id_col, "std_status")
      }

      # Aggregate by health zone
      elisa_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone)
        ) %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          elisa_vsg_pos = sum(std_status == "Positive", na.rm = TRUE),
          elisa_vsg_neg = sum(std_status == "Negative", na.rm = TRUE),
          elisa_vsg_border = sum(std_status == "Borderline", na.rm = TRUE),
          elisa_vsg_invalid = sum(std_status == "Invalid", na.rm = TRUE),
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
        ielisa_l13_border = integer(),
        ielisa_l13_neg = integer(),
        ielisa_l15_pos = integer(),
        ielisa_l15_border = integer(),
        ielisa_l15_neg = integer(),
        ielisa_any_pos = integer(),
        ielisa_any_border = integer(),
        ielisa_both_pos = integer(),
        ielisa_total = integer()
      )

      if (is.null(ielisa) || !is.data.frame(ielisa) || nrow(ielisa) == 0) {
        return(empty_result)
      }

      # Check if iELISA data already has health zone from coordinator linkage
      ielisa_work <- ielisa
      has_health_zone <- FALSE

      # Check for HealthZone (from coordinator linkage) or health_zone
      if ("HealthZone" %in% names(ielisa_work) && any(!is.na(ielisa_work$HealthZone))) {
        ielisa_work$health_zone <- ielisa_work$HealthZone
        has_health_zone <- TRUE
      } else if ("health_zone" %in% names(ielisa_work) && any(!is.na(ielisa_work$health_zone))) {
        has_health_zone <- TRUE
      }

      # If no health zone in iELISA data, try to join with biobank
      if (!has_health_zone) {
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

        ielisa_work <- ielisa_joined
      }

      # Filter to rows with health zone
      ielisa_with_zone <- ielisa_work %>%
        dplyr::filter(!is.na(health_zone))

      if (nrow(ielisa_with_zone) == 0) {
        return(empty_result)
      }

      # ========================================================================
      # STANDARDIZED iELISA COUNTING (matches Sample Overview logic)
      # Uses utils_standardized_counting.R for consistent classification
      # Thresholds: >=30% = Positive, 25-30% = Borderline, <25% = Negative
      # ========================================================================

      # First, deduplicate samples to get one result per sample
      sample_id_col <- if ("code_barres_kps" %in% names(ielisa_with_zone)) "code_barres_kps"
                       else if ("barcode" %in% names(ielisa_with_zone)) "barcode"
                       else if ("sample_id" %in% names(ielisa_with_zone)) "sample_id"
                       else NULL

      # Get inhibition values for classification
      ielisa_results <- ielisa_with_zone %>%
        dplyr::mutate(
          health_zone_norm = normalize_zone_name(health_zone),
          # Get inhibition values (prefer F1, fallback to F2)
          inh_L13 = dplyr::coalesce(
            if ("pct_inh_f1_13" %in% names(.)) pct_inh_f1_13 else NA_real_,
            if ("pct_inh_f2_13" %in% names(.)) pct_inh_f2_13 else NA_real_
          ),
          inh_L15 = dplyr::coalesce(
            if ("pct_inh_f1_15" %in% names(.)) pct_inh_f1_15 else NA_real_,
            if ("pct_inh_f2_15" %in% names(.)) pct_inh_f2_15 else NA_real_
          ),
          # L1.3 classification using standardized thresholds
          l13_status = std_classify_ielisa(inh_L13, positive_threshold = 30, borderline_threshold = 25),
          l13_pos = l13_status == "Positive",
          l13_border = l13_status == "Borderline",
          # Override with boolean if available
          l13_pos = dplyr::if_else(
            !is.na(l13_pos), l13_pos,
            if ("positive_L13" %in% names(.)) positive_L13 == TRUE else FALSE
          ),
          # L1.5 classification using standardized thresholds
          l15_status = std_classify_ielisa(inh_L15, positive_threshold = 30, borderline_threshold = 25),
          l15_pos = l15_status == "Positive",
          l15_border = l15_status == "Borderline",
          # Override with boolean if available
          l15_pos = dplyr::if_else(
            !is.na(l15_pos), l15_pos,
            if ("positive_L15" %in% names(.)) positive_L15 == TRUE else FALSE
          ),
          # Combined status
          any_pos = l13_pos | l15_pos,
          both_pos = l13_pos & l15_pos,
          any_border = l13_border | l15_border
        )

      # Replace NA with FALSE for logical operations
      ielisa_results$l13_pos[is.na(ielisa_results$l13_pos)] <- FALSE
      ielisa_results$l15_pos[is.na(ielisa_results$l15_pos)] <- FALSE
      ielisa_results$l13_border[is.na(ielisa_results$l13_border)] <- FALSE
      ielisa_results$l15_border[is.na(ielisa_results$l15_border)] <- FALSE
      ielisa_results$any_pos[is.na(ielisa_results$any_pos)] <- FALSE
      ielisa_results$both_pos[is.na(ielisa_results$both_pos)] <- FALSE
      ielisa_results$any_border[is.na(ielisa_results$any_border)] <- FALSE

      # Deduplicate if sample ID is available
      if (!is.null(sample_id_col)) {
        ielisa_results <- ielisa_results %>%
          dplyr::mutate(std_status = dplyr::case_when(
            any_pos ~ "Positive",
            any_border ~ "Borderline",
            TRUE ~ "Negative"
          ))
        ielisa_results <- deduplicate_test_results(ielisa_results, sample_id_col, "std_status")
      }

      ielisa_results %>%
        dplyr::group_by(health_zone_norm) %>%
        dplyr::summarise(
          ielisa_l13_pos = sum(l13_pos, na.rm = TRUE),
          ielisa_l13_border = sum(l13_border, na.rm = TRUE),
          ielisa_l13_neg = sum(!l13_pos & !l13_border, na.rm = TRUE),
          ielisa_l15_pos = sum(l15_pos, na.rm = TRUE),
          ielisa_l15_border = sum(l15_border, na.rm = TRUE),
          ielisa_l15_neg = sum(!l15_pos & !l15_border, na.rm = TRUE),
          ielisa_any_pos = sum(any_pos, na.rm = TRUE),
          ielisa_any_border = sum(any_border & !any_pos, na.rm = TRUE),  # Borderline but not positive
          ielisa_both_pos = sum(both_pos, na.rm = TRUE),
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
      format_count_with_denominator(pos, total, format_style = "full")
    })

    output$mic_rna_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$mic_total, na.rm = TRUE)
      pos <- sum(data$mic_rna_pos, na.rm = TRUE)
      format_count_with_denominator(pos, total, format_style = "full")
    })

    output$elisa_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$elisa_total, na.rm = TRUE)
      pos <- sum(data$elisa_pos, na.rm = TRUE)
      format_count_with_denominator(pos, total, format_style = "full")
    })

    output$ielisa_positive <- renderText({
      data <- combined_geo_data()
      if (nrow(data) == 0) return("0")
      total <- sum(data$ielisa_total, na.rm = TRUE)
      pos <- sum(data$ielisa_any_pos, na.rm = TRUE)
      format_count_with_denominator(pos, total, format_style = "full")
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

      # Build base map with polygons
      base_map <- leaflet::leaflet(map) %>%
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(maxZoom = 18)
        ) %>%
        leaflet::addPolygons(
          layerId = ~zone_norm,
          group = "zones",
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
          label = ~zonesante,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-family" = "Inter, sans-serif",
              "font-size" = "14px",
              "font-weight" = "bold",
              "padding" = "4px 8px",
              "border-radius" = "4px"
            ),
            textsize = "14px",
            direction = "auto"
          )
        ) %>%
        leaflet::addLegend(
          pal = pal,
          values = ~display_value,
          opacity = 0.8,
          title = legend_title,
          position = "bottomright"
        )

      # NOTE: Markers are added via leafletProxy in separate observers below
      # This prevents full map re-render when only markers change

      base_map %>%
        leaflet::setView(lng = 23.8, lat = -6.0, zoom = 8)
    })

    # ========================================================================
    # MARKER UPDATES VIA LEAFLET PROXY (prevents blinking)
    # ========================================================================

    # Debounced reactive for mobile units to prevent rapid updates
    mobile_units_debounced <- reactive({
      mobile_units_data()
    }) %>% shiny::debounce(300)

    # Update health structure markers
    observeEvent(list(input$show_structures), {
      show_structures <- input$show_structures %||% TRUE

      proxy <- leaflet::leafletProxy("main_map")

      # Clear existing structure markers
      proxy <- proxy %>% leaflet::clearGroup("structures")

      if (show_structures && nrow(health_structures_data) > 0) {
        # Color palette for endemicity levels
        endemicity_colors <- c("A" = "red", "B" = "orange", "C" = "green")

        # Add row index for layerId
        struct_data <- health_structures_data
        struct_data$structure_id <- paste0("struct_", seq_len(nrow(struct_data)))

        # Create hospital icons with endemicity colors
        hospital_icons <- leaflet::awesomeIcons(
          icon = "hospital",
          iconColor = "white",
          library = "fa",
          markerColor = endemicity_colors[struct_data$endemicite]
        )

        proxy %>%
          leaflet::addAwesomeMarkers(
            data = struct_data,
            layerId = ~structure_id,
            group = "structures",
            lng = ~longitude,
            lat = ~latitude,
            icon = hospital_icons,
            label = ~paste0(structure, " (", zone_sante, ")"),
            labelOptions = leaflet::labelOptions(
              style = list(
                "font-family" = "Inter, sans-serif",
                "font-size" = "12px",
                "font-weight" = "bold",
                "padding" = "6px 10px",
                "background-color" = "white",
                "border" = "2px solid #333"
              )
            )
          )
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # Update mobile unit markers (debounced to prevent rapid re-renders)
    observeEvent(list(input$show_mobile_units, mobile_units_debounced()), {
      show_mobile <- input$show_mobile_units %||% TRUE
      mobile_data <- mobile_units_debounced()

      proxy <- leaflet::leafletProxy("main_map")

      # Clear existing mobile unit markers
      proxy <- proxy %>% leaflet::clearGroup("mobile_units")

      if (show_mobile && !is.null(mobile_data) && nrow(mobile_data) > 0) {
        # Filter to only those with valid coordinates
        mobile_with_coords <- mobile_data %>%
          dplyr::filter(!is.na(latitude) & !is.na(longitude))

        if (nrow(mobile_with_coords) > 0) {
          # Add row index for layerId
          mobile_with_coords$mobile_id <- paste0("mobile_", seq_len(nrow(mobile_with_coords)))

          # Create truck-medical icons for mobile units
          mobile_icons <- leaflet::awesomeIcons(
            icon = "truck-medical",
            iconColor = "white",
            library = "fa",
            markerColor = "blue"
          )

          proxy %>%
            leaflet::addAwesomeMarkers(
              data = mobile_with_coords,
              layerId = ~mobile_id,
              group = "mobile_units",
              lng = ~longitude,
              lat = ~latitude,
              icon = mobile_icons,
              label = ~paste0(structure_name, " (", health_zone_norm, ") - ", n_samples, " samples"),
              labelOptions = leaflet::labelOptions(
                style = list(
                  "font-family" = "Inter, sans-serif",
                  "font-size" = "12px",
                  "font-weight" = "bold",
                  "padding" = "6px 10px",
                  "background-color" = "#E0F2FE",
                  "border" = "2px solid #0284C7"
                )
              )
            )
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # ========================================================================
    # MAP CLICK OBSERVERS
    # ========================================================================

    # Observe clicks on health zones (polygons)
    observeEvent(input$main_map_shape_click, {
      click <- input$main_map_shape_click
      if (!is.null(click) && !is.null(click$id)) {
        # Zone polygon click
        selected_zone(click$id)
        selected_structure(NULL)
        selected_mobile_unit(NULL)
      }
    })

    # Observe clicks on structure markers (CircleMarkers) and mobile unit markers
    observeEvent(input$main_map_marker_click, {
      click <- input$main_map_marker_click
      if (!is.null(click) && !is.null(click$id)) {
        if (grepl("^struct_", click$id)) {
          # Structure click - extract index
          idx <- as.integer(gsub("struct_", "", click$id))
          if (idx > 0 && idx <= nrow(health_structures_data)) {
            selected_structure(health_structures_data[idx, ])
            selected_zone(NULL)
            selected_mobile_unit(NULL)
          }
        } else if (grepl("^mobile_", click$id)) {
          # Mobile unit click - extract index
          idx <- as.integer(gsub("mobile_", "", click$id))
          mobile_data <- mobile_units_data()
          # Filter to get the same data used for markers
          mobile_with_coords <- mobile_data %>%
            dplyr::filter(!is.na(latitude) & !is.na(longitude))
          if (idx > 0 && idx <= nrow(mobile_with_coords)) {
            selected_mobile_unit(mobile_with_coords[idx, ])
            selected_zone(NULL)
            selected_structure(NULL)
          }
        }
      }
    })

    # ========================================================================
    # ZONE DETAILS PANEL OUTPUT
    # ========================================================================

    output$zone_details_panel <- renderUI({
      zone_name <- selected_zone()
      structure_info <- selected_structure()
      mobile_unit_info <- selected_mobile_unit()

      # If mobile unit is selected, show mobile unit details
      if (!is.null(mobile_unit_info) && is.data.frame(mobile_unit_info) && nrow(mobile_unit_info) > 0) {
        return(
          div(
            class = "row",
            div(
              class = "col-md-4",
              h5(
                icon("truck-medical", style = "color: #0284C7;"),
                " ",
                mobile_unit_info$structure_name[1]
              ),
              tags$table(
                class = "table table-sm table-borderless mb-0",
                tags$tr(tags$td(tags$strong("Zone de Sant\u00e9:")), tags$td(mobile_unit_info$health_zone_norm[1])),
                tags$tr(tags$td(tags$strong("Province:")), tags$td(mobile_unit_info$province[1])),
                tags$tr(
                  tags$td(tags$strong("Type:")),
                  tags$td(
                    span(
                      style = "color: #0284C7; font-weight: bold;",
                      icon("truck-medical", style = "margin-right: 4px;"),
                      "Unit\u00e9 Mobile"
                    )
                  )
                ),
                tags$tr(
                  tags$td(tags$strong("Samples:")),
                  tags$td(
                    tags$strong(
                      style = "color: #0284C7;",
                      scales::comma(mobile_unit_info$n_samples[1])
                    )
                  )
                )
              )
            ),
            div(
              class = "col-md-8",
              div(
                style = "background: #E0F2FE; border-left: 3px solid #0284C7; padding: 10px; border-radius: 4px;",
                icon("info-circle", style = "color: #0284C7; margin-right: 8px;"),
                "Mobile units (Unit\u00e9s Mobiles) are identified by the prefix 'UM' in their name. ",
                "They collect samples across multiple locations within their assigned health zone."
              )
            )
          )
        )
      }

      # If structure is selected, show structure details
      if (!is.null(structure_info) && is.data.frame(structure_info) && nrow(structure_info) > 0) {
        endemicity_colors <- c("A" = "#DC2626", "B" = "#F59E0B", "C" = "#10B981")
        endemicity_labels <- c("A" = "Haute", "B" = "Moyenne", "C" = "Basse")
        end_level <- structure_info$endemicite[1]

        return(
          div(
            class = "row",
            div(
              class = "col-md-4",
              h5(icon("hospital"), " ", structure_info$structure[1]),
              tags$table(
                class = "table table-sm table-borderless mb-0",
                tags$tr(tags$td(tags$strong("Zone de Sant\u00e9:")), tags$td(structure_info$zone_sante[1])),
                tags$tr(tags$td(tags$strong("Province:")), tags$td(structure_info$province[1])),
                tags$tr(
                  tags$td(tags$strong("End\u00e9micit\u00e9:")),
                  tags$td(
                    span(
                      style = paste0("color: ", endemicity_colors[end_level], "; font-weight: bold;"),
                      paste0(end_level, " - ", endemicity_labels[end_level])
                    )
                  )
                ),
                tags$tr(tags$td(tags$strong("Coordinates:")), tags$td(sprintf("%.4f, %.4f", structure_info$latitude[1], structure_info$longitude[1])))
              )
            ),
            div(
              class = "col-md-8",
              p(class = "text-muted mb-0", icon("info-circle"), " Click on a health zone polygon to see sample statistics.")
            )
          )
        )
      }

      # If zone is selected, show zone details
      if (!is.null(zone_name) && nzchar(zone_name)) {
        geo_data <- combined_geo_data()

        if (nrow(geo_data) > 0) {
          # Match by health_zone_norm (normalized name)
          zone_data <- geo_data %>% dplyr::filter(health_zone_norm == zone_name)

          if (nrow(zone_data) > 0) {
            zd <- zone_data[1, ]

            # Look up province from map data
            map <- map_data()
            if (!is.null(map) && "province" %in% names(map)) {
              map_zone <- map %>%
                sf::st_drop_geometry() %>%
                dplyr::filter(normalize_zone_name(zonesante) == zone_name) %>%
                dplyr::slice(1)
              zd$province <- if (nrow(map_zone) > 0) map_zone$province else NA_character_
            } else {
              zd$province <- NA_character_
            }

            # Find all health structures in this zone (from hardcoded data)
            zone_search_pattern <- paste0("ZS ", zone_name)
            all_zone_structures <- health_structures_data %>%
              dplyr::filter(zone_sante == zone_search_pattern)

            # Get biobank structures for this zone (including mobile units)
            biobank_structs <- biobank_structures_by_zone()
            zone_biobank_structs <- biobank_structs %>%
              dplyr::filter(health_zone_norm == zone_name)

            # Filter hardcoded structures to only show those with samples in filtered data
            # This ensures Zone Details respects the global structure filter
            biobank_struct_names <- toupper(trimws(zone_biobank_structs$structure_name))
            zone_structures <- all_zone_structures %>%
              dplyr::filter(toupper(trimws(structure)) %in% biobank_struct_names)

            # Get mobile units for this zone
            zone_mobile_units <- zone_biobank_structs %>%
              dplyr::filter(is_mobile_unit == TRUE)

            # Get other biobank structures (not mobile units and not in hardcoded list)
            hardcoded_names <- toupper(trimws(zone_structures$structure))
            zone_other_structs <- zone_biobank_structs %>%
              dplyr::filter(is_mobile_unit == FALSE) %>%
              dplyr::filter(!toupper(trimws(structure_name)) %in% hardcoded_names)

            # Endemicity styling
            endemicity_colors <- c("A" = "#DC2626", "B" = "#F59E0B", "C" = "#10B981")
            endemicity_labels <- c("A" = "Haute", "B" = "Moyenne", "C" = "Basse")
            endemicity_icons <- c("A" = "exclamation-triangle", "B" = "exclamation-circle", "C" = "check-circle")
            endemicity_bg <- c("A" = "rgba(220, 38, 38, 0.1)", "B" = "rgba(245, 158, 11, 0.1)", "C" = "rgba(16, 185, 129, 0.1)")

            # Build health structures UI from hardcoded data
            structures_ui <- if (nrow(zone_structures) > 0) {
              structure_items <- lapply(seq_len(nrow(zone_structures)), function(i) {
                s <- zone_structures[i, ]
                end_color <- endemicity_colors[s$endemicite]
                end_bg <- endemicity_bg[s$endemicite]

                # Determine structure type icon
                struct_icon <- if (grepl("^HGR", s$structure)) {
                  "hospital"
                } else if (grepl("^CS", s$structure)) {
                  "clinic-medical"
                } else if (grepl("^HS", s$structure)) {
                  "house-medical"
                } else {
                  "plus-square"
                }

                # Get sample count from biobank data by matching structure name
                struct_name_upper <- toupper(trimws(s$structure))
                matching_biobank <- zone_biobank_structs %>%
                  dplyr::filter(toupper(trimws(structure_name)) == struct_name_upper)
                sample_count <- if (nrow(matching_biobank) > 0) {
                  sum(matching_biobank$n_samples)
                } else {
                  0
                }

                div(
                  style = paste0(
                    "background: ", end_bg, "; ",
                    "border-left: 3px solid ", end_color, "; ",
                    "padding: 6px 10px; margin-bottom: 6px; border-radius: 4px;"
                  ),
                  div(
                    style = "display: flex; align-items: center; justify-content: space-between;",
                    div(
                      icon(struct_icon, style = paste0("color: ", end_color, "; margin-right: 8px;")),
                      tags$span(style = "font-weight: 500;", s$structure)
                    ),
                    div(
                      style = paste0("color: ", end_color, "; font-size: 0.85em;"),
                      icon("vial", style = "margin-right: 4px;"),
                      tags$span(paste0(sample_count, " samples"))
                    )
                  )
                )
              })
              div(structure_items)
            } else {
              NULL
            }

            # Build other biobank structures UI (not in hardcoded list)
            other_structs_ui <- if (nrow(zone_other_structs) > 0) {
              other_items <- lapply(seq_len(nrow(zone_other_structs)), function(i) {
                s <- zone_other_structs[i, ]

                # Determine structure type icon
                struct_icon <- if (grepl("^HGR", s$structure_name)) {
                  "hospital"
                } else if (grepl("^CS", s$structure_name)) {
                  "clinic-medical"
                } else if (grepl("^HS", s$structure_name)) {
                  "house-medical"
                } else {
                  "building"
                }

                div(
                  style = paste0(
                    "background: rgba(107, 114, 128, 0.1); ",
                    "border-left: 3px solid #6B7280; ",
                    "padding: 6px 10px; margin-bottom: 6px; border-radius: 4px;"
                  ),
                  div(
                    style = "display: flex; align-items: center; justify-content: space-between;",
                    div(
                      icon(struct_icon, style = "color: #6B7280; margin-right: 8px;"),
                      tags$span(style = "font-weight: 500;", s$structure_name)
                    ),
                    div(
                      style = "color: #6B7280; font-size: 0.85em;",
                      tags$span(paste0(s$n_samples, " samples"))
                    )
                  )
                )
              })
              div(other_items)
            } else {
              NULL
            }

            # Build mobile units UI
            mobile_units_ui <- if (nrow(zone_mobile_units) > 0) {
              mobile_items <- lapply(seq_len(nrow(zone_mobile_units)), function(i) {
                m <- zone_mobile_units[i, ]
                div(
                  style = paste0(
                    "background: rgba(2, 132, 199, 0.1); ",
                    "border-left: 3px solid #0284C7; ",
                    "padding: 6px 10px; margin-bottom: 6px; border-radius: 4px;"
                  ),
                  div(
                    style = "display: flex; align-items: center; justify-content: space-between;",
                    div(
                      icon("truck-medical", style = "color: #0284C7; margin-right: 8px;"),
                      tags$span(style = "font-weight: 500;", m$structure_name)
                    ),
                    div(
                      style = "color: #0284C7; font-size: 0.85em;",
                      icon("vial", style = "margin-right: 4px;"),
                      tags$span(paste0(m$n_samples, " samples"))
                    )
                  )
                )
              })
              div(mobile_items)
            } else {
              NULL
            }

            # Check if we have any structures to display
            has_any_structures <- nrow(zone_structures) > 0 || nrow(zone_other_structs) > 0 || nrow(zone_mobile_units) > 0

            # Combine all structure types or show empty message
            combined_structures_ui <- if (has_any_structures) {
              div(
                structures_ui,
                other_structs_ui,
                mobile_units_ui
              )
            } else {
              div(
                class = "text-muted",
                style = "font-style: italic; padding: 8px;",
                icon("info-circle", class = "me-1"),
                "No health structures registered in this zone"
              )
            }

            return(
              div(
                # First row: Zone info and test results
                div(
                  class = "row",
                  div(
                    class = "col-md-3",
                    h5(icon("map-marker-alt"), " ", zone_name),
                    tags$table(
                      class = "table table-sm table-borderless mb-0",
                      tags$tr(tags$td(tags$strong("Province:")), tags$td(zd$province)),
                      tags$tr(tags$td(tags$strong("Total Samples:")), tags$td(tags$strong(scales::comma(zd$n_samples))))
                    )
                  ),
                  div(
                    class = "col-md-4",
                    h6(icon("dna"), " Molecular (MIC)"),
                    tags$table(
                      class = "table table-sm table-borderless mb-0",
                      tags$tr(tags$td("DNA+ (177T):"), tags$td(zd$mic_dna_pos)),
                      tags$tr(tags$td("RNA+ (18S2):"), tags$td(zd$mic_rna_pos)),
                      tags$tr(tags$td("TNA+ (DNA+RNA):"), tags$td(zd$mic_tna_pos)),
                      tags$tr(tags$td("Any positive:"), tags$td(zd$mic_any_pos)),
                      tags$tr(tags$td("Total tested:"), tags$td(zd$mic_total)),
                      tags$tr(tags$td(tags$strong("Rate:")), tags$td(sprintf("%.1f%%", zd$molecular_rate)))
                    )
                  ),
                  div(
                    class = "col-md-5",
                    h6(icon("flask"), " Serological"),
                    div(
                      class = "row",
                      div(
                        class = "col-6",
                        tags$table(
                          class = "table table-sm table-borderless mb-0",
                          tags$tr(tags$td("ELISA-PE+:"), tags$td(paste0(zd$elisa_pe_pos, " / ", zd$elisa_pe_total))),
                          tags$tr(tags$td("ELISA-VSG+:"), tags$td(paste0(zd$elisa_vsg_pos, " / ", zd$elisa_vsg_total)))
                        )
                      ),
                      div(
                        class = "col-6",
                        tags$table(
                          class = "table table-sm table-borderless mb-0",
                          tags$tr(tags$td("iELISA L1.3+:"), tags$td(zd$ielisa_l13_pos)),
                          tags$tr(tags$td("iELISA L1.5+:"), tags$td(zd$ielisa_l15_pos)),
                          tags$tr(tags$td(tags$strong("Sero Rate:")), tags$td(sprintf("%.1f%%", zd$sero_rate)))
                        )
                      )
                    )
                  )
                ),
                # Second row: Health Structures and Mobile Units in this zone
                tags$hr(style = "margin: 12px 0 10px 0;"),
                div(
                  h6(
                    icon("hospital-alt", style = "color: #6366F1;"),
                    " Structures Sanitaires ",
                    if (nrow(zone_mobile_units) > 0) {
                      span(
                        icon("truck-medical", style = "color: #0284C7; margin-left: 8px;"),
                        " & Unit\u00e9s Mobiles"
                      )
                    },
                    " in ", tags$strong(zone_name),
                    if (has_any_structures) {
                      tags$span(
                        class = "badge bg-secondary ms-2",
                        nrow(zone_structures) + nrow(zone_other_structs) + nrow(zone_mobile_units)
                      )
                    }
                  ),
                  div(
                    style = "display: flex; flex-wrap: wrap; gap: 8px; margin-top: 8px;",
                    combined_structures_ui
                  )
                )
              )
            )
          }
        }
      }

      # Default message when nothing selected
      div(
        class = "text-center text-muted py-2",
        icon("mouse-pointer", class = "me-2"),
        "Click on a health zone or structure marker on the map to view details here."
      )
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
