# R/modules/mod_data_manager.R
# Complete data management module - handles loading, cleaning, filtering
# ============================================================================

#' Data Manager UI
#' @param id Module namespace ID
#' @export
mod_data_manager_ui <- function(id) {
  ns <- NS(id)
  
  bslib::sidebar(
    width = 280,

    # Site Information Section
    if (!is.null(config$sites) && !is.null(config$current_site)) {
      tagList(
        div(
          class = "alert alert-info mb-3",
          style = "padding: 8px 12px; margin-bottom: 12px;",
          tags$strong(icon("hospital"), " Current Site:"),
          tags$br(),
          tags$small(config$sites[[config$current_site]]$short_name),
          tags$br(),
          tags$small(
            class = "text-muted",
            config$sites[[config$current_site]]$location
          )
        ),
        hr()
      )
    },

    # Data Loading Section
    h5(icon("folder-open"), " Data Source"),
    textInput(ns("data_dir"), "Directory",
              value = if (!is.null(config$site_paths)) config$site_paths$biobank_dir else config$paths$biobank_dir),
    uiOutput(ns("file_selector")),
    actionButton(ns("load_data"), "Load Data", 
                 icon = icon("upload"),
                 class = "btn-primary w-100 mb-3"),
    
    hr(),
    
    # Filters Section
    h5(icon("filter"), " Filters"),
    dateRangeInput(
      ns("date_range"),
      "Sample Date",
      start = as.Date("2025-04-01"),
      end = Sys.Date()
    ),
    selectInput(ns("filter_study"), "Study",
                choices = c("All" = "all")),
    selectInput(ns("filter_province"), "Province",
                choices = c("All" = "all")),
    selectInput(ns("filter_zone"), "Health Zone",
                choices = c("All" = "all")),
    selectInput(ns("filter_structure"), "Structure Sanitaire",
                choices = c("All" = "all")),
    
    hr(),
    
    # Status Section
    h5(icon("info-circle"), " Status"),
    uiOutput(ns("data_status"))
  )
}

#' Data Manager Server
#' @param id Module namespace ID
#' @export
mod_data_manager_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # ========================================================================
    # REACTIVE VALUES
    # ========================================================================
    
    rv <- reactiveValues(
      raw_data = NULL,
      clean_data = NULL,
      extraction_data = NULL,
      quality_report = NULL,
      files_available = character(0)
    )
    
    # ========================================================================
    # FILE DISCOVERY
    # ========================================================================
    
    # Scan directory for files
    observe({
      req(input$data_dir)
      
      if (dir.exists(input$data_dir)) {
        rv$files_available <- list_biobank_files(input$data_dir)
      } else {
        rv$files_available <- character(0)
      }
    })
    
    # File selector UI
    output$file_selector <- renderUI({
      ns <- session$ns
      
      files <- rv$files_available
      
      if (length(files) == 0) {
        div(
          class = "alert alert-warning",
          icon("exclamation-triangle"),
          " No Excel files found in directory"
        )
      } else {
        selectInput(
          ns("selected_file"),
          "Select File",
          choices = setNames(files, basename(files))
        )
      }
    })
    
    # ========================================================================
    # DATA LOADING
    # ========================================================================
    
    observeEvent(input$load_data, {
      req(input$selected_file)
      
      # Show loading notification
      loading_id <- showNotification(
        "Loading data...",
        duration = NULL,
        type = "message"
      )
      
      tryCatch({
        # Step 1: Load raw data
        df_raw <- load_biobank_file(input$selected_file)
        rv$raw_data <- df_raw
        
        # Step 2: Analyze quality BEFORE cleaning
        quality <- analyze_data_quality(df_raw)
        
        # Step 3: Clean data using the improved cleaner
        df_clean <- clean_biobank_data_improved(df_raw)
        rv$clean_data <- df_clean

        # Step 3b: Load extraction quality data and link to biobank
        extraction_df <- tryCatch({
          load_all_extractions()  # Uses site-aware paths by default
        }, error = function(e) {
          message("Failed to load extraction dataset: ", e$message)
          tibble::tibble()
        })

        rv$extraction_data <- tryCatch({
          link_extraction_to_biobank(extraction_df, df_clean)
        }, error = function(e) {
          message("Failed to link extraction dataset: ", e$message)
          extraction_df
        })
        
        # Step 4: Re-analyze quality AFTER cleaning
        quality_clean <- analyze_data_quality(df_clean)
        
        # Combine both reports
        flags_weekly <- NULL
        if (is.data.frame(quality$row_flags_detailed) &&
            all(c("date_sample", "reason") %in% names(quality$row_flags_detailed))) {
          flags_weekly <- quality$row_flags_detailed %>%
            dplyr::mutate(
              date_sample = suppressWarnings(lubridate::as_date(date_sample)),
              week = lubridate::floor_date(date_sample, "week"),
              quality_flag = dplyr::case_when(
                is.na(reason) | reason == "" | reason == "OK" ~ "Valid",
                reason == "Duplicate key" ~ "Duplicate",
                reason == "Barcode conflict" ~ "Barcode conflict",
                reason == "Missing barcode" ~ "Missing barcode",
                reason == "Missing lab ID" ~ "Missing lab ID",
                TRUE ~ reason
              )
            ) %>%
            dplyr::filter(!is.na(week)) %>%
            dplyr::count(week, quality_flag, name = "n") %>%
            dplyr::arrange(week, quality_flag)
        }

        rv$quality_report <- list(
          summary = c(
            quality$summary,
            rows_clean = nrow(df_clean),
            rows_dropped = nrow(df_raw) - nrow(df_clean),
            drop_rate = round((nrow(df_raw) - nrow(df_clean)) / nrow(df_raw) * 100, 1)
          ),
          missing_barcode = quality$missing_barcode,
          missing_labid = quality$missing_labid,
          duplicates = quality$duplicates,
          barcode_conflicts = quality$barcode_conflicts,
          completeness = quality_clean$completeness,
          quality_flags = if (is.data.frame(quality$row_flags_detailed)) {
            quality$row_flags_detailed %>%
              dplyr::mutate(
                flag = dplyr::case_when(
                  reason == "OK" ~ "Valid",
                  TRUE ~ "Invalid"
                )
              ) %>%
              dplyr::count(flag, name = "n") %>%
              dplyr::arrange(dplyr::desc(n)) %>%
              dplyr::rename(quality_flag = flag)
          } else {
            df_clean %>%
              dplyr::count(quality_flag) %>%
              dplyr::arrange(dplyr::desc(n))
          },
          quality_flags_by_week = flags_weekly,
          row_flags = quality$row_flags,
          row_flags_detailed = quality$row_flags_detailed
        )
        
        # Step 5: Update filter choices
        update_filter_choices(session, df_clean)
        
        # Success notification
        removeNotification(loading_id)
        showNotification(
          sprintf("Loaded %d rows (cleaned to %d valid rows)",
                  nrow(df_raw), nrow(df_clean)),
          type = "message",
          duration = 5
        )
        
      }, error = function(e) {
        removeNotification(loading_id)
        showNotification(
          paste("Error loading data:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
    
    # ========================================================================
    # DATA FILTERING
    # ========================================================================
    
    sanitize_date_range <- function(x) {
      if (is.null(x) || length(x) != 2) {
        return(NULL)
      }

      dates <- tryCatch(
        suppressWarnings(as.Date(x)),
        warning = function(w) rep(NA, length(x)),
        error = function(e) rep(NA, length(x))
      )

      if (any(is.na(dates))) {
        return(NULL)
      }

      dates
    }

    normalize_filter_choice <- function(x) {
      if (is.null(x) || !length(x)) {
        return("all")
      }
      x
    }

    filtered_data <- reactive({
      req(rv$clean_data)

      apply_filters(
        rv$clean_data,
        date_range = input$date_range,
        study = input$filter_study,
        province = input$filter_province,
        zone = input$filter_zone,
        structure = input$filter_structure
      )
    })

    filters <- reactive({
      list(
        date_range = sanitize_date_range(input$date_range),
        province = normalize_filter_choice(input$filter_province),
        zone = normalize_filter_choice(input$filter_zone),
        structure = normalize_filter_choice(input$filter_structure),
        cohort = normalize_filter_choice(input$filter_study)
      )
    })

    filtered_for_choices <- function(exclude = character()) {
      df <- rv$clean_data

      if (is.null(df) || !nrow(df)) {
        return(df)
      }

      if (!"study" %in% exclude &&
          !is.null(input$filter_study) &&
          input$filter_study != "all" &&
          "study" %in% names(df)) {
        df <- df %>%
          dplyr::filter(.data$study == !!input$filter_study)
      }

      if (!"province" %in% exclude &&
          !is.null(input$filter_province) &&
          input$filter_province != "all" &&
          "province" %in% names(df)) {
        df <- df %>%
          dplyr::filter(.data$province == !!input$filter_province)
      }

      if (!"zone" %in% exclude &&
          !is.null(input$filter_zone) &&
          input$filter_zone != "all" &&
          "health_zone" %in% names(df)) {
        df <- df %>%
          dplyr::filter(.data$health_zone == !!input$filter_zone)
      }

      if (!"structure" %in% exclude &&
          !is.null(input$filter_structure) &&
          input$filter_structure != "all") {
        target <- input$filter_structure
        candidate_cols <- intersect(
          c(
            "health_structure",
            "health_facility",
            "structure_sanitaire",
            "biobank_health_facility",
            "biobank_structure_sanitaire"
          ),
          names(df)
        )

        if (length(candidate_cols) && !is.na(target)) {
          df <- df %>%
            dplyr::filter(
              dplyr::if_any(
                dplyr::all_of(candidate_cols),
                ~ normalize_structure_value(.x) == target
              )
            )
        }
      }

      df
    }

    update_select_with_values <- function(input_id, values) {
      current_value <- input[[input_id]]
      values <- sort(unique(values[!is.na(values)]))

      choices <- c("All" = "all")

      if (length(values)) {
        choices <- c(choices, stats::setNames(values, values))
      }

      selected <- "all"

      if (!is.null(current_value)) {
        if (identical(current_value, "all")) {
          selected <- "all"
        } else if (length(values) && current_value %in% values) {
          selected <- current_value
        }
      }

      updateSelectInput(session, input_id, choices = choices, selected = selected)
    }

    observeEvent(
      list(
        rv$clean_data,
        input$filter_study,
        input$filter_province,
        input$filter_zone,
        input$filter_structure
      ),
      {
        df <- rv$clean_data

        if (is.null(df) || !nrow(df)) {
          return()
        }

        if ("study" %in% names(df)) {
          df_study <- filtered_for_choices("study")
          update_select_with_values("filter_study", df_study$study)
        }

        if ("province" %in% names(df)) {
          df_province <- filtered_for_choices("province")
          update_select_with_values("filter_province", df_province$province)
        }

        if ("health_zone" %in% names(df)) {
          df_zone <- filtered_for_choices("zone")
          update_select_with_values("filter_zone", df_zone$health_zone)
        }

        structure_df <- filtered_for_choices("structure")
        structure_choices <- build_structure_choices(structure_df)

        current_structure <- input$filter_structure
        selected_structure <- "all"

        if (!is.null(current_structure)) {
          if (identical(current_structure, "all")) {
            selected_structure <- "all"
          } else if (nrow(structure_choices) && current_structure %in% structure_choices$key) {
            selected_structure <- current_structure
          }
        }

        structure_select_choices <- c("All" = "all")

        if (nrow(structure_choices)) {
          structure_select_choices <- c(
            structure_select_choices,
            stats::setNames(structure_choices$label, structure_choices$key)
          )
        }

        updateSelectInput(
          session,
          "filter_structure",
          choices = structure_select_choices,
          selected = selected_structure
        )
      },
      ignoreNULL = FALSE
    )

    normalize_filter_value <- function(x) {
      if (is.null(x)) {
        return(rep(NA_character_, length.out = 0))
      }

      values <- stringr::str_trim(as.character(x))
      values[values %in% c("", "NA", "N/A", "NULL")] <- NA_character_
      values <- stringi::stri_trans_general(values, "Latin-ASCII")
      stringr::str_to_upper(values)
    }

    filtered_extractions <- reactive({
      df <- rv$extraction_data

      if (is.null(df)) {
        return(tibble::tibble())
      }

      if (!is.data.frame(df)) {
        df <- tibble::as_tibble(df)
      }

      if (!nrow(df)) {
        return(df)
      }

      # Date filter uses extraction date when available
      if (!is.null(input$date_range) && length(input$date_range) == 2 &&
          "extraction_date" %in% names(df)) {
        start_date <- suppressWarnings(as.Date(input$date_range[1]))
        end_date <- suppressWarnings(as.Date(input$date_range[2]))

        if (!is.na(start_date) && !is.na(end_date)) {
          df <- df %>%
            dplyr::filter(
              is.na(.data$extraction_date) |
                (.data$extraction_date >= start_date & .data$extraction_date <= end_date)
            )
        }
      }

      # Study filter (prefer linked biobank study)
      if (!is.null(input$filter_study) && input$filter_study != "all") {
        target <- normalize_filter_value(input$filter_study)

        if ("biobank_study" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$biobank_study) == target)
        } else if ("study" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$study) == target)
        }
      }

      # Province filter
      if (!is.null(input$filter_province) && input$filter_province != "all") {
        target <- normalize_filter_value(input$filter_province)

        if ("biobank_province" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$biobank_province) == target)
        } else if ("province" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$province) == target)
        }
      }

      # Health zone filter
      if (!is.null(input$filter_zone) && input$filter_zone != "all") {
        target <- normalize_filter_value(input$filter_zone)

        if ("biobank_health_zone" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$biobank_health_zone) == target)
        } else if ("health_zone" %in% names(df)) {
          df <- df %>%
            dplyr::filter(normalize_filter_value(.data$health_zone) == target)
        }
      }

      # Structure filter (fall back across known columns)
      if (!is.null(input$filter_structure) && input$filter_structure != "all") {
        target <- normalize_structure_value(input$filter_structure)
        candidate_cols <- intersect(
          c("health_structure", "biobank_health_facility", "structure_sanitaire", "biobank_structure_sanitaire"),
          names(df)
        )

        if (length(candidate_cols) && !is.na(target)) {
          df <- df %>%
            dplyr::filter(
              dplyr::if_any(
                dplyr::all_of(candidate_cols),
                ~ normalize_structure_value(.x) == target
              )
            )
        }
      }

      df
    })
    
    # ========================================================================
    # STATUS DISPLAY
    # ========================================================================
    
    output$data_status <- renderUI({
      if (is.null(rv$clean_data)) {
        div(
          class = "alert alert-info",
          icon("info-circle"),
          " No data loaded"
        )
      } else {
        total <- nrow(rv$clean_data)
        filtered <- nrow(filtered_data())
        
        tagList(
          div(
            class = "d-flex justify-content-between mb-2",
            span("Total:"),
            strong(scales::comma(total))
          ),
          div(
            class = "d-flex justify-content-between mb-2",
            span("Filtered:"),
            strong(scales::comma(filtered))
          ),
          if (filtered < total) {
            div(
              class = "text-muted small",
              sprintf("(%d%% shown)", round(filtered/total*100))
            )
          }
        )
      }
    })
    
    # ========================================================================
    # RETURN VALUES
    # ========================================================================
    
    return(list(
      raw_data = reactive(rv$raw_data),
      clean_data = reactive(rv$clean_data),
      filtered_data = filtered_data,
      extraction_data = reactive(rv$extraction_data),
      filtered_extractions = filtered_extractions,
      quality_report = reactive(rv$quality_report),
      filters = filters
    ))
  })
}
