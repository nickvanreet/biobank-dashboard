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

    # Biobank Selection Section
    h5(icon("hospital"), " Select Biobank"),
    tags$p(
      class = "text-muted small",
      "Select a biobank to load all data automatically"
    ),

    # Biobank buttons
    if (!is.null(config$sites)) {
      tagList(
        lapply(names(config$sites), function(site_id) {
          site_info <- config$sites[[site_id]]
          actionButton(
            ns(paste0("load_site_", site_id)),
            site_info$short_name,
            icon = icon("database"),
            class = "btn-primary w-100 mb-2",
            style = "text-align: left;"
          )
        })
      )
    },

    hr(),

    # Current Site Information
    uiOutput(ns("current_site_info")),

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

    # Data Quality Filter
    div(class = "mt-3",
        checkboxInput(
          ns("exclude_quality_issues"),
          label = tags$span(
            icon("shield-alt"),
            " Exclude Quality Issues"
          ),
          value = FALSE
        ),
        tags$small(
          class = "text-muted",
          "Filter out rows with conflicts or missing data"
        )
    ),
    
    hr(),

    # Status Section
    h5(icon("info-circle"), " Status"),
    uiOutput(ns("data_status")),

    hr(),

    # Cache Management Section
    h5(icon("database"), " Cache"),
    uiOutput(ns("cache_status")),
    div(
      class = "d-flex gap-2 mt-2",
      actionButton(
        ns("clear_cache"),
        "Clear Cache",
        icon = icon("trash"),
        class = "btn-outline-danger btn-sm flex-grow-1"
      ),
      actionButton(
        ns("refresh_data"),
        "Reload",
        icon = icon("sync"),
        class = "btn-outline-primary btn-sm"
      )
    ),
    tags$small(
      class = "text-muted mt-1 d-block",
      "Clear cache if data seems stale"
    )
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
      ielisa_data = NULL,
      elisa_data = NULL,
      mic_data = NULL,
      current_site = config$current_site
    )

    # ========================================================================
    # CURRENT SITE INFO UI
    # ========================================================================

    output$current_site_info <- renderUI({
      site_id <- rv$current_site

      if (!is.null(site_id) && !is.null(config$sites[[site_id]])) {
        site_info <- config$sites[[site_id]]
        div(
          class = "alert alert-success",
          style = "padding: 8px 12px;",
          tags$strong(icon("check-circle"), " Active:"),
          tags$br(),
          tags$small(site_info$short_name),
          tags$br(),
          tags$small(
            class = "text-muted",
            site_info$location
          )
        )
      } else {
        div(
          class = "alert alert-warning",
          style = "padding: 8px 12px;",
          icon("info-circle"),
          " No biobank selected"
        )
      }
    })
    
    # ========================================================================
    # BIOBANK SITE LOADING
    # ========================================================================

    # Helper function to load all data for a site
    load_site_data <- function(site_id) {
      start_time <- Sys.time()

      # Show loading notification with progress
      withProgress(message = paste("Loading", config$sites[[site_id]]$short_name, "data..."), value = 0, {

      tryCatch({
        # Update current site
        rv$current_site <- site_id

        # Get site-specific paths
        site_paths <- get_site_paths(site_id)

        incProgress(0.05, detail = "Finding biobank file...")

        # Find latest biobank file
        biobank_file <- get_latest_biobank_file(site_paths$biobank_dir)

        if (is.null(biobank_file)) {
          showNotification(
            paste("No biobank files found for", config$sites[[site_id]]$short_name),
            type = "warning",
            duration = 5
          )
          return()
        }

        incProgress(0.05, detail = "Loading biobank data...")

        # Load biobank data with RDS caching
        df_raw <- load_biobank_file_cached(biobank_file, cache_dir = site_paths$cache_dir)
        rv$raw_data <- df_raw

        incProgress(0.1, detail = "Analyzing data quality...")

        # Analyze quality BEFORE cleaning
        quality <- analyze_data_quality(df_raw)

        incProgress(0.1, detail = "Cleaning data...")

        # Clean data using the improved cleaner
        df_clean <- clean_biobank_data_improved(df_raw)

        # Add quality tracking to clean data
        if (!is.null(quality$invalid_row_indices) && length(quality$invalid_row_indices) > 0) {
          df_clean$.__has_quality_issues <- FALSE

          nms_raw <- names(df_raw)

          .find_col_local <- function(nms, patterns) {
            for (p in patterns) {
              hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
              if (length(hit)) return(hit[1])
            }
            NULL
          }

          barcode_col_raw <- .find_col_local(nms_raw, c("code.*barres.*kps", "code.*barre", "\\bbarcode\\b", "kps"))
          labid_col_raw <- .find_col_local(nms_raw, c("^num[eé]ro$", "^numero$", "^num$", "lab.?id"))

          barcode_col_clean <- if ("barcode" %in% names(df_clean)) "barcode" else NULL
          labid_col_clean <- if ("lab_id" %in% names(df_clean)) "lab_id" else NULL

          if (!is.null(barcode_col_raw) && !is.null(labid_col_raw) &&
              !is.null(barcode_col_clean) && !is.null(labid_col_clean)) {

            invalid_rows <- df_raw[quality$invalid_row_indices, , drop = FALSE]

            for (i in seq_len(nrow(invalid_rows))) {
              bc <- invalid_rows[[barcode_col_raw]][i]
              lid <- invalid_rows[[labid_col_raw]][i]

              bc_ok <- !is.null(bc) && length(bc) == 1 && !is.na(bc)
              lid_ok <- !is.null(lid) && length(lid) == 1 && !is.na(lid)

              if (bc_ok && lid_ok) {
                matches <- which(
                  !is.na(df_clean[[barcode_col_clean]]) &
                  !is.na(df_clean[[labid_col_clean]]) &
                  as.character(df_clean[[barcode_col_clean]]) == as.character(bc) &
                  as.character(df_clean[[labid_col_clean]]) == as.character(lid)
                )
                if (length(matches) > 0) {
                  df_clean$.__has_quality_issues[matches] <- TRUE
                }
              }
            }
          }
        } else {
          df_clean$.__has_quality_issues <- FALSE
        }

        rv$clean_data <- df_clean

        incProgress(0.1, detail = "Loading extractions...")

        # Load extraction data
        extraction_df <- tryCatch({
          load_all_extractions(site_paths$extractions_dir)
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

        # Re-analyze quality AFTER cleaning
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
          labid_conflicts = quality$labid_conflicts,
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
          row_flags_detailed = quality$row_flags_detailed,
          invalid_reasons = quality$invalid_reasons,
          invalid_row_indices = quality$invalid_row_indices
        )

        # Update filter choices
        update_filter_choices(session, df_clean)

        incProgress(0.1, detail = "Loading iELISA data...")

        # Load iELISA data
        tryCatch({
          ielisa_data <- load_ielisa_data(
            ielisa_dir = site_paths$ielisa_dir,
            cache_dir = site_paths$cache_dir
          )
          rv$ielisa_data <- ielisa_data
        }, error = function(e) {
          message("Failed to load iELISA data: ", e$message)
          rv$ielisa_data <- NULL
        })

        incProgress(0.15, detail = "Loading ELISA PE/VSG data...")

        # Load ELISA PE/VSG data
        tryCatch({
          elisa_data <- load_elisa_data(
            dirs = c(site_paths$elisa_pe_dir, site_paths$elisa_vsg_dir),
            biobank_df = df_clean
          )
          rv$elisa_data <- elisa_data
        }, error = function(e) {
          message("Failed to load ELISA PE/VSG data: ", e$message)
          rv$elisa_data <- NULL
        })

        incProgress(0.15, detail = "Loading MIC qPCR data...")

        # Load MIC qPCR data
        tryCatch({
          if (dir.exists(site_paths$mic_dir)) {
            # Use default settings
            mic_settings <- list(
              thresholds = list(
                `177T` = list(positive = 35, negative = 40),
                `18S2` = list(positive = 35, negative = 40),
                RNAseP_DNA = list(positive = 32, negative = 45),
                RNAseP_RNA = list(positive = 30, negative = 45)
              ),
              late_window = c(38, 40),
              delta_rp_limit = 8,
              allow_review_controls = FALSE,
              min_positive_reps = 2
            )

            # Initialize cache state if not exists
            if (is.null(rv$mic_cache_state)) {
              rv$mic_cache_state <- list()
            }

            mic_data <- parse_mic_directory(
              site_paths$mic_dir,
              mic_settings,
              rv$mic_cache_state
            )
            rv$mic_data <- mic_data
            rv$mic_cache_state <- mic_data$cache
          } else {
            rv$mic_data <- NULL
          }
        }, error = function(e) {
          message("Failed to load MIC qPCR data: ", e$message)
          rv$mic_data <- NULL
        })

        incProgress(0.1, detail = "Finalizing...")

        # Calculate loading time
        elapsed_time <- round(difftime(Sys.time(), start_time, units = "secs"), 1)

        # Calculate counts for all data types
        n_extractions <- if (!is.null(rv$extraction_data)) nrow(rv$extraction_data) else 0
        n_ielisa <- if (!is.null(rv$ielisa_data)) nrow(rv$ielisa_data) else 0
        n_elisa <- if (!is.null(rv$elisa_data)) nrow(rv$elisa_data) else 0
        n_mic <- if (!is.null(rv$mic_data) && !is.null(rv$mic_data$samples)) nrow(rv$mic_data$samples) else 0

        showNotification(
          HTML(sprintf(
            "<strong>Loaded %s data in %.1fs:</strong><br/>
            • Biobank: %d rows (cleaned to %d)<br/>
            • Extractions: %d samples<br/>
            • iELISA: %d samples<br/>
            • ELISA PE/VSG: %d samples<br/>
            • MIC qPCR: %d samples",
            config$sites[[site_id]]$short_name,
            elapsed_time,
            nrow(df_raw), nrow(df_clean),
            n_extractions, n_ielisa, n_elisa, n_mic
          )),
          type = "message",
          duration = 7
        )

      }, error = function(e) {
        showNotification(
          paste("Error loading data:", e$message),
          type = "error",
          duration = 10
        )
      })
      }) # End withProgress
    }

    # Create observers for each biobank button
    if (!is.null(config$sites)) {
      lapply(names(config$sites), function(site_id) {
        observeEvent(input[[paste0("load_site_", site_id)]], {
          load_site_data(site_id)
        })
      })
    }

    # ========================================================================
    # LEGACY DATA LOADING (KEPT FOR BACKWARD COMPATIBILITY)
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

        # Step 3a: Add quality tracking to clean data
        # Mark rows that had quality issues in the raw data analysis
        if (!is.null(quality$invalid_row_indices) && length(quality$invalid_row_indices) > 0) {
          # Add a column to track if row had quality issues in raw data
          df_clean$.__has_quality_issues <- FALSE

          # Find which rows in clean data correspond to invalid rows in raw data
          # We need to find the column names in the raw data (before janitor::clean_names)
          nms_raw <- names(df_raw)

          # Helper function to detect columns
          .find_col_local <- function(nms, patterns) {
            for (p in patterns) {
              hit <- grep(p, nms, ignore.case = TRUE, value = TRUE)
              if (length(hit)) return(hit[1])
            }
            NULL
          }

          # Detect barcode and lab_id columns in RAW data (original column names)
          barcode_col_raw <- .find_col_local(nms_raw, c("code.*barres.*kps", "code.*barre", "\\bbarcode\\b", "kps"))
          labid_col_raw <- .find_col_local(nms_raw, c("^num[eé]ro$", "^numero$", "^num$", "lab.?id"))

          # Column names in clean data (after cleaning)
          barcode_col_clean <- if ("barcode" %in% names(df_clean)) "barcode" else NULL
          labid_col_clean <- if ("lab_id" %in% names(df_clean)) "lab_id" else NULL

          if (!is.null(barcode_col_raw) && !is.null(labid_col_raw) &&
              !is.null(barcode_col_clean) && !is.null(labid_col_clean)) {

            # Get invalid rows from raw data
            invalid_rows <- df_raw[quality$invalid_row_indices, , drop = FALSE]

            # Find matching rows in clean data by barcode and lab_id
            for (i in seq_len(nrow(invalid_rows))) {
              bc <- invalid_rows[[barcode_col_raw]][i]
              lid <- invalid_rows[[labid_col_raw]][i]

              # Safely check for NA values - handle NULL and length issues
              bc_ok <- !is.null(bc) && length(bc) == 1 && !is.na(bc)
              lid_ok <- !is.null(lid) && length(lid) == 1 && !is.na(lid)

              if (bc_ok && lid_ok) {
                matches <- which(
                  !is.na(df_clean[[barcode_col_clean]]) &
                  !is.na(df_clean[[labid_col_clean]]) &
                  as.character(df_clean[[barcode_col_clean]]) == as.character(bc) &
                  as.character(df_clean[[labid_col_clean]]) == as.character(lid)
                )
                if (length(matches) > 0) {
                  df_clean$.__has_quality_issues[matches] <- TRUE
                }
              }
            }
          }
        } else {
          df_clean$.__has_quality_issues <- FALSE
        }

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
          labid_conflicts = quality$labid_conflicts,
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
          row_flags_detailed = quality$row_flags_detailed,
          invalid_reasons = quality$invalid_reasons,
          invalid_row_indices = quality$invalid_row_indices
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
    # iELISA DATA LOADING
    # ========================================================================

    observeEvent(input$load_ielisa, {
      req(input$ielisa_dir)

      # Show loading notification
      loading_id <- showNotification(
        "Loading iELISA data...",
        duration = NULL,
        type = "message"
      )

      tryCatch({
        # Load iELISA data using the utility function
        ielisa_data <- load_ielisa_data(
          ielisa_dir = input$ielisa_dir,
          # Use default QC parameters
          neg_od_min = 1,
          neg_od_max = 3,
          pos_od_min = 0.3,
          pos_od_max = 0.7,
          ctrl_inh_min = 50,
          ctrl_inh_max = 80,
          ctrl_cv_max = 20
        )

        rv$ielisa_data <- ielisa_data

        # Success notification
        removeNotification(loading_id)

        if (nrow(ielisa_data) > 0) {
          n_files <- dplyr::n_distinct(ielisa_data$file)
          showNotification(
            sprintf("Loaded %d iELISA samples from %d files",
                    nrow(ielisa_data), n_files),
            type = "message",
            duration = 5
          )
        } else {
          showNotification(
            "No iELISA files found in directory",
            type = "warning",
            duration = 5
          )
        }

      }, error = function(e) {
        removeNotification(loading_id)
        showNotification(
          paste("Error loading iELISA data:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    # iELISA status output
    output$ielisa_status <- renderUI({
      if (is.null(rv$ielisa_data)) {
        div(
          class = "alert alert-info mt-2",
          style = "padding: 6px 10px; font-size: 0.875rem;",
          icon("info-circle"),
          " No iELISA data loaded"
        )
      } else {
        n_samples <- nrow(rv$ielisa_data)
        n_files <- dplyr::n_distinct(rv$ielisa_data$file)

        div(
          class = "alert alert-success mt-2",
          style = "padding: 6px 10px; font-size: 0.875rem;",
          icon("check-circle"),
          sprintf(" Loaded: %d samples from %d files", n_samples, n_files)
        )
      }
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

      df <- rv$clean_data

      # Apply quality filter if enabled
      if (isTRUE(input$exclude_quality_issues) && ".__has_quality_issues" %in% names(df)) {
        df <- df %>%
          dplyr::filter(!.__has_quality_issues)
      }

      # Apply other filters
      apply_filters(
        df,
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
          # Use key as value (sent to server) and label as name (displayed)
          # This ensures filter comparisons work with normalized keys
          structure_select_choices <- c(
            structure_select_choices,
            stats::setNames(structure_choices$key, structure_choices$label)
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
    # CACHE MANAGEMENT
    # ========================================================================

    # Cache status display
    output$cache_status <- renderUI({
      # Invalidate on clear_cache or refresh_data clicks
      input$clear_cache
      input$refresh_data

      site_paths <- if (!is.null(rv$current_site)) {
        get_site_paths(rv$current_site)
      } else if (!is.null(config$site_paths)) {
        config$site_paths
      } else {
        NULL
      }

      if (is.null(site_paths)) {
        return(div(
          class = "text-muted small",
          "No site selected"
        ))
      }

      status <- tryCatch(
        get_cache_status(site_paths),
        error = function(e) NULL
      )

      if (is.null(status)) {
        return(div(
          class = "text-muted small",
          "Cache status unavailable"
        ))
      }

      div(
        class = "small",
        style = "font-size: 0.8rem;",
        if (status$total$n_files > 0) {
          tagList(
            div(
              class = "d-flex justify-content-between",
              span("Cached files:"),
              strong(status$total$n_files)
            ),
            div(
              class = "d-flex justify-content-between text-muted",
              span("Size:"),
              span(sprintf("%.1f MB", status$total$size_mb))
            )
          )
        } else {
          span(class = "text-muted", "No cached data")
        }
      )
    })

    # Clear cache button handler
    observeEvent(input$clear_cache, {
      site_paths <- if (!is.null(rv$current_site)) {
        get_site_paths(rv$current_site)
      } else if (!is.null(config$site_paths)) {
        config$site_paths
      } else {
        NULL
      }

      if (!is.null(site_paths)) {
        results <- clear_all_caches(site_paths, clear_rds = TRUE, clear_memory = TRUE)
        total_cleared <- results$biobank + results$elisa + results$ielisa + results$mic

        showNotification(
          sprintf("Cache cleared: %d files removed", total_cleared),
          type = "message",
          duration = 3
        )
      } else {
        showNotification(
          "No site selected to clear cache",
          type = "warning",
          duration = 3
        )
      }
    })

    # Refresh data button handler (reload current site)
    observeEvent(input$refresh_data, {
      if (!is.null(rv$current_site)) {
        load_site_data(rv$current_site)
      } else {
        showNotification(
          "No site loaded to refresh",
          type = "warning",
          duration = 3
        )
      }
    })

    # ========================================================================
    # AUTO-LOAD DEFAULT SITE ON STARTUP
    # ========================================================================

    # Automatically load the default site when the app starts
    observe({
      # Only run once when the session starts
      isolate({
        if (is.null(rv$clean_data) && !is.null(config$app$default_site)) {
          default_site <- config$app$default_site
          message("Auto-loading default site: ", default_site)

          # Use a small delay to ensure UI is ready
          shiny::invalidateLater(100)

          # Trigger the load for the default site
          load_site_data(default_site)
        }
      })
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
      filters = filters,
      ielisa_data = reactive(rv$ielisa_data),
      elisa_data = reactive(rv$elisa_data),
      mic_data = reactive(rv$mic_data)
    ))
  })
}
