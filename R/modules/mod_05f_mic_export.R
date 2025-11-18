# ==============================================================================
# MODULE 5: EXPORT - All download handlers
# ==============================================================================

mod_mic_export_ui <- function(id) {
  ns <- NS(id)
  
  layout_columns(
    col_widths = c(6, 6),
    
    # Core data exports
    card(
      card_header("Core Data Exports"),
      card_body(
        class = "p-4",
        h5("Sample-Level Data", class = "mb-3"),
        downloadButton(ns("dl_samples"), "All Sample Results", class = "btn-primary w-100 mb-3"),
        downloadButton(ns("dl_positives"), "Positive Samples Only", class = "btn-success w-100 mb-3"),
        downloadButton(ns("dl_flags"), "Flagged Samples", class = "btn-warning w-100 mb-3"),
        
        hr(),
        
        h5("Run-Level Data", class = "mb-3"),
        downloadButton(ns("dl_runs"), "Run Metadata", class = "btn-info w-100 mb-3"),
        downloadButton(ns("dl_controls"), "Control Performance", class = "btn-info w-100 mb-3")
      )
    ),
    
    # Analysis exports
    card(
      card_header("Analysis Exports"),
      card_body(
        class = "p-4",
        h5("Quality Metrics", class = "mb-3"),
        downloadButton(ns("dl_deltas"), "ΔCq Summary", class = "btn-secondary w-100 mb-3"),
        downloadButton(ns("dl_lj"), "Levey-Jennings Stats", class = "btn-secondary w-100 mb-3"),
        downloadButton(ns("dl_replicates"), "Replicate-Level Data", class = "btn-secondary w-100 mb-3"),
        downloadButton(ns("dl_decision_paths"), "Decision Paths (Flagged)", class = "btn-secondary w-100 mb-3"),
        p(class = "small text-muted mb-3", "Decision tree paths for flagged/conflict samples"),

        hr(),

        h5("Complete Dataset", class = "mb-3"),
        downloadButton(ns("dl_calling_summary"), "Calling Summary & Rules", class = "btn-primary w-100 mb-3"),
        p(class = "small text-muted mb-3", "Summary report with calling criteria used"),
        downloadButton(ns("dl_complete"), "Full Export (Excel)", class = "btn-dark w-100 mb-3"),
        p(class = "small text-muted", "All data in multiple sheets")
      )
    )
  )
}

mod_mic_export_server <- function(id, processed_data, filtered_base, settings = NULL) {
  moduleServer(id, function(input, output, session) {

    # Calling Summary export
    output$dl_calling_summary <- downloadHandler(
      filename = function() sprintf("mic_calling_summary_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>% filter(ControlType == "Sample")

        # Get current settings
        current_settings <- if (!is.null(settings)) settings() else list()

        # Extract thresholds
        th_177t_pos <- if (is.null(current_settings$thresholds$`177T`$positive)) 35 else current_settings$thresholds$`177T`$positive
        th_177t_neg <- if (is.null(current_settings$thresholds$`177T`$negative)) 40 else current_settings$thresholds$`177T`$negative
        th_18s2_pos <- if (is.null(current_settings$thresholds$`18S2`$positive)) 35 else current_settings$thresholds$`18S2`$positive
        th_18s2_neg <- if (is.null(current_settings$thresholds$`18S2`$negative)) 40 else current_settings$thresholds$`18S2`$negative
        min_reps <- if (is.null(current_settings$min_positive_reps)) 2 else current_settings$min_positive_reps

        # Create summary tibble
        summary <- tibble(
          Metric = c(
            "Total Samples",
            "TNA Positive (both targets)",
            "DNA Only Positive (177T)",
            "RNA Only Positive (18S2)",
            "Negative",
            "Invalid (QC Fail)",
            "Indeterminate",
            "Late Positive",
            "",
            "Calling Criteria Used:",
            "177T Positive Threshold",
            "177T Negative Threshold",
            "18S2 Positive Threshold",
            "18S2 Negative Threshold",
            "Min Replicates for Positive Call",
            "",
            "Prevalence (%)",
            "QC Failure Rate (%)"
          ),
          Value = c(
            nrow(df),
            sum(df$FinalCall == "Positive", na.rm = TRUE),
            sum(df$FinalCall == "Positive_DNA", na.rm = TRUE),
            sum(df$FinalCall == "Positive_RNA", na.rm = TRUE),
            sum(df$FinalCall == "Negative", na.rm = TRUE),
            sum(df$FinalCall %in% c("Invalid", "Invalid_NoDNA"), na.rm = TRUE),
            sum(df$FinalCall == "Indeterminate", na.rm = TRUE),
            sum(df$FinalCall == "LatePositive", na.rm = TRUE),
            "",
            "",
            paste0("≤ ", th_177t_pos),
            paste0("> ", th_177t_neg),
            paste0("≤ ", th_18s2_pos),
            paste0("> ", th_18s2_neg),
            paste0(min_reps, "/4 replicates"),
            "",
            if (nrow(df) > 0) {
              paste0(round(100 * sum(df$FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA"), na.rm = TRUE) / nrow(df), 1), "%")
            } else "0%",
            if (nrow(df) > 0) {
              paste0(round(100 * sum(df$FinalCall %in% c("Invalid", "Invalid_NoDNA"), na.rm = TRUE) / nrow(df), 1), "%")
            } else "0%"
          )
        )

        write_csv(summary, file)
      }
    )

    # Core samples export
    output$dl_samples <- downloadHandler(
      filename = function() sprintf("mic_samples_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>% filter(ControlType == "Sample")
        write_csv(df, file)
      }
    )
    
    # Positives only
    output$dl_positives <- downloadHandler(
      filename = function() sprintf("mic_positives_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>%
          filter(ControlType == "Sample", FinalCall == "Positive")
        write_csv(df, file)
      }
    )
    
    # Flagged samples
    output$dl_flags <- downloadHandler(
      filename = function() sprintf("mic_flags_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>%
          filter(ControlType == "Sample", AnyFlag | FinalCall == "Invalid_NoDNA")
        write_csv(df, file)
      }
    )
    
    # Runs metadata
    output$dl_runs <- downloadHandler(
      filename = function() sprintf("mic_runs_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$runs, file)
    )
    
    # Controls
    output$dl_controls <- downloadHandler(
      filename = function() sprintf("mic_controls_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$control_status, file)
    )
    
    # Deltas
    output$dl_deltas <- downloadHandler(
      filename = function() sprintf("mic_deltas_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>%
          select(RunID, SampleID, SampleName, Delta_18S2_177T, Delta_RP, 
                 any_of(c("Flag_SampleDecay", "Flags")))
        write_csv(df, file)
      }
    )
    
    # Levey-Jennings stats
    output$dl_lj <- downloadHandler(
      filename = function() sprintf("mic_lj_stats_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        lj <- processed_data()$lj_stats
        stats <- map_dfr(lj, "summary")
        write_csv(stats, file)
      }
    )
    
    # Replicates
    output$dl_replicates <- downloadHandler(
      filename = function() sprintf("mic_replicates_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$replicates, file)
    )

    # Decision paths for flagged samples
    output$dl_decision_paths <- downloadHandler(
      filename = function() sprintf("mic_decision_paths_%s.txt", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        # Get flagged or problematic samples
        df <- filtered_base() %>%
          filter(
            ControlType == "Sample",
            AnyFlag | WellAggregateConflict | ConfidenceScore == "Low" | FinalCall == "Indeterminate"
          )

        if (nrow(df) == 0) {
          writeLines("No flagged or problematic samples found.", file)
          return()
        }

        # Generate decision paths for each sample
        paths <- character(nrow(df))
        for (i in seq_len(nrow(df))) {
          sample_data <- df[i, , drop = FALSE]
          path_text <- tryCatch({
            visualize_decision_path(sample_data)
          }, error = function(e) {
            sprintf("Error generating decision path for %s: %s", sample_data$SampleName, e$message)
          })
          paths[i] <- path_text
        }

        # Combine all paths with separators
        output_text <- paste(paths, collapse = "\n\n\n")

        # Write to file
        writeLines(output_text, file)
      }
    )

    # Complete export (Excel)
    output$dl_complete <- downloadHandler(
      filename = function() sprintf("mic_complete_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        require(writexl)
        pd <- processed_data()
        
        sheets <- list(
          "Samples" = filtered_base() %>% filter(ControlType == "Sample"),
          "Runs" = pd$runs,
          "Controls" = pd$control_status,
          "LJ_Stats" = map_dfr(pd$lj_stats, "summary"),
          "Replicates" = pd$replicates
        )
        
        # Remove empty sheets
        sheets <- sheets[sapply(sheets, nrow) > 0]
        
        write_xlsx(sheets, file)
      }
    )
    
  })
}
