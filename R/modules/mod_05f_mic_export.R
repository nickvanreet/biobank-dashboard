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
        
        hr(),
        
        h5("Complete Dataset", class = "mb-3"),
        downloadButton(ns("dl_complete"), "Full Export (Excel)", class = "btn-dark w-100 mb-3"),
        p(class = "small text-muted", "All data in multiple sheets")
      )
    )
  )
}

mod_mic_export_server <- function(id, processed_data, filtered_base) {
  moduleServer(id, function(input, output, session) {
    
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
        stats <- map_dfr(lj, "summary", .id = "Target")
        write_csv(stats, file)
      }
    )
    
    # Replicates
    output$dl_replicates <- downloadHandler(
      filename = function() sprintf("mic_replicates_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$replicates, file)
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
          "LJ_Stats" = map_dfr(pd$lj_stats, "summary", .id = "Target"),
          "Replicates" = pd$replicates
        )
        
        # Remove empty sheets
        sheets <- sheets[sapply(sheets, nrow) > 0]
        
        write_xlsx(sheets, file)
      }
    )
    
  })
}
