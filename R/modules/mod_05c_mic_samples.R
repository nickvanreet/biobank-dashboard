# ==============================================================================
# MODULE 2: SAMPLES - Main results table with filters
# ==============================================================================

mod_mic_samples_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Results table - filters are applied from sidebar
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Sample Results"),
        downloadButton(ns("dl_filtered"), "Download", class = "btn-sm btn-outline-primary")
      ),
      card_body(
        DTOutput(ns("tbl_samples")),
        class = "p-3"
      )
    )
  )
}

mod_mic_samples_server <- function(id, filtered_base, processed_data) {
  moduleServer(id, function(input, output, session) {

    # Main samples table
    output$tbl_samples <- renderDT({
      df <- filtered_base()
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples match filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Only show samples (not controls)
      if ("ControlType" %in% names(df)) {
        df <- df %>% filter(ControlType == "Sample")
      }
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found (only controls)"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Round numeric columns
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA", 
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(df)
      )
      
      if (length(numeric_cols) > 0) {
        df <- df %>% mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }
      
      # Select columns to display - including Barcode
      available_cols <- intersect(
        c("RunID", "SampleName", "Barcode", "FinalCall",
          "Cq_median_177T", "Cq_median_18S2",
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "Province", "Structure", "HealthZone",
          "BiobankMatched", "ExtractionMatched", "Flags"),
        names(df)
      )
      
      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
          columnDefs = list(
            list(className = 'dt-center', targets = c('FinalCall', 'BiobankMatched', 'ExtractionMatched'))
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover",
        filter = 'top'
      ) %>%
        formatStyle('FinalCall',
                    backgroundColor = styleEqual(
                      c('Positive', 'LatePositive', 'Negative', 'Indeterminate', 'Invalid_NoDNA', 'Control', 'Control_Fail'),
                      c('#d4edda', '#ffe8a1', '#f8f9fa', '#fff3cd', '#f8d7da', '#dbe9ff', '#f5c6cb')
                    ))
    })

    # Download filtered
    output$dl_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_base() %>% filter(ControlType == "Sample")
        write_csv(df, file)
      }
    )
    
  })
}
