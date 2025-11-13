# ==============================================================================
# MODULE 2: SAMPLES - Main results table with filters
# ==============================================================================

mod_mic_samples_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Filters card
    card(
      class = "mb-3",
      card_header("Filters"),
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          selectInput(
            ns("filter_call"),
            "Final Call",
            choices = c("All" = "all", "Positive", "Negative", "LatePositive", "Invalid_NoDNA"),
            selected = "all"
          ),
          selectInput(
            ns("filter_province"),
            "Province",
            choices = c("All" = "all"),
            selected = "all"
          ),
          selectInput(
            ns("filter_structure"),
            "Health Structure",
            choices = c("All" = "all"),
            selected = "all"
          ),
          checkboxInput(
            ns("filter_flagged_only"),
            "Show flagged only",
            value = FALSE
          )
        )
      )
    ),
    
    # Results table
    card(
      full_screen = TRUE,
      card_header(
        class = "d-flex justify-content-between align-items-center",
        span("Sample Results"),
        div(
          downloadButton(ns("dl_filtered"), "Download Filtered", class = "btn-sm btn-outline-primary"),
          actionButton(ns("show_columns"), "Columns", icon = icon("columns"), class = "btn-sm btn-outline-secondary ms-2")
        )
      ),
      card_body(
        DTOutput(ns("tbl_samples")),
        class = "p-3"
      )
    ),
    
    # Flagged samples card
    card(
      class = "mt-3",
      card_header(
        class = "bg-warning",
        "Samples with QC Issues"
      ),
      card_body(
        DTOutput(ns("tbl_flags")),
        class = "p-3"
      )
    )
  )
}

mod_mic_samples_server <- function(id, filtered_base, processed_data) {
  moduleServer(id, function(input, output, session) {
    
    # Apply UI filters on top of global filters
    filtered_samples <- reactive({
      df <- filtered_base()
      if (!nrow(df)) return(df)
      
      # Final call filter
      if (!is.null(input$filter_call) && input$filter_call != "all") {
        df <- df %>% filter(FinalCall == input$filter_call)
      }
      
      # Province filter
      if (!is.null(input$filter_province) && input$filter_province != "all") {
        df <- df %>% filter(Province == input$filter_province)
      }
      
      # Structure filter
      if (!is.null(input$filter_structure) && input$filter_structure != "all") {
        df <- df %>% filter(Structure == input$filter_structure)
      }
      
      # Flagged only
      if (isTRUE(input$filter_flagged_only)) {
        df <- df %>% filter(AnyFlag == TRUE)
      }
      
      df
    })
    
    # Update filter dropdowns dynamically
    observe({
      df <- filtered_base()
      if (nrow(df) > 0 && "Province" %in% names(df)) {
        provinces <- sort(unique(na.omit(df$Province)))
        updateSelectInput(session, "filter_province", 
                          choices = c("All" = "all", provinces))
      }
      
      if (nrow(df) > 0 && "Structure" %in% names(df)) {
        structures <- sort(unique(na.omit(df$Structure)))
        updateSelectInput(session, "filter_structure", 
                          choices = c("All" = "all", structures))
      }
    })
    
    # Main samples table
    output$tbl_samples <- renderDT({
      df <- filtered_samples()
      
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
      
      # Select columns to display
      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", 
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
                      c('Positive', 'Negative', 'Invalid_NoDNA'),
                      c('#d4edda', '#f8f9fa', '#f8d7da')
                    ))
    })
    
    # Flagged samples table
    output$tbl_flags <- renderDT({
      df <- filtered_samples()
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      if (!"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return(datatable(
          tibble(Message = "Required columns missing"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      df <- df %>%
        filter(
          ControlType == "Sample",
          AnyFlag == TRUE | (if ("FinalCall" %in% names(df)) FinalCall == "Invalid_NoDNA" else FALSE)
        )
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "✓ No flagged samples - all QC passed!"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", "Flags", "Delta_RP", 
          "BiobankMatched", "ExtractionMatched"),
        names(df)
      )
      
      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv')
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      )
    })
    
    # Download filtered
    output$dl_filtered <- downloadHandler(
      filename = function() sprintf("mic_samples_filtered_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )
    
  })
}
