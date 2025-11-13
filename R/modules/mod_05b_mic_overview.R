# ==============================================================================
# MODULE 1: OVERVIEW - KPIs and Run Summary
# ==============================================================================

mod_mic_overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # KPI Dashboard - 2 rows
    layout_column_wrap(
      width = 1/5,
      heights_equal = "row",
      
      value_box(
        title = "Total Runs",
        value = textOutput(ns("kpi_runs")),
        showcase = icon("folder-open"),
        theme = "primary"
      ),
      
      value_box(
        title = "Total Samples",
        value = textOutput(ns("kpi_samples")),
        showcase = icon("vial"),
        theme = "info"
      ),
      
      value_box(
        title = "Positives",
        value = textOutput(ns("kpi_positives")),
        showcase = icon("check-circle"),
        theme = "success"
      ),
      
      value_box(
        title = "Prevalence",
        value = textOutput(ns("kpi_prevalence")),
        showcase = icon("percent"),
        theme = "success"
      ),
      
      value_box(
        title = "QC Issues",
        value = textOutput(ns("kpi_flagged")),
        showcase = icon("flag"),
        theme = "warning"
      )
    ),
    
    layout_column_wrap(
      width = 1/5,
      heights_equal = "row",
      
      value_box(
        title = "Biobank Linked",
        value = textOutput(ns("kpi_biobank")),
        showcase = icon("link"),
        theme = "secondary"
      ),
      
      value_box(
        title = "Extractions Linked",
        value = textOutput(ns("kpi_extractions")),
        showcase = icon("flask"),
        theme = "secondary"
      ),
      
      value_box(
        title = "RNA Quality Good",
        value = textOutput(ns("kpi_rna_good")),
        showcase = icon("star"),
        theme = "info"
      ),
      
      value_box(
        title = "DNA Quality Good",
        value = textOutput(ns("kpi_dna_good")),
        showcase = icon("star"),
        theme = "info"
      ),
      
      value_box(
        title = "Valid Runs",
        value = textOutput(ns("kpi_valid_runs")),
        showcase = icon("check-square"),
        theme = "success"
      )
    ),
    
    # Run summary table
    card(
      full_screen = TRUE,
      card_header("Run Metadata"),
      card_body(
        DTOutput(ns("tbl_runs")),
        class = "p-3"
      )
    )
  )
}

mod_mic_overview_server <- function(id, processed_data, filtered_base) {
  moduleServer(id, function(input, output, session) {
    
    # KPIs
    output$kpi_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0")
      nrow(runs) %>% scales::comma()
    })
    
    output$kpi_samples <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample") %>% nrow() %>% scales::comma()
    })
    
    output$kpi_positives <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Positive") %>% 
        nrow() %>% scales::comma()
    })
    
    output$kpi_prevalence <- renderText({
      df <- filtered_base() %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")
      n_pos <- sum(df$FinalCall == "Positive", na.rm = TRUE)
      pct <- round(100 * n_pos / nrow(df), 1)
      paste0(pct, "%")
    })
    
    output$kpi_flagged <- renderText({
      df <- filtered_base()
      if (!nrow(df)) return("0")
      df %>% filter(ControlType == "Sample", AnyFlag == TRUE) %>% 
        nrow() %>% scales::comma()
    })
    
    output$kpi_biobank <- renderText({
      df <- filtered_base() %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")
      pct <- round(100 * sum(df$BiobankMatched, na.rm = TRUE) / nrow(df))
      paste0(pct, "%")
    })
    
    output$kpi_extractions <- renderText({
      df <- filtered_base() %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")
      pct <- round(100 * sum(df$ExtractionMatched, na.rm = TRUE) / nrow(df))
      paste0(pct, "%")
    })
    
    output$kpi_rna_good <- renderText({
      rep_data <- processed_data()$replicates
      if (!nrow(rep_data) || !"rnasep_rna" %in% names(rep_data)) return("N/A")
      good <- sum(rep_data$rnasep_rna == "Good", na.rm = TRUE)
      total <- sum(!is.na(rep_data$rnasep_rna))
      if (total == 0) return("N/A")
      paste0(round(100 * good / total), "%")
    })
    
    output$kpi_dna_good <- renderText({
      rep_data <- processed_data()$replicates
      if (!nrow(rep_data) || !"rnasep_dna" %in% names(rep_data)) return("N/A")
      good <- sum(rep_data$rnasep_dna == "Good", na.rm = TRUE)
      total <- sum(!is.na(rep_data$rnasep_dna))
      if (total == 0) return("N/A")
      paste0(round(100 * good / total), "%")
    })
    
    output$kpi_valid_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0/0")
      valid <- sum(runs$RunValid, na.rm = TRUE)
      glue::glue("{valid}/{nrow(runs)}")
    })
    
    # Runs table
    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs
      
      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No runs found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      display <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗")
        )
      
      datatable(
        display,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'lfrtip',
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe hover"
      ) %>%
        formatStyle('RunValid', 
                    color = styleEqual(c('✓', '✗'), c('green', 'red')))
    })
    
  })
}
