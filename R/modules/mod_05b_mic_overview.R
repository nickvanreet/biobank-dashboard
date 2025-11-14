# ==============================================================================
# MODULE 1: OVERVIEW - KPIs and Run Summary
# ==============================================================================

mod_mic_overview_ui <- function(id) {
  ns <- NS(id)

  # Note: ns() is already applied by the parent coordinator module
  # So we need to get the parent namespace
  parent_ns <- function(x) paste0(gsub("-overview$", "", id), "-", x)

  tagList(
    # Action bar for MIC controls
    card(
      class = "mb-3",
      card_body(
        class = "py-2",
        layout_columns(
          col_widths = c(8, 4),
          textInput(
            parent_ns("mic_dir"),
            NULL,
            value = "data/MIC",
            placeholder = "Path to MIC Excel files",
            width = "100%"
          ),
          div(
            class = "d-flex gap-2 align-items-end",
            actionButton(
              parent_ns("refresh"),
              "Refresh",
              icon = icon("sync"),
              class = "btn-primary"
            ),
            actionButton(
              parent_ns("settings"),
              "Settings",
              icon = icon("sliders"),
              class = "btn-outline-secondary"
            )
          )
        )
      )
    ),

    # KPI Dashboard - Reorganized with clear focus on TNA detection
    # Row 1: Sample Counts
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "Total Samples",
        value = textOutput(ns("kpi_samples")),
        showcase = icon("vial"),
        theme = "primary"
      ),

      value_box(
        title = "Valid Samples",
        value = textOutput(ns("kpi_valid_samples")),
        showcase = icon("check"),
        theme = "success"
      ),

      value_box(
        title = "Invalid (QC Fail)",
        value = textOutput(ns("kpi_invalid")),
        showcase = icon("exclamation-triangle"),
        theme = "danger"
      ),

      value_box(
        title = "Indeterminate",
        value = textOutput(ns("kpi_indeterminate")),
        showcase = icon("question-circle"),
        theme = "warning"
      )
    ),

    # Row 2: Trypanosoma Detection
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "TNA Positive",
        value = textOutput(ns("kpi_tna_positive")),
        showcase = icon("dna"),
        theme = "success"
      ),

      value_box(
        title = "DNA Only Positive",
        value = textOutput(ns("kpi_dna_only")),
        showcase = icon("circle-half-stroke"),
        theme = "info"
      ),

      value_box(
        title = "RNA Only Positive",
        value = textOutput(ns("kpi_rna_only")),
        showcase = icon("circle-half-stroke"),
        theme = "info"
      ),

      value_box(
        title = "Negative",
        value = textOutput(ns("kpi_negative")),
        showcase = icon("times-circle"),
        theme = "secondary"
      )
    ),

    # Row 3: Detection Quality
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "Strong Positive (Cq<35)",
        value = textOutput(ns("kpi_strong_positive")),
        showcase = icon("signal"),
        theme = "success"
      ),

      value_box(
        title = "Late Positive (35-40)",
        value = textOutput(ns("kpi_late_positive")),
        showcase = icon("clock"),
        theme = "warning"
      ),

      value_box(
        title = "Prevalence",
        value = textOutput(ns("kpi_prevalence")),
        showcase = icon("percent"),
        theme = "primary"
      ),

      value_box(
        title = "Replicate Positivity",
        value = textOutput(ns("kpi_replicate_positivity")),
        showcase = icon("flask"),
        theme = "info"
      )
    ),

    # Row 4: RNA Preservation & QC
    layout_column_wrap(
      width = 1/4,
      heights_equal = "row",
      gap = "12px",

      value_box(
        title = "Good RNA (ΔCq ≤5)",
        value = textOutput(ns("kpi_rna_good")),
        showcase = icon("star"),
        theme = "success"
      ),

      value_box(
        title = "Moderate Loss (ΔCq 5-8)",
        value = textOutput(ns("kpi_rna_moderate")),
        showcase = icon("star-half-stroke"),
        theme = "warning"
      ),

      value_box(
        title = "Poor RNA (ΔCq >8)",
        value = textOutput(ns("kpi_rna_poor")),
        showcase = icon("exclamation"),
        theme = "danger"
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

    # Row 1: Sample Counts
    output$kpi_samples <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample") %>% nrow() %>% scales::comma()
    })

    output$kpi_valid_samples <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", !FinalCall %in% c("Invalid", "Invalid_NoDNA")) %>%
        nrow() %>% scales::comma()
    })

    output$kpi_invalid <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall %in% c("Invalid", "Invalid_NoDNA")) %>%
        nrow() %>% scales::comma()
    })

    output$kpi_indeterminate <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Indeterminate") %>%
        nrow() %>% scales::comma()
    })

    # Row 2: Trypanosoma Detection
    output$kpi_tna_positive <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Positive") %>%
        nrow() %>% scales::comma()
    })

    output$kpi_dna_only <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Positive_DNA") %>%
        nrow() %>% scales::comma()
    })

    output$kpi_rna_only <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Positive_RNA") %>%
        nrow() %>% scales::comma()
    })

    output$kpi_negative <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Negative") %>%
        nrow() %>% scales::comma()
    })

    # Row 3: Detection Quality
    output$kpi_strong_positive <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "Positive") %>%
        nrow() %>% scales::comma()
    })

    output$kpi_late_positive <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      df %>% filter(ControlType == "Sample", FinalCall == "LatePositive") %>%
        nrow() %>% scales::comma()
    })

    output$kpi_prevalence <- renderText({
      df <- filtered_base() %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")
      n_pos <- sum(df$FinalCall %in% c("Positive", "Positive_DNA", "Positive_RNA"), na.rm = TRUE)
      pct <- round(100 * n_pos / nrow(df), 1)
      paste0(pct, "%")
    })

    output$kpi_replicate_positivity <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "Wells_TNA_Positive", "ReplicatesTotal") %in% names(df))) {
        return("0%")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0%")

      total_wells <- sum(df$ReplicatesTotal, na.rm = TRUE)
      positive_wells <- sum(df$Wells_TNA_Positive, na.rm = TRUE)

      if (total_wells == 0) return("0%")
      paste0(round(100 * positive_wells / total_wells, 1), "%")
    })

    # Row 4: RNA Preservation & QC
    output$kpi_rna_good <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      good <- sum(!is.na(df$RNA_Preservation_Delta) & df$RNA_Preservation_Delta <= 5, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(good, " (", round(100 * good / total), "%)")
    })

    output$kpi_rna_moderate <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      moderate <- sum(!is.na(df$RNA_Preservation_Delta) &
                      df$RNA_Preservation_Delta > 5 &
                      df$RNA_Preservation_Delta <= 8, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(moderate, " (", round(100 * moderate / total), "%)")
    })

    output$kpi_rna_poor <- renderText({
      df <- filtered_base()
      if (!nrow(df) || !all(c("ControlType", "RNA_Preservation_Delta") %in% names(df))) {
        return("N/A")
      }

      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("N/A")

      poor <- sum(!is.na(df$RNA_Preservation_Delta) & df$RNA_Preservation_Delta > 8, na.rm = TRUE)
      total <- sum(!is.na(df$RNA_Preservation_Delta))
      if (total == 0) return("N/A")

      paste0(poor, " (", round(100 * poor / total), "%)")
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
        select(-any_of(c("FilePath", "FileName", "FileMTime", "RunID", "ThresholdsJSON"))) %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          RunValid = if_else(RunValid, "✓", "✗")
        )

      datatable(
        display,
        options = list(
          pageLength = 15,
          autoWidth = TRUE,
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
