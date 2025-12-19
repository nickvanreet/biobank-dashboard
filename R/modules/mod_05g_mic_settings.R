# ==============================================================================
# MODULE 6: SETTINGS - QC Threshold Configuration (MODERN DESIGN)
# ==============================================================================

mod_mic_settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$style(HTML("
      .mic-settings-panel {
        min-width: 800px;
      }
      .mic-settings-panel .card {
        margin-bottom: 1.5rem;
      }
      .mic-settings-panel .card-body {
        padding: 1.5rem;
      }
      .mic-settings-panel h6 {
        font-weight: 600;
        color: #374151;
        margin-bottom: 1rem;
      }
      .mic-settings-panel .form-group {
        margin-bottom: 1.25rem;
      }
      .settings-section-header {
        font-size: 1rem;
        font-weight: 600;
        color: #2c3e50;
        margin-bottom: 1rem;
        padding-bottom: 0.5rem;
        border-bottom: 2px solid #e9ecef;
      }
      .threshold-group {
        background: #f8f9fa;
        border-radius: 8px;
        padding: 1.25rem;
        margin-bottom: 1.25rem;
      }
      .threshold-label {
        font-size: 0.875rem;
        font-weight: 600;
        color: #495057;
        margin-bottom: 0.75rem;
      }
    ")),

    div(
      class = "mic-settings-panel",

    layout_columns(
      col_widths = c(6, 6),

      # Trypanozoon targets
      card(
        card_header(
          div(class = "d-flex align-items-center gap-2",
              icon("dna", class = "text-white"),
              "Trypanozoon Targets (DNA & RNA)"
          ),
          class = "bg-primary text-white"
        ),
        card_body(
          class = "p-3",
          
          h6("177T (DNA Target)", class = "mt-2 mb-3"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("th_177t_pos"), 
              "Positive ≤", 
              value = 35, 
              min = 0, 
              max = 50, 
              step = 0.5,
              width = "100%"
            ),
            numericInput(
              ns("th_177t_neg"), 
              "Negative >", 
              value = 40, 
              min = 0, 
              max = 50, 
              step = 0.5,
              width = "100%"
            )
          ),
          
          h6("18S2 (RNA Target)", class = "mt-3 mb-3"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("th_18s2_pos"), 
              "Positive ≤", 
              value = 35, 
              min = 0, 
              max = 50, 
              step = 0.5,
              width = "100%"
            ),
            numericInput(
              ns("th_18s2_neg"), 
              "Negative >", 
              value = 40, 
              min = 0, 
              max = 50, 
              step = 0.5,
              width = "100%"
            )
          )
        )
      ),
      
      # RNAseP targets
      card(
        card_header(
          div(class = "d-flex align-items-center gap-2",
              icon("vial-circle-check", class = "text-white"),
              "RNAseP Targets (Quality Control)"
          ),
          class = "bg-info text-white"
        ),
        card_body(
          class = "p-3",
          
          h6("RNAseP-DNA", class = "mt-2 mb-3"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("th_rnp_dna_pos"),
              "Positive ≤",
              value = 28,
              min = 0,
              max = 50,
              step = 0.5,
              width = "100%"
            ),
            numericInput(
              ns("th_rnp_dna_neg"),
              "Negative >",
              value = 40,
              min = 0,
              max = 50,
              step = 0.5,
              width = "100%"
            )
          ),
          
          h6("RNAseP-RNA", class = "mt-3 mb-3"),
          layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("th_rnp_rna_pos"),
              "Positive ≤",
              value = 35,
              min = 0,
              max = 50,
              step = 0.5,
              width = "100%"
            ),
            numericInput(
              ns("th_rnp_rna_neg"),
              "Negative >",
              value = 40,
              min = 0,
              max = 50,
              step = 0.5,
              width = "100%"
            )
          )
        )
      )
    ),
    
    # QC parameters
    card(
      card_header(
        div(class = "d-flex align-items-center gap-2",
            icon("chart-line"),
            "Quality Control Parameters"
        ),
        class = "bg-warning"
      ),
      card_body(
        class = "p-3",
        layout_columns(
          col_widths = c(4, 4, 4),

          div(
            h6("Late Positive Window"),
            p(class = "small text-muted mb-2", "Cq values falling inside this range are flagged as LatePositive for downstream calling."),
            layout_columns(
              col_widths = c(6, 6),
              numericInput(
                ns("late_min"),
                "Min",
                value = 35,
                min = 0,
                max = 50,
                step = 0.5,
                width = "100%"
              ),
              numericInput(
                ns("late_max"),
                "Max",
                value = 40,
                min = 0,
                max = 50,
                step = 0.5,
                width = "100%"
              )
            )
          ),

          div(
            h6("RNA Preservation"),
            numericInput(
              ns("delta_rp_limit"),
              "ΔCq Limit",
              value = 8,
              min = 0,
              max = 20,
              step = 0.5,
              width = "100%"
            ),
            p(class = "small text-muted mt-1", "Max acceptable difference between RNA and DNA")
          ),

          div(
            h6("Control Handling"),
            checkboxInput(
              ns("allow_review"),
              "Allow review despite control failures",
              value = FALSE
            ),
            p(class = "small text-muted mt-1", "Process samples even when controls fail")
          )
        )
      )
    ),

    # Calling Rules Configuration
    card(
      card_header(
        div(class = "d-flex align-items-center gap-2",
            icon("check-double", class = "text-white"),
            "Sample Calling Rules"
        ),
        class = "bg-success text-white"
      ),
      card_body(
        class = "p-3",
        layout_columns(
          col_widths = c(6, 6),

          div(
            h6("Replicate Consensus", class = "mt-2 mb-3"),
            radioButtons(
              ns("min_positive_reps"),
              "Min replicates for positive call:",
              choices = c("1/4 (any replicate)" = 1,
                         "2/4 (majority, recommended)" = 2,
                         "3/4 (strict consensus)" = 3),
              selected = 2
            ),
            p(class = "small text-muted mt-1",
              "Number of TNA-positive replicates required to call a sample positive")
          ),

          div(
            h6("Calling Priority", class = "mt-2 mb-3"),
            p(class = "small", style = "line-height: 1.6;",
              strong("Decision tree order:"), br(),
              "1. QC validity check", br(),
              "2. TNA positive (≥ threshold)", br(),
              "3. Single target detection", br(),
              "4. Late positives", br(),
              "5. True negative", br(),
              "6. Indeterminate"
            )
          )
        )
      )
    ),

    # Apply button
    div(
      class = "text-end mt-3",
      actionButton(ns("apply"), "Apply Settings", class = "btn-primary btn-lg"),
      actionButton(ns("reset"), "Reset to Defaults", class = "btn-outline-secondary ms-2")
    )
    ) # Close mic-settings-panel div
  )
}

mod_mic_settings_server <- function(id, current_settings) {
  moduleServer(id, function(input, output, session) {
    
    # Initialize inputs with current settings
    observe({
      cs <- current_settings()

      updateNumericInput(session, "th_177t_pos", value = cs$thresholds$`177T`$positive)
      updateNumericInput(session, "th_177t_neg", value = cs$thresholds$`177T`$negative)
      updateNumericInput(session, "th_18s2_pos", value = cs$thresholds$`18S2`$positive)
      updateNumericInput(session, "th_18s2_neg", value = cs$thresholds$`18S2`$negative)
      updateNumericInput(session, "th_rnp_dna_pos", value = cs$thresholds$RNAseP_DNA$positive)
      updateNumericInput(session, "th_rnp_dna_neg", value = cs$thresholds$RNAseP_DNA$negative)
      updateNumericInput(session, "th_rnp_rna_pos", value = cs$thresholds$RNAseP_RNA$positive)
      updateNumericInput(session, "th_rnp_rna_neg", value = cs$thresholds$RNAseP_RNA$negative)
      updateNumericInput(session, "late_min", value = cs$late_window[1])
      updateNumericInput(session, "late_max", value = cs$late_window[2])
      updateNumericInput(session, "delta_rp_limit", value = cs$delta_rp_limit)
      updateCheckboxInput(session, "allow_review", value = cs$allow_review_controls)

      # Update min_positive_reps if it exists in settings
      if (!is.null(cs$min_positive_reps)) {
        updateRadioButtons(session, "min_positive_reps", selected = as.character(cs$min_positive_reps))
      }
    })
    
    # Return updated settings when applied
    updated <- reactiveVal(NULL)
    
    observeEvent(input$apply, {
      new_settings <- list(
        thresholds = list(
          `177T` = list(positive = input$th_177t_pos, negative = input$th_177t_neg),
          `18S2` = list(positive = input$th_18s2_pos, negative = input$th_18s2_neg),
          RNAseP_DNA = list(positive = input$th_rnp_dna_pos, negative = input$th_rnp_dna_neg),
          RNAseP_RNA = list(positive = input$th_rnp_rna_pos, negative = input$th_rnp_rna_neg)
        ),
        late_window = c(input$late_min, input$late_max),
        delta_rp_limit = input$delta_rp_limit,
        allow_review_controls = isTRUE(input$allow_review),
        min_positive_reps = as.numeric(input$min_positive_reps)
      )

      updated(new_settings)
      removeModal()
      showNotification("Settings applied. Refresh data to reprocess.", type = "message", duration = 5)
    })
    
    observeEvent(input$reset, {
      # Reset to defaults
      updateNumericInput(session, "th_177t_pos", value = 35)
      updateNumericInput(session, "th_177t_neg", value = 40)
      updateNumericInput(session, "th_18s2_pos", value = 35)
      updateNumericInput(session, "th_18s2_neg", value = 40)
      updateNumericInput(session, "th_rnp_dna_pos", value = 28)
      updateNumericInput(session, "th_rnp_dna_neg", value = 40)
      updateNumericInput(session, "th_rnp_rna_pos", value = 35)
      updateNumericInput(session, "th_rnp_rna_neg", value = 40)
      updateNumericInput(session, "late_min", value = 35)
      updateNumericInput(session, "late_max", value = 40)
      updateNumericInput(session, "delta_rp_limit", value = 8)
      updateCheckboxInput(session, "allow_review", value = FALSE)
      updateRadioButtons(session, "min_positive_reps", selected = "2")

      showNotification("Settings reset to defaults", type = "message")
    })
    
    return(updated)
  })
}
