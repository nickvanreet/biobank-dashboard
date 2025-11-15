# ==============================================================================
# MODULE 3: QC & CONTROLS - Levey-Jennings and control validation
# ==============================================================================

mod_mic_qc_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Levey-Jennings plots - shown first per user request
    # Each chart is now a separate, independent card
    layout_columns(
      col_widths = c(6, 6),
      gap = "16px",
      card(
        full_screen = TRUE,
        card_header("177T Positive Control"),
        card_body(
          plotlyOutput(ns("lj_177t"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("18S2 Positive Control"),
        card_body(
          plotlyOutput(ns("lj_18s2"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("RNAseP-DNA Positive Control"),
        card_body(
          plotlyOutput(ns("lj_rnp_dna"), height = "400px", width = "100%")
        )
      ),
      card(
        full_screen = TRUE,
        card_header("RNAseP-RNA Positive Control"),
        card_body(
          plotlyOutput(ns("lj_rnp_rna"), height = "400px", width = "100%")
        )
      )
    ),

    # Control status table - shown below charts
    card(
      class = "mt-3",
      full_screen = TRUE,
      card_header("Control Status by Run"),
      card_body(
        DTOutput(ns("tbl_controls")),
        class = "p-3"
      )
    )
  )
}

mod_mic_qc_server <- function(id, processed_data) {
  moduleServer(id, function(input, output, session) {
    
    # Controls table
    output$tbl_controls <- renderDT({
      ctrl <- processed_data()$control_status
      
      if (!nrow(ctrl)) {
        return(datatable(
          tibble(Message = "No control data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      available_cols <- intersect(
        c("RunID", "SampleName", "ControlType", "ControlPass", "ControlFlag"),
        names(ctrl)
      )
      
      ctrl <- ctrl %>%
        mutate(ControlPass = if_else(ControlPass, "✓ Pass", "✗ Fail"))
      
      datatable(
        ctrl %>% select(all_of(available_cols)),
        options = list(
          dom = 'tp', 
          paging = TRUE,
          pageLength = 20,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all')
          )
        ),
        rownames = FALSE,
        class = "display compact stripe"
      ) %>%
        formatStyle('ControlPass',
                    color = styleEqual(c('✓ Pass', '✗ Fail'), c('green', 'red')))
    })
    
    # Levey-Jennings plot generator
    render_lj_plot <- function(target_name, output_name) {
      output[[output_name]] <- renderPlotly({
        lj <- processed_data()$lj_stats[[target_name]]
        
        if (is.null(lj) || !is.list(lj)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue::glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        if (!nrow(lj$data)) {
          return(plotly_empty() %>%
                   layout(title = list(text = glue::glue("No {target_name} control data"),
                                       font = list(size = 14))))
        }

        fig <- plot_ly(lj$data, x = ~RunID, y = ~Cq_mean,
                        type = 'scatter', mode = 'markers+lines',
                        name = 'Run Mean',
                        marker = list(size = 12, color = '#2c3e50'),
                        line = list(width = 3, color = '#2c3e50'),
                        hovertemplate = paste0(
                          "<b>Run: %{x}</b><br>",
                          "Mean Cq: %{y:.2f}<br>",
                          "<extra></extra>"
                        )) %>%
          add_lines(data = lj$data, y = ~Mean, name = 'Mean',
                    line = list(color = 'black', width = 2, dash = 'solid'),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~plus1, name = '+1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~minus1, name = '-1 SD',
                    line = list(color = '#3498db', dash = 'dot', width = 1),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~plus2, name = '+2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~minus2, name = '-2 SD',
                    line = list(color = '#f39c12', dash = 'dash', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~plus3, name = '+3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          add_lines(data = lj$data, y = ~minus3, name = '-3 SD',
                    line = list(color = '#e74c3c', dash = 'dashdot', width = 2),
                    mode = 'lines',
                    hoverinfo = 'skip',
                    inherit = FALSE) %>%
          layout(
            xaxis = list(title = "Run ID", tickangle = -45, automargin = TRUE),
            yaxis = list(title = "Cq Value", automargin = TRUE),
            legend = list(orientation = 'h', y = -0.25, x = 0),
            margin = list(t = 60, r = 40, b = 120, l = 60),
            hovermode = 'closest'
          )

        if (isTRUE(lj$fallback)) {
          fig <- fig %>%
            layout(
              annotations = list(
                list(
                  text = "No control wells detected – showing all samples",
                  x = 0.5,
                  xref = "paper",
                  y = 1.1,
                  yref = "paper",
                  showarrow = FALSE,
                  font = list(color = "#e74c3c", size = 12)
                )
              )
            )
        }

        fig
      })
    }
    
    # Render all 4 LJ plots
    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")
    
  })
}
