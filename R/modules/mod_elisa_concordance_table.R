# ELISA Concordance Table Module
# Displays detailed sample-by-sample comparison table

# UI ----
mod_elisa_concordance_table_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Summary and export buttons
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_body(
          p(class = "mb-1",
            strong("Matched Samples: "),
            textOutput(ns("n_samples"), inline = TRUE)),
          p(class = "mb-0 small text-muted",
            "Use the filters below each column to search and filter the table")
        )
      ),
      card(
        card_body(
          class = "text-end",
          p(class = "mb-2 small text-muted", "Export data:"),
          div(
            class = "btn-group btn-group-sm",
            downloadButton(ns("download_csv"), "CSV", class = "btn-sm"),
            downloadButton(ns("download_excel"), "Excel", class = "btn-sm")
          )
        )
      )
    ),

    # Main comparison table
    card(
      card_body(
        class = "p-0",
        div(
          class = "table-responsive",
          DT::dataTableOutput(ns("comparison_table"))
        )
      )
    ),

    # Legend
    card(
      card_header(
        class = "bg-light",
        icon("info-circle"),
        " Legend"
      ),
      card_body(
        class = "small",
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            h6("Concordance Status:"),
            tags$ul(
              tags$li(
                span(class = "badge bg-success", "Both Positive"),
                " - Both tests positive"
              ),
              tags$li(
                span(class = "badge bg-secondary", "Both Negative"),
                " - Both tests negative"
              ),
              tags$li(
                span(class = "badge bg-warning", "PE+ / VSG-"),
                " - PE positive, VSG negative"
              ),
              tags$li(
                span(class = "badge bg-warning", "PE- / VSG+"),
                " - PE negative, VSG positive"
              )
            )
          ),
          div(
            h6("Result Status:"),
            tags$ul(
              tags$li(
                span(class = "badge bg-danger", "Positive"),
                " - Sample meets positivity threshold"
              ),
              tags$li(
                span(class = "badge bg-success", "Negative"),
                " - Sample below positivity threshold"
              )
            )
          ),
          div(
            h6("QC Status:"),
            tags$ul(
              tags$li(
                span(class = "badge bg-success", "✓"),
                " - Passed QC"
              ),
              tags$li(
                span(class = "badge bg-danger", "✗"),
                " - Failed QC"
              )
            )
          )
        )
      )
    )
  )
}

# Server ----
mod_elisa_concordance_table_server <- function(id, concordance_results) {
  moduleServer(id, function(input, output, session) {

    # Classified data
    classified_data <- reactive({
      concordance_results()$data
    })

    # Number of samples
    output$n_samples <- renderText({
      format(nrow(classified_data()), big.mark = ",")
    })

    # Prepare table data
    table_data <- reactive({
      data <- classified_data()

      if (nrow(data) == 0) {
        return(tibble())
      }

      data %>%
        mutate(
          # Format identifiers
          sample_id = coalesce(pe_barcode, vsg_barcode, pe_numero, vsg_numero),
          barcode = paste0(
            ifelse(!is.na(pe_barcode), pe_barcode, "-"),
            " / ",
            ifelse(!is.na(vsg_barcode), vsg_barcode, "-")
          ),
          lab_number = paste0(
            ifelse(!is.na(pe_numero), pe_numero, "-"),
            " / ",
            ifelse(!is.na(vsg_numero), vsg_numero, "-")
          ),

          # Format dates
          pe_date = format(pe_plate_date, "%Y-%m-%d"),
          vsg_date = format(vsg_plate_date, "%Y-%m-%d"),

          # Format numeric values
          pe_dod_fmt = sprintf("%.3f", pe_DOD),
          vsg_dod_fmt = sprintf("%.3f", vsg_DOD),
          pe_pp_fmt = sprintf("%.1f%%", pe_PP_percent),
          vsg_pp_fmt = sprintf("%.1f%%", vsg_PP_percent),

          # Result status
          pe_result = ifelse(pe_positive, "Positive", "Negative"),
          vsg_result = ifelse(vsg_positive, "Positive", "Negative"),

          # QC status
          pe_qc = ifelse(pe_qc_overall, "✓", "✗"),
          vsg_qc = ifelse(vsg_qc_overall, "✓", "✗"),

          # Biobank fields
          province = ifelse(is.na(Province), "-", as.character(Province)),
          health_zone = ifelse(is.na(HealthZone), "-", as.character(HealthZone)),
          structure = ifelse(is.na(Structure), "-", as.character(Structure)),
          sex = ifelse(is.na(Sex), "-", as.character(Sex)),
          age_group = ifelse(is.na(AgeGroup), "-", as.character(AgeGroup))
        ) %>%
        select(
          sample_id,
          barcode,
          lab_number,
          match_method,
          concordance_type,
          pe_date,
          pe_dod_fmt,
          pe_pp_fmt,
          pe_result,
          pe_qc,
          vsg_date,
          vsg_dod_fmt,
          vsg_pp_fmt,
          vsg_result,
          vsg_qc,
          province,
          health_zone,
          structure,
          sex,
          age_group
        )
    })

    # Render DataTable
    output$comparison_table <- DT::renderDataTable({
      data <- table_data()

      if (nrow(data) == 0) {
        return(DT::datatable(
          tibble(Message = "No matched samples found"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      DT::datatable(
        data,
        filter = "top",
        rownames = FALSE,
        colnames = c(
          "Sample ID" = "sample_id",
          "Barcode (PE / VSG)" = "barcode",
          "Lab Number (PE / VSG)" = "lab_number",
          "Match Method" = "match_method",
          "Concordance" = "concordance_type",
          "PE Date" = "pe_date",
          "PE DOD" = "pe_dod_fmt",
          "PE PP%" = "pe_pp_fmt",
          "PE Result" = "pe_result",
          "PE QC" = "pe_qc",
          "VSG Date" = "vsg_date",
          "VSG DOD" = "vsg_dod_fmt",
          "VSG PP%" = "vsg_pp_fmt",
          "VSG Result" = "vsg_result",
          "VSG QC" = "vsg_qc",
          "Province" = "province",
          "Health Zone" = "health_zone",
          "Structure" = "structure",
          "Sex" = "sex",
          "Age Group" = "age_group"
        ),
        extensions = c('Buttons', 'FixedHeader'),
        options = list(
          dom = 'Bfrtip',
          buttons = list(
            list(extend = 'copy', className = 'btn-sm'),
            list(extend = 'csv', className = 'btn-sm'),
            list(extend = 'excel', className = 'btn-sm')
          ),
          fixedHeader = TRUE,
          scrollX = TRUE,
          scrollY = "500px",
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, 500),
          order = list(list(4, 'asc'))  # Sort by concordance type
        )
      ) %>%
        # Color code concordance type
        DT::formatStyle(
          'concordance_type',
          backgroundColor = DT::styleEqual(
            c("Both Positive", "Both Negative", "PE+ / VSG-", "PE- / VSG+"),
            c("#d4edda", "#e2e3e5", "#fff3cd", "#fff3cd")
          ),
          fontWeight = 'bold'
        ) %>%
        # Color code PE result
        DT::formatStyle(
          'pe_result',
          backgroundColor = DT::styleEqual(
            c("Positive", "Negative"),
            c("#f8d7da", "#d4edda")
          )
        ) %>%
        # Color code VSG result
        DT::formatStyle(
          'vsg_result',
          backgroundColor = DT::styleEqual(
            c("Positive", "Negative"),
            c("#f8d7da", "#d4edda")
          )
        ) %>%
        # Color code PE QC
        DT::formatStyle(
          'pe_qc',
          backgroundColor = DT::styleEqual(
            c("✓", "✗"),
            c("#d4edda", "#f8d7da")
          ),
          textAlign = 'center'
        ) %>%
        # Color code VSG QC
        DT::formatStyle(
          'vsg_qc',
          backgroundColor = DT::styleEqual(
            c("✓", "✗"),
            c("#d4edda", "#f8d7da")
          ),
          textAlign = 'center'
        )
    })

    # Download handlers
    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("elisa_concordance_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write_csv(classified_data(), file)
      }
    )

    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("elisa_concordance_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        write_xlsx(classified_data(), file)
      }
    )
  })
}
