# ELISA Concordance Summary Module
# Displays KPIs, metrics, and confusion matrix

# UI ----
mod_elisa_concordance_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # KPI Cards
    h4("Overview Metrics", class = "mt-3 mb-3"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Total Matched Samples",
        value = textOutput(ns("kpi_total")),
        showcase = bsicons::bs_icon("link-45deg"),
        theme = "primary"
      ),
      value_box(
        title = "Concordant",
        value = textOutput(ns("kpi_concordant")),
        showcase = bsicons::bs_icon("check-circle"),
        theme = "success"
      ),
      value_box(
        title = "Discordant",
        value = textOutput(ns("kpi_discordant")),
        showcase = bsicons::bs_icon("x-circle"),
        theme = "warning"
      ),
      value_box(
        title = "Co-positivity Rate",
        value = textOutput(ns("kpi_copositivity")),
        showcase = bsicons::bs_icon("plus-circle"),
        theme = "info"
      )
    ),

    # Concordance breakdown
    h4("Concordance Breakdown", class = "mt-4 mb-3"),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        title = "Both Positive",
        value = textOutput(ns("kpi_both_pos")),
        showcase = bsicons::bs_icon("check2-all"),
        theme = "success",
        showcase_layout = "left center"
      ),
      value_box(
        title = "Both Negative",
        value = textOutput(ns("kpi_both_neg")),
        showcase = bsicons::bs_icon("dash-circle"),
        theme = "secondary",
        showcase_layout = "left center"
      ),
      value_box(
        title = "PE+ / VSG-",
        value = textOutput(ns("kpi_pe_only")),
        showcase = bsicons::bs_icon("arrow-right-circle"),
        theme = "warning",
        showcase_layout = "left center"
      ),
      value_box(
        title = "PE- / VSG+",
        value = textOutput(ns("kpi_vsg_only")),
        showcase = bsicons::bs_icon("arrow-left-circle"),
        theme = "warning",
        showcase_layout = "left center"
      )
    ),

    # Statistical Metrics and Confusion Matrix
    h4("Statistical Analysis", class = "mt-4 mb-3"),
    layout_columns(
      col_widths = c(6, 6),

      # Statistical metrics card
      card(
        card_header("Agreement Metrics"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            div(
              h6("Cohen's Kappa", class = "text-muted"),
              h4(textOutput(ns("metric_kappa")), class = "mb-3")
            ),
            div(
              h6("Overall Agreement", class = "text-muted"),
              h4(textOutput(ns("metric_agreement")), class = "mb-3")
            )
          ),
          hr(),
          h6("Diagnostic Performance (using PE as reference)", class = "text-muted mb-3"),
          layout_columns(
            col_widths = c(6, 6),
            div(
              tags$dl(
                tags$dt("Sensitivity"),
                tags$dd(textOutput(ns("metric_sensitivity")), class = "text-primary"),
                tags$dt("Specificity"),
                tags$dd(textOutput(ns("metric_specificity")), class = "text-primary")
              )
            ),
            div(
              tags$dl(
                tags$dt("PPV (Positive Predictive Value)"),
                tags$dd(textOutput(ns("metric_ppv")), class = "text-success"),
                tags$dt("NPV (Negative Predictive Value)"),
                tags$dd(textOutput(ns("metric_npv")), class = "text-success")
              )
            )
          )
        )
      ),

      # Confusion matrix card
      card(
        card_header("Confusion Matrix (2x2 Table)"),
        card_body(
          div(
            class = "table-responsive",
            tableOutput(ns("confusion_matrix"))
          ),
          p(class = "text-muted small mt-2 mb-0",
            "Rows: PE-PGRS results | Columns: VSG results")
        )
      )
    ),

    # Interpretation guide
    card(
      card_header(
        class = "bg-light",
        icon("info-circle"),
        " Interpretation Guide"
      ),
      card_body(
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            h6("Cohen's Kappa Interpretation:"),
            tags$ul(
              class = "small",
              tags$li("< 0.20: Slight agreement"),
              tags$li("0.20 - 0.40: Fair agreement"),
              tags$li("0.40 - 0.60: Moderate agreement"),
              tags$li("0.60 - 0.80: Substantial agreement"),
              tags$li("> 0.80: Almost perfect agreement")
            )
          ),
          div(
            h6("Concordance:"),
            p(class = "small",
              strong("Concordant:"), " Both tests agree (both positive or both negative)"),
            p(class = "small",
              strong("Discordant:"), " Tests disagree (one positive, one negative)")
          ),
          div(
            h6("Co-positivity:"),
            p(class = "small",
              "Proportion of samples that are positive in ", strong("both"), " PE and VSG tests."),
            p(class = "small text-muted",
              "High co-positivity suggests the tests detect similar infections.")
          )
        )
      )
    )
  )
}

# Server ----
mod_elisa_concordance_summary_server <- function(id, concordance_results) {
  moduleServer(id, function(input, output, session) {

    # Extract metrics
    metrics <- reactive({
      concordance_results()$metrics
    })

    # KPI outputs
    output$kpi_total <- renderText({
      m <- metrics()
      format(m$n_total, big.mark = ",")
    })

    output$kpi_concordant <- renderText({
      m <- metrics()
      format_count_pct(m$n_concordant, m$n_total)
    })

    output$kpi_discordant <- renderText({
      m <- metrics()
      format_count_pct(m$n_discordant, m$n_total)
    })

    output$kpi_copositivity <- renderText({
      m <- metrics()
      format_count_pct(m$n_both_positive, m$n_total)
    })

    # Breakdown outputs
    output$kpi_both_pos <- renderText({
      m <- metrics()
      format_count_pct(m$n_both_positive, m$n_total)
    })

    output$kpi_both_neg <- renderText({
      m <- metrics()
      format_count_pct(m$n_both_negative, m$n_total)
    })

    output$kpi_pe_only <- renderText({
      m <- metrics()
      format_count_pct(m$n_pe_only_positive, m$n_total)
    })

    output$kpi_vsg_only <- renderText({
      m <- metrics()
      format_count_pct(m$n_vsg_only_positive, m$n_total)
    })

    # Statistical metrics
    output$metric_kappa <- renderText({
      m <- metrics()
      format_kappa(m$kappa)
    })

    output$metric_agreement <- renderText({
      m <- metrics()
      format_pct(m$pct_concordant)
    })

    output$metric_sensitivity <- renderText({
      m <- metrics()
      if (is.na(m$sensitivity)) return("N/A")
      sprintf("%.1f%%", m$sensitivity * 100)
    })

    output$metric_specificity <- renderText({
      m <- metrics()
      if (is.na(m$specificity)) return("N/A")
      sprintf("%.1f%%", m$specificity * 100)
    })

    output$metric_ppv <- renderText({
      m <- metrics()
      if (is.na(m$ppv)) return("N/A")
      sprintf("%.1f%%", m$ppv * 100)
    })

    output$metric_npv <- renderText({
      m <- metrics()
      if (is.na(m$npv)) return("N/A")
      sprintf("%.1f%%", m$npv * 100)
    })

    # Confusion matrix
    output$confusion_matrix <- renderTable({
      cm <- concordance_results()$confusion_matrix

      # Format the table nicely
      cm %>%
        mutate(
          `PE Result` = pe_result,
          `VSG Positive` = format(vsg_positive, big.mark = ","),
          `VSG Negative` = format(vsg_negative, big.mark = ","),
          Total = format(vsg_positive + vsg_negative, big.mark = ",")
        ) %>%
        select(`PE Result`, `VSG Positive`, `VSG Negative`, Total)
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    spacing = "s",
    width = "100%",
    align = "lrrr"
    )
  })
}
