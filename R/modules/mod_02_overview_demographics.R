# R/modules/mod_02_overview_demographics.R
# Overview & Demographics Module
# ============================================================================

# ============================================================================
# MODULE UI
# ============================================================================

#' Overview & Demographics Module UI
#' @param id Module namespace ID
#' @export
mod_overview_demographics_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Overview & Demographics",
    icon = icon("chart-pie"),
    
    div(class = "container-fluid",
        
        # ==== OVERVIEW SECTION ================================================
        h4(class = "mb-3", icon("chart-line"), " Summary Overview"),
        
        # KPI Cards (Top Row)
        layout_column_wrap(
          width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          
          value_box(
            title = "Total Samples",
            value = textOutput(ns("total_samples")),
            showcase = icon("vial"),
            theme = "primary"
          ),
          value_box(
            title = "Active Screening (DA)",
            value = textOutput(ns("da_samples")),
            showcase = icon("users"),
            theme = "info"
          ),
          value_box(
            title = "Passive Screening (DP)",
            value = textOutput(ns("dp_samples")),
            showcase = icon("user"),
            theme = "success"
          ),
          value_box(
            title = "Date Range",
            value = textOutput(ns("date_range")),
            showcase = icon("calendar"),
            theme = "secondary"
          )
        ),
        
        # Geographic and Study Distribution
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          
          card(
            card_header("Samples by Province"),
            card_body_fill(
              plotly::plotlyOutput(ns("province_plot"), height = "350px")
            )
          ),
          card(
            card_header("Samples by Health Zone"),
            card_body_fill(
              plotly::plotlyOutput(ns("zone_plot"), height = "350px")
            )
          )
        ),
        
        # Timeline
        card(
          full_screen = TRUE,
          card_header("Sample Collection Timeline"),
          card_body_fill(
            plotly::plotlyOutput(ns("timeline_plot"), height = "400px")
          )
        ),
        
        # ==== DEMOGRAPHICS SECTION ============================================
        h4(class = "mt-4 mb-3", icon("users"), " Demographics Analysis"),
        
        # Demographics KPIs
        layout_column_wrap(
          width = 1/4, fixed_width = TRUE, heights_equal = "row", gap = "12px",
          
          value_box(
            title = "Male",
            value = textOutput(ns("male_count")),
            showcase = icon("mars"),
            theme = "info"
          ),
          value_box(
            title = "Female",
            value = textOutput(ns("female_count")),
            showcase = icon("venus"),
            theme = "danger"
          ),
          value_box(
            title = "Median Age",
            value = textOutput(ns("median_age")),
            showcase = icon("birthday-cake"),
            theme = "success"
          ),
          value_box(
            title = "Age Range",
            value = textOutput(ns("age_range")),
            showcase = icon("arrows-alt-h"),
            theme = "secondary"
          )
        ),
        
        # Demographics Charts
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          
          card(
            card_header("Age Distribution"),
            card_body_fill(
              plotly::plotlyOutput(ns("age_histogram"), height = "350px")
            )
          ),
          card(
            card_header("Age by Sex"),
            card_body_fill(
              plotly::plotlyOutput(ns("age_sex_box"), height = "350px")
            )
          )
        ),
        
        layout_columns(
          col_widths = c(6, 6), gap = "16px",
          
          card(
            card_header("Age Groups by Sex"),
            card_body_fill(
              plotly::plotlyOutput(ns("age_group_bar"), height = "350px")
            )
          ),
          card(
            card_header("Sex Distribution by Study Type"),
            card_body_fill(
              plotly::plotlyOutput(ns("sex_study_bar"), height = "350px")
            )
          )
        ),
        
        # Demographics Summary Table
        card(
          card_header("Demographic Summary by Health Zone"),
          card_body(
            DT::DTOutput(ns("demographics_table"))
          )
        )
    )
  )
}

# ============================================================================
# MODULE SERVER
# ============================================================================

#' Overview & Demographics Module Server
#' @param id Module namespace ID
#' @param filtered_data Reactive expression containing filtered data
#' @export
mod_overview_demographics_server <- function(id, filtered_data) {
  
  moduleServer(id, function(input, output, session) {
    case_category_levels <- c("New case", "Previous/Treated", "Uncertain", "Unknown")
    case_category_colors <- c(
      "New case" = "#3498DB",
      "Previous/Treated" = "#E67E22",
      "Uncertain" = "#9B59B6",
      "Unknown" = "#95A5A6"
    )

    case_enriched_data <- reactive({
      req(filtered_data())
      data <- filtered_data()

      if (nrow(data) == 0) {
        return(data)
      }

      if (!"previous_case" %in% names(data)) {
        data$previous_case <- NA_character_
      }

      if (!"treated" %in% names(data)) {
        data$treated <- NA_character_
      }

      existing_case_category <- if ("case_category" %in% names(data)) {
        data$case_category
      } else {
        rep(NA_character_, nrow(data))
      }

      data$case_category <- dplyr::case_when(
        !is.na(existing_case_category) & existing_case_category != "" ~ existing_case_category,
        data$previous_case == "Oui" | data$treated == "Oui" ~ "Previous/Treated",
        data$previous_case == "Non" & data$treated == "Non" ~ "New case",
        data$previous_case == "Incertain" | data$treated == "Incertain" ~ "Uncertain",
        TRUE ~ "Unknown"
      )

      data$case_category <- factor(data$case_category, levels = case_category_levels)

      data
    })
    
    # ========================================================================
    # REACTIVE CALCULATIONS - OVERVIEW
    # ========================================================================
    
    overview_metrics <- reactive({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0) {
        return(list(
          total = 0,
          da_count = 0,
          dp_count = 0,
          da_pct = 0,
          dp_pct = 0,
          date_min = NA,
          date_max = NA
        ))
      }
      
      total <- nrow(data)
      
      # Study counts
      da_count <- if ("study" %in% names(data)) {
        sum(data$study == "DA", na.rm = TRUE)
      } else 0
      
      dp_count <- if ("study" %in% names(data)) {
        sum(data$study == "DP", na.rm = TRUE)
      } else 0
      
      # Date range
      date_range <- if ("date_sample" %in% names(data)) {
        c(min(data$date_sample, na.rm = TRUE),
          max(data$date_sample, na.rm = TRUE))
      } else c(NA, NA)
      
      list(
        total = total,
        da_count = da_count,
        dp_count = dp_count,
        da_pct = if (total > 0) da_count / total * 100 else 0,
        dp_pct = if (total > 0) dp_count / total * 100 else 0,
        date_min = date_range[1],
        date_max = date_range[2]
      )
    })
    
    # ========================================================================
    # REACTIVE CALCULATIONS - DEMOGRAPHICS
    # ========================================================================
    
    demographics_metrics <- reactive({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"sex" %in% names(data) || !"age" %in% names(data)) {
        return(list(
          male_count = 0,
          female_count = 0,
          male_pct = 0,
          female_pct = 0,
          median_age = NA,
          age_min = NA,
          age_max = NA
        ))
      }
      
      # Sex counts
      male_count <- sum(data$sex == "M", na.rm = TRUE)
      female_count <- sum(data$sex == "F", na.rm = TRUE)
      total_with_sex <- male_count + female_count
      
      # Age statistics
      ages <- data$age[!is.na(data$age)]
      
      list(
        male_count = male_count,
        female_count = female_count,
        male_pct = if (total_with_sex > 0) male_count / total_with_sex * 100 else 0,
        female_pct = if (total_with_sex > 0) female_count / total_with_sex * 100 else 0,
        median_age = if (length(ages) > 0) median(ages) else NA,
        age_min = if (length(ages) > 0) min(ages) else NA,
        age_max = if (length(ages) > 0) max(ages) else NA
      )
    })
    
    # ========================================================================
    # OUTPUTS - OVERVIEW KPIs
    # ========================================================================
    
    output$total_samples <- renderText({
      m <- overview_metrics()
      scales::comma(m$total)
    })
    
    output$da_samples <- renderText({
      m <- overview_metrics()
      sprintf("%s (%.1f%%)", scales::comma(m$da_count), m$da_pct)
    })
    
    output$dp_samples <- renderText({
      m <- overview_metrics()
      sprintf("%s (%.1f%%)", scales::comma(m$dp_count), m$dp_pct)
    })
    
    output$date_range <- renderText({
      m <- overview_metrics()
      if (is.na(m$date_min) || is.na(m$date_max)) {
        "No dates"
      } else {
        sprintf("%s to %s",
                format(m$date_min, "%b %Y"),
                format(m$date_max, "%b %Y"))
      }
    })
    
    # ========================================================================
    # OUTPUTS - DEMOGRAPHICS KPIs
    # ========================================================================
    
    output$male_count <- renderText({
      m <- demographics_metrics()
      sprintf("%s (%.1f%%)", scales::comma(m$male_count), m$male_pct)
    })
    
    output$female_count <- renderText({
      m <- demographics_metrics()
      sprintf("%s (%.1f%%)", scales::comma(m$female_count), m$female_pct)
    })
    
    output$median_age <- renderText({
      m <- demographics_metrics()
      if (is.na(m$median_age)) "N/A" else sprintf("%.0f years", m$median_age)
    })
    
    output$age_range <- renderText({
      m <- demographics_metrics()
      if (is.na(m$age_min) || is.na(m$age_max)) {
        "N/A"
      } else {
        sprintf("%d - %d years", m$age_min, m$age_max)
      }
    })
    
    # ========================================================================
    # OUTPUTS - OVERVIEW PLOTS
    # ========================================================================
    
    output$province_plot <- plotly::renderPlotly({
      req(case_enriched_data())
      data <- case_enriched_data()

      if (nrow(data) == 0 || !"province" %in% names(data)) {
        return(plotly::plotly_empty())
      }

      province_totals <- data %>%
        dplyr::filter(!is.na(province)) %>%
        dplyr::count(province, name = "total") %>%
        dplyr::arrange(dplyr::desc(total))

      province_data <- data %>%
        dplyr::filter(!is.na(province)) %>%
        dplyr::count(province, case_category, name = "count") %>%
        dplyr::left_join(province_totals, by = "province") %>%
        dplyr::mutate(province = factor(province, levels = province_totals$province))

      if (nrow(province_data) == 0) {
        return(plotly::plotly_empty())
      }

      province_colors <- case_category_colors[names(case_category_colors) %in%
                                                unique(as.character(province_data$case_category))]
      if (length(province_colors) == 0) {
        province_colors <- case_category_colors
      }

      plotly::plot_ly(
        province_data,
        x = ~province,
        y = ~count,
        color = ~case_category,
        colors = province_colors,
        type = "bar",
        hovertemplate = "Province: %{x}<br>Case category: %{fullData.name}<br>Count: %{y}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = as.character(province_totals$province)
          ),
          yaxis = list(title = "Number of Samples"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$zone_plot <- plotly::renderPlotly({
      req(case_enriched_data())
      data <- case_enriched_data()

      if (nrow(data) == 0 || !"health_zone" %in% names(data)) {
        return(plotly::plotly_empty())
      }

      zone_totals <- data %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::count(health_zone, name = "total") %>%
        dplyr::arrange(dplyr::desc(total)) %>%
        dplyr::slice_head(n = 15)

      zone_data <- data %>%
        dplyr::filter(!is.na(health_zone), health_zone %in% zone_totals$health_zone) %>%
        dplyr::count(health_zone, case_category, name = "count") %>%
        dplyr::left_join(zone_totals, by = "health_zone") %>%
        dplyr::mutate(health_zone = factor(health_zone, levels = zone_totals$health_zone))

      if (nrow(zone_data) == 0) {
        return(plotly::plotly_empty())
      }

      zone_colors <- case_category_colors[names(case_category_colors) %in%
                                            unique(as.character(zone_data$case_category))]
      if (length(zone_colors) == 0) {
        zone_colors <- case_category_colors
      }

      plotly::plot_ly(
        zone_data,
        x = ~count,
        y = ~health_zone,
        color = ~case_category,
        colors = zone_colors,
        type = "bar",
        orientation = "h",
        hovertemplate = "Health Zone: %{y}<br>Case category: %{fullData.name}<br>Count: %{x}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Number of Samples"),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(as.character(zone_totals$health_zone))
          ),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    output$timeline_plot <- plotly::renderPlotly({
      req(case_enriched_data())
      data <- case_enriched_data()

      if (nrow(data) == 0 || !"date_sample" %in% names(data)) {
        return(plotly::plotly_empty())
      }

      # Aggregate by week and study type
      timeline_data <- data %>%
        dplyr::filter(!is.na(date_sample)) %>%
        dplyr::mutate(week = lubridate::floor_date(date_sample, "week"))

      if ("study" %in% names(data) && any(!is.na(data$study))) {
        timeline_data <- timeline_data %>%
          dplyr::filter(!is.na(study)) %>%
          dplyr::count(week, study) %>%
          dplyr::arrange(week)

        if (nrow(timeline_data) == 0) {
          return(plotly::plotly_empty())
        }

        study_levels <- unique(as.character(timeline_data$study))
        study_colors <- grDevices::hcl.colors(length(study_levels), palette = "Dynamic")
        names(study_colors) <- study_levels

        plotly::plot_ly(
          timeline_data,
          x = ~week,
          y = ~n,
          color = ~study,
          colors = study_colors,
          type = "scatter",
          mode = "lines+markers",
          hovertemplate = "Week: %{x|%Y-%m-%d}<br>Study: %{fullData.name}<br>Samples: %{y}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Number of Samples"),
            legend = list(orientation = "h", y = -0.15)
          )
      } else if ("case_category" %in% names(data)) {
        timeline_data <- timeline_data %>%
          dplyr::count(week, case_category) %>%
          dplyr::arrange(week)

        if (nrow(timeline_data) == 0) {
          return(plotly::plotly_empty())
        }

        timeline_colors <- case_category_colors[names(case_category_colors) %in%
                                                  unique(as.character(timeline_data$case_category))]
        if (length(timeline_colors) == 0) {
          timeline_colors <- case_category_colors
        }

        plotly::plot_ly(
          timeline_data,
          x = ~week,
          y = ~n,
          color = ~case_category,
          colors = timeline_colors,
          type = "scatter",
          mode = "lines+markers",
          hovertemplate = "Week: %{x|%Y-%m-%d}<br>Case category: %{fullData.name}<br>Samples: %{y}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Number of Samples"),
            legend = list(orientation = "h", y = -0.15)
          )
      } else {
        timeline_data <- timeline_data %>%
          dplyr::count(week) %>%
          dplyr::arrange(week)

        plotly::plot_ly(
          timeline_data,
          x = ~week,
          y = ~n,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = "#3498DB", width = 2),
          marker = list(size = 6),
          hovertemplate = "Week: %{x|%Y-%m-%d}<br>Samples: %{y}<extra></extra>"
        ) %>%
          plotly::layout(
            xaxis = list(title = "Week"),
            yaxis = list(title = "Number of Samples"),
            showlegend = FALSE
          )
      }
    })
    
    # ========================================================================
    # OUTPUTS - DEMOGRAPHICS PLOTS
    # ========================================================================
    
    output$age_histogram <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"age" %in% names(data)) {
        return(plotly::plotly_empty())
      }
      
      age_data <- data %>%
        dplyr::filter(!is.na(age))
      
      plotly::plot_ly(
        age_data,
        x = ~age,
        type = "histogram",
        marker = list(color = "#3498DB"),
        hovertemplate = "Age: %{x}<br>Count: %{y}<extra></extra>",
        nbinsx = 30
      ) %>%
        plotly::layout(
          xaxis = list(title = "Age (years)"),
          yaxis = list(title = "Number of Samples"),
          showlegend = FALSE
        )
    })
    
    output$age_sex_box <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"age" %in% names(data) || !"sex" %in% names(data)) {
        return(plotly::plotly_empty())
      }
      
      box_data <- data %>%
        dplyr::filter(!is.na(age), !is.na(sex), sex %in% c("M", "F"))
      
      plotly::plot_ly(
        box_data,
        x = ~sex,
        y = ~age,
        color = ~sex,
        colors = c(M = "#3498DB", F = "#E91E63"),
        type = "box",
        hovertemplate = "Sex: %{x}<br>Age: %{y}<extra></extra>"
      ) %>%
        plotly::layout(
          xaxis = list(title = "Sex"),
          yaxis = list(title = "Age (years)"),
          showlegend = FALSE
        )
    })
    
    output$age_group_bar <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"age" %in% names(data) || !"sex" %in% names(data)) {
        return(plotly::plotly_empty())
      }
      
      # Create age groups if not already present
      age_group_data <- data %>%
        dplyr::filter(!is.na(age), !is.na(sex), sex %in% c("M", "F")) %>%
        dplyr::mutate(
          age_group = cut(
            age,
            breaks = c(0, 5, 15, 25, 35, 45, 55, 65, Inf),
            labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
            right = FALSE
          )
        ) %>%
        dplyr::count(age_group, sex) %>%
        dplyr::arrange(age_group)
      
      plotly::plot_ly(
        age_group_data,
        x = ~age_group,
        y = ~n,
        color = ~sex,
        colors = c(M = "#3498DB", F = "#E91E63"),
        type = "bar",
        hovertemplate = "Age Group: %{x}<br>Sex: %{fullData.name}<br>Count: %{y}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Age Group"),
          yaxis = list(title = "Number of Samples"),
          legend = list(orientation = "h", y = -0.15)
        )
    })
    
    output$sex_study_bar <- plotly::renderPlotly({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"sex" %in% names(data) || !"study" %in% names(data)) {
        return(plotly::plotly_empty())
      }
      
      sex_study_data <- data %>%
        dplyr::filter(!is.na(sex), !is.na(study), sex %in% c("M", "F")) %>%
        dplyr::count(study, sex) %>%
        dplyr::arrange(study, sex)
      
      plotly::plot_ly(
        sex_study_data,
        x = ~study,
        y = ~n,
        color = ~sex,
        colors = c(M = "#3498DB", F = "#E91E63"),
        type = "bar",
        hovertemplate = "Study: %{x}<br>Sex: %{fullData.name}<br>Count: %{y}<extra></extra>"
      ) %>%
        plotly::layout(
          barmode = "group",
          xaxis = list(title = "Study Type"),
          yaxis = list(title = "Number of Samples"),
          legend = list(orientation = "h", y = -0.15)
        )
    })
    
    # ========================================================================
    # OUTPUTS - DEMOGRAPHICS TABLE
    # ========================================================================
    
    output$demographics_table <- DT::renderDT({
      req(filtered_data())
      data <- filtered_data()
      
      if (nrow(data) == 0 || !"health_zone" %in% names(data)) {
        return(DT::datatable(
          data.frame(Message = "No data available"),
          options = list(dom = 't', paging = FALSE)
        ))
      }
      
      # Create summary by health zone
      summary_data <- data %>%
        dplyr::filter(!is.na(health_zone)) %>%
        dplyr::group_by(health_zone) %>%
        dplyr::summarise(
          total = dplyr::n(),
          male = sum(sex == "M", na.rm = TRUE),
          female = sum(sex == "F", na.rm = TRUE),
          pct_male = round(male / sum(!is.na(sex)) * 100, 1),
          pct_female = round(female / sum(!is.na(sex)) * 100, 1),
          median_age = round(median(age, na.rm = TRUE), 1),
          age_range = sprintf("%d-%d", 
                              min(age, na.rm = TRUE), 
                              max(age, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(total)) %>%
        dplyr::rename(
          `Health Zone` = health_zone,
          `Total` = total,
          `Male` = male,
          `Female` = female,
          `% Male` = pct_male,
          `% Female` = pct_female,
          `Median Age` = median_age,
          `Age Range` = age_range
        )
      
      DT::datatable(
        summary_data,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = 'frtip'
        ),
        class = "table-sm"
      ) %>%
        DT::formatStyle(
          "Total",
          background = DT::styleColorBar(range(summary_data$Total), "#3498DB"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })
    
  })
}
