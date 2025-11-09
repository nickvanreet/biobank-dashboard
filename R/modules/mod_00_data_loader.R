# R/modules/mod_00_data_loader.R

mod_data_loader_ui <- function(id) {
  ns <- NS(id)
  
  sidebar(
    width = 280,
    h5("Data Source"),
    textInput(ns("data_dir"), "Directory", value = app_config$paths$biobank_dir),
    uiOutput(ns("file_selector")),
    actionButton(ns("load_data"), "Load Data", class = "btn-primary w-100 mb-3"),
    
    hr(),
    h5("Filters"),
    dateRangeInput(ns("date_range"), "Sample Date", 
                   start = Sys.Date() - 180, end = Sys.Date()),
    selectInput(ns("filter_study"), "Study", choices = c("All" = "all")),
    selectInput(ns("filter_province"), "Province", choices = c("All" = "all")),
    selectInput(ns("filter_zone"), "Zone", choices = c("All" = "all")),
    
    hr(),
    h5("Data Status"),
    uiOutput(ns("data_status"))
  )
}

mod_data_loader_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values
    raw_data <- reactiveVal(NULL)
    clean_data <- reactiveVal(NULL)
    data_quality <- reactiveVal(NULL)
    
    # File loading logic
    observeEvent(input$load_data, {
      req(input$selected_file)
      
      # Load raw data
      df_raw <- load_biobank_file(input$selected_file)
      raw_data(df_raw)
      
      # Analyze quality
      dq <- analyze_data_quality(df_raw)
      data_quality(dq)
      
      # Clean data
      df_clean <- clean_biobank_data(df_raw)
      clean_data(df_clean)
      
      # Update filters
      update_filter_choices(session, df_clean)
    })
    
    # Filtered data
    filtered_data <- reactive({
      df <- clean_data()
      req(df)
      
      apply_filters(df, 
                    date_range = input$date_range,
                    study = input$filter_study,
                    province = input$filter_province,
                    zone = input$filter_zone)
    })
    
    # Return reactive data
    list(
      raw_data = raw_data,
      clean_data = clean_data,
      filtered_data = filtered_data,
      data_quality = data_quality
    )
  })
}