# R/data/data_cleaner.R

#' Load column mappings from JSON
load_column_mappings <- function(path = "data/column_mappings.json") {
  jsonlite::fromJSON(path)$column_mappings
}

#' Smart column renaming using configuration
smart_rename_columns <- function(df, mappings = load_column_mappings()) {
  current_names <- names(df)
  
  for (mapping_name in names(mappings)) {
    mapping <- mappings[[mapping_name]]
    target <- mapping$target_name
    patterns <- mapping$patterns
    
    # Find matching column
    for (pattern in patterns) {
      matches <- grep(pattern, current_names, ignore.case = TRUE, value = TRUE)
      if (length(matches) > 0) {
        # Use first match
        old_name <- matches[1]
        if (old_name != target && !target %in% names(df)) {
          names(df)[names(df) == old_name] <- target
          break
        }
      }
    }
  }
  
  df
}

#' Clean biobank data with improved logic
clean_biobank_data <- function(df_raw, mappings = load_column_mappings()) {
  # Step 1: Smart column renaming
  df <- smart_rename_columns(df_raw, mappings)
  
  # Step 2: Ensure critical columns exist
  required_columns <- unique(sapply(mappings, function(x) x$target_name))
  for (col in required_columns) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Step 3: Type conversions based on mapping configuration
  for (mapping_name in names(mappings)) {
    mapping <- mappings[[mapping_name]]
    col_name <- mapping$target_name
    
    if (col_name %in% names(df)) {
      df[[col_name]] <- clean_column_by_type(
        df[[col_name]], 
        type = mapping$type,
        values_map = mapping$values_map
      )
    }
  }
  
  # Step 4: Calculate derived columns
  df <- df %>%
    mutate(
      # Transport calculations
      transport_sample_to_cpltha = safe_days_between(date_reception_cpltha, date_sample, 90),
      transport_cpltha_to_inrb = safe_days_between(date_envoi_inrb, date_reception_cpltha, 90),
      
      # Conservation proxy (no treatment date available)
      conservation_days = safe_days_between(
        coalesce(date_envoi_inrb, date_reception_cpltha), 
        date_sample, 
        365
      ),
      
      # Shipment status
      shipped_to_inrb = !is.na(date_envoi_inrb)
    )
  
  # Step 5: Filter valid rows (both identifiers required)
  df %>%
    filter(
      !is.na(barcode) & nzchar(trimws(barcode)),
      !is.na(lab_id) & nzchar(trimws(lab_id))
    ) %>%
    distinct(barcode, lab_id, .keep_all = TRUE)
}

#' Clean column based on type
clean_column_by_type <- function(x, type, values_map = NULL) {
  switch(type,
         "identifier" = trimws(as.character(x)),
         "date" = parse_any_date(x),
         "age" = parse_age(x),
         "categorical" = map_categorical_values(x, values_map),
         "numeric" = parse_decimal_number(x),
         "text" = trimws(as.character(x)),
         as.character(x)
  )
}

#' Map categorical values using configuration
map_categorical_values <- function(x, values_map) {
  if (is.null(values_map)) return(x)
  
  x_clean <- toupper(trimws(as.character(x)))
  result <- rep(NA_character_, length(x))
  
  for (target_value in names(values_map)) {
    source_values <- toupper(values_map[[target_value]])
    result[x_clean %in% source_values] <- target_value
  }
  
  result
}