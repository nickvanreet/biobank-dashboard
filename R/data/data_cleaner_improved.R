# R/data/data_cleaner_improved.R
# Improved data cleaning module for Mbuji-Mayi Biobank Dashboard
# Tailored to the actual Excel file structure

library(tidyverse)
library(lubridate)
library(stringi)

# ============================================================================
# HEALTH ZONE NORMALIZATION MAPPING
# ============================================================================
# Maps biobank spellings to correct geojson map spellings
# Uses uppercase keys for case-insensitive matching

get_health_zone_mapping <- function() {
  c(
    # Biobank spelling (UPPERCASE) -> Correct geojson spelling
    # Citenge variants
    "TSHITENGE" = "Citenge",
    "CITENGE" = "Citenge",
    # Kabeya Kamwanga variants
    "KABEYA KAMUANGA" = "Kabeya Kamwanga",
    "KABEYA KAMWANGA" = "Kabeya Kamwanga",
    "KABEYA-KAMWANGA" = "Kabeya Kamwanga",
    # Mweneditu variants
    "MWENE-DITU" = "Mweneditu",
    "MWENE DITU" = "Mweneditu",
    "MUENE DITU" = "Mweneditu",
    "MUENE-DITU" = "Mweneditu",
    "MWENA-DITU" = "Mweneditu",
    "MWENA DITU" = "Mweneditu",
    "MWENEDITU" = "Mweneditu",
    # Kalambayi Kabanga variants
    "KALAMBAYI" = "Kalambayi Kabanga",
    "KALAMBAYI KABANGA" = "Kalambayi Kabanga",
    "KALAMBAYI-KABANGA" = "Kalambayi Kabanga",
    # Standard zone names (for case normalization)
    "BIBANGA" = "Bibanga",
    "BIPEMBA" = "Bipemba",
    "BONZOLA" = "Bonzola",
    "CILUNDU" = "Cilundu",
    "DIBINDI" = "Dibindi",
    "DIULU" = "Diulu",
    "KABINDA" = "Kabinda",
    "KALENDA" = "Kalenda",
    "KALONDA EST" = "Kalonda Est",
    "KAMANA" = "Kamana",
    "KAMIJI" = "Kamiji",
    "KANDA KANDA" = "Kanda Kanda",
    "KANDA-KANDA" = "Kanda Kanda",
    "KANSELE" = "Kansele",
    "KASANSA" = "Kasansa",
    "LUBAO" = "Lubao",
    "LUBILANJI" = "Lubilanji",
    "LUDIMBI LUKULA" = "Ludimbi Lukula",
    "LUDIMBI-LUKULA" = "Ludimbi Lukula",
    "LUKELENGE" = "Lukelenge",
    "LUPUTA" = "Luputa",
    "MAKOTA" = "Makota",
    "MIABI" = "Miabi",
    "MPOKOLO" = "Mpokolo",
    "MUKUMBI" = "Mukumbi",
    "MULUMBA" = "Mulumba",
    "MUYA" = "Muya",
    "NGANDAJIKA" = "Ngandajika",
    "NZABA" = "Nzaba",
    "TSHILENGE" = "Tshilenge",
    "TSHISHIMBI" = "Tshishimbi",
    "TSHOFA" = "Tshofa",
    "WIKONG" = "Wikong"
  )
}

#' Get list of valid health zone names from geojson
#' @return Character vector of valid health zone names
get_valid_health_zones <- function() {
  c(
    "Bibanga", "Bipemba", "Bonzola", "Cilundu", "Citenge",
    "Dibindi", "Diulu", "Kabeya Kamwanga", "Kabinda",
    "Kalambayi Kabanga", "Kalenda", "Kalonda Est", "Kamana",
    "Kamiji", "Kanda Kanda", "Kansele", "Kasansa", "Lubao",
    "Lubilanji", "Ludimbi Lukula", "Lukelenge", "Luputa",
    "Makota", "Miabi", "Mpokolo", "Mukumbi", "Mulumba",
    "Muya", "Mweneditu", "Ngandajika", "Nzaba", "Tshilenge",
    "Tshishimbi", "Tshofa", "Wikong"
  )
}

#' Normalize health zone names to match geojson map
#' Uses exact mapping first, then fuzzy matching for unknown names
#' @param x Character vector of health zone names
#' @return Normalized health zone names
normalize_health_zone <- function(x) {
  mapping <- get_health_zone_mapping()
  valid_zones <- get_valid_health_zones()

  normalized <- x
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && nzchar(trimws(x[i]))) {
      input_upper <- toupper(trimws(x[i]))

      # Check exact match in mapping
      if (input_upper %in% names(mapping)) {
        normalized[i] <- mapping[input_upper]
      } else {
        # Try fuzzy matching against valid zones
        # Calculate string distance (Levenshtein)
        distances <- sapply(toupper(valid_zones), function(vz) {
          utils::adist(input_upper, vz)[1, 1]
        })

        # If best match is close enough (distance <= 3), use it
        min_dist <- min(distances)
        if (min_dist <= 3) {
          best_match_idx <- which.min(distances)
          normalized[i] <- valid_zones[best_match_idx]
        }
        # Otherwise keep original value
      }
    }
  }

  normalized
}

# ============================================================================
# COLUMN MAPPING CONFIGURATION
# ============================================================================

#' Get column mapping configuration
#' Based on actual Excel headers from the biobank file
get_column_config <- function() {
  list(
    # Core identifiers
    numero = list(
      target = "lab_id",
      patterns = c("^numéro$", "^numero$", "^num[eé]ro$"),
      type = "identifier",
      required = TRUE
    ),
    
    code_barres_kps = list(
      target = "barcode",
      patterns = c("code.*barres.*kps", "code_barres_kps", "barcode"),
      type = "identifier", 
      required = TRUE
    ),
    
    # Study info
    etude = list(
      target = "study",
      patterns = c("^etude$", "^étude$", "study"),
      type = "study_code",
      required = TRUE
    ),
    
    # Location hierarchy
    structure_sanitaire = list(
      target = "health_facility",
      patterns = c("structure.*sanitaire", "unité.*mobile"),
      type = "text",
      required = FALSE
    ),
    
    zone_de_sante = list(
      target = "health_zone",
      patterns = c("zone.*santé", "zone.*sante", "health.*zone"),
      type = "text",
      required = TRUE
    ),
    
    province = list(
      target = "province",
      patterns = c("^province$"),
      type = "text",
      required = TRUE
    ),
    
    # Contact info
    responsable = list(
      target = "responsible_person",
      patterns = c("responsable.*structure", "responsable.*cpltha"),
      type = "text",
      required = FALSE
    ),
    
    contact = list(
      target = "contact",
      patterns = c("^contact$"),
      type = "text",
      required = FALSE
    ),
    
    # Dates
    date_prelevement = list(
      target = "date_sample",
      patterns = c("date.*prélèvement", "date.*prelevement", "date.*collection"),
      type = "date",
      required = TRUE
    ),
    
    date_envoi_cpltha = list(
      target = "date_sent_cpltha",
      patterns = c("date.*envoi.*vers.*cpltha", "date.*envoi.*cpltha"),
      type = "date",
      required = FALSE
    ),
    
    date_reception_cpltha = list(
      target = "date_received_cpltha",
      patterns = c("date.*réception.*cpltha", "date.*reception.*cpltha"),
      type = "date",
      required = FALSE
    ),
    
    date_envoi_inrb = list(
      target = "date_sent_inrb",
      patterns = c("date.*envoi.*inrb", "date.*inrb"),
      type = "date",
      required = FALSE
    ),
    
    # Demographics
    age = list(
      target = "age",
      patterns = c("age.*année.*naissance", "age.*annee.*naissance", "^age$"),
      type = "age",
      required = FALSE
    ),
    
    sexe = list(
      target = "sex",
      patterns = c("^sexe", "sex.*m.*f"),
      type = "sex_code",
      required = FALSE
    ),
    
    # Case history
    ancien_cas = list(
      target = "previous_case",
      patterns = c("ancien.*cas", "previous.*case"),
      type = "yes_no_uncertain",
      required = FALSE
    ),
    
    traite = list(
      target = "treated",
      patterns = c("^traité", "^traite", "treated"),
      type = "yes_no_uncertain",
      required = FALSE
    ),
    
    # Storage temperatures
    stockage_avant_cpltha = list(
      target = "storage_before_cpltha",
      patterns = c("stockage.*avant.*cpltha", "storage.*before"),
      type = "temperature",
      required = FALSE
    ),
    
    temp_transport = list(
      target = "transport_temperature",
      patterns = c("température.*transport", "temperature.*transport", "temp.*transport"),
      type = "temperature",
      required = FALSE
    ),
    
    temp_stockage_cpltha = list(
      target = "storage_temp_cpltha",
      patterns = c("température.*stockage.*cpltha", "temperature.*stockage.*cpltha"),
      type = "temperature",
      required = FALSE
    ),
    
    # Sample presence
    presence_drs = list(
      target = "has_drs",
      patterns = c("présence.*drs", "presence.*drs"),
      type = "yes_no",
      required = FALSE
    ),
    
    presence_dbs = list(
      target = "has_dbs",
      patterns = c("présence.*dbs", "presence.*dbs"),
      type = "yes_no",
      required = FALSE
    ),
    
    nombre_dbs = list(
      target = "dbs_count",
      patterns = c("nombre.*dbs", "number.*dbs"),
      type = "numeric",
      required = FALSE
    ),
    
    # Other
    remarques = list(
      target = "remarks",
      patterns = c("^remarques$", "^remarks$", "comments"),
      type = "text",
      required = FALSE
    )
  )
}

# ============================================================================
# MAIN CLEANING FUNCTION
# ============================================================================

#' Clean biobank data with improved logic
#' @param df_raw Raw data frame from Excel
#' @param skip_columns Columns to skip (default: columns 24-26 which often have issues)
#' @return Cleaned data frame
clean_biobank_data_improved <- function(df_raw, skip_columns = 24:26) {
  
  # Skip problematic columns if they exist
  if (ncol(df_raw) >= max(skip_columns)) {
    df_raw <- df_raw[, -skip_columns]
  }
  
  # Get column configuration
  config <- get_column_config()
  
  # Step 1: Standardize column names
  df <- df_raw %>% janitor::clean_names()
  
  # Step 2: Smart column mapping
  df <- map_columns_smart(df, config)

  # Harmonize health structure naming for downstream filters/plots
  if ("health_facility" %in% names(df) && !"health_structure" %in% names(df)) {
    df$health_structure <- df$health_facility
  } else if (!"health_facility" %in% names(df) && "health_structure" %in% names(df)) {
    df$health_facility <- df$health_structure
  }

  # Step 3: Parse each column by type
  for (col_config in config) {
    col_name <- col_config$target
    if (col_name %in% names(df)) {
      df[[col_name]] <- parse_by_type(
        df[[col_name]], 
        type = col_config$type
      )
    }
  }

  # Step 3b: Normalize health zone names to match geojson map
  if ("health_zone" %in% names(df)) {
    df$health_zone <- normalize_health_zone(df$health_zone)
  }

  # Step 4: Calculate derived fields
  df <- df %>%
    mutate(
      # Transport durations
      transport_field_to_cpltha = safe_days_between(
        date_sent_cpltha, date_sample, max_days = 90
      ),
      
      transport_received_at_cpltha = safe_days_between(
        date_received_cpltha, date_sent_cpltha, max_days = 30
      ),
      
      transport_cpltha_to_inrb = safe_days_between(
        date_sent_inrb, date_received_cpltha, max_days = 90
      ),
      
      # Total transport time
      total_transport_days = safe_days_between(
        coalesce(date_sent_inrb, date_received_cpltha),
        date_sample, 
        max_days = 365
      ),
      
      # Conservation proxy (no treatment date in data)
      conservation_days = total_transport_days,
      
      # Shipment flags
      shipped_to_cpltha = !is.na(date_sent_cpltha),
      received_at_cpltha = !is.na(date_received_cpltha),
      shipped_to_inrb = !is.na(date_sent_inrb),
      
      # Sample type flags
      has_drs_sample = has_drs == "Oui",
      has_dbs_sample = has_dbs == "Oui",
      
      # Case categorization
      case_category = case_when(
        previous_case == "Oui" | treated == "Oui" ~ "Previous/Treated",
        previous_case == "Non" & treated == "Non" ~ "New case",
        previous_case == "Incertain" | treated == "Incertain" ~ "Uncertain",
        TRUE ~ "Unknown"
      ),
      
      # Age groups for analysis
      age_group = cut(
        age, 
        breaks = c(0, 5, 15, 25, 35, 45, 55, 65, Inf),
        labels = c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
        right = FALSE
      ),
      
      # Data completeness score
      completeness_score = calculate_completeness_score(.)
    )
  
  # Step 5: Quality control - keep only valid rows
  df_clean <- df %>%
    filter(
      # Must have both identifiers
      !is.na(lab_id) & nzchar(trimws(as.character(lab_id))),
      !is.na(barcode) & nzchar(trimws(as.character(barcode))),
      
      # Must have valid date
      !is.na(date_sample),
      
      # Remove obvious data entry errors
      age <= 120 | is.na(age),  # Remove impossible ages
      
      # Remove duplicate entries
      !duplicated(paste(barcode, lab_id))
    ) %>%
    
    # Add metadata
    mutate(
      import_date = Sys.Date(),
      import_timestamp = Sys.time(),
      row_number = row_number()
    )
  
  # Step 6: Add data quality flags
  df_clean <- df_clean %>%
    mutate(
      quality_flag = case_when(
        is.na(province) ~ "Missing province",
        is.na(health_zone) ~ "Missing health zone",
        is.na(study) ~ "Missing study type",
        transport_field_to_cpltha > 30 ~ "Long transport time",
        age > 100 ~ "Check age",
        TRUE ~ "OK"
      )
    )
  df_clean %>%
    mutate(sample_week = floor_date(date_sample, "week")) %>%
    count(sample_week, quality_flag, name = "rows")
  
  return(df_clean)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Smart column mapping using patterns
map_columns_smart <- function(df, config) {
  current_names <- names(df)
  
  for (item_name in names(config)) {
    item <- config[[item_name]]
    target_name <- item$target
    
    # Skip if target already exists
    if (target_name %in% names(df)) next
    
    # Try each pattern
    for (pattern in item$patterns) {
      matches <- grep(pattern, current_names, ignore.case = TRUE, value = TRUE)
      
      if (length(matches) > 0) {
        # Use first match
        old_name <- matches[1]
        names(df)[names(df) == old_name] <- target_name
        break
      }
    }
    
    # Create column if required but missing
    if (item$required && !target_name %in% names(df)) {
      df[[target_name]] <- NA
      warning(sprintf("Required column '%s' not found, created as NA", target_name))
    }
  }
  
  df
}

#' Parse column by type
parse_by_type <- function(x, type) {
  switch(type,
    "identifier" = clean_identifier(x),
    "text" = clean_text(x),
    "date" = parse_date_flexible(x),
    "age" = parse_age_smart(x),
    "numeric" = parse_numeric_safe(x),
    "sex_code" = parse_sex(x),
    "study_code" = parse_study(x),
    "temperature" = parse_temperature(x),
    "yes_no" = parse_yes_no(x),
    "yes_no_uncertain" = parse_yes_no_uncertain(x),
    as.character(x)
  )
}

#' Clean identifier columns
clean_identifier <- function(x) {
  x_clean <- trimws(as.character(x))
  x_clean[x_clean %in% c("", "NA", "N/A", "n/a")] <- NA_character_
  x_clean
}

#' Clean text columns
clean_text <- function(x) {
  x_clean <- trimws(as.character(x))
  x_clean[x_clean %in% c("", "NA", "N/A")] <- NA_character_
  stringi::stri_trans_general(x_clean, "Latin-ASCII")
}

#' Parse dates flexibly
parse_date_flexible <- function(x) {
  if (all(is.na(x))) return(as.Date(x))
  
  x_chr <- as.character(x)
  
  # Try different formats
  parsed <- lubridate::parse_date_time(
    x_chr, 
    orders = c("dmy", "mdy", "ymd", "dmY", "mdY", "Ymd"),
    quiet = TRUE
  )
  
  # Handle Excel date serials
  numeric_indices <- !is.na(suppressWarnings(as.numeric(x_chr)))
  if (any(numeric_indices)) {
    excel_dates <- as.Date(as.numeric(x_chr[numeric_indices]), origin = "1899-12-30")
    parsed[numeric_indices] <- excel_dates
  }
  
  as.Date(parsed)
}

#' Parse age intelligently
parse_age_smart <- function(x) {
  age_num <- suppressWarnings(as.numeric(x))
  
  # Handle birth years
  current_year <- year(Sys.Date())
  age_num <- ifelse(
    age_num > 1900 & age_num <= current_year,
    current_year - age_num,
    age_num
  )
  
  # Validate reasonable ages
  age_num[age_num < 0 | age_num > 120] <- NA_real_
  
  age_num
}

#' Parse numeric safely
parse_numeric_safe <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Parse sex codes
parse_sex <- function(x) {
  x_upper <- toupper(trimws(as.character(x)))
  
  case_when(
    x_upper %in% c("M", "MALE", "HOMME", "H") ~ "M",
    x_upper %in% c("F", "FEMALE", "FEMME") ~ "F",
    TRUE ~ NA_character_
  )
}

#' Parse study codes (DA/DP)
parse_study <- function(x) {
  x_upper <- toupper(trimws(as.character(x)))
  
  case_when(
    x_upper %in% c("DA", "ACTIF", "ACTIVE") ~ "DA",
    x_upper %in% c("DP", "PASSIF", "PASSIVE") ~ "DP",
    grepl("ACTIF|ACTIVE", x_upper) ~ "DA",
    grepl("PASSIF|PASSIVE", x_upper) ~ "DP",
    TRUE ~ NA_character_
  )
}

#' Parse temperature storage codes
parse_temperature <- function(x) {
  x_clean <- toupper(trimws(as.character(x)))
  
  # Handle single letters and full words
  case_when(
    x_clean %in% c("A", "AMB", "AMBIANTE", "AMBIENT", "ROOM") ~ "Ambiante",
    x_clean %in% c("F", "FRIGO", "FRIDGE", "REFRIGERATOR") ~ "Frigo",
    x_clean %in% c("C", "CONG", "CONGELATEUR", "FREEZER") ~ "Congelateur",
    grepl("^A", x_clean) ~ "Ambiante",
    grepl("^F", x_clean) ~ "Frigo",
    grepl("^C", x_clean) ~ "Congelateur",
    TRUE ~ NA_character_
  )
}

#' Parse yes/no responses
parse_yes_no <- function(x) {
  x_upper <- toupper(trimws(as.character(x)))
  
  case_when(
    x_upper %in% c("OUI", "YES", "Y", "O", "1", "TRUE") ~ "Oui",
    x_upper %in% c("NON", "NO", "N", "0", "FALSE") ~ "Non",
    TRUE ~ NA_character_
  )
}

#' Parse yes/no/uncertain responses
parse_yes_no_uncertain <- function(x) {
  x_upper <- toupper(trimws(as.character(x)))
  
  case_when(
    x_upper %in% c("OUI", "YES", "Y", "O", "1") ~ "Oui",
    x_upper %in% c("NON", "NO", "N", "0") ~ "Non",
    x_upper %in% c("INCERTAIN", "UNCERTAIN", "?", "MAYBE") ~ "Incertain",
    TRUE ~ NA_character_
  )
}

#' Calculate days between dates safely
safe_days_between <- function(date_to, date_from, max_days = NULL) {
  days <- as.numeric(difftime(date_to, date_from, units = "days"))
  
  # Handle invalid values
  days[!is.finite(days) | days < 0] <- NA_real_
  
  # Cap at maximum if specified
  if (!is.null(max_days)) {
    days[days > max_days] <- NA_real_
  }
  
  days
}

#' Calculate data completeness score for each row
calculate_completeness_score <- function(df) {
  critical_cols <- c("lab_id", "barcode", "date_sample", "study", 
                     "province", "health_zone")
  important_cols <- c("age", "sex", "date_sent_cpltha", "date_received_cpltha")
  
  critical_complete <- rowSums(!is.na(df[, critical_cols, drop = FALSE]))
  important_complete <- rowSums(!is.na(df[, important_cols, drop = FALSE]))
  
  score <- (critical_complete / length(critical_cols)) * 0.7 + 
           (important_complete / length(important_cols)) * 0.3
  
  round(score * 100)
}
