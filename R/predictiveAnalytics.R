# Predictive Analytics Module
# Epidemiological forecasting and risk prediction for HAT surveillance
# ============================================================================

#' Get column name flexibly (handles both CamelCase and snake_case)
#'
#' @param df Data frame
#' @param possible_names Character vector of possible column names
#' @return Actual column name found or NULL
get_col_name <- function(df, possible_names) {
  for (name in possible_names) {
    if (name %in% names(df)) return(name)
  }
  return(NULL)
}

#' Standardize column names for analysis
#'
#' @param df Data frame
#' @return Data frame with standardized column names
standardize_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  # Create mapping of standardized names to possible original names
  mappings <- list(
    health_zone = c("health_zone", "HealthZone", "healthzone", "Health_Zone", "zone_sante"),
    structure = c("structure", "Structure", "health_structure", "Structure_Sante"),
    province = c("province", "Province"),
    sex = c("sex", "Sex", "sexe", "Sexe", "gender", "Gender"),
    age = c("age", "Age", "AGE"),
    date_prel = c("date_prel", "DatePrel", "date_prelevement", "DatePrelevement", "sample_date", "SampleDate")
  )

  for (std_name in names(mappings)) {
    found_col <- get_col_name(df, mappings[[std_name]])
    if (!is.null(found_col) && found_col != std_name) {
      df[[std_name]] <- df[[found_col]]
    }
  }

  return(df)
}


#' Calculate Health Zone Risk Scores
#'
#' Analyzes historical data to predict which health zones are at highest risk
#' for future positive cases based on past positivity rates, trends, and patterns.
#'
#' @param biobank_df Data frame with biobank data
#' @param mic_df Data frame with MIC qPCR results
#' @param elisa_df Data frame with ELISA results
#' @param ielisa_df Data frame with iELISA results
#' @param lookback_months Number of months to consider for trend analysis
#' @return Data frame with risk scores per health zone
#' @export
calculate_healthzone_risk <- function(biobank_df, mic_df = NULL, elisa_df = NULL,
                                       ielisa_df = NULL, lookback_months = 6) {
  tryCatch({
    # Standardize column names
    biobank_df <- standardize_columns(biobank_df)

    # Check for health_zone column
    if (!"health_zone" %in% names(biobank_df)) {
      warning("No health_zone column found in biobank data")
      return(data.frame(
        health_zone = character(),
        risk_score = numeric(),
        risk_category = character()
      ))
    }

    # Check for date column
    date_col <- get_col_name(biobank_df, c("date_prel", "DatePrel", "sample_date"))
    if (is.null(date_col)) {
      biobank_df$date_prel <- Sys.Date()
    } else {
      biobank_df$date_prel <- as.Date(biobank_df[[date_col]])
    }

    # Get health zone data from biobank
    zone_data <- biobank_df %>%
      dplyr::filter(!is.na(health_zone)) %>%
      dplyr::mutate(
        collection_month = lubridate::floor_date(date_prel, "month")
      )

    if (nrow(zone_data) == 0) {
      return(data.frame(
        health_zone = character(),
        risk_score = numeric(),
        risk_category = character()
      ))
    }

    # Get structure column name
    structure_col <- get_col_name(zone_data, c("structure", "Structure"))

    # Calculate base metrics per zone
    zone_summary <- zone_data %>%
      dplyr::group_by(health_zone) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        unique_structures = if (!is.null(structure_col)) dplyr::n_distinct(.data[[structure_col]], na.rm = TRUE) else 0,
        date_range_days = as.numeric(difftime(max(date_prel, na.rm = TRUE),
                                               min(date_prel, na.rm = TRUE), units = "days")),
        last_sample_date = max(date_prel, na.rm = TRUE),
        .groups = "drop"
      )

    # Calculate molecular positivity if MIC data available
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      mic_df <- standardize_columns(mic_df)
      hz_col <- get_col_name(mic_df, c("health_zone", "HealthZone"))

      if (!is.null(hz_col)) {
        # MIC data uses FinalCall column with values: Positive, Negative, LatePositive, etc.
        final_call_col <- get_col_name(mic_df, c("FinalCall", "final_call", "Result", "result"))

        if (!is.null(final_call_col)) {
          mic_positivity <- mic_df %>%
            dplyr::rename(health_zone = !!rlang::sym(hz_col)) %>%
            dplyr::filter(!is.na(health_zone)) %>%
            dplyr::group_by(health_zone) %>%
            dplyr::summarise(
              mic_tested = dplyr::n(),
              mic_any_positive = sum(grepl("Positive|Pos|DNA\\+|RNA\\+", .data[[final_call_col]], ignore.case = TRUE), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              mic_positivity_rate = dplyr::if_else(mic_tested > 0, mic_any_positive / mic_tested * 100, 0)
            )
          zone_summary <- dplyr::left_join(zone_summary, mic_positivity, by = "health_zone")
        }
      }
    }

    # Calculate serological positivity if ELISA data available
    if (!is.null(elisa_df) && nrow(elisa_df) > 0) {
      elisa_df <- standardize_columns(elisa_df)
      hz_col <- get_col_name(elisa_df, c("health_zone", "HealthZone"))

      if (!is.null(hz_col)) {
        # Check for result column
        result_col <- get_col_name(elisa_df, c("Result", "result", "positive", "Positive"))

        if (!is.null(result_col)) {
          elisa_positivity <- elisa_df %>%
            dplyr::rename(health_zone = !!rlang::sym(hz_col)) %>%
            dplyr::filter(!is.na(health_zone)) %>%
            dplyr::group_by(health_zone) %>%
            dplyr::summarise(
              elisa_tested = dplyr::n(),
              elisa_positive = sum(.data[[result_col]] %in% c("Positive", "POSITIVE", "POS", "Pos", TRUE, 1), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              elisa_positivity_rate = dplyr::if_else(elisa_tested > 0, elisa_positive / elisa_tested * 100, 0)
            )
          zone_summary <- dplyr::left_join(zone_summary, elisa_positivity, by = "health_zone")
        }
      }
    }

    # Replace NAs with 0 for numeric columns
    zone_summary <- zone_summary %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

    # Add missing columns with default values if they don't exist
    if (!"mic_positivity_rate" %in% names(zone_summary)) {
      zone_summary$mic_positivity_rate <- 0
    }
    if (!"elisa_positivity_rate" %in% names(zone_summary)) {
      zone_summary$elisa_positivity_rate <- 0
    }

    # Calculate composite risk score
    zone_summary <- zone_summary %>%
      dplyr::mutate(
        # Normalize metrics to 0-100 scale
        sample_density_score = if (max(total_samples) > min(total_samples)) {
          scales::rescale(total_samples, to = c(0, 100))
        } else {
          50
        },
        # Higher positivity = higher risk (scale up for visibility)
        molecular_risk = pmin(100, mic_positivity_rate * 10),
        serological_risk = pmin(100, elisa_positivity_rate * 10),
        # Recency factor - more recent activity = higher monitoring priority
        days_since_last = as.numeric(Sys.Date() - last_sample_date),
        recency_score = pmax(0, 100 - days_since_last / 2),  # Decays over ~200 days
        # Composite risk score (weighted average)
        risk_score = (
          molecular_risk * 0.35 +
          serological_risk * 0.30 +
          sample_density_score * 0.20 +
          recency_score * 0.15
        ),
        # Risk category
        risk_category = dplyr::case_when(
          risk_score >= 75 ~ "Very High",
          risk_score >= 50 ~ "High",
          risk_score >= 25 ~ "Medium",
          TRUE ~ "Low"
        ),
        risk_category = factor(risk_category, levels = c("Low", "Medium", "High", "Very High"))
      ) %>%
      dplyr::arrange(dplyr::desc(risk_score))

    zone_summary

  }, error = function(e) {
    warning(paste("Error in calculate_healthzone_risk:", e$message))
    data.frame(
      health_zone = character(),
      risk_score = numeric(),
      risk_category = character()
    )
  })
}


#' Calculate Structure-Level Risk Predictions
#'
#' Predicts which structures (health facilities) are most likely to have
#' positive cases based on historical patterns.
#'
#' @param biobank_df Data frame with biobank data
#' @param mic_df Data frame with MIC qPCR results
#' @param elisa_df Data frame with ELISA results
#' @param ielisa_df Data frame with iELISA results
#' @return Data frame with structure-level risk predictions
#' @export
calculate_structure_risk <- function(biobank_df, mic_df = NULL, elisa_df = NULL,
                                      ielisa_df = NULL) {
  tryCatch({
    # Standardize column names
    biobank_df <- standardize_columns(biobank_df)

    # Check for required columns
    if (!"structure" %in% names(biobank_df) || !"health_zone" %in% names(biobank_df)) {
      warning("Missing structure or health_zone column in biobank data")
      return(data.frame(
        health_zone = character(),
        structure = character(),
        risk_score = numeric(),
        risk_category = character()
      ))
    }

    # Get date column
    date_col <- get_col_name(biobank_df, c("date_prel", "DatePrel", "sample_date"))
    if (is.null(date_col)) {
      biobank_df$date_prel <- Sys.Date()
    } else {
      biobank_df$date_prel <- as.Date(biobank_df[[date_col]])
    }

    # Get structure data from biobank
    structure_data <- biobank_df %>%
      dplyr::filter(!is.na(structure), !is.na(health_zone)) %>%
      dplyr::group_by(health_zone, structure) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        first_sample = min(date_prel, na.rm = TRUE),
        last_sample = max(date_prel, na.rm = TRUE),
        unique_dates = dplyr::n_distinct(date_prel),
        .groups = "drop"
      )

    if (nrow(structure_data) == 0) {
      return(data.frame(
        health_zone = character(),
        structure = character(),
        risk_score = numeric(),
        risk_category = character()
      ))
    }

    # Add MIC positivity by structure
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      mic_df <- standardize_columns(mic_df)

      if ("structure" %in% names(mic_df) && "health_zone" %in% names(mic_df)) {
        # MIC data uses FinalCall column
        final_call_col <- get_col_name(mic_df, c("FinalCall", "final_call", "Result", "result"))

        if (!is.null(final_call_col)) {
          mic_structure <- mic_df %>%
            dplyr::filter(!is.na(structure)) %>%
            dplyr::group_by(health_zone, structure) %>%
            dplyr::summarise(
              mic_tested = dplyr::n(),
              mic_positive = sum(grepl("Positive|Pos|DNA\\+|RNA\\+", .data[[final_call_col]], ignore.case = TRUE), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(mic_positivity = dplyr::if_else(mic_tested > 0, mic_positive / mic_tested * 100, 0))

          structure_data <- dplyr::left_join(structure_data, mic_structure,
                                              by = c("health_zone", "structure"))
        }
      }
    }

    # Add serological positivity by structure
    if (!is.null(elisa_df) && nrow(elisa_df) > 0) {
      elisa_df <- standardize_columns(elisa_df)

      if ("structure" %in% names(elisa_df) && "health_zone" %in% names(elisa_df)) {
        result_col <- get_col_name(elisa_df, c("Result", "result", "positive"))

        if (!is.null(result_col)) {
          elisa_structure <- elisa_df %>%
            dplyr::filter(!is.na(structure)) %>%
            dplyr::group_by(health_zone, structure) %>%
            dplyr::summarise(
              elisa_tested = dplyr::n(),
              elisa_positive = sum(.data[[result_col]] %in% c("Positive", "POSITIVE", "POS", "Pos", TRUE, 1), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(elisa_positivity = dplyr::if_else(elisa_tested > 0, elisa_positive / elisa_tested * 100, 0))

          structure_data <- dplyr::left_join(structure_data, elisa_structure,
                                              by = c("health_zone", "structure"))
        }
      }
    }

    # Initialize missing columns with 0
    if (!"mic_positivity" %in% names(structure_data)) structure_data$mic_positivity <- 0
    if (!"elisa_positivity" %in% names(structure_data)) structure_data$elisa_positivity <- 0

    # Replace NAs and calculate risk score
    structure_data <- structure_data %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
      dplyr::mutate(
        # Historical positivity rate (combined)
        historical_positivity = dplyr::case_when(
          mic_positivity > 0 & elisa_positivity > 0 ~ (mic_positivity + elisa_positivity) / 2,
          mic_positivity > 0 ~ mic_positivity,
          elisa_positivity > 0 ~ elisa_positivity,
          TRUE ~ 0
        ),
        # Activity level
        days_active = as.numeric(difftime(last_sample, first_sample, units = "days")),
        sampling_frequency = total_samples / pmax(days_active, 1) * 30, # samples per month
        # Days since last sample
        days_since_last = as.numeric(Sys.Date() - last_sample),
        # Risk score
        risk_score = dplyr::case_when(
          historical_positivity > 10 ~ 90 + (historical_positivity - 10) * 0.5,
          historical_positivity > 5 ~ 70 + (historical_positivity - 5) * 4,
          historical_positivity > 0 ~ 40 + historical_positivity * 6,
          total_samples >= 50 ~ 30,
          total_samples >= 20 ~ 20,
          TRUE ~ 10
        ),
        # Adjust for recency
        risk_score = risk_score * dplyr::case_when(
          days_since_last <= 30 ~ 1.0,
          days_since_last <= 90 ~ 0.9,
          days_since_last <= 180 ~ 0.7,
          TRUE ~ 0.5
        ),
        risk_score = pmin(100, risk_score),
        # Risk category
        risk_category = dplyr::case_when(
          risk_score >= 75 ~ "Very High",
          risk_score >= 50 ~ "High",
          risk_score >= 25 ~ "Medium",
          TRUE ~ "Low"
        ),
        risk_category = factor(risk_category, levels = c("Low", "Medium", "High", "Very High")),
        # Prediction label
        prediction = dplyr::case_when(
          risk_score >= 75 ~ "Likely to find positives",
          risk_score >= 50 ~ "Possible positives",
          risk_score >= 25 ~ "Monitor closely",
          TRUE ~ "Low probability"
        )
      ) %>%
      dplyr::arrange(dplyr::desc(risk_score))

    structure_data

  }, error = function(e) {
    warning(paste("Error in calculate_structure_risk:", e$message))
    data.frame(
      health_zone = character(),
      structure = character(),
      risk_score = numeric(),
      risk_category = character()
    )
  })
}


#' Calculate Demographic Risk Profile
#'
#' Analyzes which demographic groups (age, sex) are most likely to test positive
#' based on historical data patterns.
#'
#' @param biobank_df Data frame with biobank data including demographics
#' @param mic_df Data frame with MIC qPCR results
#' @param elisa_df Data frame with ELISA results
#' @param ielisa_df Data frame with iELISA results
#' @return List with demographic risk profiles
#' @export
calculate_demographic_risk <- function(biobank_df, mic_df = NULL, elisa_df = NULL,
                                        ielisa_df = NULL) {
  tryCatch({
    results <- list()

    # Standardize column names
    biobank_df <- standardize_columns(biobank_df)

    # Sex-based risk analysis
    sex_col <- get_col_name(biobank_df, c("sex", "Sex", "sexe", "Sexe"))

    if (!is.null(sex_col)) {
      sex_risk <- biobank_df %>%
        dplyr::rename(sex = !!rlang::sym(sex_col)) %>%
        dplyr::filter(!is.na(sex), sex %in% c("M", "F", "Male", "Female", "Homme", "Femme")) %>%
        dplyr::mutate(
          Sex = dplyr::case_when(
            sex %in% c("M", "Male", "Homme") ~ "Male",
            sex %in% c("F", "Female", "Femme") ~ "Female",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(Sex)) %>%
        dplyr::group_by(Sex) %>%
        dplyr::summarise(
          total_samples = dplyr::n(),
          .groups = "drop"
        )

      # Add positivity data by sex if available
      if (!is.null(mic_df) && nrow(mic_df) > 0) {
        mic_df <- standardize_columns(mic_df)
        mic_sex_col <- get_col_name(mic_df, c("sex", "Sex", "sexe"))

        if (!is.null(mic_sex_col)) {
          # Get FinalCall column
          final_call_col <- get_col_name(mic_df, c("FinalCall", "final_call", "Result", "result"))

          if (!is.null(final_call_col)) {
            mic_sex <- mic_df %>%
              dplyr::rename(sex = !!rlang::sym(mic_sex_col)) %>%
              dplyr::mutate(
                Sex = dplyr::case_when(
                  sex %in% c("M", "Male", "Homme") ~ "Male",
                  sex %in% c("F", "Female", "Femme") ~ "Female",
                  TRUE ~ NA_character_
                )
              ) %>%
              dplyr::filter(!is.na(Sex)) %>%
              dplyr::group_by(Sex) %>%
              dplyr::summarise(
                mic_tested = dplyr::n(),
                mic_positive = sum(grepl("Positive|Pos|DNA\\+|RNA\\+", .data[[final_call_col]], ignore.case = TRUE), na.rm = TRUE),
                .groups = "drop"
              ) %>%
              dplyr::mutate(mic_positivity = dplyr::if_else(mic_tested > 0, mic_positive / mic_tested * 100, 0))

            sex_risk <- dplyr::left_join(sex_risk, mic_sex, by = "Sex")
          }
        }
      }

      results$sex_risk <- sex_risk
    } else {
      results$sex_risk <- data.frame(Sex = character(), total_samples = numeric())
    }

    # Age group risk analysis
    age_col <- get_col_name(biobank_df, c("age", "Age", "AGE"))

    if (!is.null(age_col)) {
      age_risk <- biobank_df %>%
        dplyr::rename(age = !!rlang::sym(age_col)) %>%
        dplyr::filter(!is.na(age), age >= 0, age <= 120) %>%
        dplyr::mutate(
          age_group = dplyr::case_when(
            age < 5 ~ "0-4",
            age < 15 ~ "5-14",
            age < 25 ~ "15-24",
            age < 35 ~ "25-34",
            age < 45 ~ "35-44",
            age < 55 ~ "45-54",
            age < 65 ~ "55-64",
            TRUE ~ "65+"
          ),
          age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34",
                                                     "35-44", "45-54", "55-64", "65+"))
        ) %>%
        dplyr::group_by(age_group) %>%
        dplyr::summarise(
          total_samples = dplyr::n(),
          mean_age = mean(age, na.rm = TRUE),
          .groups = "drop"
        )

      # Add positivity data by age if available
      if (!is.null(mic_df) && nrow(mic_df) > 0) {
        mic_df <- standardize_columns(mic_df)
        mic_age_col <- get_col_name(mic_df, c("age", "Age"))

        if (!is.null(mic_age_col)) {
          # Get FinalCall column
          final_call_col <- get_col_name(mic_df, c("FinalCall", "final_call", "Result", "result"))

          if (!is.null(final_call_col)) {
            mic_age <- mic_df %>%
              dplyr::rename(age = !!rlang::sym(mic_age_col)) %>%
              dplyr::filter(!is.na(age), age >= 0, age <= 120) %>%
              dplyr::mutate(
                age_group = dplyr::case_when(
                  age < 5 ~ "0-4",
                  age < 15 ~ "5-14",
                  age < 25 ~ "15-24",
                  age < 35 ~ "25-34",
                  age < 45 ~ "35-44",
                  age < 55 ~ "45-54",
                  age < 65 ~ "55-64",
                  TRUE ~ "65+"
                ),
                age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34",
                                                           "35-44", "45-54", "55-64", "65+"))
              ) %>%
              dplyr::group_by(age_group) %>%
              dplyr::summarise(
                mic_tested = dplyr::n(),
                mic_positive = sum(grepl("Positive|Pos|DNA\\+|RNA\\+", .data[[final_call_col]], ignore.case = TRUE), na.rm = TRUE),
                .groups = "drop"
              ) %>%
              dplyr::mutate(mic_positivity = dplyr::if_else(mic_tested > 0, mic_positive / mic_tested * 100, 0))

            age_risk <- dplyr::left_join(age_risk, mic_age, by = "age_group")
          }
        }
      }

      results$age_risk <- age_risk
    } else {
      results$age_risk <- data.frame(age_group = character(), total_samples = numeric())
    }

    # Combined sex + age analysis
    if (!is.null(sex_col) && !is.null(age_col)) {
      combined_risk <- biobank_df %>%
        dplyr::rename(sex = !!rlang::sym(sex_col), age = !!rlang::sym(age_col)) %>%
        dplyr::filter(
          !is.na(sex), sex %in% c("M", "F", "Male", "Female", "Homme", "Femme"),
          !is.na(age), age >= 0, age <= 120
        ) %>%
        dplyr::mutate(
          Sex = dplyr::case_when(
            sex %in% c("M", "Male", "Homme") ~ "Male",
            sex %in% c("F", "Female", "Femme") ~ "Female",
            TRUE ~ NA_character_
          ),
          age_group = dplyr::case_when(
            age < 15 ~ "0-14",
            age < 30 ~ "15-29",
            age < 45 ~ "30-44",
            age < 60 ~ "45-59",
            TRUE ~ "60+"
          ),
          age_group = factor(age_group, levels = c("0-14", "15-29", "30-44", "45-59", "60+"))
        ) %>%
        dplyr::filter(!is.na(Sex)) %>%
        dplyr::group_by(Sex, age_group) %>%
        dplyr::summarise(
          total_samples = dplyr::n(),
          .groups = "drop"
        )

      results$combined_risk <- combined_risk
    } else {
      results$combined_risk <- data.frame(Sex = character(), age_group = character(), total_samples = numeric())
    }

    results

  }, error = function(e) {
    warning(paste("Error in calculate_demographic_risk:", e$message))
    list(
      sex_risk = data.frame(Sex = character(), total_samples = numeric()),
      age_risk = data.frame(age_group = character(), total_samples = numeric()),
      combined_risk = data.frame(Sex = character(), age_group = character(), total_samples = numeric())
    )
  })
}


#' Calculate Time-Based Predictions
#'
#' Analyzes temporal patterns to predict when/where cases are likely to occur.
#'
#' @param biobank_df Data frame with biobank data
#' @param mic_df Data frame with MIC qPCR results
#' @param forecast_months Number of months to forecast
#' @return List with temporal predictions
#' @export
calculate_temporal_predictions <- function(biobank_df, mic_df = NULL, forecast_months = 3) {
  tryCatch({
    results <- list()

    # Standardize column names
    biobank_df <- standardize_columns(biobank_df)

    # Get date column
    date_col <- get_col_name(biobank_df, c("date_prel", "DatePrel", "sample_date"))

    if (is.null(date_col)) {
      warning("No date column found for temporal predictions")
      return(list(
        monthly_trend = data.frame(),
        error = "No date column found"
      ))
    }

    biobank_df$date_prel <- as.Date(biobank_df[[date_col]])

    # Monthly sampling trends
    monthly_trend <- biobank_df %>%
      dplyr::filter(!is.na(date_prel)) %>%
      dplyr::mutate(
        sample_month = lubridate::floor_date(date_prel, "month")
      ) %>%
      dplyr::filter(!is.na(sample_month)) %>%
      dplyr::group_by(sample_month) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        unique_zones = if ("health_zone" %in% names(.)) dplyr::n_distinct(health_zone, na.rm = TRUE) else 0,
        unique_structures = if ("structure" %in% names(.)) dplyr::n_distinct(structure, na.rm = TRUE) else 0,
        .groups = "drop"
      ) %>%
      dplyr::arrange(sample_month)

    results$monthly_trend <- monthly_trend

    # Monthly positivity trend if MIC data available
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      mic_df <- standardize_columns(mic_df)
      mic_date_col <- get_col_name(mic_df, c("date_prel", "DatePrel", "sample_date", "plate_date"))

      if (!is.null(mic_date_col)) {
        mic_df$date_prel <- as.Date(mic_df[[mic_date_col]])

        # Get FinalCall column
        final_call_col <- get_col_name(mic_df, c("FinalCall", "final_call", "Result", "result"))

        if (!is.null(final_call_col)) {
          monthly_positivity <- mic_df %>%
            dplyr::filter(!is.na(date_prel)) %>%
            dplyr::mutate(
              sample_month = lubridate::floor_date(date_prel, "month")
            ) %>%
            dplyr::filter(!is.na(sample_month)) %>%
            dplyr::group_by(sample_month) %>%
            dplyr::summarise(
              mic_tested = dplyr::n(),
              mic_positive = sum(grepl("Positive|Pos|DNA\\+|RNA\\+", .data[[final_call_col]], ignore.case = TRUE), na.rm = TRUE),
              .groups = "drop"
            ) %>%
            dplyr::mutate(
              positivity_rate = dplyr::if_else(mic_tested > 0, mic_positive / mic_tested * 100, 0)
            ) %>%
            dplyr::arrange(sample_month)

        # Add moving average if enough data
        if (nrow(monthly_positivity) >= 3) {
          monthly_positivity <- monthly_positivity %>%
            dplyr::mutate(
              positivity_ma = zoo::rollmean(positivity_rate, k = 3, fill = NA, align = "right")
            )
        }

        results$positivity_trend <- monthly_positivity

        # Simple trend-based forecast
        if (nrow(monthly_positivity) >= 6) {
          recent_data <- tail(monthly_positivity, 6)
          avg_positivity <- mean(recent_data$positivity_rate, na.rm = TRUE)
          trend_direction <- (tail(recent_data$positivity_rate, 1) - head(recent_data$positivity_rate, 1)) / 6

          # Generate forecast
          last_month <- max(monthly_positivity$sample_month)
          forecast_months_seq <- seq(last_month, by = "month", length.out = forecast_months + 1)[-1]

          forecast_data <- data.frame(
            sample_month = forecast_months_seq,
            predicted_positivity = pmax(0, avg_positivity + trend_direction * seq_len(forecast_months)),
            type = "Forecast"
          )

          results$forecast <- forecast_data
          results$trend_summary <- list(
            recent_avg_positivity = round(avg_positivity, 2),
            trend_direction = dplyr::case_when(
              trend_direction > 0.5 ~ "Increasing",
              trend_direction < -0.5 ~ "Decreasing",
              TRUE ~ "Stable"
            ),
            confidence = ifelse(nrow(monthly_positivity) >= 12, "High", "Moderate")
          )
        }
        }  # Close final_call_col check
      }
    }

    # Seasonal patterns (by month of year)
    seasonal_pattern <- biobank_df %>%
      dplyr::filter(!is.na(date_prel)) %>%
      dplyr::mutate(
        month_of_year = lubridate::month(date_prel, label = TRUE)
      ) %>%
      dplyr::filter(!is.na(month_of_year)) %>%
      dplyr::group_by(month_of_year) %>%
      dplyr::summarise(
        avg_samples = dplyr::n(),
        .groups = "drop"
      )

    results$seasonal_pattern <- seasonal_pattern

    results

  }, error = function(e) {
    warning(paste("Error in calculate_temporal_predictions:", e$message))
    list(
      monthly_trend = data.frame(),
      error = e$message
    )
  })
}


#' Generate Watchlist Report
#'
#' Creates a prioritized watchlist of health zones and structures
#' that should be monitored closely for HAT cases.
#'
#' @param zone_risk Data frame from calculate_healthzone_risk()
#' @param structure_risk Data frame from calculate_structure_risk()
#' @param demographic_risk List from calculate_demographic_risk()
#' @param top_n Number of items to include in each category
#' @return List with watchlist components
#' @export
generate_watchlist <- function(zone_risk, structure_risk, demographic_risk = NULL,
                                top_n = 10) {
  tryCatch({
    watchlist <- list()

    # Check for valid data
    if (is.null(zone_risk) || nrow(zone_risk) == 0) {
      zone_risk <- data.frame(
        health_zone = character(),
        risk_score = numeric(),
        risk_category = factor(levels = c("Low", "Medium", "High", "Very High"))
      )
    }

    if (is.null(structure_risk) || nrow(structure_risk) == 0) {
      structure_risk <- data.frame(
        health_zone = character(),
        structure = character(),
        risk_score = numeric(),
        risk_category = factor(levels = c("Low", "Medium", "High", "Very High")),
        prediction = character(),
        historical_positivity = numeric(),
        days_since_last = numeric()
      )
    }

    # Top risk health zones
    watchlist$priority_zones <- zone_risk %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(
        health_zone, risk_score, risk_category,
        dplyr::any_of(c("total_samples", "mic_any_positive", "mic_positivity_rate",
                        "elisa_positive", "elisa_positivity_rate"))
      )

    # Top risk structures
    watchlist$priority_structures <- structure_risk %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(
        health_zone, structure, risk_score, risk_category, prediction,
        dplyr::any_of(c("total_samples", "historical_positivity", "days_since_last"))
      )

    # Structures with recent positives (high alert)
    if ("historical_positivity" %in% names(structure_risk) && "days_since_last" %in% names(structure_risk)) {
      watchlist$recent_positives <- structure_risk %>%
        dplyr::filter(historical_positivity > 0, days_since_last <= 90) %>%
        dplyr::arrange(dplyr::desc(historical_positivity)) %>%
        dplyr::slice_head(n = top_n)
    } else {
      watchlist$recent_positives <- data.frame()
    }

    # Emerging hotspots (structures with increasing activity)
    if ("days_since_last" %in% names(structure_risk)) {
      watchlist$emerging_hotspots <- structure_risk %>%
        dplyr::filter(
          risk_category %in% c("High", "Very High"),
          days_since_last <= 60
        ) %>%
        dplyr::arrange(dplyr::desc(risk_score)) %>%
        dplyr::slice_head(n = top_n)
    } else {
      watchlist$emerging_hotspots <- data.frame()
    }

    # Summary statistics
    watchlist$summary <- list(
      total_zones_monitored = nrow(zone_risk),
      high_risk_zones = sum(zone_risk$risk_category %in% c("High", "Very High"), na.rm = TRUE),
      total_structures_monitored = nrow(structure_risk),
      high_risk_structures = sum(structure_risk$risk_category %in% c("High", "Very High"), na.rm = TRUE),
      structures_with_positives = if ("historical_positivity" %in% names(structure_risk)) {
        sum(structure_risk$historical_positivity > 0, na.rm = TRUE)
      } else 0
    )

    # Add demographic insights if available
    if (!is.null(demographic_risk)) {
      if (!is.null(demographic_risk$sex_risk) && "mic_positivity" %in% names(demographic_risk$sex_risk)) {
        watchlist$demographic_focus <- list(
          highest_risk_sex = demographic_risk$sex_risk %>%
            dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
            dplyr::pull(Sex),
          sex_positivity = demographic_risk$sex_risk %>%
            dplyr::select(Sex, dplyr::any_of(c("mic_tested", "mic_positive", "mic_positivity")))
        )
      }

      if (!is.null(demographic_risk$age_risk) && "mic_positivity" %in% names(demographic_risk$age_risk)) {
        if (is.null(watchlist$demographic_focus)) watchlist$demographic_focus <- list()
        watchlist$demographic_focus$highest_risk_age <- demographic_risk$age_risk %>%
          dplyr::filter(mic_positivity == max(mic_positivity, na.rm = TRUE)) %>%
          dplyr::pull(age_group)
        watchlist$demographic_focus$age_positivity <- demographic_risk$age_risk %>%
          dplyr::select(age_group, dplyr::any_of(c("mic_tested", "mic_positive", "mic_positivity")))
      }
    }

    watchlist

  }, error = function(e) {
    warning(paste("Error in generate_watchlist:", e$message))
    list(
      priority_zones = data.frame(),
      priority_structures = data.frame(),
      recent_positives = data.frame(),
      emerging_hotspots = data.frame(),
      summary = list(
        total_zones_monitored = 0,
        high_risk_zones = 0,
        total_structures_monitored = 0,
        high_risk_structures = 0,
        structures_with_positives = 0
      ),
      error = e$message
    )
  })
}
