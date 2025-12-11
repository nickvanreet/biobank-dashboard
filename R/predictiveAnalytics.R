# Predictive Analytics Module
# Epidemiological forecasting and risk prediction for HAT surveillance
# ============================================================================

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
    # Get health zone data from biobank
    zone_data <- biobank_df %>%
      dplyr::filter(!is.na(HealthZone)) %>%
      dplyr::mutate(
        collection_month = floor_date(as.Date(DatePrel), "month")
      )

    # Calculate base metrics per zone
    zone_summary <- zone_data %>%
      dplyr::group_by(HealthZone) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        unique_structures = dplyr::n_distinct(Structure, na.rm = TRUE),
        date_range_days = as.numeric(difftime(max(DatePrel, na.rm = TRUE),
                                               min(DatePrel, na.rm = TRUE), units = "days")),
        last_sample_date = max(DatePrel, na.rm = TRUE),
        .groups = "drop"
      )

    # Calculate molecular positivity if MIC data available
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      mic_positivity <- mic_df %>%
        dplyr::group_by(HealthZone) %>%
        dplyr::summarise(
          mic_tested = dplyr::n(),
          mic_dna_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          mic_rna_positive = sum(RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          mic_any_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                                   RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          mic_positivity_rate = mic_any_positive / mic_tested * 100
        )
      zone_summary <- dplyr::left_join(zone_summary, mic_positivity, by = "HealthZone")
    }

    # Calculate serological positivity if ELISA data available
    if (!is.null(elisa_df) && nrow(elisa_df) > 0) {
      elisa_positivity <- elisa_df %>%
        dplyr::group_by(HealthZone) %>%
        dplyr::summarise(
          elisa_tested = dplyr::n(),
          elisa_positive = sum(Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          elisa_positivity_rate = elisa_positive / elisa_tested * 100
        )
      zone_summary <- dplyr::left_join(zone_summary, elisa_positivity, by = "HealthZone")
    }

    # Calculate iELISA positivity if available
    if (!is.null(ielisa_df) && nrow(ielisa_df) > 0) {
      ielisa_positivity <- ielisa_df %>%
        dplyr::group_by(HealthZone) %>%
        dplyr::summarise(
          ielisa_tested = dplyr::n(),
          ielisa_l13_positive = sum(L13_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          ielisa_l15_positive = sum(L15_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          ielisa_any_positive = sum(L13_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                                      L15_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          ielisa_positivity_rate = ielisa_any_positive / ielisa_tested * 100
        )
      zone_summary <- dplyr::left_join(zone_summary, ielisa_positivity, by = "HealthZone")
    }

    # Replace NAs with 0 for numeric columns
    zone_summary <- zone_summary %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

    # Calculate composite risk score
    zone_summary <- zone_summary %>%
      dplyr::mutate(
        # Normalize metrics to 0-100 scale
        sample_density_score = scales::rescale(total_samples, to = c(0, 100)),
        # Higher positivity = higher risk
        molecular_risk = dplyr::if_else(
          "mic_positivity_rate" %in% names(.),
          scales::rescale(mic_positivity_rate, to = c(0, 100)),
          0
        ),
        serological_risk = dplyr::case_when(
          "elisa_positivity_rate" %in% names(.) & "ielisa_positivity_rate" %in% names(.) ~
            scales::rescale((elisa_positivity_rate + ielisa_positivity_rate) / 2, to = c(0, 100)),
          "elisa_positivity_rate" %in% names(.) ~ scales::rescale(elisa_positivity_rate, to = c(0, 100)),
          "ielisa_positivity_rate" %in% names(.) ~ scales::rescale(ielisa_positivity_rate, to = c(0, 100)),
          TRUE ~ 0
        ),
        # Recency factor - more recent activity = higher monitoring priority
        days_since_last = as.numeric(Sys.Date() - last_sample_date),
        recency_score = scales::rescale(pmax(0, 180 - days_since_last), to = c(0, 100)),
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
      HealthZone = character(),
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
    # Get structure data from biobank
    structure_data <- biobank_df %>%
      dplyr::filter(!is.na(Structure), !is.na(HealthZone)) %>%
      dplyr::group_by(HealthZone, Structure) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        first_sample = min(DatePrel, na.rm = TRUE),
        last_sample = max(DatePrel, na.rm = TRUE),
        unique_dates = dplyr::n_distinct(as.Date(DatePrel)),
        .groups = "drop"
      )

    # Add MIC positivity by structure
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      mic_structure <- mic_df %>%
        dplyr::filter(!is.na(Structure)) %>%
        dplyr::group_by(HealthZone, Structure) %>%
        dplyr::summarise(
          mic_tested = dplyr::n(),
          mic_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                               RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(mic_positivity = mic_positive / mic_tested * 100)

      structure_data <- dplyr::left_join(structure_data, mic_structure,
                                          by = c("HealthZone", "Structure"))
    }

    # Add serological positivity by structure
    if (!is.null(elisa_df) && nrow(elisa_df) > 0) {
      elisa_structure <- elisa_df %>%
        dplyr::filter(!is.na(Structure)) %>%
        dplyr::group_by(HealthZone, Structure) %>%
        dplyr::summarise(
          elisa_tested = dplyr::n(),
          elisa_positive = sum(Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(elisa_positivity = elisa_positive / elisa_tested * 100)

      structure_data <- dplyr::left_join(structure_data, elisa_structure,
                                          by = c("HealthZone", "Structure"))
    }

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
      HealthZone = character(),
      Structure = character(),
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

    # Sex-based risk analysis
    sex_risk <- biobank_df %>%
      dplyr::filter(!is.na(Sex), Sex %in% c("M", "F", "Male", "Female", "Homme", "Femme")) %>%
      dplyr::mutate(
        Sex = dplyr::case_when(
          Sex %in% c("M", "Male", "Homme") ~ "Male",
          Sex %in% c("F", "Female", "Femme") ~ "Female",
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
    if (!is.null(mic_df) && nrow(mic_df) > 0 && "Sex" %in% names(mic_df)) {
      mic_sex <- mic_df %>%
        dplyr::mutate(
          Sex = dplyr::case_when(
            Sex %in% c("M", "Male", "Homme") ~ "Male",
            Sex %in% c("F", "Female", "Femme") ~ "Female",
            TRUE ~ NA_character_
          )
        ) %>%
        dplyr::filter(!is.na(Sex)) %>%
        dplyr::group_by(Sex) %>%
        dplyr::summarise(
          mic_tested = dplyr::n(),
          mic_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                               RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(mic_positivity = mic_positive / mic_tested * 100)

      sex_risk <- dplyr::left_join(sex_risk, mic_sex, by = "Sex")
    }

    results$sex_risk <- sex_risk

    # Age group risk analysis
    age_risk <- biobank_df %>%
      dplyr::filter(!is.na(Age), Age >= 0, Age <= 120) %>%
      dplyr::mutate(
        age_group = dplyr::case_when(
          Age < 5 ~ "0-4",
          Age < 15 ~ "5-14",
          Age < 25 ~ "15-24",
          Age < 35 ~ "25-34",
          Age < 45 ~ "35-44",
          Age < 55 ~ "45-54",
          Age < 65 ~ "55-64",
          TRUE ~ "65+"
        ),
        age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34",
                                                   "35-44", "45-54", "55-64", "65+"))
      ) %>%
      dplyr::group_by(age_group) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        mean_age = mean(Age, na.rm = TRUE),
        .groups = "drop"
      )

    # Add positivity data by age if available
    if (!is.null(mic_df) && nrow(mic_df) > 0 && "Age" %in% names(mic_df)) {
      mic_age <- mic_df %>%
        dplyr::filter(!is.na(Age), Age >= 0, Age <= 120) %>%
        dplyr::mutate(
          age_group = dplyr::case_when(
            Age < 5 ~ "0-4",
            Age < 15 ~ "5-14",
            Age < 25 ~ "15-24",
            Age < 35 ~ "25-34",
            Age < 45 ~ "35-44",
            Age < 55 ~ "45-54",
            Age < 65 ~ "55-64",
            TRUE ~ "65+"
          ),
          age_group = factor(age_group, levels = c("0-4", "5-14", "15-24", "25-34",
                                                     "35-44", "45-54", "55-64", "65+"))
        ) %>%
        dplyr::group_by(age_group) %>%
        dplyr::summarise(
          mic_tested = dplyr::n(),
          mic_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                               RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(mic_positivity = mic_positive / mic_tested * 100)

      age_risk <- dplyr::left_join(age_risk, mic_age, by = "age_group")
    }

    results$age_risk <- age_risk

    # Combined sex + age analysis
    combined_risk <- biobank_df %>%
      dplyr::filter(
        !is.na(Sex), Sex %in% c("M", "F", "Male", "Female", "Homme", "Femme"),
        !is.na(Age), Age >= 0, Age <= 120
      ) %>%
      dplyr::mutate(
        Sex = dplyr::case_when(
          Sex %in% c("M", "Male", "Homme") ~ "Male",
          Sex %in% c("F", "Female", "Femme") ~ "Female",
          TRUE ~ NA_character_
        ),
        age_group = dplyr::case_when(
          Age < 15 ~ "0-14",
          Age < 30 ~ "15-29",
          Age < 45 ~ "30-44",
          Age < 60 ~ "45-59",
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

    # Monthly sampling trends
    monthly_trend <- biobank_df %>%
      dplyr::mutate(
        sample_month = lubridate::floor_date(as.Date(DatePrel), "month")
      ) %>%
      dplyr::filter(!is.na(sample_month)) %>%
      dplyr::group_by(sample_month) %>%
      dplyr::summarise(
        total_samples = dplyr::n(),
        unique_zones = dplyr::n_distinct(HealthZone, na.rm = TRUE),
        unique_structures = dplyr::n_distinct(Structure, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(sample_month)

    results$monthly_trend <- monthly_trend

    # Monthly positivity trend if MIC data available
    if (!is.null(mic_df) && nrow(mic_df) > 0) {
      monthly_positivity <- mic_df %>%
        dplyr::mutate(
          sample_month = lubridate::floor_date(as.Date(DatePrel), "month")
        ) %>%
        dplyr::filter(!is.na(sample_month)) %>%
        dplyr::group_by(sample_month) %>%
        dplyr::summarise(
          mic_tested = dplyr::n(),
          mic_positive = sum(DNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos") |
                               RNA_Result %in% c("Positive", "POSITIVE", "POS", "Pos"), na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          positivity_rate = mic_positive / mic_tested * 100,
          # Moving average for trend
          positivity_ma = zoo::rollmean(positivity_rate, k = 3, fill = NA, align = "right")
        ) %>%
        dplyr::arrange(sample_month)

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
    }

    # Seasonal patterns (by month of year)
    seasonal_pattern <- biobank_df %>%
      dplyr::mutate(
        month_of_year = lubridate::month(as.Date(DatePrel), label = TRUE)
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


#' Build Predictive Model for Positivity
#'
#' Uses machine learning to predict which samples are likely to be positive
#' based on geographic, demographic, and temporal features.
#'
#' @param training_data Data frame with known outcomes
#' @param feature_cols Character vector of feature column names
#' @param outcome_col Character name of outcome column
#' @param model_type Character type of model ("rf" or "xgboost")
#' @return List with model and predictions
#' @export
build_positivity_predictor <- function(training_data, feature_cols, outcome_col,
                                        model_type = "rf") {
  tryCatch({
    # Use existing ML infrastructure from concordancePredictive.R
    model_result <- build_predictive_model(
      data = training_data,
      outcome_col = outcome_col,
      feature_cols = feature_cols,
      model_type = model_type,
      train_fraction = 0.75,
      cv_folds = 5
    )

    if (is.null(model_result$model)) {
      return(list(
        model = NULL,
        error = model_result$error
      ))
    }

    # Add prediction summary
    model_result$prediction_summary <- list(
      model_type = toupper(model_type),
      features_used = feature_cols,
      outcome_variable = outcome_col,
      training_samples = nrow(model_result$train_predictions),
      test_samples = nrow(model_result$test_predictions),
      test_accuracy = model_result$test_performance %>%
        dplyr::filter(metric == "Accuracy") %>%
        dplyr::pull(value),
      test_auc = model_result$test_performance %>%
        dplyr::filter(metric == "AUC") %>%
        dplyr::pull(value)
    )

    model_result

  }, error = function(e) {
    warning(paste("Error in build_positivity_predictor:", e$message))
    list(model = NULL, error = e$message)
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

    # Top risk health zones
    watchlist$priority_zones <- zone_risk %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(
        HealthZone, risk_score, risk_category,
        dplyr::any_of(c("total_samples", "mic_any_positive", "mic_positivity_rate",
                        "elisa_positive", "elisa_positivity_rate"))
      )

    # Top risk structures
    watchlist$priority_structures <- structure_risk %>%
      dplyr::slice_head(n = top_n) %>%
      dplyr::select(
        HealthZone, Structure, risk_score, risk_category, prediction,
        dplyr::any_of(c("total_samples", "historical_positivity", "days_since_last"))
      )

    # Structures with recent positives (high alert)
    watchlist$recent_positives <- structure_risk %>%
      dplyr::filter(historical_positivity > 0, days_since_last <= 90) %>%
      dplyr::arrange(dplyr::desc(historical_positivity)) %>%
      dplyr::slice_head(n = top_n)

    # Emerging hotspots (structures with increasing activity)
    watchlist$emerging_hotspots <- structure_risk %>%
      dplyr::filter(
        risk_category %in% c("High", "Very High"),
        days_since_last <= 60
      ) %>%
      dplyr::arrange(dplyr::desc(risk_score)) %>%
      dplyr::slice_head(n = top_n)

    # Summary statistics
    watchlist$summary <- list(
      total_zones_monitored = nrow(zone_risk),
      high_risk_zones = sum(zone_risk$risk_category %in% c("High", "Very High")),
      total_structures_monitored = nrow(structure_risk),
      high_risk_structures = sum(structure_risk$risk_category %in% c("High", "Very High")),
      structures_with_positives = sum(structure_risk$historical_positivity > 0, na.rm = TRUE)
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
    list(error = e$message)
  })
}


#' Create Risk Prediction Visualizations
#'
#' Generates plotly visualizations for the predictive analytics module.
#'
#' @param zone_risk Data frame from calculate_healthzone_risk()
#' @param color_palette Character vector of colors for risk categories
#' @return List of plotly objects
#' @export
create_risk_visualizations <- function(zone_risk,
                                        color_palette = c("Low" = "#28a745",
                                                          "Medium" = "#ffc107",
                                                          "High" = "#fd7e14",
                                                          "Very High" = "#dc3545")) {
  tryCatch({
    plots <- list()

    # Risk score bar chart
    plots$risk_bar <- plotly::plot_ly(
      data = zone_risk %>% dplyr::slice_head(n = 15),
      x = ~reorder(HealthZone, risk_score),
      y = ~risk_score,
      color = ~risk_category,
      colors = color_palette,
      type = "bar",
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Risk Score: %{y:.1f}<br>",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "Health Zone Risk Ranking",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = "Risk Score (0-100)"),
        showlegend = TRUE,
        legend = list(title = list(text = "Risk Level"))
      )

    # Risk distribution pie
    risk_dist <- zone_risk %>%
      dplyr::count(risk_category) %>%
      dplyr::mutate(pct = n / sum(n) * 100)

    plots$risk_pie <- plotly::plot_ly(
      data = risk_dist,
      labels = ~risk_category,
      values = ~n,
      type = "pie",
      marker = list(colors = color_palette[risk_dist$risk_category]),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Zones: %{value}<br>",
        "Percentage: %{percent}<br>",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "Risk Distribution",
        showlegend = TRUE
      )

    # Risk score gauge for top zone
    if (nrow(zone_risk) > 0) {
      top_zone <- zone_risk[1, ]
      plots$top_zone_gauge <- plotly::plot_ly(
        type = "indicator",
        mode = "gauge+number+delta",
        value = top_zone$risk_score,
        title = list(text = paste("Highest Risk:", top_zone$HealthZone)),
        gauge = list(
          axis = list(range = list(0, 100)),
          bar = list(color = color_palette[as.character(top_zone$risk_category)]),
          steps = list(
            list(range = c(0, 25), color = "#e8f5e9"),
            list(range = c(25, 50), color = "#fff3e0"),
            list(range = c(50, 75), color = "#ffe0b2"),
            list(range = c(75, 100), color = "#ffcdd2")
          ),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = 75
          )
        )
      ) %>%
        plotly::layout(
          margin = list(l = 20, r = 20, t = 60, b = 20)
        )
    }

    plots

  }, error = function(e) {
    warning(paste("Error in create_risk_visualizations:", e$message))
    list()
  })
}
