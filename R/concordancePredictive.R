# Concordance Predictive Modeling Module
# Machine learning models for predicting test outcomes and discordance
# ============================================================================

#' Build predictive models for test outcomes
#'
#' @param data Data frame with features and outcomes
#' @param outcome_col Character name of outcome column
#' @param feature_cols Character vector of feature column names
#' @param model_type Character type of model ("rf" or "xgboost")
#' @param train_fraction Numeric fraction for training set (default: 0.75)
#' @param cv_folds Integer number of cross-validation folds (default: 5)
#' @return List with model, performance metrics, and predictions
#' @export
build_predictive_model <- function(data,
                                   outcome_col,
                                   feature_cols,
                                   model_type = "rf",
                                   train_fraction = 0.75,
                                   cv_folds = 5) {

  tryCatch({
    # Prepare data
    data_complete <- data %>%
      dplyr::select(dplyr::all_of(c(outcome_col, feature_cols))) %>%
      tidyr::drop_na()

    if (nrow(data_complete) < 50) {
      return(list(
        model = NULL,
        performance = NULL,
        error = "Insufficient data (minimum 50 complete cases required)"
      ))
    }

    # Convert outcome to factor
    data_complete[[outcome_col]] <- as.factor(data_complete[[outcome_col]])

    # Train/test split
    set.seed(42)
    train_idx <- sample(1:nrow(data_complete),
                       size = floor(train_fraction * nrow(data_complete)))
    train_data <- data_complete[train_idx, ]
    test_data <- data_complete[-train_idx, ]

    # Prepare formula
    formula_str <- paste(outcome_col, "~",
                        paste(feature_cols, collapse = " + "))
    formula_obj <- as.formula(formula_str)

    # Build model based on type
    if (model_type == "rf") {
      # Random Forest
      model <- tryCatch({
        randomForest::randomForest(
          formula_obj,
          data = train_data,
          ntree = 500,
          mtry = max(1, floor(sqrt(length(feature_cols)))),
          importance = TRUE,
          proximity = FALSE
        )
      }, error = function(e) {
        NULL
      })

      if (is.null(model)) {
        return(list(
          model = NULL,
          performance = NULL,
          error = "Random Forest model failed to build"
        ))
      }

      # Make predictions
      train_pred <- predict(model, train_data, type = "prob")[, 2]
      test_pred <- predict(model, test_data, type = "prob")[, 2]
      train_pred_class <- predict(model, train_data, type = "response")
      test_pred_class <- predict(model, test_data, type = "response")

      # Variable importance
      importance <- randomForest::importance(model)
      var_importance <- data.frame(
        feature = rownames(importance),
        importance = importance[, "MeanDecreaseGini"],
        stringsAsFactors = FALSE
      ) %>%
        dplyr::arrange(dplyr::desc(importance))

    } else if (model_type == "xgboost") {
      # XGBoost
      # Prepare matrices
      train_matrix <- xgboost::xgb.DMatrix(
        data = as.matrix(train_data[, feature_cols]),
        label = as.numeric(train_data[[outcome_col]]) - 1
      )
      test_matrix <- xgboost::xgb.DMatrix(
        data = as.matrix(test_data[, feature_cols]),
        label = as.numeric(test_data[[outcome_col]]) - 1
      )

      # Train model
      model <- tryCatch({
        xgboost::xgb.train(
          params = list(
            objective = "binary:logistic",
            eval_metric = "auc",
            max_depth = 6,
            eta = 0.3,
            subsample = 0.8,
            colsample_bytree = 0.8
          ),
          data = train_matrix,
          nrounds = 100,
          watchlist = list(train = train_matrix, test = test_matrix),
          early_stopping_rounds = 10,
          verbose = 0
        )
      }, error = function(e) {
        NULL
      })

      if (is.null(model)) {
        return(list(
          model = NULL,
          performance = NULL,
          error = "XGBoost model failed to build"
        ))
      }

      # Make predictions
      train_pred <- predict(model, train_matrix)
      test_pred <- predict(model, test_matrix)
      train_pred_class <- factor(ifelse(train_pred > 0.5, levels(train_data[[outcome_col]])[2],
                                        levels(train_data[[outcome_col]])[1]))
      test_pred_class <- factor(ifelse(test_pred > 0.5, levels(test_data[[outcome_col]])[2],
                                       levels(test_data[[outcome_col]])[1]))

      # Variable importance
      importance_matrix <- xgboost::xgb.importance(
        feature_names = feature_cols,
        model = model
      )
      var_importance <- data.frame(
        feature = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )

    } else {
      return(list(
        model = NULL,
        performance = NULL,
        error = "Invalid model_type. Choose 'rf' or 'xgboost'"
      ))
    }

    # Calculate performance metrics
    train_performance <- calculate_classification_metrics(
      actual = train_data[[outcome_col]],
      predicted_prob = train_pred,
      predicted_class = train_pred_class
    )

    test_performance <- calculate_classification_metrics(
      actual = test_data[[outcome_col]],
      predicted_prob = test_pred,
      predicted_class = test_pred_class
    )

    # Cross-validation
    cv_results <- tryCatch({
      perform_cross_validation(
        data = data_complete,
        formula = formula_obj,
        model_type = model_type,
        cv_folds = cv_folds,
        feature_cols = feature_cols,
        outcome_col = outcome_col
      )
    }, error = function(e) {
      NULL
    })

    # Return results
    list(
      model = model,
      model_type = model_type,
      feature_cols = feature_cols,
      outcome_col = outcome_col,
      train_performance = train_performance,
      test_performance = test_performance,
      cv_results = cv_results,
      variable_importance = var_importance,
      train_predictions = data.frame(
        actual = train_data[[outcome_col]],
        predicted_prob = train_pred,
        predicted_class = train_pred_class
      ),
      test_predictions = data.frame(
        actual = test_data[[outcome_col]],
        predicted_prob = test_pred,
        predicted_class = test_pred_class
      )
    )

  }, error = function(e) {
    warning(paste("Error in build_predictive_model:", e$message))
    list(
      model = NULL,
      performance = NULL,
      error = e$message
    )
  })
}


#' Calculate classification performance metrics
#'
#' @param actual Factor or logical actual outcomes
#' @param predicted_prob Numeric predicted probabilities
#' @param predicted_class Factor or logical predicted classes
#' @return Data frame with performance metrics
calculate_classification_metrics <- function(actual, predicted_prob, predicted_class) {

  tryCatch({
    # Convert to factors with same levels
    actual_factor <- as.factor(actual)
    predicted_factor <- factor(predicted_class, levels = levels(actual_factor))

    # Confusion matrix
    cm <- table(Actual = actual_factor, Predicted = predicted_factor)

    # Extract values (assuming binary classification)
    if (nrow(cm) == 2 && ncol(cm) == 2) {
      TP <- cm[2, 2]
      FP <- cm[1, 2]
      FN <- cm[2, 1]
      TN <- cm[1, 1]
    } else {
      return(data.frame(metric = "Error", value = NA_real_))
    }

    # Calculate metrics
    accuracy <- (TP + TN) / (TP + TN + FP + FN)
    sensitivity <- TP / (TP + FN)
    specificity <- TN / (TN + FP)
    ppv <- TP / (TP + FP)
    npv <- TN / (TN + FN)
    f1_score <- 2 * (ppv * sensitivity) / (ppv + sensitivity)

    # ROC AUC
    roc_auc <- tryCatch({
      roc_obj <- pROC::roc(actual_factor, predicted_prob, quiet = TRUE)
      as.numeric(roc_obj$auc)
    }, error = function(e) {
      NA_real_
    })

    # Brier score
    brier_score <- mean((predicted_prob - as.numeric(actual_factor == levels(actual_factor)[2]))^2)

    data.frame(
      metric = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV",
                "F1 Score", "AUC", "Brier Score"),
      value = c(accuracy, sensitivity, specificity, ppv, npv,
               f1_score, roc_auc, brier_score),
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(paste("Error in calculate_classification_metrics:", e$message))
    data.frame(metric = "Error", value = NA_real_)
  })
}


#' Perform k-fold cross-validation
#'
#' @param data Data frame with complete cases
#' @param formula Formula object
#' @param model_type Character model type
#' @param cv_folds Integer number of folds
#' @param feature_cols Character vector of features
#' @param outcome_col Character outcome column
#' @return Data frame with CV results
perform_cross_validation <- function(data, formula, model_type, cv_folds,
                                     feature_cols, outcome_col) {

  tryCatch({
    set.seed(42)
    n <- nrow(data)
    fold_indices <- sample(rep(1:cv_folds, length.out = n))

    cv_results <- lapply(1:cv_folds, function(fold) {
      # Split data
      test_idx <- which(fold_indices == fold)
      train_data <- data[-test_idx, ]
      test_data <- data[test_idx, ]

      # Build model
      if (model_type == "rf") {
        model <- randomForest::randomForest(
          formula,
          data = train_data,
          ntree = 500,
          mtry = max(1, floor(sqrt(length(feature_cols))))
        )
        test_pred <- predict(model, test_data, type = "prob")[, 2]
        test_pred_class <- predict(model, test_data, type = "response")
      } else {
        train_matrix <- xgboost::xgb.DMatrix(
          data = as.matrix(train_data[, feature_cols]),
          label = as.numeric(train_data[[outcome_col]]) - 1
        )
        test_matrix <- xgboost::xgb.DMatrix(
          data = as.matrix(test_data[, feature_cols])
        )
        model <- xgboost::xgb.train(
          params = list(objective = "binary:logistic", eval_metric = "auc"),
          data = train_matrix,
          nrounds = 100,
          verbose = 0
        )
        test_pred <- predict(model, test_matrix)
        test_pred_class <- factor(ifelse(test_pred > 0.5,
                                         levels(test_data[[outcome_col]])[2],
                                         levels(test_data[[outcome_col]])[1]))
      }

      # Calculate metrics
      metrics <- calculate_classification_metrics(
        actual = test_data[[outcome_col]],
        predicted_prob = test_pred,
        predicted_class = test_pred_class
      )

      metrics$fold <- fold
      return(metrics)
    })

    # Combine results
    cv_results_df <- dplyr::bind_rows(cv_results)

    # Calculate mean and SD across folds
    cv_summary <- cv_results_df %>%
      dplyr::group_by(metric) %>%
      dplyr::summarise(
        mean = mean(value, na.rm = TRUE),
        sd = sd(value, na.rm = TRUE),
        .groups = "drop"
      )

    list(
      fold_results = cv_results_df,
      summary = cv_summary
    )

  }, error = function(e) {
    warning(paste("Error in perform_cross_validation:", e$message))
    NULL
  })
}


#' Predict test outcomes with probability
#'
#' @param model Trained model object
#' @param new_data Data frame with new observations
#' @param model_type Character model type
#' @param feature_cols Character vector of features
#' @return Data frame with predictions
#' @export
predict_test_outcomes <- function(model, new_data, model_type, feature_cols) {

  tryCatch({
    if (is.null(model)) {
      return(data.frame(
        predicted_prob = numeric(),
        predicted_class = character(),
        error = "No model provided"
      ))
    }

    # Prepare data
    new_data_complete <- new_data %>%
      dplyr::select(dplyr::all_of(feature_cols)) %>%
      tidyr::drop_na()

    if (nrow(new_data_complete) == 0) {
      return(data.frame(
        predicted_prob = numeric(),
        predicted_class = character(),
        error = "No complete cases in new_data"
      ))
    }

    # Make predictions
    if (model_type == "rf") {
      predicted_prob <- predict(model, new_data_complete, type = "prob")[, 2]
      predicted_class <- predict(model, new_data_complete, type = "response")
    } else if (model_type == "xgboost") {
      new_matrix <- xgboost::xgb.DMatrix(data = as.matrix(new_data_complete))
      predicted_prob <- predict(model, new_matrix)
      predicted_class <- ifelse(predicted_prob > 0.5, "Positive", "Negative")
    } else {
      return(data.frame(
        predicted_prob = numeric(),
        predicted_class = character(),
        error = "Invalid model_type"
      ))
    }

    # Return predictions
    data.frame(
      predicted_prob = predicted_prob,
      predicted_class = predicted_class,
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(paste("Error in predict_test_outcomes:", e$message))
    data.frame(
      predicted_prob = numeric(),
      predicted_class = character(),
      error = e$message
    )
  })
}


#' Predict discordance risk
#'
#' @param data Data frame with test results and features
#' @param test1_col Character name of test 1 result column
#' @param test2_col Character name of test 2 result column
#' @param feature_cols Character vector of feature columns
#' @return List with risk model and predictions
#' @export
predict_discordance_risk <- function(data, test1_col, test2_col, feature_cols) {

  tryCatch({
    # Create discordance outcome
    data_with_outcome <- data %>%
      dplyr::mutate(
        is_discordant = (.data[[test1_col]] != .data[[test2_col]])
      )

    # Build model
    model_result <- build_predictive_model(
      data = data_with_outcome,
      outcome_col = "is_discordant",
      feature_cols = feature_cols,
      model_type = "rf"
    )

    if (is.null(model_result$model)) {
      return(list(
        model = NULL,
        risk_scores = NULL,
        error = model_result$error
      ))
    }

    # Calculate risk scores for all data
    risk_predictions <- predict_test_outcomes(
      model = model_result$model,
      new_data = data_with_outcome,
      model_type = "rf",
      feature_cols = feature_cols
    )

    # Add risk categories
    risk_predictions$risk_category <- cut(
      risk_predictions$predicted_prob,
      breaks = c(0, 0.2, 0.5, 0.8, 1),
      labels = c("Low", "Medium", "High", "Very High"),
      include.lowest = TRUE
    )

    list(
      model = model_result$model,
      performance = model_result$test_performance,
      risk_scores = risk_predictions,
      variable_importance = model_result$variable_importance
    )

  }, error = function(e) {
    warning(paste("Error in predict_discordance_risk:", e$message))
    list(
      model = NULL,
      risk_scores = NULL,
      error = e$message
    )
  })
}


#' Calculate SHAP values for model interpretability
#'
#' @param model Trained model object
#' @param data Data frame with features
#' @param model_type Character model type
#' @param feature_cols Character vector of features
#' @param n_samples Integer number of samples for SHAP (default: 100)
#' @return List with SHAP values and summary
#' @export
calculate_shap_values <- function(model, data, model_type, feature_cols, n_samples = 100) {

  tryCatch({
    if (is.null(model)) {
      return(list(
        shap_values = NULL,
        error = "No model provided"
      ))
    }

    # Sample data if too large
    if (nrow(data) > n_samples) {
      set.seed(42)
      data_sample <- data[sample(1:nrow(data), n_samples), ]
    } else {
      data_sample <- data
    }

    # Prepare data
    data_features <- data_sample %>%
      dplyr::select(dplyr::all_of(feature_cols)) %>%
      tidyr::drop_na()

    if (nrow(data_features) == 0) {
      return(list(
        shap_values = NULL,
        error = "No complete cases"
      ))
    }

    # Calculate SHAP values (simplified approximation)
    # For production, consider using the {fastshap} or {shapviz} packages

    # Calculate feature importance as proxy for SHAP
    if (model_type == "rf") {
      importance_scores <- randomForest::importance(model)
      feature_importance <- data.frame(
        feature = rownames(importance_scores),
        importance = importance_scores[, "MeanDecreaseGini"],
        stringsAsFactors = FALSE
      ) %>%
        dplyr::arrange(dplyr::desc(importance))

    } else if (model_type == "xgboost") {
      importance_matrix <- xgboost::xgb.importance(
        feature_names = feature_cols,
        model = model
      )
      feature_importance <- data.frame(
        feature = importance_matrix$Feature,
        importance = importance_matrix$Gain,
        stringsAsFactors = FALSE
      )
    }

    # Normalize importance scores
    feature_importance$normalized_importance <-
      feature_importance$importance / sum(feature_importance$importance)

    list(
      feature_importance = feature_importance,
      note = "Using feature importance as SHAP proxy. For true SHAP values, use {fastshap} package."
    )

  }, error = function(e) {
    warning(paste("Error in calculate_shap_values:", e$message))
    list(
      shap_values = NULL,
      error = e$message
    )
  })
}


#' Generate comprehensive model report
#'
#' @param model_result Result from build_predictive_model
#' @return List with formatted report components
#' @export
generate_model_report <- function(model_result) {

  tryCatch({
    if (is.null(model_result$model)) {
      return(list(
        summary = "Model building failed",
        error = model_result$error
      ))
    }

    # Model summary
    model_summary <- data.frame(
      item = c("Model Type", "Outcome Variable", "Number of Features",
              "Training Samples", "Test Samples"),
      value = c(
        toupper(model_result$model_type),
        model_result$outcome_col,
        length(model_result$feature_cols),
        nrow(model_result$train_predictions),
        nrow(model_result$test_predictions)
      ),
      stringsAsFactors = FALSE
    )

    # Performance comparison
    performance_comparison <- dplyr::left_join(
      model_result$train_performance %>% dplyr::rename(train_value = value),
      model_result$test_performance %>% dplyr::rename(test_value = value),
      by = "metric"
    ) %>%
      dplyr::mutate(
        difference = train_value - test_value,
        pct_difference = (difference / train_value) * 100
      )

    # Top 10 important features
    top_features <- model_result$variable_importance %>%
      dplyr::slice_head(n = 10)

    # Cross-validation summary
    cv_summary <- if (!is.null(model_result$cv_results)) {
      model_result$cv_results$summary
    } else {
      NULL
    }

    list(
      model_summary = model_summary,
      performance_comparison = performance_comparison,
      top_features = top_features,
      cv_summary = cv_summary,
      confusion_matrices = list(
        train = table(
          Actual = model_result$train_predictions$actual,
          Predicted = model_result$train_predictions$predicted_class
        ),
        test = table(
          Actual = model_result$test_predictions$actual,
          Predicted = model_result$test_predictions$predicted_class
        )
      )
    )

  }, error = function(e) {
    warning(paste("Error in generate_model_report:", e$message))
    list(
      summary = "Report generation failed",
      error = e$message
    )
  })
}
