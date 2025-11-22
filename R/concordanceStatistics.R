# Concordance Statistics Module
# Statistical analysis functions for test concordance
# ============================================================================

#' Calculate comprehensive concordance metrics
#'
#' @param test1 Numeric or logical vector for test 1 results
#' @param test2 Numeric or logical vector for test 2 results
#' @param test1_name Character name for test 1 (default: "Test1")
#' @param test2_name Character name for test 2 (default: "Test2")
#' @param cutoff1 Numeric cutoff for test 1 positivity (if test1 is numeric)
#' @param cutoff2 Numeric cutoff for test 2 positivity (if test2 is numeric)
#' @param bootstrap Boolean whether to calculate bootstrap CIs (default: TRUE)
#' @param n_bootstrap Integer number of bootstrap iterations (default: 1000)
#' @return List with concordance metrics and confusion matrix
#' @export
calculate_concordance_metrics <- function(test1, test2,
                                         test1_name = "Test1",
                                         test2_name = "Test2",
                                         cutoff1 = NULL,
                                         cutoff2 = NULL,
                                         bootstrap = TRUE,
                                         n_bootstrap = 1000) {

  tryCatch({
    # Convert to logical if numeric
    if (is.numeric(test1) && !is.null(cutoff1)) {
      test1 <- test1 >= cutoff1
    }
    if (is.numeric(test2) && !is.null(cutoff2)) {
      test2 <- test2 >= cutoff2
    }

    # Remove missing values
    valid_idx <- !is.na(test1) & !is.na(test2)
    test1 <- test1[valid_idx]
    test2 <- test2[valid_idx]

    n <- length(test1)

    if (n == 0) {
      # Create empty confusion matrix with proper row/column names
      cm_empty <- matrix(0, nrow = 2, ncol = 2)
      rownames(cm_empty) <- c("Positive", "Negative")
      colnames(cm_empty) <- c("Positive", "Negative")

      return(list(
        n = 0,
        test1_name = test1_name,
        test2_name = test2_name,
        metrics = data.frame(),
        confusion_matrix = cm_empty
      ))
    }

    # Create confusion matrix
    # Rows: Test1, Cols: Test2
    cm <- table(
      factor(test1, levels = c(TRUE, FALSE), labels = c("Positive", "Negative")),
      factor(test2, levels = c(TRUE, FALSE), labels = c("Positive", "Negative"))
    )

    # Extract counts
    a <- cm[1, 1]  # Both positive
    b <- cm[1, 2]  # Test1+, Test2-
    c <- cm[2, 1]  # Test1-, Test2+
    d <- cm[2, 2]  # Both negative

    # Overall agreement
    percent_agreement <- ((a + d) / n) * 100

    # Cohen's Kappa
    observed_agreement <- (a + d) / n
    expected_agreement <- ((a + b) * (a + c) + (c + d) * (b + d)) / (n^2)

    kappa <- if (expected_agreement < 1) {
      (observed_agreement - expected_agreement) / (1 - expected_agreement)
    } else {
      NA_real_
    }

    # Calculate using irr package for consistency
    kappa_irr <- tryCatch({
      ratings <- data.frame(test1 = as.numeric(test1), test2 = as.numeric(test2))
      result <- irr::kappa2(ratings, weight = "unweighted")
      list(
        kappa = result$value,
        z = result$statistic,
        p_value = result$p.value
      )
    }, error = function(e) {
      list(kappa = kappa, z = NA_real_, p_value = NA_real_)
    })

    # Sensitivity (Test2 positive | Test1 positive)
    sensitivity <- if ((a + b) > 0) a / (a + b) else NA_real_

    # Specificity (Test2 negative | Test1 negative)
    specificity <- if ((c + d) > 0) d / (c + d) else NA_real_

    # Positive Predictive Value (Test1 positive | Test2 positive)
    ppv <- if ((a + c) > 0) a / (a + c) else NA_real_

    # Negative Predictive Value (Test1 negative | Test2 negative)
    npv <- if ((b + d) > 0) d / (b + d) else NA_real_

    # McNemar's test for discordance
    mcnemar_result <- tryCatch({
      if (b + c > 0) {
        test <- mcnemar.test(cm, correct = TRUE)
        list(
          statistic = test$statistic,
          p_value = test$p.value
        )
      } else {
        list(statistic = 0, p_value = 1)
      }
    }, error = function(e) {
      list(statistic = NA_real_, p_value = NA_real_)
    })

    # Youden's J statistic
    youden_j <- sensitivity + specificity - 1

    # Matthews Correlation Coefficient
    mcc_numerator <- (a * d) - (b * c)
    mcc_denominator <- sqrt((a + b) * (a + c) * (d + b) * (d + c))
    mcc <- if (mcc_denominator > 0) {
      mcc_numerator / mcc_denominator
    } else {
      NA_real_
    }

    # Compile metrics
    metrics <- data.frame(
      metric = c("N", "Percent Agreement", "Cohen's Kappa", "Kappa Z-score",
                 "Kappa P-value", "Sensitivity", "Specificity", "PPV", "NPV",
                 "Youden's J", "MCC", "McNemar Chi-sq", "McNemar P-value"),
      value = c(n, percent_agreement, kappa_irr$kappa, kappa_irr$z,
                kappa_irr$p_value, sensitivity, specificity, ppv, npv,
                youden_j, mcc, mcnemar_result$statistic, mcnemar_result$p_value),
      stringsAsFactors = FALSE
    )

    # Bootstrap confidence intervals
    if (bootstrap && n >= 30) {
      boot_ci <- tryCatch({
        bootstrap_confidence_intervals(test1, test2, n_bootstrap)
      }, error = function(e) {
        NULL
      })

      if (!is.null(boot_ci)) {
        metrics$ci_lower <- boot_ci$ci_lower[match(metrics$metric, boot_ci$metric)]
        metrics$ci_upper <- boot_ci$ci_upper[match(metrics$metric, boot_ci$metric)]
      }
    }

    # Return results
    list(
      n = n,
      test1_name = test1_name,
      test2_name = test2_name,
      metrics = metrics,
      confusion_matrix = cm,
      counts = list(
        both_positive = a,
        test1_only = b,
        test2_only = c,
        both_negative = d
      )
    )

  }, error = function(e) {
    warning(paste("Error in calculate_concordance_metrics:", e$message))

    # Create empty confusion matrix with proper row/column names
    cm_empty <- matrix(0, nrow = 2, ncol = 2)
    rownames(cm_empty) <- c("Positive", "Negative")
    colnames(cm_empty) <- c("Positive", "Negative")

    list(
      n = 0,
      test1_name = test1_name,
      test2_name = test2_name,
      metrics = data.frame(metric = character(), value = numeric()),
      confusion_matrix = cm_empty,
      error = e$message
    )
  })
}


#' Calculate Fleiss' Kappa for multiple raters/tests
#'
#' @param data Data frame where each row is a subject and each column is a rater/test
#' @param convert_to_binary Boolean whether to convert numeric to binary (default: FALSE)
#' @param cutoffs Named numeric vector of cutoffs for each test (if convert_to_binary = TRUE)
#' @return List with Fleiss' Kappa and related statistics
#' @export
calculate_fleiss_kappa <- function(data, convert_to_binary = FALSE, cutoffs = NULL) {

  tryCatch({
    # Convert to binary if needed
    if (convert_to_binary && !is.null(cutoffs)) {
      for (col in names(data)) {
        if (col %in% names(cutoffs)) {
          data[[col]] <- data[[col]] >= cutoffs[col]
        }
      }
    }

    # Convert to numeric (1/0)
    data_numeric <- as.data.frame(lapply(data, function(x) as.numeric(as.logical(x))))

    # Remove rows with all NA
    complete_rows <- complete.cases(data_numeric)
    data_numeric <- data_numeric[complete_rows, ]

    if (nrow(data_numeric) < 2) {
      return(list(
        kappa = NA_real_,
        subjects = 0,
        raters = 0,
        p_value = NA_real_,
        error = "Insufficient data"
      ))
    }

    # Calculate Fleiss' Kappa using irr package
    result <- tryCatch({
      irr::kappam.fleiss(data_numeric)
    }, error = function(e) {
      NULL
    })

    if (is.null(result)) {
      return(list(
        kappa = NA_real_,
        subjects = nrow(data_numeric),
        raters = ncol(data_numeric),
        p_value = NA_real_,
        error = "Calculation failed"
      ))
    }

    # Interpretation
    interpretation <- dplyr::case_when(
      result$value < 0 ~ "Poor",
      result$value < 0.20 ~ "Slight",
      result$value < 0.40 ~ "Fair",
      result$value < 0.60 ~ "Moderate",
      result$value < 0.80 ~ "Substantial",
      TRUE ~ "Almost Perfect"
    )

    list(
      kappa = result$value,
      subjects = result$subjects,
      raters = result$raters,
      z_score = result$statistic,
      p_value = result$p.value,
      interpretation = interpretation
    )

  }, error = function(e) {
    warning(paste("Error in calculate_fleiss_kappa:", e$message))
    list(
      kappa = NA_real_,
      subjects = 0,
      raters = 0,
      p_value = NA_real_,
      error = e$message
    )
  })
}


#' Bootstrap confidence intervals for concordance metrics
#'
#' @param test1 Logical vector for test 1
#' @param test2 Logical vector for test 2
#' @param n_bootstrap Integer number of bootstrap iterations
#' @param alpha Numeric confidence level (default: 0.05 for 95% CI)
#' @return Data frame with metrics and confidence intervals
#' @export
bootstrap_confidence_intervals <- function(test1, test2, n_bootstrap = 1000, alpha = 0.05) {

  tryCatch({
    n <- length(test1)

    # Storage for bootstrap results
    boot_results <- matrix(NA, nrow = n_bootstrap, ncol = 8)
    colnames(boot_results) <- c("agreement", "kappa", "sensitivity", "specificity",
                                 "ppv", "npv", "youden_j", "mcc")

    # Bootstrap resampling
    for (i in 1:n_bootstrap) {
      # Resample with replacement
      idx <- sample(1:n, n, replace = TRUE)
      t1_boot <- test1[idx]
      t2_boot <- test2[idx]

      # Calculate metrics
      cm <- table(t1_boot, t2_boot)

      # Ensure 2x2 matrix
      if (nrow(cm) == 2 && ncol(cm) == 2) {
        a <- cm[2, 2]  # Both positive (TRUE, TRUE)
        b <- cm[2, 1]  # Test1+, Test2-
        c <- cm[1, 2]  # Test1-, Test2+
        d <- cm[1, 1]  # Both negative

        # Agreement
        boot_results[i, "agreement"] <- ((a + d) / n) * 100

        # Kappa
        observed <- (a + d) / n
        expected <- ((a + b) * (a + c) + (c + d) * (b + d)) / (n^2)
        boot_results[i, "kappa"] <- if (expected < 1) {
          (observed - expected) / (1 - expected)
        } else {
          NA_real_
        }

        # Sensitivity, Specificity, PPV, NPV
        boot_results[i, "sensitivity"] <- if ((a + b) > 0) a / (a + b) else NA_real_
        boot_results[i, "specificity"] <- if ((c + d) > 0) d / (c + d) else NA_real_
        boot_results[i, "ppv"] <- if ((a + c) > 0) a / (a + c) else NA_real_
        boot_results[i, "npv"] <- if ((b + d) > 0) d / (b + d) else NA_real_

        # Youden's J
        sens <- boot_results[i, "sensitivity"]
        spec <- boot_results[i, "specificity"]
        boot_results[i, "youden_j"] <- if (!is.na(sens) && !is.na(spec)) {
          sens + spec - 1
        } else {
          NA_real_
        }

        # MCC
        mcc_num <- (a * d) - (b * c)
        mcc_den <- sqrt((a + b) * (a + c) * (d + b) * (d + c))
        boot_results[i, "mcc"] <- if (mcc_den > 0) mcc_num / mcc_den else NA_real_
      }
    }

    # Calculate percentile confidence intervals
    ci_lower <- apply(boot_results, 2, function(x) {
      quantile(x, probs = alpha/2, na.rm = TRUE)
    })

    ci_upper <- apply(boot_results, 2, function(x) {
      quantile(x, probs = 1 - alpha/2, na.rm = TRUE)
    })

    # Compile results
    data.frame(
      metric = c("Percent Agreement", "Cohen's Kappa", "Sensitivity", "Specificity",
                 "PPV", "NPV", "Youden's J", "MCC"),
      ci_lower = as.numeric(ci_lower),
      ci_upper = as.numeric(ci_upper),
      stringsAsFactors = FALSE
    )

  }, error = function(e) {
    warning(paste("Error in bootstrap_confidence_intervals:", e$message))
    data.frame(
      metric = character(),
      ci_lower = numeric(),
      ci_upper = numeric()
    )
  })
}


#' Calculate ROC metrics for quantitative concordance
#'
#' @param test_values Numeric vector of test values
#' @param reference Logical or numeric vector of reference standard
#' @param test_name Character name for the test
#' @return List with ROC curve data, AUC, and optimal cutpoint
#' @export
calculate_roc_metrics <- function(test_values, reference, test_name = "Test") {

  tryCatch({
    # Remove missing values
    valid_idx <- !is.na(test_values) & !is.na(reference)
    test_values <- test_values[valid_idx]
    reference <- as.logical(reference[valid_idx])

    if (length(test_values) == 0) {
      return(list(
        auc = NA_real_,
        roc_data = data.frame(),
        optimal_cutpoint = NA_real_,
        error = "No valid data"
      ))
    }

    # Calculate ROC using pROC package
    roc_obj <- tryCatch({
      pROC::roc(reference, test_values, quiet = TRUE)
    }, error = function(e) {
      NULL
    })

    if (is.null(roc_obj)) {
      return(list(
        auc = NA_real_,
        roc_data = data.frame(),
        optimal_cutpoint = NA_real_,
        error = "ROC calculation failed"
      ))
    }

    # Extract ROC curve data
    roc_data <- data.frame(
      threshold = roc_obj$thresholds,
      sensitivity = roc_obj$sensitivities,
      specificity = roc_obj$specificities,
      fpr = 1 - roc_obj$specificities
    )

    # Calculate Youden's J to find optimal cutpoint
    roc_data$youden_j <- roc_data$sensitivity + roc_data$specificity - 1
    optimal_idx <- which.max(roc_data$youden_j)
    optimal_cutpoint <- roc_data$threshold[optimal_idx]

    # AUC with confidence interval
    auc_ci <- tryCatch({
      ci <- pROC::ci.auc(roc_obj)
      c(lower = ci[1], auc = ci[2], upper = ci[3])
    }, error = function(e) {
      c(lower = NA_real_, auc = roc_obj$auc, upper = NA_real_)
    })

    list(
      auc = as.numeric(roc_obj$auc),
      auc_ci_lower = auc_ci["lower"],
      auc_ci_upper = auc_ci["upper"],
      roc_data = roc_data,
      optimal_cutpoint = optimal_cutpoint,
      optimal_sensitivity = roc_data$sensitivity[optimal_idx],
      optimal_specificity = roc_data$specificity[optimal_idx],
      test_name = test_name
    )

  }, error = function(e) {
    warning(paste("Error in calculate_roc_metrics:", e$message))
    list(
      auc = NA_real_,
      roc_data = data.frame(),
      optimal_cutpoint = NA_real_,
      error = e$message
    )
  })
}


#' Bland-Altman analysis for quantitative agreement
#'
#' @param method1 Numeric vector for method 1
#' @param method2 Numeric vector for method 2
#' @param method1_name Character name for method 1
#' @param method2_name Character name for method 2
#' @return List with Bland-Altman statistics and plot data
#' @export
bland_altman_analysis <- function(method1, method2,
                                  method1_name = "Method1",
                                  method2_name = "Method2") {

  tryCatch({
    # Remove missing values
    valid_idx <- !is.na(method1) & !is.na(method2)
    method1 <- method1[valid_idx]
    method2 <- method2[valid_idx]

    n <- length(method1)

    if (n == 0) {
      return(list(
        n = 0,
        statistics = data.frame(),
        plot_data = data.frame(),
        error = "No valid data"
      ))
    }

    # Calculate mean and difference
    mean_values <- (method1 + method2) / 2
    diff_values <- method1 - method2

    # Statistics
    mean_diff <- mean(diff_values, na.rm = TRUE)
    sd_diff <- sd(diff_values, na.rm = TRUE)

    # Limits of agreement (95%)
    upper_loa <- mean_diff + 1.96 * sd_diff
    lower_loa <- mean_diff - 1.96 * sd_diff

    # Standard error for LOA
    se_mean_diff <- sd_diff / sqrt(n)
    se_loa <- sd_diff * sqrt(3 / n)

    # 95% CI for mean difference
    ci_mean_diff_lower <- mean_diff - 1.96 * se_mean_diff
    ci_mean_diff_upper <- mean_diff + 1.96 * se_mean_diff

    # 95% CI for limits of agreement
    ci_upper_loa_lower <- upper_loa - 1.96 * se_loa
    ci_upper_loa_upper <- upper_loa + 1.96 * se_loa
    ci_lower_loa_lower <- lower_loa - 1.96 * se_loa
    ci_lower_loa_upper <- lower_loa + 1.96 * se_loa

    # Correlation between mean and difference (check for proportional bias)
    correlation <- cor(mean_values, diff_values, use = "complete.obs")

    # Test for proportional bias
    proportional_bias_test <- tryCatch({
      lm_result <- lm(diff_values ~ mean_values)
      summary_lm <- summary(lm_result)
      list(
        slope = coef(lm_result)[2],
        p_value = summary_lm$coefficients[2, 4]
      )
    }, error = function(e) {
      list(slope = NA_real_, p_value = NA_real_)
    })

    # Statistics data frame
    statistics <- data.frame(
      statistic = c("N", "Mean Difference", "SD of Difference",
                   "Upper LOA", "Lower LOA",
                   "Correlation (Mean vs Diff)", "Proportional Bias Slope",
                   "Proportional Bias P-value"),
      value = c(n, mean_diff, sd_diff, upper_loa, lower_loa,
               correlation, proportional_bias_test$slope,
               proportional_bias_test$p_value),
      stringsAsFactors = FALSE
    )

    # Plot data
    plot_data <- data.frame(
      mean = mean_values,
      difference = diff_values
    )

    # Return results
    list(
      n = n,
      method1_name = method1_name,
      method2_name = method2_name,
      statistics = statistics,
      plot_data = plot_data,
      mean_diff = mean_diff,
      sd_diff = sd_diff,
      upper_loa = upper_loa,
      lower_loa = lower_loa,
      ci_mean_diff = c(lower = ci_mean_diff_lower, upper = ci_mean_diff_upper),
      ci_upper_loa = c(lower = ci_upper_loa_lower, upper = ci_upper_loa_upper),
      ci_lower_loa = c(lower = ci_lower_loa_lower, upper = ci_lower_loa_upper),
      proportional_bias = proportional_bias_test$p_value < 0.05
    )

  }, error = function(e) {
    warning(paste("Error in bland_altman_analysis:", e$message))
    list(
      n = 0,
      statistics = data.frame(),
      plot_data = data.frame(),
      error = e$message
    )
  })
}
