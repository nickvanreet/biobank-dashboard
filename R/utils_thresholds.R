# ==============================================================================
# THRESHOLD CONFIGURATION LOADER
# ==============================================================================
# Utilities for loading and accessing test thresholds from config/thresholds.yaml
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml)
})

# Global cache for thresholds (avoids repeated file reads)
.thresholds_cache <- new.env(parent = emptyenv())

#' Load thresholds from YAML configuration file
#'
#' @param config_path Path to thresholds.yaml (default: config/thresholds.yaml)
#' @param force_reload If TRUE, reload even if cached
#' @return List with all threshold configurations
#' @export
load_thresholds <- function(config_path = NULL, force_reload = FALSE) {
  # Default path
 if (is.null(config_path)) {
    # Try multiple possible locations
    possible_paths <- c(
      "config/thresholds.yaml",
      "../config/thresholds.yaml",
      file.path(getwd(), "config/thresholds.yaml"),
      system.file("config", "thresholds.yaml", package = "biobankdashboard")
    )

    for (path in possible_paths) {
      if (file.exists(path)) {
        config_path <- path
        break
      }
    }
  }

  # Check cache
  cache_key <- "thresholds"
  if (!force_reload && exists(cache_key, envir = .thresholds_cache)) {
    return(get(cache_key, envir = .thresholds_cache))
  }

  # Load from file
  if (!is.null(config_path) && file.exists(config_path)) {
    tryCatch({
      thresholds <- yaml::read_yaml(config_path)
      assign(cache_key, thresholds, envir = .thresholds_cache)
      message("Loaded thresholds from: ", config_path)
      return(thresholds)
    }, error = function(e) {
      warning("Failed to load thresholds from ", config_path, ": ", e$message)
    })
  }

  # Return defaults if file not found
  warning("Thresholds config not found, using defaults")
  defaults <- get_default_thresholds()
  assign(cache_key, defaults, envir = .thresholds_cache)
  return(defaults)
}

#' Get default thresholds (fallback when config file not available)
#' @return List with default threshold values
get_default_thresholds <- function() {
  list(
    mic = list(
      targets = list(
        `177T` = list(positive = 35, borderline = 40),
        `18S2` = list(positive = 35, borderline = 40),
        RNAseP_DNA = list(positive = 32, fail = 45),
        RNAseP_RNA = list(positive = 30, fail = 45)
      ),
      rna_preservation = list(good = 5, moderate = 8),
      replicates = list(
        min_for_positive = 2,
        min_for_negative = 2,
        total_expected = 4
      )
    ),
    elisa = list(
      pp_percent = list(positive = 20, borderline_low = 15),
      dod = list(positive = 0.3, borderline_low = 0.2),
      qc = list(max_cv_percent = 20, max_cv_invalid = 30)
    ),
    ielisa = list(
      inhibition = list(positive = 30, borderline_low = 25),
      formula_disagreement = list(max_diff_percent = 25)
    ),
    resolution = list(
      default_strategy = "conservative",
      status_priority = list(
        Positive = 100,
        Borderline = 50,
        Negative = 10,
        Invalid = 0
      )
    )
  )
}

#' Get thresholds for a specific test type
#'
#' @param test_type One of: "mic", "elisa", "ielisa", "resolution", "classification"
#' @return List with thresholds for the specified test type
#' @export
get_test_thresholds <- function(test_type) {
  thresholds <- load_thresholds()

  if (!test_type %in% names(thresholds)) {
    warning("Unknown test type: ", test_type, ". Available: ", paste(names(thresholds), collapse = ", "))
    return(list())
  }

  thresholds[[test_type]]
}

#' Get MIC target threshold
#' @param target Target name (e.g., "177T", "18S2")
#' @param threshold_type Type of threshold ("positive", "borderline", "fail")
#' @return Numeric threshold value
#' @export
get_mic_threshold <- function(target, threshold_type = "positive") {
  thresholds <- get_test_thresholds("mic")

  if (!target %in% names(thresholds$targets)) {
    warning("Unknown MIC target: ", target)
    return(NA_real_)
  }

  thresholds$targets[[target]][[threshold_type]]
}

#' Get ELISA threshold
#' @param metric Either "pp_percent" or "dod"
#' @param threshold_type Either "positive" or "borderline_low"
#' @return Numeric threshold value
#' @export
get_elisa_threshold <- function(metric = "pp_percent", threshold_type = "positive") {
  thresholds <- get_test_thresholds("elisa")
  thresholds[[metric]][[threshold_type]]
}

#' Get iELISA threshold
#' @param threshold_type Either "positive" or "borderline_low"
#' @return Numeric threshold value
#' @export
get_ielisa_threshold <- function(threshold_type = "positive") {
  thresholds <- get_test_thresholds("ielisa")
  thresholds$inhibition[[threshold_type]]
}

#' Clear threshold cache (useful for testing or after config changes)
#' @export
clear_threshold_cache <- function() {
  rm(list = ls(envir = .thresholds_cache), envir = .thresholds_cache)
  message("Threshold cache cleared")
}

#' Format thresholds for display (e.g., in UI or reports)
#' @param test_type Test type to format
#' @return Character string with formatted thresholds
#' @export
format_thresholds_for_display <- function(test_type = "all") {
  thresholds <- load_thresholds()

  format_mic <- function() {
    mic <- thresholds$mic
    paste0(
      "MIC (qPCR) Thresholds:\n",
      "  177T: Positive < ", mic$targets$`177T`$positive,
      ", Borderline ", mic$targets$`177T`$positive, "-", mic$targets$`177T`$borderline, "\n",
      "  18S2: Positive < ", mic$targets$`18S2`$positive,
      ", Borderline ", mic$targets$`18S2`$positive, "-", mic$targets$`18S2`$borderline, "\n",
      "  Replicate voting: ", mic$replicates$min_for_positive, "+/", mic$replicates$total_expected, " for positive"
    )
  }

  format_elisa <- function() {
    elisa <- thresholds$elisa
    paste0(
      "ELISA Thresholds:\n",
      "  PP%: Positive >= ", elisa$pp_percent$positive,
      ", Borderline ", elisa$pp_percent$borderline_low, "-", elisa$pp_percent$positive, "\n",
      "  DOD: Positive >= ", elisa$dod$positive,
      ", Borderline ", elisa$dod$borderline_low, "-", elisa$dod$positive
    )
  }

  format_ielisa <- function() {
    ielisa <- thresholds$ielisa
    paste0(
      "iELISA Thresholds:\n",
      "  % Inhibition: Positive >= ", ielisa$inhibition$positive,
      ", Borderline ", ielisa$inhibition$borderline_low, "-", ielisa$inhibition$positive
    )
  }

  if (test_type == "all") {
    paste(format_mic(), format_elisa(), format_ielisa(), sep = "\n\n")
  } else if (test_type == "mic") {
    format_mic()
  } else if (test_type == "elisa") {
    format_elisa()
  } else if (test_type == "ielisa") {
    format_ielisa()
  } else {
    paste("Unknown test type:", test_type)
  }
}
