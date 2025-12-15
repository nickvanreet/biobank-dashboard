# R/core/app_logger.R
# Structured logging for Biobank Dashboard
# Replaces ad-hoc DEBUG statements with consistent, configurable logging
# ==============================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

# ==============================================================================
# LOGGING CONFIGURATION
# ==============================================================================

# Log levels (numeric for comparison)
.log_levels <- list(
  DEBUG = 10,
  INFO = 20,
  WARN = 30,
  ERROR = 40,
  FATAL = 50
)

# Logging environment for state
.logger_env <- new.env(parent = emptyenv())
.logger_env$level <- "INFO"
.logger_env$output <- "console"  # "console", "file", or "both"
.logger_env$file_path <- NULL
.logger_env$json_format <- FALSE
.logger_env$include_timestamp <- TRUE
.logger_env$include_session <- TRUE
.logger_env$session_id <- NULL

# ==============================================================================
# CONFIGURATION FUNCTIONS
# ==============================================================================

#' Configure the application logger
#'
#' @param level Log level threshold ("DEBUG", "INFO", "WARN", "ERROR", "FATAL")
#' @param output Where to write logs ("console", "file", "both")
#' @param file_path Path to log file (required if output includes "file")
#' @param json_format Use JSON format for structured logging
#' @param include_timestamp Include timestamp in log messages
#' @param include_session Include session ID in log messages
#' @export
configure_logger <- function(
  level = "INFO",
  output = "console",
  file_path = NULL,
  json_format = FALSE,
  include_timestamp = TRUE,
  include_session = TRUE
) {
  # Validate level
  if (!level %in% names(.log_levels)) {
    stop(sprintf("Invalid log level: %s. Must be one of: %s",
                 level, paste(names(.log_levels), collapse = ", ")))
  }

  # Validate output
  if (!output %in% c("console", "file", "both")) {
    stop("Output must be 'console', 'file', or 'both'")
  }

  # Require file path for file output

  if (output %in% c("file", "both") && is.null(file_path)) {
    stop("file_path required when output includes 'file'")
  }

  # Store configuration

  .logger_env$level <- level
  .logger_env$output <- output
  .logger_env$file_path <- file_path
  .logger_env$json_format <- json_format
  .logger_env$include_timestamp <- include_timestamp
  .logger_env$include_session <- include_session

  # Generate session ID if not set
  if (is.null(.logger_env$session_id)) {
    .logger_env$session_id <- substr(digest::digest(Sys.time()), 1, 8)
  }

  # Create log directory if needed
  if (!is.null(file_path)) {
    dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)
  }

  invisible(TRUE)
}

#' Set the current session ID for logging
#'
#' @param session_id Session identifier (typically from Shiny session)
#' @export
set_log_session <- function(session_id) {
  .logger_env$session_id <- as.character(session_id)
}

#' Get current logger configuration
#'
#' @return List with current configuration
#' @export
get_logger_config <- function() {
  list(
    level = .logger_env$level,
    output = .logger_env$output,
    file_path = .logger_env$file_path,
    json_format = .logger_env$json_format,
    session_id = .logger_env$session_id
  )
}

# ==============================================================================
# CORE LOGGING FUNCTION
# ==============================================================================

#' Write a log message
#'
#' @param level Log level
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context as named arguments
#' @keywords internal
write_log <- function(level, message, module = NULL, ...) {
  # Check if this level should be logged
  if (.log_levels[[level]] < .log_levels[[.logger_env$level]]) {
    return(invisible(FALSE))
  }

  # Build log entry
  entry <- list()

  if (.logger_env$include_timestamp) {
    entry$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  }

  entry$level <- level

  if (.logger_env$include_session && !is.null(.logger_env$session_id)) {
    entry$session <- .logger_env$session_id
  }

  if (!is.null(module)) {
    entry$module <- module
  }

  entry$message <- message

  # Add extra context
  extra <- list(...)
  if (length(extra) > 0) {
    entry$context <- extra
  }

  # Format the entry
  if (.logger_env$json_format) {
    formatted <- jsonlite::toJSON(entry, auto_unbox = TRUE)
  } else {
    # Human-readable format
    parts <- c()

    if (!is.null(entry$timestamp)) {
      parts <- c(parts, sprintf("[%s]", entry$timestamp))
    }

    parts <- c(parts, sprintf("%-5s", entry$level))

    if (!is.null(entry$session)) {
      parts <- c(parts, sprintf("[%s]", entry$session))
    }

    if (!is.null(entry$module)) {
      parts <- c(parts, sprintf("[%s]", entry$module))
    }

    parts <- c(parts, entry$message)

    if (length(extra) > 0) {
      context_str <- paste(
        names(extra),
        sapply(extra, function(x) {
          if (is.character(x) || is.numeric(x)) as.character(x) else "..."
        }),
        sep = "=",
        collapse = ", "
      )
      parts <- c(parts, sprintf("(%s)", context_str))
    }

    formatted <- paste(parts, collapse = " ")
  }

  # Write to output(s)
  if (.logger_env$output %in% c("console", "both")) {
    # Color-code by level for console
    if (level == "ERROR" || level == "FATAL") {
      message(formatted)  # Will be red in most terminals
    } else if (level == "WARN") {
      message(formatted)
    } else {
      cat(formatted, "\n")
    }
  }

  if (.logger_env$output %in% c("file", "both") && !is.null(.logger_env$file_path)) {
    cat(formatted, "\n", file = .logger_env$file_path, append = TRUE)
  }

  invisible(TRUE)
}

# ==============================================================================
# PUBLIC LOGGING FUNCTIONS
# ==============================================================================

#' Log a DEBUG message
#'
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context
#' @export
log_debug <- function(message, module = NULL, ...) {
  write_log("DEBUG", message, module, ...)
}

#' Log an INFO message
#'
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context
#' @export
log_info <- function(message, module = NULL, ...) {
  write_log("INFO", message, module, ...)
}

#' Log a WARN message
#'
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context
#' @export
log_warn <- function(message, module = NULL, ...) {
  write_log("WARN", message, module, ...)
}

#' Log an ERROR message
#'
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context
#' @export
log_error <- function(message, module = NULL, ...) {
  write_log("ERROR", message, module, ...)
}

#' Log a FATAL message
#'
#' @param message Log message
#' @param module Module/component name
#' @param ... Additional context
#' @export
log_fatal <- function(message, module = NULL, ...) {
  write_log("FATAL", message, module, ...)
}

# ==============================================================================
# SPECIALIZED LOGGING FUNCTIONS
# ==============================================================================

#' Log data loading event
#'
#' @param data_type Type of data being loaded
#' @param source Source file or directory
#' @param n_records Number of records loaded
#' @param duration_secs Time taken in seconds
#' @export
log_data_load <- function(data_type, source, n_records = NULL, duration_secs = NULL) {
  log_info(
    sprintf("Loaded %s data", data_type),
    module = "data_loader",
    data_type = data_type,
    source = basename(source),
    n_records = n_records,
    duration_secs = round(duration_secs, 2)
  )
}

#' Log pipeline step completion
#'
#' @param pipeline Pipeline name (MIC, ELISA, iELISA)
#' @param step Step name (ingest, qc, interpret, output)
#' @param n_samples Number of samples processed
#' @param n_valid Number of valid samples
#' @export
log_pipeline_step <- function(pipeline, step, n_samples, n_valid = NULL) {
  log_info(
    sprintf("%s pipeline: %s complete", pipeline, step),
    module = pipeline,
    step = step,
    n_samples = n_samples,
    n_valid = n_valid
  )
}

#' Log QC validation result
#'
#' @param run_id Run identifier
#' @param test_type Test type
#' @param passed Whether QC passed
#' @param issues List of QC issues (if any)
#' @export
log_qc_result <- function(run_id, test_type, passed, issues = NULL) {
  if (passed) {
    log_info(
      sprintf("QC passed for run %s", run_id),
      module = "qc",
      run_id = run_id,
      test_type = test_type
    )
  } else {
    log_warn(
      sprintf("QC failed for run %s: %s", run_id, paste(issues, collapse = "; ")),
      module = "qc",
      run_id = run_id,
      test_type = test_type,
      issues = paste(issues, collapse = "; ")
    )
  }
}

#' Log user action
#'
#' @param action Action name
#' @param user_id User identifier (if available)
#' @param details Additional details
#' @export
log_user_action <- function(action, user_id = NULL, details = NULL) {
  log_info(
    sprintf("User action: %s", action),
    module = "user",
    action = action,
    user_id = user_id,
    details = details
  )
}

#' Log cache operation
#'
#' @param operation Operation type (hit, miss, clear, write)
#' @param cache_type Cache type (biobank, elisa, ielisa, mic)
#' @param key Cache key (optional)
#' @export
log_cache_op <- function(operation, cache_type, key = NULL) {
  log_debug(
    sprintf("Cache %s: %s", operation, cache_type),
    module = "cache",
    operation = operation,
    cache_type = cache_type,
    key = key
  )
}

# ==============================================================================
# ERROR HANDLING HELPERS
# ==============================================================================

#' Log and re-raise an error with context
#'
#' @param expr Expression to evaluate
#' @param context Description of what was being attempted
#' @param module Module name
#' @export
log_try <- function(expr, context, module = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      log_error(
        sprintf("%s: %s", context, conditionMessage(e)),
        module = module,
        error_class = class(e)[1]
      )
      stop(e)
    },
    warning = function(w) {
      log_warn(
        sprintf("%s: %s", context, conditionMessage(w)),
        module = module
      )
      invokeRestart("muffleWarning")
    }
  )
}

#' Wrap a function with automatic error logging
#'
#' @param fn Function to wrap
#' @param module Module name for logging
#' @return Wrapped function
#' @export
with_logging <- function(fn, module) {
  function(...) {
    tryCatch(
      fn(...),
      error = function(e) {
        log_error(
          sprintf("Function error: %s", conditionMessage(e)),
          module = module,
          fn_name = deparse(substitute(fn))
        )
        stop(e)
      }
    )
  }
}

# ==============================================================================
# INITIALIZATION
# ==============================================================================

# Initialize with defaults from config if available
.init_logger <- function() {
  # Check for config
  debug_mode <- FALSE

  if (exists("config") && !is.null(config$app$debug_mode)) {
    debug_mode <- config$app$debug_mode
  }

  # Set default level based on debug mode
  level <- if (debug_mode) "DEBUG" else "INFO"

  configure_logger(level = level)

  log_debug("Logger initialized", module = "logger", level = level)
}

# Auto-initialize when sourced
.init_logger()
