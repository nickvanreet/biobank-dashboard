# R/core/cache_manager.R
# Unified cache management for all data types
# Provides centralized cache clearing, status reporting, and preloading
# ============================================================================

suppressPackageStartupMessages({
  library(digest)
})

# ============================================================================
# CACHE CONFIGURATION
# ============================================================================

# Cache version - increment when data structure changes require invalidation
.cache_version <- "v2.0"

# In-memory cache environment for quick lookups
.app_cache <- new.env(parent = emptyenv())

# ============================================================================
# CACHE STATUS FUNCTIONS
# ============================================================================

#' Get cache status for all data types
#' @param site_paths Site-specific paths from config
#' @return List with cache status for each data type
#' @export
get_cache_status <- function(site_paths = NULL) {
  if (is.null(site_paths) && exists("config") && !is.null(config$site_paths)) {
    site_paths <- config$site_paths
  }

  status <- list()

  # Biobank cache
  if (!is.null(site_paths$cache_dir)) {
    biobank_cache_files <- list.files(
      site_paths$cache_dir,
      pattern = "^[^_].*\\.rds$",  # RDS files not starting with underscore
      full.names = TRUE
    )
    # Filter out ELISA/iELISA/MIC cache files
    biobank_cache_files <- biobank_cache_files[!grepl("(elisa|ielisa|mic|Run)", biobank_cache_files, ignore.case = TRUE)]

    status$biobank <- list(
      type = "Biobank",
      n_files = length(biobank_cache_files),
      size_mb = round(sum(file.info(biobank_cache_files)$size, na.rm = TRUE) / 1024 / 1024, 2),
      last_modified = if (length(biobank_cache_files)) max(file.info(biobank_cache_files)$mtime, na.rm = TRUE) else NA
    )
  }

  # ELISA cache
  elisa_cache_files <- list.files(
    site_paths$cache_dir,
    pattern = "\\.rds$",
    full.names = TRUE
  )
  elisa_cache_files <- elisa_cache_files[grepl("^(?!ielisa_)", basename(elisa_cache_files), perl = TRUE)]
  elisa_cache_files <- elisa_cache_files[!grepl("(biobank|Run)", elisa_cache_files, ignore.case = TRUE)]

  status$elisa <- list(
    type = "ELISA",
    n_files = length(elisa_cache_files),
    size_mb = round(sum(file.info(elisa_cache_files)$size, na.rm = TRUE) / 1024 / 1024, 2),
    last_modified = if (length(elisa_cache_files)) max(file.info(elisa_cache_files)$mtime, na.rm = TRUE) else NA
  )

  # iELISA cache
  ielisa_cache_files <- list.files(
    site_paths$cache_dir,
    pattern = "^ielisa_.*\\.rds$",
    full.names = TRUE
  )

  status$ielisa <- list(
    type = "iELISA",
    n_files = length(ielisa_cache_files),
    size_mb = round(sum(file.info(ielisa_cache_files)$size, na.rm = TRUE) / 1024 / 1024, 2),
    last_modified = if (length(ielisa_cache_files)) max(file.info(ielisa_cache_files)$mtime, na.rm = TRUE) else NA
  )

  # MIC cache (Run files)
  mic_cache_files <- list.files(
    site_paths$cache_dir,
    pattern = "Run.*\\.rds$",
    full.names = TRUE
  )

  status$mic <- list(
    type = "MIC qPCR",
    n_files = length(mic_cache_files),
    size_mb = round(sum(file.info(mic_cache_files)$size, na.rm = TRUE) / 1024 / 1024, 2),
    last_modified = if (length(mic_cache_files)) max(file.info(mic_cache_files)$mtime, na.rm = TRUE) else NA
  )

  # Calculate totals
  total_files <- sum(sapply(status, function(x) x$n_files))
  total_size <- sum(sapply(status, function(x) x$size_mb))

  status$total <- list(
    type = "Total",
    n_files = total_files,
    size_mb = round(total_size, 2)
  )

  status
}

#' Format cache status for display
#' @param status Cache status list from get_cache_status()
#' @return Character string for display
#' @export
format_cache_status <- function(status) {
  lines <- character()

  for (name in names(status)) {
    if (name == "total") next
    s <- status[[name]]
    if (s$n_files > 0) {
      lines <- c(lines, sprintf("%s: %d files (%.1f MB)", s$type, s$n_files, s$size_mb))
    }
  }

  if (length(lines) == 0) {
    return("No cached data")
  }

  c(lines, sprintf("Total: %.1f MB", status$total$size_mb))
}

# ============================================================================
# CACHE CLEARING FUNCTIONS
# ============================================================================

#' Clear all caches
#' @param site_paths Site-specific paths
#' @param clear_rds Clear RDS files from disk (default TRUE)
#' @param clear_memory Clear in-memory caches (default TRUE)
#' @return List with clearing results
#' @export
clear_all_caches <- function(site_paths = NULL, clear_rds = TRUE, clear_memory = TRUE) {
  if (is.null(site_paths) && exists("config") && !is.null(config$site_paths)) {
    site_paths <- config$site_paths
  }

  results <- list(
    biobank = 0,
    elisa = 0,
    ielisa = 0,
    mic = 0,
    memory = FALSE
  )

  # Clear RDS files

if (clear_rds && !is.null(site_paths$cache_dir) && dir.exists(site_paths$cache_dir)) {
    cache_files <- list.files(site_paths$cache_dir, pattern = "\\.rds$", full.names = TRUE)

    if (length(cache_files) > 0) {
      # Categorize files
      biobank_files <- cache_files[!grepl("(elisa|ielisa|mic|Run)", cache_files, ignore.case = TRUE)]
      elisa_files <- cache_files[grepl("^(?!ielisa_)", basename(cache_files), perl = TRUE) &
                                  !grepl("(biobank|Run|ielisa)", cache_files, ignore.case = TRUE)]
      ielisa_files <- cache_files[grepl("^ielisa_", basename(cache_files))]
      mic_files <- cache_files[grepl("Run", cache_files, ignore.case = TRUE)]

      # Remove files
      if (length(biobank_files)) {
        file.remove(biobank_files)
        results$biobank <- length(biobank_files)
      }
      if (length(elisa_files)) {
        file.remove(elisa_files)
        results$elisa <- length(elisa_files)
      }
      if (length(ielisa_files)) {
        file.remove(ielisa_files)
        results$ielisa <- length(ielisa_files)
      }
      if (length(mic_files)) {
        file.remove(mic_files)
        results$mic <- length(mic_files)
      }
    }
  }

  # Clear in-memory caches
  if (clear_memory) {
    # Clear ELISA session cache
    if (exists(".elisa_cache_env")) {
      .elisa_cache_env$data <- NULL
      .elisa_cache_env$hash <- NULL
      .elisa_cache_env$version <- NULL
    }

    # Clear app-level cache
    rm(list = ls(.app_cache), envir = .app_cache)

    results$memory <- TRUE
  }

  message(sprintf(
    "Cache cleared: Biobank=%d, ELISA=%d, iELISA=%d, MIC=%d files; Memory=%s",
    results$biobank, results$elisa, results$ielisa, results$mic,
    if (results$memory) "yes" else "no"
  ))

  invisible(results)
}

#' Clear specific cache type
#' @param cache_type One of "biobank", "elisa", "ielisa", "mic", "all"
#' @param site_paths Site-specific paths
#' @export
clear_cache <- function(cache_type = "all", site_paths = NULL) {
  if (is.null(site_paths) && exists("config") && !is.null(config$site_paths)) {
    site_paths <- config$site_paths
  }

  if (cache_type == "all") {
    return(clear_all_caches(site_paths))
  }

  if (is.null(site_paths$cache_dir) || !dir.exists(site_paths$cache_dir)) {
    message("Cache directory not found")
    return(invisible(0))
  }

  pattern <- switch(cache_type,
    biobank = "^[^_].*\\.rds$",
    elisa = "\\.rds$",
    ielisa = "^ielisa_.*\\.rds$",
    mic = "Run.*\\.rds$",
    NULL
  )

  if (is.null(pattern)) {
    message("Unknown cache type: ", cache_type)
    return(invisible(0))
  }

  cache_files <- list.files(site_paths$cache_dir, pattern = pattern, full.names = TRUE)

  # Additional filtering for specific types
  if (cache_type == "biobank") {
    cache_files <- cache_files[!grepl("(elisa|ielisa|mic|Run)", cache_files, ignore.case = TRUE)]
  } else if (cache_type == "elisa") {
    cache_files <- cache_files[!grepl("(biobank|Run|ielisa)", cache_files, ignore.case = TRUE)]
  }

  if (length(cache_files) > 0) {
    file.remove(cache_files)
    message(sprintf("Cleared %d %s cache file(s)", length(cache_files), cache_type))
  } else {
    message(sprintf("No %s cache files to clear", cache_type))
  }

  invisible(length(cache_files))
}

# ============================================================================
# LAZY LOADING HELPERS
# ============================================================================

#' Check if data should be loaded (for lazy loading)
#' @param data_key Key identifying the data type
#' @return TRUE if data needs loading
#' @export
needs_loading <- function(data_key) {
  !exists(data_key, envir = .app_cache) || is.null(.app_cache[[data_key]])
}

#' Mark data as loaded
#' @param data_key Key identifying the data type
#' @param timestamp Loading timestamp (default: current time)
#' @export
mark_loaded <- function(data_key, timestamp = Sys.time()) {
  .app_cache[[data_key]] <- timestamp
}

#' Get loading timestamp
#' @param data_key Key identifying the data type
#' @return Loading timestamp or NULL
#' @export
get_load_time <- function(data_key) {
  if (exists(data_key, envir = .app_cache)) {
    .app_cache[[data_key]]
  } else {
    NULL
  }
}

# ============================================================================
# PRELOADING / WARM-UP FUNCTIONS
# ============================================================================

#' Preload critical data into cache
#' This function can be called on app startup to warm the cache
#' @param site_id Site identifier
#' @param progress_callback Optional callback function for progress updates
#' @export
preload_cache <- function(site_id = NULL, progress_callback = NULL) {
  if (is.null(site_id) && exists("config")) {
    site_id <- config$app$default_site
  }

  if (is.null(site_id)) {
    message("No site specified for preloading")
    return(invisible(FALSE))
  }

  site_paths <- get_site_paths(site_id)

  update_progress <- function(msg, value = NULL) {
    if (!is.null(progress_callback)) {
      progress_callback(msg, value)
    } else {
      message(msg)
    }
  }

  start_time <- Sys.time()

  # 1. Preload biobank data
  update_progress("Preloading biobank data...", 0.1)
  biobank_file <- get_latest_biobank_file(site_paths$biobank_dir)
  if (!is.null(biobank_file)) {
    load_biobank_file_cached(biobank_file, cache_dir = site_paths$cache_dir)
  }

  # 2. Preload iELISA (usually smaller)
  update_progress("Preloading iELISA data...", 0.3)
  tryCatch({
    load_ielisa_data(
      ielisa_dir = site_paths$ielisa_dir,
      cache_dir = site_paths$cache_dir
    )
  }, error = function(e) message("iELISA preload skipped: ", e$message))

  # 3. Preload ELISA (larger, but commonly used)
  update_progress("Preloading ELISA data...", 0.5)
  tryCatch({
    load_elisa_data(
      dirs = c(site_paths$elisa_pe_dir, site_paths$elisa_vsg_dir)
    )
  }, error = function(e) message("ELISA preload skipped: ", e$message))

  # Note: MIC qPCR is typically loaded on-demand due to settings dependency

  elapsed <- difftime(Sys.time(), start_time, units = "secs")
  update_progress(sprintf("Preloading complete (%.1f seconds)", elapsed), 1.0)

  invisible(TRUE)
}

# ============================================================================
# CACHE METRICS
# ============================================================================

#' Get cache performance metrics
#' @return List with cache hit/miss statistics
#' @export
get_cache_metrics <- function() {
  list(
    elisa_cache_hit = !is.null(.elisa_cache_env$data),
    app_cache_keys = ls(.app_cache),
    cache_version = .cache_version
  )
}
