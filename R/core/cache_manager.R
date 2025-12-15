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

# ============================================================================
# UNIFIED CACHE API
# ============================================================================
# Provides a consistent interface for caching any data type
# Automatically handles file-based persistence and in-memory caching

#' Generate a cache key from parameters
#'
#' @param data_type Type of data (biobank, elisa, ielisa, mic)
#' @param ... Additional parameters to include in key (e.g., file path, settings)
#' @return Character cache key
#' @export
make_cache_key <- function(data_type, ...) {
  params <- list(...)
  key_content <- c(data_type, .cache_version, unlist(params))
  digest::digest(key_content, algo = "md5")
}

#' Get data from cache
#'
#' Attempts to retrieve data from in-memory cache first, then file cache.
#'
#' @param cache_key Cache key (from make_cache_key)
#' @param cache_dir Directory for file-based cache
#' @param max_age_hours Maximum age in hours before cache is considered stale
#' @return Cached data or NULL if not found/expired
#' @export
cache_get <- function(cache_key, cache_dir = NULL, max_age_hours = 24) {
  # Try in-memory cache first
  if (exists(cache_key, envir = .app_cache)) {
    cache_entry <- .app_cache[[cache_key]]

    # Check if it's just a timestamp (old format)
    if (inherits(cache_entry, "POSIXct")) {
      # Old format, need to check file
    } else if (is.list(cache_entry) && !is.null(cache_entry$data)) {
      # Check age
      age_hours <- as.numeric(difftime(Sys.time(), cache_entry$timestamp, units = "hours"))
      if (age_hours < max_age_hours) {
        # Log cache hit
        if (exists("log_cache_op")) {
          log_cache_op("hit", "memory", cache_key)
        }
        return(cache_entry$data)
      }
    }
  }

  # Try file-based cache
  if (!is.null(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

    if (file.exists(cache_file)) {
      file_info <- file.info(cache_file)
      age_hours <- as.numeric(difftime(Sys.time(), file_info$mtime, units = "hours"))

      if (age_hours < max_age_hours) {
        tryCatch({
          data <- readRDS(cache_file)

          # Store in memory for faster subsequent access
          .app_cache[[cache_key]] <- list(
            data = data,
            timestamp = file_info$mtime,
            source = "file"
          )

          if (exists("log_cache_op")) {
            log_cache_op("hit", "file", cache_key)
          }

          return(data)
        }, error = function(e) {
          message("Cache read error: ", e$message)
          return(NULL)
        })
      }
    }
  }

  # Cache miss
  if (exists("log_cache_op")) {
    log_cache_op("miss", "all", cache_key)
  }

  NULL
}

#' Store data in cache
#'
#' Stores data in both in-memory and file-based cache.
#'
#' @param cache_key Cache key (from make_cache_key)
#' @param data Data to cache
#' @param cache_dir Directory for file-based cache (optional)
#' @param persist Whether to persist to file (default TRUE if cache_dir provided)
#' @return Invisible TRUE on success
#' @export
cache_set <- function(cache_key, data, cache_dir = NULL, persist = TRUE) {
  timestamp <- Sys.time()

  # Store in memory
  .app_cache[[cache_key]] <- list(
    data = data,
    timestamp = timestamp,
    source = "set"
  )

  # Persist to file if requested
  if (persist && !is.null(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))

    tryCatch({
      saveRDS(data, cache_file)
      if (exists("log_cache_op")) {
        log_cache_op("write", "file", cache_key)
      }
    }, error = function(e) {
      message("Cache write error: ", e$message)
    })
  }

  invisible(TRUE)
}

#' Remove specific item from cache
#'
#' @param cache_key Cache key to remove
#' @param cache_dir Directory for file-based cache
#' @export
cache_remove <- function(cache_key, cache_dir = NULL) {
  # Remove from memory
  if (exists(cache_key, envir = .app_cache)) {
    rm(list = cache_key, envir = .app_cache)
  }

  # Remove file
  if (!is.null(cache_dir)) {
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    if (file.exists(cache_file)) {
      file.remove(cache_file)
    }
  }

  invisible(TRUE)
}

#' Cache-aware data loading wrapper
#'
#' Wraps a data loading function with caching. Returns cached data if available,
#' otherwise calls the loader function and caches the result.
#'
#' @param cache_key Unique cache key
#' @param loader_fn Function to call if cache miss (should return data)
#' @param cache_dir Directory for file cache
#' @param max_age_hours Maximum cache age
#' @param force_reload If TRUE, bypass cache and reload
#' @return Data (from cache or freshly loaded)
#' @export
cache_load <- function(cache_key, loader_fn, cache_dir = NULL, max_age_hours = 24, force_reload = FALSE) {
  # Check cache unless force reload
  if (!force_reload) {
    cached <- cache_get(cache_key, cache_dir, max_age_hours)
    if (!is.null(cached)) {
      return(cached)
    }
  }

  # Load fresh data
  data <- loader_fn()

  # Cache the result
  if (!is.null(data)) {
    cache_set(cache_key, data, cache_dir)
  }

  data
}

# ============================================================================
# CONVENIENCE FUNCTIONS FOR SPECIFIC DATA TYPES
# ============================================================================

#' Cache biobank data with standard settings
#'
#' @param file_path Path to biobank file
#' @param loader_fn Function to load biobank data
#' @param site_paths Site paths configuration
#' @return Biobank data
#' @export
cache_biobank <- function(file_path, loader_fn, site_paths = NULL) {
  if (is.null(site_paths) && exists("config")) {
    site_paths <- config$site_paths
  }

  cache_key <- make_cache_key("biobank", basename(file_path), file.info(file_path)$mtime)

  cache_load(
    cache_key = cache_key,
    loader_fn = loader_fn,
    cache_dir = site_paths$cache_dir,
    max_age_hours = 168  # 1 week
  )
}

#' Cache pipeline results with standard settings
#'
#' @param pipeline_name Pipeline name (mic, elisa_pe, elisa_vsg, ielisa)
#' @param run_id Run identifier
#' @param loader_fn Function to load pipeline results
#' @param site_paths Site paths configuration
#' @return Pipeline results
#' @export
cache_pipeline <- function(pipeline_name, run_id, loader_fn, site_paths = NULL) {
  if (is.null(site_paths) && exists("config")) {
    site_paths <- config$site_paths
  }

  cache_key <- make_cache_key(pipeline_name, run_id)

  cache_load(
    cache_key = cache_key,
    loader_fn = loader_fn,
    cache_dir = site_paths$cache_dir,
    max_age_hours = 24
  )
}

#' Invalidate all cache entries for a data type
#'
#' @param data_type Data type to invalidate
#' @param site_paths Site paths configuration
#' @export
invalidate_cache_type <- function(data_type, site_paths = NULL) {
  if (is.null(site_paths) && exists("config")) {
    site_paths <- config$site_paths
  }

  # Clear from memory
  keys_to_remove <- grep(data_type, ls(.app_cache), value = TRUE)
  if (length(keys_to_remove) > 0) {
    rm(list = keys_to_remove, envir = .app_cache)
  }

  # Clear from file (use existing clear_cache function)
  clear_cache(data_type, site_paths)
}
