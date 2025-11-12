# R/modules/mod_05_mic_qpcr.R
# =============================================================================
# Module 05 — MIC qPCR: BioMérieux MIC Analysis with Full QC (REBUILT)
# =============================================================================
# Clean, modular architecture for MIC file analysis with:
# - Incremental file parsing with caching
# - User-adjustable thresholds and QC parameters
# - Levey-Jennings monitoring
# - ΔCq metrics and flagging
# - Comprehensive QC scatter plots
# - Export capabilities
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(tidyverse)
  library(readxl)
  library(jsonlite)
  library(digest)
  library(plotly)
  library(DT)
  library(glue)
  library(lubridate)
})

# =============================================================================
# CONFIGURATION & DEFAULTS
# =============================================================================

default_mic_settings <- function() {
  list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40),
      `18S2` = list(positive = 35, negative = 40),
      RNAseP_DNA = list(positive = 32, negative = 45),
      RNAseP_RNA = list(positive = 30, negative = 45)
    ),
    late_window = c(38, 40),
    delta_rp_limit = 8,
    allow_review_controls = FALSE,
    pc_aliases = c("PC", "POS", "POSITIVE", "CP"),
    nc_aliases = c("NC", "NEG", "NTC", "CN")
  )
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

normalize_id <- function(x) {
  if (is.null(x)) return(NA_character_)
  x %>% as.character() %>% str_trim() %>% toupper()
}

safe_get_column <- function(df, possible_names, default = NA_character_) {
  # Try each possible column name and return the first one found
  for (name in possible_names) {
    if (!is.null(df) && name %in% names(df)) {
      return(df[[name]])
    }
  }
  rep(default, nrow(df))
}

parse_aliases <- function(text) {
  if (is.null(text) || !nzchar(trimws(text))) return(character())
  text %>% 
    str_split(",") %>% 
    unlist() %>% 
    str_trim() %>% 
    discard(~.x == "") %>% 
    toupper()
}

safe_median <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  median(x, na.rm = TRUE)
}

# =============================================================================
# FILE DISCOVERY & CACHING
# =============================================================================

scan_mic_directory <- function(path) {
  if (!dir.exists(path)) {
    return(tibble(
      file_path = character(),
      file_name = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      hash = character()
    ))
  }
  
  files <- list.files(path, pattern = "\\.(xls|xlsx)$", 
                      full.names = TRUE, ignore.case = TRUE)
  
  if (!length(files)) {
    return(tibble(
      file_path = character(),
      file_name = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      hash = character()
    ))
  }
  
  tibble(
    file_path = files,
    file_name = basename(files)
  ) %>%
    mutate(
      size = map_dbl(file_path, ~file.info(.x)$size),
      mtime = map_dbl(file_path, ~as.numeric(file.info(.x)$mtime)) %>% 
        as.POSIXct(origin = "1970-01-01", tz = Sys.timezone()),
      hash = map_chr(file_path, ~digest(paste(.x, file.info(.x)$size, 
                                              file.info(.x)$mtime), algo = "md5"))
    )
}

parse_run_datetime <- function(filename) {
  base <- tools::file_path_sans_ext(filename)
  parsed <- suppressWarnings(
    parse_date_time(base, orders = c("Ymd HMS", "Ymd HM", "Ymd"), truncated = 2)
  )
  if (is.na(parsed)) return(as.POSIXct(NA))
  parsed
}

# =============================================================================
# FILE PARSING (leveraging qpcr_analysis.R)
# =============================================================================

parse_single_mic_file <- function(file_info, settings) {
  result <- tryCatch({
    
    # Use the analyze_qpcr function from qpcr_analysis.R
    analysis <- analyze_qpcr(
      micrun_file = file_info$file_path,
      rnasep_rna_cutoff = settings$thresholds$RNAseP_RNA$positive,
      cutoffs = settings$thresholds,
      verbose = FALSE
    )
    
    run_id <- tools::file_path_sans_ext(file_info$file_name)
    run_datetime <- parse_run_datetime(file_info$file_name)
    
    # Get replicate data and ensure it has required columns
    rep_data <- analysis$replicate_data
    
    if (!nrow(rep_data)) {
      warning(glue("No replicate data in {file_info$file_name}"))
      return(list(success = FALSE, error = "No replicate data"))
    }
    
    # Create replicates_long with proper structure
    replicates_long <- tibble()
    
    # RIGHT BEFORE THE FOR LOOP, ADD THIS:
    message(sprintf("\n🔍 parse_single_mic_file: Checking rep_data for %s", file_info$file_name))
    message(sprintf("   rep_data has %d rows, %d columns", nrow(rep_data), ncol(rep_data)))
    message(sprintf("   Column names: %s", paste(names(rep_data), collapse = ", ")))
    
    # Process each target
    target_mappings <- list(
      "177T" = list(cq = "Cq_177T", marker = "marker_177T"),
      "18S2" = list(cq = "Cq_18S2", marker = "marker_18S2"),
      "RNAseP_DNA" = list(cq = "RNAseP_DNA_Cq", marker = "RNAseP_DNA"),  # DIFFERENT!
      "RNAseP_RNA" = list(cq = "RNAseP_RNA_Cq", marker = "RNAseP_RNA")   # DIFFERENT!
    )
    
    for (target in names(target_mappings)) {
      cq_col <- target_mappings[[target]]$cq
      marker_col <- target_mappings[[target]]$marker
      
      # ADD THIS:
      message(sprintf("\n   [%s] Looking for columns: %s, %s", target, cq_col, marker_col))
      message(sprintf("      %s exists? %s", cq_col, cq_col %in% names(rep_data)))
      message(sprintf("      %s exists? %s", marker_col, marker_col %in% names(rep_data)))
      
      if (cq_col %in% names(rep_data) && marker_col %in% names(rep_data)) {
        target_data <- rep_data %>%
          mutate(
            RunID = run_id,
            SampleID = normalize_id(Name),
            SampleName = Name,
            Target = target,
            Cq = .data[[cq_col]],
            Call = .data[[marker_col]],
            ControlType = case_when(
              Type %in% c("Positive", "Standard", "CP") ~ "PC",
              Type %in% c("Negative", "NTC", "CN") ~ "NC",
              TRUE ~ "Sample"
            )
          ) %>%
          select(RunID, SampleID, SampleName, Replicate, ControlType, Target, Cq, Call)
        
        replicates_long <- bind_rows(replicates_long, target_data)
      }
    }
    
    if (!nrow(replicates_long)) {
      warning(glue("No valid target data in {file_info$file_name}"))
      return(list(success = FALSE, error = "No valid target data"))
    }
    
    # Aggregate samples
    samples <- aggregate_samples_from_replicates(replicates_long, settings, run_id)
    
    if (!nrow(samples)) {
      warning(glue("Failed to aggregate samples in {file_info$file_name}"))
      return(list(success = FALSE, error = "Failed to aggregate samples"))
    }
    
    # Run metadata - FIXED: Convert JSON to character
    run_meta <- tibble(
      RunID = run_id,
      FilePath = file_info$file_path,
      FileName = file_info$file_name,
      FileMTime = file_info$mtime,
      RunDateTime = run_datetime,
      WellCount = n_distinct(rep_data$Name),
      ThresholdsJSON = as.character(toJSON(settings$thresholds, auto_unbox = TRUE))
    )
    
    list(
      run = run_meta,
      replicates = replicates_long,
      samples = samples,
      success = TRUE
    )
    
  }, error = function(e) {
    warning(glue("Failed to parse {file_info$file_name}: {e$message}"))
    list(success = FALSE, error = as.character(e$message))
  })
  
  result
}

# =============================================================================
# SAMPLE AGGREGATION & INTERPRETATION
# =============================================================================

aggregate_samples_from_replicates <- function(replicates_long, settings, run_id) {
  
  if (!nrow(replicates_long)) {
    return(tibble(
      RunID = character(),
      SampleID = character(),
      SampleName = character(),
      ControlType = character(),
      Cq_median_177T = numeric(),
      Cq_median_18S2 = numeric(),
      Cq_median_RNAseP_DNA = numeric(),
      Cq_median_RNAseP_RNA = numeric(),
      Call_177T = character(),
      Call_18S2 = character(),
      Call_RNAseP_DNA = character(),
      Call_RNAseP_RNA = character(),
      Delta_18S2_177T = numeric(),
      Delta_RP = numeric(),
      FinalCall = character(),
      Flags = character(),
      AnyFlag = logical()
    ))
  }
  
  # Calculate median Cq per sample per target
  target_summary <- replicates_long %>%
    group_by(RunID, SampleID, SampleName, ControlType, Target) %>%
    summarise(
      Cq_median = safe_median(Cq),
      Cq_mean = mean(Cq, na.rm = TRUE),
      n_reps = sum(!is.na(Cq)),
      .groups = "drop"
    )
  
  # Pivot wide
  samples_wide <- target_summary %>%
    select(RunID, SampleID, SampleName, ControlType, Target, Cq_median) %>%
    pivot_wider(
      names_from = Target,
      values_from = Cq_median,
      names_prefix = "Cq_median_"
    )
  
  # Ensure all target columns exist
  for (target in c("177T", "18S2", "RNAseP_DNA", "RNAseP_RNA")) {
    col <- paste0("Cq_median_", target)
    if (!col %in% names(samples_wide)) {
      samples_wide[[col]] <- NA_real_
    }
  }
  
  # Then in aggregate_samples_from_replicates, replace classify_target with classify_target_vectorized:
  samples_wide <- samples_wide %>%
    mutate(
      Call_177T = classify_target_vectorized(Cq_median_177T, "177T", settings),
      Call_18S2 = classify_target_vectorized(Cq_median_18S2, "18S2", settings),
      Call_RNAseP_DNA = classify_target_vectorized(Cq_median_RNAseP_DNA, "RNAseP_DNA", settings),
      Call_RNAseP_RNA = classify_target_vectorized(Cq_median_RNAseP_RNA, "RNAseP_RNA", settings)
    )
  
  # Calculate deltas
  samples_wide <- samples_wide %>%
    mutate(
      Delta_18S2_177T = Cq_median_18S2 - Cq_median_177T,
      Delta_RP = Cq_median_RNAseP_RNA - Cq_median_RNAseP_DNA
    )
  
  # Determine final call and flags - use rowwise for complex logic
  samples_wide <- samples_wide %>%
    rowwise() %>%
    mutate(
      PositiveTryp = (Call_177T == "Positive" | Call_18S2 == "Positive"),
      LateTryp = !PositiveTryp & (Call_177T == "LatePositive" | Call_18S2 == "LatePositive"),
      HostOK = (Call_RNAseP_DNA %in% c("Positive", "LatePositive")),
      Flag_SampleDecay = !is.na(Delta_RP) & Delta_RP > settings$delta_rp_limit,
      
      FinalCall = case_when(
        ControlType %in% c("PC", "NC") ~ "Control",
        PositiveTryp ~ "Positive",
        LateTryp ~ "LatePositive",
        !HostOK ~ "Invalid_NoDNA",
        TRUE ~ "Negative"
      ),
      
      Flags = {
        flags <- character()
        if (Flag_SampleDecay) flags <- c(flags, "SampleDecay")
        if (!HostOK && ControlType == "Sample") flags <- c(flags, "NoDNA")
        if (is.na(Delta_18S2_177T) && PositiveTryp) flags <- c(flags, "IncompleteTargets")
        if (length(flags)) paste(flags, collapse = ";") else NA_character_
      },
      
      AnyFlag = !is.na(Flags)
    ) %>%
    ungroup() %>%
    select(-PositiveTryp, -LateTryp, -HostOK, -Flag_SampleDecay)
  
  samples_wide
}

classify_target <- function(cq, target_name, settings) {
  # Vectorized version that handles NA values properly
  if (length(cq) == 0) return(character(0))
  
  # Get thresholds for this target
  thresholds <- settings$thresholds[[target_name]]
  if (is.null(thresholds)) {
    return(rep("Undetermined", length(cq)))
  }
  
  pos_cutoff <- as.numeric(thresholds$positive)
  neg_cutoff <- as.numeric(thresholds$negative)
  late_window <- settings$late_window
  
  # Vectorized classification
  result <- rep("Undetermined", length(cq))
  
  # Handle NA values
  valid <- !is.na(cq)
  
  # Classify valid values
  if (any(valid)) {
    result[valid & cq <= pos_cutoff] <- "Positive"
    result[valid & cq > neg_cutoff] <- "Negative"
    
    # Late positive window
    if (!is.null(late_window) && length(late_window) == 2) {
      late_pos <- valid & cq > late_window[1] & cq <= late_window[2]
      result[late_pos] <- "LatePositive"
    }
    
    # Everything else (between positive and negative) is Negative
    middle <- valid & cq > pos_cutoff & cq <= neg_cutoff
    result[middle] <- "Negative"
  }
  
  result
}

# Simpler classify_target using map
classify_target_vectorized <- function(cq_vector, target_name, settings) {
  map_chr(cq_vector, function(cq) {
    if (is.na(cq)) return("Undetermined")
    
    thresholds <- settings$thresholds[[target_name]]
    if (is.null(thresholds)) return("Undetermined")
    
    pos_cutoff <- as.numeric(thresholds$positive)
    neg_cutoff <- as.numeric(thresholds$negative)
    late_window <- settings$late_window
    
    if (cq <= pos_cutoff) return("Positive")
    if (cq > neg_cutoff) return("Negative")
    if (!is.null(late_window) && length(late_window) == 2 && 
        cq > late_window[1] && cq <= late_window[2]) {
      return("LatePositive")
    }
    "Negative"
  })
}

# =============================================================================
# DIRECTORY PARSING WITH CACHING
# =============================================================================

parse_mic_directory <- function(path, settings, cache_state) {
  files <- scan_mic_directory(path)
  
  if (!nrow(files)) {
    return(list(
      runs = tibble(),
      replicates = tibble(),
      samples = tibble(),
      cache = list(),
      files = files
    ))
  }
  
  cache <- cache_state
  all_runs <- list()
  all_samples <- list()
  all_replicates <- list()
  new_cache <- list()
  
  for (i in seq_len(nrow(files))) {
    file_row <- files[i, ]
    
    # Check cache
    if (!is.null(cache[[file_row$hash]])) {
      parsed <- cache[[file_row$hash]]
    } else {
      parsed <- parse_single_mic_file(file_row, settings)
    }
    
    if (!parsed$success) next
    
    new_cache[[file_row$hash]] <- parsed
    all_runs[[length(all_runs) + 1]] <- parsed$run
    all_samples[[length(all_samples) + 1]] <- parsed$samples
    all_replicates[[length(all_replicates) + 1]] <- parsed$replicates
  }
  
  runs_df <- if (length(all_runs)) bind_rows(all_runs) else tibble()
  samples_df <- if (length(all_samples)) bind_rows(all_samples) else tibble()
  replicates_df <- if (length(all_replicates)) bind_rows(all_replicates) else tibble()
  
  list(
    runs = runs_df,
    replicates = replicates_df,
    samples = samples_df,
    cache = new_cache,
    files = files
  )
}

# =============================================================================
# CONTROL QC & RUN VALIDATION
# =============================================================================

validate_controls <- function(samples_df, settings) {
  if (!nrow(samples_df)) return(tibble())
  
  controls <- samples_df %>%
    filter(ControlType %in% c("PC", "NC"))
  
  if (!nrow(controls)) return(tibble())
  
  thresholds <- settings$thresholds
  
  # Extract threshold values safely
  th_177t_pos <- as.numeric(thresholds$`177T`$positive)
  th_177t_neg <- as.numeric(thresholds$`177T`$negative)
  th_18s2_pos <- as.numeric(thresholds$`18S2`$positive)
  th_18s2_neg <- as.numeric(thresholds$`18S2`$negative)
  
  controls %>%
    mutate(
      # For PC: both targets should be positive (Cq <= pos cutoff) OR NA
      PC_177T_ok = if_else(is.na(Cq_median_177T), TRUE, Cq_median_177T <= th_177t_pos),
      PC_18S2_ok = if_else(is.na(Cq_median_18S2), TRUE, Cq_median_18S2 <= th_18s2_pos),
      
      # For NC: both targets should be negative (Cq > neg cutoff) OR NA
      NC_177T_ok = if_else(is.na(Cq_median_177T), TRUE, Cq_median_177T > th_177t_neg),
      NC_18S2_ok = if_else(is.na(Cq_median_18S2), TRUE, Cq_median_18S2 > th_18s2_neg),
      
      # Determine pass/fail based on control type
      ControlPass = case_when(
        ControlType == "PC" ~ (PC_177T_ok & PC_18S2_ok),
        ControlType == "NC" ~ (NC_177T_ok & NC_18S2_ok),
        TRUE ~ TRUE
      ),
      
      ControlFlag = case_when(
        !ControlPass & ControlType == "PC" ~ "FailedPositiveControl",
        !ControlPass & ControlType == "NC" ~ "FailedNegativeControl",
        TRUE ~ NA_character_
      )
    ) %>%
    select(RunID, SampleID, SampleName, ControlType, ControlPass, ControlFlag)
}

create_run_summary <- function(samples_df, runs_df, control_status) {
  if (!nrow(samples_df)) return(tibble())
  
  sample_counts <- samples_df %>%
    group_by(RunID) %>%
    summarise(
      TotalSamples = sum(ControlType == "Sample"),
      TotalControls = sum(ControlType %in% c("PC", "NC")),
      Positives = sum(FinalCall == "Positive" & ControlType == "Sample", na.rm = TRUE),
      Negatives = sum(FinalCall == "Negative" & ControlType == "Sample", na.rm = TRUE),
      LatePositives = sum(FinalCall == "LatePositive" & ControlType == "Sample", na.rm = TRUE),
      Flagged = sum(AnyFlag & ControlType == "Sample", na.rm = TRUE),
      .groups = "drop"
    )
  
  control_summary <- control_status %>%
    group_by(RunID) %>%
    summarise(
      ControlsPassing = all(ControlPass, na.rm = TRUE),
      FailedControls = paste(na.omit(ControlFlag), collapse = "; "),
      .groups = "drop"
    )
  
  runs_df %>%
    left_join(sample_counts, by = "RunID") %>%
    left_join(control_summary, by = "RunID") %>%
    mutate(
      RunValid = if_else(is.na(ControlsPassing), TRUE, ControlsPassing),
      TotalSamples = replace_na(TotalSamples, 0),
      Positives = replace_na(Positives, 0),
      Negatives = replace_na(Negatives, 0)
    )
}

# =============================================================================
# BIOBANK & EXTRACTION LINKING
# =============================================================================

# Replace link_to_biobank with this corrected version:

link_to_biobank <- function(samples_df, biobank_df) {
  if (!nrow(samples_df) || is.null(biobank_df) || !nrow(biobank_df)) {
    message("⚠️ No biobank data available for linking")
    return(samples_df %>% 
             mutate(
               BiobankMatched = FALSE, 
               Province = NA_character_,
               Structure = NA_character_,
               Cohort = NA_character_,
               SampleDate = as.Date(NA)
             ))
  }
  
  # DEBUG: Print what columns we actually have
  message("📊 Biobank data columns: ", paste(names(biobank_df), collapse = ", "))
  message("📊 Biobank data has ", nrow(biobank_df), " rows")
  
  # Find the ID column (use lab_id or barcode from your actual data)
  id_col <- NULL
  for (col in c("lab_id", "barcode", "numero", "Numero", "Sample", "sample_id")) {
    if (col %in% names(biobank_df)) {
      id_col <- col
      message("✓ Found ID column: ", col)
      break
    }
  }
  
  if (is.null(id_col)) {
    warning("❌ No ID column found in biobank data")
    return(samples_df %>% 
             mutate(
               BiobankMatched = FALSE, 
               Province = NA_character_,
               Structure = NA_character_,
               Cohort = NA_character_,
               SampleDate = as.Date(NA)
             ))
  }
  
  # Build lookup using your actual column names
  bb_lookup <- tibble(
    id_norm = normalize_id(biobank_df[[id_col]]),
    Province = if ("province" %in% names(biobank_df)) biobank_df$province else NA_character_,
    Structure = if ("health_facility" %in% names(biobank_df)) biobank_df$health_facility 
    else if ("health_structure" %in% names(biobank_df)) biobank_df$health_structure
    else NA_character_,
    Cohort = if ("study" %in% names(biobank_df)) biobank_df$study else NA_character_,
    SampleDate = if ("date_sample" %in% names(biobank_df)) suppressWarnings(as.Date(biobank_df$date_sample)) else as.Date(NA)
  ) %>%
    filter(!is.na(id_norm), id_norm != "") %>%
    distinct(id_norm, .keep_all = TRUE)
  
  message("📊 Created biobank lookup with ", nrow(bb_lookup), " unique samples")
  if (nrow(bb_lookup) > 0) {
    message("   Example biobank IDs: ", paste(head(bb_lookup$id_norm, 5), collapse = ", "))
  }
  
  # Get MIC sample IDs for comparison
  mic_ids <- normalize_id(samples_df$SampleID) %>% unique() %>% .[!is.na(.) & . != ""]
  message("📊 MIC has ", length(mic_ids), " unique sample IDs")
  if (length(mic_ids) > 0) {
    message("   Example MIC IDs: ", paste(head(mic_ids, 5), collapse = ", "))
  }
  
  # Join MIC SampleID to biobank ID
  result <- samples_df %>%
    mutate(SampleID_norm = normalize_id(SampleID)) %>%
    left_join(bb_lookup, by = c("SampleID_norm" = "id_norm")) %>%
    mutate(BiobankMatched = !is.na(Province) | !is.na(Structure) | !is.na(Cohort)) %>%
    select(-SampleID_norm)
  
  n_matched <- sum(result$BiobankMatched, na.rm = TRUE)
  message("✓ Matched ", n_matched, " MIC samples to biobank (", 
          round(100 * n_matched / nrow(samples_df)), "%)")
  
  result
}

link_to_extractions <- function(samples_df, extractions_df) {
  if (!nrow(samples_df) || is.null(extractions_df) || !nrow(extractions_df)) {
    message("⚠️ No extractions data available for linking")
    return(samples_df %>% mutate(ExtractionMatched = FALSE))
  }
  
  # DEBUG: Print what columns we actually have
  message("📊 Extractions data columns: ", paste(names(extractions_df), collapse = ", "))
  message("📊 Extractions data has ", nrow(extractions_df), " rows")
  
  # Try to find ANY column that might contain sample identifiers
  possible_id_cols <- c(
    "SampleID", "Sample", "numero", "sample_id", "Numero", 
    "sample", "SAMPLEID", "ID", "id", "Name", "name",
    "code_barres_kps", "barcode"  # Also try barcode matching
  )
  
  extraction_col <- NULL
  for (col in possible_id_cols) {
    if (col %in% names(extractions_df)) {
      extraction_col <- col
      message("✓ Found extraction ID column: ", col)
      break
    }
  }
  
  if (is.null(extraction_col)) {
    warning("❌ No matching ID column found in extractions data. Available columns: ", 
            paste(names(extractions_df), collapse = ", "))
    return(samples_df %>% mutate(ExtractionMatched = FALSE))
  }
  
  # Get extraction IDs and normalize
  ex_ids <- extractions_df[[extraction_col]] %>%
    as.character() %>%
    normalize_id() %>%
    unique() %>%
    .[!is.na(.) & . != ""]
  
  message("📊 Found ", length(ex_ids), " unique extraction IDs")
  if (length(ex_ids) > 0) {
    message("   Examples: ", paste(head(ex_ids, 3), collapse = ", "))
  }
  
  # Match MIC SampleID to extraction IDs
  samples_df %>%
    mutate(
      SampleID_norm = normalize_id(SampleID),
      ExtractionMatched = SampleID_norm %in% ex_ids
    ) %>%
    select(-SampleID_norm)
}

# =============================================================================
# LEVEY-JENNINGS CALCULATION
# =============================================================================

compute_levey_jennings <- function(replicates_long, target, control_type = "PC") {
  df <- replicates_long %>%
    filter(ControlType == control_type, Target == target, !is.na(Cq))
  
  if (!nrow(df)) {
    return(list(
      data = tibble(),
      summary = tibble(Target = target, Mean = NA_real_, SD = NA_real_, N = 0)
    ))
  }
  
  # Calculate mean Cq per run
  run_stats <- df %>%
    group_by(RunID) %>%
    summarise(Cq_mean = mean(Cq, na.rm = TRUE), .groups = "drop")
  
  # Overall statistics
  mu <- mean(df$Cq, na.rm = TRUE)
  sdv <- sd(df$Cq, na.rm = TRUE)
  
  plot_data <- run_stats %>%
    mutate(
      Mean = mu,
      SD = sdv,
      plus1 = mu + sdv,
      minus1 = mu - sdv,
      plus2 = mu + 2 * sdv,
      minus2 = mu - 2 * sdv,
      plus3 = mu + 3 * sdv,
      minus3 = mu - 3 * sdv
    )
  
  list(
    data = plot_data,
    summary = tibble(Target = target, Mean = mu, SD = sdv, N = nrow(df))
  )
}

# =============================================================================
# GLOBAL FILTER APPLICATION
# =============================================================================

apply_global_filters <- function(df, filters) {
  if (is.null(filters) || !is.list(filters) || !nrow(df)) return(df)
  
  # Date range filter
  if (!is.null(filters$date_range) && length(filters$date_range) == 2 && "SampleDate" %in% names(df)) {
    date_range <- tryCatch(
      suppressWarnings(as.Date(filters$date_range)),
      error = function(e) rep(NA, 2)
    )
    if (!any(is.na(date_range))) {
      df <- df %>%
        mutate(SampleDate = suppressWarnings(as.Date(SampleDate))) %>%
        filter(is.na(SampleDate) | (SampleDate >= date_range[1] & SampleDate <= date_range[2]))
    }
  }
  
  # Province filter
  if (!is.null(filters$province) && !identical(filters$province, "all") && "Province" %in% names(df)) {
    df <- df %>% filter(is.na(Province) | Province %in% filters$province)
  }
  
  # Structure filter
  if (!is.null(filters$structure) && !identical(filters$structure, "all") && "Structure" %in% names(df)) {
    df <- df %>% filter(is.na(Structure) | Structure %in% filters$structure)
  }
  
  # Cohort filter
  if (!is.null(filters$cohort) && !identical(filters$cohort, "all") && "Cohort" %in% names(df)) {
    df <- df %>% filter(is.na(Cohort) | Cohort %in% filters$cohort)
  }
  
  df
}

# =============================================================================
# UI
# =============================================================================

mod_mic_qpcr_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "MIC qPCR",
    icon = icon("dna"),
    
    # Header card with controls
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        div(class = "h5 mb-0", icon("dna"), " MIC qPCR Analysis"),
        div(
          actionButton(ns("refresh"), "Refresh", icon = icon("sync"), class = "btn-primary"),
          actionButton(ns("export_qc"), "Export QC", icon = icon("download"), class = "btn-success ms-2")
        )
      ),
      card_body(
        textInput(ns("mic_dir"), "MIC Directory", value = "data/MIC", 
                  placeholder = "Path to MIC Excel files"),
        
        h6("Thresholds", class = "mt-3"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          numericInput(ns("th_177t_pos"), "177T Positive ≤", value = 35, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_177t_neg"), "177T Negative >", value = 40, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_18s2_pos"), "18S2 Positive ≤", value = 35, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_18s2_neg"), "18S2 Negative >", value = 40, min = 0, max = 50, step = 0.5)
        ),
        
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          numericInput(ns("th_rnp_dna_pos"), "RNAseP DNA Pos ≤", value = 32, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_rnp_dna_neg"), "RNAseP DNA Neg >", value = 45, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_rnp_rna_pos"), "RNAseP RNA Pos ≤", value = 30, min = 0, max = 50, step = 0.5),
          numericInput(ns("th_rnp_rna_neg"), "RNAseP RNA Neg >", value = 45, min = 0, max = 50, step = 0.5)
        ),
        
        h6("QC Parameters", class = "mt-3"),
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          numericInput(ns("late_min"), "Late Positive Min", value = 38, min = 0, max = 50, step = 0.5),
          numericInput(ns("late_max"), "Late Positive Max", value = 40, min = 0, max = 50, step = 0.5),
          numericInput(ns("delta_rp_limit"), "ΔCq RNA Limit", value = 8, min = 0, max = 20, step = 0.5),
          checkboxInput(ns("allow_review"), "Allow review despite control failures", value = FALSE)
        )
      )
    ),
    
    # KPI boxes
    layout_column_wrap(
      width = 1/6,
      value_box(title = "Runs", value = textOutput(ns("kpi_runs")), showcase = icon("folder-open")),
      value_box(title = "Samples", value = textOutput(ns("kpi_samples")), showcase = icon("vial")),
      value_box(title = "Biobank Linked", value = textOutput(ns("kpi_biobank")), showcase = icon("link")),
      value_box(title = "Extractions Linked", value = textOutput(ns("kpi_extractions")), showcase = icon("flask")),
      value_box(title = "Positives", value = textOutput(ns("kpi_positives")), showcase = icon("check-circle"), theme = "success"),
      value_box(title = "Flagged", value = textOutput(ns("kpi_flagged")), showcase = icon("flag"), theme = "warning")
    ),
    
    # Main tabs
    navset_card_tab(
      nav_panel(
        "Runs",
        icon = icon("clipboard-list"),
        DTOutput(ns("tbl_runs"))
      ),
      
      nav_panel(
        "Samples",
        icon = icon("vials"),
        DTOutput(ns("tbl_samples"))
      ),
      
      nav_panel(
        "Controls & L-J",
        icon = icon("flask"),
        card(card_header("Control Status"), DTOutput(ns("tbl_controls"))),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("177T Positive Control"), plotlyOutput(ns("lj_177t"), height = "400px")),
          card(card_header("18S2 Positive Control"), plotlyOutput(ns("lj_18s2"), height = "400px"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("RNAseP DNA Positive Control"), plotlyOutput(ns("lj_rnp_dna"), height = "400px")),
          card(card_header("RNAseP RNA Positive Control"), plotlyOutput(ns("lj_rnp_rna"), height = "400px"))
        )
      ),
      
      nav_panel(
        "QC Scatter",
        icon = icon("chart-scatter"),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("18S2 vs 177T"), plotlyOutput(ns("scatter_tryp"), height = "500px")),
          card(card_header("RNAseP RNA vs DNA (ΔCq)"), plotlyOutput(ns("scatter_rnp"), height = "500px"))
        )
      ),
      
      nav_panel(
        "Flags",
        icon = icon("flag"),
        DTOutput(ns("tbl_flags"))
      ),
      
      nav_panel(
        "Exports",
        icon = icon("download"),
        card(
          card_body(
            h5("Download Data"),
            downloadButton(ns("dl_runs"), "Run Metadata", class = "btn-primary w-100 mb-2"),
            downloadButton(ns("dl_samples"), "Sample Calls", class = "btn-primary w-100 mb-2"),
            downloadButton(ns("dl_deltas"), "ΔCq Summary", class = "btn-primary w-100 mb-2"),
            downloadButton(ns("dl_flags"), "Flagged Samples", class = "btn-warning w-100 mb-2"),
            downloadButton(ns("dl_lj"), "Levey-Jennings Stats", class = "btn-info w-100")
          )
        )
      )
    )
  )
}

# =============================================================================
# SERVER
# =============================================================================

mod_mic_qpcr_server <- function(id, biobank_df, extractions_df, filters) {
  moduleServer(id, function(input, output, session) {
    
    # Settings reactive
    settings <- reactive({
      list(
        thresholds = list(
          `177T` = list(positive = input$th_177t_pos, negative = input$th_177t_neg),
          `18S2` = list(positive = input$th_18s2_pos, negative = input$th_18s2_neg),
          RNAseP_DNA = list(positive = input$th_rnp_dna_pos, negative = input$th_rnp_dna_neg),
          RNAseP_RNA = list(positive = input$th_rnp_rna_pos, negative = input$th_rnp_rna_neg)
        ),
        late_window = c(input$late_min, input$late_max),
        delta_rp_limit = input$delta_rp_limit,
        allow_review_controls = isTRUE(input$allow_review),
        pc_aliases = c("PC", "POS", "POSITIVE", "CP"),
        nc_aliases = c("NC", "NEG", "NTC", "CN")
      )
    })
    
    # Cache state
    cache_state <- reactiveVal(list())
    
    # Raw data parsing
    raw_data <- reactive({
      req(input$mic_dir)
      
      withProgress(message = "Loading MIC files...", value = 0.3, {
        parsed <- parse_mic_directory(input$mic_dir, settings(), cache_state())
        cache_state(parsed$cache)
        parsed
      })
    })
    
    # Force refresh
    observeEvent(input$refresh, {
      cache_state(list())
      raw_data()
    })
    
    # Processed data with linkage and validation
    processed_data <- reactive({
      rd <- raw_data()
      
      if (!nrow(rd$samples)) {
        return(list(
          runs = tibble(),
          samples = tibble(),
          replicates = tibble(),
          control_status = tibble(),
          lj_stats = list()
        ))
      }
      
      # Validate controls
      control_status <- validate_controls(rd$samples, settings())
      
      # Create run summary
      runs_summary <- create_run_summary(rd$samples, rd$runs, control_status)
      
      # Link to biobank and extractions
      samples_linked <- rd$samples %>%
        link_to_biobank(if (is.null(biobank_df)) NULL else biobank_df()) %>%
        link_to_extractions(if (is.null(extractions_df)) NULL else extractions_df())
      
      # Apply control flags to samples
      samples_linked <- samples_linked %>%
        left_join(
          control_status %>% select(RunID, SampleID, ControlFlag),
          by = c("RunID", "SampleID")
        ) %>%
        mutate(
          Flags = if_else(
            !is.na(ControlFlag),
            if_else(is.na(Flags), ControlFlag, paste(Flags, ControlFlag, sep = ";")),
            Flags
          ),
          AnyFlag = AnyFlag | !is.na(ControlFlag)
        )
      
      # Mark invalid runs if controls failed
      if (!settings()$allow_review_controls) {
        invalid_runs <- runs_summary %>% 
          filter(!RunValid) %>% 
          pull(RunID)
        
        samples_linked <- samples_linked %>%
          mutate(
            FinalCall = if_else(RunID %in% invalid_runs, "RunInvalid", FinalCall),
            Flags = if_else(RunID %in% invalid_runs, 
                            paste(Flags, "RunInvalid", sep = ";"), 
                            Flags),
            AnyFlag = if_else(RunID %in% invalid_runs, TRUE, AnyFlag)
          )
      }
      
      # Compute Levey-Jennings stats
      lj_stats <- list(
        `177T` = compute_levey_jennings(rd$replicates, "177T"),
        `18S2` = compute_levey_jennings(rd$replicates, "18S2"),
        RNAseP_DNA = compute_levey_jennings(rd$replicates, "RNAseP_DNA"),
        RNAseP_RNA = compute_levey_jennings(rd$replicates, "RNAseP_RNA")
      )
      
      list(
        runs = runs_summary,
        samples = samples_linked,
        replicates = rd$replicates,
        control_status = control_status,
        lj_stats = lj_stats
      )
    })
    
    # Filtered samples
    filtered_samples <- reactive({
      pd <- processed_data()
      apply_global_filters(pd$samples, if (is.null(filters)) NULL else filters())
    })
    
    # ========================================================================
    # KPIs
    # ========================================================================
    
    # ========================================================================
    # KPIs - SAFE VERSION WITH ERROR HANDLING
    # ========================================================================
    
    output$kpi_runs <- renderText({
      runs <- processed_data()$runs
      if (!nrow(runs)) return("0")
      nrow(runs) %>% scales::comma()
    })
    
    output$kpi_samples <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df)) return("0")
      
      df %>% 
        filter(ControlType == "Sample") %>% 
        nrow() %>% 
        scales::comma()
    })
    
    output$kpi_biobank <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"BiobankMatched" %in% names(df)) {
        return("0")
      }
      
      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0")
      
      n_linked <- sum(df$BiobankMatched, na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_linked / total) else 0
      
      glue("{scales::comma(n_linked)} ({pct}%)")
    })
    
    output$kpi_extractions <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"ExtractionMatched" %in% names(df)) {
        return("0")
      }
      
      df <- df %>% filter(ControlType == "Sample")
      if (!nrow(df)) return("0")
      
      n_linked <- sum(df$ExtractionMatched, na.rm = TRUE)
      total <- nrow(df)
      pct <- if (total > 0) round(100 * n_linked / total) else 0
      
      glue("{scales::comma(n_linked)} ({pct}%)")
    })
    
    output$kpi_positives <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"FinalCall" %in% names(df)) {
        return("0")
      }
      
      df %>%
        filter(ControlType == "Sample", FinalCall == "Positive") %>%
        nrow() %>%
        scales::comma()
    })
    
    output$kpi_flagged <- renderText({
      df <- filtered_samples()
      if (!nrow(df) || !"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return("0")
      }
      
      df %>%
        filter(ControlType == "Sample", AnyFlag == TRUE) %>%
        nrow() %>%
        scales::comma()
    })
    
    # ========================================================================
    # TABLES - SAFE VERSION WITH ERROR HANDLING
    # ========================================================================
    
    output$tbl_runs <- renderDT({
      runs <- processed_data()$runs
      
      if (!nrow(runs)) {
        return(datatable(
          tibble(Message = "No MIC runs found. Check that files exist in the MIC directory."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      display_runs <- runs %>%
        mutate(
          RunDateTime = as.character(RunDateTime),
          FileMTime = as.character(FileMTime)
        )
      
      # Only select columns that exist
      available_cols <- intersect(
        c("RunID", "FileName", "RunDateTime", "WellCount", "TotalSamples", 
          "Positives", "Negatives", "LatePositives", "Flagged", "RunValid"),
        names(display_runs)
      )
      
      datatable(
        display_runs %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    output$tbl_samples <- renderDT({
      df <- filtered_samples()
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found. Check MIC files and filters."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      if (!"ControlType" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: ControlType column missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      df <- df %>% filter(ControlType == "Sample")
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No samples found (only controls detected)."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Build display dataframe with only available columns
      display_df <- df
      
      # Round numeric columns if they exist
      numeric_cols <- intersect(
        c("Cq_median_177T", "Cq_median_18S2", "Cq_median_RNAseP_DNA", 
          "Cq_median_RNAseP_RNA", "Delta_18S2_177T", "Delta_RP"),
        names(display_df)
      )
      
      if (length(numeric_cols) > 0) {
        display_df <- display_df %>%
          mutate(across(all_of(numeric_cols), ~round(.x, 2)))
      }
      
      # Select columns that exist
      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", 
          "Cq_median_177T", "Cq_median_18S2", 
          "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA",
          "Delta_18S2_177T", "Delta_RP",
          "BiobankMatched", "ExtractionMatched", "Flags"),
        names(display_df)
      )
      
      datatable(
        display_df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    output$tbl_controls <- renderDT({
      ctrl <- processed_data()$control_status
      
      if (!nrow(ctrl)) {
        return(datatable(
          tibble(Message = "No control data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Select available columns
      available_cols <- intersect(
        c("RunID", "SampleName", "ControlType", "ControlPass", "ControlFlag"),
        names(ctrl)
      )
      
      datatable(
        ctrl %>% select(all_of(available_cols)),
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE
      )
    })
    
    output$tbl_flags <- renderDT({
      df <- filtered_samples()
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No data available."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      if (!"ControlType" %in% names(df) || !"AnyFlag" %in% names(df)) {
        return(datatable(
          tibble(Message = "Data structure error: required columns missing."),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Filter for flagged samples
      df <- df %>%
        filter(
          ControlType == "Sample",
          AnyFlag == TRUE | (if ("FinalCall" %in% names(df)) FinalCall == "Invalid_NoDNA" else FALSE)
        )
      
      if (!nrow(df)) {
        return(datatable(
          tibble(Message = "No flagged samples - all QC passed!"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      # Select available columns
      available_cols <- intersect(
        c("RunID", "SampleName", "FinalCall", "Flags", "Delta_RP", 
          "BiobankMatched", "ExtractionMatched"),
        names(df)
      )
      
      datatable(
        df %>% select(all_of(available_cols)),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        rownames = FALSE
      )
    })
    
    # ========================================================================
    # LEVEY-JENNINGS PLOTS
    # ========================================================================
    
    render_lj_plot <- function(target_name, output_name) {
      output[[output_name]] <- renderPlotly({
        lj <- processed_data()$lj_stats[[target_name]]
        
        if (!nrow(lj$data)) {
          return(plotly_empty() %>% 
                   layout(title = glue("No {target_name} control data")))
        }
        
        plot_ly(lj$data, x = ~RunID, y = ~Cq_mean, 
                type = 'scatter', mode = 'markers+lines',
                name = 'Run Mean', marker = list(size = 8)) %>%
          add_lines(y = ~Mean, name = 'Mean', 
                    line = list(color = 'black', width = 2)) %>%
          add_lines(y = ~plus1, name = '+1 SD', 
                    line = list(color = 'blue', dash = 'dot')) %>%
          add_lines(y = ~minus1, name = '-1 SD', 
                    line = list(color = 'blue', dash = 'dot')) %>%
          add_lines(y = ~plus2, name = '+2 SD', 
                    line = list(color = 'orange', dash = 'dash')) %>%
          add_lines(y = ~minus2, name = '-2 SD', 
                    line = list(color = 'orange', dash = 'dash')) %>%
          add_lines(y = ~plus3, name = '+3 SD', 
                    line = list(color = 'red', dash = 'dashdot')) %>%
          add_lines(y = ~minus3, name = '-3 SD', 
                    line = list(color = 'red', dash = 'dashdot')) %>%
          layout(
            title = glue("Levey-Jennings: {target_name}"),
            xaxis = list(title = "Run ID"),
            yaxis = list(title = "Cq"),
            legend = list(orientation = 'h', y = -0.2)
          )
      })
    }
    
    render_lj_plot("177T", "lj_177t")
    render_lj_plot("18S2", "lj_18s2")
    render_lj_plot("RNAseP_DNA", "lj_rnp_dna")
    render_lj_plot("RNAseP_RNA", "lj_rnp_rna")
    
    # ========================================================================
    # QC SCATTER PLOTS
    # ========================================================================
    
    output$scatter_tryp <- renderPlotly({
      df <- filtered_samples()
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      required_cols <- c("ControlType", "Cq_median_177T", "Cq_median_18S2", "FinalCall", "SampleName")
      missing_cols <- setdiff(required_cols, names(df))
      
      if (length(missing_cols) > 0) {
        return(plotly_empty() %>% 
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }
      
      df <- df %>%
        filter(
          ControlType == "Sample", 
          !is.na(Cq_median_177T), 
          !is.na(Cq_median_18S2)
        )
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with both 177T and 18S2 data"))
      }
      
      plot_ly(df, x = ~Cq_median_177T, y = ~Cq_median_18S2,
              color = ~FinalCall, type = 'scatter', mode = 'markers',
              text = ~SampleName, hoverinfo = 'text') %>%
        layout(
          title = "18S2 vs 177T",
          xaxis = list(title = "177T Cq"),
          yaxis = list(title = "18S2 Cq")
        )
    })
    
    output$scatter_rnp <- renderPlotly({
      df <- filtered_samples()
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      required_cols <- c("ControlType", "Cq_median_RNAseP_DNA", "Cq_median_RNAseP_RNA", "SampleName")
      missing_cols <- setdiff(required_cols, names(df))
      
      if (length(missing_cols) > 0) {
        return(plotly_empty() %>% 
                 layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
      }
      
      # Check if Flag_SampleDecay exists, if not create it
      if (!"Flag_SampleDecay" %in% names(df)) {
        df <- df %>% mutate(Flag_SampleDecay = FALSE)
      }
      
      df <- df %>%
        filter(
          ControlType == "Sample",
          !is.na(Cq_median_RNAseP_DNA),
          !is.na(Cq_median_RNAseP_RNA)
        )
      
      if (!nrow(df)) {
        return(plotly_empty() %>% layout(title = "No samples with RNAseP data"))
      }
      
      plot_ly(df, x = ~Cq_median_RNAseP_DNA, y = ~Cq_median_RNAseP_RNA,
              color = ~Flag_SampleDecay, type = 'scatter', mode = 'markers',
              text = ~SampleName, hoverinfo = 'text') %>%
        layout(
          title = "RNAseP RNA vs DNA (Sample Quality)",
          xaxis = list(title = "RNAseP DNA Cq"),
          yaxis = list(title = "RNAseP RNA Cq")
        )
    })
    
    # ========================================================================
    # DOWNLOADS
    # ========================================================================
    
    output$dl_runs <- downloadHandler(
      filename = function() sprintf("mic_runs_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(processed_data()$runs, file)
    )
    
    output$dl_samples <- downloadHandler(
      filename = function() sprintf("mic_samples_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) write_csv(filtered_samples(), file)
    )
    
    output$dl_deltas <- downloadHandler(
      filename = function() sprintf("mic_deltas_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          select(RunID, SampleID, SampleName, Delta_18S2_177T, Delta_RP, Flag_SampleDecay)
        write_csv(df, file)
      }
    )
    
    output$dl_flags <- downloadHandler(
      filename = function() sprintf("mic_flags_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        df <- filtered_samples() %>%
          filter(ControlType == "Sample", AnyFlag | FinalCall == "Invalid_NoDNA")
        write_csv(df, file)
      }
    )
    
    output$dl_lj <- downloadHandler(
      filename = function() sprintf("mic_lj_stats_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        lj <- processed_data()$lj_stats
        stats <- map_dfr(lj, "summary")
        write_csv(stats, file)
      }
    )
    
    # Quick export button
    observeEvent(input$export_qc, {
      pd <- processed_data()
      if (!nrow(pd$samples)) {
        showNotification("No data to export", type = "warning")
        return()
      }
      
      dir.create("outputs", showWarnings = FALSE)
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      path <- file.path("outputs", glue("MIC_QC_{timestamp}.csv"))
      write_csv(pd$samples, path)
      
      showNotification(
        glue("QC export saved: {path}"),
        type = "message",
        duration = 5
      )
    })
    
  })
}

# =============================================================================
# MOUNTING INSTRUCTIONS
# =============================================================================
# In your main UI:
#   mod_mic_qpcr_ui("mic")
#
# In your main server:
#   mod_mic_qpcr_server("mic", 
#                       biobank_df = reactive(biobank_data),
#                       extractions_df = reactive(extractions_data),
#                       filters = global_filters)