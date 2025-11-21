# =============================================================================
# Indirect ELISA Parser - UNIFIED (Single OR 4-Plate Format)
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(janitor)
  library(lubridate)
})

# -----------------------------------------------------------------------------
# Parse deepwell pattern
# -----------------------------------------------------------------------------
parse_deepwell_pattern <- function(x) {
  if (is.na(x) || x == "") return(character(0))

  x %>%
    as.character() %>%
    str_replace_all("\\s+", "") %>%
    str_replace_all("ou", "/") %>%
    str_replace_all(";", "/") %>%
    str_replace_all(",", "/") %>%
    str_split("/") %>%
    pluck(1)
}

# -----------------------------------------------------------------------------
# Ensure expected columns are present
# -----------------------------------------------------------------------------
ensure_columns <- function(df, cols, default = NA_character_) {
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- default
    }
  }
  df
}

# -----------------------------------------------------------------------------
# Parse date from filename
# -----------------------------------------------------------------------------
parse_plate_date_from_filename <- function(filename) {
  stem   <- basename(filename)
  digits <- str_extract(stem, "^\\d+")
  if (is.na(digits)) return(as.Date(NA))
  
  if (nchar(digits) == 6) {
    yy <- substr(digits, 1, 2)
    mm <- substr(digits, 3, 4)
    dd <- substr(digits, 5, 6)
    full <- paste0("20", yy, "-", mm, "-", dd)
    return(as.Date(full))
  }
  
  if (nchar(digits) == 8) {
    yyyy <- substr(digits, 1, 4)
    mm   <- substr(digits, 5, 6)
    dd   <- substr(digits, 7, 8)
    full <- paste0(yyyy, "-", mm, "-", dd)
    return(as.Date(full))
  }
  
  as.Date(NA)
}

# -----------------------------------------------------------------------------
# Read OD grid - HANDLES BOTH SINGLE AND 4-PLATE FORMATS
# -----------------------------------------------------------------------------
read_elisa_od_grid <- function(path, sheet = "450 nm - 600 nm") {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE)
  
  # Check if this is 4-plate format (look for "Plaque" markers)
  plate_markers <- which(grepl("Plaque \\d+:", raw[[1]], ignore.case = TRUE))
  
  if (length(plate_markers) > 0) {
    # ==== 4-PLATE FORMAT ====
    cat("  Detected 4-plate format\n")
    
    all_plates_od <- list()
    
    for (i in seq_along(plate_markers)) {
      plate_num <- i
      start_row <- plate_markers[i]
      
      # Skip 3 rows (marker, empty, header)
      data_start <- start_row + 3
      
      # Read 8 rows (A-H)
      plate_data <- raw[data_start:(data_start + 7), 1:13]
      names(plate_data) <- c("row_label", as.character(1:12))
      
      od_long <- plate_data %>%
        filter(row_label %in% LETTERS[1:8]) %>%
        mutate(across(`1`:`12`, as.numeric)) %>%
        pivot_longer(
          cols = `1`:`12`,
          names_to = "col",
          values_to = "od"
        ) %>%
        mutate(
          col = as.integer(col),
          plate_num = plate_num
        ) %>%
        select(plate_num, row_label, col, od)
      
      all_plates_od[[paste0("plate_", plate_num)]] <- od_long
    }
    
    # Combine and map to deepwell positions
    od_all <- bind_rows(all_plates_od)
    
    # For 4-plate format, map columns to deepwell positions
    # Deepwell cols 1-3 → ELISA plate 1, cols 4-6 → plate 2, etc.
    od_all <- od_all %>%
      mutate(
        # Get position within set of 3
        pos_in_set = ((col - 1) %% 3) + 1,
        # Determine Ag+/Ag0 and replicate number
        ag_replicate = case_when(
          col <= 3  ~ paste0("Ag_plus_", pos_in_set),
          col <= 6  ~ paste0("Ag_plus_", pos_in_set),
          col <= 9  ~ paste0("Ag0_", pos_in_set),
          col <= 12 ~ paste0("Ag0_", pos_in_set)
        ),
        ag_state = if_else(col <= 6, "Ag_plus", "Ag0"),
        # Map to deepwell column
        deepwell_col = case_when(
          plate_num == 1 ~ pos_in_set,
          plate_num == 2 ~ pos_in_set + 3,
          plate_num == 3 ~ pos_in_set + 6,
          plate_num == 4 ~ pos_in_set + 9
        ),
        well_id = paste0(row_label, deepwell_col)
      ) %>%
      select(plate_num, well_id, row_label, col, ag_state, od)
    
    return(list(
      od = od_all,
      elisa_type = "ELISA_vsg",
      plate_format = "multi"
    ))
    
  } else {
    # ==== SINGLE-PLATE FORMAT ====
    cat("  Detected single-plate format\n")
    
    names(raw)[1] <- "row_label"
    
    raw_plate <- raw %>%
      mutate(row_label = as.character(row_label)) %>%
      filter(row_label %in% LETTERS[1:8])
    
    n_plate_cols <- ncol(raw_plate) - 1
    if (n_plate_cols <= 0) {
      stop("No OD columns detected")
    }
    
    plate_cols <- as.character(seq_len(n_plate_cols))
    names(raw_plate)[2:ncol(raw_plate)] <- plate_cols
    
    od_long <- raw_plate %>%
      mutate(across(all_of(plate_cols), ~ suppressWarnings(as.numeric(.)))) %>%
      rename(row = row_label) %>%
      pivot_longer(
        cols = all_of(plate_cols),
        names_to = "col",
        values_to = "od"
      ) %>%
      mutate(
        col = as.integer(col),
        well_id = paste0(row, col),
        ag_state = if_else(col <= 6, "Ag_plus", "Ag0"),
        plate_num = 1  # Single plate = plate 1
      ) %>%
      select(plate_num, well_id, row, col, ag_state, od)
    
    return(list(
      od = od_long,
      elisa_type = "ELISA_pe",
      plate_format = "single"
    ))
  }
}

# -----------------------------------------------------------------------------
# Read layouts - HANDLES BOTH FORMATS
# -----------------------------------------------------------------------------
read_elisa_layout <- function(path) {
  plate_id <- tools::file_path_sans_ext(basename(path))
  
  # ----- Samples -----
  df_samples <- read_excel(path, sheet = "Results") %>%
    clean_names() %>%
    ensure_columns(c("sample", "numero_labo", "code_barres_kps", "sample_code"))
  
  dw_col_s <- names(df_samples)[str_detect(names(df_samples), "^deepwell")]
  if (length(dw_col_s) == 0) {
    stop("No 'Deepwell' column in 'Results' sheet")
  }
  dw_col_s <- dw_col_s[1]
  
  # Check if ELISA column exists (4-plate format)
  has_elisa_col <- "elisa" %in% names(df_samples)
  
  if (has_elisa_col) {
    # 4-plate format
    samples_layout <- df_samples %>%
      filter(!is.na(.data[[dw_col_s]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = as.integer(elisa),
        sample_type = "sample",
        sample_code = NA_character_,  # Add sample_code column for samples
        well_id = .data[[dw_col_s]]  # Direct mapping in 4-plate
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, well_id, everything())
  } else {
    # Single-plate format
    samples_layout <- df_samples %>%
      filter(!is.na(.data[[dw_col_s]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = 1,
        sample_type = "sample",
        sample_code = NA_character_,  # Add sample_code column for samples
        wells_raw = .data[[dw_col_s]],
        wells = map(wells_raw, parse_deepwell_pattern)
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, wells, everything()) %>%
      unnest_longer(wells, values_to = "well_id")
  }
  
  # ----- Controls -----
  df_controls <- read_excel(path, sheet = "Controls") %>%
    clean_names() %>%
    ensure_columns(c("sample", "sample_code"))
  
  dw_col_c <- names(df_controls)[str_detect(names(df_controls), "^deepwell")]
  if (length(dw_col_c) == 0) {
    stop("No 'Deepwell' column in 'Controls' sheet")
  }
  dw_col_c <- dw_col_c[1]
  
  has_elisa_col_c <- "elisa" %in% names(df_controls)
  
  if (has_elisa_col_c) {
    # 4-plate format
    controls_layout <- df_controls %>%
      filter(!is.na(.data[[dw_col_c]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = as.integer(elisa),
        sample_type = "control",
        numero_labo = NA_character_,  # Add numero_labo column for controls
        code_barres_kps = NA_character_,  # Add code_barres_kps column for controls
        well_id = .data[[dw_col_c]]
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, well_id, everything())
  } else {
    # Single-plate format
    controls_layout <- df_controls %>%
      filter(!is.na(.data[[dw_col_c]])) %>%
      mutate(
        plate_id = plate_id,
        plate_num = 1,
        sample_type = "control",
        numero_labo = NA_character_,  # Add numero_labo column for controls
        code_barres_kps = NA_character_,  # Add code_barres_kps column for controls
        wells_raw = .data[[dw_col_c]],
        wells = map(wells_raw, parse_deepwell_pattern)
      ) %>%
      select(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps, wells, everything()) %>%
      unnest_longer(wells, values_to = "well_id")
  }
  
  list(
    samples_layout = samples_layout,
    controls_layout = controls_layout
  )
}

# -----------------------------------------------------------------------------
# Calculate CV
# -----------------------------------------------------------------------------
calculate_cv <- function(val1, val2) {
  if (is.na(val1) || is.na(val2)) return(NA_real_)
  vals <- c(val1, val2)
  cv <- (sd(vals) / mean(vals)) * 100
  return(cv)
}

# -----------------------------------------------------------------------------
# Main extraction function - UNIFIED
# -----------------------------------------------------------------------------
extract_elisa_plate_summary <- function(path, delta_max = 0.15, cv_max_ag_plus = 15, cv_max_ag0 = 15) {

  plate_id <- tools::file_path_sans_ext(basename(path))
  plate_date <- parse_plate_date_from_filename(basename(path))

  cat(sprintf("Processing: %s (Date: %s)\n", basename(path),
              ifelse(is.na(plate_date), "Unknown", as.character(plate_date))))

  # 1) Read OD data
  od_grid_info <- read_elisa_od_grid(path)
  od_grid <- od_grid_info$od
  elisa_type <- od_grid_info$elisa_type

  # Fallback: Infer elisa_type from file path if not set correctly
  if (is.null(elisa_type) || is.na(elisa_type) || elisa_type == "") {
    if (grepl("elisa_vsg|vsg", path, ignore.case = TRUE)) {
      elisa_type <- "ELISA_vsg"
    } else if (grepl("elisa_pe|pe", path, ignore.case = TRUE)) {
      elisa_type <- "ELISA_pe"
    } else {
      # Default to PE if cannot infer
      elisa_type <- "ELISA_pe"
    }
    cat(sprintf("  Inferred elisa_type from path: %s\n", elisa_type))
  }
  
  # 2) Read layouts
  layout <- read_elisa_layout(path)
  samples_layout <- layout$samples_layout
  controls_layout <- layout$controls_layout

  # Ensure all expected columns exist in layouts before harmonizing
  samples_layout <- ensure_columns(
    samples_layout,
    c("plate_id", "plate_num", "sample_type", "sample", "sample_code",
      "numero_labo", "code_barres_kps", "well_id")
  )

  controls_layout <- ensure_columns(
    controls_layout,
    c("plate_id", "plate_num", "sample_type", "sample", "sample_code",
      "numero_labo", "code_barres_kps", "well_id")
  )

  # Harmonize columns - ensure they're character type
  # (Columns now created in mutate above, just need type conversion)
  samples_layout <- samples_layout %>%
    mutate(
      numero_labo = as.character(numero_labo),
      code_barres_kps = as.character(code_barres_kps),
      sample_code = as.character(sample_code)
    )

  controls_layout <- controls_layout %>%
    mutate(
      numero_labo = as.character(numero_labo),
      code_barres_kps = as.character(code_barres_kps),
      sample_code = as.character(sample_code)
    )
  
  # 3) Join with OD
  # Note: Each well_id can have multiple OD records (Ag_plus and Ag0), so use many-to-many
  sample_wells <- samples_layout %>%
    left_join(od_grid, by = c("plate_num", "well_id"), relationship = "many-to-many")

  control_wells <- controls_layout %>%
    left_join(od_grid, by = c("plate_num", "well_id"), relationship = "many-to-many")

  wells_long <- bind_rows(sample_wells, control_wells)

  wells_long <- ensure_columns(
    wells_long,
    c(
      "plate_id", "plate_num", "sample_type", "sample", "sample_code",
      "numero_labo", "code_barres_kps"
    )
  )
  
  # 4) Summarize by sample/control
  # Note: group_modify passes group keys in .y, not in .x
  summarize_entry <- function(df_group, group_keys) {
    plus <- df_group$od[df_group$ag_state == "Ag_plus"]
    zero <- df_group$od[df_group$ag_state == "Ag0"]

    if (length(plus) < 2) plus <- c(plus, rep(NA_real_, 2 - length(plus)))
    if (length(zero) < 2) zero <- c(zero, rep(NA_real_, 2 - length(zero)))

    Ag_plus_1 <- plus[1]
    Ag_plus_2 <- plus[2]
    Ag0_1 <- zero[1]
    Ag0_2 <- zero[2]

    mean_Ag_plus <- mean(plus, na.rm = TRUE)
    mean_Ag0 <- mean(zero, na.rm = TRUE)
    DOD <- mean_Ag_plus - mean_Ag0

    cv_Ag_plus <- calculate_cv(Ag_plus_1, Ag_plus_2)
    cv_Ag0 <- calculate_cv(Ag0_1, Ag0_2)

    qc_Ag_plus <- if(is.na(cv_Ag_plus)) NA else cv_Ag_plus <= cv_max_ag_plus
    qc_Ag0 <- if(is.na(cv_Ag0)) NA else cv_Ag0 <= cv_max_ag0
    qc_overall <- if(is.na(qc_Ag_plus) || is.na(qc_Ag0)) NA else (qc_Ag_plus & qc_Ag0)

    # NOTE: Do NOT include grouping variables (plate_id, plate_num, sample_type,
    # sample, sample_code, numero_labo, code_barres_kps) here.
    # group_modify() automatically adds them back.
    tibble(
      plate_date = plate_date,
      elisa_type = elisa_type,
      source_path = path,  # Track source file path for inference
      Ag_plus_1 = Ag_plus_1,
      Ag_plus_2 = Ag_plus_2,
      Ag0_1 = Ag0_1,
      Ag0_2 = Ag0_2,
      mean_Ag_plus = mean_Ag_plus,
      mean_Ag0 = mean_Ag0,
      DOD = DOD,
      cv_Ag_plus = cv_Ag_plus,
      cv_Ag0 = cv_Ag0,
      qc_Ag_plus = qc_Ag_plus,
      qc_Ag0 = qc_Ag0,
      qc_overall = qc_overall
    )
  }

  # NOTE: 'sample' column contains PLATE POSITION (e.g., S1, S2, PC1, NC1), not sample ID
  # Actual sample identifiers are in 'numero_labo' (lab ID) and 'code_barres_kps' (barcode)
  # We group by sample identifiers to aggregate across replicates (multiple wells per sample)
  df_summary <- wells_long %>%
    group_by(plate_id, plate_num, sample_type, sample, sample_code,
             numero_labo, code_barres_kps) %>%
    group_modify(~ summarize_entry(.x, .y)) %>%
    ungroup()
  
  # 5) Calculate PP% using PC and NC per plate
  df_summary <- df_summary %>%
    group_by(plate_id, plate_num) %>%
    mutate(
      PC_DOD = mean(DOD[sample_type == "control" &
                          (grepl("^PC", sample) | sample_code == "CP")], na.rm = TRUE),
      NC_DOD = mean(DOD[sample_type == "control" &
                          (grepl("^NC", sample) | sample_code == "CN")], na.rm = TRUE),
      PP_fraction = (DOD - NC_DOD) / (PC_DOD - NC_DOD),
      PP_percent = PP_fraction * 100,
      positive_control_od = mean(mean_Ag_plus[sample_type == "control" &
                                                (grepl("^PC", sample) | sample_code == "CP")],
                                 na.rm = TRUE),
      negative_control_od = mean(mean_Ag_plus[sample_type == "control" &
                                                (grepl("^NC", sample) | sample_code == "CN")],
                                 na.rm = TRUE),
      positive_control_od = ifelse(is.nan(positive_control_od), NA_real_, positive_control_od),
      negative_control_od = ifelse(is.nan(negative_control_od), NA_real_, negative_control_od),
      plate_positive_control_valid = ifelse(is.na(positive_control_od), NA,
                                            positive_control_od >= 0.5 & positive_control_od <= 1.5),
      plate_negative_control_valid = ifelse(is.na(negative_control_od), NA,
                                            negative_control_od < 0.5),
      plate_valid = plate_positive_control_valid & plate_negative_control_valid
    ) %>%
    ungroup() %>%
    arrange(plate_num, sample_type, sample)
  
  return(df_summary)
}

# -----------------------------------------------------------------------------
# Parse folder
# -----------------------------------------------------------------------------
parse_indirect_elisa_folder <- function(dir,
                                        pattern = "\\.xlsx$",
                                        exclude_pattern = NULL,
                                        recursive = FALSE,
                                        cv_max_ag_plus = 20,
                                        cv_max_ag0 = 20) {
  dir <- unlist(dir)
  
  files <- unlist(lapply(dir, function(path_dir) {
    if (!dir.exists(path_dir)) {
      warning("Directory does not exist: ", path_dir)
      return(character(0))
    }
    list.files(
      path = path_dir,
      pattern = pattern,
      full.names = TRUE,
      recursive = recursive
    )
  }))
  
  if (!is.null(exclude_pattern)) {
    files <- files[!grepl(exclude_pattern, basename(files))]
  }
  
  if (length(files) == 0) {
    stop("No Excel files found in: ", paste(dir, collapse = ", "))
  }
  
  message("Processing ", length(files), " file(s).")
  
  res_list <- lapply(files, function(f) {
    tryCatch(
      extract_elisa_plate_summary(f, cv_max_ag_plus = cv_max_ag_plus, cv_max_ag0 = cv_max_ag0),
      error = function(e) {
        warning("Failed on file: ", f, " — ", conditionMessage(e))
        NULL
      }
    )
  })

  df_all <- bind_rows(Filter(Negate(is.null), res_list))

  if (!nrow(df_all)) {
    return(tibble(
      plate_id = character(),
      plate_num = integer(),
      plate_date = as.Date(character()),
      elisa_type = character(),
      sample_type = character(),
      sample = character(),
      sample_code = character(),
      numero_labo = character(),
      code_barres_kps = character(),
      Ag_plus_1 = double(),
      Ag_plus_2 = double(),
      Ag0_1 = double(),
      Ag0_2 = double(),
      mean_Ag_plus = double(),
      mean_Ag0 = double(),
      DOD = double(),
      cv_Ag_plus = double(),
      cv_Ag0 = double(),
      qc_Ag_plus = logical(),
      qc_Ag0 = logical(),
      qc_overall = logical()
    ))
  }

  # CRITICAL: Always infer elisa_type from source_path (source of truth)
  # The plate format detection may misclassify VSG files as PE if they lack
  # "Plaque" markers, so we use the source directory as the authoritative source
  if ("source_path" %in% names(df_all)) {
    message("DEBUG: Applying source_path-based elisa_type classification")
    message("DEBUG: Sample source_paths: ", paste(head(unique(df_all$source_path), 3), collapse = ", "))

    # Show elisa_type BEFORE classification
    message("DEBUG: elisa_type BEFORE source_path classification: ",
            paste(unique(df_all$elisa_type), collapse = ", "))

    df_all <- df_all %>%
      mutate(
        elisa_type = case_when(
          grepl("elisa_vsg|/vsg/", source_path, ignore.case = TRUE) ~ "ELISA_vsg",
          grepl("elisa_pe|/pe/", source_path, ignore.case = TRUE) ~ "ELISA_pe",
          TRUE ~ "ELISA_pe"  # Default to PE if path doesn't indicate type
        )
      )

    # Show elisa_type AFTER classification (before removing source_path)
    message("DEBUG: elisa_type AFTER source_path classification: ",
            paste(unique(df_all$elisa_type), collapse = ", "))

    df_all <- df_all %>% select(-source_path)  # Remove source_path after using it for inference
    message("DEBUG: source_path classification complete")
  } else {
    message("WARNING: source_path column not found, using fallback classification")
    # Fallback if source_path not available (shouldn't happen with current code)
    df_all <- df_all %>%
      mutate(
        elisa_type = if_else(is.na(elisa_type) | elisa_type == "", "ELISA_pe", elisa_type)
      )
  }

  # Report elisa_type distribution
  type_counts <- table(df_all$elisa_type)
  message("✓ ELISA type distribution: ", paste(names(type_counts), "=", type_counts, collapse = ", "))

  # Assign unique plate number across all files
  plate_index <- df_all %>%
    distinct(plate_id, plate_num, plate_date, elisa_type) %>%
    arrange(plate_date, plate_id, plate_num) %>%
    mutate(plate_number = row_number())

  df_all <- df_all %>%
    left_join(plate_index, by = c("plate_id", "plate_num", "plate_date", "elisa_type"), relationship = "many-to-one") %>%
    arrange(plate_number, sample_type, sample)

  return(df_all)
}

# =============================================================================
# USAGE EXAMPLE (commented out to prevent execution when sourced)
# =============================================================================

# To use this parser interactively, uncomment and run the following code:
#
# # Process folder (single or multiple directories)
# elisa_dirs <- c("data/ELISA_pe", "data/ELISA_vsg")
#
# res <- parse_indirect_elisa_folder(
#   elisa_dirs,
#   exclude_pattern = "^251021 Résultats indirect ELISA vF\\.5",
#   recursive = TRUE
# )
#
# View(res)
# write.csv(res, "all_elisa_results.csv", row.names = FALSE)
#
# # Summary
# cat("\n=== Summary ===\n")
# cat(sprintf("Total plates: %d\n", n_distinct(res$plate_number)))
# cat(sprintf("Total samples: %d\n", sum(res$sample_type == "sample")))
# cat(sprintf("QC pass rate: %.1f%%\n",
#             100 * mean(res$qc_overall[res$sample_type == "sample"], na.rm = TRUE)))
