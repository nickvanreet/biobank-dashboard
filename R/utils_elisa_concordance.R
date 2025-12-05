# ELISA Concordance Analysis Utilities
# Functions for matching samples across PE and VSG ELISA tests
# and calculating concordance metrics

#' Normalize identifier for ELISA sample matching
#' More comprehensive than the global normalize_id function
#' Preserves dashes, underscores, and dots to prevent sample ID collisions
#' @param x Character vector of identifiers
#' @return Normalized character vector
normalize_elisa_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  # Remove KPS prefix (case-insensitive)
  x <- gsub("^kps[-_]?", "", x)
  # Only remove spaces and special punctuation, but keep dashes, underscores, dots
  # This prevents "001-A" and "001-B" from collapsing to the same ID
  x <- gsub("[^a-z0-9._-]", "", x)
  # Treat various forms of NA as missing
  x[x %in% c("", "na", "n/a", "null")] <- NA_character_
  x
}

#' Match samples between PE and VSG ELISA tests
#' @param pe_data Data frame with PE ELISA results (samples only)
#' @param vsg_data Data frame with VSG ELISA results (samples only)
#' @return Data frame with matched samples from both tests
match_elisa_samples <- function(pe_data, vsg_data) {

  # Ensure we're only working with samples (not controls)
  pe_samples <- pe_data %>%
    filter(sample_type == "sample") %>%
    mutate(
      barcode_norm = normalize_elisa_id(code_barres_kps),
      # NOTE: 'sample' column contains PLATE POSITION, not sample ID
      # Only use numero_labo or sample_code for matching
      numero_norm = normalize_elisa_id(coalesce(numero_labo, sample_code))
    ) %>%
    select(
      # Identifiers
      pe_sample = sample,
      pe_barcode = code_barres_kps,
      pe_numero = numero_labo,
      barcode_norm,
      numero_norm,
      # PE Results
      pe_plate_id = plate_id,
      pe_plate_date = plate_date,
      pe_DOD = DOD,
      pe_PP_percent = PP_percent,
      pe_qc_overall = qc_overall,
      pe_plate_valid = plate_valid,
      # Biobank data (if available)
      pe_Province = Province,
      pe_HealthZone = HealthZone,
      pe_Structure = Structure,
      pe_Sex = Sex,
      pe_Age = Age,
      pe_AgeGroup = AgeGroup,
      pe_SampleDate = SampleDate,
      pe_Cohort = Cohort,
      pe_BiobankMatched = BiobankMatched
    )

  vsg_samples <- vsg_data %>%
    filter(sample_type == "sample") %>%
    mutate(
      barcode_norm = normalize_elisa_id(code_barres_kps),
      # NOTE: 'sample' column contains PLATE POSITION, not sample ID
      # Only use numero_labo or sample_code for matching
      numero_norm = normalize_elisa_id(coalesce(numero_labo, sample_code))
    ) %>%
    select(
      # Identifiers
      vsg_sample = sample,
      vsg_barcode = code_barres_kps,
      vsg_numero = numero_labo,
      barcode_norm,
      numero_norm,
      # VSG Results
      vsg_plate_id = plate_id,
      vsg_plate_date = plate_date,
      vsg_DOD = DOD,
      vsg_PP_percent = PP_percent,
      vsg_qc_overall = qc_overall,
      vsg_plate_valid = plate_valid,
      # Biobank data (if available)
      vsg_Province = Province,
      vsg_HealthZone = HealthZone,
      vsg_Structure = Structure,
      vsg_Sex = Sex,
      vsg_Age = Age,
      vsg_AgeGroup = AgeGroup,
      vsg_SampleDate = SampleDate,
      vsg_Cohort = Cohort,
      vsg_BiobankMatched = BiobankMatched
    )

  # First try matching by barcode
  matched_by_barcode <- pe_samples %>%
    filter(!is.na(barcode_norm)) %>%
    inner_join(
      vsg_samples %>% filter(!is.na(barcode_norm)),
      by = "barcode_norm",
      suffix = c("_pe", "_vsg"),
      relationship = "many-to-many"  # Same sample may be tested multiple times
    ) %>%
    mutate(match_method = "barcode")

  # Then match remaining by numero_labo
  unmatched_pe <- pe_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  unmatched_vsg <- vsg_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  matched_by_numero <- unmatched_pe %>%
    inner_join(
      unmatched_vsg,
      by = "numero_norm",
      suffix = c("_pe", "_vsg"),
      relationship = "many-to-many"  # Same sample may be tested multiple times
    ) %>%
    mutate(match_method = "numero_labo")

  # Combine all matches
  all_matches <- bind_rows(
    matched_by_barcode,
    matched_by_numero
  )

  # Clean up duplicate column names and select final columns
  all_matches <- all_matches %>%
    mutate(
      # Use PE biobank data as primary, fill with VSG if missing
      Province = coalesce(pe_Province, vsg_Province),
      HealthZone = coalesce(pe_HealthZone, vsg_HealthZone),
      Structure = coalesce(pe_Structure, vsg_Structure),
      Sex = coalesce(pe_Sex, vsg_Sex),
      Age = coalesce(pe_Age, vsg_Age),
      AgeGroup = coalesce(pe_AgeGroup, vsg_AgeGroup),
      SampleDate = coalesce(pe_SampleDate, vsg_SampleDate),
      Cohort = coalesce(pe_Cohort, vsg_Cohort),
      BiobankMatched = pe_BiobankMatched | vsg_BiobankMatched
    ) %>%
    select(
      # Identifiers
      pe_sample, vsg_sample,
      pe_barcode, vsg_barcode,
      pe_numero, vsg_numero,
      match_method,
      barcode_norm, numero_norm,
      # PE Results
      pe_plate_id, pe_plate_date,
      pe_DOD, pe_PP_percent,
      pe_qc_overall, pe_plate_valid,
      # VSG Results
      vsg_plate_id, vsg_plate_date,
      vsg_DOD, vsg_PP_percent,
      vsg_qc_overall, vsg_plate_valid,
      # Biobank data
      Province, HealthZone, Structure,
      Sex, Age, AgeGroup, SampleDate, Cohort,
      BiobankMatched
    )

  # Add screening numbers for PE tests (rank by date for each unique sample)
  # Use the matching identifier (barcode or numero) to group samples
  all_matches <- all_matches %>%
    mutate(
      sample_id = coalesce(barcode_norm, numero_norm)
    ) %>%
    group_by(sample_id) %>%
    arrange(pe_plate_date, .by_group = TRUE) %>%
    mutate(
      pe_screening_num = dense_rank(pe_plate_date),
      pe_is_first_screening = (pe_screening_num == 1)
    ) %>%
    arrange(vsg_plate_date, .by_group = TRUE) %>%
    mutate(
      vsg_screening_num = dense_rank(vsg_plate_date),
      vsg_is_first_screening = (vsg_screening_num == 1)
    ) %>%
    ungroup() %>%
    arrange(sample_id, pe_plate_date, vsg_plate_date) %>%
    select(-sample_id)  # Remove temporary column

  return(all_matches)
}

#' Calculate sample positivity based on thresholds
#' @param pp_percent Percent positivity value
#' @param dod Difference of OD value
#' @param pp_cutoff PP% cutoff (default 20)
#' @param dod_cutoff DOD cutoff (default 0.3)
#' @return Logical vector indicating positivity
calculate_positivity <- function(pp_percent, dod, pp_cutoff = 20, dod_cutoff = 0.3) {
  pp_clean <- dplyr::coalesce(pp_percent, -Inf)
  dod_clean <- dplyr::coalesce(dod, -Inf)

  (pp_clean >= pp_cutoff) | (dod_clean >= dod_cutoff)
}

#' Calculate concordance metrics
#' @param matched_data Data frame with matched PE and VSG samples
#' @param pe_pp_cutoff PE percent positivity cutoff
#' @param pe_dod_cutoff PE DOD cutoff
#' @param vsg_pp_cutoff VSG percent positivity cutoff
#' @param vsg_dod_cutoff VSG DOD cutoff
#' @return List with concordance metrics and classified data
calculate_concordance <- function(matched_data,
                                  pe_pp_cutoff = 20,
                                  pe_dod_cutoff = 0.3,
                                  vsg_pp_cutoff = 20,
                                  vsg_dod_cutoff = 0.3) {

  if (nrow(matched_data) == 0) {
    return(list(
      data = tibble(),
      metrics = list(
        n_total = 0,
        n_unique_samples = 0,
        n_first_screenings = 0,
        n_repeat_tests = 0,
        n_concordant = 0,
        n_discordant = 0,
        n_both_positive = 0,
        n_both_negative = 0,
        n_pe_only_positive = 0,
        n_vsg_only_positive = 0,
        pct_concordant = NA,
        pct_discordant = NA,
        pct_copositivity = NA,
        kappa = NA,
        sensitivity = NA,
        specificity = NA,
        ppv = NA,
        npv = NA
      ),
      confusion_matrix = tibble(
        pe_result = c("Positive", "Negative"),
        vsg_positive = c(0, 0),
        vsg_negative = c(0, 0)
      )
    ))
  }

  # Classify samples
  classified <- matched_data %>%
    mutate(
      pe_positive = calculate_positivity(pe_PP_percent, pe_DOD, pe_pp_cutoff, pe_dod_cutoff),
      vsg_positive = calculate_positivity(vsg_PP_percent, vsg_DOD, vsg_pp_cutoff, vsg_dod_cutoff),

      # Concordance classification
      concordance_type = case_when(
        pe_positive & vsg_positive ~ "Both Positive",
        !pe_positive & !vsg_positive ~ "Both Negative",
        pe_positive & !vsg_positive ~ "PE+ / VSG-",
        !pe_positive & vsg_positive ~ "PE- / VSG+"
      ),

      is_concordant = (pe_positive == vsg_positive),
      is_discordant = !is_concordant
    )

  # Calculate metrics
  n_total <- nrow(classified)
  n_both_positive <- sum(classified$pe_positive & classified$vsg_positive, na.rm = TRUE)
  n_both_negative <- sum(!classified$pe_positive & !classified$vsg_positive, na.rm = TRUE)
  n_pe_only_positive <- sum(classified$pe_positive & !classified$vsg_positive, na.rm = TRUE)
  n_vsg_only_positive <- sum(!classified$pe_positive & classified$vsg_positive, na.rm = TRUE)
  n_concordant <- n_both_positive + n_both_negative
  n_discordant <- n_pe_only_positive + n_vsg_only_positive

  # Calculate unique sample counts
  n_unique_samples <- classified %>%
    mutate(sample_id = coalesce(
      normalize_elisa_id(pe_barcode),
      normalize_elisa_id(vsg_barcode),
      normalize_elisa_id(pe_numero),
      normalize_elisa_id(vsg_numero)
    )) %>%
    filter(!is.na(sample_id)) %>%
    distinct(sample_id) %>%
    nrow()

  n_first_screenings <- sum(classified$pe_is_first_screening & classified$vsg_is_first_screening, na.rm = TRUE)
  n_repeat_tests <- n_total - n_first_screenings

  # Percentages
  pct_concordant <- (n_concordant / n_total) * 100
  pct_discordant <- (n_discordant / n_total) * 100
  pct_copositivity <- (n_both_positive / n_total) * 100

  # Cohen's Kappa
  po <- n_concordant / n_total  # Observed agreement
  pe_positive_total <- sum(classified$pe_positive, na.rm = TRUE)
  vsg_positive_total <- sum(classified$vsg_positive, na.rm = TRUE)
  pe_negative_total <- n_total - pe_positive_total
  vsg_negative_total <- n_total - vsg_positive_total

  pe_expected <- ((pe_positive_total * vsg_positive_total) +
                   (pe_negative_total * vsg_negative_total)) / (n_total^2)

  kappa <- if (pe_expected < 1) {
    (po - pe_expected) / (1 - pe_expected)
  } else {
    NA_real_
  }

  # Sensitivity, Specificity, PPV, NPV (using PE as reference)
  sensitivity <- if (pe_positive_total > 0) {
    n_both_positive / pe_positive_total
  } else {
    NA_real_
  }

  specificity <- if (pe_negative_total > 0) {
    n_both_negative / pe_negative_total
  } else {
    NA_real_
  }

  ppv <- if (vsg_positive_total > 0) {
    n_both_positive / vsg_positive_total
  } else {
    NA_real_
  }

  npv <- if (vsg_negative_total > 0) {
    n_both_negative / vsg_negative_total
  } else {
    NA_real_
  }

  # Create confusion matrix
  confusion_matrix <- tibble(
    pe_result = c("Positive", "Negative"),
    vsg_positive = c(n_both_positive, n_vsg_only_positive),
    vsg_negative = c(n_pe_only_positive, n_both_negative)
  )

  # Return results
  list(
    data = classified,
    metrics = list(
      n_total = n_total,
      n_unique_samples = n_unique_samples,
      n_first_screenings = n_first_screenings,
      n_repeat_tests = n_repeat_tests,
      n_concordant = n_concordant,
      n_discordant = n_discordant,
      n_both_positive = n_both_positive,
      n_both_negative = n_both_negative,
      n_pe_only_positive = n_pe_only_positive,
      n_vsg_only_positive = n_vsg_only_positive,
      pct_concordant = pct_concordant,
      pct_discordant = pct_discordant,
      pct_copositivity = pct_copositivity,
      kappa = kappa,
      sensitivity = sensitivity,
      specificity = specificity,
      ppv = ppv,
      npv = npv
    ),
    confusion_matrix = confusion_matrix
  )
}

#' Format percentage with 1 decimal place
#' @param x Numeric value (0-100)
#' @return Formatted string with % sign
format_pct <- function(x) {
  if (is.na(x)) return("N/A")
  sprintf("%.1f%%", x)
}

#' Format count with percentage in parentheses
#' @param count Integer count
#' @param total Integer total
#' @return Formatted string "count (pct%)"
format_count_pct <- function(count, total) {
  if (total == 0) return("0 (0%)")
  pct <- (count / total) * 100
  sprintf("%d (%.1f%%)", count, pct)
}

#' Format Cohen's Kappa with interpretation
#' @param kappa Numeric kappa value (-1 to 1)
#' @return Formatted string with interpretation
format_kappa <- function(kappa) {
  if (is.na(kappa)) return("N/A")

  interpretation <- case_when(
    kappa < 0 ~ "Poor",
    kappa < 0.20 ~ "Slight",
    kappa < 0.40 ~ "Fair",
    kappa < 0.60 ~ "Moderate",
    kappa < 0.80 ~ "Substantial",
    TRUE ~ "Almost Perfect"
  )

  sprintf("%.3f (%s)", kappa, interpretation)
}

#' Match iELISA samples with ELISA samples
#' @param ielisa_data Data frame with iELISA results
#' @param elisa_data Data frame with ELISA results (PE or VSG)
#' @param ielisa_antigen Antigen type ("L13" or "L15")
#' @param elisa_type ELISA type ("PE" or "VSG")
#' @return Data frame with matched samples in concordance format
match_ielisa_elisa <- function(ielisa_data, elisa_data, ielisa_antigen, elisa_type) {

  # Prepare iELISA data
  # Note: iELISA data typically doesn't include controls, but filter just in case
  ielisa_cols <- names(ielisa_data)
  has_is_control <- "is_control" %in% ielisa_cols
  has_sample_type <- "sample_type" %in% ielisa_cols
  has_labid <- "LabID" %in% ielisa_cols
  has_barcode <- "Barcode" %in% ielisa_cols
  has_code_barres <- "code_barres_kps" %in% ielisa_cols
  has_numero_labo <- "numero_labo" %in% ielisa_cols

  # Filter to samples
  ielisa_samples <- ielisa_data
  if (has_is_control) {
    ielisa_samples <- ielisa_samples %>% filter(!is_control)
  }
  if (has_sample_type) {
    ielisa_samples <- ielisa_samples %>% filter(sample_type == "sample")
  }

  # Add missing columns with NA to avoid evaluation errors
  if (!has_code_barres) ielisa_samples$code_barres_kps <- NA_character_
  if (!has_barcode) ielisa_samples$Barcode <- NA_character_
  if (!has_numero_labo) ielisa_samples$numero_labo <- NA_character_
  if (!has_labid) ielisa_samples$LabID <- NA_character_

  # Normalize identifiers and select columns
  ielisa_samples <- ielisa_samples %>%
    mutate(
      # Use coalesce to pick first non-NA value
      barcode_norm = normalize_elisa_id(coalesce(code_barres_kps, Barcode)),
      numero_norm = normalize_elisa_id(coalesce(numero_labo, LabID)),
      ielisa_labid = coalesce(LabID, numero_labo),
      ielisa_barcode = coalesce(Barcode, code_barres_kps)
    ) %>%
    select(
      ielisa_labid, ielisa_barcode,
      barcode_norm, numero_norm,
      # iELISA Results
      ielisa_plate_date = plate_date,
      ielisa_file = file,
      ielisa_pct_inh_13 = pct_inh_f2_13,
      ielisa_pct_inh_15 = pct_inh_f2_15,
      ielisa_positive_13 = positive_L13,
      ielisa_positive_15 = positive_L15,
      ielisa_plate_valid_13 = plate_valid_L13,
      ielisa_plate_valid_15 = plate_valid_L15
    )

  # Prepare ELISA data
  elisa_samples <- elisa_data %>%
    filter(sample_type == "sample") %>%
    mutate(
      barcode_norm = normalize_elisa_id(code_barres_kps),
      numero_norm = normalize_elisa_id(coalesce(numero_labo, sample_code))
    ) %>%
    select(
      # Identifiers
      elisa_sample = sample,
      elisa_barcode = code_barres_kps,
      elisa_numero = numero_labo,
      barcode_norm,
      numero_norm,
      # ELISA Results
      elisa_plate_id = plate_id,
      elisa_plate_date = plate_date,
      elisa_DOD = DOD,
      elisa_PP_percent = PP_percent,
      elisa_qc_overall = qc_overall,
      elisa_plate_valid = plate_valid,
      # Biobank data (if available)
      Province,
      HealthZone,
      Structure,
      Sex,
      Age
    )

  # Debug: Log sample preparation
  message(sprintf("iELISA samples prepared: %s rows", nrow(ielisa_samples)))
  message(sprintf("ELISA samples prepared: %s rows", nrow(elisa_samples)))
  message(sprintf("iELISA with valid barcode: %s", sum(!is.na(ielisa_samples$barcode_norm))))
  message(sprintf("ELISA with valid barcode: %s", sum(!is.na(elisa_samples$barcode_norm))))
  message(sprintf("iELISA with valid numero: %s", sum(!is.na(ielisa_samples$numero_norm))))
  message(sprintf("ELISA with valid numero: %s", sum(!is.na(elisa_samples$numero_norm))))

  # Match by barcode
  matched_by_barcode <- ielisa_samples %>%
    filter(!is.na(barcode_norm)) %>%
    inner_join(
      elisa_samples %>% filter(!is.na(barcode_norm)),
      by = "barcode_norm",
      suffix = c("_ielisa", "_elisa"),
      relationship = "many-to-many"
    ) %>%
    mutate(match_method = "barcode")

  message(sprintf("Matches by barcode: %s", nrow(matched_by_barcode)))

  # Match remaining by numero
  unmatched_ielisa <- ielisa_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  unmatched_elisa <- elisa_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  matched_by_numero <- unmatched_ielisa %>%
    inner_join(
      unmatched_elisa,
      by = "numero_norm",
      suffix = c("_ielisa", "_elisa"),
      relationship = "many-to-many"
    ) %>%
    mutate(match_method = "numero_labo")

  message(sprintf("Matches by numero: %s", nrow(matched_by_numero)))

  # Combine matches
  all_matches <- bind_rows(
    matched_by_barcode,
    matched_by_numero
  )

  message(sprintf("Total matches before filtering: %s", nrow(all_matches)))

  # Create concordance data format based on antigen type
  if (ielisa_antigen == "L13") {
    result <- all_matches %>%
      mutate(
        test1_value = ielisa_pct_inh_13,
        test1_binary = ielisa_positive_13,
        test1_name = "iELISA-L13"
      )
  } else {  # L15
    result <- all_matches %>%
      mutate(
        test1_value = ielisa_pct_inh_15,
        test1_binary = ielisa_positive_15,
        test1_name = "iELISA-L15"
      )
  }

  # Add ELISA results
  result <- result %>%
    mutate(
      test2_value = elisa_PP_percent,
      test2_binary = calculate_positivity(elisa_PP_percent, elisa_DOD),
      test2_name = paste0("ELISA-", elisa_type),
      health_zone = HealthZone,
      province = Province,
      date = coalesce(ielisa_plate_date, elisa_plate_date)
    ) %>%
    select(
      test1_value, test2_value,
      test1_binary, test2_binary,
      test1_name, test2_name,
      health_zone, province, date,
      match_method,
      ielisa_labid, ielisa_barcode,
      elisa_sample, elisa_barcode, elisa_numero,
      Sex, Age, Structure
    )

  return(result)
}

#' Match MIC qPCR samples with ELISA samples
#' @param mic_data Data frame with MIC qPCR results
#' @param elisa_data Data frame with ELISA results (PE or VSG)
#' @param elisa_type ELISA type ("PE" or "VSG")
#' @return Data frame with matched samples in concordance format
match_mic_elisa <- function(mic_data, elisa_data, elisa_type) {

  # Check if mic_data is null or empty
  if (is.null(mic_data) || nrow(mic_data) == 0) {
    return(tibble::tibble(
      test1_value = numeric(),
      test2_value = numeric(),
      test1_binary = logical(),
      test2_binary = logical(),
      test1_name = character(),
      test2_name = character()
    ))
  }

  # Prepare MIC data
  # Note: MIC data may have different column names depending on version
  # Try multiple possible column names for each field

  # Determine which columns exist
  mic_cols <- names(mic_data)
  has_control_type <- "ControlType" %in% mic_cols
  has_is_control <- "is_control" %in% mic_cols
  has_sample_name <- "SampleName" %in% mic_cols
  has_name <- "Name" %in% mic_cols
  has_sample_id <- "SampleID" %in% mic_cols
  has_final_call <- "FinalCall" %in% mic_cols
  has_confidence <- "ConfidenceScore" %in% mic_cols
  has_run_id <- "RunID" %in% mic_cols
  has_run_date <- "RunDate" %in% mic_cols
  has_plate_date <- "PlateDate" %in% mic_cols
  has_date <- "Date" %in% mic_cols

  # Filter to samples only
  if (has_control_type) {
    mic_samples <- mic_data %>% filter(ControlType == "Sample")
  } else if (has_is_control) {
    mic_samples <- mic_data %>% filter(!is_control)
  } else {
    mic_samples <- mic_data
  }

  # Add missing columns with NA to avoid evaluation errors
  if (!has_sample_name) mic_samples$SampleName <- NA_character_
  if (!has_name) mic_samples$Name <- NA_character_
  if (!has_sample_id) mic_samples$SampleID <- NA_character_
  if (!has_final_call) mic_samples$FinalCall <- NA_character_
  if (!has_confidence) mic_samples$ConfidenceScore <- NA_character_
  if (!has_run_id) mic_samples$RunID <- NA_character_
  if (!has_run_date) mic_samples$RunDate <- NA
  if (!has_plate_date) mic_samples$PlateDate <- NA
  if (!has_date) mic_samples$Date <- NA
  if (!"Province" %in% mic_cols) mic_samples$Province <- NA_character_
  if (!"HealthZone" %in% mic_cols) mic_samples$HealthZone <- NA_character_
  if (!"Structure" %in% mic_cols) mic_samples$Structure <- NA_character_
  if (!"Sex" %in% mic_cols) mic_samples$Sex <- NA_character_
  if (!"Age" %in% mic_cols) mic_samples$Age <- NA_real_

  # Normalize identifiers
  mic_samples <- mic_samples %>%
    mutate(
      barcode_norm = normalize_elisa_id(coalesce(SampleName, Name)),
      numero_norm = normalize_elisa_id(SampleID)
    )

  # Select and rename columns
  mic_samples <- mic_samples %>%
    mutate(
      mic_sample_name = coalesce(SampleName, Name),
      mic_barcode = coalesce(SampleName, Name),
      mic_test_number = SampleID,
      mic_final_call = FinalCall,
      mic_confidence = ConfidenceScore,
      mic_run_id = RunID,
      mic_date = coalesce(RunDate, PlateDate, Date)
    ) %>%
    select(
      mic_sample_name, mic_barcode, mic_test_number,
      barcode_norm, numero_norm,
      mic_final_call, mic_confidence, mic_run_id, mic_date,
      Province, HealthZone, Structure, Sex, Age
    )

  # Prepare ELISA data
  elisa_samples <- elisa_data %>%
    filter(sample_type == "sample") %>%
    mutate(
      barcode_norm = normalize_elisa_id(code_barres_kps),
      numero_norm = normalize_elisa_id(coalesce(numero_labo, sample_code))
    ) %>%
    select(
      # Identifiers
      elisa_sample = sample,
      elisa_barcode = code_barres_kps,
      elisa_numero = numero_labo,
      barcode_norm,
      numero_norm,
      # ELISA Results
      elisa_plate_id = plate_id,
      elisa_plate_date = plate_date,
      elisa_DOD = DOD,
      elisa_PP_percent = PP_percent,
      elisa_qc_overall = qc_overall,
      elisa_plate_valid = plate_valid,
      # Biobank data (if available)
      elisa_Province = Province,
      elisa_HealthZone = HealthZone,
      elisa_Structure = Structure,
      elisa_Sex = Sex,
      elisa_Age = Age
    )

  # Debug: Log sample preparation
  message(sprintf("MIC samples prepared: %s rows", nrow(mic_samples)))
  message(sprintf("ELISA samples prepared: %s rows", nrow(elisa_samples)))
  message(sprintf("MIC with valid barcode: %s", sum(!is.na(mic_samples$barcode_norm))))
  message(sprintf("ELISA with valid barcode: %s", sum(!is.na(elisa_samples$barcode_norm))))
  message(sprintf("MIC with valid numero: %s", sum(!is.na(mic_samples$numero_norm))))
  message(sprintf("ELISA with valid numero: %s", sum(!is.na(elisa_samples$numero_norm))))

  # Match by barcode
  matched_by_barcode <- mic_samples %>%
    filter(!is.na(barcode_norm)) %>%
    inner_join(
      elisa_samples %>% filter(!is.na(barcode_norm)),
      by = "barcode_norm",
      suffix = c("_mic", "_elisa"),
      relationship = "many-to-many"
    ) %>%
    mutate(match_method = "barcode")

  message(sprintf("Matches by barcode: %s", nrow(matched_by_barcode)))

  # Match remaining by numero
  unmatched_mic <- mic_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  unmatched_elisa <- elisa_samples %>%
    filter(!barcode_norm %in% matched_by_barcode$barcode_norm | is.na(barcode_norm)) %>%
    filter(!is.na(numero_norm))

  matched_by_numero <- unmatched_mic %>%
    inner_join(
      unmatched_elisa,
      by = "numero_norm",
      suffix = c("_mic", "_elisa"),
      relationship = "many-to-many"
    ) %>%
    mutate(match_method = "numero_labo")

  message(sprintf("Matches by numero: %s", nrow(matched_by_numero)))

  # Combine matches
  all_matches <- bind_rows(
    matched_by_barcode,
    matched_by_numero
  )

  message(sprintf("Total matches before filtering: %s", nrow(all_matches)))

  if (nrow(all_matches) == 0) {
    return(tibble::tibble(
      test1_value = numeric(),
      test2_value = numeric(),
      test1_binary = logical(),
      test2_binary = logical(),
      test1_name = character(),
      test2_name = character()
    ))
  }

  # Create concordance data format
  # MIC uses FinalCall: "Positive", "Positive_DNA", "Positive_RNA", "LatePositive",
  # "Negative", "Invalid_NoDNA", "Indeterminate", etc.
  # Convert confidence score to a numeric value (0-100)
  result <- all_matches %>%
    mutate(
      # MIC test1: Use confidence score as continuous value, FinalCall for binary
      # Handle confidence score (may be NA or may have % sign)
      test1_value = suppressWarnings(as.numeric(gsub("%", "", as.character(mic_confidence)))),
      # Consider any result starting with "Positive" as positive (including Positive_DNA, Positive_RNA, LatePositive)
      test1_binary = grepl("^Positive", mic_final_call, ignore.case = TRUE),
      test1_name = "MIC",
      # ELISA test2
      test2_value = elisa_PP_percent,
      test2_binary = calculate_positivity(elisa_PP_percent, elisa_DOD),
      test2_name = paste0("ELISA-", elisa_type),
      # Geographic data (prefer ELISA biobank match, fall back to MIC)
      health_zone = coalesce(elisa_HealthZone, HealthZone),
      province = coalesce(elisa_Province, Province),
      date = coalesce(as.Date(mic_date), elisa_plate_date),
      # Demographic data (prefer ELISA biobank match, fall back to MIC)
      Sex = coalesce(elisa_Sex, Sex),
      Age = coalesce(elisa_Age, Age),
      Structure = coalesce(elisa_Structure, Structure)
    ) %>%
    # Filter to valid MIC calls only (exclude Invalid, Indeterminate, NA)
    filter(
      !is.na(mic_final_call),
      !mic_final_call %in% c("Invalid_NoDNA", "Indeterminate", "")
    ) %>%
    select(
      test1_value, test2_value,
      test1_binary, test2_binary,
      test1_name, test2_name,
      health_zone, province, date,
      match_method,
      mic_sample_name, mic_barcode, mic_test_number,
      elisa_sample, elisa_barcode, elisa_numero,
      Sex, Age, Structure
    )

  return(result)
}
