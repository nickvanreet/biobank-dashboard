# ELISA Concordance Analysis Utilities
# Functions for matching samples across PE and VSG ELISA tests
# and calculating concordance metrics

#' Normalize identifier for ELISA sample matching
#' More comprehensive than the global normalize_id function
#' @param x Character vector of identifiers
#' @return Normalized character vector
normalize_elisa_id <- function(x) {
  x <- tolower(trimws(as.character(x)))
  # Remove KPS prefix and leading zeros from barcodes
  x <- gsub("^kps", "", x)
  x <- gsub("^0+", "", x)
  # Remove non-alphanumeric characters
  x <- gsub("[^a-z0-9]", "", x)
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

  return(all_matches)
}

#' Calculate sample positivity based on thresholds
#' @param pp_percent Percent positivity value
#' @param dod Difference of OD value
#' @param pp_cutoff PP% cutoff (default 20)
#' @param dod_cutoff DOD cutoff (default 0.3)
#' @return Logical vector indicating positivity
calculate_positivity <- function(pp_percent, dod, pp_cutoff = 20, dod_cutoff = 0.3) {
  (pp_percent >= pp_cutoff) | (dod >= dod_cutoff)
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
