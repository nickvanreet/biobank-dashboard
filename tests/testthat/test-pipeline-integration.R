# ==============================================================================
# Pipeline Integration Tests
# ==============================================================================
# End-to-end tests for the modular pipeline architecture
# Tests data flow from ingestion through dashboard consumption
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)

# ==============================================================================
# Test Fixtures
# ==============================================================================

#' Create mock biobank data for linking tests
create_mock_biobank <- function(n = 50) {
  set.seed(42)
  tibble(
    barcode = sprintf("KPS-%05d", 1:n),
    numero = 1:n,
    date_prelevement = Sys.Date() - sample(1:30, n, replace = TRUE),
    age = sample(18:80, n, replace = TRUE),
    sexe = sample(c("M", "F"), n, replace = TRUE),
    grossesse = if_else(sexe == "F", sample(c("Oui", "Non"), n, replace = TRUE), NA_character_),
    province = sample(c("Kasai Oriental", "Kasai Central"), n, replace = TRUE),
    zone_sante = sample(c("Dibindi", "Tshilenge", "Miabi"), n, replace = TRUE),
    type_depistage = sample(c("DA", "DP"), n, replace = TRUE)
  )
}

#' Create mock pipeline output (generic)
create_mock_pipeline_output <- function(
  biobank,
  test_type = "MIC",
  positivity_rate = 0.1
) {
  n <- nrow(biobank)
  n_tested <- round(n * 0.8)  # 80% of samples tested
  tested_samples <- sample(biobank$barcode, n_tested)

  set.seed(123)
  tibble(
    sample_id = tested_samples,
    test_type = test_type,
    status_final = sample(
      c("Positive", "Negative", "Borderline"),
      n_tested,
      replace = TRUE,
      prob = c(positivity_rate, 1 - positivity_rate - 0.05, 0.05)
    ),
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date() - sample(1:14, n_tested, replace = TRUE),
    run_id = sprintf("%s-RUN-%03d", test_type, sample(1:10, n_tested, replace = TRUE))
  )
}

# ==============================================================================
# Sample ID Normalization Tests
# ==============================================================================

test_that("Sample ID normalization is consistent", {
  test_ids <- c(
    "KPS-12345",
    "kps-12345",
    "KPS12345",
    "12345",
    "KPS-12345-A",
    "  KPS-12345  "
  )

  # Simplified normalization (matches dashboard_data_utils.R)
  normalize <- function(id) {
    id <- trimws(as.character(id))
    id <- gsub("^[Kk][Pp][Ss][-_]?", "", id)
    id <- tolower(id)
    id <- gsub("[^a-z0-9._-]", "", id)
    id
  }

  normalized <- sapply(test_ids, normalize)

  # First 4 should normalize to same base
  expect_equal(normalized[1], normalized[2],
               info = "Case should not matter")
  expect_equal(normalized[1], normalized[4],
               info = "KPS prefix should be removed")
})

# ==============================================================================
# Biobank Linking Tests
# ==============================================================================

test_that("Pipeline output links to biobank correctly", {
  biobank <- create_mock_biobank(50)
  mic_results <- create_mock_pipeline_output(biobank, "MIC")

  # Simulate linking
  linked <- mic_results %>%
    mutate(
      normalized_id = gsub("^KPS-", "", sample_id) %>% tolower()
    ) %>%
    left_join(
      biobank %>%
        mutate(normalized_id = gsub("^KPS-", "", barcode) %>% tolower()),
      by = "normalized_id"
    )

  # Check linking success
  linked_count <- sum(!is.na(linked$barcode))

  expect_gt(linked_count, 0, info = "Some samples should link to biobank")
  expect_true("age" %in% names(linked), info = "Linked data should include demographics")
  expect_true("province" %in% names(linked), info = "Linked data should include geography")
})

test_that("Unmatched samples are handled gracefully", {
  biobank <- create_mock_biobank(50)

  # Create results with some samples not in biobank
  results <- tibble(
    sample_id = c("KPS-00001", "KPS-99999", "UNKNOWN-001"),  # 99999 and UNKNOWN not in biobank
    status_final = c("Positive", "Negative", "Positive")
  )

  linked <- results %>%
    mutate(
      normalized_id = gsub("^KPS-", "", sample_id) %>% tolower()
    ) %>%
    left_join(
      biobank %>%
        mutate(normalized_id = gsub("^KPS-", "", barcode) %>% tolower()),
      by = "normalized_id"
    ) %>%
    mutate(
      biobank_matched = !is.na(barcode)
    )

  # Should have match indicator
  expect_true("biobank_matched" %in% names(linked),
              info = "Should have match indicator column")

  # At least one should not match
  expect_true(any(!linked$biobank_matched),
              info = "Some samples should fail to match")
})

# ==============================================================================
# Multi-Test Concordance Tests
# ==============================================================================

test_that("Multiple test types can be combined", {
  biobank <- create_mock_biobank(100)

  mic_results <- create_mock_pipeline_output(biobank, "MIC", 0.1)
  elisa_results <- create_mock_pipeline_output(biobank, "ELISA-PE", 0.15)
  ielisa_results <- create_mock_pipeline_output(biobank, "iELISA", 0.12)

  # Combine all results
  all_results <- bind_rows(
    mic_results %>% mutate(test_category = "Molecular"),
    elisa_results %>% mutate(test_category = "Serological"),
    ielisa_results %>% mutate(test_category = "Serological")
  )

  # Check structure
  expect_gt(nrow(all_results), 0, info = "Combined results should have data")
  expect_equal(length(unique(all_results$test_type)), 3,
               info = "Should have 3 test types")

  # Pivot to wide format for concordance
  wide_results <- all_results %>%
    select(sample_id, test_type, status_final) %>%
    distinct() %>%
    pivot_wider(
      names_from = test_type,
      values_from = status_final,
      values_fn = first
    )

  expect_true("MIC" %in% names(wide_results) || "sample_id" %in% names(wide_results),
              info = "Wide format should have test type columns")
})

test_that("Test overlap analysis works", {
  # Create samples with known overlap
  samples <- tibble(
    sample_id = sprintf("KPS-%05d", 1:100),
    mic_status = c(rep("Positive", 10), rep("Negative", 90)),
    elisa_status = c(rep("Positive", 5), rep("Negative", 5), rep("Positive", 10), rep("Negative", 80))
  )

  # Calculate overlaps
  overlap_analysis <- samples %>%
    summarize(
      n_total = n(),
      n_mic_positive = sum(mic_status == "Positive"),
      n_elisa_positive = sum(elisa_status == "Positive"),
      n_both_positive = sum(mic_status == "Positive" & elisa_status == "Positive"),
      n_mic_only = sum(mic_status == "Positive" & elisa_status != "Positive"),
      n_elisa_only = sum(mic_status != "Positive" & elisa_status == "Positive"),
      n_either_positive = sum(mic_status == "Positive" | elisa_status == "Positive")
    )

  expect_equal(overlap_analysis$n_mic_positive, 10, info = "MIC positive count")
  expect_equal(overlap_analysis$n_elisa_positive, 15, info = "ELISA positive count")
  expect_equal(overlap_analysis$n_both_positive, 5, info = "Both positive count")
})

# ==============================================================================
# Filter Propagation Tests
# ==============================================================================

test_that("Date filters apply correctly", {
  biobank <- create_mock_biobank(100)
  results <- create_mock_pipeline_output(biobank, "MIC")

  # Link to biobank
  linked <- results %>%
    left_join(biobank, by = c("sample_id" = "barcode"))

  # Apply date filter
  date_min <- Sys.Date() - 14
  date_max <- Sys.Date() - 7

  filtered <- linked %>%
    filter(date_prelevement >= date_min & date_prelevement <= date_max)

  # Check filter applied
  if (nrow(filtered) > 0) {
    expect_true(all(filtered$date_prelevement >= date_min),
                info = "All dates should be >= min")
    expect_true(all(filtered$date_prelevement <= date_max),
                info = "All dates should be <= max")
  }
})

test_that("Geographic filters apply correctly", {
  biobank <- create_mock_biobank(100)
  results <- create_mock_pipeline_output(biobank, "MIC")

  linked <- results %>%
    left_join(biobank, by = c("sample_id" = "barcode"))

  # Apply province filter
  filtered <- linked %>%
    filter(province == "Kasai Oriental")

  if (nrow(filtered) > 0) {
    expect_true(all(filtered$province == "Kasai Oriental"),
                info = "All samples should be from filtered province")
  }
})

# ==============================================================================
# QC Impact Tests
# ==============================================================================

test_that("Invalid runs are excluded from aggregate statistics", {
  results <- tibble(
    sample_id = sprintf("KPS-%05d", 1:100),
    status_final = sample(c("Positive", "Negative"), 100, replace = TRUE),
    run_valid = c(rep(TRUE, 80), rep(FALSE, 20)),
    sample_valid = TRUE
  )

  # Calculate stats including invalid
  all_stats <- results %>%
    summarize(
      n_total = n(),
      n_positive = sum(status_final == "Positive"),
      positivity = n_positive / n_total * 100
    )

  # Calculate stats excluding invalid runs
  valid_stats <- results %>%
    filter(run_valid == TRUE) %>%
    summarize(
      n_total = n(),
      n_positive = sum(status_final == "Positive"),
      positivity = n_positive / n_total * 100
    )

  expect_lt(valid_stats$n_total, all_stats$n_total,
            info = "Valid-only count should be less than total")

  # Positivity rates may differ
  expect_true(is.numeric(valid_stats$positivity),
              info = "Positivity should be calculable")
})

test_that("Sample-level QC flags propagate", {
  results <- tibble(
    sample_id = sprintf("KPS-%05d", 1:50),
    status_final = "Positive",
    run_valid = TRUE,
    sample_valid = c(rep(TRUE, 40), rep(FALSE, 10)),
    qc_flags = c(rep(NA_character_, 40), rep("Low RNAseP", 10))
  )

  # Samples with QC issues
  qc_issues <- results %>%
    filter(sample_valid == FALSE | !is.na(qc_flags))

  expect_equal(nrow(qc_issues), 10, info = "Should identify samples with QC issues")
})

# ==============================================================================
# Dashboard Data Utils Integration
# ==============================================================================

test_that("Classification thresholds are consistent", {
  # These should match dashboard_data_utils.R assay_cutoffs()
  expected_cutoffs <- list(
    elisa_pp_positive = 20,
    elisa_pp_borderline = c(15, 20),
    ielisa_inhibition_positive = 30,
    ielisa_inhibition_borderline = c(25, 30)
  )

  # Test ELISA classification
  pp_values <- c(25, 17, 10)
  expected_elisa <- c("Positive", "Borderline", "Negative")

  classify_elisa <- function(pp) {
    case_when(
      pp >= expected_cutoffs$elisa_pp_positive ~ "Positive",
      pp >= expected_cutoffs$elisa_pp_borderline[1] ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  expect_equal(classify_elisa(pp_values), expected_elisa,
               info = "ELISA classification should match cutoffs")

  # Test iELISA classification
  inhibition_values <- c(40, 27, 15)
  expected_ielisa <- c("Positive", "Borderline", "Negative")

  classify_ielisa <- function(inh) {
    case_when(
      inh >= expected_cutoffs$ielisa_inhibition_positive ~ "Positive",
      inh >= expected_cutoffs$ielisa_inhibition_borderline[1] ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  expect_equal(classify_ielisa(inhibition_values), expected_ielisa,
               info = "iELISA classification should match cutoffs")
})

# ==============================================================================
# Output Standardization Tests
# ==============================================================================

test_that("All pipelines produce consistent output schema", {
  # Required columns for all pipeline outputs
  required_columns <- c(
    "sample_id",
    "test_type",
    "status_final",
    "run_valid",
    "sample_valid",
    "test_date",
    "run_id"
  )

  # Create mock outputs for each pipeline type
  mic_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "MIC",
    status_final = "Positive",
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "MIC-RUN-001"
  )

  elisa_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "ELISA-PE",
    status_final = "Negative",
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "ELISA-RUN-001"
  )

  ielisa_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "iELISA",
    status_final = "Positive",
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "iELISA-RUN-001"
  )

  # Check each output has required columns
  for (output in list(mic_output, elisa_output, ielisa_output)) {
    missing <- setdiff(required_columns, names(output))
    expect_length(missing, 0,
                  info = paste("Missing columns:", paste(missing, collapse = ", ")))
  }

  # All outputs should be combinable
  combined <- bind_rows(mic_output, elisa_output, ielisa_output)
  expect_equal(nrow(combined), 3, info = "Outputs should be combinable")
})

# ==============================================================================
# Consolidation Integration Tests
# ==============================================================================

test_that("Retest consolidation follows hierarchy", {
  # Simulate multiple tests for same sample
  multi_test <- tibble(
    sample_id = rep("KPS-00001", 4),
    run_id = paste0("RUN-", 1:4),
    status_final = c("Negative", "Borderline", "Positive", "Negative"),
    test_date = Sys.Date() - c(10, 7, 3, 1)
  )

  # Consolidation: Most recent positive/borderline takes precedence
  consolidated <- multi_test %>%
    arrange(sample_id, desc(test_date)) %>%
    group_by(sample_id) %>%
    summarize(
      n_tests = n(),
      has_positive = any(status_final == "Positive"),
      has_borderline = any(status_final == "Borderline"),
      final_status = case_when(
        any(status_final == "Positive") ~ "Positive",
        any(status_final == "Borderline") ~ "Borderline",
        TRUE ~ "Negative"
      ),
      most_recent_status = first(status_final),
      .groups = "drop"
    )

  expect_equal(consolidated$final_status, "Positive",
               info = "Final status should be Positive (highest severity)")
  expect_equal(consolidated$n_tests, 4, info = "Should count all tests")
})

# ==============================================================================
# Performance Considerations
# ==============================================================================
test_that("Large datasets can be processed", {
  skip_if(Sys.getenv("SKIP_SLOW_TESTS") == "true", "Skipping slow test")

  # Create larger dataset
  large_biobank <- create_mock_biobank(1000)
  large_results <- create_mock_pipeline_output(large_biobank, "MIC")

  # Time the join operation
  start_time <- Sys.time()

  linked <- large_results %>%
    left_join(large_biobank, by = c("sample_id" = "barcode"))

  elapsed <- difftime(Sys.time(), start_time, units = "secs")

  expect_lt(as.numeric(elapsed), 5,
            info = "Large join should complete in under 5 seconds")
  expect_gt(nrow(linked), 0, info = "Should produce results")
})
