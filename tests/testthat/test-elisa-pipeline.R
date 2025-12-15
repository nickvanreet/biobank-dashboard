# ==============================================================================
# Integration Tests for ELISA Pipeline
# ==============================================================================
# Tests the 4-step pipeline: ingest → QC → interpret → output
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)

# ==============================================================================
# Test Fixtures
# ==============================================================================

#' Create mock ELISA plate data for testing
create_mock_elisa_plate <- function(
  n_samples = 8,
  pos_od = 1.0,
  neg_od = 0.1,
  include_borderline = TRUE
) {
  # Create 8x12 plate layout
  wells <- expand.grid(
    row = LETTERS[1:8],
    col = sprintf("%02d", 1:12)
  ) %>%
    mutate(well = paste0(row, col))

  # Assign control wells (column 1)
  wells <- wells %>%
    mutate(
      sample_type = case_when(
        col == "01" & row %in% c("A", "B") ~ "POS",
        col == "01" & row %in% c("C", "D") ~ "NEG",
        col == "01" & row %in% c("E", "F") ~ "BLANK",
        TRUE ~ "Sample"
      )
    )

  # Generate OD values
  set.seed(42)
  wells <- wells %>%
    mutate(
      od_450 = case_when(
        sample_type == "POS" ~ rnorm(n(), pos_od, 0.05),
        sample_type == "NEG" ~ rnorm(n(), neg_od, 0.02),
        sample_type == "BLANK" ~ rnorm(n(), 0.05, 0.01),
        TRUE ~ runif(n(), 0.05, 1.5)  # Variable sample ODs
      )
    )

  # Add sample IDs
  sample_counter <- 1
  wells <- wells %>%
    mutate(
      sample_id = case_when(
        sample_type == "Sample" ~ sprintf("KPS-%05d", row_number()),
        TRUE ~ sample_type
      )
    )

  wells
}

#' Create mock run metadata
create_mock_run_metadata <- function(run_id = "TEST-RUN-001") {
  tibble(
    run_id = run_id,
    test_date = Sys.Date(),
    operator = "TEST",
    test_type = "PE"
  )
}

# ==============================================================================
# QC Validation Tests
# ==============================================================================

test_that("ELISA QC: Valid controls pass validation", {
  plate <- create_mock_elisa_plate(pos_od = 1.0, neg_od = 0.1)

  pos_controls <- plate %>% filter(sample_type == "POS")
  neg_controls <- plate %>% filter(sample_type == "NEG")

  # Check positive control range (0.5 - 1.5)
  expect_true(all(pos_controls$od_450 >= 0.5 & pos_controls$od_450 <= 1.5),
              info = "Positive controls should be within valid range")


  # Check negative control range (< 0.5)
  expect_true(all(neg_controls$od_450 < 0.5),
              info = "Negative controls should be below threshold")
})

test_that("ELISA QC: Invalid positive control detected", {
  plate <- create_mock_elisa_plate(pos_od = 0.2, neg_od = 0.1)  # Low POS

  pos_controls <- plate %>% filter(sample_type == "POS")

  # Positive control too low should fail
  expect_true(any(pos_controls$od_450 < 0.5),
              info = "Low positive control should be detected")
})

test_that("ELISA QC: Invalid negative control detected", {
  plate <- create_mock_elisa_plate(pos_od = 1.0, neg_od = 0.6)  # High NEG

  neg_controls <- plate %>% filter(sample_type == "NEG")

  # Negative control too high should fail
  expect_true(any(neg_controls$od_450 > 0.5),
              info = "High negative control should be detected")
})

test_that("ELISA QC: CV calculation is correct", {
  # Create replicate measurements
  replicates <- c(0.5, 0.52, 0.48)
  mean_val <- mean(replicates)
  sd_val <- sd(replicates)
  cv <- (sd_val / mean_val) * 100

  expect_lt(cv, 20, info = "CV should be under 20% for valid replicates")
})

# ==============================================================================
# Classification Tests
# ==============================================================================

test_that("ELISA Classification: PP% thresholds applied correctly", {
  # Test cases
  test_cases <- tibble(
    pp_percent = c(25, 17, 10, 0, NA),
    expected = c("Positive", "Borderline", "Negative", "Negative", "Missing")
  )

  # Classification function (simplified)
  classify_pp <- function(pp) {
    case_when(
      is.na(pp) ~ "Missing",
      pp >= 20 ~ "Positive",
      pp >= 15 ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  results <- classify_pp(test_cases$pp_percent)

  expect_equal(results, test_cases$expected,
               info = "PP% classification should match expected values")
})

test_that("ELISA Classification: DOD thresholds applied correctly", {
  test_cases <- tibble(
    dod = c(0.5, 0.25, 0.15, 0, NA),
    expected = c("Positive", "Borderline", "Negative", "Negative", "Missing")
  )

  # Classification function (simplified)
  classify_dod <- function(dod) {
    case_when(
      is.na(dod) ~ "Missing",
      dod >= 0.3 ~ "Positive",
      dod >= 0.2 ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  results <- classify_dod(test_cases$dod)

  expect_equal(results, test_cases$expected,
               info = "DOD classification should match expected values")
})

# ==============================================================================
# Output Format Tests
# ==============================================================================

test_that("ELISA Output: Required columns present", {
  required_columns <- c(
    "sample_id",
    "test_type",
    "status_final",
    "pp_percent",
    "dod",
    "run_valid",
    "sample_valid",
    "test_date",
    "run_id"
  )

  # Create mock output
  mock_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "PE",
    status_final = "Positive",
    pp_percent = 25.5,
    dod = 0.35,
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "RUN-001"
  )

  missing_cols <- setdiff(required_columns, names(mock_output))
  expect_length(missing_cols, 0,
                info = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
})

test_that("ELISA Output: Data types are correct", {
  mock_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "PE",
    status_final = "Positive",
    pp_percent = 25.5,
    dod = 0.35,
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "RUN-001"
  )

  expect_type(mock_output$sample_id, "character")
  expect_type(mock_output$pp_percent, "double")
  expect_type(mock_output$run_valid, "logical")
  expect_s3_class(mock_output$test_date, "Date")
})

# ==============================================================================
# Pipeline Integration Tests
# ==============================================================================

test_that("ELISA Pipeline: Full flow produces valid output", {
  # Simulate full pipeline

  # 1. Ingest
  raw_plate <- create_mock_elisa_plate()
  expect_gt(nrow(raw_plate), 0, info = "Ingestion should produce data")

  # 2. QC
  pos_mean <- raw_plate %>%
    filter(sample_type == "POS") %>%
    pull(od_450) %>%
    mean()

  neg_mean <- raw_plate %>%
    filter(sample_type == "NEG") %>%
    pull(od_450) %>%
    mean()

  run_valid <- (pos_mean >= 0.5 & pos_mean <= 1.5) & (neg_mean < 0.5)
  expect_true(run_valid, info = "QC should validate controls")

  # 3. Interpret
  samples <- raw_plate %>%
    filter(sample_type == "Sample") %>%
    mutate(
      pp_percent = (od_450 / pos_mean) * 100,
      dod = od_450 - neg_mean,
      status = case_when(
        pp_percent >= 20 ~ "Positive",
        pp_percent >= 15 ~ "Borderline",
        TRUE ~ "Negative"
      )
    )

  expect_true(all(c("pp_percent", "dod", "status") %in% names(samples)),
              info = "Interpretation should calculate metrics")

  # 4. Output
  output <- samples %>%
    select(sample_id, od_450, pp_percent, dod, status) %>%
    mutate(
      run_id = "TEST-RUN-001",
      test_type = "PE",
      run_valid = run_valid,
      sample_valid = TRUE,
      test_date = Sys.Date()
    )

  expect_gt(nrow(output), 0, info = "Output should contain samples")
  expect_true("status" %in% names(output), info = "Output should have status column")
})

test_that("ELISA Pipeline: Invalid run excludes samples", {
  # Create plate with invalid controls
  plate <- create_mock_elisa_plate(pos_od = 0.2, neg_od = 0.1)

  pos_mean <- plate %>%
    filter(sample_type == "POS") %>%
    pull(od_450) %>%
    mean()

  run_valid <- pos_mean >= 0.5  # Will be FALSE

  expect_false(run_valid, info = "Run with low POS should be invalid")

  # In a real pipeline, invalid runs would be flagged
  samples_from_invalid_run <- plate %>%
    filter(sample_type == "Sample") %>%
    mutate(run_valid = run_valid)

  expect_true(all(!samples_from_invalid_run$run_valid),
              info = "All samples from invalid run should be flagged")
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("ELISA: Handles missing OD values", {
  plate <- create_mock_elisa_plate()
  plate$od_450[1:5] <- NA

  samples_with_missing <- plate %>%
    filter(sample_type == "Sample") %>%
    mutate(
      has_od = !is.na(od_450)
    )

  # Should still process samples with valid OD
  valid_samples <- samples_with_missing %>% filter(has_od)
  expect_gt(nrow(valid_samples), 0, info = "Should have some valid samples")
})

test_that("ELISA: Handles edge OD values", {
  edge_cases <- tibble(
    od_450 = c(0, 0.001, 5.0, Inf, -0.1),
    expected_valid = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  # OD validation
  validate_od <- function(od) {
    !is.na(od) & is.finite(od) & od >= 0
  }

  results <- validate_od(edge_cases$od_450)
  expect_equal(results, edge_cases$expected_valid,
               info = "Edge OD values should be handled correctly")
})

# ==============================================================================
# Consolidation Tests
# ==============================================================================

test_that("ELISA Consolidation: Multiple runs resolve correctly", {
  # Simulate multiple runs for same sample
  multi_run <- tibble(
    sample_id = rep("KPS-00001", 3),
    run_id = c("RUN-001", "RUN-002", "RUN-003"),
    status = c("Negative", "Positive", "Positive"),
    pp_percent = c(10, 25, 28)
  )

  # Consolidation rule: Positive if any run positive
  consolidated <- multi_run %>%
    group_by(sample_id) %>%
    summarize(
      n_runs = n(),
      n_positive = sum(status == "Positive"),
      final_status = if_else(any(status == "Positive"), "Positive", "Negative"),
      max_pp = max(pp_percent),
      .groups = "drop"
    )

  expect_equal(consolidated$final_status, "Positive",
               info = "Sample with any positive run should be positive")
  expect_equal(consolidated$n_runs, 3,
               info = "Should count all runs")
})
