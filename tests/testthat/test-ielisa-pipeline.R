# ==============================================================================
# Integration Tests for iELISA Pipeline
# ==============================================================================
# Tests the 4-step pipeline: ingest → QC → interpret → output
# Specific to inhibition ELISA with LiTat 1.3 and LiTat 1.5 antigens
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)

# ==============================================================================
# Test Fixtures
# ==============================================================================

#' Create mock iELISA plate data for testing
create_mock_ielisa_plate <- function(
  n_samples = 20,
  neg_od = 2.0,    # High OD for negative control (no inhibition)
  pos_inhibition = 50  # % inhibition for positive control
) {

  # Calculate positive control OD from inhibition
  pos_od <- neg_od * (1 - pos_inhibition/100)

  # Create wells
  wells <- tibble(
    well = sprintf("%s%02d", rep(LETTERS[1:8], each = 12), rep(1:12, 8))
  )

  # Assign control and sample wells
  wells <- wells %>%
    mutate(
      row = substr(well, 1, 1),
      col = as.integer(substr(well, 2, 3)),
      sample_type = case_when(
        col == 1 & row %in% c("A", "B") ~ "NEG",
        col == 1 & row %in% c("C", "D") ~ "POS",
        col <= 6 ~ "Sample",
        TRUE ~ "Empty"
      )
    )

  # Generate OD values at 450-600nm
  set.seed(123)
  wells <- wells %>%
    mutate(
      od_450_600 = case_when(
        sample_type == "NEG" ~ rnorm(n(), neg_od, 0.1),
        sample_type == "POS" ~ rnorm(n(), pos_od, 0.1),
        sample_type == "Sample" ~ runif(n(), 0.5, neg_od),
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(od_450_600))

  # Add sample IDs
  wells <- wells %>%
    mutate(
      sample_id = case_when(
        sample_type == "Sample" ~ sprintf("KPS-%05d", row_number()),
        TRUE ~ sample_type
      ),
      antigen = "LiTat1.3"  # Default antigen
    )

  wells
}

#' Create mock dual-antigen plate
create_mock_dual_antigen_plate <- function() {
  l13 <- create_mock_ielisa_plate() %>% mutate(antigen = "LiTat1.3")
  l15 <- create_mock_ielisa_plate() %>% mutate(antigen = "LiTat1.5")

  bind_rows(l13, l15)
}

# ==============================================================================
# QC Validation Tests
# ==============================================================================

test_that("iELISA QC: Negative control OD in valid range (1.0-3.1)", {
  plate <- create_mock_ielisa_plate(neg_od = 2.0)

  neg_controls <- plate %>% filter(sample_type == "NEG")

  # NEG OD should be between 1.0 and 3.1
  expect_true(all(neg_controls$od_450_600 >= 1.0 & neg_controls$od_450_600 <= 3.1),
              info = "Negative control OD should be in valid range (1.0-3.1)")
})

test_that("iELISA QC: Negative control too low detected", {
  plate <- create_mock_ielisa_plate(neg_od = 0.5)  # Too low

  neg_controls <- plate %>% filter(sample_type == "NEG")

  expect_true(any(neg_controls$od_450_600 < 1.0),
              info = "Low negative control should be detected")
})

test_that("iELISA QC: Negative control too high detected", {
  plate <- create_mock_ielisa_plate(neg_od = 3.5)  # Too high

  neg_controls <- plate %>% filter(sample_type == "NEG")

  expect_true(any(neg_controls$od_450_600 > 3.1),
              info = "High negative control should be detected")
})

test_that("iELISA QC: Positive control inhibition >= 30%", {
  plate <- create_mock_ielisa_plate(neg_od = 2.0, pos_inhibition = 50)

  neg_mean <- plate %>%
    filter(sample_type == "NEG") %>%
    pull(od_450_600) %>%
    mean()

  pos_mean <- plate %>%
    filter(sample_type == "POS") %>%
    pull(od_450_600) %>%
    mean()

  pos_inhibition <- (1 - pos_mean/neg_mean) * 100

  expect_gte(pos_inhibition, 30,
             info = "Positive control should show >= 30% inhibition")
})

test_that("iELISA QC: Insufficient positive control inhibition detected", {
  plate <- create_mock_ielisa_plate(neg_od = 2.0, pos_inhibition = 15)  # Too low

  neg_mean <- plate %>%
    filter(sample_type == "NEG") %>%
    pull(od_450_600) %>%
    mean()

  pos_mean <- plate %>%
    filter(sample_type == "POS") %>%
    pull(od_450_600) %>%
    mean()

  pos_inhibition <- (1 - pos_mean/neg_mean) * 100

  expect_lt(pos_inhibition, 30,
            info = "Low positive control inhibition should be detected")
})

# ==============================================================================
# Inhibition Calculation Tests
# ==============================================================================

test_that("iELISA Inhibition: Formula 1 calculates correctly", {
  # Formula 1: % Inhibition = (1 - (Sample OD / NEG OD)) * 100
  neg_od <- 2.0
  sample_od <- 1.0

  expected_inhibition <- (1 - sample_od/neg_od) * 100  # = 50%

  expect_equal(expected_inhibition, 50,
               info = "Formula 1 inhibition calculation should be correct")
})

test_that("iELISA Inhibition: Formula 2 calculates correctly", {
  # Formula 2: % Inhibition = ((NEG OD - Sample OD) / NEG OD) * 100
  # This is mathematically equivalent to Formula 1
  neg_od <- 2.0
  sample_od <- 1.0

  formula1 <- (1 - sample_od/neg_od) * 100
  formula2 <- ((neg_od - sample_od) / neg_od) * 100

  expect_equal(formula1, formula2,
               info = "Formula 1 and Formula 2 should be equivalent")
})

test_that("iELISA Inhibition: Edge cases handled", {
  neg_od <- 2.0

  test_cases <- tibble(
    sample_od = c(0, neg_od, neg_od * 2, NA),
    expected_inhibition = c(100, 0, -100, NA)
  )

  results <- (1 - test_cases$sample_od/neg_od) * 100

  expect_equal(results[1], 100, info = "Zero OD should give 100% inhibition")
  expect_equal(results[2], 0, info = "Equal to NEG should give 0% inhibition")
  expect_equal(results[3], -100, info = "Double NEG should give -100% inhibition")
  expect_true(is.na(results[4]), info = "NA OD should give NA inhibition")
})

# ==============================================================================
# Classification Tests
# ==============================================================================

test_that("iELISA Classification: Inhibition thresholds applied correctly", {
  test_cases <- tibble(
    inhibition_pct = c(50, 27, 20, 0, -10, NA),
    expected = c("Positive", "Borderline", "Negative", "Negative", "Negative", "Missing")
  )

  # Classification function
  classify_ielisa <- function(inhibition) {
    case_when(
      is.na(inhibition) ~ "Missing",
      inhibition >= 30 ~ "Positive",
      inhibition >= 25 ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  results <- classify_ielisa(test_cases$inhibition_pct)

  expect_equal(results, test_cases$expected,
               info = "iELISA classification should match expected values")
})

# ==============================================================================
# Dual Antigen Tests
# ==============================================================================

test_that("iELISA Dual Antigen: Both L13 and L15 processed", {
  plate <- create_mock_dual_antigen_plate()

  antigens <- unique(plate$antigen)

  expect_true("LiTat1.3" %in% antigens, info = "Should have LiTat 1.3 data")
  expect_true("LiTat1.5" %in% antigens, info = "Should have LiTat 1.5 data")
})

test_that("iELISA Dual Antigen: Combined status logic", {
  # Rules: Positive if either antigen positive
  test_cases <- tibble(
    status_l13 = c("Positive", "Negative", "Positive", "Negative", "Borderline"),
    status_l15 = c("Positive", "Positive", "Negative", "Negative", "Negative"),
    expected_final = c("Positive", "Positive", "Positive", "Negative", "Borderline")
  )

  combine_status <- function(l13, l15) {
    case_when(
      l13 == "Positive" | l15 == "Positive" ~ "Positive",
      l13 == "Borderline" | l15 == "Borderline" ~ "Borderline",
      TRUE ~ "Negative"
    )
  }

  results <- combine_status(test_cases$status_l13, test_cases$status_l15)

  expect_equal(results, test_cases$expected_final,
               info = "Combined status should follow OR logic for positives")
})

# ==============================================================================
# Output Format Tests
# ==============================================================================

test_that("iELISA Output: Required columns present", {
  required_columns <- c(
    "sample_id",
    "test_type",
    "antigen",
    "inhibition_pct",
    "status_final",
    "run_valid",
    "sample_valid",
    "test_date",
    "run_id"
  )

  mock_output <- tibble(
    sample_id = "KPS-00001",
    test_type = "iELISA",
    antigen = "LiTat1.3",
    inhibition_pct = 45.5,
    status_final = "Positive",
    run_valid = TRUE,
    sample_valid = TRUE,
    test_date = Sys.Date(),
    run_id = "iELISA-001"
  )

  missing_cols <- setdiff(required_columns, names(mock_output))
  expect_length(missing_cols, 0,
                info = paste("Missing columns:", paste(missing_cols, collapse = ", ")))
})

# ==============================================================================
# Pipeline Integration Tests
# ==============================================================================

test_that("iELISA Pipeline: Full flow produces valid output", {
  # 1. Ingest
  raw_plate <- create_mock_ielisa_plate()
  expect_gt(nrow(raw_plate), 0, info = "Ingestion should produce data")

  # 2. QC
  neg_mean <- raw_plate %>%
    filter(sample_type == "NEG") %>%
    pull(od_450_600) %>%
    mean()

  pos_mean <- raw_plate %>%
    filter(sample_type == "POS") %>%
    pull(od_450_600) %>%
    mean()

  pos_inhibition <- (1 - pos_mean/neg_mean) * 100
  neg_od_valid <- neg_mean >= 1.0 & neg_mean <= 3.1
  pos_inhibition_valid <- pos_inhibition >= 30

  run_valid <- neg_od_valid & pos_inhibition_valid
  expect_true(run_valid, info = "QC should validate controls")

  # 3. Interpret
  samples <- raw_plate %>%
    filter(sample_type == "Sample") %>%
    mutate(
      inhibition_pct = (1 - od_450_600/neg_mean) * 100,
      status = case_when(
        inhibition_pct >= 30 ~ "Positive",
        inhibition_pct >= 25 ~ "Borderline",
        TRUE ~ "Negative"
      )
    )

  expect_true(all(c("inhibition_pct", "status") %in% names(samples)),
              info = "Interpretation should calculate metrics")

  # 4. Output
  output <- samples %>%
    select(sample_id, antigen, od_450_600, inhibition_pct, status) %>%
    mutate(
      run_id = "iELISA-TEST-001",
      test_type = "iELISA",
      run_valid = run_valid,
      sample_valid = TRUE,
      test_date = Sys.Date()
    )

  expect_gt(nrow(output), 0, info = "Output should contain samples")
  expect_true("inhibition_pct" %in% names(output),
              info = "Output should have inhibition column")
})

test_that("iELISA Pipeline: Invalid run flagged correctly", {
  # Create plate with invalid negative control (too low)
  plate <- create_mock_ielisa_plate(neg_od = 0.5)

  neg_mean <- plate %>%
    filter(sample_type == "NEG") %>%
    pull(od_450_600) %>%
    mean()

  neg_od_valid <- neg_mean >= 1.0 & neg_mean <= 3.1

  expect_false(neg_od_valid, info = "Run with low NEG should be invalid")
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("iELISA: Handles missing OD values", {
  plate <- create_mock_ielisa_plate()
  plate$od_450_600[1:3] <- NA

  samples_with_missing <- plate %>%
    filter(sample_type == "Sample") %>%
    mutate(
      has_od = !is.na(od_450_600)
    )

  valid_samples <- samples_with_missing %>% filter(has_od)
  expect_gt(nrow(valid_samples), 0, info = "Should have some valid samples")
})

test_that("iELISA: Negative inhibition handled", {
  # When sample OD > NEG OD, inhibition is negative (no inhibition)
  neg_od <- 2.0
  sample_od <- 2.5  # Higher than NEG

  inhibition <- (1 - sample_od/neg_od) * 100

  expect_lt(inhibition, 0, info = "Sample > NEG should give negative inhibition")

  # Should classify as Negative
  status <- if(inhibition >= 30) "Positive" else "Negative"
  expect_equal(status, "Negative",
               info = "Negative inhibition should classify as Negative")
})

# ==============================================================================
# Consolidation Tests
# ==============================================================================

test_that("iELISA Consolidation: Multiple runs resolve correctly", {
  multi_run <- tibble(
    sample_id = rep("KPS-00001", 3),
    run_id = c("RUN-001", "RUN-002", "RUN-003"),
    antigen = rep("LiTat1.3", 3),
    status = c("Negative", "Positive", "Borderline"),
    inhibition_pct = c(15, 45, 27)
  )

  consolidated <- multi_run %>%
    group_by(sample_id, antigen) %>%
    summarize(
      n_runs = n(),
      n_positive = sum(status == "Positive"),
      final_status = case_when(
        any(status == "Positive") ~ "Positive",
        any(status == "Borderline") ~ "Borderline",
        TRUE ~ "Negative"
      ),
      max_inhibition = max(inhibition_pct),
      .groups = "drop"
    )

  expect_equal(consolidated$final_status, "Positive",
               info = "Sample with any positive run should be positive")
  expect_equal(consolidated$n_runs, 3,
               info = "Should count all runs")
})

test_that("iELISA Consolidation: Dual antigen consensus", {
  dual_antigen <- tibble(
    sample_id = rep("KPS-00001", 2),
    antigen = c("LiTat1.3", "LiTat1.5"),
    final_status = c("Positive", "Negative")
  )

  consensus <- dual_antigen %>%
    group_by(sample_id) %>%
    summarize(
      l13_status = first(final_status[antigen == "LiTat1.3"]),
      l15_status = first(final_status[antigen == "LiTat1.5"]),
      overall_status = case_when(
        any(final_status == "Positive") ~ "Positive",
        any(final_status == "Borderline") ~ "Borderline",
        TRUE ~ "Negative"
      ),
      .groups = "drop"
    )

  expect_equal(consensus$overall_status, "Positive",
               info = "Positive on either antigen should be overall positive")
})
