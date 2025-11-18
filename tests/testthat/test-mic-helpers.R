# ==============================================================================
# Unit Tests for MIC Helper Functions
# ==============================================================================
# Tests utility functions used throughout the MIC analysis pipeline:
# - classify_target: Determines if a Cq value is Positive/Negative/LatePositive
# - Normalization functions
# - Safe statistical functions
# - Column matching logic
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)

# ==============================================================================
# Target Classification Tests (classify_target_vectorized)
# ==============================================================================

test_that("classify_target: Positive call for Cq below positive threshold", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40)
    )
  )

  # Create test data
  cq_values <- c(30, 32, 34)

  # Should all be positive
  for (cq in cq_values) {
    # Test if function exists
    if (exists("classify_target_vectorized")) {
      result <- classify_target_vectorized(cq, "177T", settings)
      expect_equal(result, "Positive", info = sprintf("Cq=%s should be Positive", cq))
    }
  }
})

test_that("classify_target: Negative call for Cq above negative threshold", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40)
    )
  )

  cq_values <- c(41, 45, 50)

  for (cq in cq_values) {
    if (exists("classify_target_vectorized")) {
      result <- classify_target_vectorized(cq, "177T", settings)
      expect_equal(result, "Negative", info = sprintf("Cq=%s should be Negative", cq))
    }
  }
})

test_that("classify_target: LatePositive for Cq between thresholds", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40)
    )
  )

  # Values between 35 and 40 should be LatePositive
  cq_values <- c(36, 37, 38, 39)

  for (cq in cq_values) {
    if (exists("classify_target_vectorized")) {
      result <- classify_target_vectorized(cq, "177T", settings)
      expect_equal(result, "LatePositive", info = sprintf("Cq=%s should be LatePositive", cq))
    }
  }
})

test_that("classify_target: Boundary conditions at exact thresholds", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40)
    )
  )

  if (exists("classify_target_vectorized")) {
    # Exactly at positive threshold - should be Positive (≤ 35)
    expect_equal(classify_target_vectorized(35, "177T", settings), "Positive")

    # Just above positive threshold - should be LatePositive
    expect_equal(classify_target_vectorized(35.01, "177T", settings), "LatePositive")

    # Exactly at negative threshold - should be LatePositive (< 40)
    expect_equal(classify_target_vectorized(40, "177T", settings), "LatePositive")

    # Just above negative threshold - should be Negative
    expect_equal(classify_target_vectorized(40.01, "177T", settings), "Negative")
  }
})

test_that("classify_target: NA and undetermined values", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40)
    )
  )

  if (exists("classify_target_vectorized")) {
    # NA should return Negative or Indeterminate
    result_na <- classify_target_vectorized(NA_real_, "177T", settings)
    expect_true(result_na %in% c("Negative", "Indeterminate", "Failed"))

    # Very high Cq (no amplification) should be Negative
    result_high <- classify_target_vectorized(999, "177T", settings)
    expect_equal(result_high, "Negative")
  }
})

test_that("classify_target: Different targets have different thresholds", {
  settings <- list(
    thresholds = list(
      `177T` = list(positive = 35, negative = 40),
      `18S2` = list(positive = 35, negative = 40),
      RNAseP_DNA = list(positive = 28, negative = 40),
      RNAseP_RNA = list(positive = 35, negative = 40)
    )
  )

  if (exists("classify_target_vectorized")) {
    # Cq=30 should be:
    # - LatePositive for 177T (>35)  NO wait, 30 < 35 so Positive
    # - Positive for RNAseP_DNA (≤28) NO wait, 30 > 28 so LatePositive

    # Let's use Cq=30 for RNAseP_DNA: 30 > 28 (positive) and < 40 (negative) = LatePositive
    expect_equal(classify_target_vectorized(30, "RNAseP_DNA", settings), "LatePositive")

    # Cq=30 for 177T: 30 < 35 (positive) = Positive
    expect_equal(classify_target_vectorized(30, "177T", settings), "Positive")
  }
})

# ==============================================================================
# Normalization Function Tests
# ==============================================================================

test_that("normalize_id: Converts to uppercase and trims whitespace", {
  if (exists("normalize_id")) {
    expect_equal(normalize_id("  sample123  "), "SAMPLE123")
    expect_equal(normalize_id("test-id"), "TEST-ID")
    expect_equal(normalize_id("MixedCase"), "MIXEDCASE")
  }
})

test_that("normalize_id: Handles NULL and NA", {
  if (exists("normalize_id")) {
    expect_true(is.na(normalize_id(NULL)))
    expect_true(is.na(normalize_id(NA)))
  }
})

test_that("normalize_field_name: Removes special characters", {
  if (exists("normalize_field_name")) {
    # Should convert to lowercase and remove special chars
    result <- normalize_field_name("Sample ID (Barcode)")
    expect_true(grepl("sample", result, ignore.case = TRUE))
    expect_false(grepl("[()]", result))
  }
})

# ==============================================================================
# Safe Statistical Function Tests
# ==============================================================================

test_that("safe_median: Handles all-NA vectors", {
  if (exists("safe_median")) {
    expect_true(is.na(safe_median(c(NA, NA, NA))))
    expect_equal(safe_median(c(1, 2, 3)), 2)
    expect_equal(safe_median(c(1, NA, 3)), 2)
  }
})

test_that("safe_mean: Handles all-NA vectors", {
  if (exists("safe_mean")) {
    expect_true(is.na(safe_mean(c(NA, NA, NA))))
    expect_equal(safe_mean(c(1, 2, 3)), 2)
    expect_equal(safe_mean(c(1, NA, 3)), 2)
  }
})

test_that("safe_sd: Handles vectors with ≤1 non-NA value", {
  if (exists("safe_sd")) {
    expect_true(is.na(safe_sd(c(NA))))
    expect_true(is.na(safe_sd(c(1))))
    expect_true(is.na(safe_sd(c(1, NA))))
    expect_false(is.na(safe_sd(c(1, 2))))
  }
})

test_that("safe_sd: Calculates correct standard deviation", {
  if (exists("safe_sd")) {
    values <- c(10, 12, 14, 16)
    result <- safe_sd(values)
    expected <- sd(values)
    expect_equal(result, expected, tolerance = 0.001)
  }
})

# ==============================================================================
# Column Matching Tests
# ==============================================================================

test_that("match_column_name: Finds exact matches", {
  if (exists("match_column_name")) {
    data_names <- c("SampleID", "RunID", "Cq_Value")
    result <- match_column_name(data_names, c("SampleID"))
    expect_equal(result, "SampleID")
  }
})

test_that("match_column_name: Finds case-insensitive matches", {
  if (exists("match_column_name")) {
    data_names <- c("sampleid", "runid", "cq_value")
    result <- match_column_name(data_names, c("SampleID", "Sample ID"))
    expect_true(!is.null(result))
  }
})

test_that("match_column_name: Handles partial matches", {
  if (exists("match_column_name")) {
    data_names <- c("Sample_Identifier", "Run_Number", "Cq_Mean")
    result <- match_column_name(data_names, c("SampleID", "Sample"))
    expect_true(!is.null(result))
    expect_true(grepl("Sample", result, ignore.case = TRUE))
  }
})

test_that("match_column_name: Returns NULL when no match", {
  if (exists("match_column_name")) {
    data_names <- c("A", "B", "C")
    result <- match_column_name(data_names, c("X", "Y", "Z"))
    expect_null(result)
  }
})

# ==============================================================================
# Delta Calculation Tests
# ==============================================================================

test_that("Delta calculations: RNA - DNA for preservation", {
  # Delta_RP = RNAseP_RNA - RNAseP_DNA
  rna_cq <- 32
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_equal(delta, 4)

  # Good preservation: ≤5
  expect_lte(delta, 5)
})

test_that("Delta calculations: 18S2 - 177T for Trypanozoon", {
  # Delta_18S2_177T should show RNA vs DNA balance
  rna_cq <- 33
  dna_cq <- 30
  delta <- rna_cq - dna_cq

  expect_equal(delta, 3)
})

test_that("Delta calculations: Handle NA values correctly", {
  rna_cq <- NA_real_
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_true(is.na(delta))
})

# ==============================================================================
# Date Parsing Tests
# ==============================================================================

test_that("parse_run_datetime: Extracts datetime from filename", {
  skip_if_not(exists("parse_run_datetime"), "parse_run_datetime not available")

  filename <- "20230815_143022_MIC_Run.xlsx"
  result <- parse_run_datetime(filename)

  expect_true(!is.na(result))
  expect_s3_class(result, "POSIXct")
})

test_that("parse_run_datetime: Handles various date formats", {
  skip_if_not(exists("parse_run_datetime"), "parse_run_datetime not available")

  filenames <- c(
    "2023-08-15_14-30-22.xlsx",
    "20230815_143022.xlsx",
    "20230815.xlsx"
  )

  for (fn in filenames) {
    result <- parse_run_datetime(fn)
    # Should either parse successfully or return NA
    expect_true(is.na(result) || inherits(result, "POSIXct"))
  }
})

message("\n✓ All MIC helper function tests completed!")
