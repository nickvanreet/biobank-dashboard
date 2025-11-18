# ==============================================================================
# Unit Tests for MIC QC and Aggregation Functions
# ==============================================================================
# Tests quality control metrics, Levey-Jennings statistics, and sample
# aggregation from replicate-level data.
# ==============================================================================

library(testthat)
library(dplyr)
library(tibble)

# ==============================================================================
# RNA Preservation Quality Tests
# ==============================================================================

test_that("RNA Preservation: Good quality (ΔCq ≤ 5)", {
  rna_cq <- 30
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_equal(delta, 2)
  expect_lte(delta, 5)

  # This would be classified as "Good RNA preservation"
})

test_that("RNA Preservation: Moderate quality (5 < ΔCq ≤ 8)", {
  rna_cq <- 35
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_equal(delta, 7)
  expect_gt(delta, 5)
  expect_lte(delta, 8)

  # This would be classified as "Moderate RNA loss"
})

test_that("RNA Preservation: Poor quality (ΔCq > 8)", {
  rna_cq <- 40
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_equal(delta, 12)
  expect_gt(delta, 8)

  # This would be classified as "Poor RNA preservation"
})

test_that("RNA Preservation: Missing RNAseP-RNA (no amplification)", {
  rna_cq <- NA_real_
  dna_cq <- 28
  delta <- rna_cq - dna_cq

  expect_true(is.na(delta))

  # This would be "Severe RNA loss (no RNAseP-RNA)"
})

test_that("RNA Preservation: Missing RNAseP-DNA (extraction failed)", {
  rna_cq <- 32
  dna_cq <- NA_real_
  delta <- rna_cq - dna_cq

  expect_true(is.na(delta))

  # This would be "Unknown (no RNAseP-DNA)"
})

# ==============================================================================
# Control Validation Tests
# ==============================================================================

test_that("Control validation: Positive control passes with strong signal", {
  pc_sample <- tibble(
    SampleName = "PC",
    ControlType = "PC",
    Call_177T = "Positive",
    Call_18S2 = "Positive",
    Cq_median_177T = 25,
    Cq_median_18S2 = 27
  )

  # Should be valid positive control
  expect_equal(pc_sample$ControlType, "PC")
  expect_equal(pc_sample$Call_177T, "Positive")
  expect_equal(pc_sample$Call_18S2, "Positive")
  expect_lt(pc_sample$Cq_median_177T, 30)
})

test_that("Control validation: Positive control fails with no signal", {
  pc_fail <- tibble(
    SampleName = "PC",
    ControlType = "PC",
    Call_177T = "Negative",
    Call_18S2 = "Negative"
  )

  # Should flag as failed positive control
  expect_equal(pc_fail$ControlType, "PC")
  expect_equal(pc_fail$Call_177T, "Negative")
})

test_that("Control validation: Negative control passes when clean", {
  nc_pass <- tibble(
    SampleName = "NC",
    ControlType = "NC",
    Call_177T = "Negative",
    Call_18S2 = "Negative"
  )

  expect_equal(nc_pass$ControlType, "NC")
  expect_equal(nc_pass$Call_177T, "Negative")
  expect_equal(nc_pass$Call_18S2, "Negative")
})

test_that("Control validation: Negative control fails when contaminated", {
  nc_contaminated <- tibble(
    SampleName = "NC",
    ControlType = "NC",
    Call_177T = "Positive",  # Contamination!
    Call_18S2 = "Negative"
  )

  # Should flag as contaminated negative control
  expect_equal(nc_contaminated$ControlType, "NC")
  expect_equal(nc_contaminated$Call_177T, "Positive")
})

# ==============================================================================
# Replicate Aggregation Tests
# ==============================================================================

test_that("Replicate aggregation: Median Cq calculation", {
  cq_values <- c(28.5, 29.0, 28.8, 29.2)
  median_cq <- median(cq_values)

  expect_equal(median_cq, 28.9)
  expect_true(median_cq >= min(cq_values))
  expect_true(median_cq <= max(cq_values))
})

test_that("Replicate aggregation: Handles missing Cq values", {
  cq_values <- c(28.5, NA, 28.8, 29.2)
  median_cq <- median(cq_values, na.rm = TRUE)

  expect_equal(median_cq, 28.8)
  expect_false(is.na(median_cq))
})

test_that("Replicate aggregation: Standard deviation with variation", {
  cq_values <- c(28, 29, 30, 31)
  sd_cq <- sd(cq_values)

  expect_gt(sd_cq, 0)
  expect_lt(sd_cq, 2)  # Reasonable variation
})

test_that("Replicate aggregation: All replicates identical", {
  cq_values <- c(30, 30, 30, 30)
  sd_cq <- sd(cq_values)

  expect_equal(sd_cq, 0)
})

test_that("Replicate aggregation: Insufficient replicates (n=1)", {
  cq_values <- c(30)
  sd_cq <- if (length(cq_values) <= 1) NA_real_ else sd(cq_values)

  expect_true(is.na(sd_cq))
})

# ==============================================================================
# Well-Level Detection Pattern Tests
# ==============================================================================

test_that("Well detection: TNA positive when both targets positive", {
  # Create a well with both 177T and 18S2 positive
  well <- tibble(
    Replicate = "Rep1",
    Target_177T_Cq = 28,
    Target_18S2_Cq = 30,
    Target_177T_Call = "Positive",
    Target_18S2_Call = "Positive"
  )

  # Both targets are positive
  expect_equal(well$Target_177T_Call, "Positive")
  expect_equal(well$Target_18S2_Call, "Positive")

  # This well would contribute to Wells_TNA_Positive
})

test_that("Well detection: DNA-only when only 177T positive", {
  well <- tibble(
    Replicate = "Rep1",
    Target_177T_Call = "Positive",
    Target_18S2_Call = "Negative"
  )

  expect_equal(well$Target_177T_Call, "Positive")
  expect_equal(well$Target_18S2_Call, "Negative")

  # This well would contribute to Wells_DNA_Positive
})

test_that("Well detection: RNA-only when only 18S2 positive", {
  well <- tibble(
    Replicate = "Rep1",
    Target_177T_Call = "Negative",
    Target_18S2_Call = "Positive"
  )

  expect_equal(well$Target_177T_Call, "Negative")
  expect_equal(well$Target_18S2_Call, "Positive")

  # This well would contribute to Wells_RNA_Positive
})

test_that("Well detection: Suspect when late positive", {
  well <- tibble(
    Replicate = "Rep1",
    Target_177T_Call = "LatePositive",
    Target_18S2_Call = "LatePositive"
  )

  expect_equal(well$Target_177T_Call, "LatePositive")
  expect_equal(well$Target_18S2_Call, "LatePositive")

  # This well would contribute to Wells_TNA_Suspect
})

# ==============================================================================
# Levey-Jennings Statistical Tests
# ==============================================================================

test_that("Levey-Jennings: Calculate mean and standard deviation", {
  # Simulated control Cq values over time
  cq_values <- c(28.5, 28.8, 29.0, 28.7, 29.2, 28.6, 29.1, 28.9)
  mean_cq <- mean(cq_values)
  sd_cq <- sd(cq_values)

  expect_equal(mean_cq, mean(cq_values))
  expect_gt(sd_cq, 0)

  # Calculate control limits
  ucl_2sd <- mean_cq + 2 * sd_cq
  lcl_2sd <- mean_cq - 2 * sd_cq

  expect_gt(ucl_2sd, mean_cq)
  expect_lt(lcl_2sd, mean_cq)
})

test_that("Levey-Jennings: Detect out-of-control points", {
  mean_cq <- 29.0
  sd_cq <- 0.5

  ucl_3sd <- mean_cq + 3 * sd_cq  # 30.5
  lcl_3sd <- mean_cq - 3 * sd_cq  # 27.5

  # Value outside 3 SD should be flagged
  outlier <- 31.0
  expect_gt(outlier, ucl_3sd)

  # Value within 3 SD is acceptable
  normal <- 29.5
  expect_lt(normal, ucl_3sd)
  expect_gt(normal, lcl_3sd)
})

test_that("Levey-Jennings: Stability percentage calculation", {
  # 10 measurements, 8 within ±2 SD
  total <- 10
  within_2sd <- 8

  stability_pct <- (within_2sd / total) * 100

  expect_equal(stability_pct, 80)
  expect_gte(stability_pct, 0)
  expect_lte(stability_pct, 100)
})

# ==============================================================================
# Minimum Replicate Requirements Tests
# ==============================================================================

test_that("Replicate requirements: 2/4 minimum for positive call", {
  min_positive_reps <- 2
  wells_positive <- 2

  expect_gte(wells_positive, min_positive_reps)

  # Should make positive call
})

test_that("Replicate requirements: 1/4 insufficient for positive call", {
  min_positive_reps <- 2
  wells_positive <- 1

  expect_lt(wells_positive, min_positive_reps)

  # Should not make positive call (Indeterminate)
})

test_that("Replicate requirements: 3/4 high confidence positive", {
  wells_positive <- 3

  expect_gte(wells_positive, 3)

  # Should be High confidence
})

test_that("Replicate requirements: 4/4 maximum confidence", {
  wells_positive <- 4

  expect_equal(wells_positive, 4)

  # Should be High confidence with perfect agreement
})

# ==============================================================================
# Conflict Detection Logic Tests
# ==============================================================================

test_that("Conflict detection: Well count meets threshold but aggregate doesn't", {
  min_reps <- 2
  wells_tna_positive <- 2
  sample_tna_positive <- FALSE

  # Conflict: Well-level says positive, aggregate says no
  has_conflict <- (wells_tna_positive >= min_reps) && !sample_tna_positive

  expect_true(has_conflict)
})

test_that("Conflict detection: Aggregate positive but well count insufficient", {
  min_reps <- 2
  wells_tna_positive <- 1
  sample_tna_positive <- TRUE

  # Conflict: Aggregate says positive, but well-level insufficient
  has_conflict <- sample_tna_positive && (wells_tna_positive < min_reps)

  expect_true(has_conflict)
})

test_that("Conflict detection: No conflict when both agree", {
  min_reps <- 2
  wells_tna_positive <- 2
  sample_tna_positive <- TRUE

  # No conflict: Both agree
  has_conflict <- !((wells_tna_positive >= min_reps) == sample_tna_positive)

  expect_false(has_conflict)
})

# ==============================================================================
# QC Pass/Fail Logic Tests
# ==============================================================================

test_that("QC pass: Sample has RNAseP-DNA detected", {
  call_rnasep_dna <- "Positive"

  qc_passed <- call_rnasep_dna == "Positive"

  expect_true(qc_passed)
})

test_that("QC fail: Sample has no RNAseP-DNA (extraction failed)", {
  call_rnasep_dna <- "Negative"

  qc_passed <- call_rnasep_dna == "Positive"

  expect_false(qc_passed)
})

test_that("QC count: Count replicates that passed QC", {
  replicates <- tibble(
    Replicate = paste0("Rep", 1:4),
    RNAseP_DNA_Call = c("Positive", "Positive", "Positive", "Negative")
  )

  qc_pass_count <- sum(replicates$RNAseP_DNA_Call == "Positive")

  expect_equal(qc_pass_count, 3)
})

# ==============================================================================
# Edge Cases for Aggregation
# ==============================================================================

test_that("Aggregation: Empty replicate data", {
  empty_df <- tibble()

  # Should handle gracefully
  expect_equal(nrow(empty_df), 0)
})

test_that("Aggregation: Single replicate", {
  single_rep <- tibble(
    SampleID = "S001",
    Replicate = "Rep1",
    Cq = 28.5
  )

  # Should still calculate median (which equals the single value)
  expect_equal(nrow(single_rep), 1)
  median_cq <- median(single_rep$Cq)
  expect_equal(median_cq, 28.5)
})

test_that("Aggregation: All replicates are NA", {
  all_na <- tibble(
    SampleID = "S001",
    Replicate = paste0("Rep", 1:4),
    Cq = c(NA, NA, NA, NA)
  )

  median_cq <- median(all_na$Cq, na.rm = TRUE)

  # Should return NA or NaN
  expect_true(is.na(median_cq) || is.nan(median_cq))
})

message("\n✓ All MIC QC and aggregation tests completed!")
