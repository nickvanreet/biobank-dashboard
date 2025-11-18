# MIC Module Unit Tests

This directory contains comprehensive unit tests for the MIC (qPCR) analysis modules.

## Test Structure

```
tests/
├── testthat.R                          # Main test runner
├── README.md                           # This file
└── testthat/
    ├── test-mic-decision-tree.R        # Decision tree logic tests (9 steps)
    ├── test-mic-helpers.R              # Helper function tests
    └── test-mic-qc-aggregation.R       # QC and aggregation tests
```

## What's Tested

### 1. Decision Tree Logic (`test-mic-decision-tree.R`)
Tests the 9-step decision tree that classifies samples:
- **Step 0**: Control sample handling
- **Step 1**: QC validity check (RNAseP-DNA detection)
- **Step 2**: TNA Positive (≥2 wells with DNA+RNA)
- **Step 3**: Single TNA well → Indeterminate
- **Step 4a**: DNA-only pattern (≥2 wells)
- **Step 4b**: RNA-only pattern (≥2 wells)
- **Step 5**: Late positive TNA
- **Step 6a**: Single outlier with negative evidence
- **Step 6b**: Weak mixed signals
- **Step 7**: Clear negative (≥3 negative replicates)
- **Step 8**: Insufficient data

Also tests:
- Confidence scoring (High/Medium/Low)
- Conflict detection (well-level vs aggregate disagreements)
- Edge cases and boundary conditions

### 2. Helper Functions (`test-mic-helpers.R`)
Tests utility functions:
- `classify_target_vectorized()`: Cq → Positive/Negative/LatePositive
- `normalize_id()`: ID standardization
- `normalize_field_name()`: Column name normalization
- `safe_median()`, `safe_mean()`, `safe_sd()`: Robust statistics
- `match_column_name()`: Flexible column matching
- Date parsing and delta calculations

### 3. QC & Aggregation (`test-mic-qc-aggregation.R`)
Tests quality control and data processing:
- RNA preservation quality assessment (ΔCq)
- Control validation (PC/NC pass/fail)
- Replicate aggregation (median, mean, SD)
- Well-level detection patterns
- Levey-Jennings statistical process control
- Minimum replicate requirements
- Conflict detection logic
- QC pass/fail criteria

## Running the Tests

### From R Console

```r
# Install testthat if needed
install.packages("testthat")

# Run all tests
library(testthat)
test_dir("tests/testthat")

# Run specific test file
test_file("tests/testthat/test-mic-decision-tree.R")
```

### From Command Line

```bash
# Navigate to project root
cd /home/user/biobank-dashboard

# Run all tests
Rscript -e "library(testthat); test_dir('tests/testthat')"

# Run with verbose output
Rscript -e "library(testthat); test_dir('tests/testthat', reporter='progress')"
```

### Using RStudio

1. Open the project in RStudio
2. Go to **Build** → **Test Package**
3. Or run `devtools::test()` in the console

## Test Output Example

```
✓ | F W S  OK | Context
✓ |        32 | MIC Decision Tree
✓ |        28 | MIC Helpers
✓ |        35 | MIC QC and Aggregation

══ Results ════════════════════════════════════════════════════════════
Duration: 2.3 s

[ FAIL 0 | WARN 0 | SKIP 0 | PASS 95 ]
```

## Understanding Test Results

- ✓ **PASS**: Test passed successfully
- ✗ **FAIL**: Test failed - indicates a bug
- ⚠ **WARN**: Warning - potential issue
- ⊘ **SKIP**: Test skipped (function not available)

## Adding New Tests

When you add new features to the MIC modules:

1. **Create test cases** that cover:
   - Normal operation
   - Edge cases
   - Error conditions
   - Boundary values

2. **Follow naming convention**:
   ```r
   test_that("Feature: Expected behavior", {
     # Arrange
     input <- create_test_data()

     # Act
     result <- your_function(input)

     # Assert
     expect_equal(result, expected_value)
   })
   ```

3. **Use descriptive test names** that explain:
   - What's being tested
   - The expected outcome

## Test Coverage

Current test coverage focuses on:
- ✓ Decision tree logic (all 9 steps)
- ✓ Confidence scoring
- ✓ Conflict detection
- ✓ Helper functions
- ✓ QC metrics
- ✓ Aggregation logic
- ⚠ Integration tests (to be added)
- ⚠ File parsing (to be added)

## Troubleshooting

### Tests fail with "function not found"

The test files check if functions exist before testing:
```r
skip_if_not(exists("function_name"), "function not available")
```

If many tests are skipped, ensure:
1. The MIC module files are properly sourced
2. All required libraries are loaded
3. Functions are exported if in a package

### Tests fail after code changes

This is expected! Tests catch regressions. Either:
1. Fix the code to match the expected behavior
2. Update the test if the expected behavior changed

## Best Practices

1. **Run tests before committing** - Catch bugs early
2. **Write tests for bugs** - Prevent regressions
3. **Keep tests independent** - Each test should work alone
4. **Use meaningful assertions** - Clear error messages
5. **Test edge cases** - NA, empty, boundary values

## Questions?

- Review the test files for examples
- Check testthat documentation: https://testthat.r-lib.org/
- Ask the team for clarification on expected behavior
