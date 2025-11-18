# Sample Count Diagnostic Report

## Current Dashboard Statistics
- **Files with Barcodes**: 33
- **Total Samples**: 1,341
- **Linked to Biobank**: 490
- **Average**: ~40.6 samples per file

## Is This Normal?

### ✅ This is EXPECTED if:
1. **You extract 30-50 samples per day** - Each file is a daily extraction form, so 33 days × 40 samples = ~1,320 samples
2. **Files contain individual sample rows** - Each row in the Excel file is one sample extraction
3. **No duplicate data across files** - Each file covers a different day/batch

### ⚠️ This is SUSPICIOUS if:
1. **You expect fewer samples** - E.g., if you only process 5-10 samples per day
2. **Files might overlap** - Same samples appearing in multiple files
3. **Files were exported multiple times** - Duplicate exports of the same data

## How to Verify Your Data

### Step 1: Run the Diagnostic Script

Open your R/RStudio and run:
```r
source("scripts/diagnose_sample_counts.R")
```

This will show you:
- Exact row count for each Excel file
- How many empty rows were removed
- How many duplicates were removed
- Samples per source file after processing

### Step 2: Check a Sample File Manually

1. Open one of your extraction files: `data/extractions/251111 Fiche d'extraction d'échantillons.xlsx`
2. Count the rows with actual data (excluding headers and empty rows)
3. Does it have ~40 rows of sample data?

### Step 3: Look for Duplicates

Check if the same barcode/sample appears in multiple files:
```r
# In R console:
source("R/core/extraction_data_utils.R")
df <- load_all_extractions("data/extractions")

# Check for barcodes that appear multiple times
library(dplyr)
df %>%
  filter(!is.na(barcode)) %>%
  group_by(barcode) %>%
  summarise(
    count = n(),
    files = paste(unique(source_file), collapse = ", ")
  ) %>%
  filter(count > 1) %>%
  arrange(desc(count))
```

## What the System Does

The loading process (`R/core/extraction_data_utils.R`):

1. **Loads 33 Excel files** from `data/extractions/`
2. **Combines all rows** into one dataset
3. **Removes empty rows** (rows with no barcode, sample_id, date, etc.)
4. **Detects duplicates** by matching:
   - Barcode (primary)
   - Numero
   - Sample_id
   - Record_number
5. **Removes duplicates** (keeps most recent by extraction date)
6. **Counts remaining rows** = Total Samples

## Recent Fixes

Two important fixes were recently applied:
- **Nov 18, 2025** - Fixed empty row detection (was counting metadata-only rows)
- **Nov 18, 2025** - Fixed duplicate removal (now actually removes, not just marks)

These fixes are already in your current code.

## Recommended Next Steps

### If you're not sure whether 1,341 is correct:

1. **Manual spot check**: Open 2-3 Excel files and count rows
2. **Check your lab records**: How many samples did you process in the time period covered by these 33 files?
3. **Run the diagnostic**: Execute `scripts/diagnose_sample_counts.R`

### If 1,341 seems too high:

Possible issues to investigate:
- Files exported multiple times (duplicate data)
- Files contain test/practice data mixed with real data
- Multiple tabs in Excel files being counted
- Foreign key relationships being flattened (many-to-many creating extra rows)

### If you want more detailed analysis:

I can add features to show:
- Sample counts by date range
- Which files have unusually high/low counts
- Barcode distribution analysis
- Duplicate patterns

Let me know what you'd like to investigate!
