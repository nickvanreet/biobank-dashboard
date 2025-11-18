# Extraction Loading Fixes

## Issues Fixed

### 1. Empty Row Detection Bug

**Problem:**
- The `.is_empty_extraction_row()` function was checking ALL columns in the dataframe, including the `source_file` metadata column
- Since `source_file` is always populated during the load process, rows with no actual extraction data but a source_file value were not being filtered out
- This caused many empty rows to be counted in the final dataset

**Solution:**
- Modified the function to explicitly check only **core data columns** (identifiers and key extraction fields)
- Added explicit exclusion of metadata columns (`source_file`, `has_cn`, `is_duplicate`, `dup_group`, etc.)
- Now correctly filters rows where all core fields are empty, regardless of metadata column values
- Added `safe_col()` helper to safely access columns that may not exist

**Changes:**
- Core columns reduced to essential fields: `barcode`, `numero`, `sample_id`, `record_number`, `extraction_date`, `drs_volume_ml`
- Two-tier check: (1) core fields empty, OR (2) all non-metadata fields empty
- Better handling of missing columns

### 2. Duplicate Detection Improvements

**Problem:**
- Identifiers were not normalized before comparison, leading to false duplicates/non-duplicates due to case differences or whitespace
- All rows with NA identifiers were grouped together, though they weren't marked as duplicates (correct behavior, but inefficient)
- Less clear logic flow

**Solution:**
- Added identifier normalization: trim whitespace and convert to lowercase before comparison
- Improved grouping logic to only assign `dup_group` when there are actual duplicates
- Added `group_size` tracking for clearer logic
- Better handling of edge cases (rows with no identifiers)

**Changes:**
- Created normalized versions of each identifier field (`barcode_norm`, `numero_norm`, etc.)
- Updated grouping logic to only mark rows as duplicates when:
  1. Identifier is not NA/empty
  2. There are multiple rows with the same identifier
- Improved `most_recent` flag calculation

### 3. Enhanced Logging

**Problem:**
- Limited visibility into what happens during the loading process
- Hard to debug when counts seem wrong

**Solution:**
- Added detailed logging messages showing:
  - Number of empty rows removed
  - Final count of duplicates and duplicate groups
  - Number of records with "CN" mentions
- Messages appear during load to help track processing

**Example Output:**
```
Loading 34 extraction files...
Removed 127 empty rows (1542 records remain)
Loaded 1542 extraction records (48 duplicates in 22 groups, 15 with CN)
```

## Impact

These fixes ensure:
1. **Accurate counts**: Empty rows are properly excluded from totals
2. **Correct duplicate detection**: Case and whitespace variations are normalized
3. **Better debugging**: Clear messages show what's happening during load
4. **More robust**: Handles missing columns and edge cases gracefully

## Testing Recommendations

After these changes, you should see:
- Lower total extraction counts (empty rows removed)
- More accurate duplicate detection
- Clear log messages during data load

If you want to verify the changes are working correctly, check:
1. The console output when loading extractions (look for the summary messages)
2. The "Total Samples" KPI on the Extraction Quality dashboard
3. The "CN Mentions" count should be accurate
4. Duplicate detection should properly identify records with the same barcode/numero
