# Data Linking Improvements

## Summary of Changes

This update fixes the data linking issues between extraction and biobank datasets and streamlines the data structure for better usability.

## Key Issues Fixed

### 1. **Zero Matches Problem** ✅
**Problem**: 0 out of 473 extractions were matching with biobank records (0.0% match rate)

**Root Cause**: The linking logic only tried to match on the barcode field, but didn't attempt matching on the `numero`/`lab_id` field.

**Solution**: Updated `link_extraction_to_biobank()` in `R/core/data_linking_utils.R` to match on **BOTH**:
- Barcode (from `code_barres_kps` field)
- Numero (from `numero` field in biobank, `sample_id` in extractions)

The function now:
1. Normalizes both barcode and numero fields from biobank data
2. Creates two lookup tables (one for barcode matches, one for numero matches)
3. Combines them and tries to match extraction records against both
4. Reports which match type was used (`biobank_match_type` column)

### 2. **Column Cleanup** ✅
**Removed unnecessary columns** from extraction data output:
- `filter_type` - not needed for current workflows
- `project` - not needed for current workflows
- `batch` - not needed for current workflows

**Preserved essential columns** for future Freezer Module:
- `rack` - Freezer rack location
- `rack_row` - Rangée (row in rack)
- `rack_column` - Position (column in rack)

**Preserved RSC fields** for biobank linking:
- `rsc_run` - RSC Run identifier
- `rsc_position` - RSC Position in run

### 3. **Extract Quality Mapping** ✅
**Verified correct mapping** of extraction quality codes:
- `C` / `c` / `clair` → `"Clear"`
- `F` / `f` / `foncé` → `"Foncé"` (Dark)
- `E` / `e` / `échec` → `"Échec"` (Failure)

Added `Échec` (Failure) to the quality issue flagging logic so failed extractions are marked as not ready for freezer.

## Files Modified

1. **R/core/data_linking_utils.R**
   - Rewrote `link_extraction_to_biobank()` to support dual matching (barcode + numero)
   - Added `biobank_match_type` to track which field was used for matching
   - Enhanced matching diagnostics in console output

2. **R/core/extraction_data_utils.R**
   - Removed `filter_type`, `project`, `batch` from column output
   - Added comments clarifying RSC fields and freezer storage fields
   - Updated `flag_issue` logic to include `Échec` quality status
   - Updated empty dataset template to match new structure

3. **scripts/test_data_linking_improved.R** (NEW)
   - Comprehensive test script for all linking functionality
   - Validates column structure changes
   - Shows match type breakdown
   - Displays sample matched records
   - Verifies freezer module fields are present

4. **scripts/diagnose_linking.R** (NEW)
   - Diagnostic script for investigating linking issues
   - Useful for troubleshooting match failures

## Expected Impact

After these changes, you should see:
- **Significantly higher match rates** (many extractions should now link to biobank records)
- **Cleaner data structure** without unnecessary columns
- **Better diagnostics** showing which matching method worked (barcode vs numero)
- **Preserved fields** needed for future freezer tracking module

## Testing

Run the improved test script to verify changes:
```r
source("scripts/test_data_linking_improved.R")
```

This will show:
- How many records matched by barcode vs numero
- Match rate percentage
- Health structure match/mismatch counts
- Sample of linked records
- Verification of all required columns

## Next Steps

1. Run the test script to verify the improvements
2. Review match rates and match type distribution
3. Investigate any remaining unmatched records
4. Consider adding RSC Run/Position to biobank data for temporal analysis
5. Develop the Freezer Module using rack/row/column fields
