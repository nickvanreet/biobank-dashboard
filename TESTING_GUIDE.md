# 🧪 Testing Guide for Modular ELISA Implementation

This guide will help you test the new modular ELISA-PE and ELISA-VSG processing pipeline.

---

## ✅ Pre-Test Validation

All modules have passed syntax validation:
- ✅ 11 modules validated
- ✅ No syntax errors detected
- ✅ Balanced braces and parentheses
- ✅ Dependencies properly structured

---

## 🚀 How to Test

### **Option 1: Test via Shiny App** (Recommended)

The modular pipeline is now integrated and will be used automatically when you launch the app.

#### Steps:

1. **Clear the ELISA cache** (to force fresh processing):
   ```r
   source("R/utils_elisa.R")
   clear_elisa_cache(clear_rds = TRUE)
   ```

2. **Launch the Shiny app**:
   ```r
   shiny::runApp()
   ```

3. **Navigate to ELISA-PE or ELISA-VSG tabs**

4. **Check for:**
   - ✅ Data loads successfully
   - ✅ Sample counts match expected values
   - ✅ Positive/Negative/Borderline classifications appear
   - ✅ QC threshold settings button works
   - ✅ Plots and tables display correctly

---

### **Option 2: Direct Testing** (Command Line)

Test individual files without launching the full app:

```r
# Load the modular processing functions
source("R/modules/elisa_shared/process_elisa_modular.R")

# Test ELISA-PE file
pe_result <- process_elisa_file_modular(
  "data/elisa_pe/250507 ELISA indirect pre-coated plaque (1 plaque).xlsx"
)

# Check results
head(pe_result)
table(pe_result$status_final)

# Test ELISA-VSG file
vsg_result <- process_elisa_file_modular(
  "data/elisa_vsg/250709 Résultats indirect ELISA vF.6.xlsx"
)

# Check results
head(vsg_result)
table(vsg_result$status_final)
unique(vsg_result$plate_num)  # Should show 4 plates
```

---

### **Option 3: Run Test Script**

Use the provided test script:

```r
source("test_modular_elisa.R")
```

This will test:
1. ✅ ELISA-PE processing
2. ✅ ELISA-VSG processing
3. ✅ Legacy format conversion
4. ✅ Standardized output validation

---

## 🔍 What to Check

### **1. Data Loading**

**Expected behavior:**
- Console shows: `"✓ Loaded ELISA modular pipeline from ..."`
- Console shows: `"Processing X file(s) using modular pipeline"`
- Console shows progress: `"📥 INGESTION (PE/VSG)"`, `"🔍 QC VALIDATION"`, etc.

**What to verify:**
- [ ] No error messages during loading
- [ ] File paths are correct
- [ ] Number of files matches expectations

---

### **2. ELISA-PE Processing**

**Expected behavior:**
- Single-plate format detected
- Console shows: `"Detected single-plate format"`
- Test type: `"ELISA-PE"`

**What to verify:**
- [ ] Plate numbers assigned correctly
- [ ] Samples and controls separated properly
- [ ] PP% and DOD calculated
- [ ] Status classifications: Positive/Negative/Borderline/Invalid
- [ ] QC flags appear for samples with high CV

**Sample data check:**
```r
# Count samples by status
table(pe_result$status_final[pe_result$sample_type == "sample"])

# Check QC validity
table(pe_result$run_valid)

# Check metrics are present
summary(pe_result$PP_percent)
summary(pe_result$DOD)
```

---

### **3. ELISA-VSG Processing**

**Expected behavior:**
- 4-plate format detected
- Console shows: `"Detected 4-plate format"`
- Test type: `"ELISA-VSG"`
- 4 unique `plate_num` values

**What to verify:**
- [ ] All 4 plates processed
- [ ] Samples mapped to correct plates
- [ ] Each plate has independent QC validation
- [ ] Status classifications work across plates

**Sample data check:**
```r
# Check plate distribution
table(vsg_result$plate_num)

# Check samples per plate
vsg_result %>%
  filter(sample_type == "sample") %>%
  group_by(plate_num) %>%
  summarise(n_samples = n_distinct(sample_id))

# Check QC per plate
vsg_result %>%
  distinct(plate_num, run_valid)
```

---

### **4. QC Threshold Configuration**

**What to test:**
1. Open ELISA-PE or ELISA-VSG tab
2. Click **"QC Thresholds"** button
3. Change thresholds (e.g., set Positive Control Min OD to 0.6)
4. Click **"Apply"**
5. Check that plates are re-validated based on new thresholds

**What to verify:**
- [ ] Modal dialog opens
- [ ] Current threshold values displayed
- [ ] Changes applied successfully
- [ ] Notification shows: "Settings updated successfully"
- [ ] Data refreshes with new validation

**Console check:**
```r
# You should see:
# "DEBUG: Recalculated plate validation with custom thresholds"
```

---

### **5. Standardized Output Format**

**Expected columns:**
```r
expected_cols <- c(
  "sample_id", "test_type", "run_id", "run_valid", "sample_valid",
  "status_final", "status_raw", "n_runs", "metric_1", "metric_2",
  "PP_percent", "DOD", "qc_flags", "file_origin", "test_date"
)

# Check all columns present
all(expected_cols %in% names(pe_result))  # Should be TRUE
```

**What to verify:**
- [ ] All expected columns present
- [ ] `sample_id` populated (barcodes or lab IDs)
- [ ] `test_type` is "ELISA-PE" or "ELISA-VSG"
- [ ] `run_id` unique per plate
- [ ] `run_valid` and `sample_valid` are TRUE/FALSE
- [ ] `status_final` is one of: Positive/Negative/Borderline/Invalid
- [ ] `metric_1` = PP%, `metric_2` = DOD
- [ ] `qc_flags` shows warnings for samples with high CV

---

### **6. Biobank Linkage & Global Filters** 🔗

**CRITICAL:** The modular pipeline must preserve biobank demographic data so global filters work correctly.

**What to test:**

1. **Launch the app** and navigate to ELISA-PE or ELISA-VSG tabs

2. **Check biobank columns are present:**
   ```r
   # After data loads, verify biobank columns exist
   biobank_cols <- c("Province", "HealthZone", "Structure", "Sex",
                     "Age", "Cohort", "BiobankMatched")

   # Check columns present
   all(biobank_cols %in% names(elisa_data))  # Should be TRUE
   ```

3. **Test global filters** in the side panel:
   - [ ] **Province filter** - Select a province → verify data filters correctly
   - [ ] **Health Zone filter** - Select a zone → verify data filters correctly
   - [ ] **Structure filter** - Select a structure → verify data filters correctly
   - [ ] **Cohort filter** - Select a cohort → verify data filters correctly
   - [ ] **Date range filter** - Adjust dates → verify data filters correctly

4. **Verify biobank match rate** in console:
   ```
   ✓ Matched 285/320 records to biobank (89.1%)
   ```

**Sample data check:**
```r
# View biobank linkage for samples
elisa_data %>%
  filter(sample_type == "sample") %>%
  select(sample_id, numero_labo, code_barres_kps,
         Province, HealthZone, BiobankMatched) %>%
  head(20)
```

**What to verify:**
- [ ] Biobank columns present in final dataset
- [ ] Match rate displayed in console during data loading
- [ ] Global filters modify the visible data correctly
- [ ] Unmatched samples still appear (BiobankMatched = FALSE)
- [ ] Demographic columns show NA for unmatched samples
- [ ] Filter combinations work correctly (e.g., Province + Cohort)

**Expected behavior:**
- Samples matched by `code_barres_kps` (barcode) first
- If no barcode match, tries `numero_labo` (lab ID)
- Matched samples get: Province, HealthZone, Structure, Sex, Age, etc.
- Unmatched samples: BiobankMatched = FALSE, demographics = NA
- Global filters apply to matched samples only

---

### **7. Backward Compatibility**

The new pipeline maintains full backward compatibility with existing coordinators.

**What to verify:**
- [ ] ELISA-PE coordinator displays data correctly
- [ ] ELISA-VSG coordinator displays data correctly
- [ ] Runs tab shows plates
- [ ] Samples tab shows individual samples
- [ ] Analysis tab shows QC plots
- [ ] No broken visualizations or missing data

**Expected behavior:**
- Legacy columns still present: `plate_number`, `sample_positive`, `plate_valid`
- Data structure matches old format
- Existing plots work without modification

---

## 🐛 Troubleshooting

### **Issue: "ELISA modular processing script not found"**

**Solution:**
```bash
# Check file exists
ls -la R/modules/elisa_shared/process_elisa_modular.R

# If missing, ensure you're on the correct branch
git branch
git checkout claude/streamline-test-modules-014WZHgN8zS97ASWhXjRtkRy
```

---

### **Issue: "Object not found" errors**

**Cause:** Missing dependencies or incorrect sourcing

**Solution:**
```r
# Ensure all packages installed
install.packages(c("readxl", "dplyr", "tidyr", "purrr", "janitor", "digest"))

# Clear environment and reload
rm(list=ls())
.rs.restartR()  # In RStudio
```

---

### **Issue: Cache shows old data**

**Solution:**
```r
# Clear all ELISA caches
source("R/utils_elisa.R")
clear_elisa_cache(clear_rds = TRUE)

# Then reload the app
shiny::runApp()
```

---

### **Issue: QC threshold changes don't apply**

**Cause:** Coordinat or not recalculating validation

**Solution:**
- Check console for: `"DEBUG: Recalculated plate validation"`
- If missing, check `mod_elisa_coordinator.R` is using `elisa_data_typed()` reactive

---

### **Issue: "Plaque markers not found" for VSG files**

**Cause:** File may not have 4-plate format markers

**Check:**
```r
# Manually check if file has "Plaque 1:" markers
library(readxl)
raw <- read_excel("path/to/file.xlsx", sheet = "450 nm - 600 nm", col_names = FALSE)
any(grepl("Plaque \\d+:", raw[[1]], ignore.case = TRUE))
```

**Solution:** File might be PE format in VSG directory - move to correct directory

---

## 📊 Expected Console Output

When processing runs successfully, you should see:

```
✓ Loaded ELISA modular pipeline from R/modules/elisa_shared/process_elisa_modular.R
Loading 5 ELISA file(s) (using RDS cache where available)...

═══════════════════════════════════════════════
Processing ELISA-PE: 250507 ELISA indirect pre-coated plaque.xlsx
═══════════════════════════════════════════════
📥 INGESTION (PE): 250507 ELISA indirect pre-coated plaque.xlsx
  ✓ Read OD grid: 96 wells
  ✓ Mapped 32 samples, 4 controls
  ✓ Ingestion complete: 32 samples, 1 plate(s)
🔍 QC VALIDATION (PE): 250507 ELISA indirect pre-coated plaque
  ✓ Summarized 36 entries (samples + controls)
  ✓ QC complete: 1 valid plates, 0 invalid plates
📊 INTERPRETATION (PE)
  ✓ Calculated PP% and classified 36 entries
  ✓ Sample classifications: Positive=8, Negative=22, Borderline=2, Invalid=0
  ✓ Interpretation complete: 32 unique samples
📤 OUTPUT (PE): Creating standardized format
  ✓ Created standardized output: 36 rows
  ✓ Summary: 8 Positive, 22 Negative, 2 Borderline, 0 Invalid
═══════════════════════════════════════════════
✅ ELISA-PE processing complete
═══════════════════════════════════════════════
```

---

## ✅ Success Criteria

The implementation is working correctly if:

### **Data Loading**
- ✅ No error messages during module loading
- ✅ Console shows modular pipeline being used
- ✅ Files processed without errors

### **ELISA-PE**
- ✅ Single-plate format detected
- ✅ Samples and controls separated
- ✅ PP% and DOD calculated
- ✅ Status classifications assigned
- ✅ QC flags present for invalid samples

### **ELISA-VSG**
- ✅ 4-plate format detected
- ✅ All 4 plates processed independently
- ✅ Samples mapped to correct plates
- ✅ Per-plate QC validation

### **QC Configuration**
- ✅ QC Thresholds button opens modal
- ✅ Threshold changes apply successfully
- ✅ Plates re-validated with new settings
- ✅ Notification confirms update

### **Output Format**
- ✅ All standardized columns present
- ✅ Data types correct (logical, character, numeric)
- ✅ No NA values in critical columns
- ✅ Legacy columns maintained for compatibility

### **UI Integration**
- ✅ All ELISA tabs load without errors
- ✅ Tables display correctly
- ✅ Plots render without errors
- ✅ Filters work as expected
- ✅ No console errors in browser DevTools

---

## 📝 Reporting Issues

If you encounter problems, please provide:

1. **Console Output:** Full error message and stack trace
2. **Environment Info:**
   ```r
   sessionInfo()
   ```
3. **File Info:** Which ELISA file caused the issue
4. **Steps to Reproduce:** Exact steps that led to the error
5. **Expected vs Actual:** What you expected vs what happened

---

## 🎉 Next Steps After Testing

Once testing is successful:

1. ✅ **Phase 1 Complete:** ELISA-PE and ELISA-VSG modular architecture working
2. 🚀 **Phase 2:** Apply same architecture to MIC (rename functions for consistency)
3. 🚀 **Phase 3:** Apply to iELISA (requires UI changes for formula selection)
4. 🚀 **Phase 4:** Update dashboard to consume standardized format directly

---

## 📚 Reference

- **Architecture Documentation:** `MODULAR_ARCHITECTURE_SUMMARY.md`
- **Validation Script:** `validate_modules.R`
- **Test Script:** `test_modular_elisa.R`
- **Module Directory:** `R/modules/elisa_pe/`, `R/modules/elisa_vsg/`, `R/modules/elisa_shared/`

---

**Happy Testing! 🧪✨**

If everything works as expected, you should see improved performance, cleaner code, and configurable QC thresholds!
