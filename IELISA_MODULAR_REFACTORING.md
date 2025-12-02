# iELISA Modular Architecture Refactoring

## Summary

Successfully refactored iELISA processing to use a modular 4-step pipeline architecture, matching the design pattern used for ELISA-PE and ELISA-VSG modules.

## Changes Made

### 1. New Modular Pipeline (`R/modules/ielisa/`)

Created 4-step processing modules following the established pattern:

#### **Step 1: Ingestion** (`ingest_ielisa.R`)
- Reads iELISA-specific Excel format:
  - OD values from `"450-600 nm"` sheet
  - Sample metadata from `"ECHANTILLONS"` sheet
- Extracts controls from fixed positions (row 13)
- Maps samples to wells using iELISA-specific algorithm
- Handles dual antigen testing (LiTat 1.3 and LiTat 1.5)

#### **Step 2: QC Validation** (`qc_ielisa.R`)
- Validates control performance per spec:
  - **NEG control OD**: 1.000 < OD < 3.100
  - **POS control inhibition**: ≥ 30% (using Formula 1)
  - **Control CV**: < 20%
- Calculates control statistics (means, CVs)
- Flags invalid plates (separate validation for L13 and L15)

#### **Step 3: Interpretation** (`interpret_ielisa.R`)
- Calculates % inhibition using TWO formulas:
  - **Formula 1 (NEG-based, OFFICIAL)**: `% inh = 100 × (1 - OD_sample / OD_NEG)`
  - **Formula 2 (NEG-POS normalized)**: `% inh = 100 × (OD_NEG - OD_sample) / (OD_NEG - OD_POS)`
- Classifies samples as positive/negative (threshold: 30%)
- **Positivity rule**: Sample is positive if **EITHER** LiTat 1.3 **OR** LiTat 1.5 ≥ 30% inhibition
- Flags formula disagreements (QC metric)

#### **Step 4: Output Formatting** (`output_ielisa.R`)
- Standardizes results for dashboard compatibility
- Includes all control data, sample results, and QC flags
- Provides `process_ielisa_file()` wrapper for complete pipeline

### 2. Updated Core Infrastructure

#### **Shared Processing** (`elisa_shared/process_elisa_modular.R`)
- Enhanced file type detection to recognize iELISA files:
  - Path-based detection (`ielisa` directory)
  - Sheet-based detection (`ECHANTILLONS` + `450-600 nm` sheets)
- Routes iELISA files to appropriate pipeline
- Supports iELISA-specific parameters (threshold, formula)

#### **Utilities** (`R/utils_ielisa.R`)
- Complete rewrite to use modular pipeline
- Maintained backward compatibility with existing API
- Functions now call modular components internally:
  - `parse_ielisa_file()` → 4-step pipeline
  - `parse_ielisa_folder()` → batch processing
  - `load_ielisa_data()` → cached loading with modular pipeline
- Updated cache file naming: `ielisa_modular_*.rds`

### 3. Biobank Integration

- iELISA uses same identifiers as ELISA-PE/VSG:
  - `numero_labo` (Lab ID)
  - `code_barres_kps` (Barcode)
- Compatible with existing `link_elisa_to_biobank()` function
- Automatic linkage when loaded through coordinator

### 4. Coordinator Compatibility

- `mod_ielisa_coordinator.R` requires **NO CHANGES**
- Already calls `load_ielisa_data()`, which now uses modular pipeline
- All existing features work seamlessly:
  - Custom QC thresholds
  - Formula selection (F1 vs F2)
  - Positivity threshold adjustment
  - Retest filtering
  - Invalid plate exclusion

## Key Differences: iELISA vs PE/VSG

| Feature | ELISA-PE/VSG | iELISA |
|---------|--------------|--------|
| **Assay Type** | Direct binding (Ag+/Ag0) | Inhibition assay |
| **Excel Sheets** | "Results" + "Controls" | "450-600 nm" + "ECHANTILLONS" |
| **Sample Layout** | Deepwell column mapping | Fixed row/column algorithm |
| **Controls** | Variable positions | Fixed positions (row 13) |
| **Antigens** | Single antigen per test | Dual antigens (L13 + L15) |
| **Calculation** | PP% and DOD | % Inhibition (2 formulas) |
| **Positivity** | PP% ≥ 20% OR DOD ≥ 0.3 | % Inhibition ≥ 30% (either antigen) |
| **QC Criteria** | Control OD ranges, CV% | NEG OD range, POS inhibition %, CV% |

## Testing

Created test script (`test_ielisa_modular.R`) to verify:
- [x] Single file parsing
- [x] Batch folder processing
- [x] Caching mechanism
- [x] QC validation
- [x] Formula calculations
- [x] Positivity classification

## Benefits

1. **Consistency**: Same 4-step architecture across all ELISA types
2. **Maintainability**: Modular design makes debugging easier
3. **Testability**: Each step can be tested independently
4. **Flexibility**: Easy to adjust QC thresholds, formulas, and thresholds
5. **Transparency**: Clear separation of concerns (ingest → QC → interpret → output)
6. **Performance**: Maintained caching for fast reloads
7. **Backward Compatibility**: Existing code continues to work

## Files Created

```
R/modules/ielisa/
├── ingest_ielisa.R      # Step 1: Data ingestion
├── qc_ielisa.R          # Step 2: QC validation
├── interpret_ielisa.R   # Step 3: Interpretation & classification
└── output_ielisa.R      # Step 4: Output formatting
```

## Files Modified

```
R/modules/elisa_shared/process_elisa_modular.R  # Added iELISA detection & routing
R/utils_ielisa.R                                 # Refactored to use modular pipeline
```

## Migration Notes

- Existing dashboards will continue to work without changes
- Cache files will be regenerated (new naming scheme)
- Old cache files can be safely deleted
- No breaking changes to public API

## Next Steps (Optional Enhancements)

1. Add biobank-enhanced reporting (already compatible)
2. Implement cross-file retest consolidation
3. Add plate-level QC summary visualizations
4. Create iELISA-specific analysis plots
5. Add export functionality for iELISA results

## References

- iELISA Spec: Section 6.1 (Run Validation), 6.2 (Calculation), 6.3 (Cut-off)
- Formula 1 (OFFICIAL): `% inhibition = 100 – (OD sample / OD Negative control × 100)`
- Formula 2 (Alternative): Linear normalization between NEG (0%) and POS (100%)
