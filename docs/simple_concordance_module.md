# Simple Concordance Module

## Overview

The Simple Concordance module provides a streamlined, qualitative approach to comparing diagnostic test results based on simple positive/negative classification. This module was created to replace the overly complex concordance analysis module with a more practical and maintainable solution.

## Key Features

### 1. **Binary Classification**
All tests are simplified to positive/negative results:
- **MIC qPCR**: Positive/Negative (from FinalCall field)
- **ELISA-PE**: Positive if PP% ≥ 20% OR DOD ≥ 0.3
- **ELISA-VSG**: Positive if PP% ≥ 20% OR DOD ≥ 0.3
- **iELISA-L13**: Positive based on threshold (from positive_L13 field)
- **iELISA-L15**: Positive based on threshold (from positive_L15 field)

### 2. **Supported Test Pairs**
- ELISA-PE vs ELISA-VSG
- MIC vs ELISA-PE
- MIC vs ELISA-VSG
- iELISA-L13 vs ELISA-PE
- iELISA-L13 vs ELISA-VSG
- iELISA-L15 vs ELISA-PE
- iELISA-L15 vs ELISA-VSG

### 3. **Simple Metrics**
- Total matched samples
- Overall agreement percentage
- Cohen's Kappa with interpretation
- Confusion matrix (2×2 table)
- Concordant counts (both positive, both negative)
- Discordant counts (test1 only, test2 only)

### 4. **Beautiful Visualizations**
- **Confusion Matrix Heatmap**: Interactive plotly heatmap showing test result concordance
- **Agreement Bar Chart**: Visual breakdown of concordant vs discordant results
- **Stratified Analysis**: Optional stratification by province or health zone

### 5. **Data Export**
- Excel export with multiple sheets:
  - Summary metrics
  - Confusion matrix
  - Complete matched sample data
  - Stratified results (if applicable)

## Files Created

1. **R/modules/mod_simple_concordance.R** (~550 lines)
   - Main module UI and server functions
   - Clean, focused interface
   - Reactive data handling

2. **R/utils_simple_concordance.R** (~300 lines)
   - `calculate_simple_concordance()`: Calculate all metrics
   - `plot_simple_confusion_matrix()`: Interactive confusion matrix
   - `plot_agreement_bars()`: Agreement visualization
   - `plot_stratified_agreement()`: Stratified analysis plots

## Usage

The module is automatically integrated into the dashboard and appears in the navigation bar as **"Simple Concordance"**.

### Steps:
1. Select a test pair from the dropdown
2. (Optional) Enable QC filtering to exclude failed samples
3. (Optional) Stratify results by province or health zone
4. View metrics, visualizations, and confusion matrix
5. Download results as Excel file

## Technical Details

### Data Matching
The module reuses existing matching functions:
- `match_elisa_samples()`: For ELISA-PE vs ELISA-VSG
- `match_mic_elisa()`: For MIC vs ELISA comparisons
- `match_ielisa_elisa()`: For iELISA vs ELISA comparisons

### Metrics Calculation
Cohen's Kappa is calculated as:
```
κ = (p_o - p_e) / (1 - p_e)

where:
p_o = observed agreement
p_e = expected agreement by chance
```

Interpretation scale:
- < 0: Poor
- 0-0.20: Slight
- 0.20-0.40: Fair
- 0.40-0.60: Moderate
- 0.60-0.80: Substantial
- 0.80-1.00: Almost Perfect

## Advantages Over Complex Module

| Feature | Simple Concordance | Old Complex Module |
|---------|-------------------|-------------------|
| Lines of code | ~850 | ~1440+ across multiple files |
| Visualizations | 3 focused plots | 10+ complex plots |
| Machine learning | ❌ No | ✅ Yes (Random Forest, XGBoost) |
| ROC curves | ❌ No | ✅ Yes |
| Bland-Altman | ❌ No | ✅ Yes |
| Bootstrap CIs | ❌ No | ✅ Yes |
| Focus | Qualitative agreement | Quantitative predictions |
| Maintainability | ⭐⭐⭐⭐⭐ High | ⭐⭐ Low |
| User friendliness | ⭐⭐⭐⭐⭐ Very clear | ⭐⭐ Overwhelming |
| Loading speed | ⭐⭐⭐⭐⭐ Fast | ⭐⭐⭐ Slow |

## Integration

The module is integrated into the dashboard via:

1. **global.R** (lines 186-188):
```r
# Simple Concordance module (qualitative positive/negative comparison)
source("R/utils_simple_concordance.R")
source("R/modules/mod_simple_concordance.R")
```

2. **app.R** UI (line 57):
```r
mod_simple_concordance_ui("simple_concordance"),
```

3. **app.R** Server (lines 154-162):
```r
mod_simple_concordance_server(
  "simple_concordance",
  biobank_df = data$clean_data,
  mic_df = mic_data$qpcr_samples,
  elisa_pe_df = elisa_pe_data$samples,
  elisa_vsg_df = elisa_vsg_data$samples,
  ielisa_df = ielisa_data$samples
)
```

## Future Enhancements

Potential additions (if needed):
- Temporal trends (concordance over time)
- Multi-way comparison (3+ tests simultaneously)
- Export to PDF report
- Customizable positivity thresholds

## Author
Created as part of the biobank dashboard rebuild to provide a simpler, more maintainable concordance analysis solution.
