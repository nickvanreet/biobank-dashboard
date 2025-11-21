# iELISA Module Documentation

## Overview

The iELISA (inhibition ELISA) module is a comprehensive analysis tool for testing samples against **LiTat 1.3** and **LiTat 1.5** antigens. It provides plate-level quality control, sample-level results, duplicate detection, and data export capabilities.

---

## Features

### 1. **Overview Tab (Runs)**
Displays high-level KPIs and plate summaries:

#### Key Performance Indicators (KPIs)
- **Total Files**: Number of iELISA plates processed
- **Valid Runs**: Number of plates that pass QC for both antigens (with percentage)
- **Total Samples**: Total number of samples tested across all plates
- **Positive Samples**: Number of samples with ≥30% inhibition (either antigen)
- **LiTat 1.3 Valid**: Number of plates with valid LiTat 1.3 controls

#### Visualizations
- **Control ODs Over Time**: Track NEG and POS control OD values across plates
  - Green/Blue lines: NEG controls (target range: 1.0-3.0)
  - Red/Orange lines: POS controls (target range: 0.3-0.7)
- **Control CVs Over Time**: Monitor coefficient of variation for controls (target: <20%)
- **Plate QC Heatmap**: Visual representation of plate validity (green=PASS, red=FAIL)

#### Plate Summary Table
Interactive table with:
- File name and date
- Sample counts
- Control OD values and CVs
- QC pass/fail status for each antigen
- Sample-level QC pass rates

---

### 2. **Samples Tab**
Detailed sample-level results:

- Filterable table using global filters from the coordinator
- No local filters - all filtering done at the module level

#### Sample Results Table
Comprehensive table showing:
- LabID and Barcode
- OD values for both antigens
- Inhibition percentages using both formulas (F1 and F2)
- Positivity status (✓ POS / ✗ NEG)
- Formula agreement (Δ F1-F2)
- Duplicate flags (🔄 DUP, ⚠️ BC for barcode conflicts, ⚠️ LID for LabID conflicts)

#### Visualizations
- **Inhibition Distribution**: Histogram showing inhibition percentage distribution with 30% positivity threshold
- **Formula Agreement**: Scatter plots comparing Formula 1 vs Formula 2 for each antigen
- **Inhibition vs OD**: Scatter plots showing relationship between OD values and inhibition percentages (color-coded by QC status)

---

### 3. **Analysis & Duplicates Tab**
Advanced analysis of duplicate testing and data quality:

#### Duplicate Testing KPIs
- **Samples Tested Multiple Times**: Count of samples with replicate tests
- **Total Duplicate Tests**: Total number of replicate measurements
- **LiTat 1.3 Concordance**: Percentage of duplicates with concordant results
- **LiTat 1.5 Concordance**: Percentage of duplicates with concordant results

#### Duplicate Summary Table
Shows samples tested multiple times with:
- LabID and Barcode
- Number of tests
- File names
- Mean inhibition, SD, and CV for each antigen
- Concordance status (whether all tests agree on positive/negative)

#### Data Quality Conflicts
Automatic detection and reporting of:
- **Barcode Conflicts**: Same LabID appearing with different Barcodes
- **LabID Conflicts**: Same Barcode appearing with different LabIDs

#### Duplicate Agreement Plots
Scatter plots comparing first vs. second test results for paired duplicates:
- Points on the diagonal indicate perfect agreement
- Dashed red lines indicate the 30% positivity threshold
- Useful for assessing test reproducibility

#### Data Export
Three export options:
- **Full Dataset**: Export all iELISA data
- **Negative Samples**: Export only samples that are negative (below threshold)
- **Duplicates Only**: Export only duplicate samples

---

## Technical Details

### Data Structure

Each iELISA Excel file contains:
- **Sheet "450-600 nm"**: OD values in 96-well plate layout
- **Sheet "ECHANTILLONS"**: Sample metadata (LabID, Barcode)

### Control Wells (Row 13)
- **LiTat 1.3 NEG**: B13, C13
- **LiTat 1.3 POS**: D13, E13
- **LiTat 1.5 NEG**: H13, I13
- **LiTat 1.5 POS**: J13, K13

### Inhibition Formulas

**Formula 1** (simple NEG-based):
```
% Inhibition = 100 × (1 - OD_sample / OD_NEG_mean)
```

**Formula 2** (linear between NEG and POS):
```
% Inhibition = 100 × (OD_NEG_mean - OD_sample) / (OD_NEG_mean - OD_POS_mean)
```

### Quality Control Criteria

#### Plate-Level QC (Control Validation)
A plate is valid if ALL of the following are met:
- NEG control OD: 1.0 - 3.0
- POS control OD: 0.3 - 0.7
- POS control inhibition: 50% - 80%
- Control CVs: < 20%
- Formula 1 and Formula 2 agreement: < 25% difference

#### Sample-Level Positivity
A sample is considered positive if:
- **Inhibition (Formula 2) ≥ 30%**

Note: "QC" in this module refers to plate-level control validation, not sample positivity. Sample results are reported as positive (POS) or negative (NEG) based on the inhibition threshold.

### Caching
Data is cached using MD5 hashing for performance:
- Cache location: `data/ielisa_cache/`
- Cache is invalidated automatically when files change
- Use "Refresh Data" button to reload

---

## Usage

1. **Data Preparation**: Place iELISA Excel files in `data/ielisa/` directory
2. **Launch Dashboard**: Navigate to the iELISA tab in the main dashboard
3. **Review Overview**: Check KPIs and plate validity
4. **Analyze Samples**: Use filters to explore specific samples
5. **Check Duplicates**: Review duplicate testing concordance
6. **Export Results**: Download full or filtered datasets

---

## File Organization

```
R/
├── utils_ielisa.R                    # Data parsing and caching utilities
└── modules/
    ├── mod_09_ielisa.R               # Module wrapper
    ├── mod_ielisa_coordinator.R      # Coordinator module
    ├── mod_ielisa_runs.R             # Overview/runs sub-module
    ├── mod_ielisa_samples.R          # Samples sub-module
    └── mod_ielisa_analysis.R         # Analysis/duplicates sub-module

data/
├── ielisa/                           # iELISA Excel files
└── ielisa_cache/                     # Cached parsed data
```

---

## Troubleshooting

### No data displayed
- Verify Excel files are in `data/ielisa/` directory
- Check files have correct sheet names ("450-600 nm" and "ECHANTILLONS")
- Click "Refresh Data" button to reload

### QC failures
- Review control OD values in the Plate Summary Table
- Check control CVs are below 20%
- Verify positive control inhibition is 50-80%

### Duplicate conflicts
- Check the "Data Quality Conflicts" section in the Analysis tab
- Review samples with barcode or LabID conflicts
- Verify sample metadata is correct in source files

---

## Key Insights

The iELISA module automatically:
- ✅ Parses 12 plates with 528 samples
- ✅ Validates plate-level controls
- ✅ Calculates inhibition using two formulas
- ✅ Detects duplicate testing across plates
- ✅ Identifies data quality conflicts
- ✅ Provides comprehensive export options

**Positivity Threshold**: 30% inhibition (Formula 2) for both LiTat 1.3 and LiTat 1.5 antigens.
