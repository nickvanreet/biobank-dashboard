# Biobank-Extraction Data Linking Feature

## Overview

This feature links extraction quality data with the biobank registry to provide comprehensive tracking of health structure performance, sample matching, and DRS collection volume monitoring over time.

## Key Features

### 1. Automated Data Linking
- **Barcode Matching**: Automatically links extraction records to biobank samples using normalized KPS barcodes
- **Smart Normalization**: Handles various barcode formats (KPS-12345, kps0012345, 12345, etc.)
- **Health Structure Validation**: Compares health structure names between datasets to identify mismatches

### 2. New KPIs

The extraction module now displays additional KPIs:

- **Matched to Biobank**: Number of extraction records successfully linked to biobank samples
- **Unmatched Samples**: Extraction records without corresponding biobank entries
- **Structure Matches**: Records where health structure names match between datasets
- **Structure Mismatches**: Records where health structures don't match (potential data quality issues)
- **Linkage Rate**: Percentage of extraction records matched to biobank (%)

### 3. Volume Monitoring Over Time

**Health Structure DRS Volume Collection Over Time**
- Multi-line time series chart showing total DRS volume collected by each health structure per month
- Hover details include: extractions count, median volume, ready rate
- Enables tracking of collection trends and identifying declining performance

**Volume Target Achievement**
- Bar chart comparing actual vs expected monthly DRS collection volumes
- Visual indicator (red dashed line) shows 100% target threshold
- Color-coded bars: Green (meets target) vs Red (below target)
- Default target: 50 mL per health structure per month (configurable)

**Biobank Linkage Status Distribution**
- Pie chart showing distribution of linkage quality:
  - Matched - Structure OK (Green): Full match with correct health structure
  - Matched - No Structure Info (Blue): Matched but missing structure data
  - Matched - Structure Mismatch (Orange): Matched but structure names differ
  - Unmatched to Biobank (Red): No biobank record found

### 4. Data Quality Tables

**Unmatched Samples Tab**
- Lists all extraction records that couldn't be matched to biobank
- Helps identify missing biobank entries or barcode errors
- Columns: Sample ID, Barcode, Date, Health Structure, Volume, State, Quality

**Health Structure Mismatches Tab**
- Shows records where extraction and biobank health structures don't match
- Critical for data quality auditing
- Columns: Sample ID, Barcode, Date, Extraction Structure, Biobank Structure, Volume

## Technical Implementation

### New Files

#### `R/core/data_linking_utils.R`
Core utilities for linking data:

**Key Functions:**
- `normalize_barcode(barcode)`: Standardizes barcode format for matching
- `link_extraction_to_biobank(extraction_df, biobank_df)`: Main linking function
- `summarise_linkage_metrics(linked_df)`: Computes linkage KPIs
- `summarise_health_structure_volumes_over_time(linked_df)`: Time series aggregation
- `calculate_volume_targets(linked_df, expected_monthly_volume)`: Target comparison
- `get_unmatched_extractions(linked_df)`: Extract unmatched records
- `get_health_structure_mismatches(linked_df)`: Extract structure mismatches

### Modified Files

#### `R/modules/mod_04_extractions.R`
Enhanced extraction module with:
- Biobank data loading on initialization
- Automatic linking of extraction and biobank data
- 5 new KPI value boxes
- 3 new interactive visualizations
- 2 new data quality tables

**Server Changes:**
- Added `biobank_data` reactive value
- Added `linked_data` reactive (combines extraction + biobank)
- Added `linkage_metrics` reactive
- Uses `linked_data` instead of `extraction_data` in filters
- New plot outputs: `volume_evolution_plot`, `target_achievement_plot`, `linkage_status_plot`
- New table outputs: `unmatched_table`, `mismatch_table`

## Barcode Normalization Logic

The linking algorithm normalizes barcodes to handle variations:

```
Input:           Output:
"KPS-12345"   -> "12345"
"kps0012345"  -> "12345"
"  12345  "   -> "12345"
"KPS 12345"   -> "12345"
```

Steps:
1. Convert to lowercase
2. Remove "KPS" prefix
3. Remove non-alphanumeric characters (except hyphens)
4. Remove leading zeros
5. Trim whitespace

## Health Structure Matching

Health structures are compared case-insensitively after trimming:

```
Extraction: "Clinique Bonzola"  ↔  Biobank: "Clinique Bonzola"  → MATCH
Extraction: "HGR Bipemba"       ↔  Biobank: "HGR BIPEMBA"       → MATCH
Extraction: "CS Mulombodi"      ↔  Biobank: "HGR Bipemba"       → MISMATCH
```

## Volume Target Configuration

Default target: **50 mL per health structure per month**

To adjust the expected volume, modify in `mod_04_extractions.R` line 766:

```r
targets <- calculate_volume_targets(df, expected_monthly_volume = 50)
```

Change `50` to your desired target volume.

## Data Flow

```
┌─────────────────┐
│ Extraction Data │
│  (31 files)     │
└────────┬────────┘
         │
         │ load_extraction_dataset()
         │
         ▼
┌─────────────────┐       ┌─────────────────┐
│ Extraction DF   │       │  Biobank Data   │
│ - sample_id     │       │  - barcode      │
│ - health_struct │       │  - health_fac   │
│ - volume        │       │  - study        │
└────────┬────────┘       └────────┬────────┘
         │                         │
         │                         │ analyze_data_quality()
         │                         │
         │                         ▼
         │                ┌─────────────────┐
         │                │ Clean Biobank   │
         │                │    DataFrame    │
         │                └────────┬────────┘
         │                         │
         └─────────┬───────────────┘
                   │
                   │ link_extraction_to_biobank()
                   │
                   ▼
         ┌─────────────────────┐
         │   Linked Dataset    │
         │ - All extraction    │
         │ - biobank_matched   │
         │ - structure_match   │
         │ - biobank_*         │
         └─────────┬───────────┘
                   │
                   ├─→ summarise_linkage_metrics()      → KPIs
                   ├─→ summarise_..._over_time()         → Time series
                   ├─→ calculate_volume_targets()        → Target analysis
                   ├─→ get_unmatched_extractions()       → Quality tables
                   └─→ get_health_structure_mismatches() → Quality tables
```

## Usage

The linking happens automatically when the Extraction Quality module loads:

1. **On Module Load**:
   - Extraction data is loaded from `data/extractions/`
   - Biobank data is loaded from `data/biobank/` (most recent file)
   - Data is automatically linked via barcodes

2. **Filtering**:
   - All existing filters work on the linked dataset
   - New columns available: `biobank_matched`, `health_structure_match`

3. **Monitoring**:
   - Check KPIs at the top to see linkage quality
   - Use the "Volume Collection Over Time" chart to monitor trends
   - Review "Target Achievement" to see which structures are meeting goals
   - Check the "Unmatched Samples" tab to identify data quality issues

## Benefits

1. **Quality Assurance**: Identifies samples that exist in extraction but not in biobank registry
2. **Structure Validation**: Flags mismatches in health structure names for data cleaning
3. **Performance Tracking**: Monitors DRS collection volume trends by health structure
4. **Target Monitoring**: Ensures health structures are collecting expected volumes
5. **Data Completeness**: Provides clear metrics on linkage success rate

## Troubleshooting

### Low Linkage Rate
- Check barcode format consistency
- Verify extraction sample_id column contains KPS barcodes
- Ensure biobank file has code_barres_kps column

### Many Structure Mismatches
- Review health structure naming conventions
- Standardize structure names in source data
- Check for typos or abbreviation differences

### No Biobank Data
- Ensure biobank Excel file exists in `data/biobank/`
- Check file format (should be .xlsx or .xls)
- Verify biobank file has required columns

## Future Enhancements

Potential additions:
- Configurable target volumes per health structure
- Alerts for declining collection trends
- Automated barcode correction suggestions
- Export unmatched records for data cleaning
- Historical linkage rate trends
- Multi-file biobank support with version tracking
