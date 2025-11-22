# Sample Journey Module - Documentation

## Overview

The Sample Journey Module (Module 10) provides comprehensive tracking of individual samples across all testing platforms in the biobank dashboard. It allows users to search for a specific sample ID and view its complete testing history, results, and quality alerts.

## Files Created

### 1. `/home/user/biobank-dashboard/R/sampleJourneyHelpers.R` (481 lines)

Core helper functions for data gathering and processing:

#### Functions:
- **`gather_sample_journey(sample_id, ...)`** - Main function that searches all data sources for a sample ID
  - Searches: biobank, extractions, MIC, ELISA PE, ELISA VSG, iELISA
  - Returns list with all found data and timeline
  - Uses `normalize_barcode()` for matching

- **`create_sample_timeline(journey_data)`** - Creates chronological timeline of all test events
  - Combines dates from all test types
  - Calculates day numbers relative to first event
  - Returns tibble with event, date, category, details

- **`generate_sample_alerts(journey_data)`** - Generates QC alerts based on test results
  - Low DRS volume (<30µL) - warning
  - High RNAseP Ct (>35) - warning
  - Discordant MIC results - danger
  - Borderline ELISA (PP 40-60%) - info

- **`get_sample_autocomplete()`** - Returns list of all sample IDs for autocomplete
  - Aggregates IDs from all data sources
  - Deduplicates and sorts

### 2. `/home/user/biobank-dashboard/R/sampleJourneyVisualizations.R` (446 lines)

Visualization functions using Plotly:

#### Functions:
- **`plot_sample_timeline(timeline_data)`** - Horizontal Gantt-style timeline chart
  - Shows all events chronologically
  - Color-coded by category (Biobank, Extraction, MIC, ELISA, iELISA)
  - Interactive hover tooltips

- **`plot_drs_gauge(volume_ul)`** - Radial gauge for DRS volume (0-200µL scale)
  - Green: ≥30µL (good)
  - Orange: 20-30µL (warning)
  - Red: <20µL (critical)
  - Shows delta from 30µL threshold

- **`plot_mic_heatmap(mic_data)`** - Heatmap for MIC target detection
  - Targets: TNA, gTNA, TBG, gTBG, TCO, TEV, RNAseP
  - Color intensity based on Cq values
  - Red for strong detection, gray for not detected

- **`plot_elisa_cards(elisa_data, elisa_type)`** - Bootstrap cards for ELISA results
  - Shows DOD and PP% for each test
  - Color-coded borders (red=positive, orange=borderline, green=negative)
  - Supports both PE and VSG

- **`plot_ielisa_results(ielisa_data)`** - Table display for iELISA iterations
  - Shows LiTat 1.3 and 1.5 results
  - Displays PP% for each antigen

#### Color Scheme:
```r
COLORS <- list(
  positive = "#e41a1c",      # Red - POS/detected
  negative = "#4daf4a",      # Green - NEG
  borderline = "#ff7f00",    # Orange - borderline
  invalid = "#984ea3",       # Purple - invalid
  biobank = "#4F46E5",       # Blue - biobank events
  extraction = "#10B981",    # Green - extraction events
  mic = "#F59E0B",           # Amber - MIC events
  elisa = "#06B6D4",         # Cyan - ELISA events
  ielisa = "#8B5CF6"         # Violet - iELISA events
)
```

### 3. `/home/user/biobank-dashboard/R/modules/mod_10_sample_journey.R` (520 lines)

Main Shiny module with UI and server logic:

#### UI Components:
- **Search Box** - Text input with search button
- **Search Status** - Success/warning alerts based on search results
- **Timeline Card** - Interactive Plotly timeline visualization
- **4-Column Results Layout**:
  1. **Biobank & Extraction/QC** - Sample info + DRS gauge
  2. **MIC qPCR** - Summary stats + target heatmap
  3. **ELISA (PE/VSG)** - Result cards for both test types
  4. **iELISA** - Iteration results table
- **Alerts Section** - Quality control alerts with color coding

#### Server Logic:
- **Search Handler** - Gathers data from all reactive data sources
- **Progress Indicators** - Shows loading status during search
- **Dynamic Rendering** - Only shows sections with data
- **Reactive Updates** - Updates all visualizations when new sample selected

## Usage in App

### Integration with app.R

The module has been added to `global.R` and is ready to use. To add it to your app navigation:

```r
# In app.R or server.R
nav_panel(
  "Sample Journey",
  icon = icon("route"),
  mod_sample_journey_ui("sample_journey")
)

# In server
mod_sample_journey_server(
  "sample_journey",
  biobank_data = reactive(biobank_clean),
  extraction_data = reactive(extraction_linked),
  mic_data = reactive(mic_samples),
  elisa_pe_data = reactive(elisa_pe_combined),
  elisa_vsg_data = reactive(elisa_vsg_combined),
  ielisa_data = reactive(ielisa_combined)
)
```

### Data Requirements

The module expects the following reactive data sources:

1. **biobank_data** - Must have columns:
   - `code_barres_kps` or `barcode`
   - `numero_labo`, `numero`, or `lab_id`
   - `province`, `health_zone`, `health_structure` (optional)
   - `date_sample` or `date_prelevement`

2. **extraction_data** - Must have columns:
   - `sample_id`
   - `extraction_date`
   - `drs_volume_ml` (for gauge)

3. **mic_data** - Must have columns:
   - `SampleID`, `SampleName`, or `Barcode`
   - `RunDate` or `RunDateTime`
   - `FinalCall`
   - `Cq_median_*` columns for targets
   - `Cq_median_RNAseP_DNA`, `Cq_median_RNAseP_RNA` (for QC alerts)

4. **elisa_pe_data** & **elisa_vsg_data** - Must have columns:
   - `code_barres_kps` or `numero_labo`
   - `plate_date`
   - `sample_positive`, `DOD`, `PP_percent`

5. **ielisa_data** - Must have columns:
   - `code_barres_kps` or `numero_labo`
   - `plate_date`
   - Result columns (implementation-specific)

## Features

### Search Functionality
- Searches across all 6 data sources simultaneously
- Uses `normalize_barcode()` for flexible matching
- Handles both barcode and numero/lab ID formats
- Shows count of sources with data

### Timeline Visualization
- Chronological view of all test events
- Color-coded by test category
- Interactive hover with details
- Shows day numbers from first event

### Test Results Display
- **Extraction QC**: DRS volume gauge with threshold indicators
- **MIC Results**: Target detection heatmap showing Cq values
- **ELISA Results**: Color-coded cards with DOD and PP%
- **iELISA Results**: Structured table with LiTat results

### Quality Alerts
- Automatically generated based on configurable thresholds
- Color-coded severity (danger/warning/info)
- Categories: Extraction QC, MIC QC, MIC Results, ELISA PE/VSG
- Shows specific values that triggered alerts

## Alert Thresholds

| Alert Type | Threshold | Severity |
|------------|-----------|----------|
| Low DRS Volume | <30µL | Warning |
| High RNAseP DNA Ct | >35 | Warning |
| High RNAseP RNA Ct | >35 | Warning |
| Discordant MIC Results | Multiple different calls | Danger |
| Borderline ELISA PE | PP 40-60% | Info |
| Borderline ELISA VSG | PP 40-60% | Info |

## Testing Recommendations

1. **Test with known sample IDs** from each data source
2. **Verify timeline ordering** is correct
3. **Check alert generation** with edge cases:
   - Sample with low DRS volume
   - Sample with high RNAseP Ct
   - Sample with discordant MIC results
   - Sample with borderline ELISA
4. **Test search variations**:
   - Full barcode (e.g., KPS12345)
   - Barcode without prefix (e.g., 12345)
   - Numero/lab ID
5. **Verify UI responsiveness** with different data combinations

## Customization Options

### Modify Alert Thresholds
Edit thresholds in `generate_sample_alerts()` function in `sampleJourneyHelpers.R`

### Add New Test Types
1. Add data source parameter to `gather_sample_journey()`
2. Add search logic in the function
3. Add timeline event creation in `create_sample_timeline()`
4. Add visualization in `sampleJourneyVisualizations.R`
5. Add UI section in `mod_10_sample_journey.R`

### Customize Colors
Edit `COLORS` list in `sampleJourneyVisualizations.R`

### Modify Gauge Ranges
Edit `plot_drs_gauge()` function to change scale or thresholds

## Troubleshooting

### "No data found" message
- Verify sample ID format matches data
- Check that `normalize_barcode()` is working correctly
- Ensure reactive data sources are providing data

### Timeline not showing
- Check that date columns exist and are properly formatted
- Verify `create_sample_timeline()` is finding date columns

### Visualizations not rendering
- Check browser console for JavaScript errors
- Verify Plotly is loaded (`library(plotly)`)
- Ensure data structures match expected format

## Dependencies

- **R Packages**: tidyverse, plotly, shiny, bslib, lubridate, scales, glue
- **Internal Functions**: `normalize_barcode()` from `data_linking_utils.R`
- **Data Sources**: All 6 data streams must be available as reactives

## Performance Notes

- Search is performed synchronously with progress indicators
- Large datasets may take a few seconds to search
- Consider adding debouncing to search input if needed
- Visualizations are rendered on-demand only when data exists

## Future Enhancements

Potential improvements:
1. Add autocomplete suggestions to search box
2. Export sample journey as PDF report
3. Add comparison view for multiple samples
4. Include specimen tracking information
5. Add custom date range filtering
6. Include batch/run QC metrics
7. Add notes/comments functionality
