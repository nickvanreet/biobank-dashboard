# Overview Module Improvements

## Summary

The Overview module has been redesigned to better measure concordance between molecular (MIC qPCR) and serological tests (ELISA PE/VSG, iELISA). This "Claude-style" redesign focuses on clinical interpretation and provides clearer insights into test agreement patterns.

## What Changed

### 1. Enhanced Concordance Calculations (`R/data/dashboard_data_utils.R`)

Added three new analytical components to `prepare_assay_dashboard_data()`:

#### a) **Molecular-Serology Concordance Analysis**
- Groups assays into "Molecular" (MIC qPCR) vs "Serology" (ELISA PE/VSG, iELISA)
- Calculates per-sample concordance categories:
  - **Both Positive**: MIC+ AND any serology+
  - **Both Negative**: MIC- AND all serology-
  - **MIC+ / Serology-**: Discordant (MIC positive, all serology negative)
  - **MIC- / Serology+**: Discordant (MIC negative, any serology positive)
- Tracks samples tested with only one category

#### b) **Summary Statistics** (`mic_serology_summary`)
Provides aggregate metrics:
- Sample counts per concordance category
- Concordance and discordance percentages
- Positivity rates for molecular vs serology

#### c) **Single-Test Analysis** (`single_test_summary`)
Tracks patterns across all 5 tests:
- Samples positive on exactly 1 test
- Samples positive on multiple tests
- Samples positive on all tests performed

### 2. Redesigned User Interface (`R/modules/mod_overview_assays.R`)

#### New KPI Layout
Reorganized into two focused sections:

**Section 1: Test Results by Assay** (4 KPIs)
- MIC qPCR positives
- ELISA PE positives
- ELISA VSG positives
- iELISA positives (combined L13 + L15)

**Section 2: Molecular vs Serology Concordance** (8 KPIs)
- Both Positive (concordant)
- MIC+ / Serology- (discordant)
- MIC- / Serology+ (discordant)
- Overall concordance %
- Single-test positives
- All-tests positives
- Samples tested
- Total tests completed

#### New Visualizations

**1. Molecular vs Serology Sankey Diagram**
- Flow visualization showing:
  - MIC Positive → Serology Positive/Negative
  - MIC Negative → Serology Positive/Negative
- Color-coded by concordance type:
  - Green: Both positive (concordant)
  - Orange: Discordant
  - Gray: Both negative (concordant)

**2. Concordance Categories Bar Chart**
- Stacked bars showing distribution of:
  - Both Positive
  - Both Negative
  - MIC+ / Serology-
  - MIC- / Serology+
- Includes counts and percentages

**3. Enhanced Upset Plot**
- Color-coded by number of tests:
  - **Red bars**: Single-test positives
  - **Blue bars**: Multiple-test positives
- Emphasizes clinical significance of isolated positives

### 3. What Defines "Positive"

The module uses consistent positivity definitions across all assays:

| Assay | Positive Criteria | Logic |
|-------|------------------|-------|
| **ELISA PE** | PP% ≥ 20 OR DOD ≥ 0.3 | OR logic (either metric) |
| **ELISA VSG** | PP% ≥ 20 OR DOD ≥ 0.3 | OR logic (either metric) |
| **iELISA L13** | % Inhibition ≥ 30 | Formula 2 (linear interpolation) |
| **iELISA L15** | % Inhibition ≥ 30 | Formula 2 (linear interpolation) |
| **MIC qPCR** | FinalCall = "Positive" or "Detected" | String match |

**Serology = ANY of:** ELISA PE, ELISA VSG, iELISA L13, or iELISA L15
**Molecular = MIC qPCR**

## Clinical Interpretation

### Understanding Concordance

**High Concordance (Both Positive or Both Negative)**
- Expected for validated diagnostic tests
- Confirms consistent detection across methods

**MIC+ / Serology- (Molecular only)**
- Potential early infection (before antibody response)
- Low parasitemia (below serological detection)
- Recent treatment (parasites cleared, antibodies waning)

**MIC- / Serology+ (Serology only)**
- Past infection with cleared parasitemia
- Persistent antibodies post-treatment
- False positive serology (cross-reactivity)

**Single-Test Positives**
- Clinical concern: May indicate:
  - Test-specific artifacts
  - Edge cases near detection limits
  - Need for repeat testing or confirmatory assays

## Usage

### Filters
- **Assay date**: Restrict analysis to specific time periods
- **Assays**: Select which tests to include in concordance
- **Statuses**: Include/exclude Positive, Borderline, Negative, Invalid
- **Samples with any assay**: Toggle to show only samples with at least one result

### Interactive Features
- **Click KPIs**: Drill down into specific categories in the sample table
- **Click charts**: Filter table by clicking bars or heatmap cells
- **Export**: Download filtered sample data as CSV

## Technical Notes

### Data Flow
1. Raw assay data → `prepare_assay_dashboard_data()`
2. Classification into Positive/Borderline/Negative/Invalid
3. Sample deduplication (best status per sample-assay)
4. Concordance calculation (molecular vs serology)
5. Visualization preparation

### Performance
- Calculations are reactive and update automatically with filter changes
- Data is cached at the prepared() level to avoid redundant processing
- Large datasets (>10,000 samples) may take 2-3 seconds to render

### Dependencies
- `dplyr`, `tidyr`: Data manipulation
- `plotly`: Interactive visualizations (Sankey, bars, heatmaps)
- `ggplot2`: Static plots converted to plotly
- `stringr`: Text pattern matching

## Future Enhancements

Potential additions based on user feedback:
- [ ] Export concordance summary statistics table
- [ ] Add filter for "Molecular only", "Serology only", "Both"
- [ ] Option to treat iELISA L13/L15 separately or combined
- [ ] Add Cohen's Kappa calculation for concordance
- [ ] Time-based concordance trends
- [ ] Geographic stratification of concordance

## Version History

**v3.2.0** (2025-12-01)
- Initial redesign with molecular-serology concordance focus
- New KPI organization and visualizations
- Enhanced single-test positive tracking

---

**Questions or Issues?**
Contact the development team or open an issue on the project repository.
