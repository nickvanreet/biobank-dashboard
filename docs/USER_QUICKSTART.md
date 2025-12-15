# Biobank Dashboard Quick-Start Guide

Welcome to the Biobank Dashboard! This guide will help you get started with navigating and using the dashboard effectively.

---

## Getting Started

### Accessing the Dashboard

1. Open your web browser (Chrome, Firefox, or Edge recommended)
2. Navigate to the dashboard URL provided by your administrator
3. The dashboard will load with the Data Manager sidebar on the left

### First-Time Setup

When you first access the dashboard:

1. **Wait for data to load** - The sidebar will show loading progress
2. **Check the data summary** - Verify sample counts match expectations
3. **Set your filters** - Use the sidebar to filter by date range or location

---

## Dashboard Layout

```
+------------------+----------------------------------------+
|                  |                                        |
|   SIDEBAR        |           MAIN CONTENT AREA            |
|                  |                                        |
|  - Data Loading  |   Navigation tabs at top:              |
|  - Filters       |   [Data Quality] [Overview] [MIC]...   |
|  - Status        |                                        |
|                  |   Selected module content below        |
+------------------+----------------------------------------+
```

### Sidebar (Data Manager)

The sidebar controls what data is displayed:

- **Data Status**: Shows loaded biobank samples and test results
- **Date Range**: Filter by sample collection date
- **Location Filters**: Filter by Province and Health Zone
- **Study Filter**: Filter by study code

All filters apply globally to every module in the dashboard.

---

## Module Guide

### 1. Data Quality

**Purpose**: Assess data completeness and identify issues

**Key features**:
- Missing data summary
- Duplicate detection
- Data validation errors

**When to use**: Start here to verify data quality before analysis

### 2. Overview & Demographics

**Purpose**: Population-level summary statistics

**Key features**:
- Age distribution (histogram)
- Sex ratio (pie chart)
- Pregnancy status tracking
- Screening type breakdown (active vs passive)

**When to use**: For general population characterization

### 3. Geographic

**Purpose**: Map-based visualization of sample locations

**Key features**:
- Interactive Leaflet map
- Color-coded markers by test result
- Health zone clustering
- Filter by test type (MIC, ELISA, iELISA)

**When to use**: To identify geographic patterns in test results

### 4. Transport

**Purpose**: Analyze sample transport times and conditions

**Key features**:
- Collection-to-arrival time analysis
- Transport delay distribution
- Impact on sample quality

**When to use**: To monitor pre-analytical factors

### 5. Extractions

**Purpose**: Track sample extraction status

**Key features**:
- Extraction success rates
- Volume analysis
- Linkage to biobank records

**When to use**: To monitor laboratory processing

### 6. MIC qPCR

**Purpose**: Molecular detection results (DNA/RNA)

**Key features**:
- Run-level overview with QC status
- Sample-level results table
- Positivity rates by target (177T, 18S2)
- RNAseP quality assessment
- Export functionality

**Tabs**:
- **Overview**: Summary statistics and KPIs
- **Samples**: Detailed sample results table
- **QC Metrics**: Quality control analysis
- **Analysis**: Distribution plots and trends

### 7. ELISA-PE

**Purpose**: Serological detection (PE antigen)

**Key features**:
- Run validation status
- PP% and DOD calculations
- Positive/Negative/Borderline classification
- Control validation

### 8. ELISA-VSG

**Purpose**: Serological detection (VSG antigen)

**Key features**:
- 4-plate format support
- Per-plate QC validation
- Sample classification

### 9. iELISA

**Purpose**: Inhibition ELISA results (LiTat 1.3 & 1.5)

**Key features**:
- Dual antigen analysis
- % Inhibition calculation
- Positive/Negative classification

### 10. ELISA Concordance

**Purpose**: Compare PE vs VSG results

**Key features**:
- Agreement statistics
- Discordant sample identification
- Kappa coefficient

### 11. Sample Journey

**Purpose**: Track individual sample through all tests

**Key features**:
- Search by barcode or lab ID
- Timeline visualization
- PDF export for individual samples

**How to use**:
1. Enter barcode in search box
2. View sample timeline
3. Click "Export PDF" for printable report

### 12. Sample Processing

**Purpose**: Overall processing pipeline status

**Key features**:
- Samples at each processing stage
- Funnel visualization
- Bottleneck identification

### 13. Predictive Analytics

**Purpose**: Risk prediction and trend analysis

**Key features**:
- Machine learning predictions
- Temporal trend analysis
- Risk factor identification

---

## Common Tasks

### Finding a Specific Sample

1. Go to **Sample Journey** module
2. Enter the barcode (e.g., `KPS-12345`) or lab ID
3. View all test results for that sample

### Exporting Data

Most modules support data export:

1. Look for the **Export** button or tab
2. Choose format: CSV, Excel, or PDF
3. Click to download

### Filtering by Date

1. In the sidebar, find **Date Range**
2. Select start and end dates
3. All modules update automatically

### Checking Test Positivity

1. Go to **Overview** module
2. View the "Test Overlap Analysis" section
3. See positivity rates for each test type

---

## Interpreting Results

### Status Colors

| Color | Meaning |
|-------|---------|
| Green | Positive / Detected |
| Yellow/Orange | Borderline / Review needed |
| Gray | Negative / Not detected |
| Red | Invalid / QC failed |

### QC Indicators

- **Valid Run**: All controls passed QC thresholds
- **Invalid Run**: One or more controls failed
- **Sample Valid**: Individual sample passed QC
- **Sample Invalid**: Sample excluded from analysis

### Classification Thresholds

| Test | Positive | Borderline |
|------|----------|------------|
| ELISA PP% | >= 20% | 15-20% |
| ELISA DOD | >= 0.3 | 0.2-0.3 |
| iELISA Inhibition | >= 30% | 25-30% |
| MIC qPCR | Detected | Indeterminate |

---

## Tips & Best Practices

### Performance

- Large date ranges may slow loading; narrow filters when possible
- Use the cache reload button sparingly
- Close unused browser tabs running the dashboard

### Data Interpretation

- Always check **Data Quality** before analysis
- Note the **n=** sample counts shown in each module
- Invalid runs are excluded from aggregate statistics

### Getting Help

- Hover over charts for detailed tooltips
- Check the **?** help icons where available
- Contact your administrator for data questions

---

## Troubleshooting

### Dashboard won't load

1. Check your internet connection
2. Try refreshing the page (Ctrl+F5)
3. Clear browser cache
4. Contact administrator

### Data looks outdated

1. Check the "Last updated" timestamp in sidebar
2. Click the refresh button in Data Manager
3. Wait for reload to complete

### Export fails

1. Ensure pop-ups are not blocked
2. For PDF export, wait for generation to complete
3. Try CSV export as alternative

### Charts don't display

1. Check if filters are too restrictive
2. Verify data exists for selected date range
3. Try resetting filters to defaults

---

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl + F` | Browser find (search in tables) |
| `Ctrl + P` | Print current view |
| `Esc` | Close modal dialogs |

---

## Glossary

| Term | Definition |
|------|------------|
| **Biobank** | Central repository of biological samples |
| **Barcode** | Unique sample identifier (KPS-XXXXX format) |
| **PP%** | Percent Positivity (ELISA metric) |
| **DOD** | Density Over Dilution (ELISA metric) |
| **Cq** | Cycle quantification (qPCR metric) |
| **QC** | Quality Control |
| **RNAseP** | Human housekeeping gene (sample quality marker) |

---

*For technical support, contact your system administrator.*

*Last updated: December 2024*
