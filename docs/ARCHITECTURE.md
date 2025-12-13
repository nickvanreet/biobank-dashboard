# Biobank Dashboard Architecture

## Overview

The Mbuji-Mayi Biobank Dashboard is a Shiny (R) application designed to manage, analyze, and visualize biobank samples and their associated diagnostic test results. This document explains how data flows through the system, how modules interact, and how quality control mechanisms ensure data integrity.

---

## Table of Contents

1. [Core Architecture](#1-core-architecture)
2. [Biobank Module (Data Source)](#2-biobank-module-data-source)
3. [Demographics & Geographic Data](#3-demographics--geographic-data)
4. [Test Modules](#4-test-modules)
   - [MIC qPCR](#41-mic-qpcr-molecular-detection)
   - [ELISA-PE](#42-elisa-pe-serological-detection)
   - [ELISA-VSG](#43-elisa-vsg-serological-detection)
   - [iELISA](#44-ielisa-inhibition-assay)
5. [QC Impact on Results](#5-qc-impact-on-results)
6. [Supporting Modules](#6-supporting-modules)
   - [Extraction & Preanalytics](#61-extraction--preanalytics)
   - [Transport](#62-transport)
   - [Sample Journey](#63-sample-journey)
   - [Sample Processing](#64-sample-processing)
   - [Prediction Module](#65-prediction-module)
7. [Overview Module](#7-overview-module)
8. [Data Flow Diagram](#8-data-flow-diagram)
9. [Filter Propagation](#9-filter-propagation)

---

## 1. Core Architecture

### Hub-and-Spoke Data Model

The application uses a **centralized Data Manager** (`mod_data_manager.R`) as the single source of truth:

```
                        ┌─────────────────────┐
                        │   DATA MANAGER      │
                        │  (Central Hub)      │
                        └─────────┬───────────┘
                                  │
        ┌─────────────────────────┼─────────────────────────┐
        │                         │                         │
        ▼                         ▼                         ▼
┌───────────────┐       ┌─────────────────┐       ┌─────────────────┐
│  Test Modules │       │ Analysis Modules│       │ Tracking Modules│
│ MIC, ELISA,   │       │ Concordance,    │       │ Sample Journey, │
│ iELISA        │       │ Predictive      │       │ Transport       │
└───────────────┘       └─────────────────┘       └─────────────────┘
```

### What the Data Manager Provides

All modules receive data from the Data Manager via reactive objects:

| Output | Description |
|--------|-------------|
| `raw_data` | Raw biobank before cleaning |
| `clean_data` | Biobank after cleaning and validation |
| `filtered_data` | Biobank filtered by global settings (date, location, study) |
| `filtered_extractions` | Extraction data filtered by global settings |
| `elisa_data` | Combined ELISA-PE + ELISA-VSG results |
| `ielisa_data` | iELISA results |
| `mic_data` | MIC qPCR results |
| `quality_report` | Data quality assessment |
| `filters` | Currently active filter settings |

---

## 2. Biobank Module (Data Source)

### What It Reads

The Biobank module reads Excel files from `data/lsd/biobank/` containing:

| Field | Description |
|-------|-------------|
| `barcode` | KPS-format sample identifier |
| `numero` | Lab ID number |
| `date_prelevement` | Sample collection date |
| `heure_prelevement` | Collection time |
| `age`, `sexe` | Demographics |
| `grossesse` | Pregnancy status |
| `province`, `zone_sante` | Geographic location |
| `aire_sante`, `structure_sanitaire` | Health facility |
| `etude` | Study code |
| `type_depistage` | DA (active) or DP (passive) screening |

### Loading Pipeline

```
Excel File → load_biobank_file_cached()
                    │
                    ▼
          analyze_data_quality()
          (identify conflicts, missing data)
                    │
                    ▼
          clean_biobank_data_improved()
          (standardize, correct known issues)
                    │
                    ▼
          Cleaned Biobank Data
```

### Barcode Normalization

All sample IDs are normalized for consistent linking:
- Original: `KPS-12345-A`
- Normalized: `12345a`

This ensures matching works regardless of formatting differences.

### What It Filters for Other Modules

The Biobank provides **demographic enrichment** to all test modules:

1. **Test modules** (MIC, ELISA, iELISA) receive biobank demographics attached to each result
2. **Geographic module** uses province/zone from biobank
3. **Transport module** uses collection date/time + arrival date
4. **Sample Journey** uses biobank as the base for individual sample tracking

---

## 3. Demographics & Geographic Data

### Demographics Flow

```
Biobank Excel
    │
    ├── age, sexe, grossesse
    │       │
    │       └──► Overview Demographics Module
    │                 - Age distribution histograms
    │                 - Sex ratio pie charts
    │                 - Pregnancy status tracking
    │
    ├── province, zone_sante, aire_sante
    │       │
    │       └──► Geographic Module (mod_12_geographic.R)
    │                 - Leaflet interactive map
    │                 - Heat maps by test positivity
    │                 - Health zone markers
    │
    └── etude, type_depistage
            │
            └──► All Modules via filters
                      - Active vs passive screening
                      - Study-specific analysis
```

### Geographic Module Details

**Input:**
- `filtered_data` with lat/long coordinates
- Test results (MIC, ELISA, iELISA)

**Output:**
- Interactive Leaflet map
- Color-coded markers by test result (positive/negative/borderline)
- Health zone clusters with sample counts
- Filterable by metric (MIC DNA+, MIC RNA+, Serology+)

---

## 4. Test Modules

All test modules follow a **unified 4-step processing pipeline**:

```
┌─────────────────────────────────────────────────────────────────┐
│  STEP 1: INGESTION    → Read Excel files, extract raw values   │
│                          ↓                                      │
│  STEP 2: QC           → Validate controls, flag invalid runs   │
│                          ↓                                      │
│  STEP 3: INTERPRET    → Apply decision logic, classify samples │
│                          ↓                                      │
│  STEP 4: OUTPUT       → Standardized tidy format for dashboard │
└─────────────────────────────────────────────────────────────────┘
```

---

### 4.1 MIC qPCR (Molecular Detection)

**Location:** `R/modules/mic/` and `R/modules/mod_05a_mic_coordinator.R`

#### What Enters

BioMérieux Excel files containing:
- **Samples sheet:** Well-to-sample mapping, control types
- **Cq sheets:** Cycling data for each target
  - `177T` (DNA target)
  - `18S2` (RNA target)
  - `RNAseP-DNA` (DNA quality control)
  - `RNAseP-RNA` (RNA preservation indicator)

#### Processing Pipeline

**Step 1: Ingestion** (`ingest_mic.R`)
```
Read all Cq values for 4 targets
Map wells → sample IDs
Extract control metadata
```

**Step 2: QC** (`qc_mic.R`)
```
Apply Cq cutoffs:
  - 177T: ≤35 (positive), >40 (negative)
  - 18S2: ≤35 (positive), >40 (negative)
Validate positive/negative controls
Flag invalid runs
```

**Step 3: Interpretation** (`interpret_mic.R`)
```
Calculate ΔCq = RNAseP_RNA_Cq - RNAseP_DNA_Cq
  (measures RNA preservation)

Decision tree:
  ├── Both 177T & 18S2 positive → TNA POSITIVE
  ├── 177T positive only → DNA-ONLY POSITIVE
  ├── 18S2 positive only → RNA-ONLY POSITIVE
  ├── Cq 38-40 → LATE POSITIVE (needs retest)
  └── Neither positive → NEGATIVE

Aggregate by sample (requires 2+ positive replicates)
```

**Step 4: Output** (`output_mic.R`)
```
Standardized format:
  sample_id | test_type | status_final | Cq_177T | Cq_18S2 | delta_rp
```

#### How MIC Impacts Results (QC)

| QC Check | Impact |
|----------|--------|
| Invalid controls | Entire run excluded |
| High ΔCq (>5) | RNA degraded → RNA results unreliable |
| Discordant replicates | Sample flagged for retest |
| Late positives (Cq 38-40) | Marked indeterminate |

#### What Filters MIC Data

- **Data Manager:** Date range, province, study filters
- **MIC Coordinator UI:**
  - Exclude invalid runs
  - Show only retests/discordant
  - QC threshold adjustments

---

### 4.2 ELISA-PE (Serological Detection)

**Location:** `R/modules/elisa_pe/` and `R/modules/mod_06_elisa_pe.R`

#### What Enters

Excel files with 8×12 plate layout:
- **Columns 1-6:** Antigen-positive (Ag+) wells
- **Columns 7-12:** Antigen-negative (Ag0) wells
- **Fixed positions:** Positive/negative controls

#### Processing Pipeline

**Step 1: Ingestion** (`ingest_pe.R`)
```
Read 8×12 plate OD values
Map deepwell columns → sample positions
Extract controls from fixed positions
Output: Long format with Ag+/Ag0 pairs
```

**Step 2: QC** (`qc_pe.R`)
```
Validate controls:
  - Positive control OD: 0.5-1.5
  - Negative control OD: <0.5
Calculate CV (coefficient of variation)
Flag invalid plates (CV >20%)
```

**Step 3: Interpretation** (`interpret_pe.R`)
```
Calculate metrics:
  PP% = (mean_Ag+ - mean_Ag0) / mean_Ag0 × 100
  DOD = mean_Ag+ - mean_Ag0

Classification:
  ├── PP% ≥20 OR DOD ≥0.3 → POSITIVE
  ├── PP% 15-20 OR DOD 0.2-0.3 → BORDERLINE
  └── Below thresholds → NEGATIVE
```

**Step 4: Output** (`output_pe.R`)
```
sample_id | PP% | DOD | classification | plate_valid
```

#### How ELISA-PE Impacts Results (QC)

| QC Check | Impact |
|----------|--------|
| Control OD out of range | Plate excluded |
| CV >20% | Results flagged unreliable |
| Borderline results | Flagged for retest |

---

### 4.3 ELISA-VSG (Serological Detection)

**Location:** `R/modules/elisa_vsg/` and `R/modules/mod_07_elisa_vsg.R`

#### Key Difference from ELISA-PE

ELISA-VSG processes **4 plates** instead of 1:

```
Deepwell columns → Plates:
  Columns 1-3   → Plate 1
  Columns 4-6   → Plate 2
  Columns 7-9   → Plate 3
  Columns 10-12 → Plate 4
```

Same QC, interpretation, and output logic as ELISA-PE.

---

### 4.4 iELISA (Inhibition Assay)

**Location:** `R/modules/ielisa/` and `R/modules/mod_ielisa_coordinator.R`

#### What Enters

Excel files with:
- **450-600 nm sheet:** OD values (inhibition format)
- **ECHANTILLONS sheet:** Sample metadata
- **Antigens tested:** LiTat 1.3 and LiTat 1.5

#### Processing Pipeline

**Step 1: Ingestion** (`ingest_ielisa.R`)
```
Read OD values
Map samples to wells (iELISA-specific algorithm)
Extract controls from row 13 (fixed positions)
```

**Step 2: QC** (`qc_ielisa.R`)
```
Validate controls:
  - NEG control OD: 1.0 < OD < 3.1
  - POS control inhibition: ≥30%
  - CV: <20%
```

**Step 3: Interpretation** (`interpret_ielisa.R`)
```
Two formula options:

Formula 1 (OFFICIAL):
  % inhibition = 100 × (1 - OD_sample / OD_NEG)

Formula 2 (Alternative):
  Linear normalization between NEG and POS

Positivity: ≥30% inhibition on EITHER antigen (L13 or L15)
```

**Step 4: Output** (`output_ielisa.R`)
```
sample_id | inhib_L13 | inhib_L15 | classification | formula_used
```

#### How iELISA Impacts Results (QC)

| QC Check | Impact |
|----------|--------|
| NEG control out of range | Run excluded |
| POS control <30% inhibition | Run excluded |
| CV >20% | Results flagged |
| Formula selection | Can change classification |

---

## 5. QC Impact on Results

### Quality Control Flow

```
Raw Test Data
     │
     ▼
┌──────────────┐
│   QC Step    │
│  (Step 2)    │
└──────┬───────┘
       │
       ├──► Valid runs → Continue to interpretation
       │
       └──► Invalid runs → Excluded from analysis
                          (but tracked for reporting)
```

### QC Metrics Available in UI

Each test coordinator provides:

1. **Run-level validity:** Passed/failed control validation
2. **Sample-level flags:** Retests needed, borderline results
3. **QC settings panel:** Adjustable thresholds
4. **Export:** Full QC report with all flags

### How QC Filters Propagate

```
QC validation → status_qc column
                    │
                    ├──► Concordance module (excludes invalid)
                    ├──► Overview module (shows valid/invalid counts)
                    └──► Predictive module (uses only valid data)
```

---

## 6. Supporting Modules

### 6.1 Extraction & Preanalytics

**Module:** `mod_04_extractions.R` and `mod_06_drs_rnasep.R`

#### Input
- Extraction Excel files from `data/lsd/extractions/`
- Contains: barcode, volume, extraction date, DRS state, CN mentions

#### What It Provides

| Output | Used By |
|--------|---------|
| Extraction linkage rates | Data Quality module |
| Volume statistics | Preanalytics analysis |
| DRS state distribution | RNAseP correlation |
| RNAseP Cq values | RNA preservation assessment |

#### Linkage to Test Results

```
Extraction Data
     │
     └──► link_extraction_to_biobank()
               │
               └──► Enriched biobank with extraction info
                         │
                         └──► Test modules receive extraction context
                               (volume, date, transport time)
```

### 6.2 Transport

**Module:** `mod_03_transport.R`

#### Input
- `filtered_data` (collection date/time, arrival date)
- All test data (processing timestamps)

#### Output
- **Transport timeline:** Collection → Lab arrival
- **Processing time per assay:** When each test was performed
- **Temperature monitoring:** Anomaly detection
- **Sankey diagram:** Sample flow through pipeline
- **SLA compliance:** Days in transit tracking

#### Impact on Other Modules

Transport time affects:
- RNAseP Cq (RNA degradation over time)
- ELISA quality (serum stability)
- Sample Journey timeline visualization

---

### 6.3 Sample Journey

**Module:** `mod_10_sample_journey.R`

#### Input
- All data sources: biobank, extraction, MIC, ELISA-PE, ELISA-VSG, iELISA

#### Output
- **Individual sample tracking:** Search by barcode or lab ID
- **Timeline visualization:** Collection → Extraction → Each test
- **Comprehensive status:** All results in one view
- **Quality flags:** Accumulated from all modules
- **PDF export:** Downloadable sample report

#### Data Flow

```
User searches barcode "KPS-12345"
         │
         ▼
┌─────────────────────────────────────────────────────────┐
│  Sample Journey aggregates:                             │
│    - Biobank: demographics, collection info             │
│    - Extraction: volume, date, DRS state                │
│    - MIC: Cq values, classification                     │
│    - ELISA-PE: PP%, DOD, result                         │
│    - ELISA-VSG: PP%, DOD, result                        │
│    - iELISA: inhibition %, result                       │
└─────────────────────────────────────────────────────────┘
         │
         ▼
  Complete sample timeline with all test results
```

---

### 6.4 Sample Processing

**Module:** `mod_sample_processing.R`

#### Input
- All test data across the pipeline

#### Output
- **Sample-level summary:** One row per sample
- **Processing status:** Which tests completed/pending/failed
- **Quality flags:** Accumulated from all modules
- **Drilldown table:** Detailed view with export

#### Relationship to Sample Journey

| Sample Processing | Sample Journey |
|-------------------|----------------|
| Aggregate view (all samples) | Individual sample focus |
| Summary statistics | Detailed timeline |
| Batch operations | Single sample export |

---

### 6.5 Prediction Module

**Module:** `mod_11_predictive_analytics.R`

#### Input
- Biobank demographics (age, sex, location)
- All test results (for outcome labels)
- Historical data (time series)

#### Output
- **Risk prediction models:** Random forest, XGBoost
- **Demographic risk factors:** Age, sex, location impact
- **Geographic risk mapping:** Hotspot identification
- **Time series forecasting:** New case predictions
- **Scenario planning:** What-if analysis

#### Data Flow

```
Historical biobank + test data
         │
         ├──► Feature engineering (demographics, geography)
         │
         ├──► Model training (valid results only)
         │
         └──► Predictions displayed in dashboard
```

---

## 7. Overview Module

**Module:** `mod_overview_assays.R` and `mod_02_overview_demographics.R`

### What Comes IN

| Data Source | What's Used |
|-------------|-------------|
| Biobank | Total samples, demographics, screening type |
| MIC | Classification results (positive/negative) |
| ELISA-PE | Classification results |
| ELISA-VSG | Classification results |
| iELISA | Classification results |

### What Goes OUT (Displayed)

#### Test Prevalence Table
```
Test       | Positive | Negative | Borderline | Invalid | Total
-----------|----------|----------|------------|---------|------
MIC DNA    |    45    |   890    |     12     |   23    |  970
MIC RNA    |    32    |   903    |     15     |   20    |  970
ELISA-PE   |    78    |   756    |     34     |   45    |  913
ELISA-VSG  |    65    |   780    |     28     |   40    |  913
iELISA     |    89    |   701    |     41     |   52    |  883
```

#### Overlap Analysis
- **Exclusive positives:** Samples positive on ONLY one test
- **Shared positives:** Samples positive on multiple tests
- **All-test concordance:** Samples positive on ALL tests

#### Venn Diagram
Shows overlap between:
- Molecular (MIC)
- Serology PE
- Serology VSG
- iELISA

### Filtering Impact

The Overview module respects all global filters:
```
Global filters (date, province, study)
         │
         ▼
Overview shows filtered subset only
         │
         ▼
Statistics recalculate dynamically
```

---

## 8. Data Flow Diagram

### Complete System Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              DATA SOURCES                                    │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │   Biobank    │  │  Extractions │  │ Test Files   │  │   Config     │    │
│  │   (Excel)    │  │   (Excel)    │  │ MIC/ELISA/   │  │  (YAML)      │    │
│  └──────┬───────┘  └──────┬───────┘  │ iELISA       │  └──────────────┘    │
│         │                 │          └──────┬───────┘                       │
└─────────┼─────────────────┼─────────────────┼───────────────────────────────┘
          │                 │                 │
          ▼                 ▼                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DATA MANAGER                                       │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  load_biobank_file_cached() → analyze_data_quality()                │    │
│  │            ↓                                                         │    │
│  │  clean_biobank_data_improved() → filtered_data                      │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  Test Processing Pipelines (4-step each):                           │    │
│  │    MIC: ingest → qc → interpret → output                            │    │
│  │    ELISA-PE: ingest → qc → interpret → output                       │    │
│  │    ELISA-VSG: ingest → qc → interpret → output                      │    │
│  │    iELISA: ingest → qc → interpret → output                         │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
│  ┌─────────────────────────────────────────────────────────────────────┐    │
│  │  Linking Functions:                                                  │    │
│  │    link_extraction_to_biobank()                                     │    │
│  │    link_elisa_to_biobank()                                          │    │
│  │    link_mic_to_biobank()                                            │    │
│  │    link_ielisa_to_biobank()                                         │    │
│  └─────────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────────┘
          │
          │ Reactive data flows to all modules
          ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           CONSUMER MODULES                                   │
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │ Data Quality│  │  Overview   │  │  Transport  │  │ Extractions │        │
│  │ (quality    │  │ (prevalence,│  │ (timeline,  │  │ (linkage,   │        │
│  │  metrics)   │  │ demographics│  │  SLAs)      │  │  volumes)   │        │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘        │
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │ MIC qPCR    │  │  ELISA-PE   │  │  ELISA-VSG  │  │   iELISA    │        │
│  │ Coordinator │  │ Coordinator │  │ Coordinator │  │ Coordinator │        │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘        │
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐        │
│  │   Sample    │  │   Sample    │  │  Geographic │  │ Concordance │        │
│  │   Journey   │  │ Processing  │  │    (Map)    │  │  Analysis   │        │
│  └─────────────┘  └─────────────┘  └─────────────┘  └─────────────┘        │
│                                                                              │
│  ┌─────────────┐  ┌─────────────┐                                           │
│  │ Preanalytics│  │ Predictive  │                                           │
│  │  (RNAseP)   │  │  Analytics  │                                           │
│  └─────────────┘  └─────────────┘                                           │
└─────────────────────────────────────────────────────────────────────────────┘
          │
          ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           USER INTERFACE                                     │
│  Navigation sidebar → Module tabs → Interactive visualizations → Exports    │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## 9. Filter Propagation

### Global Filters (Set in Sidebar)

| Filter | Affects |
|--------|---------|
| Date range | All modules |
| Province | Geographic, demographics |
| Health zone | Geographic, demographics |
| Study | All analyses |
| Data quality threshold | Excludes problematic records |

### Filter Flow

```
User sets filters in sidebar
         │
         ▼
Data Manager applies filters to biobank
         │
         ▼
filtered_data reactive object updates
         │
         ▼
All modules automatically re-render with filtered subset
```

### Module-Specific Filters

Each module can apply additional local filters:

| Module | Local Filters |
|--------|---------------|
| MIC | Exclude invalid runs, show retests only |
| ELISA | Plate selection, borderline threshold |
| iELISA | Formula selection (F1 vs F2) |
| Transport | Transit time range, temperature anomalies |
| Sample Journey | Individual barcode search |

---

## Summary

The Biobank Dashboard implements a modular, hub-and-spoke architecture where:

1. **Data Manager** is the central hub that loads, cleans, and distributes all data
2. **Test modules** follow a standardized 4-step pipeline (ingest → QC → interpret → output)
3. **QC mechanisms** at each step ensure data quality and flag unreliable results
4. **Linking functions** connect test results back to biobank demographics
5. **Global filters** propagate through all modules for consistent analysis
6. **Supporting modules** (Transport, Extraction, Sample Journey) provide context and traceability
7. **Overview and Prediction** modules aggregate across all data sources

This architecture ensures:
- **Traceability:** Every result links back to sample metadata
- **Quality control:** Invalid data is excluded but tracked
- **Flexibility:** New test types can be added following the same pattern
- **Consistency:** Global filters apply uniformly across all analyses
