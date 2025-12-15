# Data Dictionary

This document defines all data elements used in the Biobank Dashboard, including field names, types, descriptions, and valid values.

---

## Table of Contents

1. [Biobank Data](#1-biobank-data)
2. [Extraction Data](#2-extraction-data)
3. [MIC qPCR Data](#3-mic-qpcr-data)
4. [ELISA Data (PE & VSG)](#4-elisa-data-pe--vsg)
5. [iELISA Data](#5-ielisa-data)
6. [Derived/Calculated Fields](#6-derivedcalculated-fields)
7. [QC Fields](#7-qc-fields)
8. [Standard Codes](#8-standard-codes)

---

## 1. Biobank Data

Source: `data/lsd/biobank/*.xlsx`

### Core Identifiers

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `barcode` | String | Primary sample identifier | `KPS-12345` | Yes |
| `numero` | Integer | Lab sequential number | `12345` | Yes |
| `numero_labo` | String | Laboratory ID | `LSD-2024-001` | No |

### Collection Information

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `date_prelevement` | Date | Sample collection date | `2024-03-15` | Yes |
| `heure_prelevement` | Time | Collection time | `09:30` | No |
| `date_arrivee` | Date | Arrival at laboratory | `2024-03-16` | No |
| `heure_arrivee` | Time | Arrival time | `14:00` | No |

### Demographics

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `age` | Integer | Age in years | 0-120 | Yes |
| `sexe` | String | Sex | `M`, `F`, `Unknown` | Yes |
| `grossesse` | String | Pregnancy status | `Oui`, `Non`, `NA` | Conditional |

### Geographic

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `province` | String | Province name | `Kasai Oriental` | Yes |
| `zone_sante` | String | Health zone | `Dibindi` | Yes |
| `aire_sante` | String | Health area | `Bakua Kenge` | No |
| `structure_sanitaire` | String | Health facility | `CS Bakua Kenge` | No |
| `latitude` | Numeric | GPS latitude | `-6.1234` | No |
| `longitude` | Numeric | GPS longitude | `23.5678` | No |

### Study Information

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `etude` | String | Study code | Study-specific | No |
| `type_depistage` | String | Screening type | `DA` (active), `DP` (passive) | Yes |
| `enqueteur` | String | Data collector ID | Initials or code | No |

---

## 2. Extraction Data

Source: `data/lsd/extractions/*.xlsx`

### Identifiers

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `barcode` | String | Sample barcode | `KPS-12345` | Yes |
| `extraction_id` | String | Extraction batch ID | `EXT-2024-001` | No |
| `extraction_date` | Date | Date of extraction | `2024-03-18` | Yes |

### Volume Data

| Field | Type | Description | Unit | Required |
|-------|------|-------------|------|----------|
| `volume_drs` | Numeric | DRS volume | mL | No |
| `volume_edta` | Numeric | EDTA tube volume | mL | No |
| `volume_serum` | Numeric | Serum volume | mL | No |
| `volume_extracted` | Numeric | Total extracted | uL | No |

### Quality

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `quality_drs` | String | DRS quality | `Good`, `Moderate`, `Poor` | No |
| `hemolysis` | String | Hemolysis level | `None`, `Mild`, `Moderate`, `Severe` | No |
| `lipemia` | String | Lipemia level | `None`, `Mild`, `Moderate`, `Severe` | No |

---

## 3. MIC qPCR Data

Source: `data/lsd/mic/*.xlsx`

### Run Information

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `RunID` | String | qPCR run identifier | `Run001` | Yes |
| `run_date` | Date | Date of run | `2024-03-20` | Yes |
| `instrument` | String | qPCR instrument | `CFX96` | No |
| `operator` | String | Operator initials | `JD` | No |

### Sample Identification

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `SampleID` | String | Sample identifier | `KPS-12345` | Yes |
| `SampleName` | String | Sample name in plate | `S001` | Yes |
| `Well` | String | Plate well position | `A01` | Yes |
| `ControlType` | String | Control designation | `Sample`, `PC`, `NC`, `NTC` | Yes |

### Target Results (Cq Values)

| Field | Type | Description | Range | Required |
|-------|------|-------------|-------|----------|
| `Cq_177T` | Numeric | Cq for 177T target (DNA) | 0-50 or NA | Yes |
| `Cq_18S2` | Numeric | Cq for 18S2 target (RNA) | 0-50 or NA | Yes |
| `Cq_RNAseP_DNA` | Numeric | Cq for RNAseP-DNA | 0-50 or NA | Yes |
| `Cq_RNAseP_RNA` | Numeric | Cq for RNAseP-RNA | 0-50 or NA | Yes |

### Target Calls

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `Call_177T` | String | 177T detection call | `Positive`, `Negative`, `Indeterminate` | Yes |
| `Call_18S2` | String | 18S2 detection call | `Positive`, `Negative`, `Indeterminate` | Yes |
| `Call_RNAseP_DNA` | String | DNA quality call | `Positive`, `Negative` | Yes |
| `Call_RNAseP_RNA` | String | RNA quality call | `Positive`, `Negative` | Yes |

### Aggregated Sample Results

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `Wells_TNA_Positive` | Integer | Wells with T. brucei detected | 0-4 | Yes |
| `Wells_DNA_Positive` | Integer | Wells with DNA target positive | 0-4 | Yes |
| `Wells_RNA_Positive` | Integer | Wells with RNA target positive | 0-4 | Yes |
| `Sample_TNA_Positive` | Boolean | Sample-level TNA detection | TRUE/FALSE | Yes |
| `Sample_DNA_Positive` | Boolean | Sample-level DNA detection | TRUE/FALSE | Yes |
| `Sample_RNA_Positive` | Boolean | Sample-level RNA detection | TRUE/FALSE | Yes |

### RNA Preservation

| Field | Type | Description | Unit | Required |
|-------|------|-------------|------|----------|
| `Delta_RP` | Numeric | RNAseP-RNA minus RNAseP-DNA | Cq units | Calculated |
| `RNA_Preservation` | String | RNA quality category | `Good`, `Moderate`, `Poor` | Calculated |

### Decision Tree Output

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `PipelineCategory` | String | Classification category | See Section 8.1 | Calculated |
| `PipelineDecision` | String | Final decision | `Positive`, `Negative`, `Review`, `Invalid` | Calculated |
| `Confidence` | String | Decision confidence | `High`, `Medium`, `Low` | Calculated |

---

## 4. ELISA Data (PE & VSG)

Source: `data/lsd/elisa_pe/*.xlsx`, `data/lsd/elisa_vsg/*.xlsx`

### Run Information

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `run_id` | String | ELISA run identifier | `ELISA-PE-001` | Yes |
| `plate_id` | String | Plate identifier | `Plate1` | Yes |
| `test_type` | String | ELISA type | `PE`, `VSG` | Yes |
| `test_date` | Date | Date of test | `2024-03-22` | Yes |

### Sample Identification

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `sample_id` | String | Sample identifier | `KPS-12345` | Yes |
| `well_position` | String | Well position | `A01` | Yes |
| `dilution` | Numeric | Sample dilution | 100 | Yes |
| `control_type` | String | Control designation | `Sample`, `POS`, `NEG`, `BLANK` | Yes |

### Raw Measurements

| Field | Type | Description | Unit | Required |
|-------|------|-------------|------|----------|
| `od_450` | Numeric | Optical density at 450nm | AU | Yes |
| `od_450_rep1` | Numeric | Replicate 1 OD | AU | No |
| `od_450_rep2` | Numeric | Replicate 2 OD | AU | No |

### Calculated Metrics

| Field | Type | Description | Formula | Required |
|-------|------|-------------|---------|----------|
| `pp_percent` | Numeric | Percent Positivity | (Sample OD / POS OD) * 100 | Calculated |
| `dod` | Numeric | Density Over Dilution | Sample OD - NEG OD | Calculated |
| `cv_percent` | Numeric | Coefficient of Variation | (SD / Mean) * 100 | Calculated |

### Classification

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `status` | String | Classification | `Positive`, `Negative`, `Borderline` | Calculated |
| `run_valid` | Boolean | Run passed QC | TRUE/FALSE | Calculated |
| `sample_valid` | Boolean | Sample passed QC | TRUE/FALSE | Calculated |

---

## 5. iELISA Data

Source: `data/lsd/ielisa/*.xlsx`

### Run Information

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `run_id` | String | iELISA run identifier | `iELISA-001` | Yes |
| `plate_id` | String | Plate identifier | `Plate1` | Yes |
| `antigen` | String | Antigen type | `LiTat1.3`, `LiTat1.5` | Yes |
| `test_date` | Date | Date of test | `2024-03-23` | Yes |

### Sample Identification

| Field | Type | Description | Example | Required |
|-------|------|-------------|---------|----------|
| `sample_id` | String | Sample identifier | `KPS-12345` | Yes |
| `well_position` | String | Well position | `A01` | Yes |
| `control_type` | String | Control designation | `Sample`, `POS`, `NEG` | Yes |

### Raw Measurements

| Field | Type | Description | Unit | Required |
|-------|------|-------------|------|----------|
| `od_450_600` | Numeric | OD at 450-600nm | AU | Yes |
| `od_neg` | Numeric | Negative control OD | AU | Yes |
| `od_pos` | Numeric | Positive control OD | AU | Yes |

### Calculated Metrics

| Field | Type | Description | Formula | Required |
|-------|------|-------------|---------|----------|
| `inhibition_pct` | Numeric | Percent inhibition | (1 - Sample/NEG) * 100 | Calculated |
| `inhibition_pct_f2` | Numeric | Formula 2 inhibition | Alternative formula | Calculated |

### Classification

| Field | Type | Description | Valid Values | Required |
|-------|------|-------------|--------------|----------|
| `status_l13` | String | LiTat 1.3 status | `Positive`, `Negative`, `Borderline` | Calculated |
| `status_l15` | String | LiTat 1.5 status | `Positive`, `Negative`, `Borderline` | Calculated |
| `status_final` | String | Combined status | `Positive`, `Negative`, `Borderline` | Calculated |

---

## 6. Derived/Calculated Fields

### Normalized Identifiers

| Field | Type | Description | Source |
|-------|------|-------------|--------|
| `normalized_id` | String | Standardized sample ID | `normalize_sample_id(barcode, lab_id)` |
| `biobank_matched` | Boolean | Successfully linked to biobank | Linking operation |

### Time Calculations

| Field | Type | Description | Formula |
|-------|------|-------------|---------|
| `transport_days` | Numeric | Days from collection to arrival | `date_arrivee - date_prelevement` |
| `processing_days` | Numeric | Days from arrival to extraction | `extraction_date - date_arrivee` |

### Aggregate Statistics

| Field | Type | Description | Context |
|-------|------|-------------|---------|
| `n_runs` | Integer | Number of runs for sample | Per-sample count |
| `n_positive_runs` | Integer | Positive run count | Per-sample count |
| `final_status` | String | Consolidated status | After retest resolution |

---

## 7. QC Fields

### Control Validation

| Field | Type | Description | Valid Values |
|-------|------|-------------|--------------|
| `pos_control_valid` | Boolean | Positive control passed | TRUE/FALSE |
| `neg_control_valid` | Boolean | Negative control passed | TRUE/FALSE |
| `ntc_valid` | Boolean | No-template control passed | TRUE/FALSE |

### Thresholds

| Field | Type | Description | Default Value |
|-------|------|-------------|---------------|
| `pos_od_min` | Numeric | Minimum positive control OD | 0.5 |
| `pos_od_max` | Numeric | Maximum positive control OD | 1.5 |
| `neg_od_max` | Numeric | Maximum negative control OD | 0.5 |
| `cv_max` | Numeric | Maximum acceptable CV% | 20 |

### Quality Flags

| Field | Type | Description | Example Values |
|-------|------|-------------|----------------|
| `qc_flags` | String | Comma-separated QC issues | `High CV`, `Low POS`, `Invalid NEG` |
| `qc_pass_count` | Integer | Number of replicates passing QC | 0-4 |

---

## 8. Standard Codes

### 8.1 MIC Pipeline Categories

| Code | Description | Criteria |
|------|-------------|----------|
| `control_ok` | Control validated | PC positive, NC/NTC negative |
| `control_fail` | Control failed | Control criteria not met |
| `positive_dna_rna` | DNA and RNA detected | Both 177T and 18S2 positive |
| `positive_dna_only` | DNA only detected | 177T positive, 18S2 negative |
| `positive_rna_only` | RNA only detected | 18S2 positive, 177T negative |
| `review_partial` | Partial detection | Some wells positive |
| `negative` | Not detected | All targets negative |
| `invalid_qc` | QC failure | RNAseP-DNA negative |

### 8.2 ELISA Classification Thresholds

| Test | Metric | Positive | Borderline | Negative |
|------|--------|----------|------------|----------|
| ELISA | PP% | >= 20% | 15-20% | < 15% |
| ELISA | DOD | >= 0.3 | 0.2-0.3 | < 0.2 |
| iELISA | Inhibition | >= 30% | 25-30% | < 25% |

### 8.3 Control Types

| Code | Description | Test Type |
|------|-------------|-----------|
| `Sample` | Unknown sample | All |
| `PC` / `POS` | Positive control | All |
| `NC` / `NEG` | Negative control | All |
| `NTC` | No-template control | MIC qPCR |
| `BLANK` | Blank well | ELISA |
| `STD` | Standard | ELISA |

### 8.4 Province Codes

| Code | Full Name |
|------|-----------|
| `KOR` | Kasai Oriental |
| `KOC` | Kasai Central |
| (Add site-specific codes) |

### 8.5 Screening Types

| Code | Description |
|------|-------------|
| `DA` | Depistage Actif (Active screening) |
| `DP` | Depistage Passif (Passive screening) |

---

## Appendix A: Data Flow

```
Source Excel Files
       │
       ▼
┌─────────────────┐
│    INGEST       │  → Raw data extraction
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│      QC         │  → Control validation, threshold checks
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   INTERPRET     │  → Classification, decision trees
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│    OUTPUT       │  → Standardized tidy format
└────────┬────────┘
         │
         ▼
   Dashboard Modules
```

---

## Appendix B: Field Naming Conventions

1. **Snake_case** for multi-word fields: `date_prelevement`, `sample_id`
2. **Prefixes** indicate source:
   - `Cq_` for Cq values
   - `Call_` for target calls
   - `Wells_` for well counts
   - `Sample_` for sample-level flags
3. **Suffixes** indicate:
   - `_percent` or `_pct` for percentages
   - `_valid` for boolean validation flags
   - `_date` for dates

---

*Last updated: December 2024*
