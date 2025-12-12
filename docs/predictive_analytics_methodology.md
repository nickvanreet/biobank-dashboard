# Predictive Analytics Module - Risk Calculation Methodology

This document explains how risk scores are calculated in the Predictive Analytics module for HAT (Human African Trypanosomiasis) surveillance.

---

## Overview

The module calculates risk at three levels:
1. **Health Zone Risk** - Which geographic zones need priority surveillance
2. **Structure Risk** - Which health facilities are likely to find positive cases
3. **Demographic Risk** - Which population groups show higher sampling activity

---

## 1. Data Sources

### Biobank Samples
- Core demographic and geographic data from collected samples
- Fields used: `health_zone`, `structure`, `sex`, `age`, `date_prel`

### MIC qPCR (Molecular)
- DNA/RNA detection using `FinalCall` column
- Possible values: Positive, Positive_DNA, Positive_RNA, LatePositive, Indeterminate, Negative

### ELISA-PE (Protein Extract)
- Serological testing using `status_final` column
- Values: Positive, Borderline, Negative, Invalid, Missing
- Thresholds: PP% >= 20 or DOD >= 0.3 = Positive; PP% 15-20 or DOD 0.2-0.3 = Borderline

### ELISA-VSG (Variable Surface Glycoprotein)
- Same structure as ELISA-PE with `status_final` column

### iELISA (Inhibition ELISA)
- Uses `positive_L13` and `positive_L15` boolean columns
- Positive if EITHER L13 OR L15 >= 30% inhibition

---

## 2. Weighted Scoring System

### Molecular (MIC qPCR) Weights

| Result | Weight | Description |
|--------|--------|-------------|
| Positive | 1.0 | Confirmed DNA+RNA detection |
| Positive_DNA | 0.8 | DNA target only detected |
| Positive_RNA | 0.8 | RNA target only detected |
| LatePositive | 0.6 | Late cycle amplification (Cq > threshold) |
| Indeterminate | 0.3 | Inconclusive result |
| Negative | 0.0 | No parasite detected |

### Serological (ELISA) Weights

| Result | Weight | Description |
|--------|--------|-------------|
| Positive | 1.0 | Clear positive (PP% >= 20 or DOD >= 0.3) |
| Borderline | 0.5 | Uncertain (PP% 15-20 or DOD 0.2-0.3) |
| Negative | 0.0 | No antibodies detected |
| Invalid/Missing | 0.0 | Not counted |

---

## 3. Health Zone Risk Calculation

### Risk Score Formula (0-100 scale)

```
Risk Score = (Molecular Risk × 0.35) +
             (Serological Risk × 0.30) +
             (Sample Density × 0.20) +
             (Recency Score × 0.15)
```

### Component Calculations

**Molecular Risk (35% weight)**
```
mic_weighted_rate = (mic_positive + late_positive*0.6 + indeterminate*0.3) / mic_tested * 100
molecular_risk = min(100, mic_weighted_rate × 10)
```

**Serological Risk (30% weight)**
```
sero_weighted_rate = (sero_positive + borderline*0.5) / sero_tested * 100
serological_risk = min(100, sero_weighted_rate × 10)
```
- Combines ELISA-PE + ELISA-VSG + iELISA data

**Sample Density (20% weight)**
```
sample_density_score = rescale(total_samples, to = [0, 100])
```
- Normalized across all zones

**Recency Score (15% weight)**
```
days_since_last = today - last_sample_date
recency_score = max(0, 100 - days_since_last / 2)
```
- Decays over ~200 days (score reaches 0 at 200 days without samples)

### Risk Categories

| Category | Score Range | Recommended Action |
|----------|-------------|--------------------|
| Very High | >= 75 | Immediate priority surveillance |
| High | 50 - 74 | Enhanced monitoring recommended |
| Medium | 25 - 49 | Regular surveillance |
| Low | < 25 | Routine monitoring |

---

## 4. Structure Risk Calculation

Structure-level predictions identify which health facilities are most likely to find positive cases.

### Historical Positivity Calculation

```
historical_positivity = average(mic_positivity, sero_positivity)
```
- If only one type available, uses that value
- Uses weighted rates including borderline/late positive

### Base Risk Score Tiers

| Historical Positivity | Base Score |
|-----------------------|------------|
| > 10% | 90 + (rate - 10) × 0.5 |
| 5-10% | 70 + (rate - 5) × 4 |
| 0-5% | 40 + rate × 6 |
| 0% with >= 50 samples | 30 |
| 0% with >= 20 samples | 20 |
| 0% with < 20 samples | 10 |

### Recency Adjustment

Final score is multiplied by a recency factor:

| Days Since Last Sample | Multiplier |
|------------------------|------------|
| <= 30 days | 1.0 (100%) |
| 31-90 days | 0.9 (90%) |
| 91-180 days | 0.7 (70%) |
| > 180 days | 0.5 (50%) |

### Prediction Labels

| Risk Score | Prediction |
|------------|------------|
| >= 75 | "Likely to find positives" |
| 50-74 | "Possible positives" |
| 25-49 | "Monitor closely" |
| < 25 | "Low probability" |

---

## 5. Serological Positivity Details

### Combined Serological Rate

All three serological assays are combined:

```
sero_tested = pe_tested + vsg_tested + ielisa_tested
sero_positive = pe_positive + vsg_positive + ielisa_positive
sero_borderline = pe_borderline + vsg_borderline
sero_weighted = sero_positive + (sero_borderline × 0.5)
sero_positivity_rate = sero_positive / sero_tested × 100
sero_weighted_rate = sero_weighted / sero_tested × 100
```

### iELISA Positivity

For iELISA, a sample is positive if:
- `positive_L13 = TRUE` (LiTat 1.3 inhibition >= 30%), OR
- `positive_L15 = TRUE` (LiTat 1.5 inhibition >= 30%)

---

## 6. Temporal Predictions

### Trend Analysis
- Uses 3-month rolling average to smooth fluctuations
- Analyzes last 6 months of data for trend direction

### Trend Direction Classification
- **Increasing**: trend > 0.5 percentage points/month
- **Decreasing**: trend < -0.5 percentage points/month
- **Stable**: between -0.5 and 0.5

### Forecast
- Simple linear extrapolation based on recent trend
- Confidence levels:
  - **High**: >= 12 months of historical data
  - **Moderate**: 6-11 months of data
  - **Low**: < 6 months of data

---

## 7. Watchlist Generation

The watchlist prioritizes surveillance targets:

1. **Priority Zones**: Top N health zones by risk score
2. **Priority Structures**: Top N structures by risk score
3. **Recent Positives**: Structures with historical positivity > 0 and activity in last 90 days
4. **Emerging Hotspots**: High/Very High risk structures with activity in last 60 days

---

## 8. Module Tabs

| Tab | Purpose |
|-----|---------|
| Watchlist | Quick view of priority targets with KPIs |
| Zone Risk | Detailed health zone risk analysis with charts |
| Structure Risk | Structure-level predictions with KPIs |
| Demographics | Sample distribution by sex and age group |
| Trends & Forecast | Temporal analysis and positivity trends |
| Geography | Geographic coverage and provincial summary |
| Model | This methodology explanation |
| Data | Raw risk analysis data export |

---

## 9. Interpretation Guidelines

### When to Prioritize a Zone
- Very High or High risk category
- Serological AND molecular data available
- Recent sampling activity (high recency score)
- Historical positives detected

### When to Prioritize a Structure
- "Likely to find positives" or "Possible positives" prediction
- Historical positivity > 5%
- Last sample within 90 days
- Multiple positive results in recent history

### Limitations
- Risk scores are based on available data only
- Zones with no recent samples may have artificially low scores
- Small sample sizes (< 20) reduce prediction confidence
- Model assumes historical patterns predict future occurrence
- Absence of positives does not guarantee absence of disease
- Serological positives may include past infections

---

*Document version: 2.0 - Updated December 2025*
*Generated for the HAT Biobank Dashboard Predictive Analytics Module*
