# Predictive Analytics Module - Risk Calculation Methodology

This document explains how risk scores are calculated in the Predictive Analytics module for HAT (Human African Trypanosomiasis) surveillance.

---

## Overview

The module calculates risk at three levels:
1. **Health Zone Risk** - Which geographic zones need priority surveillance
2. **Structure Risk** - Which health facilities are likely to find positive cases
3. **Demographic Risk** - Which population groups show higher positivity

---

## 1. Health Zone Risk Calculation

### Data Sources Used
- **Biobank data**: Sample counts, collection dates, geographic distribution
- **MIC qPCR data**: Molecular positivity (uses `FinalCall` column)
- **Serological data**: Combined from ELISA-PE, ELISA-VSG, and iELISA

### Risk Components

| Component | Weight | Description |
|-----------|--------|-------------|
| Molecular Risk | 35% | Based on MIC qPCR positivity rate |
| Serological Risk | 30% | Based on combined ELISA/iELISA positivity |
| Sample Density | 20% | Normalized sample volume (higher = more active surveillance) |
| Recency Score | 15% | How recently samples were collected |

### Calculation Steps

1. **Molecular Risk** (0-100 scale)
   ```
   mic_positivity_rate = (mic_positive / mic_tested) * 100
   molecular_risk = min(100, mic_positivity_rate * 10)
   ```
   - Positivity rate is scaled up by 10x to make small rates visible
   - Capped at 100

2. **Serological Risk** (0-100 scale)
   ```
   sero_positivity_rate = (sero_positive / sero_tested) * 100
   serological_risk = min(100, sero_positivity_rate * 10)
   ```
   - Combines ELISA-PE + ELISA-VSG + iELISA results
   - iELISA: positive if `positive_L13 = TRUE` OR `positive_L15 = TRUE`

3. **Sample Density Score** (0-100 scale)
   ```
   sample_density_score = rescale(total_samples, to = [0, 100])
   ```
   - Normalized across all zones
   - Higher sample counts indicate more active surveillance areas

4. **Recency Score** (0-100 scale)
   ```
   days_since_last = today - last_sample_date
   recency_score = max(0, 100 - days_since_last / 2)
   ```
   - Decays over ~200 days
   - Recent activity = higher priority

5. **Final Risk Score**
   ```
   risk_score = (molecular_risk * 0.35) +
                (serological_risk * 0.30) +
                (sample_density_score * 0.20) +
                (recency_score * 0.15)
   ```

### Risk Categories

| Category | Score Range |
|----------|-------------|
| Very High | >= 75 |
| High | 50 - 74 |
| Medium | 25 - 49 |
| Low | < 25 |

---

## 2. Structure Risk Calculation

Structure-level predictions identify which health facilities are most likely to find positive cases.

### Base Risk Score

The base score is determined by historical positivity:

```
historical_positivity = average(mic_positivity, sero_positivity)
```

If only one type of data exists, that value is used directly.

### Risk Score Tiers

| Positivity Rate | Base Score |
|-----------------|------------|
| > 10% | 90 + (rate - 10) * 0.5 |
| 5-10% | 70 + (rate - 5) * 4 |
| 0-5% | 40 + rate * 6 |
| 0% with >= 50 samples | 30 |
| 0% with >= 20 samples | 20 |
| 0% with < 20 samples | 10 |

### Recency Adjustment

The base score is multiplied by a recency factor:

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

## 3. Serological Positivity Calculation

### Data Sources Combined

1. **ELISA-PE** (Plasmodium falciparum Extract)
2. **ELISA-VSG** (Variant Surface Glycoprotein)
3. **iELISA** (Inhibition ELISA with L13 and L15 antigens)

### How Positivity is Determined

**ELISA (PE and VSG):**
- Uses `positive` column (boolean) or `Result` column
- Positive values: `TRUE`, `1`, `"Positive"`, `"POSITIVE"`, `"POS"`, `"Pos"`

**iELISA:**
- Uses `positive_L13` and `positive_L15` boolean columns
- A sample is positive if EITHER column is `TRUE`

### Combined Calculation

```
sero_tested = elisa_tested + ielisa_tested
sero_positive = elisa_positive + ielisa_positive
sero_positivity_rate = (sero_positive / sero_tested) * 100
```

---

## 4. Demographic Risk Analysis

### Sex-Based Analysis
- Compares molecular positivity rates between Male and Female
- Identifies which sex shows higher positivity for targeted screening

### Age Group Analysis
- Groups: 0-4, 5-14, 15-24, 25-34, 35-44, 45-54, 55-64, 65+
- Calculates positivity rate per age group
- Identifies highest-risk age cohorts

---

## 5. Temporal Predictions

### Trend Analysis
- Uses 3-month moving average to smooth fluctuations
- Analyzes last 6 months to determine trend direction:
  - **Increasing**: trend > 0.5 percentage points/month
  - **Decreasing**: trend < -0.5 percentage points/month
  - **Stable**: between -0.5 and 0.5

### Forecast
- Simple linear extrapolation based on recent trend
- Confidence level:
  - **High**: >= 12 months of historical data
  - **Moderate**: 6-11 months of data

### Seasonal Patterns
- Aggregates samples by month of year
- Identifies seasonal variations in sampling/positivity

---

## 6. Watchlist Generation

The watchlist prioritizes surveillance targets:

1. **Priority Zones**: Top N health zones by risk score
2. **Priority Structures**: Top N structures by risk score
3. **Recent Positives**: Structures with positives in last 90 days
4. **Emerging Hotspots**: High/Very High risk + activity in last 60 days

---

## Interpretation Guidelines

### When to Prioritize a Zone
- Very High or High risk category
- Rising molecular AND serological positivity
- Recent sampling activity (high recency score)

### When to Prioritize a Structure
- "Likely to find positives" prediction
- Historical positivity > 5%
- Last sample within 90 days

### Limitations
- Risk scores are based on available data only
- Zones with no recent samples may have artificially low scores
- Small sample sizes reduce prediction confidence
- Absence of positives doesn't guarantee absence of disease

---

*Document generated for the HAT Biobank Dashboard Predictive Analytics Module*
