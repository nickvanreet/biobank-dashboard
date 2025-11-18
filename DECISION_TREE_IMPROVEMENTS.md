# Decision Tree Improvements

This document describes the improvements made to the sample decision tree in Module 05 (MIC qPCR Analysis).

## Overview

The decision tree for classifying samples has been enhanced with:
1. **Explanatory columns** for better understanding of decisions
2. **Refined logic** to handle edge cases more accurately
3. **Visualization functions** for exploring decision paths
4. **Validation warnings** for conflicts between well-level and aggregate-level calls

## New Output Columns

### Decision Information

- **`FinalCall`**: The classification result (Positive, Negative, Indeterminate, etc.)
- **`DecisionStep`**: Which decision tree step was matched (Step0-Step8)
- **`DecisionReason`**: Human-readable explanation of why this classification was chosen
- **`ConfidenceScore`**: Confidence level (High, Medium, Low) based on replicate agreement
- **`WellSummary`**: Compact summary of well-level detections (e.g., "DNA:2+/0s, RNA:2+/0s, TNA:2+/0s")

### Conflict Detection

- **`WellAggregateConflict`**: TRUE if well-level and sample-level calls disagree
- **`ConflictDetails`**: Explanation of the disagreement

### Enhanced Flags

The `Flags` column now includes:
- `WellAggregateConflict`: When well-level and aggregate-level calls don't match
- `LowConfidence`: When confidence score is Low (for non-control samples)

## Decision Tree Steps

The enhanced decision tree follows this sequence:

```
Step 0: Control Handling
├─ Control samples → "Control" or "Control_Fail"

Step 1: QC Validity
├─ All replicates failed QC → "Invalid"

Step 2: Strong TNA Positive (Highest Priority)
├─ ≥2 wells with both DNA+RNA positive → "Positive"

Step 3: Weak TNA Signal
├─ 1 well with both DNA+RNA positive → "Indeterminate"

Step 4: Single-Target Detection
├─ Step 4a: ≥2 wells DNA+ only → "Positive_DNA"
├─ Step 4b: ≥2 wells RNA+ only → "Positive_RNA"

Step 5: Late Positives
├─ ≥2 wells with late TNA detection → "LatePositive"

Step 6: Weak/Mixed Signals
├─ Step 6a: 1 well DNA+ or RNA+ with ≥2 negative reps → "Negative"
├─ Step 6b: 1 well with weak signal → "Indeterminate"

Step 7: Clear Negative
├─ ≥3 negative replicates → "Negative"

Step 8: Insufficient Data
└─ Default → "Indeterminate"
```

## Improved Edge Case Handling

### 1. Single Outlier with Negative Evidence (Step 6a)

**Before:** A single positive well would always result in "Indeterminate", even with 3 negative replicates.

**After:** If 1 well is positive but ≥2 replicates are clearly negative, the result is "Negative" (likely outlier).

**Example:**
```r
# Sample with 1 DNA positive well, 3 negative replicates
Wells_DNA_Positive = 1
NegativeReplicates = 3
FinalCall = "Negative"
DecisionStep = "Step6a"
DecisionReason = "Single DNA outlier with 3 negative replicates"
```

### 2. Confidence Scoring

Samples are now assigned confidence scores based on replicate agreement:

**High Confidence:**
- ≥3 wells TNA positive
- ≥3 negative replicates
- ≥3 wells DNA-only or RNA-only

**Medium Confidence:**
- 2 wells TNA positive (minimum threshold)
- 2 wells with late detection
- 2 wells DNA-only or RNA-only

**Low Confidence:**
- 1 well with any detection
- ≤2 replicates passed QC
- Insufficient valid replicates

### 3. Conflict Detection

The system now detects when well-level and aggregate-level calls disagree:

**Scenario A:** Well-level says positive, aggregate says negative
```r
# Example: 2 wells DNA positive, but mean Cq > threshold
Wells_DNA_Positive = 2
Sample_DNA_Positive = FALSE
WellAggregateConflict = TRUE
ConflictDetails = "Well-level DNA+ but aggregate Cq suggests negative/late"
```

**Scenario B:** Aggregate says positive, well-level doesn't meet threshold
```r
# Example: Mean Cq is positive, but only 1 well positive
Sample_TNA_Positive = TRUE
Wells_TNA_Positive = 1
WellAggregateConflict = TRUE
ConflictDetails = "Aggregate TNA+ but well-level doesn't meet threshold"
```

## Visualization Functions

### 1. Visualize Decision Path for a Single Sample

```r
# Load sample data
samples <- aggregate_samples_from_replicates(replicates, summary, settings, run_id)

# Visualize decision path for a specific sample
sample_row <- samples %>% filter(SampleName == "Sample_123")
cat(visualize_decision_path(sample_row))
```

**Output:**
```
═══════════════════════════════════════════════════════════════
SAMPLE: Sample_123
═══════════════════════════════════════════════════════════════

DECISION TREE PATH:
───────────────────────────────────────────────────────────────
  ○ Step0: Controls
  ○ Step1: QC Validity Check
  ✓ Step2: TNA Positive (>=2 wells) → MATCHED
  ○ Step3: Single TNA Well
  ○ Step4a: DNA-only Pattern
  ○ Step4b: RNA-only Pattern
  ○ Step5: Late Positive TNA
  ○ Step6a: Single Outlier + Negatives
  ○ Step6b: Weak Mixed Signals
  ○ Step7: Clear Negative (>=3 reps)
  ○ Step8: Insufficient Data

RESULT:
───────────────────────────────────────────────────────────────
  Final Call:   Positive
  Confidence:   Medium
  Reason:       Strong TNA positive: 2/4 wells with both DNA+RNA positive

WELL-LEVEL SUMMARY:
───────────────────────────────────────────────────────────────
  DNA:2+/0s, RNA:2+/0s, TNA:2+/0s

AGGREGATE VS WELL-LEVEL:
───────────────────────────────────────────────────────────────
  Sample TNA+:  YES  |  Wells TNA+:  2
  Sample DNA+:  YES  |  Wells DNA+:  2
  Sample RNA+:  YES  |  Wells RNA+:  2

Cq VALUES (Mean):
───────────────────────────────────────────────────────────────
  177T (DNA):      28.50  →  Positive
  18S2 (RNA):      29.30  →  Positive
  RNAseP-DNA:      25.10  →  Positive
  Delta (RNA-DNA): 0.80
═══════════════════════════════════════════════════════════════
```

### 2. Generate Summary Report

```r
# Summary of all samples
cat(summarize_decision_quality(samples))

# Summary of only indeterminate samples
cat(summarize_decision_quality(samples, filter_by = "Indeterminate"))

# Summary of samples with conflicts
cat(summarize_decision_quality(samples, filter_by = "Conflicts"))

# Summary of low confidence samples
cat(summarize_decision_quality(samples, filter_by = "LowConfidence"))
```

**Output:**
```
═══════════════════════════════════════════════════════════════
DECISION TREE QUALITY REPORT
═══════════════════════════════════════════════════════════════
Total Samples: 96

FINAL CALL DISTRIBUTION:
───────────────────────────────────────────────────────────────
  Positive            :   12  ( 12.5%)
  Negative            :   72  ( 75.0%)
  Indeterminate       :    8  (  8.3%)
  Control             :    3  (  3.1%)
  Control_Fail        :    1  (  1.0%)

CONFIDENCE DISTRIBUTION:
───────────────────────────────────────────────────────────────
  High                :   78
  Medium              :   10
  Low                 :    5

DECISION STEP USAGE:
───────────────────────────────────────────────────────────────
  Step0     :    4
  Step2     :   12
  Step6a    :    3
  Step6b    :    5
  Step7     :   72

Well/Aggregate Conflicts: 3
═══════════════════════════════════════════════════════════════
```

## Examples of Failure Cases

### Case 1: Alternating Detection Pattern

**Scenario:** DNA detected in wells 1-2, RNA detected in wells 3-4

```
Well 1: DNA Cq=28, RNA Cq=42 (negative)
Well 2: DNA Cq=29, RNA Cq=43 (negative)
Well 3: DNA Cq=42, RNA Cq=28 (negative)
Well 4: DNA Cq=43, RNA Cq=29 (negative)

Wells_TNA_Positive = 0 (no single well has both)
Wells_DNA_Positive = 2
Wells_RNA_Positive = 2

Decision:
  - Step 4a: SKIP (Wells_RNA_Positive != 0)
  - Step 4b: SKIP (Wells_DNA_Positive != 0)
  - Step 8: FinalCall = "Indeterminate"

DecisionReason: "Insufficient valid replicates (QC passed: 4)"
```

**Why it's confusing:** Both targets are clearly detected, but not in the same wells.

**How to interpret:** This pattern suggests technical issues (e.g., pipetting errors, well contamination). The "Indeterminate" call is appropriate and the `DecisionReason` now explains why.

### Case 2: Single Late Positive

**Scenario:** One well shows late detection, three wells negative

```
Well 1: DNA Cq=36, RNA Cq=37 (both late positive)
Wells 2-4: All negative

Wells_TNA_Suspect = 1
NegativeReplicates = 3

Decision:
  - Step 6b: Wells_TNA_Suspect == 1 → FinalCall = "Indeterminate"

DecisionReason: "Single well with late/suspect TNA detection"
ConfidenceScore: "Low"
```

**Why it's confusing:** Should this be "Negative" since 3/4 replicates are negative?

**How to interpret:** Late positives near the detection limit are unreliable. A single late positive is not sufficient evidence for a positive call, but also not strong enough to ignore completely. The `DecisionReason` and `ConfidenceScore` now clarify this is a weak signal.

### Case 3: QC Failures

**Scenario:** Half the replicates fail QC

```
Wells 1-2: QC failed (no RNAseP-DNA)
Well 3: All negative
Well 4: DNA Cq=32 positive, RNA negative

QC_Pass_Count = 2
Wells_DNA_Positive = 1

Decision:
  - Step 6b: Wells_DNA_Positive == 1 → FinalCall = "Indeterminate"

DecisionReason: "Single weak signal: DNA=1, RNA=0"
ConfidenceScore: "Low"
Flags: "LowConfidence"
```

**Why it's confusing:** Only 2 wells passed QC, should this be "Invalid"?

**How to interpret:** With only 2 valid replicates, we can't make a confident call. The `ConfidenceScore` and `Flags` now highlight this issue.

## Column Order in Output

The columns are now organized for better readability:

1. Sample identifiers (RunID, SampleID, SampleName, ControlType)
2. Cq statistics (median, mean, SD for each target)
3. Target calls (Call_177T, Call_18S2, etc.)
4. Deltas (Delta_18S2_177T, Delta_RP)
5. **Flags and AnyFlag** (summary of all issues)
6. **FinalCall** (classification result)
7. **DecisionStep** (which step matched)
8. **DecisionReason** (why this classification)
9. **ConfidenceScore** (High/Medium/Low)
10. **WellSummary** (compact detection summary)
11. **WellAggregateConflict** (conflict detected?)
12. **ConflictDetails** (conflict explanation)
13. Pipeline data (PipelineCategory, etc.)
14. Well-level and sample-level detection flags

## Usage Tips

### Finding Problematic Samples

```r
# Filter for conflicts
conflicts <- samples %>% filter(WellAggregateConflict == TRUE)

# Filter for low confidence
low_conf <- samples %>% filter(ConfidenceScore == "Low")

# Filter for indeterminate + low confidence
problematic <- samples %>%
  filter(FinalCall == "Indeterminate" & ConfidenceScore == "Low")

# Visualize each problematic sample
for (i in 1:nrow(problematic)) {
  cat(visualize_decision_path(problematic[i, ]))
  cat("\n\n")
}
```

### Exporting Detailed Results

```r
# Export with decision information
samples %>%
  select(SampleName, FinalCall, DecisionStep, DecisionReason,
         ConfidenceScore, WellSummary, ConflictDetails) %>%
  write_csv("decision_tree_detailed.csv")
```

### Quality Control

```r
# Generate quality report
report <- summarize_decision_quality(samples)
writeLines(report, "decision_tree_quality_report.txt")

# Check for systematic issues
samples %>%
  group_by(DecisionStep) %>%
  summarise(
    n = n(),
    n_conflicts = sum(WellAggregateConflict, na.rm = TRUE),
    n_low_conf = sum(ConfidenceScore == "Low", na.rm = TRUE)
  )
```

## Changes to Decision Logic

### Step 6 Split

The original Step 6 has been split into two sub-steps:

**Step 6a (NEW):** Single weak signal with strong negative evidence
- If 1 well DNA+ or RNA+ AND ≥2 negative replicates → "Negative"
- This treats the single positive as a likely outlier

**Step 6b (ORIGINAL):** Mixed weak signals
- If 1 well DNA+ or RNA+ (without strong negative evidence) → "Indeterminate"
- This is appropriate when we don't have clear negative evidence

### Confidence Scoring Logic

Confidence is based on:
1. **Number of agreeing replicates** (≥3 = High, 2 = Medium, 1 = Low)
2. **QC pass count** (≤2 QC passed = Low)
3. **Control samples** (marked as "Control", not scored)
4. **Invalid samples** (marked as "Invalid", not scored)

## Future Improvements

Potential enhancements to consider:

1. **Machine Learning Integration**: Use ML to learn from validated samples and improve classification
2. **Delta Thresholds**: Add more sophisticated rules based on RNA-DNA delta values
3. **Replicate Variability**: Consider Cq standard deviation in confidence scoring
4. **Visual Dashboard**: Create an interactive dashboard for exploring decision paths
5. **Historical Comparison**: Track decision tree performance over time

## Technical Notes

- All new columns are added in the `aggregate_samples_from_replicates()` function
- Visualization functions are standalone and can be called separately
- The decision tree logic uses `case_when()` for clarity and maintainability
- Conflict detection uses separate logic to avoid circular dependencies
- Flags are accumulated and deduplicated before output

## File Location

- Code: `R/modules/mod_05_mic_qpcr.R`
- Functions:
  - `aggregate_samples_from_replicates()`: Lines 314-799 (enhanced)
  - `visualize_decision_path()`: Lines 817-935
  - `summarize_decision_quality()`: Lines 943-1047
