# MIC Analysis Visualizations

The MIC Analysis module now uses the shared `filtered_base()` reactive so every plot
responds to the dashboard sidebar filters (date range, study, province, health
zone, and structure). This ensures that the analytic view always reflects the
same cohort shown elsewhere in the MIC workspace.

## Current Visualizations

The analysis tab includes the following filter-aware charts:

- **Detection scatter plots** comparing 177T vs 18S2 and RNAseP RNA vs DNA, with
  hover details for each sample.
- **Cq distribution boxes** grouped by target and by final call category to
  highlight shifts in amplification across cohorts.
- **Replicate concordance panels** (heatmap and bar chart) that summarise how
  many replicates crossed threshold for each target.
- **Quality metrics** including ΔCq histograms, violin plots by detection
  outcome, and QC pass rate bars.
- **Clinical decision validation** via detection-pattern heatmaps and ΔCq
  distributions for 18S2 – 177T comparisons.
- **Temporal and geographic trends** showing weekly volume/positivity alongside
  province-level positivity and RNA quality summaries.

All of the plots above consume the filtered sample tibble so that tightening the
sidebar filters will immediately narrow the data displayed.

## Additional Ideas

Future iterations could add a few specialised plots that complement the current
suite:

- **Replicate-level concordance scatter** plotting replicate pairs (e.g. Cq_1 vs
  Cq_2) to surface systematic pipetting issues.
- **Run stability ribbon** that layers weekly positivity with confidence bands
  around the mean to visualise statistical noise.
- **Control drift sparklines** for each run that track positive/negative control
  Cq values alongside alert thresholds.
- **Geospatial heat map** overlaying positivity rates on an interactive map when
  shapefiles become available.
- **Longitudinal cohort tracker** to monitor how individual participants’ Ct
  values evolve across repeated visits.

These additions would deepen clinical QA by focusing on replicate behaviour,
control performance, and geographic context.
