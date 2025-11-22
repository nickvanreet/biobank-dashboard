# New Modules Documentation
## Mbuji-Mayi Biobank Dashboard - v3.2

This document describes the two comprehensive new modules added to the biobank dashboard system.

---

## MODULE 1: SAMPLE JOURNEY MODULE

### Overview
The Sample Journey Module provides a complete view of any sample's testing journey through all diagnostic platforms. Users can search by Numero Lab or KPS number and instantly see all results across extraction, MIC molecular testing, ELISA-PE, ELISA-VSG, and iELISA.

### Files Created
1. **R/sampleJourneyHelpers.R** - Helper functions for data gathering and alert generation
2. **R/sampleJourneyVisualizations.R** - Visualization functions for timelines, gauges, and heatmaps
3. **R/modules/mod_10_sample_journey.R** - Main Shiny module (UI and server)

### Key Features

#### Search Interface
- Text input with search button for Numero Lab or KPS numbers
- Real-time sample status indicators
- Integration with existing global filters (study, province, health_zone, structure)

#### Visualizations
1. **Sample Timeline** - Horizontal Gantt-style chart showing chronological test progression
2. **DRS Volume Gauge** - Radial gauge (0-200 µL scale) with color zones
3. **MIC Results Heatmap** - Shows all 7 targets (TNA, gTNA, TBG, gTBG, TC
