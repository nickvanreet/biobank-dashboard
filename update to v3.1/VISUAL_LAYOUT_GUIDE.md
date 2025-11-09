# 📐 Overview & Demographics - Visual Layout Guide

This document shows you what to expect when you open the new module.

---

## 🖥️ SECTION 1: SUMMARY OVERVIEW

### Top Row - KPI Cards (4 cards in a row)
```
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│  Total Samples   │ │ Active Screen DA │ │ Passive Screen DP│ │   Date Range     │
│                  │ │                  │ │                  │ │                  │
│      15,234      │ │  9,140 (60.0%)   │ │  6,094 (40.0%)   │ │  Jan 2024 to     │
│   [vial icon]    │ │  [users icon]    │ │  [user icon]     │ │  Oct 2025        │
│                  │ │                  │ │                  │ │  [calendar icon] │
└──────────────────┘ └──────────────────┘ └──────────────────┘ └──────────────────┘
   PRIMARY COLOR         INFO COLOR          SUCCESS COLOR       SECONDARY COLOR
```

### Middle Row - Geographic Distribution (2 charts side by side)
```
┌─────────────────────────────────────────┐ ┌─────────────────────────────────────────┐
│  Samples by Province                    │ │  Samples by Health Zone                 │
│                                         │ │                                         │
│      ▄▄▄▄                               │ │  Zone A        ████████████████████     │
│      ████                               │ │  Zone B        ███████████              │
│      ████       ▄▄▄                     │ │  Zone C        ██████                   │
│  ▄▄  ████   ▄▄  ███                     │ │  Zone D        ████                     │
│  ██  ████   ██  ███                     │ │  Zone E        ███                      │
│  ██  ████   ██  ███                     │ │  Zone F        ██                       │
│ ─────────────────────                   │ │ ────────────────────────────            │
│  Prov1 Prov2 Prov3                      │ │  (Top 15 zones shown)                   │
│                                         │ │                                         │
│  [Interactive: hover for counts]        │ │  [Interactive: hover for counts]        │
└─────────────────────────────────────────┘ └─────────────────────────────────────────┘
```

### Bottom - Timeline (Full Width)
```
┌─────────────────────────────────────────────────────────────────────────────────────┐
│  Sample Collection Timeline                                      [⛶ Full Screen]    │
│                                                                                     │
│   Samples                           Legend: ── Active (DA)  ── Passive (DP)        │
│    500│                                                                             │
│       │         ╱╲    ╱─╲                                                           │
│    400│    ╱╲  ╱  ╲  ╱   ╲    ╱─╲                                                   │
│       │   ╱  ╲╱    ╲╱     ╲  ╱   ╲                                                  │
│    300│  ╱                 ╲╱     ╲  ╱╲                                             │
│       │ ╱                          ╲╱  ╲╱╲                                          │
│    200│╱                                 ╲                                          │
│       │                                    ╲                                        │
│    100│                                     ╲╱                                       │
│       │                                                                             │
│      0└──────────────────────────────────────────────────────────────────────────  │
│       Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct                    │
│       2024                                                    2025                  │
│                                                                                     │
│  [Interactive: zoom, pan, hover, download PNG]                                     │
└─────────────────────────────────────────────────────────────────────────────────────┘
```

---

## 👥 SECTION 2: DEMOGRAPHICS ANALYSIS

### Top Row - Demographics KPI Cards (4 cards in a row)
```
┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐ ┌──────────────────┐
│      Male        │ │     Female       │ │   Median Age     │ │    Age Range     │
│                  │ │                  │ │                  │ │                  │
│  7,922 (52.0%)   │ │  7,312 (48.0%)   │ │    32 years      │ │   5 - 85 years   │
│   [♂ icon]       │ │   [♀ icon]       │ │  [cake icon]     │ │  [arrows icon]   │
│                  │ │                  │ │                  │ │                  │
└──────────────────┘ └──────────────────┘ └──────────────────┘ └──────────────────┘
   INFO COLOR          DANGER COLOR        SUCCESS COLOR       SECONDARY COLOR
```

### First Chart Row (2 charts side by side)
```
┌─────────────────────────────────────────┐ ┌─────────────────────────────────────────┐
│  Age Distribution                       │ │  Age by Sex                             │
│                                         │ │                                         │
│  Count                                  │ │   Age                                   │
│   600│    ▄▄                            │ │    85│                                  │
│      │   ████                           │ │      │                                  │
│   500│   ████ ▄▄                        │ │    65│              ┌───┐              │
│      │   ████████                       │ │      │         ┌────┤   ├────┐         │
│   400│   ████████ ▄▄                    │ │    45│    ┌────┤    │   │    ├────┐    │
│      │  ████████████ ▄▄                 │ │      │    │    │    └───┘    │    │    │
│   300│  ████████████████                │ │    25│    │    └─────────────┘    │    │
│      │  ████████████████ ▄▄             │ │      │    └───────────────────────┘    │
│   200│ ██████████████████████           │ │     5│                                  │
│      │ ██████████████████████ ▄         │ │      └────────────────────────────     │
│   100│███████████████████████████       │ │           M          F                  │
│      │                                   │ │                                         │
│     0└─────────────────────────          │ │  [Shows median, quartiles, outliers]   │
│       0  10  20  30  40  50  60  70  80  │ │  [Hover to see statistics]             │
│                 Age (years)               │ │                                         │
└─────────────────────────────────────────┘ └─────────────────────────────────────────┘
```

### Second Chart Row (2 charts side by side)
```
┌─────────────────────────────────────────┐ ┌─────────────────────────────────────────┐
│  Age Groups by Sex                      │ │  Sex Distribution by Study Type         │
│                                         │ │                                         │
│  Count                                  │ │  Count                                  │
│  2000│                                  │ │  5000│                                  │
│      │  ██ ██                           │ │      │  ██ ██                           │
│  1500│  ██ ██                           │ │  4000│  ██ ██                           │
│      │  ██ ██  ██ ██                    │ │      │  ██ ██                           │
│  1000│  ██ ██  ██ ██  ██ ██             │ │  3000│  ██ ██  ██ ██                    │
│      │  ██ ██  ██ ██  ██ ██  ██ ██      │ │      │  ██ ██  ██ ██                    │
│   500│  ██ ██  ██ ██  ██ ██  ██ ██  ██  │ │  2000│  ██ ██  ██ ██                    │
│      │  ██ ██  ██ ██  ██ ██  ██ ██  ██  │ │      │  ██ ██  ██ ██                    │
│     0└──────────────────────────────    │ │  1000│  ██ ██  ██ ██                    │
│       0-4 5-14 15-24 25-34 35-44...     │ │      │  ██ ██  ██ ██                    │
│                                         │ │     0└──────────────                    │
│       ■ Male  ■ Female                  │ │        DA    DP                         │
│                                         │ │                                         │
│  [Side-by-side bars for comparison]    │ │  ■ Male  ■ Female                       │
└─────────────────────────────────────────┘ └─────────────────────────────────────────┘
```

### Bottom - Demographics Summary Table (Full Width)
```
┌─────────────────────────────────────────────────────────────────────────────────────┐
│  Demographic Summary by Health Zone                                   🔍 Search:    │
│                                                                                     │
│  Health Zone   │ Total │ Male │ Female │ % Male │ % Female │ Median Age │Age Range │
│  ─────────────────────────────────────────────────────────────────────────────────  │
│  Dipumba       │ 3,456 │1,802 │ 1,654  │  52.1  │   47.9   │     30     │  5-78   │
│  █████████████████████████████████                                                  │
│                                                                                     │
│  Tshofa        │ 2,891 │1,450 │ 1,441  │  50.2  │   49.8   │     33     │  8-82   │
│  ███████████████████████                                                            │
│                                                                                     │
│  Nkole         │ 2,345 │1,189 │ 1,156  │  50.7  │   49.3   │     35     │  12-80  │
│  ███████████████████                                                                │
│                                                                                     │
│  Kanshi        │ 1,987 │  995 │   992  │  50.1  │   49.9   │     31     │  6-85   │
│  ████████████████                                                                   │
│                                                                                     │
│  Bipemba       │ 1,654 │  842 │   812  │  50.9  │   49.1   │     29     │  5-75   │
│  █████████████                                                                      │
│                                                                                     │
│  [Showing 1 to 5 of 15 entries]                    [1] 2 3 > Last                  │
│                                                                                     │
│  [Sortable: click any column header]                                               │
│  [Searchable: type zone name in search box]                                        │
└─────────────────────────────────────────────────────────────────────────────────────┘
```

---

## 🎨 Color Coding Guide

### Theme Colors
```
PRIMARY (Blue)    #3498DB  ■  Used for: Total samples, male, general charts
SUCCESS (Green)   #27AE60  ■  Used for: DP samples, good indicators, health zones
INFO (Cyan)       #3498DB  ■  Used for: DA samples, information
DANGER (Pink)     #E91E63  ■  Used for: Female, warnings
WARNING (Orange)  #F39C12  ■  Used for: Caution items
SECONDARY (Gray)  #95A5A6  ■  Used for: Less emphasis items
```

### Study Type Colors
```
Active Screening (DA)    ── Blue   (#3498DB)
Passive Screening (DP)   ── Green  (#27AE60)
```

### Sex Colors
```
Male     ■ Blue  (#3498DB)
Female   ■ Pink  (#E91E63)
```

---

## 📱 Interactive Features

### All Charts Support:
```
✓ Hover to see detailed values
✓ Click and drag to zoom (on applicable charts)
✓ Double-click to reset view
✓ Legend items clickable to show/hide series
✓ Download as PNG (camera icon)
✓ Auto-scaling based on data
```

### Timeline Special Features:
```
✓ Full-screen mode (⛶ button)
✓ Zoom into specific date ranges
✓ Pan left/right through time
✓ Show/hide DA or DP by clicking legend
✓ Box select for detailed zoom
```

### Table Special Features:
```
✓ Sort ascending/descending (click header)
✓ Search by health zone name
✓ Page through results (15 per page)
✓ See total entry count
✓ Color bars show relative sizes
```

---

## 📏 Responsive Layout

### Desktop (Wide Screen):
```
KPIs:    [Card] [Card] [Card] [Card]
Charts:  [Chart     ] [Chart     ]
Timeline: [                       ]
```

### Laptop (Medium Screen):
```
KPIs:    [Card] [Card]
         [Card] [Card]
Charts:  [Chart     ]
         [Chart     ]
Timeline: [                  ]
```

### Tablet (Narrow):
```
KPIs:    [Card]
         [Card]
         [Card]
         [Card]
Charts:  [Chart]
         [Chart]
Timeline: [         ]
```

---

## 🔍 What Each Visualization Shows

### KPI Cards
```
Purpose:     Quick metrics at a glance
Updates:     Real-time with filters
Use case:    Executive summary, report headers
Click:       Not clickable, display only
```

### Province Bar Chart
```
Purpose:     Geographic distribution at province level
Updates:     Real-time with filters
Use case:    Identify coverage gaps, compare regions
Click:       Hover shows exact counts
```

### Health Zone Chart
```
Purpose:     Detailed geographic distribution
Updates:     Real-time with filters  
Use case:    Find top zones, plan expansion
Click:       Hover shows exact counts
Note:        Shows top 15 zones only (keeps chart readable)
```

### Timeline
```
Purpose:     Temporal patterns over time
Updates:     Real-time with filters
Use case:    Identify gaps, trends, seasonality
Click:       Full-screen, zoom, pan, hover
Note:        Weekly aggregation for clarity
```

### Age Histogram
```
Purpose:     Overall age distribution shape
Updates:     Real-time with filters
Use case:    Check for age biases, normal distribution
Click:       Hover shows bin counts
Note:        30 bins for detail
```

### Age by Sex Box Plot
```
Purpose:     Compare age distributions M vs F
Updates:     Real-time with filters
Use case:    Check for sex-based age differences
Click:       Hover shows statistics (median, IQR, etc.)
Note:        Outliers shown as individual points
```

### Age Groups Bar Chart
```
Purpose:     Detailed age group comparison
Updates:     Real-time with filters
Use case:    Target specific age groups
Click:       Hover shows exact counts
Note:        8 age categories for WHO standards
```

### Sex by Study Chart
```
Purpose:     Compare DA vs DP sex composition
Updates:     Real-time with filters
Use case:    Check if study types screen different populations
Click:       Hover shows counts and percentages
```

### Demographics Table
```
Purpose:     Complete zone-level breakdown
Updates:     Real-time with filters
Use case:    Detailed comparison, find outliers
Click:       Sort, search, page through
Note:        Comprehensive view with all metrics
```

---

## 🎯 Reading the Visualizations

### Good Patterns to See:
```
✓ Timeline: Consistent upward trend or steady collection
✓ Age Histogram: Smooth, bell-shaped distribution
✓ Sex Balance: Roughly 50/50 in all charts
✓ Geographic: Multiple zones represented
✓ Age by Sex: Similar boxes for M and F
```

### Patterns Needing Attention:
```
⚠ Timeline: Large gaps or sudden drops
⚠ Age Histogram: Multiple peaks or heavy skew
⚠ Sex Balance: >70% one sex
⚠ Geographic: Heavy concentration in 1-2 zones
⚠ Age by Sex: Very different medians or ranges
```

---

## 💡 Pro Tips

### For Presentations:
1. Start with KPI cards for impact
2. Show timeline to demonstrate scope
3. Use demographics to show reach
4. Reference table for details

### For Analysis:
1. Use filters to isolate interesting patterns
2. Download timeline as PNG for reports
3. Sort table to find extremes
4. Compare charts side-by-side

### For Monitoring:
1. Check KPIs weekly
2. Review timeline for consistent collection
3. Monitor sex balance monthly
4. Compare zones quarterly

---

**This visual guide shows:**
- ✓ Layout and positioning
- ✓ Chart types and styles
- ✓ Interactive features
- ✓ Color coding
- ✓ Data interpretation
- ✓ Best practices

**Ready to explore?** Launch the app and see it all in action!

```bash
shiny::runApp()
```
