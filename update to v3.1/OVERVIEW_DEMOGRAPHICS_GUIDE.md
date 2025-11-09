# Overview & Demographics Module Guide

## What's New - Version 3.1

You now have a comprehensive **Overview & Demographics** module that combines high-level summaries with detailed demographic analysis. This module gives you instant insights into your biobank data distribution, coverage, and participant demographics.

## 📊 Module Overview

The module is divided into two main sections:

### 1. **Summary Overview**
Quick insights into your screening program's scope and geographic distribution

### 2. **Demographics Analysis**
Detailed breakdown of participant characteristics by age and sex

---

## 🎯 Summary Overview Section

### KPI Cards (Top Row)

**Total Samples**
- Shows the complete count of samples in your filtered dataset
- Updates in real-time as you apply filters

**Active Screening (DA)**
- Number of samples from active screening
- Shows both count and percentage of total

**Passive Screening (DP)**
- Number of samples from passive screening
- Shows both count and percentage of total

**Date Range**
- Displays the time span of sample collection
- Format: "MMM YYYY to MMM YYYY"

### Geographic Visualizations

**Samples by Province**
- Bar chart showing sample distribution across provinces
- Interactive: hover to see exact counts
- Sorted by sample count (highest first)

**Samples by Health Zone**
- Horizontal bar chart (easier to read zone names)
- Shows top 15 health zones by sample count
- Color-coded in green
- Sorted by sample count

### Timeline Analysis

**Sample Collection Timeline**
- Line chart showing samples collected over time
- Aggregated by week for clarity
- Color-coded by study type:
  - Blue: Active Screening (DA)
  - Green: Passive Screening (DP)
- Interactive features:
  - Hover for exact date and count
  - Zoom in/out
  - Full-screen mode (button in top-right)
  - Download as PNG

---

## 👥 Demographics Analysis Section

### Demographics KPI Cards

**Male Count**
- Total male participants
- Percentage of all participants with known sex

**Female Count**
- Total female participants
- Percentage of all participants with known sex

**Median Age**
- Central age value (50th percentile)
- Displayed in years

**Age Range**
- Minimum to maximum ages
- Format: "X - Y years"

### Age & Sex Visualizations

#### 1. Age Distribution Histogram
- Shows overall age distribution
- 30 bins for detailed granularity
- Helps identify:
  - Most common age groups
  - Age distribution shape
  - Potential age biases

**Interpretation:**
- Symmetric = balanced age representation
- Right-skewed = more younger participants
- Left-skewed = more older participants

#### 2. Age by Sex Box Plot
- Side-by-side comparison of male vs female age distributions
- Box shows:
  - Middle line = median
  - Box edges = 25th and 75th percentiles
  - Whiskers = range
  - Dots = outliers

**What to Look For:**
- Are males and females screened at similar ages?
- Are there age outliers?
- Which sex has a wider age range?

#### 3. Age Groups by Sex
- Grouped bar chart
- Age groups:
  - 0-4 years
  - 5-14 years
  - 15-24 years
  - 25-34 years
  - 35-44 years
  - 45-54 years
  - 55-64 years
  - 65+ years
- Side-by-side bars for easy M/F comparison

**Use Cases:**
- Identify which age groups are most/least represented
- Check for sex balance within each age group
- Target underrepresented groups

#### 4. Sex Distribution by Study Type
- Shows male/female breakdown for DA and DP separately
- Answers: "Is sex distribution consistent across study types?"

**Insights:**
- Are males and females equally represented in active vs passive screening?
- Do study types have different sex compositions?

### Demographics Summary Table

**Comprehensive breakdown by Health Zone showing:**

| Column | Description |
|--------|-------------|
| Health Zone | Zone name |
| Total | Total samples from this zone |
| Male | Male count |
| Female | Female count |
| % Male | Percentage male |
| % Female | Percentage female |
| Median Age | Central age for this zone |
| Age Range | Min-Max ages |

**Features:**
- Sortable by any column (click header)
- Searchable (type in search box)
- Shows 15 rows per page
- Total row count has color bar (blue gradient)

**Use Cases:**
- Compare demographics across zones
- Identify zones with unusual age or sex distributions
- Plan targeted screening based on zone characteristics

---

## 💡 How to Use This Module

### Basic Workflow

1. **Load Data**
   - Use the sidebar to select and load your biobank Excel file
   - Wait for data cleaning to complete

2. **Apply Filters (Optional)**
   - Set date range
   - Select study type (DA/DP/All)
   - Choose province
   - Choose health zone
   - All visualizations update automatically

3. **Navigate to Overview & Demographics**
   - Click the "Overview & Demographics" tab
   - Scroll through Overview section (top)
   - Scroll through Demographics section (bottom)

4. **Interact with Charts**
   - Hover over any chart element for details
   - Click and drag to zoom on timeline
   - Double-click to reset zoom
   - Use full-screen button on timeline for detailed view

5. **Analyze the Table**
   - Sort by clicking column headers
   - Search for specific health zones
   - Compare median ages across zones
   - Check sex distribution patterns

### Advanced Tips

**Finding Underrepresented Groups:**
1. Look at Age Groups by Sex chart
2. Identify small bars = underrepresented
3. Apply filters to focus on those groups
4. Check which zones contribute to the gap (table)

**Temporal Analysis:**
1. Use timeline to identify collection patterns
2. Look for gaps or spikes
3. Compare DA vs DP over time
4. Apply date filters to focus on specific periods

**Geographic Patterns:**
1. Check province distribution
2. Drill down to health zones
3. Use table to compare zone characteristics
4. Look for zones with unusual demographics

---

## 📈 Interpreting the Results

### What to Look For

**Good Signs:**
- ✅ Balanced sex distribution (close to 50/50)
- ✅ Wide age range coverage
- ✅ Consistent collection over time
- ✅ Multiple provinces/zones represented
- ✅ Similar demographics across study types

**Potential Concerns:**
- ⚠️ Extreme sex imbalance (>70% one sex)
- ⚠️ Missing age groups (gaps in histogram)
- ⚠️ Collection gaps in timeline
- ⚠️ Heavy concentration in few zones
- ⚠️ Very different DA vs DP demographics

**Action Items:**
- 🎯 Target underrepresented groups
- 🎯 Investigate collection gaps
- 🎯 Expand to underserved zones
- 🎯 Balance DA vs DP demographics

---

## 🔧 Technical Details

### Data Requirements

**Required Fields:**
- At least one identifier (barcode or lab_id)
- date_sample (for timeline)
- province (for geographic analysis)
- health_zone (for zone analysis)
- study (for DA/DP breakdown)
- age (for demographics)
- sex (for demographics)

**Handling Missing Data:**
- Charts automatically exclude NA values
- KPIs show counts of available data
- Table shows "N/A" for missing medians
- No errors if fields are missing entirely

### Performance

**Optimized for:**
- Datasets up to 50,000 rows
- Real-time filter updates
- Smooth chart interactions
- Fast table rendering

**If Slow:**
- Filter to reduce data size
- Check for very large date ranges
- Ensure data is properly cleaned

---

## 🆘 Troubleshooting

### Problem: "No data available" messages

**Solution:**
- Ensure data is loaded (check sidebar status)
- Verify filters aren't too restrictive
- Check that required fields exist in your data

### Problem: Charts show unexpected patterns

**Solution:**
- Check data quality in the Data Quality tab
- Verify date formats are parsed correctly
- Look for outliers (very old ages, future dates)

### Problem: Table shows NaN or Inf

**Solution:**
- This happens when a zone has no valid data
- The cleaner should prevent this, but check:
  - Are there zones with all missing ages?
  - Are there zones with all missing sex?

### Problem: Timeline has gaps

**Interpretation:**
- Actual gaps in sample collection
- Not an error - this shows real collection patterns
- Use to identify periods needing investigation

---

## 📊 Example Questions This Module Answers

### Program Scope
- How many samples do we have total?
- What's our DA vs DP balance?
- When did we start collecting?
- What's our collection rate per week?

### Geographic Coverage
- Which provinces are covered?
- Which zones have the most samples?
- Are we missing any major zones?
- Is coverage balanced across regions?

### Demographics
- What's our overall age distribution?
- Are we reaching all age groups?
- Is our male/female ratio balanced?
- Do age patterns differ by sex?
- Are demographics consistent across zones?
- Do DA and DP screen similar populations?

### Quality Assessment
- Are there collection gaps we need to explain?
- Are there zones with unusual demographics?
- Is there bias toward certain age groups?
- Is sex distribution reasonable?

---

## 🎓 Best Practices

### Regular Monitoring
1. Check overview weekly to track progress
2. Monitor timeline for consistent collection
3. Review demographics monthly for balance
4. Compare zones quarterly for equity

### Before Reporting
1. Apply appropriate date filters
2. Check for data quality issues first
3. Verify outliers make sense
4. Cross-reference with raw data if unusual

### When Presenting
1. Start with KPI cards for high-level summary
2. Show timeline to demonstrate scope
3. Use demographics to show reach
4. Highlight specific zones if relevant
5. Address any imbalances proactively

---

## 🚀 Future Enhancements

Potential additions to this module:

- [ ] Add case history breakdown (previous/new cases)
- [ ] Include treatment status distribution
- [ ] Add sample type presence (DRS/DBS)
- [ ] Include transport time metrics
- [ ] Add year-over-year comparisons
- [ ] Export summary reports
- [ ] Add benchmark comparisons

---

## 📁 File Locations

**Module File:**
```
R/modules/mod_02_overview_demographics.R
```

**Integration:**
```
app.R (lines added for UI and server)
```

**Dependencies:**
- Uses filtered_data from mod_data_manager
- Requires all core utilities and cleaners
- Uses plotly, DT, and bslib components

---

## ✅ Checklist

Before using this module, verify:

- [ ] Module file exists in R/modules/
- [ ] app.R includes the module in UI section
- [ ] app.R includes the module in server section
- [ ] Data loaded successfully
- [ ] Required fields present (date_sample, study, age, sex, etc.)
- [ ] Filters are set appropriately
- [ ] Data Quality tab shows no major issues

---

**Version:** 3.1  
**Module:** Overview & Demographics  
**Created:** November 2025  
**Status:** ✅ Production Ready

For questions or issues, check the main README.md or DEPLOYMENT_GUIDE.md files.
