# Sample Journey PDF Export Feature

## Overview

The Sample Journey module now includes a beautiful PDF export feature that generates comprehensive reports for individual samples.

## Features

The PDF report includes:

1. **Summary Statistics** - Overview of all data sources and test counts
2. **Timeline Visualization** - Visual representation of the sample's journey through different stages
3. **Biobank Information** - Province, health zone, structure, collection dates
4. **Extraction Data** - Extraction dates, DRS volume with visual gauge
5. **MIC qPCR Results** - Detailed Cq values, target detection, quality metrics
6. **ELISA Results** - Both PE and VSG tests with DOD and PP% values
7. **iELISA Results** - LiTat 1.3 and 1.5 antigen tests with inhibition percentages
8. **Quality Alerts** - All flagged issues and warnings

## Required Packages

The PDF export feature requires the following R packages:

```r
# Core packages (should already be installed)
library(tidyverse)
library(rmarkdown)
library(knitr)
library(kableExtra)

# For visualization
library(ggplot2)
library(plotly)

# Optional (for enhanced plotly exports)
library(webshot2)  # or kaleido
```

### Installing Required Packages

If any packages are missing, install them using:

```r
install.packages(c("rmarkdown", "knitr", "kableExtra", "ggplot2"))
```

For LaTeX support (required for PDF generation), you'll need TinyTeX:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

Alternatively, install a full LaTeX distribution like TeX Live or MiKTeX.

## Usage

1. **Search for a Sample**
   - Enter a barcode (e.g., KPS12345) or lab number in the search fields
   - Click the "Search" button

2. **Download PDF**
   - Once search results appear, an "Export Report" card will be displayed
   - Click the "Download PDF" button
   - Wait for the PDF generation progress (this may take 10-30 seconds)
   - The PDF will be automatically downloaded to your browser's download folder

## File Naming

PDF files are automatically named with the following pattern:
```
sample_journey_{SAMPLE_ID}_{TIMESTAMP}.pdf
```

Example: `sample_journey_KPS12345_20250122_143022.pdf`

## Troubleshooting

### PDF Generation Fails

**Error: "LaTeX failed to compile"**
This is the most common error and usually means LaTeX packages are missing.

**Quick Fix:**
```r
source("R/fix_latex_packages.R")
```

This script will automatically install all required LaTeX packages.

**Manual Fix:**
1. Ensure TinyTeX is installed:
   ```r
   tinytex::install_tinytex()
   ```

2. Install required LaTeX packages:
   ```r
   tinytex::tlmgr_install(c(
     "booktabs", "longtable", "array", "multirow", "wrapfig",
     "float", "colortbl", "pdflscape", "tabu", "threeparttable",
     "threeparttablex", "ulem", "makecell", "xcolor", "fancyhdr"
   ))
   ```

**Error: "LaTeX not found"**
- Solution: Install TinyTeX: `tinytex::install_tinytex()`

**Error: "Package not found"**
- Solution: Install the missing R package using `install.packages()`

**Error: "Template file not found"**
- Solution: Ensure `R/sample_journey_report_template.Rmd` exists in your project

**Error: "kableExtra not found"**
- Solution: Install kableExtra package: `install.packages("kableExtra")`

### Plots Not Showing in PDF

If the timeline or DRS gauge plots are not appearing:
- This is usually not critical - the tabular data will still be included
- Check that `ggplot2` is properly installed
- For plotly exports, consider installing `webshot2`:
  ```r
  install.packages("webshot2")
  ```

## Technical Details

### Files Involved

1. **R/modules/mod_10_sample_journey.R** - Main module with UI and server logic
2. **R/sampleJourneyPdfExport.R** - PDF generation functions
3. **reports/sample_report.qmd** - Quarto template for the modern dashboard PDF
4. **reports/sample_report.sty** - Shared LaTeX styling for the PDF
5. **R/sampleJourneyVisualizations.R** - Visualization functions (plots)

### PDF Generation Process

1. User clicks "Download PDF" button
2. System creates a temporary directory for intermediate files
3. Quarto renders `reports/sample_report.qmd` with:
   - Sample ID and journey data
   - Inline ggplot2 timeline
   - Logo paths from `/logo`
4. LaTeX generates final PDF from the Quarto output
5. PDF is downloaded to user's browser

### Customization

To customize the PDF appearance, edit:
- **Template**: `reports/sample_report.qmd`
- **Styling**: `reports/sample_report.sty`
- **Colors**: Theme values in the Quarto template

## Future Enhancements

Potential improvements:
- [ ] Add logo/branding to PDF header
- [ ] Include QR codes for sample tracking
- [ ] Add statistical summaries and trends
- [ ] Support batch export for multiple samples
- [ ] Include signature fields for validation
- [ ] Add watermarks for confidential data
