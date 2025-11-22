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

**Error: "LaTeX not found"**
- Solution: Install TinyTeX as described above

**Error: "Package not found"**
- Solution: Install the missing package using `install.packages()`

**Error: "Template file not found"**
- Solution: Ensure `R/sample_journey_report_template.Rmd` exists in your project

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
3. **R/sample_journey_report_template.Rmd** - R Markdown template for the report
4. **R/sampleJourneyVisualizations.R** - Visualization functions (plots)

### PDF Generation Process

1. User clicks "Download PDF" button
2. System creates temporary directory for intermediate files
3. Timeline plot is saved as static PNG image using ggplot2
4. DRS gauge (if available) is saved as static PNG image
5. R Markdown template is rendered with:
   - Sample ID
   - Journey data (all test results)
   - Paths to plot images
6. LaTeX generates final PDF from Markdown
7. PDF is downloaded to user's browser

### Customization

To customize the PDF appearance, edit:
- **Template**: `R/sample_journey_report_template.Rmd`
- **Styling**: Modify the YAML header in the template
- **Colors**: Adjust the plot generation code in `R/sampleJourneyPdfExport.R`

## Future Enhancements

Potential improvements:
- [ ] Add logo/branding to PDF header
- [ ] Include QR codes for sample tracking
- [ ] Add statistical summaries and trends
- [ ] Support batch export for multiple samples
- [ ] Include signature fields for validation
- [ ] Add watermarks for confidential data
