# ⚡ QUICK START

## 1. Download to Windows
Save everything from `/home/claude/` to:
```
C:/Users/nvanreet/biobank-dashboard/
```

## 2. Install Packages (R Console)
```r
install.packages(c("shiny", "bslib", "bsicons", "tidyverse", 
                   "data.table", "janitor", "readxl", "jsonlite", 
                   "yaml", "lubridate", "plotly", "DT", "stringi", "scales"))
```

## 3. Update config.yml
Change `biobank_dir` to your actual SharePoint path:
```yaml
paths:
  biobank_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System..."
```

## 4. Test
```r
setwd("C:/Users/nvanreet/biobank-dashboard")
source("test_structure.R")
```

## 5. Run
```r
shiny::runApp()
```

## Done! 🎉

The app will:
1. Find all Excel files in your biobank directory
2. Let you select and load one
3. Clean and validate the data automatically
4. Show data quality metrics

---

**Got Issues?** See DEPLOYMENT_GUIDE.md for detailed troubleshooting.

**Want Details?** See README.md for complete documentation.

**Understanding the Structure?** See SUMMARY.md for what was built.
