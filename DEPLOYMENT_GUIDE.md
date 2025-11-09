# DEPLOYMENT_GUIDE.md

# Deploying the Biobank Dashboard to Your System

This guide explains how to move the application from the development environment to your Windows system with your actual data.

## 📋 Prerequisites

1. R (version 4.0 or higher)
2. RStudio (recommended)
3. Your biobank data files in SharePoint/OneDrive

## 🔧 Step 1: Copy Files to Your System

### Option A: Download from this session

1. Download the entire `/home/claude` directory
2. Extract to a location like: `C:/Users/nvanreet/biobank-dashboard/`

### Option B: Copy files manually

Create this folder structure on your computer:
```
C:/Users/nvanreet/biobank-dashboard/
├── app.R
├── global.R
├── config.yml
├── test_structure.R
├── README.md
├── data/
│   └── (directories will be created automatically)
└── R/
    ├── core/
    ├── data/
    ├── ui/
    └── modules/
```

## 📝 Step 2: Update config.yml

Edit `config.yml` with your actual SharePoint paths:

```yaml
paths:
  # Update these paths to match your OneDrive/SharePoint sync location
  biobank_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/01 - Biobanque"
  extractions_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/02 - Extractions"
  pcr_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/03 - Biologie Moléculaire/0302 - Résultats qPCR"
  elisa_pe_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/04 - ELISA indirect PE/0402 - Résultats ELISA indirect PE"
  elisa_vsg_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/05 - ELISA indirect VSG/0502 - Résultats ELISA indirect VSG"
  ielisa_dir: "C:/Users/nvanreet/ITG/THA - Digital Management System - CRT Dipumba Upload - CRT Dipumba Upload/06 - iELISA/0602 - Résultats iELISA"
```

## 📦 Step 3: Install Required Packages

Open R or RStudio and run:

```r
# Install all required packages
install.packages(c(
  "shiny", "bslib", "bsicons",
  "tidyverse", "data.table", "janitor",
  "readxl", "jsonlite", "yaml",
  "lubridate", "plotly", "DT",
  "stringi", "scales"
))
```

This may take 10-15 minutes depending on your internet connection.

## ✅ Step 4: Verify Installation

Run the verification script:

```r
setwd("C:/Users/nvanreet/biobank-dashboard")
source("test_structure.R")
```

This will check:
- ✓ All required files are present
- ✓ All directories exist
- ✓ All packages are installed
- ✓ All functions load correctly
- ✓ Data files are accessible

## 🚀 Step 5: Launch the Application

### From RStudio:
1. Open `app.R` in RStudio
2. Click "Run App" button in the top-right
3. Or press Ctrl+Shift+Enter

### From R Console:
```r
setwd("C:/Users/nvanreet/biobank-dashboard")
shiny::runApp()
```

### From Command Line:
```cmd
cd C:\Users\nvanreet\biobank-dashboard
R -e "shiny::runApp()"
```

## 🎯 Step 6: Using the Dashboard

### Loading Data

1. **Select Data Directory**: The sidebar shows your configured biobank directory
2. **Choose File**: Select an Excel file from the dropdown
3. **Click "Load Data"**: The app will:
   - Load the raw data
   - Analyze quality before cleaning
   - Apply data cleaning and validation
   - Show you the results

### Applying Filters

Use the sidebar filters to narrow down your data:
- **Sample Date Range**: Filter by collection date
- **Study**: Filter by DA (Active) or DP (Passive)
- **Province**: Filter by province
- **Health Zone**: Filter by zone

The filters update automatically after loading data.

### Viewing Quality Metrics

The Data Quality tab shows:
- **Total rows** vs **valid rows** after cleaning
- **Missing identifiers** (barcode/lab ID)
- **Duplicate records** and barcode conflicts
- **Column completeness** percentages
- **Quality flags** distribution
- **Data entry timeline** visualization

## 🔍 Troubleshooting

### Problem: "Directory not found" error

**Solution**: 
1. Check that OneDrive/SharePoint is syncing
2. Verify paths in `config.yml` exactly match your folder structure
3. Use forward slashes `/` or double backslashes `\\\\` in paths

### Problem: "No Excel files found"

**Solution**:
1. Ensure `.xlsx` or `.xls` files exist in the biobank directory
2. Check that temporary files (starting with `~$`) are not the only files
3. Verify you have read permissions for the folder

### Problem: "Package 'xxx' not found"

**Solution**:
```r
install.packages("xxx")
```

### Problem: "Column not found" warnings during cleaning

**Solution**: This is normal if your Excel file has different column names. The cleaner will:
- Try multiple pattern matches for each expected column
- Create missing required columns as NA
- Continue processing with available data

### Problem: Application crashes or freezes

**Solution**:
1. Check R console for error messages
2. Enable debug mode in `config.yml`:
   ```yaml
   app:
     debug_mode: true
   ```
3. Restart R/RStudio and try again
4. Check that Excel files are not corrupted

## 🔧 Advanced Configuration

### Customizing Data Quality Thresholds

Edit `config.yml`:

```yaml
qc:
  max_transport_days: 30        # Max days for field to CPLTHA
  max_conservation_days: 365    # Max total conservation time
  min_age: 0                    # Minimum valid age
  max_age: 120                  # Maximum valid age
  drs_target_ml: 2.0           # Target DRS volume
  drs_accept_min_ml: 1.5       # Min acceptable DRS volume
  drs_accept_max_ml: 2.5       # Max acceptable DRS volume
```

### Customizing UI Colors

Edit `config.yml`:

```yaml
ui:
  theme_primary: "#2C3E50"      # Main color
  theme_success: "#27AE60"      # Success/good indicators
  theme_info: "#3498DB"         # Info indicators
  theme_warning: "#F39C12"      # Warning indicators
  theme_danger: "#E74C3C"       # Error/danger indicators
```

### Changing Default Date Range

```yaml
ui:
  default_date_range_days: 180  # Show last 180 days by default
```

## 📊 Next Steps

Once the basic dashboard is working with your data:

1. **Add More Modules**: Other analysis modules can be enabled as they're developed
2. **Customize Filters**: Add project-specific filters in `mod_data_manager.R`
3. **Export Features**: Enable data export functionality
4. **Integration**: Connect lab results from other directories

## 🆘 Getting Help

If you encounter issues:

1. **Check the logs**: R console shows detailed error messages
2. **Verify structure**: Run `test_structure.R` again
3. **Check data format**: Ensure Excel files match expected structure
4. **Review README.md**: Contains detailed format specifications

## 📝 Notes for Development

### Adding a New Module

1. Create `R/modules/mod_XX_yourmodule.R`
2. Follow the template:
   ```r
   mod_yourmodule_ui <- function(id) { ... }
   mod_yourmodule_server <- function(id, data) { ... }
   ```
3. Add to `app.R`:
   ```r
   # In UI
   mod_yourmodule_ui("yourmodule")
   
   # In Server
   mod_yourmodule_server("yourmodule", data = data$filtered_data)
   ```

### Modifying the Cleaner

Edit `R/data/data_cleaner_improved.R`:
- Column mappings are in `get_column_config()`
- Parsing logic is in `parse_by_type()`
- Derived calculations are in `clean_biobank_data_improved()`

## ✅ Final Checklist

Before going live:

- [ ] All packages installed
- [ ] Paths in `config.yml` updated
- [ ] `test_structure.R` passes all checks
- [ ] Can load at least one Excel file
- [ ] Data Quality tab shows meaningful results
- [ ] Filters work correctly
- [ ] No error messages in R console

---

**Version**: 3.0.0  
**Last Updated**: November 2025  
**Institution**: Institute of Tropical Medicine, Antwerp
