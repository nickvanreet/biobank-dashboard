# 📦 Mbuji-Mayi Biobank Dashboard v3.0 - Complete Package

## What's Inside

This package contains a **fully functional, modular Shiny dashboard** for managing and analyzing biobank data.

## 📖 Documentation Files (Start Here!)

1. **[QUICK_START.md](QUICK_START.md)** ⚡  
   → 5-step guide to get running in minutes

2. **[SUMMARY.md](SUMMARY.md)** 📊  
   → What was built and why

3. **[DEPLOYMENT_GUIDE.md](DEPLOYMENT_GUIDE.md)** 🚀  
   → Complete deployment instructions with troubleshooting

4. **[README.md](README.md)** 📚  
   → Full technical documentation

## 🗂️ Application Files

### Core Application
- `app.R` - Main application entry point
- `global.R` - Global configuration and package loading
- `config.yml` - Configuration file (edit your paths here!)
- `test_structure.R` - Verification script

### R Code Directory Structure
```
R/
├── core/
│   └── data_loader_utils.R      # File loading, quality analysis, filtering
├── data/
│   └── data_cleaner_improved.R  # Comprehensive data cleaning pipeline
├── ui/
│   └── ui_utils.R               # UI helper functions
└── modules/
    ├── mod_data_manager.R       # Data loading and management
    └── mod_01_data_quality.R    # Data quality analysis module
```

### Data Directories
```
data/
├── biobank/      ← PUT YOUR EXCEL FILES HERE
├── extractions/
├── pcr/
├── elisa_pe/
├── elisa_vsg/
└── ielisa/
```

## 🎯 What It Does

### 1. Data Loading
- Scans your biobank directory for Excel files
- Smart column mapping (handles French headers)
- Automatic data type detection

### 2. Data Cleaning
- Validates required fields (lab_id, barcode, sample date)
- Parses dates (multiple formats)
- Standardizes categorical values
- Removes duplicates
- Calculates transport durations
- Assigns quality flags

### 3. Data Quality Analysis
- Missing data detection
- Duplicate identification
- Barcode conflict analysis
- Column completeness metrics
- Quality flag distribution
- Entry timeline visualization

### 4. Filtering & Export
- Date range filtering
- Study type filtering
- Province/zone filtering
- Real-time status updates

## 🚀 Quick Start

### Absolute Minimum Steps:

1. **Extract this folder to your computer**
   ```
   Recommended: C:/Users/nvanreet/biobank-dashboard/
   ```

2. **Install R packages** (one time, ~10 minutes)
   ```r
   install.packages(c("shiny", "bslib", "bsicons", "tidyverse", 
                      "data.table", "janitor", "readxl", "jsonlite", 
                      "yaml", "lubridate", "plotly", "DT", "stringi", "scales"))
   ```

3. **Edit config.yml** - Update the biobank_dir path:
   ```yaml
   paths:
     biobank_dir: "YOUR/ACTUAL/PATH/TO/01 - Biobanque"
   ```

4. **Run the app**
   ```r
   setwd("C:/Users/nvanreet/biobank-dashboard")
   shiny::runApp()
   ```

See **QUICK_START.md** for detailed 5-step instructions.

## 📊 Architecture Highlights

### Modular Design
- Each module is self-contained
- Easy to add new features
- Clear data flow: Load → Clean → Filter → Display

### Configuration-Driven
- All paths in config.yml
- All thresholds configurable
- No hardcoded values

### Production-Ready
- Comprehensive error handling
- Data validation
- Quality checks
- Debug mode support

## 🔧 Customization

### Change Data Paths
Edit `config.yml`:
```yaml
paths:
  biobank_dir: "C:/Your/Path/Here"
```

### Adjust Quality Thresholds
Edit `config.yml`:
```yaml
qc:
  max_transport_days: 30
  max_age: 120
```

### Change Theme Colors
Edit `config.yml`:
```yaml
ui:
  theme_primary: "#2C3E50"
  theme_success: "#27AE60"
```

## 🧩 Adding New Modules

The architecture makes it easy to add new analysis modules:

1. Create `R/modules/mod_XX_newmodule.R`
2. Add to `app.R` UI section
3. Add to `app.R` server section

See README.md for the template.

## ✅ Verification

Run `test_structure.R` to verify everything is set up correctly:

```r
source("test_structure.R")
```

This checks:
- ✓ All files present
- ✓ All directories exist
- ✓ All packages installed
- ✓ All functions load correctly
- ✓ Data files accessible

## 🆘 Getting Help

### If something doesn't work:

1. **Check QUICK_START.md** - Most common issues covered
2. **Run test_structure.R** - Tells you what's missing
3. **See DEPLOYMENT_GUIDE.md** - Comprehensive troubleshooting
4. **Check R console** - Shows detailed error messages
5. **Enable debug mode** - Set `debug_mode: true` in config.yml

## 📂 File Listing

```
./app.R                               # Main application
./global.R                            # Configuration loader
./config.yml                          # Settings file
./test_structure.R                    # Verification script

./QUICK_START.md                      # 5-minute setup guide
./SUMMARY.md                          # What was built
./DEPLOYMENT_GUIDE.md                 # Full deployment instructions
./README.md                           # Technical documentation

./R/core/data_loader_utils.R          # Loading & QA functions
./R/data/data_cleaner_improved.R      # Cleaning pipeline
./R/ui/ui_utils.R                     # UI helpers
./R/modules/mod_data_manager.R        # Data management module
./R/modules/mod_01_data_quality.R     # Quality analysis module

./data/biobank/                       # ← Put Excel files here
./data/extractions/
./data/pcr/
./data/elisa_pe/
./data/elisa_vsg/
./data/ielisa/
```

## 🎊 What Makes This Special

✨ **Everything Connected**: No more scattered functions - complete pipeline  
✨ **Smart Column Mapping**: Handles French headers automatically  
✨ **Comprehensive Cleaning**: Date parsing, validation, derived calculations  
✨ **Quality Analysis**: Duplicates, conflicts, completeness metrics  
✨ **Modular Design**: Easy to extend with new features  
✨ **Configuration-Driven**: Change settings without touching code  
✨ **Well-Documented**: 4 documentation files + code comments  
✨ **Production-Ready**: Error handling, validation, testing  

## 🚀 Next Steps

1. **Get it running** - Follow QUICK_START.md
2. **Load your data** - Point to your biobank directory
3. **Explore quality** - Review the quality analysis
4. **Customize** - Edit config.yml for your needs
5. **Extend** - Add new modules as needed

---

**Version**: 3.0.0  
**Created**: November 2025  
**Institution**: Institute of Tropical Medicine, Antwerp  
**Status**: ✅ Production Ready  

**Questions?** See the documentation files above.  
**Issues?** Run `test_structure.R` first.  
**Ready?** Follow QUICK_START.md!  

---

*Built for the Mbuji-Mayi HAT Screening Program* 🔬
