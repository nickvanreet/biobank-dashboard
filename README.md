# Mbuji-Mayi Biobank Dashboard v3.0

A modular Shiny dashboard for managing and analyzing biobank data from the Mbuji-Mayi HAT screening program.

## 📁 Project Structure

```
/home/claude/
├── app.R                          # Main application entry point
├── global.R                       # Global configuration and setup
├── config.yml                     # Application configuration
├── data/                          # Data directories
│   ├── biobank/                   # Biobank Excel files go here
│   ├── extractions/              # Extraction QC data
│   ├── pcr/                       # PCR results
│   ├── elisa_pe/                 # ELISA PE results
│   ├── elisa_vsg/                # ELISA VSG results
│   └── ielisa/                    # iELISA results
└── R/                             # Application code
    ├── core/                      # Core utilities
    │   └── data_loader_utils.R   # Data loading functions
    ├── data/                      # Data processing
    │   └── data_cleaner_improved.R  # Data cleaning pipeline
    ├── ui/                        # UI utilities
    │   └── ui_utils.R            # UI helper functions
    └── modules/                   # Shiny modules
        ├── mod_data_manager.R    # Data loading & filtering
        └── mod_01_data_quality.R # Data quality analysis
```

## 🚀 Quick Start

### 1. Install Required Packages

```r
install.packages(c(
  "shiny", "bslib", "bsicons",
  "tidyverse", "data.table", "janitor",
  "readxl", "jsonlite", "yaml",
  "lubridate", "plotly", "DT",
  "stringi", "scales"
))
```

### 2. Add Your Data

Place your biobank Excel files in the `data/biobank/` directory.

### 3. Run the Application

```r
shiny::runApp("/home/claude")
```

Or from the terminal:
```bash
cd /home/claude
R -e "shiny::runApp()"
```

## 📊 Features

### Currently Implemented

✅ **Data Manager Module**
- Automatic file discovery
- Smart column mapping
- French/English header support
- Comprehensive data cleaning
- Real-time filtering

✅ **Data Quality Module**
- Row/column completeness metrics
- Duplicate detection
- Barcode conflict analysis
- Quality flag distribution
- Data entry timeline

### Planned Modules

🔲 Overview Dashboard
🔲 Transport Analysis
🔲 Demographics
🔲 Geographic Mapping
🔲 Extraction QC
🔲 Lab Results Integration
🔲 Data Export

## 📝 Expected Data Format

The application expects Excel files with French column headers. The cleaner will automatically map:

### Required Columns
- **Numéro** (Lab ID) - Required
- **Code-Barres KPS** (Barcode) - Required
- **Date de prélèvement** (Sample date) - Required
- **Etude** (Study: DA/DP) - Required
- **Province** - Required
- **Zone de santé** (Health zone) - Required

### Optional Columns
- Structure sanitaire (Health facility)
- Date envoi vers CPLTHA
- Date réception CPLTHA
- Date envoi INRB
- Age (année de naissance)
- Sexe (M/F)
- Ancien cas (Oui/Non/Incertain)
- Traité (Oui/Non/Incertain)
- Stockage avant CPLTHA (Ambiante/Frigo/Congelateur)
- Présence DRS (Oui/Non)
- Présence DBS (Oui/Non)
- Nombre DBS (numeric)

## ⚙️ Configuration

Edit `config.yml` to customize:

```yaml
paths:
  biobank_dir: "data/biobank"  # Change to your data path

ui:
  theme_primary: "#2C3E50"
  default_date_range_days: 180

qc:
  max_transport_days: 30
  max_age: 120
```

## 🧩 Module Architecture

Each module follows a consistent pattern:

```r
# UI Function
mod_example_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Module Name",
    # UI elements
  )
}

# Server Function
mod_example_server <- function(id, data, ...) {
  moduleServer(id, function(input, output, session) {
    # Server logic
  })
}
```

## 🔧 Data Flow

```
Excel File → load_biobank_file()
           ↓
Raw Data → analyze_data_quality()
         ↓
Cleaned Data → clean_biobank_data_improved()
             ↓
Filtered Data → apply_filters()
              ↓
Modules (Quality, Overview, etc.)
```

## 🐛 Debugging

Enable debug mode in `config.yml`:
```yaml
app:
  debug_mode: true
```

This enables:
- Full stack traces
- React log
- Detailed error messages

## 📦 Adding New Modules

1. Create module file in `R/modules/mod_XX_name.R`
2. Follow the module template pattern
3. Add UI to `app.R`:
   ```r
   mod_name_ui("name")
   ```
4. Add server to `app.R`:
   ```r
   mod_name_server("name", data = data$filtered_data)
   ```

## 🧪 Testing

To test without real data:
1. The app will create empty data directories
2. Place sample Excel files in `data/biobank/`
3. Load data through the sidebar interface

## 📧 Support

For issues related to:
- **Data mapping**: Check column names in `R/data/data_cleaner_improved.R`
- **File loading**: Check `R/core/data_loader_utils.R`
- **UI issues**: Check `R/ui/ui_utils.R`

## 🏗️ Development Status

**Version 3.0.0** - Modular Architecture Rebuild

✅ Complete:
- Core data loading pipeline
- Data cleaning with smart mapping
- Data quality analysis module
- Modular architecture

🚧 In Progress:
- Additional analysis modules
- Lab results integration
- Export functionality

## 📄 License

Institute of Tropical Medicine, Antwerp
