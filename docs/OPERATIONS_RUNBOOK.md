# Biobank Dashboard Operations Runbook

This document provides standard operating procedures for deploying, maintaining, and troubleshooting the Biobank Dashboard application.

---

## Table of Contents

1. [Deployment](#1-deployment)
2. [Data Management](#2-data-management)
3. [Cache Management](#3-cache-management)
4. [Backup & Recovery](#4-backup--recovery)
5. [Monitoring](#5-monitoring)
6. [Troubleshooting](#6-troubleshooting)
7. [Scheduled Tasks](#7-scheduled-tasks)

---

## 1. Deployment

### 1.1 Prerequisites

- **R version**: 4.1.0 or higher
- **Required system packages**: libcurl, libxml2, libssl (for Linux)
- **TinyTeX**: Required for PDF export functionality

```bash
# Install system dependencies (Ubuntu/Debian)
sudo apt-get install libcurl4-openssl-dev libxml2-dev libssl-dev

# Install TinyTeX for PDF export
Rscript -e "tinytex::install_tinytex()"
```

### 1.2 Initial Setup

```bash
# Clone the repository
git clone <repository-url>
cd biobank-dashboard

# Install R dependencies
Rscript -e "source('global.R')"

# Create required directories
mkdir -p data/lsd/{biobank,extractions,elisa_pe,elisa_vsg,ielisa,mic,cache}
mkdir -p outputs
```

### 1.3 Configuration

Edit `config.yml` to configure site-specific settings:

```yaml
app:
  title: "Biobank Dashboard"
  version: "3.2.0"
  debug_mode: false  # Set to false in production
  default_site: "lsd"

sites:
  lsd:
    name: "Laboratoire de Sante de Dipumba"
    short_name: "LSD"
    location: "Mbuji-Mayi, DRC"
    folder: "lsd"
```

### 1.4 Launch Application

```bash
# Development
Rscript -e "shiny::runApp('app2.R', port = 3838)"

# Production (with Shiny Server)
# Copy application to /srv/shiny-server/biobank-dashboard/
```

### 1.5 Health Check

After deployment, verify:

1. Dashboard loads without errors
2. Data Manager sidebar appears
3. Biobank data loads successfully
4. All test modules (MIC, ELISA, iELISA) display data

---

## 2. Data Management

### 2.1 Data Directory Structure

```
data/
└── lsd/                        # Site folder
    ├── biobank/               # Biobank Excel files
    │   └── *.xlsx             # Sample metadata
    ├── extractions/           # Extraction records
    │   └── *.xlsx
    ├── elisa_pe/              # ELISA-PE plate files
    │   └── *.xlsx
    ├── elisa_vsg/             # ELISA-VSG plate files
    │   └── *.xlsx
    ├── ielisa/                # iELISA plate files
    │   └── *.xlsx
    ├── mic/                   # MIC qPCR run files
    │   └── *.xlsx
    └── cache/                 # Cached parsed data (auto-generated)
        └── *.rds
```

### 2.2 Adding New Data Files

**Biobank data:**
1. Place new Excel file in `data/lsd/biobank/`
2. File must contain required columns: `barcode`, `numero`, `date_prelevement`, `age`, `sexe`, `province`, `zone_sante`
3. Refresh the dashboard - Data Manager will detect new files

**Test results (MIC/ELISA/iELISA):**
1. Place Excel files in appropriate subdirectory
2. Files must follow standard plate format (see `docs/ARCHITECTURE.md`)
3. Clear cache if needed: `clear_cache("mic")` in R console

### 2.3 Data Validation Checklist

Before adding new data:

- [ ] Verify file format matches expected Excel structure
- [ ] Check for required columns (varies by data type)
- [ ] Validate barcode format (KPS-XXXXX or numeric)
- [ ] Confirm date formats are consistent (YYYY-MM-DD or DD/MM/YYYY)
- [ ] Check for duplicate sample IDs within file

### 2.4 Data Update Procedure

```r
# 1. Clear relevant cache
source("R/core/cache_manager.R")
clear_cache("biobank")  # or "elisa", "ielisa", "mic", "all"

# 2. Verify cache is cleared
get_cache_status()

# 3. Restart application or click "Reload Data" in sidebar
```

---

## 3. Cache Management

### 3.1 Cache Overview

The application uses RDS file caching to improve load times:

| Cache Type | Location | Contents |
|------------|----------|----------|
| Biobank | `cache/*.rds` | Parsed biobank data |
| ELISA | `cache/*.rds` | Plate-level ELISA results |
| iELISA | `cache/ielisa_*.rds` | iELISA inhibition results |
| MIC | `cache/Run*.rds` | qPCR run results |

### 3.2 Cache Commands

```r
# Check cache status
source("R/core/cache_manager.R")
get_cache_status()

# Clear specific cache
clear_cache("biobank")
clear_cache("elisa")
clear_cache("ielisa")
clear_cache("mic")

# Clear all caches
clear_all_caches()

# Check if data needs reloading
needs_loading("biobank_data")
```

### 3.3 When to Clear Cache

- After adding new data files
- After modifying source data files
- After updating pipeline code
- When dashboard shows stale data
- After upgrading application version

---

## 4. Backup & Recovery

### 4.1 What to Backup

| Component | Priority | Frequency |
|-----------|----------|-----------|
| `data/` directory | Critical | Daily |
| `config.yml` | High | On change |
| `outputs/` directory | Medium | Weekly |
| Cache files | Low | Not required |

### 4.2 Backup Script

```bash
#!/bin/bash
# backup_biobank.sh

BACKUP_DIR="/backups/biobank-dashboard"
DATE=$(date +%Y%m%d)

# Create backup directory
mkdir -p "$BACKUP_DIR/$DATE"

# Backup data (excluding cache)
rsync -av --exclude='cache/' data/ "$BACKUP_DIR/$DATE/data/"

# Backup configuration
cp config.yml "$BACKUP_DIR/$DATE/"

# Backup outputs
cp -r outputs/ "$BACKUP_DIR/$DATE/outputs/"

# Keep only last 30 days
find "$BACKUP_DIR" -maxdepth 1 -type d -mtime +30 -exec rm -rf {} \;

echo "Backup completed: $BACKUP_DIR/$DATE"
```

### 4.3 Recovery Procedure

```bash
# 1. Stop application
sudo systemctl stop shiny-server

# 2. Restore data
cp -r /backups/biobank-dashboard/YYYYMMDD/data/* data/

# 3. Clear cache
rm -rf data/lsd/cache/*.rds

# 4. Restart application
sudo systemctl start shiny-server
```

---

## 5. Monitoring

### 5.1 Log Locations

| Log | Location | Contents |
|-----|----------|----------|
| Shiny Server | `/var/log/shiny-server/` | Application logs |
| Application | R console / stdout | Debug messages |

### 5.2 Key Metrics to Monitor

- Application startup time
- Data load time per module
- Cache hit rate
- Memory usage during large data loads
- User session count

### 5.3 Health Check Endpoints

```r
# Manual health check via R console
source("global.R")

# Check critical functions
critical_functions <- c(
  "normalize_barcode",
  "analyze_qpcr",
  "extract_cq_values",
  "link_extraction_to_biobank"
)

missing <- critical_functions[!sapply(critical_functions, exists)]
if (length(missing) == 0) {
  cat("All critical functions loaded\n")
} else {
  cat("Missing functions:", paste(missing, collapse = ", "), "\n")
}
```

---

## 6. Troubleshooting

### 6.1 Common Issues

#### "No data loaded" in Data Manager

**Cause**: Data files not found or in wrong format

**Solution**:
```r
# Check data directory
list.files("data/lsd/biobank/", pattern = "\\.xlsx$")

# Verify file is readable
readxl::read_excel("data/lsd/biobank/your_file.xlsx") %>% head()
```

#### Module shows "Error: object not found"

**Cause**: Missing dependency or function

**Solution**:
```r
# Check if function exists
exists("the_function_name")

# Re-source the file
source("R/modules/module_file.R")
```

#### Cache shows stale data

**Cause**: Cache not invalidated after data update

**Solution**:
```r
clear_all_caches()
# Then refresh browser
```

#### PDF export fails

**Cause**: TinyTeX not installed

**Solution**:
```r
tinytex::install_tinytex()
```

### 6.2 Debug Mode

Enable debug mode in `config.yml`:

```yaml
app:
  debug_mode: true
```

This enables verbose logging in the R console.

### 6.3 Error Log Analysis

```bash
# View recent Shiny Server errors
tail -100 /var/log/shiny-server/*.log | grep -i error

# View application startup log
grep -A5 "Starting application" /var/log/shiny-server/*.log
```

---

## 7. Scheduled Tasks

### 7.1 Recommended Cron Jobs

```cron
# Daily backup at 2 AM
0 2 * * * /path/to/backup_biobank.sh

# Weekly cache cleanup (Sunday 3 AM)
0 3 * * 0 rm -rf /path/to/biobank-dashboard/data/lsd/cache/*.rds

# Monthly log rotation
0 4 1 * * find /var/log/shiny-server/ -name "*.log" -mtime +30 -delete
```

### 7.2 Data Refresh Workflow

For automated data ingestion:

```bash
#!/bin/bash
# refresh_data.sh

# 1. Sync new data from remote source
rsync -av remote:/data/biobank/ /path/to/biobank-dashboard/data/lsd/biobank/

# 2. Clear cache
Rscript -e "source('R/core/cache_manager.R'); clear_all_caches()"

# 3. Optional: Pre-warm cache
Rscript -e "source('R/core/cache_manager.R'); preload_cache('lsd')"
```

---

## Appendix A: Contact Information

| Role | Contact |
|------|---------|
| Application Administrator | [admin@institution.org] |
| Data Manager | [data@institution.org] |
| Technical Support | [support@institution.org] |

---

## Appendix B: Version History

| Version | Date | Changes |
|---------|------|---------|
| 3.2.0 | 2024 | Modular pipeline architecture |
| 3.1.0 | 2024 | Added iELISA module |
| 3.0.0 | 2024 | Hub-and-spoke data model |

---

*Last updated: December 2024*
