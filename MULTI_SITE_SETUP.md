# Multi-Site Biobank Dashboard Setup

## Overview

The biobank dashboard now supports multiple sites with isolated data folders and caching. This allows you to manage data from different institutions (LSD, IPP, INRB, ITM) within the same application.

## Site Configuration

### Current Sites

The following sites are configured in `config.yml`:

1. **LSD** (Laboratoire de Santé de Dipumba)
   - Location: Mbuji-Mayi, DRC
   - Institution: Dipumba Hospital
   - Folder: `data/lsd/`

2. **IPP** (Institut Pasteur de Bangui)
   - Location: Bangui, CAR
   - Institution: Institut Pasteur de Bangui
   - Folder: `data/ipp/`

3. **INRB** (Institut National de Recherche Biomédicale)
   - Location: Kinshasa, DRC
   - Institution: INRB
   - Folder: `data/inrb/`

4. **ITM** (Institute of Tropical Medicine)
   - Location: Antwerp, Belgium
   - Institution: Institute of Tropical Medicine
   - Folder: `data/itm/`

## Folder Structure

Each site has its own isolated folder structure:

```
data/
├── lsd/                    # Dipumba, Mbuji-Mayi
│   ├── biobank/           # Biobank Excel files
│   ├── extractions/       # Extraction quality data
│   ├── elisa_pe/          # ELISA PE results
│   ├── elisa_vsg/         # ELISA VSG results
│   ├── ielisa/            # iELISA (inhibition ELISA) results
│   ├── mic/               # MIC qPCR run data
│   └── cache/             # RDS cache files (MIC, ELISA, iELISA)
├── ipp/                    # Bangui
│   └── [same structure]
├── inrb/                   # Kinshasa
│   └── [same structure]
└── itm/                    # Antwerp
    └── [same structure]
```

## How to Switch Sites

### Method 1: Edit config.yml (Recommended)

1. Open `config.yml`
2. Change the `default_site` value:
   ```yaml
   app:
     default_site: "lsd"  # Change to "ipp", "inrb", or "itm"
   ```
3. Restart the application

### Method 2: Environment Variable (Advanced)

Set the `BIOBANK_SITE` environment variable before launching the app:

```bash
export BIOBANK_SITE=ipp
Rscript -e "shiny::runApp('app.R')"
```

## Data Loading

All data loading functions now automatically use site-specific paths:

- **Biobank data**: Loaded from `data/{site}/biobank/`
- **Extractions**: Loaded from `data/{site}/extractions/`
- **MIC qPCR**: Loaded from `data/{site}/mic/`
- **ELISA PE/VSG**: Loaded from `data/{site}/elisa_pe/` and `data/{site}/elisa_vsg/`
- **iELISA**: Loaded from `data/{site}/ielisa/`

### Caching

Each site has its own cache directory (`data/{site}/cache/`) to prevent conflicts between sites. This means:

- Faster data loading after first parse
- Site-specific cache invalidation
- No cache conflicts when switching between sites

## UI Changes

### Header

The application header now displays:
- **Site short name** (e.g., "LSD", "IPP")
- **Site location** (e.g., "Mbuji-Mayi, DRC")

### Sidebar

The data manager sidebar shows:
- Current site information (name and location)
- Site-specific data directory paths

## Adding a New Site

To add a new site:

1. **Edit config.yml**:
   ```yaml
   sites:
     new_site:
       name: "Full Institution Name"
       short_name: "ABBR"
       location: "City, Country"
       institution: "Institution Name"
       folder: "new_site"
   ```

2. **Create folder structure**:
   The folders will be created automatically on app startup, or you can create them manually:
   ```bash
   mkdir -p data/new_site/{biobank,extractions,elisa_pe,elisa_vsg,ielisa,mic,cache}
   ```

3. **Set as default** (optional):
   ```yaml
   app:
     default_site: "new_site"
   ```

## Data Migration

If you have existing data in the old flat structure (`data/biobank/`, `data/extractions/`, etc.):

### Option 1: Move to Site Folder

```bash
# Move existing data to LSD site
mv data/biobank/* data/lsd/biobank/
mv data/extractions/* data/lsd/extractions/
mv data/elisa_pe/* data/lsd/elisa_pe/
mv data/elisa_vsg/* data/lsd/elisa_vsg/
mv data/ielisa/* data/lsd/ielisa/
mv data/MIC/* data/lsd/mic/
```

### Option 2: Keep Legacy Paths

The application maintains backward compatibility. If `config$site_paths` is not available, it falls back to legacy paths (`data/biobank/`, etc.).

## Performance Improvements

The multi-site refactoring includes:

1. **iELISA Caching**: Now uses RDS caching like MIC and ELISA modules
2. **Site-specific Caching**: Each site has isolated cache to prevent conflicts
3. **Unified Cache Management**: All modules use consistent caching strategies

## Troubleshooting

### Issue: Data not loading after switching sites

**Solution**: Check that data files exist in the correct site folder:
```bash
ls -la data/lsd/biobank/     # Should show Excel files
ls -la data/lsd/extractions/ # Should show Excel files
```

### Issue: Cache seems stale

**Solution**: Clear the site-specific cache:
```bash
rm -rf data/lsd/cache/*
```

### Issue: Site not showing in UI

**Solution**: Verify `config.yml` syntax and that the site is properly defined under the `sites` section.

## Technical Notes

### Site Path Resolution

The `get_site_paths()` function in `global.R` generates site-specific paths:

```r
get_site_paths("lsd")
# Returns:
# list(
#   biobank_dir = "data/lsd/biobank",
#   extractions_dir = "data/lsd/extractions",
#   ...
# )
```

### Backward Compatibility

All data loading functions check for site paths first, then fall back to legacy paths:

```r
load_elisa_data(dirs = NULL)  # Uses site paths if available
load_ielisa_data()            # Uses site paths if available
load_all_extractions()        # Uses site paths if available
```

## Future Enhancements

Potential improvements for multi-site support:

1. **Runtime Site Switching**: Add a dropdown in the UI to switch sites without restarting
2. **Multi-Site Dashboard**: View data from all sites simultaneously
3. **Site Comparison**: Compare metrics across different sites
4. **User Permissions**: Restrict access to specific sites based on user roles

## Support

For questions or issues, please contact the development team or create an issue in the GitHub repository.
