#!/usr/bin/env python3

import pandas as pd
import glob
import os
from pathlib import Path

# Get all extraction files
extraction_dir = "data/extractions"
files = sorted(glob.glob(os.path.join(extraction_dir, "*.xlsx")))

print(f"Found {len(files)} extraction files\n")

file_analysis = []

# Analyze each file
for file_path in files:
    try:
        # Read the Excel file
        df = pd.read_excel(file_path)

        total_rows = len(df)

        # Count non-empty rows (rows with at least one non-null value)
        non_empty_rows = df.dropna(how='all').shape[0]

        # Look for barcode columns
        barcode_cols = [col for col in df.columns if 'barcode' in str(col).lower() or 'code' in str(col).lower()]
        has_barcodes = len(barcode_cols) > 0

        barcode_count = 0
        if has_barcodes and len(barcode_cols) > 0:
            barcode_col = barcode_cols[0]
            barcode_count = df[barcode_col].notna().sum()

        file_analysis.append({
            'file': os.path.basename(file_path),
            'total_rows': total_rows,
            'non_empty_rows': non_empty_rows,
            'has_barcodes': has_barcodes,
            'barcode_count': barcode_count
        })
    except Exception as e:
        file_analysis.append({
            'file': os.path.basename(file_path),
            'total_rows': 0,
            'non_empty_rows': 0,
            'has_barcodes': False,
            'barcode_count': 0,
            'error': str(e)
        })

# Create DataFrame for analysis
analysis_df = pd.DataFrame(file_analysis)

# Print file-by-file analysis
print("File-by-file analysis:")
print("=" * 80)
pd.set_option('display.max_rows', None)
pd.set_option('display.width', 200)
print(analysis_df.sort_values('total_rows', ascending=False).to_string(index=False))

# Print summary statistics
print("\n\nSummary Statistics:")
print("=" * 80)
print(f"Total files: {len(analysis_df)}")
print(f"Files with barcodes: {analysis_df['has_barcodes'].sum()}")
print(f"Total rows across all files: {analysis_df['total_rows'].sum()}")
print(f"Total non-empty rows: {analysis_df['non_empty_rows'].sum()}")
print(f"Total barcodes found: {analysis_df['barcode_count'].sum()}")
print(f"Average rows per file: {analysis_df['total_rows'].mean():.1f}")
print(f"Median rows per file: {analysis_df['total_rows'].median():.1f}")
print(f"Min rows: {analysis_df['total_rows'].min()}")
print(f"Max rows: {analysis_df['total_rows'].max()}")

# Show some sample data from the first file with barcodes
print("\n\nSample data from first file:")
print("=" * 80)
if len(files) > 0:
    sample_file = files[0]
    print(f"File: {os.path.basename(sample_file)}")
    df_sample = pd.read_excel(sample_file)
    print(f"\nColumns: {', '.join(df_sample.columns.tolist())}")
    print(f"\nFirst 5 rows:")
    print(df_sample.head().to_string())
    print(f"\nShape: {df_sample.shape[0]} rows × {df_sample.shape[1]} columns")
