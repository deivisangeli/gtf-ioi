#!/usr/bin/env python3
"""
IOI Data Comparison Script

Compares ioi_stats_all.xlsx (from IOI website) with ioi_codeforces_results.xlsx
and identifies missing participants.

Author: Thais Takeuchi
Date: December 2025
"""
import pandas as pd
from pathlib import Path
import logging
import os

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# ==================== CONFIGURATION ====================
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Globtalent Dropbox" / "Codeforces" / "Data"

IOI_STATS_FILE = BASE_DIR / "ioi_stats_all.xlsx"
CODEFORCES_FILE = BASE_DIR / "ioi_codeforces_results.xlsx"
OUTPUT_FILE = BASE_DIR / "ioi_results_missing.xlsx"
# =======================================================

def normalize_name(name):
    """Normalize name for comparison"""
    if pd.isna(name):
        return ""
    return ' '.join(str(name).lower().strip().split())

def create_key(row, name_col='name', year_col='year'):
    """Create comparison key: name + year"""
    name = normalize_name(row.get(name_col, ''))
    year = row.get(year_col, '')
    return f"{name}|{year}"

def main():
    print("=" * 70)
    print("IOI Data Comparison Tool")
    print("=" * 70)
    print(f"IOI Stats File: {IOI_STATS_FILE}")
    print(f"Codeforces File: {CODEFORCES_FILE}")
    print(f"Output File: {OUTPUT_FILE}")
    print("=" * 70)
    
    # Load IOI stats data
    logger.info(f"Loading IOI stats from {IOI_STATS_FILE}")
    ioi_df = pd.read_excel(IOI_STATS_FILE, sheet_name='All Participants')
    
    # Check columns
    logger.info(f"IOI columns: {list(ioi_df.columns)}")
    
    ioi_df['comparison_key'] = ioi_df.apply(lambda row: create_key(row, 'name', 'year'), axis=1)
    logger.info(f"Loaded {len(ioi_df)} participants from IOI stats")
    
    # Load Codeforces data
    logger.info(f"Loading Codeforces data from {CODEFORCES_FILE}")
    cf_df = pd.read_excel(CODEFORCES_FILE)
    
    # Check columns
    logger.info(f"Codeforces columns: {list(cf_df.columns)}")
    
    # Display first few rows to understand structure
    print("\n" + "="*70)
    print("SAMPLE OF CODEFORCES DATA:")
    print("="*70)
    print(cf_df.head())
    
    # Try to create keys - need to figure out correct column names
    # Common possibilities: 'name' or 'Name', 'year' or 'Year'
    name_col = None
    year_col = None
    
    # Find name column
    for col in cf_df.columns:
        if 'name' in col.lower():
            name_col = col
            break
    
    # Find year column
    for col in cf_df.columns:
        if 'year' in col.lower():
            year_col = col
            break
    
    if not name_col or not year_col:
        logger.error(f"Could not find name and year columns!")
        logger.error(f"Available columns: {list(cf_df.columns)}")
        return
    
    logger.info(f"Using columns: name='{name_col}', year='{year_col}'")
    
    cf_df['comparison_key'] = cf_df.apply(lambda row: create_key(row, name_col, year_col), axis=1)
    cf_keys = set(cf_df['comparison_key'].dropna())
    logger.info(f"Loaded {len(cf_df)} participants from Codeforces ({len(cf_keys)} unique keys)")
    
    # Find participants only in IOI
    missing_mask = ~ioi_df['comparison_key'].isin(cf_keys)
    missing_df = ioi_df[missing_mask].copy()
    
    matched_count = (~missing_mask).sum()
    
    print("\n" + "="*70)
    print("COMPARISON RESULTS:")
    print("="*70)
    print(f"Total in IOI stats: {len(ioi_df)}")
    print(f"Total in Codeforces data: {len(cf_df)}")
    print(f"Matched (in both): {matched_count}")
    print(f"Missing from Codeforces: {len(missing_df)}")
    print("="*70)
    
    if not missing_df.empty:
        # Remove comparison key
        missing_df = missing_df.drop(columns=['comparison_key'])
        
        logger.info(f"Saving {len(missing_df)} missing participants to {OUTPUT_FILE}")
        
        with pd.ExcelWriter(OUTPUT_FILE, engine='openpyxl') as writer:
            missing_df.to_excel(writer, sheet_name='Missing Participants', index=False)
            
            # Summary by year
            summary_data = []
            for year in sorted(missing_df['year'].unique()):
                year_data = missing_df[missing_df['year'] == year]
                
                gold = len(year_data[year_data['medal'].str.contains('Gold', case=False, na=False)])
                silver = len(year_data[year_data['medal'].str.contains('Silver', case=False, na=False)])
                bronze = len(year_data[year_data['medal'].str.contains('Bronze', case=False, na=False)])
                hm = len(year_data[year_data['medal'].str.contains('HM', case=False, na=False)])
                no_award = len(year_data[year_data['medal'].str.contains('No Award', case=False, na=False)])
                with_cf = len(year_data[year_data['codeforces_handle'].notna()])
                
                summary_data.append({
                    'Year': year,
                    'Total': len(year_data),
                    'With CF': with_cf,
                    'Gold': gold,
                    'Silver': silver,
                    'Bronze': bronze,
                    'HM': hm,
                    'No Award': no_award
                })
            
            pd.DataFrame(summary_data).to_excel(writer, sheet_name='Summary by Year', index=False)
            
            # By country - FIX: Filter out NaN before sorting
            country_data = []
            # Remove NaN and empty strings, then sort
            valid_countries = [c for c in missing_df['country'].unique() if pd.notna(c) and str(c).strip()]
            
            for country in sorted(valid_countries):
                country_df = missing_df[missing_df['country'] == country]
                country_data.append({
                    'Country': country,
                    'Count': len(country_df),
                    'With CF': len(country_df[country_df['codeforces_handle'].notna()])
                })
            
            if country_data:
                pd.DataFrame(country_data).sort_values('Count', ascending=False).to_excel(
                    writer, sheet_name='By Country', index=False
                )
        
        print(f"\n✓ Missing participants saved to: {OUTPUT_FILE}")
        
        print("\nSAMPLE OF MISSING PARTICIPANTS:")
        print("="*70)
        display_cols = ['year', 'name', 'country', 'medal', 'codeforces_handle']
        print(missing_df[display_cols].head(15).to_string(index=False))
        if len(missing_df) > 15:
            print(f"\n... and {len(missing_df) - 15} more")
    else:
        print("\n✓ No missing participants - all IOI data is in Codeforces database!")

if __name__ == "__main__":
    main()