#!/usr/bin/env python3
"""
IOI Data Consolidation Script

Combines ioi_codeforces_results.xlsx and ioi_results_missing.xlsx
into a single consolidated file: ioi_total.xlsx

Author: Thais Takeuchi
Date: December 2025
"""
import pandas as pd
from pathlib import Path
import logging
import os
import re

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# ==================== CONFIGURATION ====================
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Globtalent Dropbox" / "Codeforces" / "Data"

CODEFORCES_FILE = BASE_DIR / "ioi_codeforces_results.xlsx"
MISSING_FILE = BASE_DIR / "ioi_results_missing.xlsx"
OUTPUT_FILE = BASE_DIR / "ioi_total.xlsx"
# =======================================================

def clean_country_name(country):
    """
    Clean country names by removing numeric suffixes like ' 2', ' 3', etc.
    Examples:
        'Japan 2' -> 'Japan'
        'Indonesia 2' -> 'Indonesia'
        'Bolivia 2' -> 'Bolivia'
    """
    if pd.isna(country) or not str(country).strip():
        return country
    
    # Remove numeric suffixes at the end (e.g., " 2", " 3")
    cleaned = re.sub(r'\s+\d+$', '', str(country).strip())
    return cleaned

def create_cf_link(handle):
    """Create Codeforces profile link from handle"""
    if pd.isna(handle) or not str(handle).strip():
        return None
    return f"http://codeforces.com/profile/{str(handle).strip()}"

def main():
    print("=" * 70)
    print("IOI Data Consolidation Tool")
    print("=" * 70)
    print(f"Codeforces File: {CODEFORCES_FILE}")
    print(f"Missing File: {MISSING_FILE}")
    print(f"Output File: {OUTPUT_FILE}")
    print("=" * 70)
    
    # Load Codeforces data
    logger.info(f"Loading Codeforces data from {CODEFORCES_FILE}")
    cf_df = pd.read_excel(CODEFORCES_FILE)
    logger.info(f"Codeforces columns: {list(cf_df.columns)}")
    logger.info(f"Loaded {len(cf_df)} participants from Codeforces")
    
    # Load missing data
    logger.info(f"Loading missing data from {MISSING_FILE}")
    missing_df = pd.read_excel(MISSING_FILE, sheet_name='Missing Participants')
    logger.info(f"Missing columns: {list(missing_df.columns)}")
    logger.info(f"Loaded {len(missing_df)} missing participants")
    
    # ========== CLEAN COUNTRY NAMES IN MISSING DATA ==========
    logger.info("Cleaning country names in missing data...")
    
    # Show countries before cleaning
    countries_before = missing_df['country'].unique()
    countries_with_numbers = [c for c in countries_before if pd.notna(c) and re.search(r'\s+\d+$', str(c))]
    
    if countries_with_numbers:
        logger.info(f"Found {len(countries_with_numbers)} countries with numeric suffixes:")
        for country in countries_with_numbers:
            logger.info(f"  '{country}' -> '{clean_country_name(country)}'")
    
    # Apply cleaning
    missing_df['country'] = missing_df['country'].apply(clean_country_name)
    
    logger.info("Country names cleaned!")
    # ==========================================================
    
    # Standardize Codeforces data columns
    cf_standardized = pd.DataFrame()
    
    # Map columns from Codeforces file
    for col in cf_df.columns:
        col_lower = col.lower()
        if 'year' in col_lower:
            cf_standardized['Year'] = cf_df[col]
        elif 'name' in col_lower and 'contestant' not in col_lower:
            cf_standardized['Contestant'] = cf_df[col]
        elif 'country' in col_lower:
            cf_standardized['Country'] = cf_df[col]
        elif 'medal' in col_lower or 'result' in col_lower:
            cf_standardized['Result'] = cf_df[col]
        elif 'codeforces' in col_lower and 'handle' in col_lower:
            # If we have codeforces_handle, create the link
            cf_standardized['CF_Link'] = cf_df[col].apply(create_cf_link)
        elif 'cf_link' in col_lower or ('codeforces' in col_lower and 'link' in col_lower):
            cf_standardized['CF_Link'] = cf_df[col]
    
    # Add source column
    cf_standardized['Source'] = 'Codeforces_Original'
    
    logger.info(f"Standardized Codeforces data: {len(cf_standardized)} rows")
    
    # Standardize missing data columns
    missing_standardized = pd.DataFrame()
    missing_standardized['Year'] = missing_df['year']
    missing_standardized['Contestant'] = missing_df['name']
    missing_standardized['Country'] = missing_df['country']  # Now cleaned!
    missing_standardized['Result'] = missing_df['medal']
    
    # Create CF_Link from handle if exists
    if 'codeforces_handle' in missing_df.columns:
        missing_standardized['CF_Link'] = missing_df['codeforces_handle'].apply(create_cf_link)
    else:
        missing_standardized['CF_Link'] = None
    
    missing_standardized['Source'] = 'IOI_Missing'
    
    logger.info(f"Standardized missing data: {len(missing_standardized)} rows")
    
    # Combine both datasets
    combined_df = pd.concat([cf_standardized, missing_standardized], ignore_index=True)
    
    logger.info(f"Combined total: {len(combined_df)} rows")
    
    # Sort by Year and Contestant
    combined_df = combined_df.sort_values(['Year', 'Contestant']).reset_index(drop=True)
    
    # Reorder columns
    column_order = ['Year', 'Contestant', 'Country', 'Result', 'CF_Link', 'Source']
    combined_df = combined_df[column_order]
    
    # Save to Excel
    logger.info(f"Saving consolidated data to {OUTPUT_FILE}")
    
    with pd.ExcelWriter(OUTPUT_FILE, engine='openpyxl') as writer:
        # Main sheet with all data
        combined_df.to_excel(writer, sheet_name='All Participants', index=False)
        
        # Summary by year
        summary_data = []
        for year in sorted(combined_df['Year'].unique()):
            year_data = combined_df[combined_df['Year'] == year]
            
            # Count medals
            gold = len(year_data[year_data['Result'].str.contains('Gold', case=False, na=False)])
            silver = len(year_data[year_data['Result'].str.contains('Silver', case=False, na=False)])
            bronze = len(year_data[year_data['Result'].str.contains('Bronze', case=False, na=False)])
            hm = len(year_data[year_data['Result'].str.contains('HM', case=False, na=False)])
            no_award = len(year_data[year_data['Result'].str.contains('No Award', case=False, na=False)])
            
            # Count with/without Codeforces
            with_cf = len(year_data[year_data['CF_Link'].notna()])
            without_cf = len(year_data[year_data['CF_Link'].isna()])
            
            # Count by source
            from_cf = len(year_data[year_data['Source'] == 'Codeforces_Original'])
            from_missing = len(year_data[year_data['Source'] == 'IOI_Missing'])
            
            summary_data.append({
                'Year': year,
                'Total': len(year_data),
                'From_Codeforces': from_cf,
                'From_Missing': from_missing,
                'With_CF_Link': with_cf,
                'Without_CF_Link': without_cf,
                'Gold': gold,
                'Silver': silver,
                'Bronze': bronze,
                'HM': hm,
                'No_Award': no_award
            })
        
        summary_df = pd.DataFrame(summary_data)
        summary_df.to_excel(writer, sheet_name='Summary by Year', index=False)
        
        # By country
        country_data = []
        valid_countries = [c for c in combined_df['Country'].unique() if pd.notna(c) and str(c).strip()]
        
        for country in sorted(valid_countries):
            country_df = combined_df[combined_df['Country'] == country]
            country_data.append({
                'Country': country,
                'Total': len(country_df),
                'With_CF_Link': len(country_df[country_df['CF_Link'].notna()]),
                'Without_CF_Link': len(country_df[country_df['CF_Link'].isna()]),
                'From_Codeforces': len(country_df[country_df['Source'] == 'Codeforces_Original']),
                'From_Missing': len(country_df[country_df['Source'] == 'IOI_Missing'])
            })
        
        country_summary = pd.DataFrame(country_data).sort_values('Total', ascending=False)
        country_summary.to_excel(writer, sheet_name='By Country', index=False)
        
        # Only participants with CF links
        with_cf = combined_df[combined_df['CF_Link'].notna()].copy()
        if not with_cf.empty:
            with_cf.to_excel(writer, sheet_name='With Codeforces', index=False)
        
        # Only participants without CF links
        without_cf = combined_df[combined_df['CF_Link'].isna()].copy()
        if not without_cf.empty:
            without_cf.to_excel(writer, sheet_name='Without Codeforces', index=False)
    
    logger.info(f"Successfully saved to {OUTPUT_FILE}")
    
    # Print summary
    print("\n" + "="*70)
    print("CONSOLIDATION COMPLETE!")
    print("="*70)
    print(f"Total participants: {len(combined_df)}")
    print(f"  From Codeforces original: {len(combined_df[combined_df['Source'] == 'Codeforces_Original'])}")
    print(f"  From IOI missing: {len(combined_df[combined_df['Source'] == 'IOI_Missing'])}")
    print(f"\nCodeforces links:")
    print(f"  With CF link: {len(combined_df[combined_df['CF_Link'].notna()])}")
    print(f"  Without CF link: {len(combined_df[combined_df['CF_Link'].isna()])}")
    print(f"\nSaved to: {OUTPUT_FILE}")
    
    # Show sample
    print("\n" + "="*70)
    print("SAMPLE OF CONSOLIDATED DATA:")
    print("="*70)
    print(combined_df.head(10).to_string(index=False))

if __name__ == "__main__":
    main()
