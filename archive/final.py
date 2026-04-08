#!/usr/bin/env python3
"""
IOI Complete Data Collector - Sequential Smart Scraping

Sequential workflow:
1. Scrape CPHOF (cphof.org/standings/ioi/)
2. Scrape IOI Stats (stats.ioinformatics.org/contestants/)
3. Compare: Find observations in IOI Stats but NOT in CPHOF
4. Combine: CPHOF + Missing from IOI Stats = ioi_total.xlsx

Smart: Only scrapes years not yet in ioi_total.xlsx

Author: Thais Takeuchi
Date: December 2025
"""
import requests
from bs4 import BeautifulSoup
import time
import pandas as pd
from pathlib import Path
import logging
import os
import re
from typing import List, Dict, Set

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# ==================== CONFIGURATION ====================
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Globtalent Dropbox" / "Codeforces" / "Data"

# Output files
CPHOF_FILE = BASE_DIR / "ioi_cphof.xlsx"
IOI_STATS_FILE = BASE_DIR / "ioi_stats.xlsx"
OUTPUT_FILE = BASE_DIR / "ioi_total.xlsx"

# Scraping parameters
START_YEAR = 2010
END_YEAR = 2025
DELAY_BETWEEN_REQUESTS = 1.5

CPHOF_BASE_URL = "https://cphof.org/standings/ioi/"
IOI_BASE_URL = "https://stats.ioinformatics.org/contestants/"
# =======================================================

def clean_country_name(country):
    """Remove numeric suffixes from country names (e.g., 'Japan 2' -> 'Japan')"""
    if pd.isna(country) or not str(country).strip():
        return country
    cleaned = re.sub(r'\s+\d+$', '', str(country).strip())
    return cleaned

def normalize_name(name):
    """Normalize name for comparison"""
    if pd.isna(name):
        return ""
    return ' '.join(str(name).lower().strip().split())

def create_comparison_key(name, year):
    """Create unique key for deduplication: normalized_name|year"""
    return f"{normalize_name(name)}|{year}"

def create_cf_link(handle):
    """Create Codeforces profile link from handle"""
    if pd.isna(handle) or not str(handle).strip():
        return None
    return f"http://codeforces.com/profile/{str(handle).strip()}"


class CPHOFScraper:
    """Scraper for CPHOF site"""
    
    def __init__(self, delay: float = 1.5):
        self.delay = delay
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        })
    
    def get_page_content(self, url: str):
        """Fetch and parse a web page"""
        try:
            logger.info(f"Fetching: {url}")
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            return BeautifulSoup(response.text, 'html.parser')
        except Exception as e:
            logger.error(f"Error fetching {url}: {e}")
            return None
    
    def scrape_year(self, year: int) -> List[Dict]:
        """Scrape participants from cphof.org for a given year"""
        url = f"{CPHOF_BASE_URL}{year}"
        soup = self.get_page_content(url)
        
        if not soup:
            return []
        
        participants = []
        table = soup.find('table', class_='standings')
        
        if not table:
            logger.warning(f"CPHOF: No standings table found for {year}")
            return []
        
        tbody = table.find('tbody')
        if not tbody:
            return []
        
        rows = tbody.find_all('tr')
        current_country = None
        
        for row in rows:
            cells = row.find_all('td')
            
            if len(cells) < 4:
                continue
            
            # Check if this row has country (rowspan indicates new country)
            country_cell = cells[1]
            if country_cell.get('rowspan'):
                country_link = country_cell.find('a')
                if country_link:
                    current_country = country_link.get_text().strip()
            
            # Extract participant name
            name_cell = cells[2] if country_cell.get('rowspan') else cells[1]
            name_link = name_cell.find('a')
            if not name_link:
                continue
            
            participant_name = name_link.get_text().strip()
            
            # Extract Codeforces handle
            cf_cell = cells[3] if country_cell.get('rowspan') else cells[2]
            cf_link = cf_cell.find('a', href=re.compile(r'codeforces\.com'))
            codeforces_handle = None
            
            if cf_link:
                cf_href = cf_link.get('href', '')
                cf_match = re.search(r'codeforces\.com/profile/([^/]+)', cf_href)
                if cf_match:
                    codeforces_handle = cf_match.group(1)
            
            # Extract medal/result
            result_cell = cells[4] if country_cell.get('rowspan') else cells[3]
            medal = "No Award"
            
            classes = result_cell.get('class', [])
            class_str = ' '.join(classes).lower()
            
            if 'gold' in class_str:
                medal = "Gold"
            elif 'silver' in class_str:
                medal = "Silver"
            elif 'bronze' in class_str:
                medal = "Bronze"
            elif 'hm' in class_str:
                medal = "HM"
            
            # Get full text for placement
            result_text = result_cell.get_text().strip()
            if result_text and medal != "No Award":
                medal = result_text
            
            participants.append({
                'Year': year,
                'Contestant': participant_name,
                'Country': clean_country_name(current_country),
                'Result': medal,
                'CF_Link': create_cf_link(codeforces_handle),
                'Source': 'CPHOF'
            })
        
        logger.info(f"CPHOF {year}: Found {len(participants)} participants")
        return participants
    
    def scrape_years(self, years: List[int]) -> pd.DataFrame:
        """Scrape multiple years"""
        all_participants = []
        
        for year in years:
            logger.info(f"\n{'='*60}")
            logger.info(f"CPHOF: Scraping year {year}")
            logger.info(f"{'='*60}")
            
            participants = self.scrape_year(year)
            all_participants.extend(participants)
            
            if year < max(years):
                time.sleep(self.delay)
        
        df = pd.DataFrame(all_participants)
        logger.info(f"\nCPHOF Total: {len(df)} participants from {len(years)} years")
        
        return df


class IOIStatsScraper:
    """Scraper for IOI Stats site"""
    
    def __init__(self, delay: float = 1.5):
        self.delay = delay
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36'
        })
    
    def get_page_content(self, url: str):
        """Fetch and parse a web page"""
        try:
            logger.info(f"Fetching: {url}")
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            return BeautifulSoup(response.text, 'html.parser')
        except Exception as e:
            logger.error(f"Error fetching {url}: {e}")
            return None
    
    def scrape_year(self, year: int) -> List[Dict]:
        """Scrape participants from stats.ioinformatics.org for a given year"""
        url = f"{IOI_BASE_URL}{year}"
        soup = self.get_page_content(url)
        
        if not soup:
            return []
        
        participants = []
        
        # Find the main table
        main_div = soup.find('div', class_='maincontent')
        if not main_div:
            logger.warning(f"IOI Stats {year}: No maincontent div found")
            return []
        
        table = main_div.find('table')
        if not table:
            logger.warning(f"IOI Stats {year}: No table found")
            return []
        
        # Get rows
        tbody = table.find('tbody')
        rows = tbody.find_all('tr') if tbody else table.find_all('tr')
        
        if len(rows) <= 2:
            logger.warning(f"IOI Stats {year}: No data rows found")
            return []
        
        # Skip first 2 header rows
        data_rows = rows[2:]
        current_country = None
        
        for row in data_rows:
            cells = row.find_all('td')
            
            if len(cells) < 3:
                continue
            
            # Column 0: Participant name
            name_cell = cells[0]
            name_link = name_cell.find('a', href=re.compile(r'people/\d+'))
            if not name_link:
                continue
            
            participant_name = name_link.get_text().strip()
            
            # Column 1: Country (may have rowspan)
            country_cell = cells[1]
            if country_cell.get('rowspan'):
                country_link = country_cell.find('a', href=re.compile(r'countries/'))
                if country_link:
                    current_country = country_link.get_text().strip()
            
            country = current_country
            
            # Determine column indices based on rowspan
            has_country_cell = cells[1].get('rowspan') is not None
            
            if has_country_cell:
                cf_col = 3
                result_col = 4
            else:
                cf_col = 2
                result_col = 3
            
            # Extract Codeforces handle
            codeforces_handle = None
            if len(cells) > cf_col:
                cf_cell = cells[cf_col]
                cf_link = cf_cell.find('a', href=re.compile(r'codeforces\.com/profile/'))
                if cf_link:
                    cf_href = cf_link.get('href', '')
                    cf_match = re.search(r'codeforces\.com/profile/([^/]+)', cf_href)
                    if cf_match:
                        codeforces_handle = cf_match.group(1)
            
            # Extract medal
            medal = "No Award"
            if len(cells) > result_col:
                result_cell = cells[result_col]
                classes = result_cell.get('class', [])
                class_str = ' '.join(classes).lower()
                
                if 'gold' in class_str:
                    medal = "Gold"
                elif 'silver' in class_str:
                    medal = "Silver"
                elif 'bronze' in class_str:
                    medal = "Bronze"
                elif 'hm' in class_str:
                    medal = "HM"
                
                # Get full text for placement
                result_text = result_cell.get_text().strip()
                if result_text and medal != "No Award":
                    medal = result_text
            
            participants.append({
                'Year': year,
                'Contestant': participant_name,
                'Country': clean_country_name(country),
                'Result': medal,
                'CF_Link': create_cf_link(codeforces_handle),
                'Source': 'IOI_Stats'
            })
        
        logger.info(f"IOI Stats {year}: Found {len(participants)} participants")
        return participants
    
    def scrape_years(self, years: List[int]) -> pd.DataFrame:
        """Scrape multiple years"""
        all_participants = []
        
        for year in years:
            logger.info(f"\n{'='*60}")
            logger.info(f"IOI Stats: Scraping year {year}")
            logger.info(f"{'='*60}")
            
            participants = self.scrape_year(year)
            all_participants.extend(participants)
            
            if year < max(years):
                time.sleep(self.delay)
        
        df = pd.DataFrame(all_participants)
        logger.info(f"\nIOI Stats Total: {len(df)} participants from {len(years)} years")
        
        return df


def load_existing_data() -> tuple[pd.DataFrame, Set[int]]:
    """Load existing ioi_total.xlsx if it exists"""
    if OUTPUT_FILE.exists():
        logger.info(f"Loading existing data from {OUTPUT_FILE}")
        df = pd.read_excel(OUTPUT_FILE, sheet_name='All Participants')
        existing_years = set(df['Year'].unique())
        logger.info(f"Existing data: {len(df)} participants from years {sorted(existing_years)}")
        return df, existing_years
    else:
        logger.info("No existing data found - will do full scraping")
        return pd.DataFrame(), set()


def compare_and_find_missing(cphof_df: pd.DataFrame, ioi_stats_df: pd.DataFrame) -> pd.DataFrame:
    """Find participants in IOI Stats but NOT in CPHOF"""
    logger.info("\n" + "="*70)
    logger.info("COMPARING DATASETS")
    logger.info("="*70)
    
    # Create comparison keys
    cphof_df['_key'] = cphof_df.apply(
        lambda row: create_comparison_key(row['Contestant'], row['Year']), axis=1
    )
    ioi_stats_df['_key'] = ioi_stats_df.apply(
        lambda row: create_comparison_key(row['Contestant'], row['Year']), axis=1
    )
    
    cphof_keys = set(cphof_df['_key'])
    
    # Find missing: in IOI Stats but NOT in CPHOF
    missing_mask = ~ioi_stats_df['_key'].isin(cphof_keys)
    missing_df = ioi_stats_df[missing_mask].copy()
    missing_df = missing_df.drop(columns=['_key'])
    
    # Update source to indicate these are missing from CPHOF
    missing_df['Source'] = 'IOI_Stats_Only'
    
    logger.info(f"CPHOF participants: {len(cphof_df)}")
    logger.info(f"IOI Stats participants: {len(ioi_stats_df)}")
    logger.info(f"In both: {len(ioi_stats_df) - len(missing_df)}")
    logger.info(f"Missing from CPHOF: {len(missing_df)}")
    
    # Clean up temp column
    cphof_df = cphof_df.drop(columns=['_key'])
    
    return missing_df


def remove_duplicates(df: pd.DataFrame) -> pd.DataFrame:
    """Remove duplicate participants based on name+year, keeping first occurrence"""
    df['_comparison_key'] = df.apply(
        lambda row: create_comparison_key(row['Contestant'], row['Year']), 
        axis=1
    )
    
    before_count = len(df)
    df = df.drop_duplicates(subset='_comparison_key', keep='first')
    df = df.drop(columns=['_comparison_key'])
    after_count = len(df)
    
    if before_count > after_count:
        logger.info(f"Removed {before_count - after_count} duplicates")
    
    return df


def save_consolidated_data(df: pd.DataFrame):
    """Save consolidated data with multiple sheets"""
    logger.info(f"\nSaving {len(df)} participants to {OUTPUT_FILE}")
    
    with pd.ExcelWriter(OUTPUT_FILE, engine='openpyxl') as writer:
        # Main sheet
        df.to_excel(writer, sheet_name='All Participants', index=False)
        
        # Summary by year
        summary_data = []
        for year in sorted(df['Year'].unique()):
            year_data = df[df['Year'] == year]
            
            summary_data.append({
                'Year': year,
                'Total': len(year_data),
                'From_CPHOF': len(year_data[year_data['Source'] == 'CPHOF']),
                'From_IOI_Stats_Only': len(year_data[year_data['Source'] == 'IOI_Stats_Only']),
                'With_CF_Link': len(year_data[year_data['CF_Link'].notna()]),
                'Without_CF_Link': len(year_data[year_data['CF_Link'].isna()]),
                'Gold': len(year_data[year_data['Result'].str.contains('Gold', case=False, na=False)]),
                'Silver': len(year_data[year_data['Result'].str.contains('Silver', case=False, na=False)]),
                'Bronze': len(year_data[year_data['Result'].str.contains('Bronze', case=False, na=False)]),
                'HM': len(year_data[year_data['Result'].str.contains('HM', case=False, na=False)]),
                'No_Award': len(year_data[year_data['Result'].str.contains('No Award', case=False, na=False)])
            })
        
        pd.DataFrame(summary_data).to_excel(writer, sheet_name='Summary by Year', index=False)
        
        # By country
        country_data = []
        valid_countries = [c for c in df['Country'].unique() if pd.notna(c) and str(c).strip()]
        
        for country in sorted(valid_countries):
            country_df = df[df['Country'] == country]
            country_data.append({
                'Country': country,
                'Total': len(country_df),
                'With_CF_Link': len(country_df[country_df['CF_Link'].notna()]),
                'From_CPHOF': len(country_df[country_df['Source'] == 'CPHOF']),
                'From_IOI_Stats_Only': len(country_df[country_df['Source'] == 'IOI_Stats_Only'])
            })
        
        pd.DataFrame(country_data).sort_values('Total', ascending=False).to_excel(
            writer, sheet_name='By Country', index=False
        )
        
        # With/Without Codeforces
        with_cf = df[df['CF_Link'].notna()].copy()
        if not with_cf.empty:
            with_cf.to_excel(writer, sheet_name='With Codeforces', index=False)
        
        without_cf = df[df['CF_Link'].isna()].copy()
        if not without_cf.empty:
            without_cf.to_excel(writer, sheet_name='Without Codeforces', index=False)
    
    logger.info(f"Successfully saved to {OUTPUT_FILE}")


def main():
    print("=" * 70)
    print("IOI COMPLETE DATA COLLECTOR - Sequential Smart Scraping")
    print("=" * 70)
    print(f"Base Directory: {BASE_DIR}")
    print(f"Output File: {OUTPUT_FILE}")
    print(f"Target Years: {START_YEAR}-{END_YEAR}")
    print("=" * 70)
    
    BASE_DIR.mkdir(parents=True, exist_ok=True)
    
    # Load existing data
    existing_df, existing_years = load_existing_data()
    
    # Determine which years need scraping
    target_years = set(range(START_YEAR, END_YEAR + 1))
    years_to_scrape = sorted(target_years - existing_years)
    
    if not years_to_scrape:
        logger.info("\n✓ All years already in database - no scraping needed!")
        print("\n" + "="*70)
        print("✓ DATA IS UP TO DATE!")
        print("="*70)
        print(f"Total participants: {len(existing_df)}")
        print(f"Years covered: {sorted(existing_df['Year'].unique())}")
        return
    
    logger.info(f"\nYears to scrape: {years_to_scrape}")
    
    # STEP 1: Scrape CPHOF
    print("\n" + "="*70)
    print("STEP 1: SCRAPING CPHOF")
    print("="*70)
    cphof_scraper = CPHOFScraper(delay=DELAY_BETWEEN_REQUESTS)
    cphof_df = cphof_scraper.scrape_years(years_to_scrape)
    
    if cphof_df.empty:
        logger.error("Failed to scrape CPHOF data")
        return
    
    # Save CPHOF data
    cphof_df.to_excel(CPHOF_FILE, index=False)
    logger.info(f"✓ Saved CPHOF data to {CPHOF_FILE}")
    
    time.sleep(DELAY_BETWEEN_REQUESTS)
    
    # STEP 2: Scrape IOI Stats
    print("\n" + "="*70)
    print("STEP 2: SCRAPING IOI STATS")
    print("="*70)
    ioi_scraper = IOIStatsScraper(delay=DELAY_BETWEEN_REQUESTS)
    ioi_stats_df = ioi_scraper.scrape_years(years_to_scrape)
    
    if ioi_stats_df.empty:
        logger.error("Failed to scrape IOI Stats data")
        return
    
    # Save IOI Stats data
    ioi_stats_df.to_excel(IOI_STATS_FILE, index=False)
    logger.info(f"✓ Saved IOI Stats data to {IOI_STATS_FILE}")
    
    # STEP 3: Compare and find missing
    print("\n" + "="*70)
    print("STEP 3: COMPARING AND FINDING MISSING")
    print("="*70)
    missing_df = compare_and_find_missing(cphof_df, ioi_stats_df)
    
    # STEP 4: Combine all data
    print("\n" + "="*70)
    print("STEP 4: COMBINING DATA")
    print("="*70)
    
    # Combine: CPHOF + Missing from IOI Stats
    new_data = pd.concat([cphof_df, missing_df], ignore_index=True)
    logger.info(f"New data total: {len(new_data)} participants")
    
    # Combine with existing data if any
    if not existing_df.empty:
        combined_df = pd.concat([existing_df, new_data], ignore_index=True)
    else:
        combined_df = new_data
    
    # Remove duplicates
    logger.info("\nRemoving duplicates...")
    combined_df = remove_duplicates(combined_df)
    
    # Sort
    combined_df = combined_df.sort_values(['Year', 'Contestant']).reset_index(drop=True)
    
    # Ensure column order
    column_order = ['Year', 'Contestant', 'Country', 'Result', 'CF_Link', 'Source']
    combined_df = combined_df[[col for col in column_order if col in combined_df.columns]]
    
    # Save final data
    save_consolidated_data(combined_df)
    
    # Print final summary
    print("\n" + "="*70)
    print("✓ DATA COLLECTION COMPLETE!")
    print("="*70)
    print(f"Total participants: {len(combined_df)}")
    print(f"  From CPHOF: {len(combined_df[combined_df['Source'] == 'CPHOF'])}")
    print(f"  From IOI Stats Only: {len(combined_df[combined_df['Source'] == 'IOI_Stats_Only'])}")
    print(f"\nCodeforces links:")
    print(f"  With CF link: {len(combined_df[combined_df['CF_Link'].notna()])}")
    print(f"  Without CF link: {len(combined_df[combined_df['CF_Link'].isna()])}")
    print(f"\nYears covered: {sorted(combined_df['Year'].unique())}")
    print(f"\nFiles saved:")
    print(f"  - CPHOF data: {CPHOF_FILE}")
    print(f"  - IOI Stats data: {IOI_STATS_FILE}")
    print(f"  - Final consolidated: {OUTPUT_FILE}")
    
    # Show sample
    print("\n" + "="*70)
    print("SAMPLE OF CONSOLIDATED DATA:")
    print("="*70)
    print(combined_df.head(10).to_string(index=False))


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        logger.info("\nInterrupted by user")
    except Exception as e:
        logger.error(f"Error: {e}")
        import traceback
        traceback.print_exc()