#!/usr/bin/env python3
"""
IOI Stats Scraper - Collect ALL data from stats.ioinformatics.org

This script ONLY scrapes data from the IOI official website.
No comparison with existing data.

Author: Thais Takeuchi
Date: December 2025
"""
import requests
from bs4 import BeautifulSoup
import time
import pandas as pd
from pathlib import Path
import logging
from typing import List, Dict
import os
import re

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

# ==================== CONFIGURATION ====================
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Globtalent Dropbox" / "Codeforces" / "Data"

# Output file - ALL participants from IOI site
OUTPUT_FILE = BASE_DIR / "ioi_stats_all.xlsx"

# Scraping parameters
START_YEAR = 2011
END_YEAR = 2025
DELAY_BETWEEN_REQUESTS = 1.5

BASE_URL = "https://stats.ioinformatics.org/contestants/"
# =======================================================

class IOIStatsScraper:
    """
    Scrapes ALL data from stats.ioinformatics.org
    """
    
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
    
    def extract_medal_from_class(self, cell) -> str:
        """Extract medal type from CSS class"""
        classes = cell.get('class', [])
        class_str = ' '.join(classes).lower()
        
        if 'gold' in class_str:
            return "Gold"
        elif 'silver' in class_str:
            return "Silver"
        elif 'bronze' in class_str:
            return "Bronze"
        elif 'hm' in class_str:
            return "HM"
        elif 'nomedal' in class_str:
            return "No Award"
        
        return "No Award"
    
    def scrape_ioi_year(self, year: int) -> List[Dict]:
        """
        Scrape participants from stats.ioinformatics.org for a given year
        """
        url = f"{BASE_URL}{year}"
        soup = self.get_page_content(url)
        
        if not soup:
            logger.warning(f"Could not fetch data for year {year}")
            return []
        
        participants = []
        
        # Find the main table
        main_div = soup.find('div', class_='maincontent')
        if not main_div:
            logger.warning(f"Could not find maincontent div for year {year}")
            return []
        
        table = main_div.find('table')
        if not table:
            logger.warning(f"Could not find table for year {year}")
            return []
        
        # Try to get rows from tbody first, if not found, get directly from table
        tbody = table.find('tbody')
        if tbody:
            rows = tbody.find_all('tr')
        else:
            rows = table.find_all('tr')
        
        if not rows:
            logger.warning(f"No rows found for year {year}")
            return []
        
        # Skip first 2 header rows
        data_rows = rows[2:] if len(rows) > 2 else []
        logger.info(f"Processing {len(data_rows)} data rows for year {year}")
        
        current_country = None
        
        for row in data_rows:
            cells = row.find_all('td')
            
            if len(cells) < 3:
                continue
            
            participant_name = None
            country = None
            codeforces_handle = None
            medal = "No Award"
            
            # Column 0: Participant name
            name_cell = cells[0]
            name_link = name_cell.find('a', href=re.compile(r'people/\d+'))
            if name_link:
                participant_name = name_link.get_text().strip()
            
            if not participant_name:
                continue
            
            # Column 1: Country (may have rowspan or be empty)
            if len(cells) > 1:
                country_cell = cells[1]
                if country_cell.get('rowspan'):
                    # This is a new country group
                    country_link = country_cell.find('a', href=re.compile(r'countries/'))
                    if country_link:
                        current_country = country_link.get_text().strip()
                    country = current_country
                else:
                    # Country cell is merged from previous row
                    country = current_country
            
            # Determine column indices
            has_country_cell = cells[1].get('rowspan') is not None
            
            if has_country_cell:
                tc_col = 2
                cf_col = 3
                result_col = 4
            else:
                tc_col = 1
                cf_col = 2
                result_col = 3
            
            # Extract Codeforces handle
            if len(cells) > cf_col:
                cf_cell = cells[cf_col]
                cf_link = cf_cell.find('a', href=re.compile(r'codeforces\.com/profile/'))
                if cf_link:
                    cf_href = cf_link.get('href', '')
                    cf_match = re.search(r'codeforces\.com/profile/([^/]+)', cf_href)
                    if cf_match:
                        codeforces_handle = cf_match.group(1)
            
            # Extract medal
            if len(cells) > result_col:
                result_cell = cells[result_col]
                medal = self.extract_medal_from_class(result_cell)
                
                # Get the full text including placement
                result_text = result_cell.get_text().strip()
                if result_text and medal != "No Award":
                    if '(' in result_text and ')' in result_text:
                        medal = result_text
            
            participant_info = {
                'year': year,
                'name': participant_name,
                'country': country if country else '',
                'medal': medal,
                'codeforces_handle': codeforces_handle,
                'source': 'IOI_Stats'
            }
            
            participants.append(participant_info)
        
        logger.info(f"Year {year}: Extracted {len(participants)} participants")
        return participants
    
    def scrape_all_years(self, start_year: int, end_year: int) -> pd.DataFrame:
        """Scrape all years and return DataFrame"""
        
        logger.info(f"Scraping IOI stats site for years {start_year}-{end_year}")
        all_participants = []
        
        for year in range(start_year, end_year + 1):
            try:
                participants = self.scrape_ioi_year(year)
                all_participants.extend(participants)
                
                if year < end_year:
                    time.sleep(self.delay)
            except Exception as e:
                logger.error(f"Error scraping year {year}: {e}")
                import traceback
                traceback.print_exc()
                continue
        
        df = pd.DataFrame(all_participants)
        logger.info(f"Total scraped: {len(df)} participants")
        
        return df


def main():
    """Main execution function"""
    print("=" * 70)
    print("IOI Stats Scraper - Collect ALL data from IOI website")
    print("=" * 70)
    print(f"Base Directory: {BASE_DIR}")
    print(f"Output File: {OUTPUT_FILE}")
    print(f"Years: {START_YEAR}-{END_YEAR}")
    print("=" * 70)
    
    BASE_DIR.mkdir(parents=True, exist_ok=True)
    scraper = IOIStatsScraper(delay=DELAY_BETWEEN_REQUESTS)
    
    try:
        # Scrape all data
        ioi_df = scraper.scrape_all_years(START_YEAR, END_YEAR)
        
        if not ioi_df.empty:
            logger.info(f"Saving {len(ioi_df)} participants to {OUTPUT_FILE}")
            
            # Reorder columns
            column_order = ['year', 'name', 'country', 'medal', 'codeforces_handle', 'source']
            ioi_df = ioi_df[[col for col in column_order if col in ioi_df.columns]]
            
            # Create Excel with multiple sheets
            with pd.ExcelWriter(OUTPUT_FILE, engine='openpyxl') as writer:
                # Main sheet
                ioi_df.to_excel(writer, sheet_name='All Participants', index=False)
                
                # Summary by year
                summary_data = []
                for year in sorted(ioi_df['year'].unique()):
                    year_data = ioi_df[ioi_df['year'] == year]
                    
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
                
                # By country
                country_data = []
                for country in sorted(ioi_df['country'].unique()):
                    if country and country.strip():
                        country_df = ioi_df[ioi_df['country'] == country]
                        country_data.append({
                            'Country': country,
                            'Count': len(country_df),
                            'With CF': len(country_df[country_df['codeforces_handle'].notna()])
                        })
                
                if country_data:
                    pd.DataFrame(country_data).sort_values('Count', ascending=False).to_excel(
                        writer, sheet_name='By Country', index=False
                    )
                
                # With Codeforces
                with_cf = ioi_df[ioi_df['codeforces_handle'].notna()].copy()
                if not with_cf.empty:
                    with_cf.to_excel(writer, sheet_name='With Codeforces', index=False)
            
            logger.info(f"Successfully saved to {OUTPUT_FILE}")
            
            print("\n" + "="*70)
            print("SCRAPING COMPLETE!")
            print("="*70)
            print(f"Total participants: {len(ioi_df)}")
            print(f"With CF handle: {len(ioi_df[ioi_df['codeforces_handle'].notna()])}")
            print(f"Without CF handle: {len(ioi_df[ioi_df['codeforces_handle'].isna()])}")
            print(f"\nSaved to: {OUTPUT_FILE}")
            
        else:
            logger.warning("No data scraped")
        
    except KeyboardInterrupt:
        logger.info("Interrupted by user")
    except Exception as e:
        logger.error(f"Error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()