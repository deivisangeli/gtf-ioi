#!/usr/bin/env python3
"""
IOI Complete Data Collector - Auto-Update with Scores

Features:
- Auto-detects last year in ALL files (cphof, stats, total)
- Only scrapes missing years
- APPENDS new data to existing files (preserves old years)
- Collects from BOTH sources (CPHOF + IOI Stats)
- **NOW INCLUDES SCORE from both sources**
- Handles variable number of tasks per year
- No duplicates (based on name + year)
- Full data collection mode
- Execution time tracking

Author: Thais Takeuchi
Date: December 2025
"""
import requests
from bs4 import BeautifulSoup
import time as time_module
import pandas as pd
from pathlib import Path
import logging
import os
import re
from typing import List, Dict, Set, Optional
from urllib.parse import urljoin
from datetime import datetime

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
CURRENT_YEAR = datetime.now().year
DELAY_BETWEEN_REQUESTS = 1.5

# TEST MODE: Set to False for full collection
TEST_MODE = False
TEST_LIMIT = 20

CPHOF_BASE_URL = "https://cphof.org/standings/ioi/"
IOI_BASE_URL = "https://stats.ioinformatics.org/contestants/"
IOI_STATS_BASE = "https://stats.ioinformatics.org/"
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


def get_years_from_file(filepath: Path) -> Set[int]:
    """Get years present in a given Excel file"""
    if not filepath.exists():
        return set()
    
    try:
        df = pd.read_excel(filepath, sheet_name='All Participants' if filepath == OUTPUT_FILE else 0)
        
        # Try to find year column (may be 'Year' or 'year')
        year_col = None
        for col in df.columns:
            if col.lower() == 'year':
                year_col = col
                break
        
        if year_col:
            years = set(df[year_col].unique())
            logger.info(f"  {filepath.name}: Years {sorted(years)}")
            return years
        else:
            logger.warning(f"  {filepath.name}: No 'Year' column found")
            return set()
    except Exception as e:
        logger.warning(f"  {filepath.name}: Error reading file - {e}")
        return set()


def load_existing_file(filepath: Path, file_description: str) -> pd.DataFrame:
    """Load existing data from any Excel file"""
    if filepath.exists():
        logger.info(f"Loading existing {file_description} from {filepath}")
        try:
            df = pd.read_excel(filepath)
            logger.info(f"  Loaded {len(df)} existing records")
            return df
        except Exception as e:
            logger.warning(f"  Error loading {file_description}: {e}")
            return pd.DataFrame()
    else:
        logger.info(f"No existing {file_description} found")
        return pd.DataFrame()


def determine_years_to_scrape() -> tuple[int, int, List[int]]:
    """
    Determine which years need scraping based on ALL existing files.
    Only scrapes years that are NOT in any of: cphof, stats, or total files.
    Returns: (start_year, end_year, years_to_scrape)
    """
    logger.info("="*70)
    logger.info("CHECKING EXISTING FILES FOR YEARS")
    logger.info("="*70)
    
    # Get years from all files
    cphof_years = get_years_from_file(CPHOF_FILE)
    stats_years = get_years_from_file(IOI_STATS_FILE)
    total_years = get_years_from_file(OUTPUT_FILE)
    
    # Union of all years (years present in ANY file)
    all_existing_years = cphof_years | stats_years | total_years
    
    if all_existing_years:
        logger.info(f"\n✓ Found existing data:")
        logger.info(f"  All years present in any file: {sorted(all_existing_years)}")
        logger.info(f"  Latest year in database: {max(all_existing_years)}")
        
        last_year = max(all_existing_years)
        
        # Determine if we need to add new years
        if last_year >= CURRENT_YEAR:
            logger.info(f"  Database is up to date (covers up to {last_year})")
            logger.info("="*70)
            return last_year, CURRENT_YEAR, []
        else:
            # Need to add years from (last_year + 1) to CURRENT_YEAR
            start_year = last_year + 1
            end_year = CURRENT_YEAR
            years_to_scrape = list(range(start_year, end_year + 1))
            logger.info(f"\n📌 Need to ADD new years: {years_to_scrape}")
            logger.info("="*70)
            return start_year, end_year, years_to_scrape
    else:
        logger.info("\n✗ No existing files found")
        # Start from 2011 (first year with good data)
        start_year = 2011
        end_year = CURRENT_YEAR
        years_to_scrape = list(range(start_year, end_year + 1))
        logger.info(f"  Will create NEW database with years: {start_year}-{end_year}")
        logger.info("="*70)
        return start_year, end_year, years_to_scrape


def format_duration(seconds):
    """Format duration in human-readable format"""
    if seconds < 60:
        return f"{seconds:.1f} seconds"
    elif seconds < 3600:
        minutes = seconds / 60
        return f"{minutes:.1f} minutes"
    else:
        hours = seconds / 3600
        return f"{hours:.2f} hours"


class CPHOFScraper:
    """Scraper for CPHOF site"""
    
    def __init__(self, delay: float = 1.5, test_mode: bool = False, test_limit: int = 20):
        self.delay = delay
        self.test_mode = test_mode
        self.test_limit = test_limit
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
            return BeautifulSoup(response.content, 'html.parser')
        except Exception as e:
            logger.error(f"Error fetching {url}: {e}")
            return None
    
    def get_medal_info(self, rank_cell) -> tuple:
        """Extract medal information from rank cell"""
        rank_text = rank_cell.get_text().strip()
        medal_type = "No Award"
        
        # Look for medal images or icons
        medal_img = rank_cell.find('img')
        if medal_img:
            img_src = medal_img.get('src', '').lower()
            img_alt = medal_img.get('alt', '').lower()
            
            if any(indicator in img_src for indicator in ['gold', 'primeiro', '1st', 'ouro']):
                medal_type = "Gold"
            elif any(indicator in img_src for indicator in ['silver', 'segundo', '2nd', 'prata']):
                medal_type = "Silver"
            elif any(indicator in img_src for indicator in ['bronze', 'terceiro', '3rd']):
                medal_type = "Bronze"
            elif any(indicator in img_alt for indicator in ['gold', 'primeiro', '1st', 'ouro']):
                medal_type = "Gold"
            elif any(indicator in img_alt for indicator in ['silver', 'segundo', '2nd', 'prata']):
                medal_type = "Silver"
            elif any(indicator in img_alt for indicator in ['bronze', 'terceiro', '3rd']):
                medal_type = "Bronze"
        
        # Look for CSS classes
        cell_classes = ' '.join(rank_cell.get('class', [])).lower()
        if any(indicator in cell_classes for indicator in ['gold', 'ouro']):
            medal_type = "Gold"
        elif any(indicator in cell_classes for indicator in ['silver', 'prata']):
            medal_type = "Silver"
        elif any(indicator in cell_classes for indicator in ['bronze']):
            medal_type = "Bronze"
        elif any(indicator in cell_classes for indicator in ['hm']):
            medal_type = "HM"
        
        # Extract rank number
        rank_number = None
        rank_match = re.search(r'\d+', rank_text)
        if rank_match:
            rank_number = int(rank_match.group())
        
        return rank_number, medal_type
    
    def extract_codeforces_link(self, profile_url: str) -> Optional[str]:
        """Extract Codeforces profile link from participant's profile page"""
        soup = self.get_page_content(profile_url)
        
        if not soup:
            return None
        
        # Look for Codeforces links
        links = soup.find_all('a', href=True)
        
        for link in links:
            href = link.get('href', '').lower()
            text = link.get_text().strip().lower()
            
            if 'codeforces.com' in href:
                full_url = urljoin(profile_url, link.get('href'))
                return full_url
            
            if 'codeforces' in text and link.get('href'):
                full_url = urljoin(profile_url, link.get('href'))
                return full_url
        
        return None
    
    def extract_participants_from_standings(self, year: int) -> List[Dict]:
        """Extract participant information from standings page"""
        url = f"{CPHOF_BASE_URL}{year}"
        soup = self.get_page_content(url)
        
        if not soup:
            return []
        
        participants = []
        
        # Look for the Final standings table
        final_section = soup.find('h2', string=lambda text: text and 'final' in text.lower())
        if not final_section:
            final_section = soup.find(['h1', 'h2', 'h3'], 
                                     string=lambda text: text and any(keyword in text.lower() 
                                                                     for keyword in ['final', 'result', 'standing']))
        
        # Find the table
        table = None
        if final_section:
            table = final_section.find_next('table')
        
        if not table:
            tables = soup.find_all('table')
            for t in tables:
                headers = t.find('tr')
                if headers:
                    header_texts = [th.get_text().strip().lower() for th in headers.find_all(['th', 'td'])]
                    if any(keyword in ' '.join(header_texts) for keyword in ['rank', 'name', 'country', 'score']):
                        table = t
                        break
        
        if not table:
            logger.warning(f"CPHOF: No standings table found for {year}")
            return []
        
        # Find column indices from header
        header_row = table.find('tr')
        headers = header_row.find_all(['th', 'td']) if header_row else []
        header_texts = [h.get_text().strip().lower() for h in headers]
        
        # Find Score column index
        score_col_idx = None
        for idx, header_text in enumerate(header_texts):
            if 'score' in header_text:
                score_col_idx = idx
                break
        
        # Extract participants
        rows = table.find_all('tr')
        data_rows = rows[1:] if len(rows) > 1 else rows
        
        # Apply test mode limit
        if self.test_mode and len(data_rows) > self.test_limit:
            logger.info(f"TEST MODE: Limiting to first {self.test_limit} participants")
            data_rows = data_rows[:self.test_limit]
        
        for row_idx, row in enumerate(data_rows):
            cells = row.find_all(['td', 'th'])
            
            if len(cells) < 3:
                continue
            
            participant_name = None
            participant_link = None
            country = None
            rank_number = None
            medal_type = "No Award"
            score = None
            
            # Extract data from each cell
            for cell_idx, cell in enumerate(cells):
                cell_text = cell.get_text().strip()
                links = cell.find_all('a', href=True)
                
                # First cell: rank with medal info
                if cell_idx == 0:
                    rank_number, medal_type = self.get_medal_info(cell)
                    if not rank_number:
                        try:
                            rank_number = int(cell_text) if cell_text.isdigit() else row_idx + 1
                        except:
                            rank_number = row_idx + 1
                
                # Second cell: country
                elif cell_idx == 1:
                    img = cell.find('img')
                    if img and ('flag' in img.get('src', '').lower() or 'country' in img.get('src', '').lower()):
                        country = cell_text
                    elif cell_text and not any('/profile/' in link.get('href', '') for link in links):
                        country = cell_text
                
                # Score column
                if score_col_idx is not None and cell_idx == score_col_idx:
                    try:
                        # Try to convert to float
                        score_text = cell_text.strip()
                        if score_text:
                            score = float(score_text)
                    except ValueError:
                        score = None
                
                # Look for participant name and profile link
                for link in links:
                    href = link.get('href')
                    link_text = link.get_text().strip()
                    if href and '/profile/' in href and link_text:
                        participant_name = link_text
                        participant_link = href
                        break
                
                # If no profile link, try to extract name
                if not participant_name and cell_text and not cell_text.isdigit():
                    if cell_idx != 1 and (score_col_idx is None or cell_idx != score_col_idx):
                        if not any(keyword in cell_text.lower() for keyword in ['score', 'points']):
                            if len(cell_text.split()) <= 4 and cell_text != country:
                                if not participant_name:
                                    participant_name = cell_text
            
            # Add participant if found
            if participant_name:
                full_profile_url = urljoin(url, participant_link) if participant_link else None
                
                participant_info = {
                    'year': year,
                    'rank': rank_number,
                    'medal': medal_type,
                    'name': participant_name,
                    'country': country,
                    'score': score,
                    'profile_url': full_profile_url,
                    'codeforces_url': None
                }
                
                participants.append(participant_info)
        
        logger.info(f"CPHOF {year}: Found {len(participants)} participants in standings")
        return participants
    
    def scrape_year(self, year: int) -> List[Dict]:
        """Scrape participants from cphof.org for a given year"""
        logger.info(f"\n{'='*60}")
        logger.info(f"CPHOF: Scraping year {year}")
        if self.test_mode:
            logger.info(f"TEST MODE: Limited to {self.test_limit} participants")
        logger.info(f"{'='*60}")
        
        year_start_time = time_module.time()
        
        # Get participants from standings page
        participants = self.extract_participants_from_standings(year)
        
        if not participants:
            return []
        
        # For each participant, try to get their Codeforces profile
        logger.info(f"Extracting Codeforces links from {len(participants)} profiles...")
        
        for i, participant in enumerate(participants):
            logger.info(f"Processing {i+1}/{len(participants)}: {participant['name']}")
            
            if participant['profile_url']:
                codeforces_url = self.extract_codeforces_link(participant['profile_url'])
                participant['codeforces_url'] = codeforces_url
            else:
                logger.warning(f"No profile URL for {participant['name']}")
                participant['codeforces_url'] = None
            
            if i < len(participants) - 1:
                time_module.sleep(self.delay)
        
        # Convert to standardized format
        standardized = []
        for p in participants:
            standardized.append({
                'Year': p['year'],
                'Contestant': p['name'],
                'Country': clean_country_name(p['country']),
                'Result': p['medal'],
                'Score': p['score'],
                'CF_Link': p['codeforces_url'],
                'Source': 'CPHOF'
            })
        
        year_duration = time_module.time() - year_start_time
        
        logger.info(f"CPHOF {year}: Completed with {len(standardized)} participants")
        cf_count = len([p for p in standardized if p['CF_Link']])
        logger.info(f"CPHOF {year}: {cf_count} participants have Codeforces links")
        logger.info(f"CPHOF {year}: Duration - {format_duration(year_duration)}")
        
        return standardized
    
    def scrape_years(self, years: List[int]) -> pd.DataFrame:
        """Scrape multiple years"""
        all_participants = []
        
        for year in years:
            participants = self.scrape_year(year)
            all_participants.extend(participants)
            
            if year < max(years):
                logger.info(f"Completed year {year}. Waiting before next year...")
                time_module.sleep(self.delay * 2)
        
        df = pd.DataFrame(all_participants)
        logger.info(f"\n{'='*60}")
        logger.info(f"CPHOF Total: {len(df)} participants from {len(years)} years")
        logger.info(f"CPHOF Total with CF links: {len(df[df['CF_Link'].notna()])}")
        logger.info(f"{'='*60}")
        
        return df


class IOIStatsScraper:
    """Scraper for IOI Stats site"""
    
    def __init__(self, delay: float = 1.5, test_mode: bool = False, test_limit: int = 20):
        self.delay = delay
        self.test_mode = test_mode
        self.test_limit = test_limit
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
    
    def extract_absolute_score(self, person_url: str, year: int) -> Optional[float]:
        """
        Extract absolute score from participant's individual page
        Handles variable number of task columns per year
        
        Args:
            person_url: URL like "people/7663"
            year: Year of competition
            
        Returns:
            Absolute score as float, or None if not found
        """
        full_url = urljoin(IOI_STATS_BASE, person_url)
        soup = self.get_page_content(full_url)
        
        if not soup:
            return None
        
        try:
            # Find the maincontent div with the table
            main_content = soup.find('div', class_='maincontent')
            if not main_content:
                return None
            
            table = main_content.find('table')
            if not table:
                return None
            
            # Find the row for this year
            rows = table.find_all('tr')
            for row in rows:
                cells = row.find_all('td')
                if not cells:
                    continue
                
                # Check if first cell contains the year link
                first_cell = cells[0]
                year_link = first_cell.find('a', href=re.compile(r'olympiads/\d+'))
                if year_link:
                    year_text = year_link.get_text().strip()
                    if year_text == str(year):
                        # Found the right year row
                        # Now find Score Abs column
                        # Strategy: Find first cell after tasks that contains only a number (no % or /)
                        
                        # Start from cell 2 (skip Year and Country)
                        for i in range(2, len(cells)):
                            cell_text = cells[i].get_text().strip()
                            
                            # Skip empty cells
                            if not cell_text:
                                continue
                            
                            # Skip cells with class="taskscore" (these are individual task scores)
                            if 'taskscore' in cells[i].get('class', []):
                                continue
                            
                            # Check if it's a pure number (Score Abs)
                            # - Should NOT contain '%' (Score Rel, Rank Rel)
                            # - Should NOT contain '/' (Rank Abs like "100/303")
                            # - Should be numeric
                            if '%' not in cell_text and '/' not in cell_text:
                                try:
                                    score = float(cell_text)
                                    logger.info(f"    Found Score Abs: {score} for year {year}")
                                    return score
                                except ValueError:
                                    # Not a number, continue searching
                                    continue
                        
                        # If we get here, we found the year but couldn't find the score
                        logger.warning(f"    Found year {year} but couldn't extract score")
                        return None
            
            # Year not found in table
            logger.warning(f"    Year {year} not found in participant's history")
            return None
            
        except Exception as e:
            logger.warning(f"Error extracting score from {full_url}: {e}")
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
        
        # Apply test mode limit
        if self.test_mode and len(data_rows) > self.test_limit:
            logger.info(f"TEST MODE: Limiting to first {self.test_limit} participants")
            data_rows = data_rows[:self.test_limit]
        
        current_country = None
        
        for i, row in enumerate(data_rows):
            cells = row.find_all('td')
            
            if len(cells) < 3:
                continue
            
            # Column 0: Participant name
            name_cell = cells[0]
            name_link = name_cell.find('a', href=re.compile(r'people/\d+'))
            if not name_link:
                continue
            
            participant_name = name_link.get_text().strip()
            person_url = name_link.get('href')
            
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
            
            # Extract absolute score from individual page
            logger.info(f"  [{i+1}/{len(data_rows)}] Fetching score for {participant_name}...")
            score = self.extract_absolute_score(person_url, year)
            
            participants.append({
                'Year': year,
                'Contestant': participant_name,
                'Country': clean_country_name(country),
                'Result': medal,
                'Score': score,
                'CF_Link': create_cf_link(codeforces_handle),
                'Source': 'IOI_Stats'
            })
            
            # Delay between profile visits
            if i < len(data_rows) - 1:
                time_module.sleep(self.delay)
        
        logger.info(f"IOI Stats {year}: Found {len(participants)} participants")
        return participants
    
    def scrape_years(self, years: List[int]) -> pd.DataFrame:
        """Scrape multiple years"""
        all_participants = []
        
        for year in years:
            logger.info(f"\n{'='*60}")
            logger.info(f"IOI Stats: Scraping year {year}")
            if self.test_mode:
                logger.info(f"TEST MODE: Limited to {self.test_limit} participants")
            logger.info(f"{'='*60}")
            
            year_start_time = time_module.time()
            
            participants = self.scrape_year(year)
            all_participants.extend(participants)
            
            year_duration = time_module.time() - year_start_time
            logger.info(f"IOI Stats {year}: Duration - {format_duration(year_duration)}")
            
            if year < max(years):
                time_module.sleep(self.delay)
        
        df = pd.DataFrame(all_participants)
        logger.info(f"\nIOI Stats Total: {len(df)} participants from {len(years)} years")
        
        return df


def load_existing_data() -> pd.DataFrame:
    """Load existing ioi_total.xlsx if it exists"""
    if OUTPUT_FILE.exists():
        logger.info(f"Loading existing data from {OUTPUT_FILE}")
        df = pd.read_excel(OUTPUT_FILE, sheet_name='All Participants')
        logger.info(f"Loaded {len(df)} existing participants")
        return df
    else:
        logger.info("No existing data found")
        return pd.DataFrame()


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
    
    # Update source
    missing_df['Source'] = 'IOI_Stats_Only'
    
    logger.info(f"CPHOF participants: {len(cphof_df)}")
    logger.info(f"IOI Stats participants: {len(ioi_stats_df)}")
    logger.info(f"In both: {len(ioi_stats_df) - len(missing_df)}")
    logger.info(f"Missing from CPHOF (will be added): {len(missing_df)}")
    
    # Clean up temp column
    cphof_df.drop(columns=['_key'], inplace=True)
    
    return missing_df


def remove_duplicates(df: pd.DataFrame) -> pd.DataFrame:
    """
    Remove duplicate participants based on name+year
    Keep the first occurrence (priority: CPHOF > IOI_Stats_Only)
    """
    df['_comparison_key'] = df.apply(
        lambda row: create_comparison_key(row['Contestant'], row['Year']), 
        axis=1
    )
    
    before_count = len(df)
    
    # Sort to give priority to CPHOF entries
    df['_priority'] = df['Source'].map({'CPHOF': 0, 'IOI_Stats': 1, 'IOI_Stats_Only': 2})
    df = df.sort_values('_priority')
    
    df = df.drop_duplicates(subset='_comparison_key', keep='first')
    df = df.drop(columns=['_comparison_key', '_priority'])
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
                'Avg_Score': year_data['Score'].mean() if 'Score' in year_data.columns else None,
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
                'Avg_Score': country_df['Score'].mean() if 'Score' in country_df.columns else None,
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
    # Start timer
    script_start_time = time_module.time()
    
    print("=" * 70)
    print("IOI COMPLETE DATA COLLECTOR - WITH SCORES")
    print("=" * 70)
    print(f"Base Directory: {BASE_DIR}")
    print(f"Output File: {OUTPUT_FILE}")
    print(f"Current Year: {CURRENT_YEAR}")
    if TEST_MODE:
        print(f"⚠️  TEST MODE ENABLED: Limited to {TEST_LIMIT} participants per year")
    else:
        print(f"✅ FULL MODE: Collecting ALL participants")
    print(f"🆕 NOW INCLUDES: Score from both CPHOF and IOI Stats")
    print("=" * 70)
    
    BASE_DIR.mkdir(parents=True, exist_ok=True)
    
    # Determine which years to scrape
    start_year, end_year, years_to_scrape = determine_years_to_scrape()
    
    if not years_to_scrape:
        print("\n" + "="*70)
        print("✓ DATABASE IS UP TO DATE!")
        print("="*70)
        existing_df = load_existing_data()
        print(f"Total participants: {len(existing_df)}")
        print(f"Years covered: {sorted(existing_df['Year'].unique())}")
        
        script_duration = time_module.time() - script_start_time
        print(f"\n⏱️  Total execution time: {format_duration(script_duration)}")
        return
    
    logger.info(f"\n🎯 Years to scrape: {years_to_scrape}")
    logger.info(f"📊 Will scrape {len(years_to_scrape)} year(s)")
    
    # STEP 1: Scrape CPHOF
    print("\n" + "="*70)
    print("STEP 1: SCRAPING CPHOF (with profile visits + scores)")
    print("="*70)
    step1_start = time_module.time()
    
    cphof_scraper = CPHOFScraper(delay=DELAY_BETWEEN_REQUESTS, test_mode=TEST_MODE, test_limit=TEST_LIMIT)
    new_cphof_df = cphof_scraper.scrape_years(years_to_scrape)
    
    step1_duration = time_module.time() - step1_start
    logger.info(f"✓ STEP 1 completed in {format_duration(step1_duration)}")
    
    if new_cphof_df.empty:
        logger.error("Failed to scrape CPHOF data")
        return
    
    # Load existing CPHOF data and combine
    logger.info("\n" + "="*70)
    logger.info("COMBINING CPHOF DATA WITH EXISTING")
    logger.info("="*70)
    existing_cphof_df = load_existing_file(CPHOF_FILE, "CPHOF data")
    
    if not existing_cphof_df.empty:
        cphof_df = pd.concat([existing_cphof_df, new_cphof_df], ignore_index=True)
        logger.info(f"Combined: {len(existing_cphof_df)} existing + {len(new_cphof_df)} new = {len(cphof_df)} total")
        
        # Remove duplicates from CPHOF
        cphof_df = remove_duplicates(cphof_df)
        cphof_df = cphof_df.sort_values(['Year', 'Contestant']).reset_index(drop=True)
    else:
        cphof_df = new_cphof_df
        logger.info(f"No existing CPHOF data, using new data only: {len(cphof_df)} records")
    
    # Save CPHOF data
    cphof_df.to_excel(CPHOF_FILE, index=False)
    logger.info(f"✓ Saved complete CPHOF data to {CPHOF_FILE}")
    logger.info(f"  Years in CPHOF file: {sorted(cphof_df['Year'].unique())}")
    
    time_module.sleep(DELAY_BETWEEN_REQUESTS)
    
    # STEP 2: Scrape IOI Stats
    print("\n" + "="*70)
    print("STEP 2: SCRAPING IOI STATS (with individual score visits)")
    print("⚠️  This will take longer due to individual profile scraping")
    print("="*70)
    step2_start = time_module.time()
    
    ioi_scraper = IOIStatsScraper(delay=DELAY_BETWEEN_REQUESTS, test_mode=TEST_MODE, test_limit=TEST_LIMIT)
    new_ioi_stats_df = ioi_scraper.scrape_years(years_to_scrape)
    
    step2_duration = time_module.time() - step2_start
    logger.info(f"✓ STEP 2 completed in {format_duration(step2_duration)}")
    
    if new_ioi_stats_df.empty:
        logger.error("Failed to scrape IOI Stats data")
        return
    
    # Load existing IOI Stats data and combine
    logger.info("\n" + "="*70)
    logger.info("COMBINING IOI STATS DATA WITH EXISTING")
    logger.info("="*70)
    existing_ioi_stats_df = load_existing_file(IOI_STATS_FILE, "IOI Stats data")
    
    if not existing_ioi_stats_df.empty:
        ioi_stats_df = pd.concat([existing_ioi_stats_df, new_ioi_stats_df], ignore_index=True)
        logger.info(f"Combined: {len(existing_ioi_stats_df)} existing + {len(new_ioi_stats_df)} new = {len(ioi_stats_df)} total")
        
        # Remove duplicates from IOI Stats
        ioi_stats_df = remove_duplicates(ioi_stats_df)
        ioi_stats_df = ioi_stats_df.sort_values(['Year', 'Contestant']).reset_index(drop=True)
    else:
        ioi_stats_df = new_ioi_stats_df
        logger.info(f"No existing IOI Stats data, using new data only: {len(ioi_stats_df)} records")
    
    # Save IOI Stats data
    ioi_stats_df.to_excel(IOI_STATS_FILE, index=False)
    logger.info(f"✓ Saved complete IOI Stats data to {IOI_STATS_FILE}")
    logger.info(f"  Years in IOI Stats file: {sorted(ioi_stats_df['Year'].unique())}")
    
    # STEP 3: Compare and find missing (only for NEW data)
    print("\n" + "="*70)
    print("STEP 3: COMPARING NEW DATASETS AND FINDING MISSING")
    print("="*70)
    step3_start = time_module.time()
    
    missing_df = compare_and_find_missing(new_cphof_df, new_ioi_stats_df)
    
    step3_duration = time_module.time() - step3_start
    logger.info(f"✓ STEP 3 completed in {format_duration(step3_duration)}")
    
    # STEP 4: Combine all data
    print("\n" + "="*70)
    print("STEP 4: COMBINING DATA FOR IOI_TOTAL")
    print("="*70)
    step4_start = time_module.time()
    
    # Combine: NEW CPHOF + Missing from NEW IOI Stats
    new_data = pd.concat([new_cphof_df, missing_df], ignore_index=True)
    logger.info(f"New data total: {len(new_data)} participants")
    
    # Load existing data
    existing_df = load_existing_data()
    
    # Combine with existing data if any
    if not existing_df.empty:
        combined_df = pd.concat([existing_df, new_data], ignore_index=True)
        logger.info(f"Combined with existing data: {len(combined_df)} total participants")
    else:
        combined_df = new_data
    
    # Remove duplicates
    logger.info("\nRemoving duplicates...")
    combined_df = remove_duplicates(combined_df)
    
    # Sort
    combined_df = combined_df.sort_values(['Year', 'Contestant']).reset_index(drop=True)
    
    # Ensure column order
    column_order = ['Year', 'Contestant', 'Country', 'Result', 'Score', 'CF_Link', 'Source']
    combined_df = combined_df[[col for col in column_order if col in combined_df.columns]]
    
    # Save final data
    save_consolidated_data(combined_df)
    
    step4_duration = time_module.time() - step4_start
    logger.info(f"✓ STEP 4 completed in {format_duration(step4_duration)}")
    
    # Calculate total duration
    script_duration = time_module.time() - script_start_time
    
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
    
    if 'Score' in combined_df.columns:
        print(f"\nScores:")
        print(f"  With Score: {len(combined_df[combined_df['Score'].notna()])}")
        print(f"  Average Score: {combined_df['Score'].mean():.2f}")
    
    print(f"\nYears covered: {sorted(combined_df['Year'].unique())}")
    
    print(f"\nFiles saved:")
    print(f"  - CPHOF data: {CPHOF_FILE}")
    print(f"    Years: {sorted(cphof_df['Year'].unique())}")
    print(f"  - IOI Stats data: {IOI_STATS_FILE}")
    print(f"    Years: {sorted(ioi_stats_df['Year'].unique())}")
    print(f"  - Final consolidated: {OUTPUT_FILE}")
    print(f"    Years: {sorted(combined_df['Year'].unique())}")
    
    # Execution time breakdown
    print("\n" + "="*70)
    print("⏱️  EXECUTION TIME BREAKDOWN:")
    print("="*70)
    print(f"STEP 1 (CPHOF scraping):     {format_duration(step1_duration)}")
    print(f"STEP 2 (IOI Stats scraping): {format_duration(step2_duration)}")
    print(f"STEP 3 (Comparison):         {format_duration(step3_duration)}")
    print(f"STEP 4 (Consolidation):      {format_duration(step4_duration)}")
    print(f"{'─'*70}")
    print(f"TOTAL EXECUTION TIME:        {format_duration(script_duration)}")
    print("="*70)
    
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