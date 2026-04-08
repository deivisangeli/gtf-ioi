#!/usr/bin/env python3
"""
IOI Codeforces Profile Scraper

This script scrapes IOI (International Olympiad in Informatics) standings 
from 2011 to 2025 and extracts Codeforces profile links for each participant.

Requirements:
- requests
- beautifulsoup4
- lxml (optional, for faster parsing)
- pandas (for Excel output)
- openpyxl (for Excel file support)

Install with: pip install requests beautifulsoup4 lxml pandas openpyxl
"""

import requests
from bs4 import BeautifulSoup
import time
import csv
from urllib.parse import urljoin, urlparse
import json
from typing import List, Dict, Optional
import logging
import pandas as pd

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class IOICodeforcesScraper:
    """
    A scraper class to extract Codeforces profiles from IOI standings pages
    """
    
    def __init__(self, delay: float = 1.0):
        """
        Initialize the scraper with a delay between requests
        
        Args:
            delay (float): Delay in seconds between HTTP requests to be respectful
        """
        self.base_url = "https://cphof.org/standings/ioi/"
        self.delay = delay
        self.session = requests.Session()
        
        # Set a user agent to appear more like a regular browser
        self.session.headers.update({
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
        })
        
        # Store results
        self.results = []
    
    def get_page_content(self, url: str) -> Optional[BeautifulSoup]:
        """
        Fetch and parse a web page
        
        Args:
            url (str): The URL to fetch
            
        Returns:
            BeautifulSoup: Parsed HTML content or None if failed
        """
        try:
            logger.info(f"Fetching: {url}")
            response = self.session.get(url, timeout=30)
            response.raise_for_status()
            
            # Parse with BeautifulSoup
            soup = BeautifulSoup(response.content, 'html.parser')
            return soup
            
        except requests.exceptions.RequestException as e:
            logger.error(f"Error fetching {url}: {e}")
            return None
        except Exception as e:
            logger.error(f"Unexpected error parsing {url}: {e}")
            return None
    
    def get_medal_info(self, rank_cell) -> tuple:
        """
        Extract medal information from rank cell
        
        Args:
            rank_cell: BeautifulSoup cell containing rank information
            
        Returns:
            tuple: (rank_number, medal_type)
        """
        rank_text = rank_cell.get_text().strip()
        medal_type = "No Medal"
        
        # Look for medal images or icons
        medal_img = rank_cell.find('img')
        if medal_img:
            img_src = medal_img.get('src', '').lower()
            img_alt = medal_img.get('alt', '').lower()
            img_title = medal_img.get('title', '').lower()
            
            # Check image source, alt text, or title for medal indicators
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
            elif any(indicator in img_title for indicator in ['gold', 'primeiro', '1st', 'ouro']):
                medal_type = "Gold"
            elif any(indicator in img_title for indicator in ['silver', 'segundo', '2nd', 'prata']):
                medal_type = "Silver"
            elif any(indicator in img_title for indicator in ['bronze', 'terceiro', '3rd']):
                medal_type = "Bronze"
        
        # Look for CSS classes that might indicate medal types
        cell_classes = ' '.join(rank_cell.get('class', [])).lower()
        if any(indicator in cell_classes for indicator in ['gold', 'ouro', 'primeiro']):
            medal_type = "Gold"
        elif any(indicator in cell_classes for indicator in ['silver', 'prata', 'segundo']):
            medal_type = "Silver"
        elif any(indicator in cell_classes for indicator in ['bronze', 'terceiro']):
            medal_type = "Bronze"
        
        # Extract numeric rank
        rank_number = None
        # Try to extract number from rank text
        import re
        rank_match = re.search(r'\d+', rank_text)
        if rank_match:
            rank_number = int(rank_match.group())
        
        return rank_number, medal_type

    def extract_participants_from_standings(self, year: int) -> List[Dict]:
        """
        Extract participant information from IOI standings page for a given year
        
        Args:
            year (int): The year to scrape (2010-2025)
            
        Returns:
            List[Dict]: List of participant dictionaries with basic info
        """
        url = f"{self.base_url}{year}"
        soup = self.get_page_content(url)
        
        if not soup:
            logger.warning(f"Could not fetch standings for year {year}")
            return []
        
        participants = []
        
        # Look for the Final standings table
        # First, try to find a table with "Final" heading or similar
        final_section = soup.find('h2', string=lambda text: text and 'final' in text.lower())
        if not final_section:
            # Look for any heading that might indicate the main results
            final_section = soup.find(['h1', 'h2', 'h3'], string=lambda text: text and any(keyword in text.lower() for keyword in ['final', 'result', 'standing']))
        
        # Find the table after the Final section, or just look for tables
        table = None
        if final_section:
            # Look for the next table after the Final heading
            table = final_section.find_next('table')
        
        # If no table found near Final section, look for any table with participant data
        if not table:
            tables = soup.find_all('table')
            for t in tables:
                # Check if table has typical IOI columns
                headers = t.find('tr')
                if headers:
                    header_texts = [th.get_text().strip().lower() for th in headers.find_all(['th', 'td'])]
                    # Look for typical IOI standings columns
                    if any(keyword in ' '.join(header_texts) for keyword in ['rank', 'name', 'country', 'score']):
                        table = t
                        break
        
        if not table:
            logger.warning(f"Could not find standings table for year {year}")
            return []
        
        # Extract participants from the table
        rows = table.find_all('tr')
        
        # Skip header row(s) - usually the first row
        data_rows = rows[1:] if len(rows) > 1 else rows
        
        for row_idx, row in enumerate(data_rows):
            cells = row.find_all(['td', 'th'])
            
            if len(cells) < 3:  # Skip rows with too few columns
                continue
            
            # Initialize variables
            participant_name = None
            participant_link = None
            country = None
            rank_number = None
            medal_type = "No Medal"
            score = None
            
            # Extract data from each cell
            for cell_idx, cell in enumerate(cells):
                cell_text = cell.get_text().strip()
                links = cell.find_all('a', href=True)
                
                # First cell is usually rank with medal info
                if cell_idx == 0:
                    rank_number, medal_type = self.get_medal_info(cell)
                    if not rank_number:
                        # Fallback: try to get rank from text
                        try:
                            rank_number = int(cell_text) if cell_text.isdigit() else row_idx + 1
                        except:
                            rank_number = row_idx + 1
                
                # Second cell is usually country (look for flag images or country names)
                elif cell_idx == 1:
                    # Check if this cell has a flag image
                    img = cell.find('img')
                    if img and ('flag' in img.get('src', '').lower() or 'country' in img.get('src', '').lower()):
                        country = cell_text
                    # Or if it contains country-like text without being a profile link
                    elif cell_text and not any('/profile/' in link.get('href', '') for link in links):
                        # This is likely a country name
                        country = cell_text
                
                # Look for participant name (usually has profile link)
                # Check all cells for participant profile links
                for link in links:
                    href = link.get('href')
                    link_text = link.get_text().strip()
                    if href and '/profile/' in href and link_text:
                        participant_name = link_text
                        participant_link = href
                        break
                
                # If no profile link found yet, check if this looks like a name column
                if not participant_name and cell_text and not cell_text.isdigit():
                    # Skip if this is the country cell
                    if cell_idx != 1:
                        # Check if this might be a score
                        if not any(keyword in cell_text.lower() for keyword in ['score', 'points']):
                            # Check if it's not too long to be a name and not the same as country
                            if len(cell_text.split()) <= 4 and cell_text != country:
                                # This might be a participant name
                                if not participant_name:  # Only set if we don't have one yet
                                    participant_name = cell_text
                
                # Look for score (usually numeric and > 50)
                if cell_text.replace('.', '').replace(',', '').isdigit():
                    try:
                        score_val = float(cell_text.replace(',', ''))
                        if score_val > 50:  # Likely a score rather than rank
                            score = cell_text
                    except:
                        pass
            
            # If we found a participant, add them to the list
            if participant_name:
                # Build full profile URL if we have a link
                full_profile_url = urljoin(url, participant_link) if participant_link else None
                
                participant_info = {
                    'year': year,
                    'rank': rank_number,
                    'medal': medal_type,
                    'name': participant_name,
                    'country': country,
                    'score': score,
                    'profile_url': full_profile_url,
                    'codeforces_url': None  # Will be filled later
                }
                
                participants.append(participant_info)
                logger.debug(f"Found participant: {participant_name} (Rank: {rank_number}, Medal: {medal_type})")
        
        logger.info(f"Found {len(participants)} participants for year {year}")
        return participants
    
    def extract_codeforces_link(self, profile_url: str) -> Optional[str]:
        """
        Extract Codeforces profile link from a participant's profile page
        
        Args:
            profile_url (str): URL of the participant's profile page
            
        Returns:
            str: Codeforces profile URL or None if not found
        """
        soup = self.get_page_content(profile_url)
        
        if not soup:
            return None
        
        # Look for Codeforces links in various ways
        codeforces_patterns = [
            'codeforces.com',
            'Codeforces'
        ]
        
        # Search for links containing codeforces
        links = soup.find_all('a', href=True)
        
        for link in links:
            href = link.get('href', '').lower()
            text = link.get_text().strip().lower()
            
            # Check if this is a Codeforces link
            if 'codeforces.com' in href:
                full_url = urljoin(profile_url, link.get('href'))
                logger.info(f"Found Codeforces link: {full_url}")
                return full_url
            
            # Check if the link text mentions Codeforces
            if 'codeforces' in text and link.get('href'):
                full_url = urljoin(profile_url, link.get('href'))
                logger.info(f"Found Codeforces link via text: {full_url}")
                return full_url
        
        # Also check for any external profile sections
        external_sections = soup.find_all(['div', 'section'], class_=lambda x: x and 'external' in x.lower())
        for section in external_sections:
            section_links = section.find_all('a', href=True)
            for link in section_links:
                if 'codeforces.com' in link.get('href', '').lower():
                    full_url = urljoin(profile_url, link.get('href'))
                    logger.info(f"Found Codeforces link in external section: {full_url}")
                    return full_url
        
        return None
    
    def scrape_year(self, year: int, limit_participants: int = None) -> List[Dict]:
        """
        Scrape all participants and their Codeforces profiles for a given year
        
        Args:
            year (int): The year to scrape
            limit_participants (int): If set, only process first N participants (for testing)
            
        Returns:
            List[Dict]: Complete participant data with Codeforces links
        """
        logger.info(f"Starting to scrape year {year}")
        
        # Get participants from standings page
        participants = self.extract_participants_from_standings(year)
        
        # Limit participants for testing if specified (None means no limit - process all)
        if limit_participants and len(participants) > limit_participants:
            logger.info(f"Limiting to first {limit_participants} participants for testing")
            participants = participants[:limit_participants]
        else:
            logger.info(f"Processing all {len(participants)} participants for year {year}")
        
        # For each participant, try to get their Codeforces profile
        for i, participant in enumerate(participants):
            logger.info(f"Processing participant {i+1}/{len(participants)}: {participant['name']} from {participant.get('country', 'Unknown')} (Rank: {participant.get('rank', 'N/A')}, Medal: {participant.get('medal', 'N/A')})")
            
            # Only try to get Codeforces link if we have a profile URL
            if participant['profile_url']:
                codeforces_url = self.extract_codeforces_link(participant['profile_url'])
                participant['codeforces_url'] = codeforces_url
            else:
                logger.warning(f"No profile URL for {participant['name']}")
                participant['codeforces_url'] = None
            
            # Be respectful with delays
            if i < len(participants) - 1:  # Don't delay after the last request
                time.sleep(self.delay)
        
        return participants
    
    def scrape_all_years(self, start_year: int = 2010, end_year: int = 2025, limit_participants: int = None) -> None:
        """
        Scrape all years from start_year to end_year (inclusive)
        
        Args:
            start_year (int): First year to scrape
            end_year (int): Last year to scrape
            limit_participants (int): If set, only process first N participants per year (for testing)
        """
        logger.info(f"Starting to scrape years {start_year} to {end_year}")
        if limit_participants:
            logger.info(f"Testing mode: Only processing first {limit_participants} participants per year")
        
        for year in range(start_year, end_year + 1):
            try:
                participants = self.scrape_year(year, limit_participants)
                self.results.extend(participants)
                
                # Save intermediate results
                self.save_results_to_json(f"ioi_codeforces_partial_{year}.json")
                
                # Longer delay between years
                if year < end_year:
                    logger.info(f"Completed year {year}. Waiting before next year...")
                    time.sleep(self.delay * 2)
                    
            except Exception as e:
                logger.error(f"Error scraping year {year}: {e}")
                continue
        
        logger.info(f"Completed scraping. Total participants found: {len(self.results)}")
    
    def save_results_to_csv(self, filename: str = "ioi_codeforces_results.csv") -> None:
        """
        Save results to a CSV file
        
        Args:
            filename (str): Name of the output CSV file
        """
        if not self.results:
            logger.warning("No results to save")
            return
        
        logger.info(f"Saving {len(self.results)} results to {filename}")
        
        with open(filename, 'w', newline='', encoding='utf-8') as csvfile:
            fieldnames = ['year', 'rank', 'medal', 'name', 'country', 'score', 'profile_url', 'codeforces_url']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            
            writer.writeheader()
            for result in self.results:
                # Only write the essential fields to CSV
                row = {
                    'year': result['year'],
                    'rank': result.get('rank', ''),
                    'medal': result.get('medal', 'No Medal'),
                    'name': result['name'],
                    'country': result.get('country', ''),
                    'score': result.get('score', ''),
                    'profile_url': result['profile_url'],
                    'codeforces_url': result['codeforces_url']
                }
                writer.writerow(row)
        
        logger.info(f"Results saved to {filename}")
    
    def save_results_to_excel(self, filename: str = "ioi_codeforces_results.xlsx") -> None:
        """
        Save results to an Excel file with multiple sheets
        
        Args:
            filename (str): Name of the output Excel file
        """
        if not self.results:
            logger.warning("No results to save")
            return
        
        logger.info(f"Saving {len(self.results)} results to {filename}")
        
        # Prepare data for DataFrame
        df_data = []
        for result in self.results:
            row = {
                'Year': result['year'],
                'Rank': result.get('rank', ''),
                'Medal': result.get('medal', 'No Medal'),
                'Name': result['name'],
                'Country': result.get('country', ''),
                'Score': result.get('score', ''),
                'Profile URL': result['profile_url'],
                'Codeforces URL': result['codeforces_url'],
                'Has Codeforces': 'Yes' if result['codeforces_url'] else 'No'
            }
            df_data.append(row)
        
        # Create DataFrame
        df = pd.DataFrame(df_data)
        
        # Create Excel writer object
        with pd.ExcelWriter(filename, engine='openpyxl') as writer:
            # Write main data to 'All Results' sheet
            df.to_excel(writer, sheet_name='All Results', index=False)
            
            # Create summary sheet by year
            summary_data = []
            for year in sorted(df['Year'].unique()):
                year_data = df[df['Year'] == year]
                total_participants = len(year_data)
                with_codeforces = len(year_data[year_data['Has Codeforces'] == 'Yes'])
                success_rate = (with_codeforces / total_participants * 100) if total_participants > 0 else 0
                
                # Medal counts
                gold_count = len(year_data[year_data['Medal'] == 'Gold'])
                silver_count = len(year_data[year_data['Medal'] == 'Silver'])
                bronze_count = len(year_data[year_data['Medal'] == 'Bronze'])
                
                summary_data.append({
                    'Year': year,
                    'Total Participants': total_participants,
                    'Gold Medals': gold_count,
                    'Silver Medals': silver_count,
                    'Bronze Medals': bronze_count,
                    'With Codeforces': with_codeforces,
                    'Success Rate (%)': round(success_rate, 1)
                })
            
            summary_df = pd.DataFrame(summary_data)
            summary_df.to_excel(writer, sheet_name='Summary by Year', index=False)
            
            # Create separate sheets for medal winners with Codeforces profiles
            medal_types = ['Gold', 'Silver', 'Bronze']
            for medal in medal_types:
                medal_df = df[(df['Medal'] == medal) & (df['Has Codeforces'] == 'Yes')].copy()
                if not medal_df.empty:
                    sheet_name = f'{medal} Winners - Codeforces'
                    medal_df.to_excel(writer, sheet_name=sheet_name, index=False)
            
            # Create sheet with only participants who have Codeforces profiles
            codeforces_df = df[df['Has Codeforces'] == 'Yes'].copy()
            if not codeforces_df.empty:
                codeforces_df.to_excel(writer, sheet_name='With Codeforces Only', index=False)
            
            # Adjust column widths for better readability
            for sheet_name in writer.sheets:
                worksheet = writer.sheets[sheet_name]
                for column in worksheet.columns:
                    max_length = 0
                    column_letter = column[0].column_letter
                    for cell in column:
                        try:
                            if len(str(cell.value)) > max_length:
                                max_length = len(str(cell.value))
                        except:
                            pass
                    adjusted_width = min(max_length + 2, 50)  # Cap at 50 characters
                    worksheet.column_dimensions[column_letter].width = adjusted_width
        
        logger.info(f"Excel results saved to {filename}")
    
    def save_results_to_json(self, filename: str = "ioi_codeforces_results.json") -> None:
        """
        Save results to a JSON file
        
        Args:
            filename (str): Name of the output JSON file
        """
        if not self.results:
            logger.warning("No results to save")
            return
        
        logger.info(f"Saving {len(self.results)} results to {filename}")
        
        with open(filename, 'w', encoding='utf-8') as jsonfile:
            json.dump(self.results, jsonfile, indent=2, ensure_ascii=False)
        
        logger.info(f"Results saved to {filename}")
    
    def print_summary(self) -> None:
        """
        Print a summary of the scraping results
        """
        if not self.results:
            print("No results found.")
            return
        
        total_participants = len(self.results)
        participants_with_codeforces = len([r for r in self.results if r['codeforces_url']])
        
        print(f"\n=== SCRAPING SUMMARY ===")
        print(f"Total participants found: {total_participants}")
        print(f"Participants with Codeforces profiles: {participants_with_codeforces}")
        print(f"Success rate: {participants_with_codeforces/total_participants*100:.1f}%")
        
        # Show results by year with medal information
        years_summary = {}
        for result in self.results:
            year = result['year']
            medal = result.get('medal', 'No Medal')
            if year not in years_summary:
                years_summary[year] = {
                    'total': 0, 
                    'with_codeforces': 0,
                    'gold': 0,
                    'silver': 0, 
                    'bronze': 0,
                    'no_medal': 0
                }
            years_summary[year]['total'] += 1
            if result['codeforces_url']:
                years_summary[year]['with_codeforces'] += 1
            
            # Count medals
            if medal == 'Gold':
                years_summary[year]['gold'] += 1
            elif medal == 'Silver':
                years_summary[year]['silver'] += 1
            elif medal == 'Bronze':
                years_summary[year]['bronze'] += 1
            else:
                years_summary[year]['no_medal'] += 1
        
        print(f"\nResults by year:")
        for year in sorted(years_summary.keys()):
            data = years_summary[year]
            rate = data['with_codeforces']/data['total']*100 if data['total'] > 0 else 0
            print(f"  {year}: {data['with_codeforces']}/{data['total']} with Codeforces ({rate:.1f}%)")
            print(f"       Medals: {data['gold']} Gold, {data['silver']} Silver, {data['bronze']} Bronze")


def main():
    """
    Main function to run the scraper
    """
    print("IOI Codeforces Profile Scraper")
    print("=" * 50)
    
    # Create scraper instance with 1.5 second delay between requests
    scraper = IOICodeforcesScraper(delay=1.5)
    
    # You can modify these years as needed
    start_year = 2011
    end_year = 2025
    
    try:
        # Scrape all years - FULL MODE: All participants from all years
        scraper.scrape_all_years(start_year, end_year)  # Removed limit_participants parameter
        
        # Save results
        scraper.save_results_to_csv()
        scraper.save_results_to_json()
        scraper.save_results_to_excel()  # Add Excel output
        
        # Print summary
        scraper.print_summary()
        
    except KeyboardInterrupt:
        logger.info("Scraping interrupted by user")
        print("\nScraping interrupted. Saving partial results...")
        
        # Save whatever we have so far
        scraper.save_results_to_csv("ioi_codeforces_partial.csv")
        scraper.save_results_to_json("ioi_codeforces_partial.json")
        scraper.save_results_to_excel("ioi_codeforces_partial.xlsx")
        scraper.print_summary()
        
    except Exception as e:
        logger.error(f"Unexpected error in main: {e}")
        raise


if __name__ == "__main__":
    main()