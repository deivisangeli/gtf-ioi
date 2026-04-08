#!/usr/bin/env python3
"""
Complete Codeforces Data Extractor - IOI-Specific Timing

This script extracts Codeforces ratings 1 MONTH BEFORE each IOI competition,
rather than at year-end. This gives a more accurate picture of participants'
skill level at the time they competed.

Input: ioi_total.xlsx (with CF_Link column)
Output: ioi_total_rating.xlsx (complete data)

Author: Thais Takeuchi
Date: December 2025
"""

import sys
import pandas as pd
import json
import os
from datetime import datetime, timedelta
from pathlib import Path

sys.stdout.reconfigure(encoding='utf-8')

# ==================== CONFIGURATION ====================
# Resolve base directory from environment variables.
# Preferred: db_path (e.g. "C:/Users/YourName/Globtalent Dropbox")
# Fallback:  GT_PATH (e.g. "C:/Users/YourName") — appends "Globtalent Dropbox"
_db_path = os.getenv("db_path")
if _db_path:
    BASE_DIR = Path(_db_path.rstrip("/\\")) / "Codeforces" / "Data"
else:
    BASE_DIR = Path(os.getenv("GT_PATH", os.getcwd())) / "Globtalent Dropbox" / "Codeforces" / "Data"

# File paths
INPUT_EXCEL = BASE_DIR / "ioi_total.xlsx"
OUTPUT_EXCEL = BASE_DIR / "ioi_total_rating.xlsx"
USER_INFO_DIR = BASE_DIR / "user_info"
USER_RATING_DIR = BASE_DIR / "user_rating"

SHEET_NAME = "All Participants"

# Column name configuration
CF_LINK_COLUMN = "CF_Link"  # Column containing Codeforces profile links
USE_LOWERCASE_COLUMNS = True  # Set to True for lowercase column names

# Year range to extract ratings
START_YEAR = 2011  # Codeforces started in 2011
END_YEAR = 2025    # Current year

# How to handle missing data
MISSING_DATA_STRATEGY = "nan"   # Options: "zero", "nan", "previous"

# ==================== IOI DATES ====================
# IOI competition dates - we'll extract rating 1 MONTH BEFORE each event
IOI_DATES = {
    2011: datetime(2011, 7, 22),   # Pattaya, Thailand - July 22-29
    2012: datetime(2012, 9, 23),   # Sirmione, Italy - September 23-30
    2013: datetime(2013, 7, 6),    # Brisbane, Australia - July 6-13
    2014: datetime(2014, 7, 13),   # Taipei, Taiwan - July 13-20
    2015: datetime(2015, 7, 26),   # Almaty, Kazakhstan - July 26 - Aug 2
    2016: datetime(2016, 8, 12),   # Kazan, Russia - August 12-19
    2017: datetime(2017, 7, 28),   # Tehran, Iran - July 28 - Aug 4
    2018: datetime(2018, 9, 1),    # Tsukuba, Japan - September 1-8
    2019: datetime(2019, 8, 4),    # Baku, Azerbaijan - August 4-11
    2020: datetime(2020, 9, 13),   # Online (Singapore) - September 13-19
    2021: datetime(2021, 6, 19),   # Online (Singapore) - June 19-25
    2022: datetime(2022, 8, 7),    # Yogyakarta, Indonesia - August 7-15
    2023: datetime(2023, 8, 28),   # Szeged, Hungary - Aug 28 - Sep 4
    2024: datetime(2024, 9, 1),    # Alexandria, Egypt - September 1-8
    2025: datetime(2025, 7, 27),   # Sucre, Bolivia - July 27 - Aug 3
}

# Calculate rating extraction dates (1 month before IOI)
RATING_DATES = {}
for year, ioi_date in IOI_DATES.items():
    # Subtract approximately 30 days
    rating_date = ioi_date - timedelta(days=30)
    RATING_DATES[year] = rating_date

# =======================================================

def safe_filename(name: str) -> str:
    """Make filename safe by replacing invalid characters"""
    return "".join(ch if ch.isalnum() or ch in "-_." else "_" for ch in name)


def extract_handle_from_link(cf_link: str) -> str:
    """
    Extract Codeforces handle from profile link
    
    Args:
        cf_link: Codeforces profile URL (e.g., https://codeforces.com/profile/tourist)
        
    Returns:
        Handle string or None
    """
    if pd.isna(cf_link) or not cf_link:
        return None
    
    cf_link = str(cf_link).strip()
    
    # Extract handle from URL like: https://codeforces.com/profile/tourist
    if '/profile/' in cf_link:
        handle = cf_link.split('/profile/')[-1].split('/')[0].split('?')[0]
        return handle.strip() if handle else None
    
    return None


def timestamp_to_date(timestamp: int) -> str:
    """
    Convert Unix timestamp to readable date string (YYYY-MM-DD)
    
    Args:
        timestamp: Unix timestamp in seconds
        
    Returns:
        Date string in format YYYY-MM-DD
    """
    if timestamp is None or timestamp == 0:
        return None
    
    try:
        dt = datetime.fromtimestamp(timestamp)
        return dt.strftime('%Y-%m-%d')
    except Exception as e:
        print(f"[ERROR] Could not convert timestamp {timestamp}: {e}")
        return None


def read_user_info(handle: str, user_info_dir: Path) -> dict:
    """
    Read user_info JSON file for a given handle
    
    Args:
        handle: Codeforces handle (username)
        user_info_dir: Directory containing user_info JSON files
        
    Returns:
        Dictionary with user info, or None if file doesn't exist
    """
    json_file = user_info_dir / f"{safe_filename(handle)}.json"
    
    if not json_file.exists():
        return None
    
    try:
        with open(json_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
            
        if data.get('status') != 'OK':
            return None
        
        results = data.get('result', [])
        if results:
            return results[0]
        else:
            return None
            
    except Exception as e:
        print(f"[ERROR] Error reading user_info for {handle}: {e}")
        return None


def read_user_rating_history(handle: str, rating_dir: Path) -> list:
    """
    Read user_rating JSON file for a given handle
    
    Args:
        handle: Codeforces handle
        rating_dir: Directory containing user_rating JSON files
        
    Returns:
        List of rating changes, sorted by date (oldest first)
    """
    json_file = rating_dir / f"{safe_filename(handle)}.json"
    
    if not json_file.exists():
        return []
    
    try:
        with open(json_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        if data.get('status') != 'OK':
            return []
        
        rating_changes = data.get('result', [])
        rating_changes.sort(key=lambda x: x.get('ratingUpdateTimeSeconds', 0))
        
        return rating_changes
        
    except Exception as e:
        print(f"[ERROR] Error reading user_rating for {handle}: {e}")
        return []


def get_rating_before_ioi(rating_history: list, year: int, account_creation_year: int = None) -> int:
    """
    Get the rating 1 MONTH BEFORE the IOI competition for a specific year
    
    Args:
        rating_history: List of rating change events
        year: Year of IOI competition
        account_creation_year: Year when account was created
        
    Returns:
        Rating 1 month before IOI, or None if no data
    """
    # If account wasn't created yet, return None
    if account_creation_year and year < account_creation_year:
        return None
    
    # If year not in IOI_DATES, return None
    if year not in RATING_DATES:
        return None
    
    # Get the rating extraction date (1 month before IOI)
    rating_date = RATING_DATES[year]
    rating_timestamp = int(rating_date.timestamp())
    
    # Find the last rating change before or at the rating date
    last_rating = None
    
    for change in rating_history:
        change_time = change.get('ratingUpdateTimeSeconds', 0)
        
        if change_time > rating_timestamp:
            break
        
        last_rating = change.get('newRating')
    
    return last_rating


def process_single_handle(handle: str, user_info_dir: Path, rating_dir: Path, 
                         start_year: int, end_year: int) -> dict:
    """
    Process a single handle and extract all data
    
    Args:
        handle: Codeforces handle
        user_info_dir: Directory with user_info JSONs
        rating_dir: Directory with user_rating JSONs
        start_year: First year to extract ratings
        end_year: Last year to extract ratings
        
    Returns:
        Dictionary with all extracted data for this handle
    """
    # Define column names (will be lowercased if USE_LOWERCASE_COLUMNS is True)
    # NOTE: NOT including CF_Handle here - it will be kept as separate 'handle' column
    result = {
        'CF_Registration_Date': None,
        'CF_Registration_Year': None,
        'CF_Country': None,
        'CF_Current_Rating': None,
        'CF_Max_Rating': None,
        'CF_Current_Rank': None,
        'CF_Max_Rank': None,
        'CF_Contribution': None,
        'CF_Friend_Of_Count': None
    }
    
    # Extract user info
    user_info = read_user_info(handle, user_info_dir)
    
    if user_info:
        reg_timestamp = user_info.get('registrationTimeSeconds')
        reg_date = timestamp_to_date(reg_timestamp)
        
        if reg_date:
            result['CF_Registration_Date'] = reg_date
            result['CF_Registration_Year'] = int(reg_date.split('-')[0])
        
        result['CF_Country'] = user_info.get('country')
        result['CF_Current_Rating'] = user_info.get('rating')
        result['CF_Max_Rating'] = user_info.get('maxRating')
        result['CF_Current_Rank'] = user_info.get('rank')
        result['CF_Max_Rank'] = user_info.get('maxRank')
        result['CF_Contribution'] = user_info.get('contribution')
        result['CF_Friend_Of_Count'] = user_info.get('friendOfCount')
    
    # Extract rating history
    rating_history = read_user_rating_history(handle, rating_dir)
    account_creation_year = result['CF_Registration_Year']
    
    # Extract rating for each year (1 month before IOI)
    for year in range(start_year, end_year + 1):
        rating = get_rating_before_ioi(rating_history, year, account_creation_year)
        
        if rating is not None:
            result[f'Rating_{year}'] = rating
        else:
            if MISSING_DATA_STRATEGY == "zero":
                result[f'Rating_{year}'] = 0
            elif MISSING_DATA_STRATEGY == "nan":
                result[f'Rating_{year}'] = None
            elif MISSING_DATA_STRATEGY == "previous":
                if year > start_year:
                    prev_rating = result.get(f'Rating_{year-1}')
                    result[f'Rating_{year}'] = prev_rating if prev_rating else 0
                else:
                    result[f'Rating_{year}'] = 0
    
    # Convert column names to lowercase if configured
    if USE_LOWERCASE_COLUMNS:
        result = {k.lower(): v for k, v in result.items()}
    
    return result


def calculate_rating_statistics(df: pd.DataFrame, start_year: int, end_year: int) -> pd.DataFrame:
    """
    Calculate additional rating statistics
    
    Args:
        df: DataFrame with rating columns
        start_year: First year
        end_year: Last year
        
    Returns:
        DataFrame with additional statistics columns
    """
    # Determine column prefix based on lowercase setting
    rating_prefix = 'rating_' if USE_LOWERCASE_COLUMNS else 'Rating_'
    
    rating_cols = [f'{rating_prefix}{year}' for year in range(start_year, end_year + 1)]
    
    # Define new column names
    max_rating_col = 'cf_max_rating_ever' if USE_LOWERCASE_COLUMNS else 'CF_Max_Rating_Ever'
    year_max_col = 'cf_year_max_rating' if USE_LOWERCASE_COLUMNS else 'CF_Year_Max_Rating'
    first_active_col = 'cf_first_year_active' if USE_LOWERCASE_COLUMNS else 'CF_First_Year_Active'
    last_active_col = 'cf_last_year_active' if USE_LOWERCASE_COLUMNS else 'CF_Last_Year_Active'
    years_active_col = 'cf_years_active' if USE_LOWERCASE_COLUMNS else 'CF_Years_Active'
    
    # Maximum rating ever achieved
    df[max_rating_col] = df[rating_cols].max(axis=1)
    
    # Year of maximum rating
    df[year_max_col] = df[rating_cols].idxmax(axis=1).str.replace(rating_prefix, '')
    
    # First year with rating > 0
    df[first_active_col] = None
    for idx, row in df.iterrows():
        for year in range(start_year, end_year + 1):
            col = f'{rating_prefix}{year}'
            if pd.notna(row[col]) and row[col] > 0:
                df.at[idx, first_active_col] = year
                break
    
    # Last year with rating > 0
    df[last_active_col] = None
    for idx, row in df.iterrows():
        for year in range(end_year, start_year - 1, -1):
            col = f'{rating_prefix}{year}'
            if pd.notna(row[col]) and row[col] > 0:
                df.at[idx, last_active_col] = year
                break
    
    # Years active
    df[years_active_col] = df[last_active_col] - df[first_active_col]
    df[years_active_col] = df[years_active_col].fillna(0)
    
    return df


def calculate_ioi_timing_statistics(df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate years between CF registration and FIRST IOI participation
    
    For contestants who participated multiple times, uses their FIRST (minimum) year.
    Example: Frederick Ivan Tan participated in 2021 and 2022:
        - Both rows show: 2021 - registration_year
    
    Args:
        df: DataFrame with year and cf_registration_year columns
        
    Returns:
        DataFrame with first_ioi_year and cf_years_before_first_ioi columns
    """
    # Define column names based on lowercase setting
    year_col = 'year' if USE_LOWERCASE_COLUMNS else 'Year'
    cf_reg_year_col = 'cf_registration_year' if USE_LOWERCASE_COLUMNS else 'CF_Registration_Year'
    handle_col = 'handle' if USE_LOWERCASE_COLUMNS else 'Handle'
    
    # New columns to create
    first_ioi_col = 'first_ioi_year' if USE_LOWERCASE_COLUMNS else 'First_IOI_Year'
    years_before_col = 'cf_years_before_first_ioi' if USE_LOWERCASE_COLUMNS else 'CF_Years_Before_First_IOI'
    
    # For each unique handle, find their FIRST (minimum) IOI year
    # This value will be the same for all rows of the same person
    df[first_ioi_col] = df.groupby(handle_col)[year_col].transform('min')
    
    # Calculate: first_ioi_year - registration_year
    # Positive = registered BEFORE first IOI (e.g., 2 means registered 2 years before)
    # Negative = registered AFTER first IOI (e.g., -1 means registered 1 year after)
    df[years_before_col] = df[first_ioi_col] - df[cf_reg_year_col]
    
    return df


def get_row_specific_data(handle: str, ioi_year: int, user_info_dir: Path, rating_dir: Path) -> dict:
    """
    For a specific contestant-year row, compute:
      - cf_rating_reason: why the pre-IOI rating is missing (None when a valid rating exists)
          "no_data_cached"          handle exists but rating JSON not downloaded
          "no_contests_ever"        JSON exists; account never competed in a rated contest
          "registered_after_ioi"    account created after the IOI date for this year
          "no_rating_before_cutoff" had contests, but none before the 1-month cutoff date
      - cf_contests_year: number of rated CF contests in the IOI calendar year (Jan–Dec)
    """
    safe = safe_filename(handle)
    rating_file = rating_dir / f"{safe}.json"
    info_file = user_info_dir / f"{safe}.json"

    if not rating_file.exists():
        return {"cf_rating_reason": "no_data_cached", "cf_contests_year": None}

    try:
        with open(rating_file, 'r', encoding='utf-8') as f:
            data = json.load(f)
    except Exception:
        return {"cf_rating_reason": "no_data_cached", "cf_contests_year": None}

    if data.get("status") != "OK":
        return {"cf_rating_reason": "no_data_cached", "cf_contests_year": None}

    changes = sorted(data.get("result", []), key=lambda x: x.get("ratingUpdateTimeSeconds", 0))

    # Count contests in the IOI calendar year
    year_start_ts = int(datetime(ioi_year, 1, 1).timestamp())
    year_end_ts   = int(datetime(ioi_year + 1, 1, 1).timestamp())
    contests_in_year = sum(
        1 for c in changes
        if year_start_ts <= c.get("ratingUpdateTimeSeconds", 0) < year_end_ts
    )

    if len(changes) == 0:
        return {"cf_rating_reason": "no_contests_ever", "cf_contests_year": 0}

    # Check if account was created after the IOI date
    if ioi_year in IOI_DATES and info_file.exists():
        try:
            with open(info_file, 'r', encoding='utf-8') as f:
                info_data = json.load(f)
            if info_data.get("status") == "OK" and info_data.get("result"):
                reg_ts  = info_data["result"][0].get("registrationTimeSeconds", 0)
                ioi_ts  = int(IOI_DATES[ioi_year].timestamp())
                if reg_ts > ioi_ts:
                    return {"cf_rating_reason": "registered_after_ioi", "cf_contests_year": contests_in_year}
        except Exception:
            pass

    # Check whether any contest falls before the 1-month cutoff
    if ioi_year in RATING_DATES:
        cutoff_ts = int(RATING_DATES[ioi_year].timestamp())
        has_rating_before_cutoff = any(
            c.get("ratingUpdateTimeSeconds", 0) <= cutoff_ts for c in changes
        )
        if not has_rating_before_cutoff:
            return {"cf_rating_reason": "no_rating_before_cutoff", "cf_contests_year": contests_in_year}

    return {"cf_rating_reason": None, "cf_contests_year": contests_in_year}


def main():
    """
    Main function to extract all Codeforces data
    """
    print("=" * 70)
    print("Complete Codeforces Data Extractor - IOI-Specific Timing")
    print("=" * 70)
    print(f"\nConfiguration:")
    print(f"  Base Directory: {BASE_DIR}")
    print(f"  Environment Variable GT_PATH: {os.getenv('GT_PATH', 'Not set (using current directory)')}")
    print(f"  Input File: {INPUT_EXCEL.name}")
    print(f"  Output File: {OUTPUT_EXCEL.name}")
    print(f"\n⚠️  IMPORTANT: Ratings extracted 1 MONTH BEFORE each IOI competition")
    print(f"  (Not at year-end, but at actual competition timing)")
    print(f"\nRating extraction dates:")
    for year in sorted(RATING_DATES.keys()):
        ioi_date = IOI_DATES[year]
        rating_date = RATING_DATES[year]
        print(f"  {year}: {rating_date.strftime('%B %d, %Y')} (IOI: {ioi_date.strftime('%B %d-%d')})")
    print(f"\nThis script will extract:")
    print("  1. Registration dates and user info from user_info/")
    print("  2. Rating 1 month before IOI from user_rating/")
    print(f"  3. Year range: {START_YEAR} - {END_YEAR}")
    print(f"  4. Column style: {'Lowercase' if USE_LOWERCASE_COLUMNS else 'Mixed case'}")
    print("=" * 70)
    
    # Check directories
    if not USER_INFO_DIR.exists():
        print(f"\n[ERROR] Directory not found: {USER_INFO_DIR}")
        return
    
    if not USER_RATING_DIR.exists():
        print(f"\n[ERROR] Directory not found: {USER_RATING_DIR}")
        return
    
    # Check input Excel
    if not INPUT_EXCEL.exists():
        print(f"\n[ERROR] Excel file not found: {INPUT_EXCEL}")
        return
    
    # Read Excel file
    print(f"\nReading Excel file: {INPUT_EXCEL}")
    try:
        df = pd.read_excel(INPUT_EXCEL, sheet_name=SHEET_NAME)
        print(f"✓ Loaded {len(df)} rows")
        
        # 🔧 CRITICAL FIX: Convert existing column names to lowercase if configured
        if USE_LOWERCASE_COLUMNS:
            df.columns = df.columns.str.lower()
            print(f"✓ Converted column names to lowercase")
        
    except Exception as e:
        print(f"[ERROR] Could not read Excel file: {e}")
        return
    
    # Check CF_Link column (now lowercase if USE_LOWERCASE_COLUMNS is True)
    cf_link_col = CF_LINK_COLUMN.lower() if USE_LOWERCASE_COLUMNS else CF_LINK_COLUMN
    
    if cf_link_col not in df.columns:
        print(f"\n[ERROR] Column '{cf_link_col}' not found")
        print(f"Available columns: {list(df.columns)}")
        return
    
    # Extract handles from CF_Link column (KEEP THIS COLUMN)
    print("\nExtracting handles from CF_Link column...")
    handle_col = 'handle' if USE_LOWERCASE_COLUMNS else 'Handle'
    df[handle_col] = df[cf_link_col].apply(extract_handle_from_link)
    
    valid_handles = df[handle_col].notna().sum()
    print(f"✓ Found {valid_handles} valid Codeforces handles\n")
    
    # Process all handles
    print("Processing handles...")
    print("=" * 70)
    
    total = len(df)
    processed = 0
    found_info = 0
    found_rating = 0

    reason_col   = 'cf_rating_reason' if USE_LOWERCASE_COLUMNS else 'CF_Rating_Reason'
    contests_col = 'cf_contests_year'  if USE_LOWERCASE_COLUMNS else 'CF_Contests_Year'
    year_col     = 'year'              if USE_LOWERCASE_COLUMNS else 'Year'

    for idx, row in df.iterrows():
        handle = row[handle_col]

        # No handle: mark reason and move on (no CF data to extract)
        if pd.isna(handle) or str(handle).strip() == "":
            df.at[idx, reason_col]   = "no_handle"
            df.at[idx, contests_col] = None
            processed += 1
            continue

        handle   = str(handle).strip()
        ioi_year = int(row[year_col])

        # Extract all-year CF profile and rating data
        data = process_single_handle(handle, USER_INFO_DIR, USER_RATING_DIR,
                                     START_YEAR, END_YEAR)

        # Add data to DataFrame
        for key, value in data.items():
            if key not in df.columns:
                df[key] = None
            df.at[idx, key] = value

        # Compute row-specific columns (reason + contests in this IOI year)
        row_data = get_row_specific_data(handle, ioi_year, USER_INFO_DIR, USER_RATING_DIR)
        df.at[idx, reason_col]   = row_data["cf_rating_reason"]
        df.at[idx, contests_col] = row_data["cf_contests_year"]

        # Count successes
        reg_date_col = 'cf_registration_date' if USE_LOWERCASE_COLUMNS else 'CF_Registration_Date'
        if data.get(reg_date_col) is not None:
            found_info += 1

        rating_prefix = 'rating_' if USE_LOWERCASE_COLUMNS else 'Rating_'
        has_rating = any(data.get(f'{rating_prefix}{year}') not in (None, 0)
                         for year in range(START_YEAR, END_YEAR + 1))
        if has_rating:
            found_rating += 1

        processed += 1

        # Print progress every 100 handles
        if processed % 100 == 0 or processed == total:
            print(f"[{processed}/{total}] Processed: {handle}", flush=True)
    
    print("=" * 70)
    print(f"\nProcessing Summary:")
    print(f"  Total handles processed: {processed}")
    print(f"  Handles with user_info: {found_info}")
    print(f"  Handles with ratings: {found_rating}")
    
    # Calculate statistics
    print("\nCalculating rating statistics...")
    df = calculate_rating_statistics(df, START_YEAR, END_YEAR)
    
    # Calculate IOI timing statistics (uses FIRST year of participation)
    print("Calculating IOI timing statistics...")
    df = calculate_ioi_timing_statistics(df)
    
    # Save to Excel (handle column is KEPT)
    print(f"\nSaving results to: {OUTPUT_EXCEL}")
    try:
        df.to_excel(OUTPUT_EXCEL, index=False, sheet_name=SHEET_NAME)
        print(f"✓ Successfully saved to {OUTPUT_EXCEL}")
    except Exception as e:
        print(f"[ERROR] Could not save Excel file: {e}")
        return
    
    # Print sample
    print("\n" + "=" * 70)
    print("Sample of extracted data:")
    print("=" * 70)
    
    # Adjust sample column names based on lowercase setting (WITH handle column)
    if USE_LOWERCASE_COLUMNS:
        sample_cols = ['year', 'contestant', 'country', handle_col,
                       'cf_registration_year', 'cf_current_rating', 'cf_max_rating',
                       'rating_2023', 'rating_2024', 'rating_2025',
                       'cf_max_rating_ever', 'cf_years_active',
                       'first_ioi_year', 'cf_years_before_first_ioi']
    else:
        sample_cols = ['Year', 'Contestant', 'Country', handle_col,
                       'CF_Registration_Year', 'CF_Current_Rating', 'CF_Max_Rating',
                       'Rating_2023', 'Rating_2024', 'Rating_2025',
                       'CF_Max_Rating_Ever', 'CF_Years_Active',
                       'First_IOI_Year', 'CF_Years_Before_First_IOI']
    
    available_cols = [col for col in sample_cols if col in df.columns]
    print(df[available_cols].head(10).to_string())
    
    print("\n" + "=" * 70)
    print("✅ Done! Check the output file for complete results.")
    print(f"\n⚠️  IMPORTANT CHANGES:")
    print(f"  • Ratings extracted 1 MONTH BEFORE each IOI competition")
    print(f"  • For repeat participants, ALL rows use FIRST IOI year")
    print(f"    Example: Frederick (2021, 2022) → both rows use 2021 - reg_year")
    print(f"\nColumns created:")
    print(f"  - User Info: Registration date, country, current/max rating, rank, etc.")
    
    if USE_LOWERCASE_COLUMNS:
        print(f"  - Handle: handle (from CF_Link)")
        print(f"  - Rating before IOI: rating_{START_YEAR} through rating_{END_YEAR}")
        print(f"  - Statistics: cf_max_rating_ever, cf_years_active, etc.")
        print(f"  - IOI Timing: first_ioi_year, cf_years_before_first_ioi")
    else:
        print(f"  - Handle: Handle (from CF_Link)")
        print(f"  - Rating before IOI: Rating_{START_YEAR} through Rating_{END_YEAR}")
        print(f"  - Statistics: CF_Max_Rating_Ever, CF_Years_Active, etc.")
        print(f"  - IOI Timing: First_IOI_Year, CF_Years_Before_First_IOI")
    
    print(f"  - Column style: {'Lowercase' if USE_LOWERCASE_COLUMNS else 'Mixed case'}")
    print("=" * 70)


if __name__ == "__main__":
    main()