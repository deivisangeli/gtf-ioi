#!/usr/bin/env python3
"""
Complete Codeforces Data Extractor

This script combines user_info and user_rating extraction into a single workflow.
It extracts registration dates, current stats, and rating history for each year.

Input: IOI_2011_2025.xlsx (with CF_Link column)
Output: IOI_2011_2025_with_cf_ratings.xlsx (complete data)

Author: Thais Takeuchi
Date: November 2025
"""

import pandas as pd
import json
import os
from datetime import datetime
from pathlib import Path

# ==================== CONFIGURATION ====================
# Define base directory from environment variable or fallback
# Set GT_PATH environment variable to your base path (e.g., C:\Users\YourName)
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Dropbox" / "GTAllocation" / "Data" / "IOI"

# File paths
INPUT_EXCEL = BASE_DIR / "IOI_2011_2025.xlsx"
OUTPUT_EXCEL = BASE_DIR / "IOI_2011_2025_with_cf_ratings.xlsx"
USER_INFO_DIR = BASE_DIR / "user_info"
USER_RATING_DIR = BASE_DIR / "user_rating"

SHEET_NAME = "Sheet1"

# Column name configuration
CF_LINK_COLUMN = "CF_Link"  # Column containing Codeforces profile links
USE_LOWERCASE_COLUMNS = True  # Set to True for lowercase column names

# Year range to extract ratings
START_YEAR = 2011  # Codeforces started in 2011
END_YEAR = 2025    # Current year

# How to handle missing data
MISSING_DATA_STRATEGY = "zero"  # Options: "zero", "nan", "previous"

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


def get_rating_at_year_end(rating_history: list, year: int, account_creation_year: int = None) -> int:
    """
    Get the rating at the end of a specific year (December 31, 23:59:59)
    
    Args:
        rating_history: List of rating change events
        year: Year to get rating for
        account_creation_year: Year when account was created
        
    Returns:
        Rating at end of year, or None if no data
    """
    # If account wasn't created yet, return None
    if account_creation_year and year < account_creation_year:
        return None
    
    # End of year timestamp (Dec 31, 23:59:59)
    year_end = datetime(year, 12, 31, 23, 59, 59)
    year_end_timestamp = int(year_end.timestamp())
    
    # Find the last rating change before or at year end
    last_rating = None
    
    for change in rating_history:
        change_time = change.get('ratingUpdateTimeSeconds', 0)
        
        if change_time > year_end_timestamp:
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
    result = {
        'CF_Handle': handle,
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
    
    # Extract rating for each year
    for year in range(start_year, end_year + 1):
        rating = get_rating_at_year_end(rating_history, year, account_creation_year)
        
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


def main():
    """
    Main function to extract all Codeforces data
    """
    print("=" * 70)
    print("Complete Codeforces Data Extractor")
    print("=" * 70)
    print(f"\nConfiguration:")
    print(f"  Base Directory: {BASE_DIR}")
    print(f"  Environment Variable GT_PATH: {os.getenv('GT_PATH', 'Not set (using current directory)')}")
    print(f"  Input File: {INPUT_EXCEL.name}")
    print(f"  Output File: {OUTPUT_EXCEL.name}")
    print(f"\nThis script will extract:")
    print("  1. Registration dates and user info from user_info/")
    print("  2. Rating history by year from user_rating/")
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
    except Exception as e:
        print(f"[ERROR] Could not read Excel file: {e}")
        return
    
    # Check CF_Link column
    if CF_LINK_COLUMN not in df.columns:
        print(f"\n[ERROR] Column '{CF_LINK_COLUMN}' not found")
        print(f"Available columns: {list(df.columns)}")
        return
    
    # Extract handles from CF_Link column
    print("\nExtracting handles from CF_Link column...")
    handle_col = 'Handle' if not USE_LOWERCASE_COLUMNS else 'handle'
    df[handle_col] = df[CF_LINK_COLUMN].apply(extract_handle_from_link)
    
    valid_handles = df[handle_col].notna().sum()
    print(f"✓ Found {valid_handles} valid Codeforces handles\n")
    
    # Process all handles
    print("Processing handles...")
    print("=" * 70)
    
    total = len(df)
    processed = 0
    found_info = 0
    found_rating = 0
    
    for idx, row in df.iterrows():
        handle = row[handle_col]
        
        # Skip empty handles
        if pd.isna(handle) or str(handle).strip() == "":
            processed += 1
            continue
        
        handle = str(handle).strip()
        
        # Process this handle
        data = process_single_handle(handle, USER_INFO_DIR, USER_RATING_DIR, 
                                     START_YEAR, END_YEAR)
        
        # Add data to DataFrame
        for key, value in data.items():
            if key not in df.columns:
                df[key] = None
            df.at[idx, key] = value
        
        # Count successes
        reg_date_col = 'cf_registration_date' if USE_LOWERCASE_COLUMNS else 'CF_Registration_Date'
        if data.get(reg_date_col) is not None:
            found_info += 1
        
        rating_prefix = 'rating_' if USE_LOWERCASE_COLUMNS else 'Rating_'
        has_rating = any(data.get(f'{rating_prefix}{year}', 0) > 0 for year in range(START_YEAR, END_YEAR + 1))
        if has_rating:
            found_rating += 1
        
        processed += 1
        
        # Print progress every 100 handles
        if processed % 100 == 0 or processed == total:
            print(f"[{processed}/{total}] Processed: {handle}")
    
    print("=" * 70)
    print(f"\nProcessing Summary:")
    print(f"  Total handles processed: {processed}")
    print(f"  Handles with user_info: {found_info}")
    print(f"  Handles with ratings: {found_rating}")
    
    # Calculate statistics
    print("\nCalculating rating statistics...")
    df = calculate_rating_statistics(df, START_YEAR, END_YEAR)
    
    # Save to Excel
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
    
    # Adjust sample column names based on lowercase setting
    if USE_LOWERCASE_COLUMNS:
        sample_cols = ['year', 'contestant', 'country', handle_col,
                       'cf_registration_year', 'cf_current_rating', 'cf_max_rating',
                       'rating_2023', 'rating_2024', 'rating_2025',
                       'cf_max_rating_ever', 'cf_years_active']
    else:
        sample_cols = ['Year', 'Contestant', 'Country', handle_col,
                       'CF_Registration_Year', 'CF_Current_Rating', 'CF_Max_Rating',
                       'Rating_2023', 'Rating_2024', 'Rating_2025',
                       'CF_Max_Rating_Ever', 'CF_Years_Active']
    
    available_cols = [col for col in sample_cols if col in df.columns]
    print(df[available_cols].head(10).to_string())
    
    print("\n" + "=" * 70)
    print("✅ Done! Check the output file for complete results.")
    print(f"\nColumns created:")
    print(f"  - User Info: Registration date, country, current/max rating, rank, etc.")
    
    if USE_LOWERCASE_COLUMNS:
        print(f"  - Rating by Year: rating_{START_YEAR} through rating_{END_YEAR}")
    else:
        print(f"  - Rating by Year: Rating_{START_YEAR} through Rating_{END_YEAR}")
    
    print(f"  - Statistics: Max rating ever, years active, etc.")
    print(f"  - Column style: {'Lowercase' if USE_LOWERCASE_COLUMNS else 'Mixed case'}")
    print("=" * 70)


if __name__ == "__main__":
    main()