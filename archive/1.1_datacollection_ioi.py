import pandas as pd
import os
import json
import time
import requests
from pathlib import Path
from urllib.parse import quote

# ==================== CONFIGURATION ====================
# Define base directory from environment variable or fallback
# Set GT_PATH environment variable to your base path (e.g., C:\Users\YourName)
base_dir = os.getenv("GT_PATH", os.getcwd())
BASE_DIR = Path(base_dir) / "Globtalent Dropbox" / "Codeforces" / "Data"
INPUT_FILE = BASE_DIR / "ioi_total.xlsx"
SHEET_NAME = "All Participants"
REQUEST_TIMEOUT = 15
SLEEP_BETWEEN = 0.3
OVERWRITE = False

DIR_INFO = BASE_DIR / "user_info"
DIR_RATING = BASE_DIR / "user_rating"
DIR_BLOGS = BASE_DIR / "user_blogEntries"

# Create directories if they don't exist
BASE_DIR.mkdir(parents=True, exist_ok=True)
DIR_INFO.mkdir(parents=True, exist_ok=True)
DIR_RATING.mkdir(parents=True, exist_ok=True)
DIR_BLOGS.mkdir(parents=True, exist_ok=True)
# =======================================================

print("=" * 70)
print("Codeforces Data Collector")
print("=" * 70)
print(f"\nConfiguration:")
print(f"  Base Directory: {BASE_DIR}")
print(f"  Environment Variable GT_PATH: {os.getenv('GT_PATH', 'Not set (using current directory)')}")
print(f"  Input File: {INPUT_FILE.name}")
print(f"  Output Directories:")
print(f"    - User Info: {DIR_INFO}")
print(f"    - User Rating: {DIR_RATING}")
print(f"    - User Blogs: {DIR_BLOGS}")
print("=" * 70)

# Load Excel file and extract handles
df = pd.read_excel(INPUT_FILE, sheet_name=SHEET_NAME)
df["Handle"] = df["CF_Link"].str.extract(r'profile/([^/]+)')
df.to_excel(INPUT_FILE, index=False)

handles = (
    df["Handle"]
    .dropna()
    .astype(str)
    .str.strip()
    .replace("", pd.NA)
    .dropna()
    .unique()
)

def safe_filename(name: str) -> str:
    """Make the file names safe, i.e. replaces any invalid character by an underscore"""
    return "".join(ch if ch.isalnum() or ch in "-_." else "_" for ch in name)


def fetch_and_save(session, url: str, out_file: Path, handle: str):
    """Downloads the JSON from the API, if it returns an error, just skips it"""

    if out_file.exists() and not OVERWRITE:
        print(f"[SKIP] {handle} -> already exists {out_file.name}")
        return

    try:
        resp = session.get(url, timeout=REQUEST_TIMEOUT)
        resp.raise_for_status()
        data = resp.json()

        if data.get("status") == "OK":
            with open(out_file, "w", encoding="utf-8") as f:
                json.dump(data, f, ensure_ascii=False, indent=2)
            print(f"[OK]   {handle} -> {out_file}")
        else:
            print(f"[FAIL] {handle} -> estado={data.get('status')}, not saved")

    except requests.RequestException as e:
        print(f"[NETERR] {handle} -> {e}, not saved")

session = requests.Session()
session.headers.update({"User-Agent": "CodeforcesDataCollector/1.0 (+python)"})

for h in handles:
    handle = h.strip()
    if not handle:
        continue

    # user.info
    url_info = f"https://codeforces.com/api/user.info?handles={quote(handle)}&checkHistoricHandles=false"
    fetch_and_save(session, url_info, DIR_INFO / f"{safe_filename(handle)}.json", handle)

    # user.rating
    url_rating = f"https://codeforces.com/api/user.rating?handle={quote(handle)}"
    fetch_and_save(session, url_rating, DIR_RATING / f"{safe_filename(handle)}.json", handle)

    # user.blogEntries
    url_blogs = f"https://codeforces.com/api/user.blogEntries?handle={quote(handle)}"
    fetch_and_save(session, url_blogs, DIR_BLOGS / f"{safe_filename(handle)}.json", handle)

    time.sleep(SLEEP_BETWEEN)

print("All done!")
