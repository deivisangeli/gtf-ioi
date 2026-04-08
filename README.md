# gtf-ioi

Data pipeline and analysis for the Global Talent Lab's IOI project. Combines International Olympiad in Informatics (IOI) results with Codeforces competitive programming activity to study talent identification, training dynamics, and the effect of online practice platforms on country-level performance.

## Research Questions

**1. Codeforces as a predictor of IOI success**  
How well does a contestant's Codeforces rating (measured one month before the IOI) predict their score and medal outcome? Does predictive power vary by year, country, or medal tier?

**2. Training dynamics and catch-up**  
How long does it take to prepare for the IOI? Using CF registration dates and first IOI participation, we can measure the training window for each contestant and ask whether late starters can catch up — i.e., whether the relationship between training time and performance is concave, and whether raw talent (as proxied by CF rating trajectory) substitutes for time.

**3. Causal effect of Codeforces on country performance**  
Codeforces adoption across countries was uneven and temporally staggered. This variation can be used to estimate the causal effect of access to online competitive programming on a country's IOI medal count, using a staggered difference-in-differences or event-study design around the year a country's contestants begin appearing on Codeforces in meaningful numbers.

## Data Pipeline

```
Web scraping (CPHOF + IOI Stats)
        ↓
  ioi_total.xlsx          ~5,400 participant-year obs, 2011–2025
        ↓
Extract CF handles → query Codeforces API (pre-downloaded JSON)
        ↓
  ioi_total_rating.xlsx   + CF rating 1 month before each IOI,
                            registration date, max rating, years active
        ↓
Handle cleaning           resolve outdated/duplicate CF handles
        ↓
  ioi_zero_rating.xlsx    cleaned
        ↓
Analysis & visualization
```

Data lives in Dropbox at `$db_path/Globtalent Dropbox/Codeforces/Data/`.  
Raw Codeforces API responses (one JSON per user) are stored in `Data/user_info/` and `Data/user_rating/`.

## Scripts

| File | Description |
|---|---|
| `0_scrap_ioi_cphof.py` | Scrapes IOI results from CPHOF and IOI Stats; outputs `ioi_total.xlsx` |
| `1_datacollection.py` | Merges CF data (from JSON files) into IOI dataset; outputs `ioi_total_rating.xlsx` |
| `0_cleaning.R` | Resolves ~91 outdated/duplicate CF handles; outputs `ioi_zero_rating.xlsx` |
| `0_stats.R` | Descriptive statistics and score-vs-rating visualizations |

## Setup

Set two environment variables (add to your shell profile or run once as admin):

```
setx db_path "C:/Users/YOUR_NAME/Dropbox"
setx gtl_path "C:/Users/YOUR_NAME/github/gtl"
```

The scripts resolve data paths as:
```python
BASE_DIR = Path(os.getenv("GT_PATH")) / "Globtalent Dropbox" / "Codeforces" / "Data"
```

R scripts use `Sys.getenv("db_path")` and `Sys.getenv("gtl_path")`.

## Key Variables

| Variable | Description |
|---|---|
| `year` | IOI competition year |
| `contestant` | Participant name |
| `country` | Represented country |
| `result` | Medal outcome (Gold / Silver / Bronze / No Award) |
| `score` | IOI total score |
| `handle` | Codeforces username |
| `rating_{year}` | CF rating 1 month before the IOI in that year |
| `cf_registration_year` | Year the CF account was created |
| `cf_max_rating` | Peak CF rating ever |
| `cf_years_before_first_ioi` | Years between CF registration and first IOI (training window) |
| `first_ioi_year` | First year of IOI participation |

## Contributors

- Thais Takeuchi ([@takeuchigt](https://github.com/takeuchigt))
- Deivis Angeli ([@deivisangeli](https://github.com/deivisangeli))
