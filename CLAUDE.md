# CLAUDE.md — gtf-ioi

## Project overview

IOI + Codeforces data project. Three research angles:
1. CF rating as predictor of IOI score/medal (individual level)
2. Training dynamics: `cf_years_before_first_ioi` as training window; can late starters catch up?
3. Causal effect of Codeforces on country IOI performance via staggered adoption DiD

## Data

All data lives in Dropbox, not in this repo. Resolved via environment variables:
- `db_path` — user's Dropbox root (e.g. `C:/Users/deivi/Dropbox`)
- `gtl_path` — path to this GitHub repo
- `GT_PATH` — used in Python scripts; same as `db_path` (Dropbox root)

Key data path: `$db_path/Globtalent Dropbox/Codeforces/Data/`

Key files:
- `ioi_total.xlsx` — raw IOI results with CF links, ~5,400 participant-year rows, 2011–2025
- `ioi_total_rating.xlsx` — above merged with CF data (main analysis file)
- `ioi_zero_rating.xlsx` — cleaned version after handle deduplication
- `user_info/*.json` — Codeforces API user profiles, one JSON per handle
- `user_rating/*.json` — Codeforces API rating histories, one JSON per handle

## Key variables

- `rating_{year}` — CF rating extracted 1 month before the IOI competition in that year (not year-end)
- `cf_years_before_first_ioi` — training window: `first_ioi_year - cf_registration_year`
  - Positive = registered CF before first IOI
  - Negative = registered after (retroactive account)
- `first_ioi_year` — minimum IOI year for that contestant (same for all rows of a repeat participant)
- `result` / `result_unified` — medal: Gold / Silver / Bronze / No Award (raw has variants like "Gold*")

## Conventions

- All column names are lowercase (configured via `USE_LOWERCASE_COLUMNS = True` in `1_datacollection.py`)
- Rating columns named `rating_2011` through `rating_2025`
- `score = 0` rows exist — filter with `score > 0` for analysis
- Repeat participants (same person, multiple IOI years) appear as multiple rows; `first_ioi_year` is constant per person
- ~91 handles required remapping in `0_cleaning.R` (old/abandoned accounts)

## Pipeline order

```
0_scrap_ioi_cphof.py  →  1_datacollection.py  →  0_cleaning.R  →  0_stats.R
```

Only re-run `0_scrap_ioi_cphof.py` when adding a new IOI year. Scraper auto-detects the last year already in the file and only fetches missing years.

## Research notes

**Predictor analysis**: Use `rating_{year}` matched to the contestant's actual IOI year (not all years). The relevant rating for a 2019 participant is `rating_2019`.

**Training dynamics**: `cf_years_before_first_ioi` is the main independent variable. Watch for selection: high-ability contestants may have registered on CF earlier precisely because they are high ability. Consider using CF rating trajectory (slope) alongside level.

**Country-level DiD**: Unit of analysis shifts to country-year. "Adoption" could be defined as the first year a country has ≥N contestants with CF accounts, or when median CF participation rate crosses a threshold. The outcome is medals or total score per country-year. Codeforces launched in 2011, so pre-period is limited; focus on within-country variation in uptake speed.
