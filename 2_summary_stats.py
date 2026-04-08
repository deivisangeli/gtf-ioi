#!/usr/bin/env python3
"""
Summary statistics by IOI year, output as a LaTeX table.

Variables (participant-year level):
  has_handle           1 if a CF profile link was found in CPHOF/IOI Stats
  active_before_ioi    1 if participant had a valid CF rating before the IOI
                         (cf_rating_reason is NaN, i.e., at least one rated
                          contest before the 1-month pre-IOI cutoff)
  months_on_cf         Months between CF registration and IOI date
                         (conditional on has_handle == 1)

Summary statistics reported: mean, SD, min, max — by year.
"""

import sys
import os
import numpy as np
import pandas as pd
from pathlib import Path
from datetime import datetime

sys.stdout.reconfigure(encoding='utf-8')

# ── paths ────────────────────────────────────────────────────────────────────
_db_path = os.getenv("db_path")
if _db_path:
    BASE_DIR = Path(_db_path.rstrip("/\\")) / "Codeforces" / "Data"
else:
    BASE_DIR = Path(os.getenv("GT_PATH", os.getcwd())) / "Globtalent Dropbox" / "Codeforces" / "Data"

OUTPUT_TEX = Path(__file__).parent / "output" / "tab_summary_by_year.tex"
OUTPUT_TEX.parent.mkdir(exist_ok=True)

# Overleaf mirror: gtl-allocation/overleaf/GTAllocation/tables/ioi/
_gtl_path = os.getenv("gtl_path")
OVERLEAF_TEX = (
    Path(_gtl_path).parent / "gtl-allocation" / "overleaf" / "GTAllocation" / "tables" / "ioi" / "cf_summary_by_year.tex"
    if _gtl_path else None
)

# ── IOI dates (start of competition) ─────────────────────────────────────────
IOI_DATES = {
    2011: datetime(2011, 7, 22),
    2012: datetime(2012, 9, 23),
    2013: datetime(2013, 7,  6),
    2014: datetime(2014, 7, 13),
    2015: datetime(2015, 7, 26),
    2016: datetime(2016, 8, 12),
    2017: datetime(2017, 7, 28),
    2018: datetime(2018, 9,  1),
    2019: datetime(2019, 8,  4),
    2020: datetime(2020, 9, 13),
    2021: datetime(2021, 6, 19),
    2022: datetime(2022, 8,  7),
    2023: datetime(2023, 8, 28),
    2024: datetime(2024, 9,  1),
    2025: datetime(2025, 7, 27),
}

# ── load data ─────────────────────────────────────────────────────────────────
print(f"Loading {BASE_DIR / 'ioi_total_rating.xlsx'} ...")
df = pd.read_excel(BASE_DIR / "ioi_total_rating.xlsx")
print(f"  {len(df)} rows loaded")

# ── construct variables ───────────────────────────────────────────────────────
df["has_handle"] = df["handle"].notna().astype(int)

df["active_before_ioi"] = (
    df["has_handle"].eq(1) & df["cf_rating_reason"].isna()
).astype(int)

df["cf_reg_dt"] = pd.to_datetime(df["cf_registration_date"], errors="coerce")
df["ioi_dt"]   = df["year"].apply(lambda y: IOI_DATES.get(int(y)) if pd.notna(y) else pd.NaT)

df["months_on_cf"] = np.where(
    df["active_before_ioi"].eq(1) & df["cf_reg_dt"].notna(),
    (df["ioi_dt"] - df["cf_reg_dt"]).dt.days / 30.44,
    np.nan
)

# ── summary stats by variable × period ───────────────────────────────────────
PERIODS = {"2011--2015": (2011, 2015), "2016--2025": (2016, 2025)}

VARIABLES = [
    ("Has CF handle",         "has_handle",              "{:.2f}"),
    ("Active before IOI",     "active_before_ioi",       "{:.2f}"),
    ("Months on CF",          "months_on_cf",            "{:.1f}"),
    ("Contests before IOI",   "cf_contests_before_ioi",  "{:.0f}"),
    ("Contests (6m before)",  "cf_contests_6m",          "{:.0f}"),
]

def stats(s):
    s = s.dropna()
    if len(s) == 0:
        return len(s), np.nan, np.nan, np.nan, np.nan
    return len(s), s.mean(), s.std(ddof=1), s.min(), s.max()

def fmt(x, fmt_str):
    return fmt_str.format(x) if pd.notna(x) else "---"

# ── build LaTeX ───────────────────────────────────────────────────────────────
lines = [
    r"\begin{table}[htbp]",
    r"\centering",
    r"\caption{Codeforces participation among IOI contestants}",
    r"\label{tab:cf_summary_by_year}",
    r"\begin{tabular}{l l r r r r r}",
    r"\toprule",
    r"Variable & Period & $N$ & Mean & SD & Min & Max \\",
    r"\midrule",
]

for label, col, f in VARIABLES:
    for i, (period_label, (y0, y1)) in enumerate(PERIODS.items()):
        sub  = df[df["year"].between(y0, y1)]
        n, m, s, lo, hi = stats(sub[col])
        var_cell = r"\textit{" + label + r"}" if i == 0 else ""
        lines.append(
            f"{var_cell} & {period_label} & {n}"
            f" & {fmt(m, f)} & {fmt(s, f)} & {fmt(lo, f)} & {fmt(hi, f)}"
            r" \\"
        )
    lines.append(r"\addlinespace")

# trim trailing \addlinespace
lines.pop()

lines += [
    r"\bottomrule",
    r"\end{tabular}",
    r"\begin{minipage}{\linewidth}",
    r"\smallskip",
    r"\footnotesize",
    r"\textit{Notes:} Unit of observation is participant $\times$ year. "
    r"$N$ is variable-specific (rows with non-missing values). "
    r"\textit{Has CF handle}: indicator for whether a Codeforces profile link was "
    r"found in CPHOF or IOI Stats. "
    r"\textit{Active before IOI}: indicator for having at least one rated Codeforces "
    r"contest before the month preceding the IOI. "
    r"\textit{Months on CF}: months elapsed between CF account registration "
    r"and the IOI start date, conditional on being active before the IOI.",
    r"\end{minipage}",
    r"\end{table}",
]

latex = "\n".join(lines)

# ── print and save ────────────────────────────────────────────────────────────
print("\n" + latex + "\n")
OUTPUT_TEX.write_text(latex, encoding="utf-8")
print(f"Saved to {OUTPUT_TEX}")
if OVERLEAF_TEX and OVERLEAF_TEX.parent.exists():
    OVERLEAF_TEX.write_text(latex, encoding="utf-8")
    print(f"Mirrored to {OVERLEAF_TEX}")
