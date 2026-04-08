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

# ── summary stats by year ─────────────────────────────────────────────────────
def stats(s):
    """Return (mean, sd, min, max) for a Series, or (nan,nan,nan,nan) if empty."""
    s = s.dropna()
    if len(s) == 0:
        return np.nan, np.nan, np.nan, np.nan
    return s.mean(), s.std(ddof=1), s.min(), s.max()

rows = []
for year in sorted(df["year"].dropna().unique()):
    yr   = int(year)
    sub  = df[df["year"] == yr]
    n    = len(sub)

    hh_mean, hh_sd, hh_min, hh_max = stats(sub["has_handle"].astype(float))
    ab_mean, ab_sd, ab_min, ab_max = stats(sub["active_before_ioi"].astype(float))
    ms_mean, ms_sd, ms_min, ms_max = stats(sub.loc[sub["active_before_ioi"] == 1, "months_on_cf"])

    rows.append(dict(
        year=yr, n=n,
        hh_mean=hh_mean, hh_sd=hh_sd, hh_min=hh_min, hh_max=hh_max,
        ab_mean=ab_mean, ab_sd=ab_sd, ab_min=ab_min, ab_max=ab_max,
        ms_mean=ms_mean, ms_sd=ms_sd, ms_min=ms_min, ms_max=ms_max,
    ))

tbl = pd.DataFrame(rows)

# ── format helpers ────────────────────────────────────────────────────────────
def fmt_pct(x):
    return f"{x:.2f}" if pd.notna(x) else "---"

def fmt_mo(x):
    return f"{x:.1f}" if pd.notna(x) else "---"

# ── build LaTeX ───────────────────────────────────────────────────────────────
lines = []
lines += [
    r"\begin{table}[htbp]",
    r"\centering",
    r"\caption{Codeforces participation among IOI contestants, by year}",
    r"\label{tab:cf_summary_by_year}",
    r"\small",
    r"\begin{tabular}{r r | rrrr | rrrr | rrrr}",
    r"\toprule",
    r" & & \multicolumn{4}{c|}{\textit{Has CF handle}} "
    r"& \multicolumn{4}{c|}{\textit{Active before IOI}} "
    r"& \multicolumn{4}{c}{\textit{Months on CF at IOI}} \\",
    r"\cmidrule(lr){3-6}\cmidrule(lr){7-10}\cmidrule(lr){11-14}",
    r"Year & $N$ & Mean & SD & Min & Max "
    r"& Mean & SD & Min & Max "
    r"& Mean & SD & Min & Max \\",
    r"\midrule",
]

for _, r in tbl.iterrows():
    yr  = int(r["year"])
    n   = int(r["n"])
    row = (
        f"{yr} & {n}"
        f" & {fmt_pct(r['hh_mean'])} & {fmt_pct(r['hh_sd'])}"
        f" & {fmt_pct(r['hh_min'])} & {fmt_pct(r['hh_max'])}"
        f" & {fmt_pct(r['ab_mean'])} & {fmt_pct(r['ab_sd'])}"
        f" & {fmt_pct(r['ab_min'])} & {fmt_pct(r['ab_max'])}"
        f" & {fmt_mo(r['ms_mean'])} & {fmt_mo(r['ms_sd'])}"
        f" & {fmt_mo(r['ms_min'])} & {fmt_mo(r['ms_max'])}"
        r" \\"
    )
    lines.append(row)

lines += [
    r"\midrule",
]

# totals row
all_n      = int(tbl["n"].sum())
hh_m, hh_s, hh_lo, hh_hi = stats(df["has_handle"].astype(float))
ab_m, ab_s, ab_lo, ab_hi = stats(df["active_before_ioi"].astype(float))
ms_m, ms_s, ms_lo, ms_hi = stats(df.loc[df["has_handle"] == 1, "months_on_cf"])

lines.append(
    rf"\textit{{All}} & {all_n}"
    f" & {fmt_pct(hh_m)} & {fmt_pct(hh_s)}"
    f" & {fmt_pct(hh_lo)} & {fmt_pct(hh_hi)}"
    f" & {fmt_pct(ab_m)} & {fmt_pct(ab_s)}"
    f" & {fmt_pct(ab_lo)} & {fmt_pct(ab_hi)}"
    f" & {fmt_mo(ms_m)} & {fmt_mo(ms_s)}"
    f" & {fmt_mo(ms_lo)} & {fmt_mo(ms_hi)}"
    r" \\"
)

lines += [
    r"\bottomrule",
    r"\end{tabular}",
    r"\begin{minipage}{\linewidth}",
    r"\smallskip",
    r"\footnotesize",
    r"\textit{Notes:} Unit of observation is participant $\times$ year. "
    r"\textit{Has CF handle}: indicator for whether a Codeforces profile link was "
    r"found in CPHOF or IOI Stats. "
    r"\textit{Active before IOI}: indicator for having at least one rated Codeforces "
    r"contest before the month preceding the IOI. "
    r"\textit{Months on CF at IOI}: months elapsed between CF account registration "
    r"and the IOI start date, conditional on being active before the IOI.",
    r"\end{minipage}",
    r"\end{table}",
]

latex = "\n".join(lines)

# ── print and save ────────────────────────────────────────────────────────────
print("\n" + latex + "\n")
OUTPUT_TEX.write_text(latex, encoding="utf-8")
print(f"Saved to {OUTPUT_TEX}")
