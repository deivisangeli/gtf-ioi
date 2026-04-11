#-------------------------------------------------------------------------------
# GTF: IOI -- regression table
# Apr 2026
#-------------------------------------------------------------------------------

rm(list = ls())

db_path  <- Sys.getenv("db_path")
gtl_path <- Sys.getenv("gtl_path")
cf_path  <- file.path(db_path, "Codeforces")

pacman::p_load(tidyverse, readxl, fixest)

# ── Output paths ───────────────────────────────────────────────────────────────
gtf_ioi_dir  <- file.path(dirname(gtl_path), "gtf-ioi")
OUTPUT_TEX   <- file.path(gtf_ioi_dir, "output", "tab_regressions.tex")
dir.create(dirname(OUTPUT_TEX), showWarnings = FALSE, recursive = TRUE)

overleaf_dir <- file.path(dirname(gtl_path), "gtl-allocation", "overleaf",
                           "GTAllocation", "tables", "ioi")
OVERLEAF_TEX <- file.path(overleaf_dir, "tab_regressions.tex")

# ── Load ───────────────────────────────────────────────────────────────────────
ioi <- read_xlsx(file.path(cf_path, "Data", "ioi_total_rating.xlsx"))

# ── Regression 1: score ~ cf_rating_t | participant FE ────────────────────────
# For each contestant-year row, pull the CF rating column that matches IOI year t.
# The rating columns (rating_2011 … rating_2025) are measured 1 month before each
# year's IOI, so rating_t is the participant's standing right before competing.
ioi_panel <- ioi %>%
  filter(!is.na(score)) %>%
  select(contestant, year, score, starts_with("rating_")) %>%
  pivot_longer(
    starts_with("rating_"),
    names_to        = "rating_yr",
    values_to       = "cf_rating_t",
    names_prefix    = "rating_",
    names_transform = list(rating_yr = as.integer)
  ) %>%
  filter(rating_yr == year, !is.na(cf_rating_t))

reg1 <- feols(score ~ cf_rating_t | contestant,
              data = ioi_panel, cluster = ~contestant)

cat(sprintf("Reg 1: N = %d, participants = %d\n",
            nobs(reg1), n_distinct(ioi_panel$contestant)))

# ── Regression 2: 2025 score percentile, cross-section ────────────────────────
# Outcome: within-cohort score percentile (0–100) for 2025 IOI participants.
# cf_contribution and cf_friend_of_count are current CF profile metrics.
ioi_2025 <- ioi %>%
  filter(year == 2025,
         !is.na(rating_2025), !is.na(cf_contribution), !is.na(cf_friend_of_count)) %>%
  mutate(score_pct = percent_rank(score) * 100)

reg2 <- feols(score_pct ~ rating_2025 + cf_contribution + cf_friend_of_count | country,
              data = ioi_2025, cluster = ~country)

cat(sprintf("Reg 2: N = %d\n", nobs(reg2)))

# ── Regression 3: best-ever percentile, one obs per contestant ────────────────
# Outcome: maximum within-year score percentile across all of a contestant's IOI
# appearances. Controls: career-max CF rating, contribution, friend-of count;
# country FE and last-year FE (year of final IOI appearance).

# Within-year percentile rank
ioi_pct <- ioi %>%
  filter(!is.na(score)) %>%
  group_by(year) %>%
  mutate(score_pct = percent_rank(score) * 100) %>%
  ungroup()

# Best percentile per contestant
ioi_best_pct <- ioi_pct %>%
  group_by(contestant) %>%
  summarise(best_pct = max(score_pct, na.rm = TRUE), .groups = "drop")

# Country and last year (from most recent IOI appearance)
ioi_last <- ioi %>%
  group_by(contestant) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(contestant, country, last_year = year)

# CF profile variables: same across all rows for a contestant — take first non-NA
ioi_cf <- ioi %>%
  filter(!is.na(handle)) %>%
  group_by(contestant) %>%
  summarise(
    max_cf_rating      = na.omit(cf_max_rating_ever)[1],
    cf_contribution    = na.omit(cf_contribution)[1],
    cf_friend_of_count = na.omit(cf_friend_of_count)[1],
    .groups = "drop"
  )

ioi_best <- ioi_best_pct %>%
  left_join(ioi_last, by = "contestant") %>%
  left_join(ioi_cf,   by = "contestant") %>%
  filter(!is.na(max_cf_rating), !is.na(cf_contribution), !is.na(cf_friend_of_count))

reg3 <- feols(best_pct ~ max_cf_rating + cf_contribution + cf_friend_of_count |
                country + last_year,
              data = ioi_best, cluster = ~country)

cat(sprintf("Reg 3: N = %d\n", nobs(reg3)))

# ── Outcome means (from regression samples) ───────────────────────────────────
mean_y <- c(
  round(mean(ioi_panel$score,     na.rm = TRUE), 1),
  round(mean(ioi_2025$score_pct,  na.rm = TRUE), 1),
  round(mean(ioi_best$best_pct,   na.rm = TRUE), 1)
)

# ── LaTeX table via fixest::etable (standard booktabs, no tabularray) ─────────
var_dict <- c(
  score              = "IOI score",
  score_pct          = "Score percentile",
  best_pct           = "Best percentile",
  cf_rating_t        = "CF rating (year $t$)",
  rating_2025        = "CF rating (2025)",
  max_cf_rating      = "CF rating (career max)",
  cf_contribution    = "CF contribution",
  cf_friend_of_count = "CF friend-of count"
)

notes_str <- paste0(
  "Clustered SEs: contestant level (col.~1), country level (cols.~2--3). ",
  "Col.~(1): within-participant panel, participant FE, all IOI years with ",
  "a valid same-year CF rating. ",
  "Col.~(2): 2025 IOI cohort, country FE. ",
  "Col.~(3): one obs per contestant; outcome = best within-year score percentile; ",
  "country and last-year FE."
)

etable(
  reg1, reg2, reg3,
  se.below   = TRUE,
  dict       = var_dict,
  order      = c("rating", "contribution", "friend"),
  extralines = list("Mean outcome" = mean_y),
  title     = "Codeforces experience and IOI performance",
  label     = "tab:cf_regressions",
  notes     = notes_str,
  tex       = TRUE,
  file      = OUTPUT_TEX,
  replace   = TRUE
)

cat("Saved →", OUTPUT_TEX, "\n")

if (dir.exists(overleaf_dir)) {
  file.copy(OUTPUT_TEX, OVERLEAF_TEX, overwrite = TRUE)
  cat("Mirrored →", OVERLEAF_TEX, "\n")
}
