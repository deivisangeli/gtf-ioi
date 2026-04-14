#-------------------------------------------------------------------------------
# GTF: IOI -- merge IOI participants (GTAllocation) with Codeforces ratings
# Author: Thais Takeuchi
#
# Inputs:
#   1. $db_path/GTAllocation/Data/processed/ioi_participants.xlsx   (1 row per person)
#   2. $db_path/Codeforces/Data/ioi_total_rating.xlsx               (1 row per person-year)
#
# Output:
#   $db_path/GTAllocation/Data/processed/ioi_participants_with_cf.xlsx
#
# Approach (direct, Olympics2-style):
#   - Normalize name:    stri_trans_general("Latin-ASCII") + lowercase +
#                        keep letters/spaces + collapse whitespace
#   - Normalize country: same, plus explicit alias table for cross-file
#                        discrepancies (Vietnam/Viet Nam, USA/United States,
#                        Chinese Taipei/Taiwan, etc.)
#   - Collapse CF file to one row per (name_key, country_key):
#       * person-level columns  -> first non-NA (constant across the person's rows)
#       * participation columns -> min/max/count/best
#   - Left-join onto ioi_participants by (name_key, country_key)
#-------------------------------------------------------------------------------

rm(list = ls())

### Paths (see CLAUDE.md: db_path is Dropbox root) ------------------------------
db_path  <- Sys.getenv("db_path")
gtl_path <- Sys.getenv("gtl_path")

IOI_FILE <- file.path(db_path, "GTAllocation", "Data", "processed", "ioi_participants.xlsx")
CF_FILE  <- file.path(db_path, "Codeforces",   "Data", "ioi_total_rating.xlsx")
OUT_FILE <- file.path(db_path, "GTAllocation", "Data", "processed", "ioi_participants_with_cf.xlsx")

### Packages --------------------------------------------------------------------
pacman::p_load(tidyverse, readxl, writexl, stringi)

### Helpers ---------------------------------------------------------------------

# norm_name: ASCII-fold -> lowercase -> keep [a-z ] only -> collapse whitespace
norm_name <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- tolower(x)
  x <- str_replace_all(x, "[^a-z\\s]", " ")
  x <- str_squish(x)
  x[is.na(x)] <- ""
  x
}

# Country alias table. Canonical name first; any alias is mapped to canonical.
country_aliases <- tribble(
  ~canonical,                    ~alias,
  "United States of America",   "United States",
  "United States of America",   "USA",
  "United States of America",   "U.S.A.",
  "Vietnam",                    "Viet Nam",
  "Chinese Taipei (Taiwan)",    "Taiwan",
  "Chinese Taipei (Taiwan)",    "Chinese Taipei",
  "North Macedonia",            "Macedonia",
  "North Macedonia",            "FYR Macedonia",
  "North Macedonia",            "FYROM",
  "Turkey",                     "T\ufffdrkiye",
  "Turkey",                     "T\u00fcrkiye",
  "Turkey",                     "Turkiye",
  "Czech Republic",             "Czechia",
  "Russia",                     "Russian Federation",
  "South Korea",                "Korea, Republic of",
  "South Korea",                "Republic of Korea",
  "South Korea",                "Korea",
  "Iran",                       "Iran, Islamic Republic of",
  "Iran",                       "Islamic Republic of Iran",
  "United Kingdom",             "UK",
  "United Kingdom",             "Great Britain",
  "Syria",                      "Syrian Arab Republic",
  "Moldova",                    "Republic of Moldova",
  "Bolivia",                    "Plurinational State of Bolivia",
  "Venezuela",                  "Bolivarian Republic of Venezuela",
  "Tanzania",                   "United Republic of Tanzania"
)

# norm_country: ASCII-fold+lowercase for lookup; map alias -> canonical; normalize result
alias_lookup <- country_aliases %>%
  mutate(alias_key = norm_name(alias)) %>%
  select(alias_key, canonical) %>%
  deframe()

norm_country <- function(x) {
  key  <- norm_name(x)
  canon <- ifelse(key %in% names(alias_lookup), alias_lookup[key], as.character(x))
  norm_name(canon)
}

# best_medal: pick the highest-ranked medal string in a character vector
MEDAL_RANK <- c(Gold = 3, Silver = 2, Bronze = 1,
                `Honourable Mention` = 0, `Honorable Mention` = 0,
                `No Award` = -1)

best_medal <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ranks <- MEDAL_RANK[x]
  ranks[is.na(ranks)] <- -2
  x[which.max(ranks)]
}

first_nonna <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

### 1. Load --------------------------------------------------------------------
cat(sprintf("[load] %s\n", IOI_FILE))
ioi <- read_xlsx(IOI_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(ioi), ncol(ioi)))

cat(sprintf("[load] %s\n", CF_FILE))
cf <- read_xlsx(CF_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(cf), ncol(cf)))

### 2. Normalized merge keys ---------------------------------------------------
ioi <- ioi %>%
  mutate(name_key    = norm_name(Name),
         country_key = norm_country(Country))

cf <- cf %>%
  mutate(name_key    = norm_name(contestant),
         country_key = norm_country(country))

### 3. Aggregate CF to one row per person --------------------------------------
# Person-level columns: identical across the person's rows -> first non-NA
person_level <- c(
  "cf_link", "handle", "cf_registration_date", "cf_registration_year",
  "cf_country", "cf_current_rating", "cf_max_rating", "cf_current_rank",
  "cf_max_rank", "cf_contribution", "cf_friend_of_count",
  paste0("rating_", 2011:2025),
  "cf_max_rating_ever", "cf_year_max_rating", "cf_first_year_active",
  "cf_last_year_active", "cf_years_active", "first_ioi_year"
)
person_level <- intersect(person_level, names(cf))

# Participation-level numeric columns: max is sensible
participation_max <- intersect(
  c("score", "cf_contests_before_ioi", "cf_contests_6m", "cf_years_before_first_ioi"),
  names(cf)
)

cf_agg <- cf %>%
  group_by(name_key, country_key) %>%
  summarise(
    cf_ioi_years       = paste(sort(unique(na.omit(year))), collapse = " | "),
    cf_first_ioi_year  = suppressWarnings(min(year, na.rm = TRUE)),
    cf_last_ioi_year   = suppressWarnings(max(year, na.rm = TRUE)),
    cf_n_ioi_rows      = dplyr::n(),
    cf_best_result_ioi = best_medal(result),
    cf_max_score_ioi   = suppressWarnings(max(score, na.rm = TRUE)),
    across(all_of(person_level),       first_nonna),
    across(all_of(participation_max),  ~ suppressWarnings(max(.x, na.rm = TRUE))),
    cf_contestant_raw  = first_nonna(contestant),
    cf_country_raw     = first_nonna(country),
    .groups = "drop"
  ) %>%
  # replace -Inf / Inf (produced by max() on all-NA) with NA
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA_real_, .x)))

cat(sprintf("[agg ] Codeforces collapsed to %d persons (from %d participation rows)\n",
            nrow(cf_agg), nrow(cf)))

### 4. Merge -------------------------------------------------------------------
merged <- ioi %>%
  left_join(cf_agg, by = c("name_key", "country_key")) %>%
  mutate(matched_cf = !is.na(cf_contestant_raw))

n_matched <- sum(merged$matched_cf)
cat(sprintf("[merg] matched %d/%d IOI participants (%.1f%%)\n",
            n_matched, nrow(ioi), 100 * n_matched / nrow(ioi)))

### 5. Write -------------------------------------------------------------------
dir.create(dirname(OUT_FILE), showWarnings = FALSE, recursive = TRUE)
cat(sprintf("[save] %s\n", OUT_FILE))
write_xlsx(merged, OUT_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(merged), ncol(merged)))

### 6. Diagnostics -------------------------------------------------------------
unmatched <- merged %>%
  filter(!matched_cf) %>%
  select(Name, Country, lastYear)

cat(sprintf("\n[diag] %d unmatched IOI rows.\n", nrow(unmatched)))
cat("       Breakdown by lastYear vs CF coverage (2011+):\n")
cat(sprintf("         lastYear <  2011 (expected no match): %d\n",
            sum(unmatched$lastYear < 2011, na.rm = TRUE)))
cat(sprintf("         lastYear >= 2011 (true unmatched):    %d\n",
            sum(unmatched$lastYear >= 2011, na.rm = TRUE)))

cat("\n[diag] Sample of unmatched IOI rows within CF coverage (lastYear >= 2011):\n")
print(unmatched %>% filter(lastYear >= 2011) %>% head(20))

unused_cf <- cf_agg %>%
  anti_join(ioi %>% distinct(name_key, country_key), by = c("name_key", "country_key"))
cat(sprintf("\n[diag] %d Codeforces persons with no IOI match ",
            nrow(unused_cf)))
cat("(expected: CF coverage extends to 2025, IOI file ends at 2020).\n")
print(unused_cf %>% select(cf_contestant_raw, cf_country_raw, cf_first_ioi_year) %>% head(10))
