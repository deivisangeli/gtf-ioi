#-------------------------------------------------------------------------------
# GTF: IOI -- fuzzy-match IOI participants with Codeforces, build crosswalk, merge
# Author: Thais Takeuchi
#
# Inputs:
#   1. $db_path/GTAllocation/Data/processed/ioi_participants.xlsx   (1 row per person, key = medalistID)
#   2. $db_path/Codeforces/Data/ioi_total_rating.xlsx               (1 row per person-year)
#   3. $db_path/GTAllocation/Data/processed/ioi/ioi_cf_manual_merge.xlsx   (optional, forced matches)
#   4. $db_path/GTAllocation/Data/processed/ioi/ioi_cf_manual_splits.xlsx  (optional, forced splits)
#
# Outputs (all in $db_path/GTAllocation/Data/processed/ioi/):
#   - ioi_cf_crosswalk.xlsx         : one row per (medalistID, cf_handle), with score + match_type
#   - ioi_cf_near_misses.xlsx       : pairs with combined_score in [0.70, 0.85] for manual review
#   - ioi_cf_conflicts.xlsx         : clusters with ambiguity (multiple medalistIDs -> one handle, etc.)
#   - ioi_participants_with_cf.xlsx : wide merge of IOI + CF aggregates via ioi_cf_id
#
# Approach (adapted from olympics/cleaning/*_data_clean.R):
#   1. Normalize names with standardize_name (ASCII-fold + lowercase + strip non-letters).
#   2. Apply country_aliases -> normalized canonical, then country_group_map for matching blocks
#      (groups former-USSR / former-YUG / former-CSK / former-DDR / Chinese-Taipei together).
#   3. Aggregate CF file to one row per CF-person (handle when available, fallback to name::country),
#      collecting all name variants seen across years.
#   4. Block candidate (medalistID, cf_person) pairs via 7 phonetic/token keys + country_group.
#   5. Score each pair with 7 name-similarity metrics + best_variant_jw; combined_score is weighted.
#   6. Decision tree assigns HIGH / MEDIUM / HIGH_CROSS_COUNTRY / NO_MATCH.
#   7. igraph components cluster matched pairs; detect conflicts (multiple IOI -> one CF, etc.).
#   8. Apply manual splits (remove auto match) and manual merges (force match).
#   9. Write crosswalk + near-misses + conflicts; do wide merge via ioi_cf_id.
#-------------------------------------------------------------------------------

rm(list = ls()); gc()

### 0. Paths + utilities -------------------------------------------------------
db_path      <- Sys.getenv("db_path")
# gtf_ioi_path: honor env var if set, otherwise default to the conventional location.
gtf_ioi_path <- Sys.getenv("gtf_ioi_path")
if (gtf_ioi_path == "") gtf_ioi_path <- "C:/Users/thtak/github/gtf-ioi"

IOI_FILE    <- file.path(db_path, "GTAllocation", "Data", "processed", "ioi_participants.xlsx")
CF_FILE     <- file.path(db_path, "Codeforces",   "Data", "ioi_total_rating.xlsx")

OUT_DIR     <- file.path(db_path, "GTAllocation", "Data", "processed", "ioi")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

CROSSWALK   <- file.path(OUT_DIR, "ioi_cf_crosswalk.xlsx")
NEAR_MISS   <- file.path(OUT_DIR, "ioi_cf_near_misses.xlsx")
CONFLICTS   <- file.path(OUT_DIR, "ioi_cf_conflicts.xlsx")
MERGE_FILE  <- file.path(OUT_DIR, "ioi_cf_manual_merge.xlsx")
SPLIT_FILE  <- file.path(OUT_DIR, "ioi_cf_manual_splits.xlsx")
OUT_FILE    <- file.path(OUT_DIR, "ioi_participants_with_cf.xlsx")

### 1. Packages + utilities ----------------------------------------------------
pacman::p_load(tidyverse, readxl, writexl, stringi, stringdist,
               phonics, igraph, openxlsx)

source(file.path(gtf_ioi_path, "utilities", "functions_st_names_unified.R"))

### 2. Config ------------------------------------------------------------------

# Country aliases: maps variant names -> canonical name (raw, pre-normalization).
country_aliases <- tribble(
  ~canonical,                    ~alias,
  "United States of America",   "United States",
  "United States of America",   "USA",
  "United States of America",   "U.S.A.",
  "Vietnam",                    "Viet Nam",
  "Chinese Taipei",             "Taiwan",
  "Chinese Taipei",             "Chinese Taipei (Taiwan)",
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

# Country group map: normalized canonical name -> group used for blocking.
# Groups successor states together so someone who competed for USSR at IOI 1989
# can match their current Russian CF account, etc.
country_group_map <- c(
  # Former USSR
  "soviet union"         = "FORMER_USSR",
  "russia"               = "FORMER_USSR",
  "ukraine"              = "FORMER_USSR",
  "belarus"              = "FORMER_USSR",
  "lithuania"            = "FORMER_USSR",
  "latvia"               = "FORMER_USSR",
  "estonia"              = "FORMER_USSR",
  "moldova"              = "FORMER_USSR",
  "armenia"              = "FORMER_USSR",
  "azerbaijan"           = "FORMER_USSR",
  "georgia"              = "FORMER_USSR",
  "turkmenistan"         = "FORMER_USSR",
  "uzbekistan"           = "FORMER_USSR",
  "kyrgyzstan"           = "FORMER_USSR",
  "tajikistan"           = "FORMER_USSR",
  "kazakhstan"           = "FORMER_USSR",

  # Former Yugoslavia
  "yugoslavia"           = "FORMER_YUG",
  "serbia and montenegro"= "FORMER_YUG",
  "serbia"               = "FORMER_YUG",
  "croatia"              = "FORMER_YUG",
  "bosnia and herzegovina" = "FORMER_YUG",
  "north macedonia"      = "FORMER_YUG",
  "slovenia"             = "FORMER_YUG",
  "montenegro"           = "FORMER_YUG",
  "kosovo"               = "FORMER_YUG",

  # Former Czechoslovakia
  "czechoslovakia"       = "FORMER_CSK",
  "czech republic"       = "FORMER_CSK",
  "slovakia"             = "FORMER_CSK",

  # Former East/West Germany
  "east germany"         = "FORMER_DDR",
  "germany"              = "FORMER_DDR",

  # Chinese Taipei / Taiwan
  "chinese taipei"       = "CHN_TPE",
  "taiwan"               = "CHN_TPE"
)

# Scoring weights and thresholds
WEIGHTS <- c(jw_canonical = 0.25, token_fuzzy = 0.20, jaccard = 0.15, best_variant = 0.40)
NEAR_MISS_LOW  <- 0.70  # near-miss lower bound (for manual review output)
NEAR_MISS_HIGH <- 0.85  # below this = not an automatic match

### 3. Helpers -----------------------------------------------------------------

# Normalized country name (alias -> canonical -> standardize_name)
alias_lookup <- country_aliases %>%
  mutate(alias_key = standardize_name(alias)) %>%
  select(alias_key, canonical) %>%
  deframe()

norm_country <- function(x) {
  key   <- standardize_name(x)
  canon <- ifelse(key %in% names(alias_lookup), alias_lookup[key], as.character(x))
  standardize_name(canon)
}

# Map normalized country -> country_group (falls back to country itself if ungrouped)
country_group <- function(norm_c) {
  ifelse(norm_c %in% names(country_group_map),
         country_group_map[norm_c],
         norm_c)
}

# 7 similarity helpers
token_fuzzy_similarity <- function(name_a, name_b) {
  tokens_a <- unlist(strsplit(name_a, " "))
  tokens_b <- unlist(strsplit(name_b, " "))
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0)
  best_a <- sapply(tokens_a, function(t) max(1 - stringdist(t, tokens_b, method = "jw", p = 0.1)))
  best_b <- sapply(tokens_b, function(t) max(1 - stringdist(t, tokens_a, method = "jw", p = 0.1)))
  mean(c(best_a, best_b))
}
subset_fuzzy_match <- function(name_a, name_b, threshold = 0.90) {
  tokens_a <- unlist(strsplit(name_a, " "))
  tokens_b <- unlist(strsplit(name_b, " "))
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0)
  if (length(tokens_a) <= length(tokens_b)) { smaller <- tokens_a; larger <- tokens_b }
  else { smaller <- tokens_b; larger <- tokens_a }
  matches <- sapply(smaller, function(t) max(1 - stringdist(t, larger, method = "jw", p = 0.1)))
  mean(matches >= threshold)
}
concat_match_score <- function(name_a, name_b) {
  tokens_a <- unlist(strsplit(name_a, " "))
  tokens_b <- unlist(strsplit(name_b, " "))
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0)
  concat_a <- tokens_a; concat_b <- tokens_b
  if (length(tokens_a) >= 2) for (i in 1:(length(tokens_a)-1)) concat_a <- c(concat_a, paste0(tokens_a[i], tokens_a[i+1]))
  if (length(tokens_b) >= 2) for (i in 1:(length(tokens_b)-1)) concat_b <- c(concat_b, paste0(tokens_b[i], tokens_b[i+1]))
  best_a <- sapply(tokens_a, function(t) max(1 - stringdist(t, concat_b, method = "jw", p = 0.1)))
  best_b <- sapply(tokens_b, function(t) max(1 - stringdist(t, concat_a, method = "jw", p = 0.1)))
  mean(c(best_a, best_b))
}
diff_token_similarity <- function(name_a, name_b) {
  tokens_a <- unlist(strsplit(name_a, " "))
  tokens_b <- unlist(strsplit(name_b, " "))
  if (length(tokens_a) == 0 || length(tokens_b) == 0) return(0)
  only_a <- setdiff(tokens_a, tokens_b); only_b <- setdiff(tokens_b, tokens_a)
  if (length(only_a) == 0 || length(only_b) == 0) return(1.0)
  sims <- c()
  for (ta in only_a) for (tb in only_b) sims <- c(sims, 1 - stringdist(ta, tb, method = "jw", p = 0.1))
  if (length(sims) == 0) 1.0 else max(sims)
}

# Best Jaro-Winkler across all pairs of " | "-separated variants
best_variant_jw <- function(variants_a, variants_b) {
  va <- unlist(strsplit(variants_a, " \\| "))
  vb <- unlist(strsplit(variants_b, " \\| "))
  va <- va[nzchar(va)]; vb <- vb[nzchar(vb)]
  if (length(va) == 0 || length(vb) == 0) return(NA_real_)
  sims <- outer(va, vb, function(a, b) 1 - stringdist(a, b, method = "jw", p = 0.1))
  max(sims, na.rm = TRUE)
}

# Rank medals for aggregation
MEDAL_RANK <- c(Gold = 3, Silver = 2, Bronze = 1,
                `Honourable Mention` = 0, `Honorable Mention` = 0,
                `No Award` = -1)
best_medal <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) return(NA_character_)
  ranks <- MEDAL_RANK[x]; ranks[is.na(ranks)] <- -2
  x[which.max(ranks)]
}
first_nonna <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) x[NA_integer_] else x[1]
}

### 4. Load --------------------------------------------------------------------
cat(sprintf("[load] %s\n", IOI_FILE))
ioi_raw <- read_xlsx(IOI_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(ioi_raw), ncol(ioi_raw)))

cat(sprintf("[load] %s\n", CF_FILE))
cf_raw  <- read_xlsx(CF_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(cf_raw), ncol(cf_raw)))

### 5. Normalize + country_group ----------------------------------------------
ioi_raw <- ioi_raw %>%
  mutate(name_key     = standardize_name(Name),
         country_norm = norm_country(Country),
         country_grp  = country_group(country_norm)) %>%
  relocate(name_key,     .after = Name) %>%
  relocate(country_norm, .after = Country) %>%
  relocate(country_grp,  .after = country_norm)

cf_raw <- cf_raw %>%
  mutate(name_key     = standardize_name(contestant),
         country_norm = norm_country(country),
         country_grp  = country_group(country_norm),
         # CF person identity: handle when available, else name::country fallback
         cf_person_id = ifelse(!is.na(handle) & nzchar(handle),
                               as.character(handle),
                               paste0("__nohandle__::", name_key, "::", country_norm)))

### 6. Aggregate CF to one row per CF-person ----------------------------------
cat("[agg ] aggregating CF to one row per cf_person_id...\n")

# Person-level columns: constant across a person's rows -> first non-NA
person_level <- c(
  "cf_link", "handle", "cf_registration_date", "cf_registration_year",
  "cf_country", "cf_current_rating", "cf_max_rating", "cf_current_rank",
  "cf_max_rank", "cf_contribution", "cf_friend_of_count",
  paste0("rating_", 2011:2025),
  "cf_max_rating_ever", "cf_year_max_rating", "cf_first_year_active",
  "cf_last_year_active", "cf_years_active", "first_ioi_year"
)
person_level <- intersect(person_level, names(cf_raw))
participation_max <- intersect(
  c("score", "cf_contests_before_ioi", "cf_contests_6m", "cf_years_before_first_ioi"),
  names(cf_raw)
)

cf_persons <- cf_raw %>%
  group_by(cf_person_id) %>%
  summarise(
    # canonical name = most-frequent name variant (choose_best-like: prefer most-used)
    cf_canonical_name = {
      tab <- sort(table(name_key[nzchar(name_key)]), decreasing = TRUE)
      if (length(tab) == 0) NA_character_ else names(tab)[1]
    },
    # all distinct name variants seen, pipe-separated
    cf_name_variants  = paste(sort(unique(name_key[nzchar(name_key)])), collapse = " | "),
    # most-frequent normalized country (for blocking)
    cf_country_norm   = {
      tab <- sort(table(country_norm[nzchar(country_norm)]), decreasing = TRUE)
      if (length(tab) == 0) NA_character_ else names(tab)[1]
    },
    cf_country_grp    = country_group(cf_country_norm),
    cf_all_country_grps = paste(sort(unique(country_grp[nzchar(country_grp)])), collapse = " | "),
    # raw display fields
    cf_contestant_raw = first_nonna(contestant),
    cf_country_raw    = first_nonna(country),
    # IOI-participation aggregates
    cf_ioi_years       = paste(sort(unique(na.omit(year))), collapse = " | "),
    cf_first_ioi_year  = suppressWarnings(min(year, na.rm = TRUE)),
    cf_last_ioi_year   = suppressWarnings(max(year, na.rm = TRUE)),
    cf_n_ioi_rows      = dplyr::n(),
    cf_best_result_ioi = best_medal(result),
    cf_max_score_ioi   = suppressWarnings(max(score, na.rm = TRUE)),
    # person-level and participation-max pass-throughs
    across(all_of(person_level),      first_nonna),
    across(all_of(participation_max), ~ suppressWarnings(max(.x, na.rm = TRUE))),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.infinite(.x), NA_real_, .x)))

cat(sprintf("       %d CF persons (from %d participation rows)\n",
            nrow(cf_persons), nrow(cf_raw)))

### 7. Build IOI persons table (with variants) -------------------------------
# IOI file is already 1 row per person; canonical = name_key; variants = just itself
# If you later add columns with alt name variants, concatenate them into ioi_name_variants.
ioi_persons <- ioi_raw %>%
  transmute(medalistID,
            ioi_canonical_name = name_key,
            ioi_name_variants  = name_key,
            ioi_country_norm   = country_norm,
            ioi_country_grp    = country_grp,
            ioi_name_raw       = Name,
            ioi_country_raw    = Country,
            lastYear           = if ("lastYear" %in% names(ioi_raw)) lastYear else NA_integer_)

### 8. Blocking (7 phonetic/token keys + country_group) -----------------------
cat("[match] building blocking keys...\n")

add_block_keys <- function(df, canon_col) {
  df %>% mutate(
    .cn           = .data[[canon_col]],
    .cn_nospace   = gsub(" ", "", .cn),
    soundex_canon = phonics::soundex(.cn_nospace),
    metaphone_canon = phonics::metaphone(.cn_nospace),
    first_token   = word(.cn, 1),
    last_token    = word(.cn, -1),
    block_key     = paste0(substr(first_token, 1, 3), substr(last_token, 1, 3)),
    soundex_first = phonics::soundex(first_token),
    soundex_last  = phonics::soundex(last_token)
  ) %>% select(-.cn, -.cn_nospace)
}

ioi_blk <- add_block_keys(ioi_persons, "ioi_canonical_name")
cf_blk  <- add_block_keys(cf_persons,  "cf_canonical_name")

make_candidate_pairs <- function(ioi_side, cf_side, key) {
  inner_join(
    ioi_side %>% select(medalistID, ioi_country_grp,
                        all_of(key), ioi_canonical_name, ioi_name_variants,
                        ioi_country_norm),
    cf_side  %>% select(cf_person_id, cf_country_grp,
                        all_of(key), cf_canonical_name, cf_name_variants,
                        cf_country_norm),
    by = setNames(c(key, "cf_country_grp"), c(key, "ioi_country_grp")),
    relationship = "many-to-many"
  )
}

cat("[match] generating candidate pairs across 7 blocking keys...\n")
candidate_pairs <- bind_rows(
  make_candidate_pairs(ioi_blk, cf_blk, "soundex_canon"),
  make_candidate_pairs(ioi_blk, cf_blk, "metaphone_canon"),
  make_candidate_pairs(ioi_blk, cf_blk, "block_key"),
  make_candidate_pairs(ioi_blk, cf_blk, "first_token"),
  make_candidate_pairs(ioi_blk, cf_blk, "last_token"),
  make_candidate_pairs(ioi_blk, cf_blk, "soundex_first"),
  make_candidate_pairs(ioi_blk, cf_blk, "soundex_last")
) %>%
  distinct(medalistID, cf_person_id, .keep_all = TRUE) %>%
  select(medalistID, cf_person_id,
         ioi_canonical_name, cf_canonical_name,
         ioi_name_variants, cf_name_variants,
         ioi_country_norm, cf_country_norm)

cat(sprintf("       %d candidate pairs\n", nrow(candidate_pairs)))

### 9. Also generate "cross-country" candidates (name-only blocking) ----------
# Purpose: catch cases where someone competed IOI for one country and has a CF
# account registered in another country. More selective decision rule below.
cat("[match] generating cross-country candidates (same name, different country_grp)...\n")

cross_country_pairs <- bind_rows(
  inner_join(ioi_blk %>% select(medalistID, ioi_canonical_name, ioi_name_variants,
                                ioi_country_norm, ioi_country_grp, soundex_canon, metaphone_canon),
             cf_blk  %>% select(cf_person_id, cf_canonical_name, cf_name_variants,
                                cf_country_norm, cf_country_grp, soundex_canon),
             by = "soundex_canon", relationship = "many-to-many"),
  inner_join(ioi_blk %>% select(medalistID, ioi_canonical_name, ioi_name_variants,
                                ioi_country_norm, ioi_country_grp, metaphone_canon),
             cf_blk  %>% select(cf_person_id, cf_canonical_name, cf_name_variants,
                                cf_country_norm, cf_country_grp, metaphone_canon),
             by = "metaphone_canon", relationship = "many-to-many")
) %>%
  filter(ioi_country_grp != cf_country_grp) %>%
  distinct(medalistID, cf_person_id, .keep_all = TRUE) %>%
  select(medalistID, cf_person_id,
         ioi_canonical_name, cf_canonical_name,
         ioi_name_variants, cf_name_variants,
         ioi_country_norm, cf_country_norm)

# Tag and combine
candidate_pairs      <- candidate_pairs %>% mutate(same_country_grp = TRUE)
cross_country_pairs  <- cross_country_pairs %>% mutate(same_country_grp = FALSE)

# Drop cross-country pairs that are already in same-country candidates
cross_country_pairs <- cross_country_pairs %>%
  anti_join(candidate_pairs, by = c("medalistID", "cf_person_id"))

all_candidates <- bind_rows(candidate_pairs, cross_country_pairs)
cat(sprintf("       %d cross-country candidates (additional)\n", nrow(cross_country_pairs)))

### 10. Scoring ---------------------------------------------------------------
cat("[score] computing 7 similarity metrics + best_variant_jw...\n")

all_candidates <- all_candidates %>%
  mutate(
    jw_canonical    = 1 - stringdist(ioi_canonical_name, cf_canonical_name, method = "jw", p = 0.1),
    lv_canonical    = 1 - stringdist(ioi_canonical_name, cf_canonical_name, method = "lv") /
                         pmax(nchar(ioi_canonical_name), nchar(cf_canonical_name)),
    jaccard_tokens  = map2_dbl(strsplit(ioi_canonical_name, " "),
                               strsplit(cf_canonical_name, " "),
                               ~ length(intersect(.x, .y)) / length(union(.x, .y))),
    token_fuzzy     = map2_dbl(ioi_canonical_name, cf_canonical_name, token_fuzzy_similarity),
    subset_sim      = map2_dbl(ioi_canonical_name, cf_canonical_name, subset_fuzzy_match),
    concat_sim      = map2_dbl(ioi_canonical_name, cf_canonical_name, concat_match_score),
    diff_token_sim  = map2_dbl(ioi_canonical_name, cf_canonical_name, diff_token_similarity),
    best_variant    = map2_dbl(ioi_name_variants, cf_name_variants, best_variant_jw),
    combined_score  = WEIGHTS["jw_canonical"] * jw_canonical +
                      WEIGHTS["token_fuzzy"]  * token_fuzzy +
                      WEIGHTS["jaccard"]      * jaccard_tokens +
                      WEIGHTS["best_variant"] * best_variant,
    max_tokens      = pmax(lengths(strsplit(ioi_canonical_name, " ")),
                           lengths(strsplit(cf_canonical_name, " "))),
    short_name_ok   = ifelse(max_tokens <= 3, jaccard_tokens >= 0.67, TRUE)
  )

### 11. Decision tree ---------------------------------------------------------
all_candidates <- all_candidates %>%
  mutate(match_type = case_when(
    # Same-country path (lenient -- olympics-style tree)
    same_country_grp & diff_token_sim < 0.75 ~ "NO_MATCH",
    same_country_grp & !short_name_ok ~ "NO_MATCH",
    same_country_grp & subset_sim >= 1.0 & diff_token_sim >= 0.99 ~ "HIGH",
    same_country_grp & subset_sim >= 0.95 & diff_token_sim >= 0.95 & jaccard_tokens >= 0.5 ~ "HIGH",
    same_country_grp & jw_canonical >= 0.95 & diff_token_sim >= 0.80 ~ "HIGH",
    same_country_grp & jw_canonical >= 0.92 & jaccard_tokens >= 0.7 & diff_token_sim >= 0.80 ~ "HIGH",
    same_country_grp & token_fuzzy >= 0.96 & jaccard_tokens >= 0.6 & diff_token_sim >= 0.80 ~ "HIGH",
    same_country_grp & jw_canonical >= 0.90 & jaccard_tokens >= 0.6 & diff_token_sim >= 0.80 ~ "MEDIUM",
    same_country_grp & token_fuzzy >= 0.94 & jaccard_tokens >= 0.5 & diff_token_sim >= 0.80 ~ "MEDIUM",
    same_country_grp & combined_score >= 0.90 & diff_token_sim >= 0.80 & jaccard_tokens >= 0.4 ~ "MEDIUM",

    # Cross-country path (strict -- require near-perfect name match)
    !same_country_grp & best_variant >= 0.95 & token_fuzzy >= 0.95 & diff_token_sim >= 0.90 ~ "HIGH_CROSS_COUNTRY",

    TRUE ~ "NO_MATCH"
  ))

cat("[decis] match_type distribution (candidate pairs):\n")
print(table(all_candidates$match_type, useNA = "ifany"))

### 12. Clustering + conflict detection ---------------------------------------
auto_matches <- all_candidates %>%
  filter(match_type %in% c("HIGH", "MEDIUM", "HIGH_CROSS_COUNTRY"))

# Collapse __nohandle__ matches when the same IOI person already has a real
# handle match. __nohandle__ IDs are fallbacks for CF rows where the handle
# column was NA -- when a real handle matches the same person, it wins and the
# synthetic row is just a duplicate.
ioi_with_real_handle <- auto_matches %>%
  filter(!startsWith(cf_person_id, "__nohandle__")) %>%
  pull(medalistID) %>% unique()

n_before <- nrow(auto_matches)
auto_matches <- auto_matches %>%
  filter(!(medalistID %in% ioi_with_real_handle &
           startsWith(cf_person_id, "__nohandle__")))
n_collapsed <- n_before - nrow(auto_matches)
if (n_collapsed > 0) {
  cat(sprintf("[clean] collapsed %d __nohandle__ match(es) superseded by a real handle\n",
              n_collapsed))
}

# Conflict detection is deferred to AFTER manual corrections are applied --
# otherwise the conflicts file reflects stale state that was already resolved
# by the user-maintained manual_merge / manual_splits xlsx files.
detect_conflicts <- function(matches) {
  ioi_dup <- matches %>% count(medalistID) %>% filter(n > 1) %>% pull(medalistID)
  cf_dup  <- matches %>% count(cf_person_id) %>% filter(n > 1) %>% pull(cf_person_id)
  matches %>%
    filter(medalistID %in% ioi_dup | cf_person_id %in% cf_dup) %>%
    mutate(conflict_type = case_when(
      medalistID %in% ioi_dup & cf_person_id %in% cf_dup ~ "both",
      medalistID %in% ioi_dup ~ "multiple_cf_per_ioi",
      cf_person_id %in% cf_dup ~ "multiple_ioi_per_cf"
    )) %>%
    arrange(medalistID, desc(combined_score))
}

pre_manual_conflicts <- detect_conflicts(auto_matches)
if (nrow(pre_manual_conflicts) > 0) {
  cat(sprintf("[warn ] %d conflicts before manual corrections\n",
              nrow(pre_manual_conflicts)))
}

### 13. Manual corrections: ensure template files exist ----------------------
ensure_manual_template <- function(path, cols) {
  if (!file.exists(path)) {
    empty <- as.data.frame(matrix(character(0), ncol = length(cols),
                                  dimnames = list(NULL, cols)))
    write_xlsx(empty, path)
    cat(sprintf("[init] created empty template: %s\n", path))
  }
}
ensure_manual_template(MERGE_FILE, c("medalistID", "cf_handle", "note"))
ensure_manual_template(SPLIT_FILE, c("medalistID", "cf_handle", "note"))

manual_merge  <- read_xlsx(MERGE_FILE)
manual_splits <- read_xlsx(SPLIT_FILE)

# Validate manual IDs: every medalistID must exist; every cf_handle must exist as cf_person_id.
validate_manual <- function(df, label) {
  if (nrow(df) == 0) return(invisible(TRUE))
  bad_ioi <- df %>% filter(!medalistID %in% ioi_persons$medalistID) %>% pull(medalistID)
  bad_cf  <- df %>% filter(!cf_handle %in% cf_persons$cf_person_id) %>% pull(cf_handle)
  if (length(bad_ioi) > 0) {
    cat(sprintf("[warn ] %s: %d unknown medalistID(s): %s\n",
                label, length(bad_ioi), paste(head(bad_ioi, 5), collapse = ", ")))
  }
  if (length(bad_cf) > 0) {
    cat(sprintf("[warn ] %s: %d unknown cf_handle(s): %s\n",
                label, length(bad_cf), paste(head(bad_cf, 5), collapse = ", ")))
  }
}
validate_manual(manual_merge,  "manual_merge")
validate_manual(manual_splits, "manual_splits")

### 14. Apply manual splits (remove forced non-matches) ----------------------
if (nrow(manual_splits) > 0) {
  cat(sprintf("[manual] applying %d split(s)\n", nrow(manual_splits)))
  auto_matches <- auto_matches %>%
    anti_join(manual_splits %>% rename(cf_person_id = cf_handle),
              by = c("medalistID", "cf_person_id"))
}

### 15. Apply manual merges (add forced matches) -----------------------------
if (nrow(manual_merge) > 0) {
  cat(sprintf("[manual] applying %d merge(s)\n", nrow(manual_merge)))
  forced <- manual_merge %>%
    filter(medalistID %in% ioi_persons$medalistID,
           cf_handle  %in% cf_persons$cf_person_id) %>%
    transmute(medalistID,
              cf_person_id      = cf_handle,
              match_type        = "MANUAL_MERGE",
              combined_score    = NA_real_,
              same_country_grp  = NA,
              ioi_canonical_name = ioi_persons$ioi_canonical_name[match(medalistID, ioi_persons$medalistID)],
              cf_canonical_name  = cf_persons$cf_canonical_name[match(cf_handle, cf_persons$cf_person_id)],
              ioi_name_variants  = ioi_persons$ioi_name_variants[match(medalistID, ioi_persons$medalistID)],
              cf_name_variants   = cf_persons$cf_name_variants[match(cf_handle, cf_persons$cf_person_id)],
              ioi_country_norm   = ioi_persons$ioi_country_norm[match(medalistID, ioi_persons$medalistID)],
              cf_country_norm    = cf_persons$cf_country_norm[match(cf_handle, cf_persons$cf_person_id)])
  # A manual merge (A -> B) is a definitive decision: drop *all* auto matches
  # touching medalistID A (they are overridden) AND *all* auto matches touching
  # cf_handle B (so B is not shared with other IOI persons by accident).
  # Then append the forced rows.
  auto_matches <- auto_matches %>%
    filter(!medalistID   %in% forced$medalistID,
           !cf_person_id %in% forced$cf_person_id) %>%
    bind_rows(forced)
}

### 15b. Re-detect conflicts AFTER manual corrections ------------------------
# This is the set of residual conflicts the user still needs to resolve.
conflicts <- detect_conflicts(auto_matches)
cat(sprintf("[warn ] %d conflicts remaining after manual corrections\n",
            nrow(conflicts)))

### 16. Assign ioi_cf_id ------------------------------------------------------
# Primary identity = medalistID (already unique for IOI persons).
# CF-only persons (no IOI match) get a synthetic ID.
crosswalk <- auto_matches %>%
  transmute(medalistID,
            cf_handle        = cf_person_id,
            ioi_cf_id        = medalistID,
            match_type,
            combined_score,
            jw_canonical     = if ("jw_canonical" %in% names(.)) jw_canonical else NA_real_,
            token_fuzzy      = if ("token_fuzzy"  %in% names(.)) token_fuzzy  else NA_real_,
            jaccard_tokens   = if ("jaccard_tokens" %in% names(.)) jaccard_tokens else NA_real_,
            best_variant     = if ("best_variant" %in% names(.)) best_variant else NA_real_,
            diff_token_sim   = if ("diff_token_sim" %in% names(.)) diff_token_sim else NA_real_,
            same_country_grp,
            ioi_canonical_name,
            cf_canonical_name,
            ioi_country_norm,
            cf_country_norm)

### 17. Write crosswalk, near-misses, conflicts ------------------------------
cat(sprintf("[save] %s\n", CROSSWALK))
write_xlsx(crosswalk, CROSSWALK)

near_misses <- all_candidates %>%
  filter(match_type == "NO_MATCH",
         combined_score >= NEAR_MISS_LOW,
         combined_score <  NEAR_MISS_HIGH) %>%
  arrange(desc(combined_score)) %>%
  select(medalistID, cf_person_id,
         ioi_canonical_name, cf_canonical_name,
         ioi_country_norm, cf_country_norm,
         same_country_grp, combined_score,
         jw_canonical, token_fuzzy, best_variant, diff_token_sim)

cat(sprintf("[save] %s  (%d rows)\n", NEAR_MISS, nrow(near_misses)))
write_xlsx(near_misses, NEAR_MISS)

cat(sprintf("[save] %s  (%d rows)\n", CONFLICTS, nrow(conflicts)))
write_xlsx(conflicts %>% select(-any_of(c("max_tokens", "short_name_ok"))), CONFLICTS)

### 18. Final wide merge via ioi_cf_id ---------------------------------------
# Pick one CF person per IOI person (highest score wins if there are conflicts).
best_per_ioi <- crosswalk %>%
  group_by(medalistID) %>%
  arrange(desc(is.na(combined_score)),  # MANUAL_MERGE (NA) first
          desc(combined_score)) %>%
  slice(1) %>%
  ungroup() %>%
  select(medalistID, cf_handle, match_type, combined_score)

merged <- ioi_raw %>%
  left_join(best_per_ioi, by = "medalistID") %>%
  left_join(cf_persons, by = c("cf_handle" = "cf_person_id")) %>%
  mutate(matched_cf = !is.na(cf_handle))

n_matched <- sum(merged$matched_cf)
cat(sprintf("[merg] matched %d/%d IOI participants (%.1f%%)\n",
            n_matched, nrow(ioi_raw), 100 * n_matched / nrow(ioi_raw)))

if ("lastYear" %in% names(merged)) {
  in_cov <- merged %>% filter(lastYear >= 2011)
  cat(sprintf("[merg] within CF coverage (lastYear >= 2011): %d/%d (%.2f%%)\n",
              sum(in_cov$matched_cf), nrow(in_cov),
              100 * mean(in_cov$matched_cf)))
}

cat(sprintf("[save] %s\n", OUT_FILE))
write_xlsx(merged, OUT_FILE)
cat(sprintf("       %d rows, %d cols\n", nrow(merged), ncol(merged)))

### 19. Diagnostics -----------------------------------------------------------
cat("\n[diag] match_type breakdown in crosswalk:\n")
print(table(crosswalk$match_type, useNA = "ifany"))

# Gain vs exact-match baseline: how many matches have cross-country or non-HIGH-exact?
exact_baseline <- ioi_persons %>%
  inner_join(cf_persons,
             by = c("ioi_canonical_name"  = "cf_canonical_name",
                    "ioi_country_norm"    = "cf_country_norm"),
             relationship = "many-to-many") %>%
  distinct(medalistID, cf_person_id)

fuzzy_only <- best_per_ioi %>%
  anti_join(exact_baseline, by = c("medalistID", "cf_handle" = "cf_person_id"))

cat(sprintf("\n[diag] Baseline exact-match pairs: %d\n", nrow(exact_baseline)))
cat(sprintf("[diag] Matches added by fuzzy logic: %d\n", nrow(fuzzy_only)))
if (nrow(fuzzy_only) > 0) {
  cat("[diag] Sample of fuzzy-only matches:\n")
  fuzzy_only %>%
    left_join(crosswalk %>% select(medalistID, cf_handle, ioi_canonical_name,
                                   cf_canonical_name, combined_score),
              by = c("medalistID", "cf_handle")) %>%
    head(10) %>%
    print()
}

cat("\n[diag] Unmatched IOI sample within CF coverage (lastYear >= 2011):\n")
if ("lastYear" %in% names(merged)) {
  merged %>% filter(!matched_cf, lastYear >= 2011) %>%
    select(medalistID, Name, Country, lastYear) %>%
    head(10) %>% print()
}
