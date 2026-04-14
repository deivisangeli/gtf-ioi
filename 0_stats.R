#-------------------------------------------------------------------------------
# GTF: IOI -- stats
# Nov 2025
# Author: Thais Takeuchi
#-------------------------------------------------------------------------------

# clean environment
rm(list=ls())

### Load data
## Define path
db_path <- Sys.getenv("db_path") ### Dropbox path (set via: setx db_path "C:/Users/YOUR_NAME/Globtalent Dropbox")
gtl_path <- Sys.getenv("gtl_path") ### github file path


cf_path <- file.path(db_path, "Codeforces") ### GT codeforces folder path

### Load packages
pacman ::p_load(tidyverse, RAthena, DBI, readr, reticulate, readxl, writexl, 
                dbplyr, glue, arrow, aws.s3, bit64, shiny, stargazer, gt, httr,
                jsonlite, furrr, stringdist, tictoc, foreach, doParallel, stringr,
                fuzzyjoin, purrr, data.table, Hmisc, patchwork, progress, haven, rdrobust, nprobust,
                openalexR, countrycode)

### Load data
ioi <- read_xlsx(file.path(cf_path, "Data/ioi_total_rating.xlsx")) # all participants
ioi_with_cf <- read_xlsx(file.path(cf_path, "Data/ioi_total_rating.xlsx")) %>% # participants with codeforces
  filter(!is.na(handle))

# % medal winning
ioi_with_cf %>%
  mutate(result_unified = case_when(
    result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
    result %in% c("Silver", "Silver*") ~ "Silver",
    result %in% c("Bronze", "Bronze*") ~ "Bronze",
    TRUE ~ result
  )) %>%
  count(result_unified) %>%
  mutate(percentage = n / sum(n) * 100)

# # A tibble: 4 × 3
#result_unified     n percentage
#hr>          <int>      <dbl>
#1 Bronze           982       28.2
#2 Gold             415       11.9
#3 No Award        1339       38.4
#4 Silver           747       21.4

# SCATTERPLOT ----
# score vs rating for each year

# Reshape data: rating_2011 to rating_2025 in long format
ioi_long <- ioi %>%
  select(year, contestant, country, result, score, 
         rating_2011:rating_2025) %>%
  pivot_longer(
    cols = starts_with("rating_"),
    names_to = "rating_year",
    values_to = "rating",
    names_prefix = "rating_"
  ) %>%
  mutate(rating_year = as.numeric(rating_year)) %>%
  filter(!is.na(rating), score > 0)

# Simple scatterplot
p <- ggplot(ioi_long, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "IOI Score vs Codeforces Rating (2011-2025)",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

# View correlation
cor(ioi_long$rating, ioi_long$score, use = "complete.obs")

# Graph
p 

# Save scatterplot
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating.png"),
  plot = p,
  width = 10,
  height = 6,
  dpi = 300
)


# Per year
# Scatterplot by year
p_by_year <- ggplot(ioi_long, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~ rating_year, ncol = 3) +
  labs(
    title = "IOI Score vs Codeforces Rating by Year (2011-2025)",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

# Display the graph
print(p_by_year)

# Save the graph
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating_by_year.png"),
  plot = p_by_year,
  width = 12,
  height = 10,
  dpi = 300
)

# results ----
# Add unified result to ioi_long (fold '*' and 'Gold (3rd)' variants)
ioi_long <- ioi_long %>%
  mutate(result_unified = case_when(
    result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
    result %in% c("Silver", "Silver*")            ~ "Silver",
    result %in% c("Bronze", "Bronze*")            ~ "Bronze",
    TRUE ~ result
  ))

# boxplot ----
# Boxplot: Rating by Result (all years)
p_result <- ggplot(ioi_long, aes(x = result_unified, y = rating, fill = result_unified)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Codeforces Rating by IOI Result (2011-2025)",
    x = "IOI Result",
    y = "Codeforces Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(p_result)
ggsave(
  filename = file.path(cf_path, "output", "rating_by_result.png"),
  plot = p_result,
  width = 10,
  height = 6,
  dpi = 300
)

# scatterplot ----
# Scatterplot: Rating vs Score colored by Result (all years)
p_scatter_result <- ggplot(ioi_long, aes(x = rating, y = score, color = result_unified)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "IOI Score vs Codeforces Rating by Medal Type (2011-2025)",
    x = "Codeforces Rating",
    y = "IOI Score",
    color = "Medal"
  ) +
  theme_minimal()

print(p_scatter_result)
ggsave(
  filename = file.path(cf_path, "output", "score_vs_rating_by_result.png"),
  plot = p_scatter_result,
  width = 10,
  height = 6,
  dpi = 300
)

# Scatterplot: Rating vs Score colored by Result, separated by year
p_scatter_result_year <- ggplot(ioi_long, aes(x = rating, y = score, color = result_unified)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ rating_year, ncol = 3) +
  labs(
    title = "IOI Score vs Codeforces Rating by Medal Type and Year (2011-2025)",
    x = "Codeforces Rating",
    y = "IOI Score",
    color = "Medal"
  ) +
  theme_minimal()

print(p_scatter_result_year)
ggsave(
  filename = file.path(cf_path, "output", "score_vs_rating_by_result_by_year.png"),
  plot = p_scatter_result_year,
  width = 14,
  height = 12,
  dpi = 300
)

# Conditional plots: active in 6 months before IOI ----
# Filter to participants with at least 1 contest in the 6 months before their IOI
ioi_long_active6m <- ioi %>%
  filter(!is.na(cf_contests_6m) & cf_contests_6m > 0) %>%
  select(year, contestant, country, result, score,
         rating_2011:rating_2025) %>%
  pivot_longer(
    cols = starts_with("rating_"),
    names_to = "rating_year",
    values_to = "rating",
    names_prefix = "rating_"
  ) %>%
  mutate(rating_year = as.numeric(rating_year)) %>%
  filter(!is.na(rating), score > 0)

# Scatterplot (all years), active-6m sample
p_active6m <- ggplot(ioi_long_active6m, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "IOI Score vs Codeforces Rating (2011-2025)",
    subtitle = "Restricted to participants with >0 rated CF contests in 6 months before IOI",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

print(p_active6m)
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating_active6m.png"),
  plot = p_active6m,
  width = 10,
  height = 6,
  dpi = 300
)

# By year, active-6m sample
p_active6m_by_year <- ggplot(ioi_long_active6m, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~ rating_year, ncol = 3) +
  labs(
    title = "IOI Score vs Codeforces Rating by Year (2011-2025)",
    subtitle = "Restricted to participants with >0 rated CF contests in 6 months before IOI",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

print(p_active6m_by_year)
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating_by_year_active6m.png"),
  plot = p_active6m_by_year,
  width = 12,
  height = 10,
  dpi = 300
)

# Conditional plots: 5+ contests in 6 months before IOI ----
ioi_long_active6m5 <- ioi %>%
  filter(!is.na(cf_contests_6m) & cf_contests_6m >= 5) %>%
  select(year, contestant, country, result, score,
         rating_2011:rating_2025) %>%
  pivot_longer(
    cols = starts_with("rating_"),
    names_to = "rating_year",
    values_to = "rating",
    names_prefix = "rating_"
  ) %>%
  mutate(rating_year = as.numeric(rating_year)) %>%
  filter(!is.na(rating), score > 0)

p_active6m5 <- ggplot(ioi_long_active6m5, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "IOI Score vs Codeforces Rating (2011-2025)",
    subtitle = "Restricted to participants with 5+ rated CF contests in 6 months before IOI",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

print(p_active6m5)
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating_active6m5.png"),
  plot = p_active6m5,
  width = 10,
  height = 6,
  dpi = 300
)

p_active6m5_by_year <- ggplot(ioi_long_active6m5, aes(x = rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~ rating_year, ncol = 3) +
  labs(
    title = "IOI Score vs Codeforces Rating by Year (2011-2025)",
    subtitle = "Restricted to participants with 5+ rated CF contests in 6 months before IOI",
    x = "Codeforces Rating",
    y = "IOI Score"
  ) +
  theme_minimal()

print(p_active6m5_by_year)
ggsave(
  filename = file.path(cf_path, "output", "ioi_score_vs_rating_by_year_active6m5.png"),
  plot = p_active6m5_by_year,
  width = 12,
  height = 10,
  dpi = 300
)

# Experience by medal type × year ----
IOI_DATES_R <- setNames(
  as.Date(c(
    "2011-07-22", "2012-09-23", "2013-07-06", "2014-07-13",
    "2015-07-26", "2016-08-12", "2017-07-28", "2018-09-01",
    "2019-08-04", "2020-09-13", "2021-06-19", "2022-08-07",
    "2023-08-28", "2024-09-01", "2025-07-27"
  )),
  as.character(2011:2025)
)

medal_levels  <- c("Gold", "Silver", "Bronze", "No Award")
medal_colours <- c(Gold = "#DAA520", Silver = "#909090", Bronze = "#CD7F32", "No Award" = "#BBBBBB")

ioi_exp <- ioi %>%
  filter(!is.na(handle)) %>%
  mutate(
    result_unified = factor(case_when(
      result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
      result %in% c("Silver", "Silver*")            ~ "Silver",
      result %in% c("Bronze", "Bronze*")            ~ "Bronze",
      TRUE                                          ~ "No Award"
    ), levels = medal_levels),
    ioi_date    = IOI_DATES_R[as.character(year)],
    cf_reg_date = as.Date(cf_registration_date),
    # months on CF: only for participants active before IOI (cf_rating_reason is NA)
    months_on_cf = if_else(
      is.na(cf_rating_reason) & !is.na(cf_reg_date),
      as.numeric(ioi_date - cf_reg_date) / 30.44,
      NA_real_
    )
  )

exp_summary <- ioi_exp %>%
  group_by(year, result_unified) %>%
  summarise(
    mean_contests = mean(cf_contests_before_ioi, na.rm = TRUE),
    mean_months   = mean(months_on_cf,           na.rm = TRUE),
    n_contests    = sum(!is.na(cf_contests_before_ioi)),
    n_months      = sum(!is.na(months_on_cf)),
    .groups = "drop"
  )

p_exp_contests <- ggplot(exp_summary, aes(x = year, y = mean_contests,
                                          color = result_unified, group = result_unified)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = medal_colours, name = NULL) +
  scale_x_continuous(breaks = 2011:2025) +
  labs(
    title = "Rated CF contests before IOI, by medal",
    x = NULL, y = "Mean contests before IOI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_exp_months <- ggplot(exp_summary, aes(x = year, y = mean_months,
                                        color = result_unified, group = result_unified)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = medal_colours, name = NULL) +
  scale_x_continuous(breaks = 2011:2025) +
  labs(
    title = "Months on Codeforces before IOI, by medal",
    subtitle = "Conditional on having a rated contest before the IOI",
    x = "IOI year", y = "Mean months since CF registration"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_exp_combined <- p_exp_contests / p_exp_months +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_exp_combined)
ggsave(
  filename = file.path(cf_path, "output", "cf_experience_by_medal_year.png"),
  plot = p_exp_combined,
  width = 10,
  height = 10,
  dpi = 300
)

# Experience by medal × year: zero-imputed version ----
# Contests and months set to 0 for participants with no CF activity by the IOI date.
# Denominator is all IOI participants (not just those with CF handles).
ioi_exp_all <- ioi %>%
  mutate(
    result_unified = factor(case_when(
      result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
      result %in% c("Silver", "Silver*")            ~ "Silver",
      result %in% c("Bronze", "Bronze*")            ~ "Bronze",
      TRUE                                          ~ "No Award"
    ), levels = medal_levels),
    ioi_date    = IOI_DATES_R[as.character(year)],
    cf_reg_date = as.Date(cf_registration_date),
    months_on_cf = if_else(
      is.na(cf_rating_reason) & !is.na(cf_reg_date),
      as.numeric(ioi_date - cf_reg_date) / 30.44,
      NA_real_
    ),
    contests_z = replace_na(cf_contests_before_ioi, 0),
    months_z   = replace_na(months_on_cf, 0)
  )

exp_summary_z <- ioi_exp_all %>%
  group_by(year, result_unified) %>%
  summarise(
    mean_contests = mean(contests_z),
    mean_months   = mean(months_z),
    .groups = "drop"
  )

p_exp_contests_z <- ggplot(exp_summary_z, aes(x = year, y = mean_contests,
                                               color = result_unified, group = result_unified)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = medal_colours, name = NULL) +
  scale_x_continuous(breaks = 2011:2025) +
  labs(
    title = "Rated CF contests before IOI, by medal",
    subtitle = "0 for participants with no CF activity; all IOI participants in denominator",
    x = NULL, y = "Mean contests before IOI"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_exp_months_z <- ggplot(exp_summary_z, aes(x = year, y = mean_months,
                                             color = result_unified, group = result_unified)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_color_manual(values = medal_colours, name = NULL) +
  scale_x_continuous(breaks = 2011:2025) +
  labs(
    title = "Months on Codeforces before IOI, by medal",
    subtitle = "0 for participants with no CF activity; all IOI participants in denominator",
    x = "IOI year", y = "Mean months since CF registration"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_exp_combined_z <- p_exp_contests_z / p_exp_months_z +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(p_exp_combined_z)
ggsave(
  filename = file.path(cf_path, "output", "cf_experience_by_medal_year_zeros.png"),
  plot = p_exp_combined_z,
  width = 10,
  height = 10,
  dpi = 300
)

# Scatter: log(CF friend-of count) vs score percentile ----
# One obs per contestant (friend-of count is a current snapshot, not year-varying).
# Use best-ever within-year score percentile as the performance measure.
ioi_friend_scatter <- ioi %>%
  filter(!is.na(cf_friend_of_count), !is.na(score)) %>%
  group_by(year) %>%
  mutate(score_pct = percent_rank(score) * 100) %>%
  ungroup() %>%
  group_by(contestant) %>%
  slice_max(score_pct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(log_friend_of = log(cf_friend_of_count))

# Label the top outlier by friend count
top_label <- ioi_friend_scatter %>%
  slice_max(cf_friend_of_count, n = 1)

p_friend_score <- ggplot(ioi_friend_scatter,
                         aes(x = log_friend_of, y = score_pct)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_text(data = top_label,
            aes(label = contestant), hjust = 1.1, size = 3, color = "grey30") +
  labs(
    title = "Log CF friend-of count vs IOI score percentile",
    subtitle = "One obs per contestant (best-ever within-year percentile)",
    x     = "log(CF friend-of count)",
    y     = "Best score percentile"
  ) +
  theme_minimal()

print(p_friend_score)
ggsave(
  filename = file.path(cf_path, "output", "log_friend_of_vs_score_pct.png"),
  plot     = p_friend_score,
  width    = 8,
  height   = 6,
  dpi      = 300
)

# Scatter: CF contribution vs best performance percentile ----
ioi_contrib_scatter <- ioi %>%
  filter(!is.na(cf_contribution), !is.na(score)) %>%
  group_by(year) %>%
  mutate(score_pct = percent_rank(score) * 100) %>%
  ungroup() %>%
  group_by(contestant) %>%
  slice_max(score_pct, n = 1, with_ties = FALSE) %>%
  ungroup()

top_contrib_label <- ioi_contrib_scatter %>%
  slice_max(cf_contribution, n = 3)

p_contrib_score <- ggplot(ioi_contrib_scatter,
                          aes(x = cf_contribution, y = score_pct)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  geom_text(data = top_contrib_label,
            aes(label = contestant), hjust = -0.1, size = 3, color = "grey30") +
  labs(
    title   = "CF contribution vs best IOI score percentile",
    subtitle = "One obs per contestant (best-ever within-year percentile)",
    x       = "CF contribution",
    y       = "Best score percentile"
  ) +
  theme_minimal()

print(p_contrib_score)
ggsave(
  filename = file.path(cf_path, "output", "cf_contribution_vs_score_pct.png"),
  plot     = p_contrib_score,
  width    = 8,
  height   = 6,
  dpi      = 300
)

# How many participants?
ioi_unique <- n_distinct(ioi$contestant) #3522

# ── Build matched dataset: each IOI participation matched to its year's rating ─
# For each row (contestant-year), extract the CF rating for THAT specific year.
ioi_matched <- ioi %>%
  filter(!is.na(handle)) %>%
  pivot_longer(cols = starts_with("rating_"),
               names_to = "rating_year", values_to = "rating_val",
               names_prefix = "rating_") %>%
  mutate(rating_year = as.integer(rating_year)) %>%
  filter(rating_year == year) %>%
  rename(cf_rating = rating_val) %>%
  select(-rating_year) %>%
  mutate(
    cf_rating = replace_na(cf_rating, 0),
    result_unified = case_when(
      result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
      result %in% c("Silver", "Silver*")            ~ "Silver",
      result %in% c("Bronze", "Bronze*")             ~ "Bronze",
      TRUE ~ "No Award"
    )
  )

# Version without zero-rating participants
ioi_nonzero <- ioi_matched %>% filter(cf_rating > 0)

cat("Matched dataset:", nrow(ioi_matched), "rows |",
    "Non-zero rating:", nrow(ioi_nonzero), "rows\n")

# ────────────────────────────────────────────────────────────────────────────
### FIGURES ----
# ────────────────────────────────────────────────────────────────────────────

# ── 1. FIGURES DROPPING ZERO RATING ──────────────────────────────────────────

# Scatterplot: score vs rating (no zeros, all years)
p_nz <- ggplot(ioi_nonzero, aes(x = cf_rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "IOI Score vs Codeforces Rating (Excluding Zero Ratings, 2011-2025)",
       x = "Codeforces Rating (1 month before IOI)", y = "IOI Score") +
  theme_minimal()
ggsave(file.path(cf_path, "output", "ioi_score_vs_rating_nonzero.png"),
       plot = p_nz, width = 10, height = 6, dpi = 300)

# Per year (no zeros)
p_nz_year <- ggplot(ioi_nonzero, aes(x = cf_rating, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "IOI Score vs Codeforces Rating by Year (Excluding Zero Ratings)",
       x = "Codeforces Rating", y = "IOI Score") +
  theme_minimal()
ggsave(file.path(cf_path, "output", "ioi_score_vs_rating_by_year_nonzero.png"),
       plot = p_nz_year, width = 12, height = 10, dpi = 300)

# Boxplot by result (no zeros)
p_box_nz <- ggplot(ioi_nonzero, aes(x = result_unified, y = cf_rating, fill = result_unified)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Codeforces Rating by IOI Result (Excluding Zero Ratings, 2011-2025)",
       x = "IOI Result", y = "Codeforces Rating") +
  theme_minimal() + theme(legend.position = "none")
ggsave(file.path(cf_path, "output", "rating_by_result_nonzero.png"),
       plot = p_box_nz, width = 10, height = 6, dpi = 300)

# Scatter by result (no zeros)
p_scatter_nz <- ggplot(ioi_nonzero, aes(x = cf_rating, y = score, color = result_unified)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "IOI Score vs CF Rating by Medal Type (Excluding Zero Ratings)",
       x = "Codeforces Rating", y = "IOI Score", color = "Medal") +
  theme_minimal()
ggsave(file.path(cf_path, "output", "score_vs_rating_by_result_nonzero.png"),
       plot = p_scatter_nz, width = 10, height = 6, dpi = 300)

cat("Saved 4 non-zero rating figures\n")

# ── 2. IOI SCORE VS MONTHS SINCE CF ACCOUNT CREATION ────────────────────────

# IOI competition start dates (from 1_datacollection.py)
ioi_dates <- tibble(
  year = 2011:2025,
  ioi_date = as.Date(c("2011-07-22", "2012-09-23", "2013-07-06", "2014-07-13",
                       "2015-07-26", "2016-08-12", "2017-07-28", "2018-09-01",
                       "2019-08-04", "2020-09-13", "2021-06-19", "2022-08-07",
                       "2023-08-28", "2024-09-01", "2025-07-27"))
)

ioi_months <- ioi_matched %>%
  filter(!is.na(cf_registration_date)) %>%
  left_join(ioi_dates, by = "year") %>%
  mutate(
    cf_reg_date = as.Date(cf_registration_date),
    months_since_creation = as.numeric(difftime(ioi_date, cf_reg_date, units = "days")) / 30.44
  ) %>%
  filter(months_since_creation > 0)  # only accounts created before IOI

p_months <- ggplot(ioi_months, aes(x = months_since_creation, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~ year, ncol = 3) +
  labs(title = "IOI Score vs Months Since CF Account Creation",
       x = "Months between CF Account Creation and IOI",
       y = "IOI Score") +
  theme_minimal()
ggsave(file.path(cf_path, "output", "ioi_score_vs_months_since_creation.png"),
       plot = p_months, width = 12, height = 10, dpi = 300)

# Same plot, all years combined
p_months_all <- ggplot(ioi_months, aes(x = months_since_creation, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "IOI Score vs Months Since CF Account Creation (All Years)",
       x = "Months between CF Account Creation and IOI",
       y = "IOI Score") +
  theme_minimal()
ggsave(file.path(cf_path, "output", "ioi_score_vs_months_since_creation_all.png"),
       plot = p_months_all, width = 10, height = 6, dpi = 300)

cat("Saved months-since-creation figures\n")

