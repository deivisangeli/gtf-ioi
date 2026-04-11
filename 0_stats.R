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

# How many participants?
ioi_unique <- n_distinct(ioi$contestant) #3522

# How many of them have an account on Codeforces?
ioi_unique_cf <- n_distinct(ioi_with_cf$contestant) #2062
