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

# How many participants?
ioi_unique <- n_distinct(ioi$contestant) #3522

# How many of them have an account on Codeforces?
ioi_unique_cf <- n_distinct(ioi_with_cf$contestant) #2062
