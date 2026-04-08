#-------------------------------------------------------------------------------
# GTF: IOI -- stats
# Nov 2025
# Author: Thais Takeuchi
#-------------------------------------------------------------------------------

# clean environment
rm(list=ls())

### Load data
## Define path
Sys.setenv(db_path = "C:/Users/thtak/Globtalent Dropbox")
db_path <- Sys.getenv("db_path") ### Dropbox path
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
# boxplot ----
ioi_long <- ioi_long %>%
  mutate(result_unified = case_when(
    result %in% c("Gold", "Gold (3rd)", "Gold*") ~ "Gold",
    result %in% c("Silver", "Silver*") ~ "Silver",
    result %in% c("Bronze", "Bronze*") ~ "Bronze",
    TRUE ~ result
  ))

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

# Display and save
print(p_result)
ggsave(
  filename = file.path(cf_path, "output", "rating_by_result.png"),
  plot = p_result,
  width = 10,
  height = 6,
  dpi = 300
)

# scatterplot ----
## Scatterplot: Rating vs Score colored by Result (all years)
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

# Display and save
print(p_scatter_result)
ggsave(
  filename = file.path(cf_path, "output", "score_vs_rating_by_result.png"),
  plot = p_scatter_result,
  width = 10,
  height = 6,
  dpi = 300
)

## Scatterplot: Rating vs Score colored by Result, separated by year
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

# Display and save
print(p_scatter_result_year)
ggsave(
  filename = file.path(cf_path, "output", "score_vs_rating_by_result_by_year.png"),
  plot = p_scatter_result_year,
  width = 14,
  height = 12,
  dpi = 300
)

# How many participants?
ioi_unique <- n_distinct(ioi$contestant) #3522

# How many of them have an account on Codeforces?
oi <- ioi_unique_cf %>% 
  filter(!is.na(ioi_unique_cf$handle)) #2062 
