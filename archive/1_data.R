# GTF
# Thais Takeuchi (08/2025)
# IOI - share Code Forces


# clean environment
rm(list = ls())

# get the necessary packages
pacman::p_load(readxl, dplyr, ggplot2,openxlsx, stargazer, haven, stringi,
               estimatr, pscl, sandwich, tidyr, binsreg, ggforce, lmtest, purrr,
               stringr)

setwd(Sys.getenv("db_path"))


df <- read_excel("GTAllocation/Data/IOI/IOI_2011_2025.xlsx")

df <- df %>%
  mutate(
    Result = as.character(Result),
    Result = str_squish(Result),                 # tira espaços extras
    Result = str_remove_all(Result, fixed("*")), # remove asteriscos
    Result = str_remove_all(Result, "\\s*\\(.*?\\)"), # remove parênteses e conteúdo: (1st), (2nd)...
    Result = case_when(
      str_detect(Result, regex("^gold\\b",   ignore_case = TRUE)) ~ "Gold",
      str_detect(Result, regex("^silver\\b", ignore_case = TRUE)) ~ "Silver",
      str_detect(Result, regex("^bronze\\b", ignore_case = TRUE)) ~ "Bronze",
      str_detect(Result, regex("^no\\s*award$", ignore_case = TRUE)) ~ "No Award",
      str_detect(Result, regex("^hm\\b$", ignore_case = TRUE)) ~ "HM",
      
      TRUE ~ Result
    ),
    Result = factor(Result, levels = c("Gold", "Silver", "Bronze", "No Award", "HM"))
  ) %>% 
  janitor::clean_names()

# Descriptive statistics ----

# Drop "No Award"
df_awards <- df %>%
  filter(result != "No Award")

# Share with cf_link by year and medal
share_by_year_result <- df_awards %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  group_by(year, result) %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total,
    .groups = "drop"
  )

# Share with cf_link by year (overall)
share_by_year_total <- df_awards %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  group_by(year) %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total,
    .groups = "drop"
  )

# Show results
share_by_year_result
share_by_year_total

share_by_year_total <- share_by_year_total %>%
  mutate(share_pct = round(share * 100, 1))

share_overall_total <- df_awards %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total
  )

share_overall_total
share_overall_total <- share_overall_total %>%
  mutate(share_pct = round(share * 100, 1))


#------------------------------------------------------------------------------#
# VERSION 2: Exclude HM
#------------------------------------------------------------------------------#
# Descriptive statistics VERSION 2 ----
df_awards_v2 <- df %>%
  filter(result != c("No Award", "HM"))

# Share with cf_link by year and medal (VERSION 2)
share_by_year_result_v2 <- df_awards_v2 %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  group_by(year, result) %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total,
    .groups = "drop"
  )

# Share with cf_link by year (overall) (VERSION 2)
share_by_year_total_v2 <- df_awards_v2 %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  group_by(year) %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total,
    .groups = "drop"
  )

# Show results VERSION 2
cat("=== VERSION 2 RESULTS (Excluding HM also) ===\n\n")

cat("Share by Year and Result (V2):\n")
print(share_by_year_result_v2)

cat("\nShare by Year Total (V2):\n")
print(share_by_year_total_v2)

share_by_year_total_v2 <- share_by_year_total_v2 %>%
  mutate(share_pct = round(share * 100, 1))

share_overall_total_v2 <- df_awards_v2 %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total
  )

share_overall_total_v2 <- share_overall_total_v2 %>%
  mutate(share_pct = round(share * 100, 1))

cat("\nOverall Share (V2):\n")
print(share_overall_total_v2)

# Additional comparison statistics
cat("\n=== COMPARISON: V1 vs V2 ===\n")

# Original data (V1)
share_overall_total_v1 <- df_awards %>%
  mutate(has_cf = !is.na(cf_link) & cf_link != "") %>%
  summarise(
    total = n(),
    with_cf = sum(has_cf),
    share = with_cf / total,
    share_pct = round(share * 100, 1)
  )

cat("V1 (All years 2011-2025):\n")
cat(paste("Total contestants with awards:", share_overall_total_v1$total, "\n"))
cat(paste("With CodeForces link:", share_overall_total_v1$with_cf, "\n"))
cat(paste("Share with CF link:", share_overall_total_v1$share_pct, "%\n\n"))

cat("V2 (Excluding HM):\n")
cat(paste("Total contestants with awards:", share_overall_total_v2$total, "\n"))
cat(paste("With CodeForces link:", share_overall_total_v2$with_cf, "\n"))
cat(paste("Share with CF link:", share_overall_total_v2$share_pct, "%\n\n"))

