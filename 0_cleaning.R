#-------------------------------------------------------------------------------
# GTF: IOI -- cleaning
# Dec 2025
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

# 1. Filtrar quando TODAS as colunas rating_ são zero (sempre zero)
ioi_zero_rating <- ioi %>% 
  filter(if_all(starts_with("rating_"), ~ . == 0))

# handle: duplicates
dup_handles <- ioi_zero_rating$handle[duplicated(ioi_zero_rating$handle)]
unique_dup_handles <- unique(dup_handles)
unique_dup_handles 

# handles: unique
unique <- ioi_zero_rating %>%
  count(handle) %>%
  filter(n == 1) %>%
  pull(handle)
unique

ioi_zero_rating <- ioi_zero_rating %>%
  mutate(
    handle = case_when(
      # duplicated handles
      handle == "divineneos" ~ "MudoBog",
      handle == "Ingugugus" ~ "Ingus",
      handle == "rudy102" ~ "yarek",
      handle == "zscefn" ~ "kllp",
      handle == "kristapuciitis" ~ "how_to_become_purple",
      handle == "andresrperezr" ~ "AndresRPerez",
      handle == "aitch" ~ "_h_",
      handle == "riodo" ~ "Riodo",
      handle == "navas73" ~ "Navas73",
      handle == "Rudy1444" ~ "Rudy1337",
      handle == "babin" ~ "miro",
      handle == "david_garcia" ~ "dmga44",
      handle == "egor.lifar" ~ "kiyotaka",
      handle == "PathToMaster" ~ "SoyTuHusbando",
      handle == "saba2000" ~ "bruhh",
      handle == "msaska9" ~ "Zoli9",
      handle == "Rezwan.Arefin01" ~ "RezwanArefin01",
      handle == "SebastianMestre" ~ "estoy-re-sebado",
      handle == "__.__" ~ "Tutis",
      handle == "sanroylozan" ~ "mraron",
      handle == "user202729_" ~ "user202729",
      handle == "JettyOller" ~ "Merripium",
      handle == "ISeeDS" ~ "_kernel_",
      handle == "SleepyShashwat" ~ "shashwatchan",
      handle == "Besher." ~ "sys",
      handle == "The_Aragorn" ~ "DilshodbekX",
      handle == "Mprimus" ~ "sm1lee",
      handle == "tqbfjotld" ~ "tqbfjotld",
      handle == "A.D." ~ "IGM_NEXT_YEAR",
      handle == "BERNARD" ~ "too_rusty",
      handle == "erray" ~ "ErrrrrrrayReis37",
      handle == "AHappyPotato" ~ "potatoo",
      handle == "TorresianCrow" ~ "Naseer",
      handle == "tolbi" ~ "kaIimm",
      handle == "fve5" ~ "francesco",
      handle == "otalp" ~ "Vlad288",
      handle == "yuan_li" ~ "eolienne",
      handle == "briX1210" ~ "bribritt",
      handle == "DinoHadzic" ~ "-adhd-",
      handle == "joseandreslemus" ~ "pupilemus",
      handle == "valeriu" ~ "ntherner",
      handle == "Nummer_64" ~ "carcinisation",
      handle == "VMaksimoski008" ~ "-VM-",
      handle == "viliamgott" ~ "Sanozvrz",

      # unique handles      
      handle == "2021" ~ "998batrr",
      handle == "998kover" ~ "Sanozvrz",
      handle == "Ali.sh" ~ "ToTLeS",
      handle == "Computerbox_" ~ "computerbox",
      handle == "DriciHachem" ~ "MohamedHa4em",
      handle == "GastonFontenla" ~ "gfonn",
      handle == "Hojamuhammet" ~ "MuhammetH",
      handle == "Kubalionzzale" ~ "God_Ra",
      handle == "Rewinding" ~ "heuristica",
      handle == "Shrimb" ~ "EspectroOcteto",
      handle == "SpecterByte" ~ "Sanozvrz",
      handle == "Yellowgood" ~ "leaked",
      handle == "abozzorov" ~ "Zzzzzzz...",
      handle == "adelelsenusi" ~ "Fadel_Elsenusi",
      handle == "blushinghentaigirl" ~ "bellydancer",
      handle == "celin" ~ "jcelin",
      handle == "doowey" ~ "realcomplex",
      handle == "fedoseev.timofey" ~ "StupidGuy13",
      handle == "icosahedron" ~ "leaked",
      handle == "ismail-but-noob" ~ "ismailfateen",
      handle == "jnmtz111__" ~ "martinezgjuan",
      handle == "phixyma" ~ "mcouplet",
      handle == "rusinsr" ~ "rusins",
      handle == "teraqqq" ~ "I_love_kirill22",
      handle == "thuustalu" ~ "-is-this-fft-",
      handle == "xavier158" ~ "xavier",
      handle == "yz_" ~ "tzaph_",
      handle == "znirzejskwarka" ~ "znirzej",
      TRUE ~ handle  # keep the original handle
    ),
    cf_link = case_when(
      cf_link == "http://codeforces.com/profile/divineneos" ~ "http://codeforces.com/profile/MudoBog",
      cf_link == "http://codeforces.com/profile/Ingugugus" ~ "http://codeforces.com/profile/Ingus",
      cf_link == "http://codeforces.com/profile/rudy102" ~ "http://codeforces.com/profile/yarek",
      cf_link == "http://codeforces.com/profile/zscefn" ~ "http://codeforces.com/profile/kllp",
      cf_link == "http://codeforces.com/profile/kristapuciitis" ~ "http://codeforces.com/profile/how_to_become_purple",
      cf_link == "http://codeforces.com/profile/andresrperezr" ~ "http://codeforces.com/profile/AndresRPerez",
      cf_link == "http://codeforces.com/profile/aitch" ~ "http://codeforces.com/profile/_h_",
      cf_link == "http://codeforces.com/profile/riodo" ~ "http://codeforces.com/profile/Riodo",
      cf_link == "http://codeforces.com/profile/navas73" ~ "http://codeforces.com/profile/Navas73",
      cf_link == "http://codeforces.com/profile/Rudy1444" ~ "http://codeforces.com/profile/Rudy1337",
      cf_link == "http://codeforces.com/profile/babin" ~ "http://codeforces.com/profile/miro",
      cf_link == "http://codeforces.com/profile/david_garcia" ~ "http://codeforces.com/profile/dmga44",
      cf_link == "http://codeforces.com/profile/egor.lifar" ~ "http://codeforces.com/profile/kiyotaka",
      cf_link == "http://codeforces.com/profile/PathToMaster" ~ "http://codeforces.com/profile/SoyTuHusbando",
      cf_link == "http://codeforces.com/profile/saba2000" ~ "http://codeforces.com/profile/bruhh",
      cf_link == "http://codeforces.com/profile/msaska9" ~ "http://codeforces.com/profile/Zoli9",
      cf_link == "http://codeforces.com/profile/Rezwan.Arefin01" ~ "http://codeforces.com/profile/RezwanArefin01",
      cf_link == "http://codeforces.com/profile/SebastianMestre" ~ "http://codeforces.com/profile/estoy-re-sebado",
      cf_link == "http://codeforces.com/profile/__.__" ~ "http://codeforces.com/profile/Tutis",
      cf_link == "http://codeforces.com/profile/sanroylozan" ~ "http://codeforces.com/profile/mraron",
      cf_link == "http://codeforces.com/profile/user202729_" ~ "http://codeforces.com/profile/user202729",
      cf_link == "http://codeforces.com/profile/JettyOller" ~ "http://codeforces.com/profile/Merripium",
      cf_link == "http://codeforces.com/profile/ISeeDS" ~ "http://codeforces.com/profile/_kernel_",
      cf_link == "http://codeforces.com/profile/SleepyShashwat" ~ "http://codeforces.com/profile/shashwatchan",
      cf_link == "http://codeforces.com/profile/Besher." ~ "http://codeforces.com/profile/sys",
      cf_link == "http://codeforces.com/profile/The_Aragorn" ~ "http://codeforces.com/profile/DilshodbekX",
      cf_link == "http://codeforces.com/profile/Mprimus" ~ "http://codeforces.com/profile/sm1lee",
      cf_link == "http://codeforces.com/profile/tqbfjotld" ~ "http://codeforces.com/profile/tqbfjotld",
      cf_link == "http://codeforces.com/profile/A.D." ~ "http://codeforces.com/profile/IGM_NEXT_YEAR",
      cf_link == "http://codeforces.com/profile/BERNARD" ~ "http://codeforces.com/profile/too_rusty",
      cf_link == "http://codeforces.com/profile/erray" ~ "http://codeforces.com/profile/ErrrrrrrayReis37",
      cf_link == "http://codeforces.com/profile/AHappyPotato" ~ "http://codeforces.com/profile/potatoo",
      cf_link == "http://codeforces.com/profile/TorresianCrow" ~ "http://codeforces.com/profile/Naseer",
      cf_link == "http://codeforces.com/profile/tolbi" ~ "http://codeforces.com/profile/kaIimm",
      cf_link == "http://codeforces.com/profile/fve5" ~ "http://codeforces.com/profile/francesco",
      cf_link == "http://codeforces.com/profile/otalp" ~ "http://codeforces.com/profile/Vlad288",
      cf_link == "http://codeforces.com/profile/yuan_li" ~ "http://codeforces.com/profile/eolienne",
      cf_link == "http://codeforces.com/profile/briX1210" ~ "http://codeforces.com/profile/bribritt",
      cf_link == "http://codeforces.com/profile/DinoHadzic" ~ "http://codeforces.com/profile/-adhd-",
      cf_link == "http://codeforces.com/profile/joseandreslemus" ~ "http://codeforces.com/profile/pupilemus",
      cf_link == "http://codeforces.com/profile/valeriu" ~ "http://codeforces.com/profile/ntherner",
      cf_link == "http://codeforces.com/profile/Nummer_64" ~ "http://codeforces.com/profile/carcinisation",
      cf_link == "http://codeforces.com/profile/VMaksimoski008" ~ "http://codeforces.com/profile/-VM-",
      cf_link == "http://codeforces.com/profile/viliamgott" ~ "http://codeforces.com/profile/Sanozvrz",

      cf_link == "http://codeforces.com/profile/2021" ~ "http://codeforces.com/profile/998batrr",
      cf_link == "http://codeforces.com/profile/998kover" ~ "http://codeforces.com/profile/Sanozvrz",
      cf_link == "http://codeforces.com/profile/Ali.sh" ~ "http://codeforces.com/profile/ToTLeS",
      cf_link == "http://codeforces.com/profile/Computerbox_" ~ "http://codeforces.com/profile/computerbox",
      cf_link == "http://codeforces.com/profile/DriciHachem" ~ "http://codeforces.com/profile/MohamedHa4em",
      cf_link == "http://codeforces.com/profile/GastonFontenla" ~ "http://codeforces.com/profile/gfonn",
      cf_link == "http://codeforces.com/profile/Hojamuhammet" ~ "http://codeforces.com/profile/MuhammetH",
      cf_link == "http://codeforces.com/profile/Kubalionzzale" ~ "http://codeforces.com/profile/God_Ra",
      cf_link == "http://codeforces.com/profile/Rewinding" ~ "http://codeforces.com/profile/heuristica",
      cf_link == "http://codeforces.com/profile/Shrimb" ~ "http://codeforces.com/profile/EspectroOcteto",
      cf_link == "http://codeforces.com/profile/SpecterByte" ~ "http://codeforces.com/profile/Sanozvrz",
      cf_link == "http://codeforces.com/profile/Yellowgood" ~ "http://codeforces.com/profile/leaked",
      cf_link == "http://codeforces.com/profile/abozzorov" ~ "http://codeforces.com/profile/Zzzzzzz...",
      cf_link == "http://codeforces.com/profile/adelelsenusi" ~ "http://codeforces.com/profile/Fadel_Elsenusi",
      cf_link == "http://codeforces.com/profile/blushinghentaigirl" ~ "http://codeforces.com/profile/bellydancer",
      cf_link == "http://codeforces.com/profile/celin" ~ "http://codeforces.com/profile/jcelin",
      cf_link == "http://codeforces.com/profile/doowey" ~ "http://codeforces.com/profile/realcomplex",
      cf_link == "http://codeforces.com/profile/fedoseev.timofey" ~ "http://codeforces.com/profile/StupidGuy13",
      cf_link == "http://codeforces.com/profile/icosahedron" ~ "http://codeforces.com/profile/leaked",
      cf_link == "http://codeforces.com/profile/ismail-but-noob" ~ "http://codeforces.com/profile/ismailfateen",
      cf_link == "http://codeforces.com/profile/jnmtz111__" ~ "http://codeforces.com/profile/martinezgjuan",
      cf_link == "http://codeforces.com/profile/phixyma" ~ "http://codeforces.com/profile/mcouplet",
      cf_link == "http://codeforces.com/profile/rusinsr" ~ "http://codeforces.com/profile/rusins",
      cf_link == "http://codeforces.com/profile/teraqqq" ~ "http://codeforces.com/profile/I_love_kirill22",
      cf_link == "http://codeforces.com/profile/thuustalu" ~ "http://codeforces.com/profile/-is-this-fft-",
      cf_link == "http://codeforces.com/profile/xavier158" ~ "http://codeforces.com/profile/xavier",
      cf_link == "http://codeforces.com/profile/yz_" ~ "http://codeforces.com/profile/tzaph_",
      cf_link == "http://codeforces.com/profile/znirzejskwarka" ~ "http://codeforces.com/profile/znirzej",
      TRUE ~ cf_link  # keep the original link
    )
  )

# check
ioi_zero_rating %>%
  filter(handle %in% c("MudoBog", "Ingus", "yarek", "kllp")) %>%
  select(handle, cf_link) %>%
  distinct()

# save
write_xlsx(ioi_zero_rating, path = file.path(cf_path, "Data", "ioi_zero_rating.xlsx"))



# 2. Filtrar quando PELO MENOS UMA coluna rating_ é zero -- 3,289 observations
ioi_at_least_one_zero <- ioi %>% 
  filter(if_any(starts_with("rating_"), ~ . == 0))





# Verificar duplicados baseado em TODAS essas colunas
colunas <- c("year", "country", "result", "score", "cf_link", 
             "source", "handle")

# Contar duplicados
duplicados <- ioi %>%
  group_by(across(all_of(colunas))) %>%
  filter(n() > 1) %>%
  ungroup()

# Ver quantos duplicados existem
nrow(duplicados)

# Ver quais são os duplicados
duplicados %>%
  arrange(year, contestant) %>%
  select(year, contestant, country, handle)


library(stringdist)
library(dplyr)

# 1. Normalizar nomes (remover acentos) para comparar
ioi <- ioi %>%
  mutate(contestant_normalized = stringi::stri_trans_general(contestant, "Latin-ASCII"))

# 2. Encontrar duplicados baseado no nome normalizado + ano
duplicados_nome <- ioi %>%
  group_by(year, contestant_normalized, country) %>%
  filter(n() > 1) %>%
  arrange(year, contestant_normalized) %>%
  select(year, contestant, contestant_normalized, country, handle)

duplicados_nome

# 3. Ver quantos casos existem
duplicados_nome %>%
  group_by(year, contestant_normalized) %>%
  summarise(
    nomes_diferentes = paste(unique(contestant), collapse = " | "),
    n_variantes = n(),
    .groups = "drop"
  ) %>%
  filter(n_variantes > 1)
















