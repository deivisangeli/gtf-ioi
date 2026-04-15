################################################################################
# functions_st_names_br.R
# Brazil-only name standardization & pairing utilities
# - Preserves compounds: {da,de,do,das,dos,van,von} + next token (space kept)
# - Pair generation keeps original order (i < j)
# - generate_name_pairs_unordered() intentionally returns ordered pairs for BR
################################################################################

## --- Dependencies (used via namespaces; no library() calls required) ---
# stringi  (transliteration)
# dplyr, tidyr (data frame ops)
# gtools   (permutations if you ever switch back)
# combinat (full permutations helper)

### Remove accents
remove_accents <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

### Convert to LATIN (transliteration from non-Latin scripts)
convert_to_latin <- function(x) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required. Install with install.packages('stringi').")
  }
  stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
}

### Standardize names
standardize_name <- function(x) {
  x <- tolower(x)
  x <- convert_to_latin(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")  # ensure ASCII
  x <- gsub("-", " ", x)
  x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  x
}

### Remove common prefixes like titles (e.g., Dr., Prof., Mr., etc.)
remove_name_prefix <- function(x) {
  gsub("^\\b(dr|dr\\.|prof|prof\\.|mr|mr\\.|mrs|mrs\\.|ms|ms\\.)\\b\\s*", "", x, ignore.case = TRUE)
}

### LinkedIn URL cleaners
clean_linkedin_url <- function(url) {
  gsub("/$", "", sub("^https?://(www\\.)?", "", url))
}

convert_linkedin_url <- function(url) {
  slug <- sub(".*/company/", "", url)
  paste0("http://linkedin.com/company/", slug)
}

################################################################################
# Tokenizer that preserves BR compounds:
# {da, de, do, das, dos, van, von} + next token  => single unit with space kept
################################################################################
.BR_COMPOUND_PREFIXES <- c("da","de","do","das","dos","van","von")

tokenize_preserving_compounds <- function(full_name, prefixes = .BR_COMPOUND_PREFIXES) {
  words <- unlist(strsplit(full_name, "\\s+"))
  if (length(words) == 0L) return(character(0))
  out <- character(0)
  i <- 1L; n <- length(words)
  while (i <= n) {
    w <- words[i]
    if (w %in% prefixes && i < n) {
      out <- c(out, paste(words[i], words[i + 1], sep = " "))
      i <- i + 2L
    } else {
      out <- c(out, w)
      i <- i + 1L
    }
  }
  out
}

### Order name tokens alphabetically (not used by pairings, kept for completeness)
ordered_names <- function(names) {
  sapply(names, function(name) {
    toks <- tokenize_preserving_compounds(name)
    toks <- sort(toks)
    paste(toks, collapse = " ")
  })
}

################################################################################
# Pair generation for Brazilian names: KEEP ORIGINAL ORDER (i < j)
################################################################################

# Returns all two-token combinations from left to right (i < j), preserving order.
# Example tokens: [joao, carlos, da silva, santos] ->
#   "joao carlos", "joao da silva", "joao santos", "carlos da silva",
#   "carlos santos", "da silva santos"
generate_name_pairs <- function(data, name_column, output_column = "name_pair") {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tidyr' are required.")
  }

  extract_ordered_name_pairs <- function(full_name) {
    words <- unique(tokenize_preserving_compounds(full_name))
    n <- length(words)
    if (n < 2) return(full_name)  # keep single token names verbatim
    unlist(lapply(1:(n - 1), function(i) {
      sapply((i + 1):n, function(j) paste(words[c(i, j)], collapse = " "))
    }))
  }

  data %>%
    dplyr::mutate(.temp_pairs = lapply(.data[[name_column]], extract_ordered_name_pairs)) %>%
    tidyr::unnest(cols = .temp_pairs) %>%
    dplyr::rename(!!output_column := .data$.temp_pairs)
}

# Historical name retained for compatibility; returns ORDERED pairs for BR.
generate_name_pairs_unordered <- function(data, name_column, output_column = "name_pair") {
  generate_name_pairs(data, name_column, output_column)
}

# If you ever need “preserve order (i<j)” explicitly:
generate_ordered_name_pairs <- function(data, name_column, output_column = "name_pair") {
  generate_name_pairs(data, name_column, output_column)
}

################################################################################
# All-token permutations (long), preserving compounds
################################################################################
generate_name_permutations_long <- function(df, col_name = "fullname_md") {
  if (!requireNamespace("combinat", quietly = TRUE)) {
    stop("Package 'combinat' is required. Install with install.packages('combinat').")
  }
  # build list-column of permutations
  perms_list <- lapply(df[[col_name]], function(name_string) {
    parts <- tokenize_preserving_compounds(name_string)
    if (length(parts) <= 1) return(parts)
    perms <- combinat::permn(parts)
    vapply(perms, function(x) paste(x, collapse = " "), character(1))
  })

  # expand long
  ids <- rep(seq_len(nrow(df)), times = lengths(perms_list))
  out <- df[ids, , drop = FALSE]
  out$permutation <- unlist(perms_list, use.names = FALSE)
  out
}

################################################################################
# Country match helper (unchanged; expects country_map in calling environment)
################################################################################
country_match_fun <- function(country_ug, country_md_fixed) {
  countries <- strsplit(country_ug, ";\\s*")[[1]]
  current_group <- country_map[[country_md_fixed]]
  if (is.null(current_group)) return(0L)
  as.integer(any(countries %in% current_group))
}
