################################################################################
### Functions that standardize names  (with compound surname handling)
################################################################################

### Remove accents
remove_accents <- function(x) {
  iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
}

### Convert to LATIN (transliteration from scripts like Cyrillic, Greek, etc.)
convert_to_latin <- function(x) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("O pacote 'stringi' é necessário. Instale com install.packages('stringi').")
  }
  stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
}

### Standardize names
standardize_name <- function(x) {
  x <- tolower(x)
  x <- convert_to_latin(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("-", " ", x)
  x <- gsub("[^a-z ]", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  return(x)
}

standardize_company <- function(x) {
  x <- tolower(x)
  x <- convert_to_latin(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("-", " ", x)
  x <- gsub("[^a-z0-9 ]", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  return(x)
}


### Remove common prefixes like titles (e.g., Dr., Prof., Mr., etc.)
remove_name_prefix <- function(x) {
  gsub("^\\b(dr|dr\\.|prof|prof\\.|mr|mr\\.|mrs|mrs\\.|ms|ms\\.)\\b\\s*", "", x, ignore.case = TRUE)
}

### LinkedIn URL cleaners
clean_linkedin_url <- function(url) {
  # Remove "http(s)://", "www."
  url <- sub("^https?://(www\\.)?", "", url)
  
  # Remove parâmetros após "?"
  url <- sub("\\?.*$", "", url)
  
  # Remove barra final, se existir
  url <- gsub("/$", "", url)
  
  return(url)
}

convert_linkedin_url <- function(url) {
  slug <- sub(".*/company/", "", url)
  paste0("http://linkedin.com/company/", slug)
}

################################################################################
# NEW: Tokenizer that preserves compounds like:
# {da, de, do, das, dos, van} + next token  => single unit with space preserved
################################################################################
.compound_prefixes <- c("da","de","do","das","dos","van")

tokenize_preserving_compounds <- function(full_name, prefixes = .compound_prefixes) {
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

### Order name in alphabetical order (by tokens, preserving compounds)
ordered_names <- function(names) {
  sapply(names, function(name) {
    toks <- tokenize_preserving_compounds(name)
    toks <- sort(toks)
    paste(toks, collapse = " ")
  })
}

### Create combinations of two names (unordered within pair, then combined)
generate_name_pairs <- function(data, name_column, output_column = "name_pair") {

  extract_sorted_name_pairs <- function(full_name) {
    words <- tokenize_preserving_compounds(full_name)
    if (length(words) < 2) return(character(0))
    combos <- combn(words, 2, simplify = FALSE)
    sorted_pairs <- lapply(combos, function(pair) sort(pair))
    sapply(sorted_pairs, function(pair) paste(pair, collapse = " "))
  }

  data %>%
    mutate(.temp_pairs = lapply(!!sym(name_column), extract_sorted_name_pairs)) %>%
    unnest(.temp_pairs) %>%
    rename(!!output_column := .temp_pairs)
}

### Create combinations of two names preserving original order (i<j)
generate_ordered_name_pairs <- function(data, name_column, output_column = "name_pair") {

  extract_ordered_name_pairs <- function(full_name) {
    words <- unique(tokenize_preserving_compounds(full_name))
    n <- length(words)
    if (n < 2) return(full_name)
    pairs <- unlist(lapply(1:(n - 1), function(i) {
      sapply((i + 1):n, function(j) paste(words[c(i, j)], collapse = " "))
    }))
    pairs
  }

  out <- data[, .(temp_pair = extract_ordered_name_pairs(get(name_column))),
              by = setdiff(names(data), name_column)]
  setnames(out, "temp_pair", output_column)
  out
}

### Create unordered 2-word permutations (A B and B A), treating compounds as single tokens
generate_name_pairs_unordered <- function(data, name_column, output_column = "name_pair") {
  if (!requireNamespace("gtools", quietly = TRUE)) {
    stop("Package 'gtools' is required. Install it with install.packages('gtools')")
  }

  extract_permuted_name_pairs <- function(full_name) {
    words <- unique(tokenize_preserving_compounds(full_name))
    if (length(words) < 2) return(full_name)
    perms <- gtools::permutations(n = length(words), r = 2, v = words)
    apply(perms, 1, function(pair) paste(pair, collapse = " "))
  }

  data %>%
    mutate(.temp_pairs = lapply(!!sym(name_column), extract_permuted_name_pairs)) %>%
    unnest(.temp_pairs) %>%
    rename(!!output_column := .temp_pairs)
}

### All-name permutations (long), preserving compounds
generate_name_permutations_long <- function(dt, col_name = "fullname_md") {
  dt_copy <- data.table::copy(dt)
  dt_copy[, perms := lapply(get(col_name), function(name_string) {
    parts <- tokenize_preserving_compounds(name_string)
    if (length(parts) <= 1) return(parts)
    perms <- combinat::permn(parts)
    sapply(perms, function(x) paste(x, collapse = " "))
  })]
  long_dt <- dt_copy[, .(permutation = unlist(perms)), by = setdiff(names(dt_copy), "perms")]
  long_dt
}

### Country match helper (unchanged)
country_match_fun <- function(country_ug, country_md_fixed) {
  countries <- str_split(country_ug, ";\\s*")[[1]]
  current_group <- country_map[[country_md_fixed]]
  if (is.null(current_group)) return(0)
  as.integer(any(countries %in% current_group))
}
