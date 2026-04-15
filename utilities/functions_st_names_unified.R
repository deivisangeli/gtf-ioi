################################################################################
# functions_st_names_unified.R
# Unified name standardization - NO MANUAL CHARACTER MAPPINGS
# Uses stringi for automatic Unicode transliteration (handles ALL scripts)
################################################################################

# Required packages: stringi, dplyr, tidyr, data.table (optional), combinat (optional)

################################################################################
# CORE TRANSLITERATION - Replaces ConvertLatinCharsbyChars entirely
################################################################################

#' Convert any script to ASCII Latin (automatic - no manual mapping needed)
#' Handles: Cyrillic, Greek, Arabic, Chinese, Japanese, Korean, Vietnamese,
#'          ALL accented Latin characters (Portuguese, Spanish, French, German, etc.)
#' @param x Character vector
#' @return Character vector in ASCII
convert_to_ascii <- function(x) {
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Package 'stringi' is required. Install with install.packages('stringi').")
  }
  # Two-step process for maximum coverage:
  # 1. Any-Latin: converts non-Latin scripts (Cyrillic, Greek, etc.) to Latin
  # 2. Latin-ASCII: removes diacritics from Latin characters
  stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
}

#' Apply ASCII conversion to all character columns in a data.frame
#' Direct replacement for ConvertLatinCharsbyChars()
#' @param df Data frame
#' @param include_colnames Also convert column names? Default TRUE
#' @return Data frame with ASCII-only text
convert_df_to_ascii <- function(df, include_colnames = TRUE) {
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame")
  }
  
  # Convert column names
  if (include_colnames) {
    names(df) <- convert_to_ascii(names(df))
  }
  
  # Convert character columns
  char_cols <- sapply(df, is.character)
  df[char_cols] <- lapply(df[char_cols], convert_to_ascii)
  
  df
}

################################################################################
# NAME STANDARDIZATION
################################################################################

#' Full name standardization pipeline
#' @param x Character vector of names
#' @return Standardized names (lowercase, ASCII, single spaces, letters only)
standardize_name <- function(x) {
  x |>
    tolower() |>
    convert_to_ascii() |>
    gsub(pattern = "-", replacement = " ", x = _) |>
    gsub(pattern = "[^a-z ]", replacement = "", x = _) |>
    gsub(pattern = "\\s+", replacement = " ", x = _) |>
    trimws()
}

#' Remove common name prefixes/titles
#' @param x Character vector
#' @return Names without prefixes
remove_name_prefix <- function(x) {
  prefixes <- "^\\b(dr|prof|mr|mrs|ms|miss|sir|dame|rev|fr|sr|sra|don|dona)\\b\\.?\\s*"
  gsub(prefixes, "", x, ignore.case = TRUE)
}

################################################################################
# COMPOUND SURNAME HANDLING
# Preserves: da, de, do, das, dos, van, von, del, della, di, la, le, el, al, bin, ibn
################################################################################

# Extended list of surname prefixes (international)
.COMPOUND_PREFIXES <- c(
  # Portuguese/Spanish
  "da", "de", "do", "das", "dos", "del", "dela", "della", "di",
  # Dutch/German
  "van", "von", "vander", "vonder",
  # French
  "la", "le", "du", "des",
  # Arabic
  "al", "el", "bin", "ibn", "ben", "abd",
  # Italian
  "lo", "li"
)

#' Tokenize name preserving compound surnames
#' @param full_name Single name string
#' @param prefixes Vector of compound prefixes to preserve
#' @return Character vector of tokens
tokenize_preserving_compounds <- function(full_name, prefixes = .COMPOUND_PREFIXES) {
  words <- unlist(strsplit(full_name, "\\s+"))
  if (length(words) == 0L) return(character(0))
  
  out <- character(0)
  i <- 1L
  n <- length(words)
  
  while (i <= n) {
    w <- words[i]
    # Check if current word is a prefix AND there's a next word
    if (w %in% prefixes && i < n) {
      # Combine prefix with next word
      out <- c(out, paste(words[i], words[i + 1], sep = " "))
      i <- i + 2L
    } else {
      out <- c(out, w)
      i <- i + 1L
    }
  }
  out
}

#' Get unique tokens from a name
#' @param x Character vector of names
#' @return List of unique token vectors
tokenize <- function(x) {
  lapply(x, function(name) unique(strsplit(name, "\\s+")[[1]]))
}

################################################################################
# NAME PAIR GENERATION (for matching/blocking)
################################################################################

#' Generate ordered name pairs (i < j) - preserves token order
#' @param data Data frame
#' @param name_column Name of column containing names
#' @param output_column Name of output column for pairs
#' @return Expanded data frame with name pairs
generate_name_pairs <- function(data, name_column, output_column = "name_pair") {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) {
    stop("Packages 'dplyr' and 'tidyr' are required.")
  }
  
  extract_pairs <- function(full_name) {
    words <- unique(tokenize_preserving_compounds(full_name))
    n <- length(words)
    if (n < 2) return(full_name)
    
    # All combinations where i < j (preserves order)
    pairs <- unlist(lapply(1:(n - 1), function(i) {
      sapply((i + 1):n, function(j) paste(words[c(i, j)], collapse = " "))
    }))
    pairs
  }
  
  data |>
    dplyr::mutate(.temp_pairs = lapply(.data[[name_column]], extract_pairs)) |>
    tidyr::unnest(cols = .temp_pairs) |>
    dplyr::rename(!!output_column := .data$.temp_pairs)
}

# Alias for backwards compatibility
generate_ordered_name_pairs <- generate_name_pairs
generate_name_pairs_unordered <- generate_name_pairs

#' Generate all permutations of name tokens (for fuzzy matching)
#' @param df Data frame
#' @param col_name Column with names
#' @return Expanded data frame with permutations
generate_name_permutations_long <- function(df, col_name = "fullname_md") {
  if (!requireNamespace("combinat", quietly = TRUE)) {
    stop("Package 'combinat' is required. Install with install.packages('combinat').")
  }
  
  perms_list <- lapply(df[[col_name]], function(name_string) {
    parts <- tokenize_preserving_compounds(name_string)
    if (length(parts) <= 1) return(parts)
    perms <- combinat::permn(parts)
    vapply(perms, function(x) paste(x, collapse = " "), character(1))
  })
  
  ids <- rep(seq_len(nrow(df)), times = lengths(perms_list))
  out <- df[ids, , drop = FALSE]
  out$permutation <- unlist(perms_list, use.names = FALSE)
  out
}

#' Order name tokens alphabetically
#' @param names Character vector of names
#' @return Names with tokens in alphabetical order
ordered_names <- function(names) {
  sapply(names, function(name) {
    toks <- tokenize_preserving_compounds(name)
    paste(sort(toks), collapse = " ")
  })
}

################################################################################
# COMPANY NAME STANDARDIZATION
################################################################################

#' Standardize company names (removes legal suffixes, normalizes)
#' @param x Character vector of company names
#' @return Standardized company names
standardize_company <- function(x) {
  # Legal entity suffixes to remove (international)
  stopwords <- c(
    # English
    "inc", "corp", "corporation", "llc", "ltd", "limited", "co", "company", "plc",
    # Spanish/Portuguese
    "s\\.?a", "s\\.?l", "s\\.?r\\.?l", "s\\.?a\\.?s", "cia", "ltda", "eireli", "mei", "epp",
    # French
    "sarl", "sas", "societe", "société",
    # German
    "gmbh", "ag", "ohg", "gbr", "kg",
    # Other
    "bv", "nv", "pty", "pte"
  )
  
  pattern <- paste0("\\b(", paste(stopwords, collapse = "|"), ")\\.?\\b")
  
  x |>
    tolower() |>
    convert_to_ascii() |>
    gsub(pattern = pattern, replacement = "", x = _, ignore.case = TRUE) |>
    gsub(pattern = "-", replacement = " ", x = _) |>
    gsub(pattern = "[^a-z0-9 ]", replacement = "", x = _) |>
    gsub(pattern = "\\s+", replacement = " ", x = _) |>
    trimws()
}

################################################################################
# LINKEDIN URL UTILITIES
################################################################################

clean_linkedin_url <- function(url) {
  url |>
    sub(pattern = "^https?://(www\\.)?", replacement = "", x = _) |>
    sub(pattern = "^([a-z]{2,3}\\.)?linkedin\\.com", replacement = "linkedin.com", x = _) |>
    sub(pattern = "\\?.*$", replacement = "", x = _) |>
    gsub(pattern = "/$", replacement = "", x = _)
}

convert_linkedin_url <- function(url) {
  slug <- sub(".*/company/", "", url)
  paste0("http://linkedin.com/company/", slug)
}

################################################################################
# DEMONSTRATION / TESTING
################################################################################

if (FALSE) {
  # Test convert_to_ascii with various scripts
  test_names <- c(
    "José García",           # Spanish
    "François Müller",       # French/German
    "Владимир Путин",        # Cyrillic
    "Αλέξανδρος",           # Greek
    "محمد علي",              # Arabic
    "田中太郎",              # Japanese
    "João da Silva",         # Portuguese
    "Nguyễn Văn A"           # Vietnamese
  )
  
  print(convert_to_ascii(test_names))
  # Expected output: all in ASCII Latin characters
  
  # Test standardize_name
  print(standardize_name("JOSÉ   da   SILVA-SANTOS"))
  # Expected: "jose da silva santos"
  
  # Test compound preservation
  print(tokenize_preserving_compounds("maria van der berg"))
  # Expected: c("maria", "van der", "berg")
  
  # Test DataFrame conversion (replaces ConvertLatinCharsbyChars)
  df_test <- data.frame(
    nome = c("João", "María", "François"),
    cidade = c("São Paulo", "México", "Paris"),
    stringsAsFactors = FALSE
  )
  print(convert_df_to_ascii(df_test))
}
