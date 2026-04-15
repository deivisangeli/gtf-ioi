#####################################################################################

###Function that parse json files

#####################################################################################

# ====== HELPERS ======

# 1) Extrai bloco cercado por crases ```json ... ``` (aceita json/JSON/sem marcador)
.extract_fenced_json <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  m <- str_match(txt, regex("```\\s*(?:json|JSON)?\\s*([\\s\\S]*?)\\s*```", dotall = TRUE))
  if (!is.na(m[1,2])) trimws(m[1,2]) else NA_character_
}

# 2) Extrai o PRIMEIRO objeto { ... } balanceado, ignorando aspas e escapes
.extract_balanced_json <- function(txt) {
  if (is.na(txt) || !nzchar(txt)) return(NA_character_)
  start <- regexpr("\\{", txt)
  if (start < 0) return(NA_character_)
  s <- substring(txt, start)
  depth <- 0L; in_str <- FALSE; esc <- FALSE
  n <- nchar(s)
  for (i in seq_len(n)) {
    ch <- substr(s, i, i)
    if (esc) { esc <- FALSE; next }
    if (ch == "\\") { esc <- TRUE; next }
    if (ch == "\"") { in_str <- !in_str; next }
    if (!in_str) {
      if (ch == "{") depth <- depth + 1L
      else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) return(substr(s, 1, i))
      }
    }
  }
  NA_character_
}

# 3) Estratégia de extração preferindo bloco cercado; senão, objeto balanceado
extract_json <- function(txt) {
  cand <- .extract_fenced_json(txt)
  if (!is.na(cand)) return(cand)
  .extract_balanced_json(txt)
}

# 4) Conserta aspas (") DENTRO de strings JSON que não deveriam fechar a string
#    (heurística segura o suficiente p/ respostas com títulos citados entre aspas)
repair_inner_quotes <- function(jtxt) {
  if (is.na(jtxt) || !nzchar(jtxt)) return(jtxt)
  out <- character(nchar(jtxt))
  in_str <- FALSE; esc <- FALSE
  i <- 1L; n <- nchar(jtxt)

  getc <- function(pos) substr(jtxt, pos, pos)
  skip_ws <- function(k) { while (k <= n && grepl("\\s", getc(k))) k <- k + 1L; k }

  k <- 1L
  while (k <= n) {
    ch <- getc(k)
    if (esc) { out[i] <- ch; i <- i + 1L; esc <- FALSE; k <- k + 1L; next }
    if (ch == "\\") { out[i] <- ch; i <- i + 1L; esc <- TRUE; k <- k + 1L; next }
    if (ch == "\"") {
      if (!in_str) {
        in_str <- TRUE; out[i] <- ch; i <- i + 1L; k <- k + 1L; next
      } else {
        j <- skip_ws(k + 1L); nextch <- if (j <= n) getc(j) else ""
        if (nextch %in% c(",", "]", "}", ":") || j > n) {
          in_str <- FALSE; out[i] <- "\""; i <- i + 1L; k <- k + 1L; next
        } else {
          out[i] <- "\\\""; i <- i + 1L; k <- k + 1L; next
        }
      }
    }
    out[i] <- ch; i <- i + 1L; k <- k + 1L
  }
  paste0(out[seq_len(i - 1L)], collapse = "")
}

# 5) Parser “à prova de falhas”: extrai, repara, valida e devolve tibble 1 linha
parse_ai_json_one <- function(txt, keys = all_keys) {
  raw <- extract_json(txt)
  if (is.na(raw)) return(as_tibble(setNames(rep(NA, length(keys)), keys)))

  fixed <- repair_inner_quotes(raw)
  if (!jsonlite::validate(fixed)) {  # valida JSON antes de parsear
    return(as_tibble(setNames(rep(NA, length(keys)), keys)))
  }

  x <- jsonlite::fromJSON(fixed, simplifyVector = FALSE)

  # normalizações
  # NULL -> NA
  x[sapply(x, is.null)] <- NA

  # recognitions: lista/vetor -> string única separada por '; '
  if ("recognitions" %in% names(x)) {
    x$recognitions <- if (is.null(x$recognitions)) NA_character_ else
      paste(unlist(x$recognitions, use.names = FALSE), collapse = "; ")
  }

  # garantir todas as chaves
  for (k in keys) if (!k %in% names(x)) x[[k]] <- NA

  as_tibble(x[keys])
}



parse_ai_json_multi <- function(txt, keys = all_keys) {
  raw <- extract_json(txt)
  if (is.na(raw)) 
    return(tibble::as_tibble(setNames(rep(NA, length(keys)), keys)))
  
  fixed <- repair_inner_quotes(raw)
  if (!jsonlite::validate(fixed))
    return(tibble::as_tibble(setNames(rep(NA, length(keys)), keys)))
  
  # Não usar simplifyVector=FALSE (esse era o problema!)
  x <- jsonlite::fromJSON(fixed)
  
  # garantir todas as chaves
  for (k in keys) if (!k %in% names(x)) x[[k]] <- NA
  
  # converter NULL -> NA
  x[sapply(x, is.null)] <- NA
  
  # detectar N (número de linhas)
  # usamos a coluna company como "referência primária"
  if ("company" %in% names(x)) {
    if (is.list(x$company)) x$company <- unlist(x$company, use.names = FALSE)
    N <- length(x$company)
  } else {
    N <- 1
  }
  
  # colapsar fontes (sempre vira string única replicada N vezes)
  if ("sources" %in% names(x)) {
    s <- unlist(x$sources, recursive = TRUE, use.names = FALSE)
    s <- paste(s, collapse = "; ")
    x$sources <- rep(s, N)
  }
  
  # ajustar cada coluna para comprimento N e coerção de tipos
  x <- lapply(x, function(col) {
    
    # expandir listas → vetor
    if (is.list(col)) {
      col <- unlist(col, recursive = TRUE, use.names = FALSE)
    }
    
    # SE FOR character OU numeric misturados → converter TUDO para character
    if (!is.null(col)) {
      if (is.numeric(col)) {
        col <- as.character(col)
      }
    }
    
    # regra de reciclagem
    if (length(col) == 1) {
      return(rep(col, N))
    } else if (length(col) == N) {
      return(col)
    } else {
      return(rep(NA, N))
    }
  })
  
  tibble::as_tibble(x[keys])
}

