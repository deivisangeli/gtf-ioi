#####################################################################

###In this script, I'll add some useful functions in order to map institutions

####################################################################

###Build a simpler version of the get_id function

###Get the ids of each institution
get_id_simple <- function(name) {
  # normaliza levemente, mas sem mexer demais
  q <- trimws(name)
  
  # 1) Busca ampla com `search` (mais tolerante a variações)
  tb1 <- try(
    oa_fetch(
      entity = "inst",               # "inst" == "institutions"
      search = q,                    # usa o mecanismo de busca do OpenAlex
      select = "id,display_name",    # resposta mais leve
      sort = "relevance_score:desc", # ordena por relevância quando há search
      per_page = 5
    ),
    silent = TRUE
  )
  if (!inherits(tb1, "try-error") && !is.null(tb1) && nrow(tb1) > 0) {
    return(tb1$id[1])
  }
  
  # 2) Frase exata entre aspas no campo display_name (às vezes ajuda)
  tb2 <- try(
    oa_fetch(
      entity = "inst",
      display_name.search = sprintf('"%s"', q),
      select = "id,display_name",
      sort = "relevance_score:desc",
      per_page = 5
    ),
    silent = TRUE
  )
  if (!inherits(tb2, "try-error") && !is.null(tb2) && nrow(tb2) > 0) {
    return(tb2$id[1])
  }
  
  # 3) Fallback bruto: saída em lista (crua) e pega o primeiro id se existir
  lst <- try(
    oa_fetch(
      entity = "inst",
      search = q,
      output = "list"
    ),
    silent = TRUE
  )
  if (!inherits(lst, "try-error") && length(lst) > 0 && !is.null(lst[[1]]$id)) {
    return(lst[[1]]$id)
  }
  
  # Nada encontrado
  return(NA_character_)
}




###Get the ids of each institution
get_id <- function(name) {
  # Light normalization without changing too much
  q <- trimws(name)
  
  # 1) Broad search with `search` (more tolerant to variations)
  tb1 <- try(
    oa_fetch(
      entity = "inst",               # "inst" == "institutions"
      search = q,                    # uses OpenAlex search mechanism
      select = "id,display_name",    # lighter response
      sort = "relevance_score:desc", # sorts by relevance when using search
      per_page = 5,
      mailto = "joaomcj28@gmail.com" 
    ),
    silent = TRUE
  )
  if (!inherits(tb1, "try-error") && !is.null(tb1) && nrow(tb1) > 0) {
    return(tb1$id[1])
  }
  
  # 2) Exact phrase in quotes in display_name field (sometimes helps)
  tb2 <- try(
    oa_fetch(
      entity = "inst",
      display_name.search = sprintf('"%s"', q),
      select = "id,display_name",
      sort = "relevance_score:desc",
      per_page = 5,
      mailto = "joaomcj28@gmail.com" 
    ),
    silent = TRUE
  )
  if (!inherits(tb2, "try-error") && !is.null(tb2) && nrow(tb2) > 0) {
    return(tb2$id[1])
  }
  
  # 3) Raw fallback: list output (raw) and get first id if it exists
  lst <- try(
    oa_fetch(
      entity = "inst",
      search = q,
      output = "list",
      mailto = "joaomcj28@gmail.com" 
    ),
    silent = TRUE
  )
  if (!inherits(lst, "try-error") && length(lst) > 0 && !is.null(lst[[1]]$id)) {
    return(lst[[1]]$id)
  }
  
  # Nothing found
  return(NA_character_)
}



###Create function to get both ID and country code for most relevant entry
get_id_and_country <- function(name) {
  # Light normalization without changing too much
  q <- trimws(name)
  
  # 1) Broad search with `search` (more tolerant to variations)
  tb1 <- try(
    oa_fetch(
      entity = "inst",               # "inst" == "institutions"
      search = q,                    # uses OpenAlex search mechanism
      select = "id,display_name,country_code",    # include country_code
      sort = "relevance_score:desc", # sorts by relevance when using search
      per_page = 5
    ),
    silent = TRUE
  )
  if (!inherits(tb1, "try-error") && !is.null(tb1) && nrow(tb1) > 0) {
    return(c(id = tb1$id[1], country_code = tb1$country_code[1], display_name = tb1$display_name[1]))
  }
  
  # 2) Exact phrase in quotes in display_name field (sometimes helps)
  tb2 <- try(
    oa_fetch(
      entity = "inst",
      display_name.search = sprintf('"%s"', q),
      select = "id,display_name,country_code",
      sort = "relevance_score:desc",
      per_page = 5
    ),
    silent = TRUE
  )
  if (!inherits(tb2, "try-error") && !is.null(tb2) && nrow(tb2) > 0) {
    return(c(id = tb2$id[1], country_code = tb2$country_code[1], display_name = tb2$display_name[1]))
  }
  
  # 3) Raw fallback: list output (raw) and get first id if it exists
  lst <- try(
    oa_fetch(
      entity = "inst",
      search = q,
      output = "list"
    ),
    silent = TRUE
  )
  if (!inherits(lst, "try-error") && length(lst) > 0 && !is.null(lst[[1]]$id)) {
    return(c(id = lst[[1]]$id, 
             country_code = ifelse(is.null(lst[[1]]$country_code), NA_character_, lst[[1]]$country_code),
             display_name = ifelse(is.null(lst[[1]]$display_name), NA_character_, lst[[1]]$display_name)))
  }
  
  # Nothing found
  return(c(id = NA_character_, country_code = NA_character_, display_name = NA_character_))
}

###Build a function with regular expressions for universities from the shanghai ranking






