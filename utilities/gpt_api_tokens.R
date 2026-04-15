################################################################################

###Chat gpt API + token count

################################################################################

chatGPT <- function(prompt, 
                    modelName = "gpt-4.1",
                    temperature = 0,
                    reasoning_effort = "medium",
                    apiKey = Sys.getenv("chatGPT_API_KEY")) {
  
  
  # Verifica se a chave da API está definida
  if (nchar(apiKey) < 1) {
    apiKey <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = apiKey)
  }
  
  # Verifica se o prompt é válido
  if (is.null(prompt) || nchar(prompt) == 0) {
    return("Prompt vazio ou inválido.")
  }
  
  # Monta o corpo da requisição
  body <- list(
    model = modelName,
    temperature = temperature,
    messages = list(list(
      role = "user", 
      content = prompt
    ))
  )
  
  # Executa a chamada à API com tratamento de erro
  tryCatch({
    response <- httr::POST(
      url = "https://api.openai.com/v1/chat/completions", 
      httr::add_headers(Authorization = paste("Bearer", apiKey)),
      httr::content_type_json(),
      encode = "json",
      body = body
    )
    
    content <- httr::content(response)
    
    # Verifica se há erro na resposta
    if (!is.null(content$error)) {
      return(paste("Erro da API:", content$error$message))
    }
    
    # Extrai o texto da resposta
    result <- content$choices[[1]]$message$content
    
    if (is.null(result) || length(result) == 0) {
      return("Resposta vazia da API.")
    }
    
    return(trimws(result))
    
  }, error = function(e) {
    return(paste("Erro ao acessar API:", e$message))
  })
}

chatGPT('Does the Nobel Prize has any age requirement? Answer only with a yes or no')



estimativa_custo_openai <- function(prompt_tokens, completion_tokens, n_prompts, model = "gpt-4o") {
  # Tabela de preços por mil tokens (USD)
  precos <- list(
    "gpt-4o" = list(input = 0.00000375, output = 0.000015),
    "gpt-4o-mini" = list(input = 0.00015, output = 0.0006),
    "gpt-3.5-turbo" = list(input = 0.001, output = 0.002),
    "gpt-4.1-mini" = list(input = 0.0004, output = 0.0016),
    "gpt-4.5" = list(input = 0.075, output = 0.15)
  )
  
  if (!model %in% names(precos)) {
    stop("Modelo inválido. Use: 'gpt-4o', 'gpt-4o-mini', 'gpt-3.5-turbo', 'gpt-4.1-mini' ou 'gpt-4.5'.")
  }
  
  # Preços por token (convertendo para proporção de mil tokens)
  preco_input <- precos[[model]]$input
  preco_output <- precos[[model]]$output
  
  # Cálculo total
  custo_total <- n_prompts * (prompt_tokens * preco_input + completion_tokens * preco_output)
  
  # Retorna resultado formatado
  return(list(
    modelo = model,
    prompts = n_prompts,
    tokens_total_entrada = n_prompts * prompt_tokens,
    tokens_total_saida = n_prompts * completion_tokens,
    custo_usd = round(custo_total, 4)
  ))
}

num_tokens_custom <- function(text, model = "gpt-4") {
  library(reticulate)
  tiktoken <- import("tiktoken")
  
  encoding <- switch(model,
                     "gpt-4" = "cl100k_base",
                     "gpt-3.5-turbo" = "cl100k_base",
                     "text-davinci-003" = "p50k_base",
                     "gpt2" = "gpt2",
                     "cl100k_base")  # default
  
  enc <- tiktoken$get_encoding(encoding)
  return(length(enc$encode(text)))
}