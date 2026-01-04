# Pacotes necessários
library(httr)
library(jsonlite)
library(dplyr)

# Endpoint da API (exemplo)
api_url <- "https://api.institucional.gov.mz/v1/vigilancia/casos"

# Token de autenticação (exemplo fictício)
token <- "Bearer xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# Requisitar dados da API
response <- GET(
  url = api_url,
  add_headers(Authorization = token)
)

# Verificar status da resposta
if (status_code(response) == 200) {
  
  # Converter resultado JSON em dataframe (tibble)
  dados <- content(response, "text") |>
    fromJSON(flatten = TRUE) |>
    as_tibble()
  
  print("Dados recebidos com sucesso!")
  print(head(dados))
  
} else {
  stop("Erro ao aceder API: Status ", status_code(response))
}