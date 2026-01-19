
cli::cli_alert_success("Carregamento de Bases de dados")
#DB_MASTER

#Carregamento das bases de dados

#vigilancia hospitalar


# Função para ler csv mesmo com nomes
read_data <- function(pattern) {
  files <- list.files(here("raw"), pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop(paste("Arquivo não encontrado:", pattern))
  read.csv(files[1], fileEncoding = "UTF-8-BOM")
}


read_data <- function(filename) {
  path <- file.path(dir_raw, filename)
  
  if (!file.exists(path)) {
    stop(paste("Arquivo não encontrado:", path))
  }
  
  read.csv(path,
           fileEncoding = "UTF-8-BOM",
           stringsAsFactors = FALSE)
}


library(readxl)

read_excel_raw <- function(filename, sheet) {
  path <- file.path(dir_raw, filename)
  
  if (!file.exists(path)) {
    stop(paste("Arquivo não encontrado:", path))
  }
  
  read_xlsx(path, sheet = sheet)
}


read_csv_raw <- function(filename) {
  path <- file.path(dir_raw, filename)
  
  if (!file.exists(path)) {
    stop(paste("Arquivo não encontrado:", path))
  }
  
  read.csv(path,
           fileEncoding = "UTF-8-BOM",
           stringsAsFactors = FALSE)
}



vh_1         <- read_data("Formulário da Vigilancia Hospitalar.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Hospitalar")

vh_lab       <- read_data("Formulário da Vigilancia Hospitalar _ Laboratório.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Laboratorial")

vcom         <- read_data("Formulário da Vigilancia Comunitaria.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Comunitaria")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Comunitaria")

v_ambiental  <- read_data("VIGILÂNCIA  AMBIENTAL.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Ambiental")

r_test       <- read_data("Resultados de testagem das vigilâncias.csv")
cli::cli_alert_success("Lida com Sucesso a base da dos Resultados das Vigilancias")



#Carregando base da Genomica no directorio R/Dashboard/BD_GENOMICA_LINED.csv
cli::cli_alert_success("Carregando bases de dados da Genomica")
BD_genomica_lined <- read.csv("R/Dashboard/BD_GENOMICA_LINED.csv")


cli::cli_alert_success("Bases de dados carregadas com sucesso")