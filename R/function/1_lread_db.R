
cli::cli_alert_success("Carregamento de Bases de dados")
#DB_MASTER

#Carregamento das bases de dados

#vigilancia hospitalar

# Função para ler csv mesmo com nomes "feios"
read_data <- function(pattern) {
  files <- list.files(here("raw"), pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop(paste("Arquivo não encontrado:", pattern))
  read.csv(files[1], fileEncoding = "UTF-8-BOM")
}

# Carregar as bases de dados
vh_1 <- read.csv(here("raw/Formulário da Vigilancia Hospitalar.csv"), fileEncoding = "UTF-8-BOM")
vh_lab <- read.csv(here("raw/Formulário da Vigilancia Hospitalar _ Laboratório.csv"), fileEncoding = "UTF-8-BOM")
vcom <- read.csv(here("raw/Formulário da Vigilancia Comunitaria.csv"), fileEncoding = "UTF-8-BOM")
r_test <- read.csv(here("raw/Resultados de testagem das vigilâncias.csv"), fileEncoding = "UTF-8-BOM")
v_ambiental <- read.csv(here("raw/VIGILÂNCIA  AMBIENTAL.csv"), fileEncoding = "UTF-8-BOM")


