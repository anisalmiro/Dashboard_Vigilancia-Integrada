
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

# Carregar as bases de dados
vh_1 <- read.csv(here("raw/Formulário da Vigilancia Hospitalar.csv"))
vh_lab <- read.csv(here("raw/Formulário da Vigilancia Hospitalar _ Laboratório.csv"))
vcom <- read.csv(here("raw/Formulário da Vigilancia Comunitaria.csv"),)
r_test <- read.csv(here("raw/Resultados de testagem das vigilâncias.csv"))
v_ambiental <- read.csv(here("raw/VIGILÂNCIA  AMBIENTAL.csv"))


#modificar para ler a abba "IDS-influ"
gen_sarscov2<-read_xlsx(here("raw/SARS-Flu_Geno.xlsx"),sheet = "IDS-SARS-CoV-2  (data)")
gen_influenza<-read_xlsx(here("raw/SARS-Flu_Geno.xlsx"),sheet = "IDS-Flu (data)")
gen_ww_sarscov2<-read_xlsx(here("raw/SARS-Flu_Geno.xlsx"),sheet = "IDS-Wastewater-SARS-CoV-2")
gen_ww_influenza<-read_xlsx(here("raw/SARS-Flu_Geno.xlsx"),sheet = "IDS-Wastewater-Influenza")