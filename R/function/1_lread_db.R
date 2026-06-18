
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


#bases do ODK AGGREGATE
vh_1         <- read_data("Formulário da Vigilancia Hospitalar.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Hospitalar")

vh_lab       <- read_data("Formulário da Vigilancia Hospitalar _ Laboratório.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Laboratorial")

vcom         <- read_data("Formulário da Vigilancia Comunitaria.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Comunitaria")

v_ambiental  <- read_data("VIGILÂNCIA  AMBIENTAL.csv")
cli::cli_alert_success("Lida com Sucesso a base da Vigilancia Ambiental")

r_test       <- read_data("Resultados de testagem das vigilâncias.csv")
cli::cli_alert_success("Lida com Sucesso a base da dos Resultados das Vigilancias")

cli::cli_alert_info("Lendo base de dados do ODK Central e extraindo componentes para análise...")

#estrair dados das diferentes componentes de vigilancia para formar bases separadas

# Hospitalar,comunitaria, ambiental
bd_ids_HCA_central <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form %in% c(
    "Form_hospital_d_demograficos", 
    "Form_Comunitaria_d_demograficos", 
    "Form_Ambiental_d_demograficos"
  ))


bd_ids_lab_us <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Formulario_laboratorio_US") %>% dplyr::select(lab_us_codigo_paciente,lab_us_amostras_colhidas,
                                                                                                 lab_us_resultado_colera)

# Laboratório INS Saúde Pública
bd_ids_lab_ins_cent <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Form_laborat_INS_Saude_publica")

# Genómica
bd_ids_genomica_cent <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Formulario_da_Genomica")

# Seguimento
#bd_ids_seguimento_cent <- bd_ids_combinada %>% 
#  dplyr::filter(dados_demograficos_select_form == "seguimento")

# Dados agregados
#bd_ids_agregados_cent <- bd_ids_combinada %>% 
#  dplyr::filter(dados_demograficos_select_form == "dados_agregados")

# Casos perdidos
#bd_ids_casos_perdidos_cent <- bd_ids_combinada %>% 
#  dplyr::filter(dados_demograficos_select_form == "casos_perdidos")


#Carregando base da Genomica no directorio R/Dashboard/BD_GENOMICA_LINED.csv
cli::cli_alert_info("Carregando bases de dados da Genomica")
BD_genomica_lined <- read.csv("R/Dashboard/BD_GENOMICA_LINED.csv")
cli::cli_alert_success("Base de dados da Genomica carregada com sucesso")

cli::cli_alert_success("Bases de dados carregadas com sucesso")

