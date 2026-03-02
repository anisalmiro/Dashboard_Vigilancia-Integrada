# ==========================================================
# Autor: Anisio Bule
# Fonte: http://shiny.rstudio.com/
# Descrição: Configuração inicial e carregamento de pacotes
# ==========================================================

# Definir local e opções
Sys.setlocale("LC_ALL", "Portuguese")
options(scipen = 100, digits = 2)

# Lista de bibliotecas necessárias
libraries <- c(
  # Manipulação e limpeza de dados
  "dplyr", "tidyr", "stringr", "forcats", "janitor", "lubridate", "zoo",
  
  # Visualização
  "ggplot2", "cowplot", "plotly", "scales", "esquisse",
  
  # Importação/exportação e utilitários
  "rio", "cli", "glue", "here",
  
  # Geoespacial
  "sf",
  
  # Conexão e publicação
  "rsconnect",
  
  # Conjunto tidyverse
  "tidyverse", "stringr"
)

#install.packages("readxl")
library("readxl")
library("writexl")

library(stringr)
library(here)
#
# Enviar bases de dados para o Dropbox
library(rdrop2)
#install.packages("openxlsx")
library(openxlsx)
library(stringr)
library(stringi)
library(lubridate)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(readr, ggplot2, scales, forcats)


# Instalar pacotes que não estão instalados
installed <- rownames(installed.packages())
to_install <- setdiff(libraries, installed)
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

# Carregar todas as bibliotecas
lapply(libraries, library, character.only = TRUE)

# Limpar mensagens de carregamento
invisible(gc())

#load packages ----------------------------------------------------------------
cli::cli_alert_info("Se tiver erros de Leitura de Bibliotecas, Instale-os ou faca um restart do ser R")

cli::cli_alert_success("Iniciando Dashboar - carregamento de directorios e BD")


#DEFININDO Directorios
  dir_raw <- "raw"
  dir_data <- "data_raw"
  charts_dir <- "charts"
  dir_R <- "R"
   
  #dir for get/pull/save
  dir_dashboard <- file.path(dir_R, "Dashboard")
  dir_function <- file.path(dir_R, 'function')
  dir_preliminar <- file.path(dir_R, 'Preliminar')
  dir_intermediaria <- file.path(dir_R, 'Intermediaria')
  dir_dashboard_alt<-"C:/Users/rgdti/OneDrive - INS - Instituto Nacional de Saúde/Documents/IDS/BD_DASHBOARD"
  
  
  #funcao para converter datas em varios formatos
  converter_data <- function(base_dados, coluna_data) {
    # Verifica se a coluna existe
    if (!coluna_data %in% names(base_dados)) {
      stop(paste("A coluna", coluna_data, "não existe na base de dados."))
    }
    
    base_dados %>%
      mutate(
        !!sym(coluna_data) := parse_date_time(
          !!sym(coluna_data),
          orders = c("Y-m-d", "d/m/Y", "d-m-Y", "m/d/Y", "d%m%Y", "Y%m%d", "d%b%Y", "d%b%y"),
          tz = "UTC"
        )
      )
  }
  
  #funcao que filtra a base de daods conforme dias definidos
  
  filtrar_dados_por_dias <- function(base_dados, dias_para_filtrar) {
    base_filtrada <- base_dados %>%
      mutate(
        data_convertida = lubridate::parse_date_time(
          `Dados_demograficos:DATE2`,
          orders = c("Y-m-d", "d/m/Y", "d-%m-%Y", "m/d/Y"),
          tz = "UTC"
        )
      ) %>%
      filter(data_convertida >= Sys.Date() - dias_para_filtrar)
    
    return(base_filtrada)
  }
  
  
  
  # aplicar a funcao para gerar as bases filtradas
  gerar_bases_filtradas <- function(lista_bases, dias_para_filtrar) {
    for (nome_base in lista_bases) {
      # Obter a base original a partir do nome (string)
      base_dados <- get(nome_base, envir = .GlobalEnv)
      
      # Filtrar os dados
      base_filtrada <- filtrar_dados_por_dias(base_dados, dias_para_filtrar)
      
      # Criar nome novo concatenando com o número de dias
      novo_nome <- paste0(nome_base, "_", dias_para_filtrar)
      
      # Atribuir o resultado dinamicamente
      assign(novo_nome, base_filtrada, envir = .GlobalEnv)
      
      # Mostrar mensagem informativa
      cli::cli_alert_success(paste("Base criada:", novo_nome))
    }
  }


  
  #saveRDS(token, "token.rds") # saves credentials
  token<-readRDS("token.rds") # read in credentials
  
  #list(token)
  
  token <- drop_auth(rdstoken = "token.rds")
  
  drop_acc(dtoken = token)
  
  # Autenticar no Dropbox
  # Pasta no Dropbox
  db_folder <- "/IDS"
  
  # Lista de arquivos locais + nomes no Dropbox
  files <- list(
    "data/DB_Dashboard/B_geral.rda",
    "data/DB_Dashboard/B_Comunitaria.rda",
    "data/DB_Dashboard/B_Hospitalar.rda",
    "data/DB_Dashboard/B_Ambiental.rda"
  )
  

