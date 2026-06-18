# ==========================================================
# Autor: Anisio Bule
# Fonte: http://shiny.rstudio.com/
# Descrição: Configuração inicial e carregamento de pacotes
# ==========================================================

# Definir local e opções
Sys.setlocale("LC_ALL", "Portuguese")
options(scipen = 100, digits = 2)

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
library(tidyverse)
library(readxl)
library(openxlsx)
library(ruODK)
library(here)
library(lubridate)
library(knitr)
library(readr)
library(dplyr)
library(tidyr)
library(gridExtra)

library(dplyr)
library(lubridate)
library(stringr)
library(cli)
library(plyr)
library(rlang)




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


##---------------------------------------------------------------
##--access ODK Central and read in the survey data

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
  
#DEFININDO Directorios
  dir_raw <- "raw"
  dir_data <- "data_raw"
  charts_dir <- "charts"
  dir_R <- "R"
   
  #dir for get/pull/save
  dir_dashboard <- file.path(dir_R, "Dashboard")
  dir_function <- file.path(dir_R, 'function')
  dir_util <- file.path(dir_R, 'util')
  dir_preliminar <- file.path(dir_R, 'Preliminar')
  dir_intermediaria <- file.path(dir_R, 'Intermediaria')
  dir_backup_raw_combined <- file.path(dir_raw, "ids_comb_form_data")
  dir_dashboard_alt<-"C:/Users/rgdti/OneDrive - INS - Instituto Nacional de Saúde/Documents/IDS/BD_DASHBOARD"
  


  
  
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

 # saveRDS(bd_ids_combinada, file.path(dir_raw, "bd_ids_combinada.rds"))
  # Use readRDS em vez de load
  # ler a base de dados com o nome bd_ids_combinada_{maxima data registada no backup}.rds e atribuir a um objeto chamado bd_ids_combinada

  bd_ids_combinada <- readRDS(file.path(dir_raw, "bd_ids_combinada.rds"))
  
  
  # 1. Listar os arquivos dentro do seu diretório 'dir_raw' que seguem o padrão
  #arquivos <- list.files(path = dir_raw, pattern = "^bd_ids_combinada_.*\\.rds$", full.names = TRUE)
  
  # 2. Identificar o arquivo com a data de modificação mais recente
  #arquivo_mais_recente <- arquivos[which.max(file.info(arquivos)$mtime)]
  
  # 3. Ler o arquivo mais recente (o 'arquivo_mais_recente' já inclui o caminho completo)
  #bd_ids_combinada <- readRDS(arquivo_mais_recente)
  
  
  ##--connect to ODK Central by passing hidden credentials
  ##--the linked file should be made previous to this step and stored in your project folder
  ##--ODK Central's OData URL contains base URL, project ID, and form ID
  ##--ODK Central credentials can live in .Renviron
  ##--run 'vignette("setup")' for setup and authentication options (will appear in 'Help' pane)
  

