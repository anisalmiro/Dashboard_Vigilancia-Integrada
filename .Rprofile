
#
#    http://shiny.rstudio.com/
# Author: Anisio Bule


#Definir Regiao
Sys.setlocale("LC_ALL","Portuguese")
options("scipen"=100, digits = 2)
#Clear existing data and graphics
#rm(list=ls())
#graphics.off()

#instalacao de bibliotecas caso nao estejam instaladas
   
 if(!is.element('dplyr', installed.packages()[,1])){install.packages('dplyr',dependencies = T)}
 if(!is.element('ggplot2', installed.packages()[,1])){install.packages('ggplot2',dependencies = T)}
 if(!is.element('rio', installed.packages()[,1])){install.packages('rio',dependencies = T)}
 if(!is.element('cli', installed.packages()[,1])){install.packages('cli',dependencies = T)}
 if(!is.element('lubridate', installed.packages()[,1])){install.packages('lubridate',dependencies = T)}


library("dplyr")
library("rlang")
library("ggplot2")
library("tidyr")
library("lubridate")
library("readxl")
library("rio")
library(here)
library(lubridate)

#-----------
libraries <- c(
  
  
  #carpintery
  "glue", "janitor",  "scales",
  "forcats", "lubridate",
  
  #maps
  "sf",
  
  
  #tidyverse: 
  "tidyr","tidyverse", "stringr", "dplyr",
  
  #outras
  "rio", "cli", "zoo","esquisse",
  
  #plot
  "cowplot", "ggplot2","plotly" ,
  
  #connection
  "rsconnect"
  
)

library(stringr)

#load packages ----------------------------------------------------------------
cli::cli_alert_info("Se tiver erros de Leitura de Bibliotecas, Instale-os ou faca um restart do ser R")

cli::cli_alert_success("Iniciando Dashboar - carregamento de directorios e BD")

#DEFININDO Directorios
  dir_data <- "data_raw"
  dir_R <- "R"
   
  #dir for get/pull/save
  dir_dashboard <- file.path(dir_R, "Dashboard")
  dir_function <- file.path(dir_R, 'function')
  dir_preliminar <- file.path(dir_R, 'Preliminar')
  dir_intermediaria <- file.path(dir_R, 'Intermediaria')


