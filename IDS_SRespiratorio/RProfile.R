cli::cli_alert_success('Dashboard de Reporter para Vigilancia')

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Author: Anisio Bule

if(!is.element('dplyr', installed.packages()[,1])){install.packages('dplyr',dependencies = T)}
if(!is.element('ggplot2', installed.packages()[,1])){install.packages('ggplot2',dependencies = T)}
if(!is.element('rio', installed.packages()[,1])){install.packages('rio',dependencies = T)}
if(!is.element('cli', installed.packages()[,1])){install.packages('cli',dependencies = T)}
if(!is.element('lubridate', installed.packages()[,1])){install.packages('lubridate',dependencies = T)}
if(!is.element('glue', installed.packages()[,1])){install.packages('glue',dependencies = T)}
#Definir Regiao
#Definir Regiao
Sys.setlocale("LC_ALL","Portuguese")
options("scipen"=100, digits = 2)
#Clear existing data and graphics
#rm(list=ls())
#graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data


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

library(shiny)
library(DT)
library(shinycssloaders)
library(rio)
library(tidyr)
library(dplyr)
library(shinydashboard)
library(rpivotTable)
library(lubridate)



#load packages ----------------------------------------------------------------
cli::cli_alert_info("Se tiver erros de Leitura de Bibliotecas, Instale-os ou faca um restart do ser R")
suppressWarnings({
  options(defaultPackages = c(getOption("defaultPackages"),
                              
                              libraries                        
                              
  ))
})

#Directorios do dashboar
#Definindo Directorios de trabalho
dir_data <- "data"
dir_R <- "R"
dir_limpos <- file.path(dir_data, "Analises")
dir_dashboard <- file.path(dir_R, "Dashboard")
# Fim da definicao de Directorios
#Fim de leitura dos directorios do dashboard

#Funcoes de aranjos
