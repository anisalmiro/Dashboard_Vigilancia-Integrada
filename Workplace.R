#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    Data Engeneer: Anisio Bule

# === WORKPLACE SCRIPT MAIN FILE ===
## clean envirnment 
rm(list = ls())

#1
#Leitura de todas bibliotecs
source(".RProfile", encoding = "UTF-8")

#pull data to ODK Central
source(file.path(dir_function, "00_pull_data_central.R"), encoding = "UTF-8")

#2
#Ler Base de dados 
source(file.path(dir_function, "1_lread_db.R"), encoding = "UTF-8")

#3
#corigindo variaveis e base de dados
source(file.path(dir_function, "2_rename_Var.R"), encoding = "UTF-8")

#3
#corigindo variaveis e base de dados
source(file.path(dir_function, "3_correct_data.R"), encoding = "UTF-8")

#4.1
#bases combinadas para dashboard de monitoria
source(file.path(dir_function, "4_1_create_combined_DB.R"), encoding = "UTF-8")

#5
source(file.path(dir_function, "5_create_indicator.R"), encoding = "UTF-8")

#pendentes para implementacao. procurar variavel bairro para pilotar mapas na dashboard  !
#6
source(file.path(dir_function, "7_Maps_function.R"), encoding = "UTF-8")

#Fim dos Criteios
#CREATE TO MONITORIA DASHBOARD
source(file.path(dir_function, "save_data.R"), encoding = "UTF-8")

