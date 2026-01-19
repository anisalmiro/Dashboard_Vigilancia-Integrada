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

#2
#Ler Base de dados 
source(file.path(dir_function, "1_lread_db.R"), encoding = "UTF-8")


#3
#corigindo variaveis e base de dados
source(file.path(dir_function, "2_rename_Var.R"), encoding = "UTF-8")

#3
#corigindo variaveis e base de dados
source(file.path(dir_function, "3_correct_data.R"), encoding = "UTF-8")


#4
#Coorecao de Variaveis
#Limpando Valores da base de dados
#Renomeando Resultados e Filtrando falores Null
source(file.path(dir_function, "4_export_DB_Dashboard.R"), encoding = "UTF-8")


#4.1
#bases combinadas para dashboard de monitoria
source(file.path(dir_function, "4_1_create_combined_DB.R"), encoding = "UTF-8")

  
#5
source(file.path(dir_function, "5_create_indicator.R"), encoding = "UTF-8")


#Fim dos Criteios
#CREATE TO MONITORIA DASHBOARD
source(file.path(dir_function, "save_monitoria.R"), encoding = "UTF-8")


#gerar acessos
#source(file.path(dir_function, "acessos.R"), encoding = "UTF-8")

dim(B_geral_HCA_R)
dim(B_geral_HCA_1)
names(B_geral_HCA_R)

names(B_geral_HCA_R)

View(
  vig_laboratorial %>%
    dplyr::filter(
      `Dados_demograficos:Amostras_colhidas` == "Colera",
   ) %>% dplyr::select("Dados_demograficos:Amostras_colhidas","Dados_demograficos:tdtusa","Dados_demograficos:resultado_colera")
)




names(vig_laboratorial)

#open file 6_send_dropbox.R


1
#send to dropbox
source(file.path(dir_function, "6_send_dropbox.R"), encoding = "UTF-8")


#FIM DO SCRIPT
cli::cli_alert_success("FIM DO SCRIPT WORKPLACE.R")



dim(B_geral_HCA_R)
