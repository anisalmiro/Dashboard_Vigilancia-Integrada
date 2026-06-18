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

#2 read the od files
source(file.path(dir_util, "2_read_old_files.R"), encoding = "UTF-8")

#ler base do odk central e extrair em diferentes componentes para analise
source(file.path(dir_function, "1_read_central_ids_data.R"), encoding = "UTF-8")

#3
#Fim dos Criteios
#CREATE TO MONITORIA DASHBOARD
source(file.path(dir_function, "save_monitoria.R"), encoding = "UTF-8")

#selecionar dados de localizacao, unidade sanitaria, bairro, e outras variaveis de interece para gerar mapa e positivos de influenza para mapear os casos de influenza 


#gerar acessos
#source(file.path(dir_function, "acessos.R"), encoding = "UTF-8") 


#FIM DO SCRIPT
cli::cli_alert_success("FIM DO SCRIPT WORKPLACE.R")



dim(B_geral_HCA_R)
