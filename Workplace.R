#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    Data Engeneer: Anisio Bule

#1
#Leitura de todas bibliotecs
source(".RProfile", encoding = "UTF-8")

#2
#Ler Base de dados 
source(file.path(dir_function, "1_lread_db.R"), encoding = "UTF-8")


#3
#corigindo variaveis e base de dados
source(file.path(dir_function, "2_correct_data.R"), encoding = "UTF-8")


#Coorecao de Variaveis
#Limpando Valores da base de dados
#Renomeando Resultados e Filtrando falores Null
source(file.path(dir_function, "export_DB_Dashboard.R"), encoding = "UTF-8")

#
#
#
#
#


