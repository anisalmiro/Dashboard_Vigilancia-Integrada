#' @title Exportar Bases de Dados para DashBoard

cli::cli_alert_info("Combinando base de dado Hospital e Comunitaria para analises")
BD_VH_VC_Preliminar<-full_join(vigilancia_hospitalar,vigilancia_comunitaria)


cli::cli_alert_info("Combinando base de dado Vig e ambiental para analises")
BD_VH_VC_VA_Intermidiaria<-full_join(BD_VH_VC_Preliminar,vigilancia_ambiental)


cli::cli_alert_info("Exportando Bases de Dados para DashBoard")
#Exportando Bases de dados para DashBoard
cli::cli_alert_info("Exportando Base Hospitalar")
write.csv(vigilancia_hospitalar, file = file.path(dir_preliminar, "Base_Vigilancia_Hospitalar.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Exportando Bases Laboratorial")
write.csv(vig_laboratorial, file = file.path(dir_preliminar, "Base_Laboratorio.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Exportando Base Comunitario")
write.csv(vigilancia_comunitaria, file = file.path(dir_preliminar, "Base_Comunitario.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Exportando Base Ambiental")
write.csv(vigilancia_ambiental, file = file.path(dir_dashboard, "Base_Ambiental.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Exportando Base Resultados")
write.csv(resultado_testagem, file = file.path(dir_dashboard, "Base_Resultados.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")


cli::cli_alert_info("Exportando Base Combinada Hospitalar e Comunitaria")
write.csv(BD_VH_VC_Preliminar, file = file.path(dir_dashboard, "BD_VH_VC_Preliminar.csv"),row.names = FALSE, fileEncoding = "UTF-8")


cli::cli_alert_info("Exportando Base intermidiaria para resultados")
write.csv(BD_VH_VC_VA_Intermidiaria, file = file.path(dir_dashboard, "BD_VH_VC_VA_Intermidiate.csv"),row.names = FALSE, fileEncoding = "UTF-8")



cli::cli_alert_info("Exportando Base de dados de Colera")
write.csv(B_geral_Colera, file = file.path(dir_dashboard, "B_geral_Colera.csv"),row.names = FALSE, fileEncoding = "UTF-8")

#Exportar base combinada de resultados com as tres vigilancias


cli::cli_alert_info("Exportando Base de de resultados das vigilancia combinada as tres vigilancias H,C e Ambiental")
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard, "B_geral_HCA_R.csv"),row.names = FALSE, fileEncoding = "UTF-8")


cli::cli_alert_success("Bases de Dados para monitoria exportada com sucesso!")
#exportar base de colera 
