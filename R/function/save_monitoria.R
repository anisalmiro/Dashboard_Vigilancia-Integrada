



#Exportando Bases de dados para DashBoard

cli::cli_alert_info("Salvando a base de dados Geral de analiser")
save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_geral.rda"))

cli::cli_alert_info("Salvando a base de dados da vigilancia Comunitaria")
save(BD_Final_VC_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Comunitaria.rda"))


cli::cli_alert_info("Salvando a base de dados da vigilancia Hospitalar")
save(BD_Final_VH_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Hospitalar.rda"))


cli::cli_alert_info("Salvando a base de dados da vigilancia Ambiental")
save(BD_Final_VA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Ambiental.rda"))

#Exportando base da genomica em formato .rda
save(BD_genomica_lined, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/BD_Genomica_Final.rda"))
save(BD_genomica_lined, file = paste0("C:/Github/IDS_API/IDS_Genomica/data/DB_Dashboard/BD_Genomica_Final.rda"))

save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_Genomica/data/DB_Dashboard/B_HCAR.rda"))
cli::cli_alert_success("Base de Dados da Genomica exportada com sucesso!")

cli::cli_alert_success("Base de Dados da Genomica exportada com sucesso!")



cli::cli_alert_info("Salvando a base de dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados")
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard, "Base_VIG_HOSP_COM_AMB_RESULTADOS.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Salvando a base de dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados. RDA")
save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_HCAR.rda"))


cli::cli_alert_info("Base de Dados Conjunta exportada com sucesso!")
writexl::write_xlsx(
  Base_positividade,
  path = file.path(dir_raw, "BASE_CONJUNTA.xlsx")
)

cli::cli_alert_info("Salvando a base de dados da de colera das diferentes Vigilancias")
save(B_geral_Colera, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_geral_Colera.rda"))

cli::cli_alert_success("Base de Dados de Colera exportada com sucesso!")


#export no formato .json a base de dados B_geral_HCA_R para dashboard
cli::cli_alert_info("Salvando a base de dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados. Formato JSON")
jsonlite::write_json(
  B_geral_HCA_R,
  path = file.path(dir_dashboard, "Base_VIG_HOSP_COM_AMB_RESULTADOS.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  force = TRUE
)


cli::cli_alert_info("Exportando Base de de resultados das vigilancia combinada as tres vigilancias H,C e Ambiental")
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard, "B_geral_HCA_R.csv"),row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_info("Exportando Base de dados de Colera")
write.csv(B_geral_Colera, file = file.path(dir_dashboard, "B_geral_Colera.csv"),row.names = FALSE, fileEncoding = "UTF-8")
write.csv(B_geral_Colera, file = file.path(dir_dashboard_alt, "B_geral_Colera.csv"),row.names = FALSE, fileEncoding = "UTF-8")

#Base de dados no repositorio alternartivo para actualizacao automatica de dados na Dashboard
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard_alt, "B_geral_HCA_R.csv"),row.names = FALSE, fileEncoding = "UTF-8")    




cli::cli_alert_success("Base de Dados para Dashboard exportada com sucesso!")

rm(BD_Final_VH_R,BD_Final_VC_R,BD_Final_VA_R,BD_VH_VC_Preliminar,vig_laboratorial,vigilancia_ambiental,vigilancia_comunitaria,vigilancia_hospitalar,BD_C_H_R,files,token)
