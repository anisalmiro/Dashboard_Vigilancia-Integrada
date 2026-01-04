



#Exportando Bases de dados para DashBoard

cli::cli_alert_success("Salvando a base de dados Geral de analiser")
save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_geral.rda"))

cli::cli_alert_success("Salvando a base de dados da vigilancia Comunitaria")
save(BD_Final_VC_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Comunitaria.rda"))


cli::cli_alert_success("Salvando a base de dados da vigilancia Hospitalar")
save(BD_Final_VH_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Hospitalar.rda"))


cli::cli_alert_success("Salvando a base de dados da vigilancia Ambiental")
save(BD_Final_VA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_Ambiental.rda"))


cli::cli_alert_success("Exportando Base de Dados para Conjunta")

writexl::write_xlsx(
  Base_positividade,
  path = file.path(dir_raw, "BASE_CONJUNTA.xlsx")
)

cli::cli_alert_success("Exportando Base de Dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados")
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard, "Base_VIG_HOSP_COM_AMB_RESULTADOS.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cli::cli_alert_success("Exportando Base de Dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados. RDA")
save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/R/Dashboard/B_HCAR.rda"))

#Expoetando base de dados de Sequenciamento


cli::cli_alert_success("Base de Dados Conjunta exportada com sucesso!")

#export no formato .json a base de dados B_geral_HCA_R para dashboard
jsonlite::write_json(
  B_geral_HCA_R,
  path = file.path(dir_dashboard, "Base_VIG_HOSP_COM_AMB_RESULTADOS.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  force = TRUE
)
cli::cli_alert_success("Base de Dados para Dashboard exportada com sucesso!")