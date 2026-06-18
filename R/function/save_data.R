

#####----------------------------------script para diferentes dashboards ----------------------------------------------------

#Dashboard Resumo Vigilancia Hospitalar/Comunitaria/ Ambiental/ Genoomica/ Colera/ Tifoide
cli::cli_alert_info("Exportando bases bara dashboard de monitoria de casos")



save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/B_geral.rda"))



cli::cli_alert_info("Salvando a base de dados da vigilancia Comunitaria")
save(BD_Final_VC_R, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/B_Comunitaria.rda"))


cli::cli_alert_info("Salvando a base de dados da vigilancia Hospitalar")
save(BD_Final_VH_R, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/B_Hospitalar.rda"))


cli::cli_alert_info("Salvando a base de dados da vigilancia Ambiental")
save(BD_Final_VA_R, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/B_Ambiental.rda"))

#Exportando base da genomica em formato .rda
save(BD_genomica_lined, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/BD_Genomica_Final.rda"))

save(B_geral_Colera, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/B_geral_Colera.rda"))



#gravar em rds para C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/mapa_dados_influenza_sarsc.rds
save(mapa_dados, file = paste0("C:/Github/IDS_API/IDS_SRespiratorio/data/DB_Dashboard/mapa_dados_influenza_sarsc.rda"))

save(mapa_dados, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/mapa_dados_influenza_sarsc.rda"))

save(mapa_dados, file = paste0("C:/Github/IDS_API/IDS_Genomica/data/DB_Dashboard/mapa_dados_influenza_sarsc.rda"))



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

cli::cli_alert_info("Salvando a base de dados da VIGILANCIA Hospitalar, Comunitaria, Ambiental, Laboratorial e de resultados. RDA")
save(B_geral_HCA_R, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_HCAR.rda"))


cli::cli_alert_info("Base de Dados Conjunta exportada com sucesso!")
writexl::write_xlsx(
  Base_positividade,
  path = file.path(dir_raw, "BASE_CONJUNTA.xlsx")
)

cli::cli_alert_info("Salvando a base de dados da de colera das diferentes Vigilancias")
save(B_geral_Colera, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/B_geral_Colera.rda"))

#Base de dados no repositorio alternartivo para actualizacao automatica de dados na Dashboard
write.csv(B_geral_HCA_R, file = file.path(dir_dashboard_alt, "B_geral_HCA_R.csv"),row.names = FALSE, fileEncoding = "UTF-8") 

#C:/Users/rgdti/OneDrive - INS - Instituto Nacional de Saúde/Documents/IDS/BD_DASHBOARD
cli::cli_alert_success("Exportando Bases de Dados para DashBoard - Pasta Alternativa")
# exportar todas as bases para esta pasta alternativa de actualizacao das dashboards

write.csv(B_geral_HCA_R, file = file.path(dir_dashboard_alt, "B_geral_HCA_R.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

#write.csv(BD_genomica_lined, file = file.path(dir_dashboard_alt, "BD_genomica_lined.csv"),
#          row.names = FALSE, fileEncoding = "UTF-8")

cli::cli_alert_success("Base de Dados para Dashboard exportada com sucesso!")

rm(BD_Final_VH_R,BD_Final_VA_R,BD_VH_VC_Preliminar,vig_laboratorial,vigilancia_ambiental,vigilancia_comunitaria,vigilancia_hospitalar,BD_C_H_R,bairros_sf,resultado_testagem, mapa_dados,Data_map_inicial,Data_map_corrigido,B_HCA_R_cent,B_HCA_R_limpa,dados_bairro)
