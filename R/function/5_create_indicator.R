

# Criando base para grafico da genomica

bd_conjunta <- B_geral_HCA_R %>%
  dplyr::select(
    codigo_paciente = `Dados_demograficos:codigo_paciente`,
    Influenza = `TIFOIDE:Resultado_de_Influenza`,
    `SARS-CoV-2` = `group_jz9ln80:SARSCov2`,
    DATE2 = `Dados_demograficos:DATE2`
  ) %>%
  dplyr::mutate(
    DATE2 = lubridate::parse_date_time(DATE2, orders = c("Ymd", "Y-m-d", "dmY", "d/m/Y", "mdY", "m/d/Y", "Ymd HMS", "dmY HMS")),
    Week = lubridate::week(DATE2)
  )

#view(bd_conjunta)

Base_positividade<-bd_conjunta %>% select(codigo_paciente,Influenza,`SARS-CoV-2`,Week,DATE2)



cli::cli_alert_success("Base de dado para grafico de genomica criada com sucesso para analises")

rm(bd_conjunta)
