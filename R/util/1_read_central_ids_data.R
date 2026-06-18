
cli::cli_alert_info("Lendo base de dados do ODK Central e extraindo componentes para análise...")


# Genómica
bd_ids_genomica <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Formulario_da_Genomica")

# Seguimento
bd_ids_seguimento <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "seguimento")

# Dados agregados
bd_ids_agregados <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "dados_agregados")

# Casos perdidos
bd_ids_casos_perdidos <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "casos_perdidos")




# selecionar as bases conforme as suas posuem as variaveis de interesse para a analise 

bd_ids_hospitalar <- bd_ids_hospitalar %>%
  dplyr::select(1:note_8_comment)


#selecionar as colunas apenas oque contem whota_   da base ambiental
bd_ids_ambiental <- bd_ids_ambiental %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("whota_"))


#base comunitaria selecionar as colunas que contem dados_demograficos4m_
bd_ids_comunitaria <- bd_ids_comunitaria %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("dados_demograficos4m_"))

#base ids_lab_us selecionar as colunas que contem dados_demograficos12_
bd_ids_lab_us <- bd_ids_lab_us %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("dados_demograficos12_"))

# base de dados bd_ids_lab_ins selecionar as colunas que contem detalhes_
bd_ids_lab_ins <- bd_ids_lab_ins %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("detalhes_"))


cli::cli_alert_success("Bases de dados extraídas e salvas com sucesso")



#read from root aux_chlabus.csv,result.csv
bd_chlabus <- rio::import(file.path(dir_R, "aux_chlabus.csv"))
bd_chlabus_result <- rio::import(file.path(dir_R, "result.csv"))


names(bd_chlabus)[duplicated(names(bd_chlabus))]
names(bd_chlabus) <- make.unique(names(bd_chlabus))


#classificar quantos nao tem ligacao com a base bd_chlabus e bd_chlabus_result apartir do codigo_paciente
bd_chlabus %>%
  anti_join(bd_chlabus_result, by = "codigo_paciente") %>%
  nrow()




#inner join das bases de dados bd_chlabus e bd_chlabus_result pela coluna id codigo_paciente

bd_ids_combinada_analise <- bd_chlabus %>%
  inner_join(bd_chlabus_result, by = "codigo_paciente")
names(bd_ids_combinada_analise)


bd_ids_combinada_analise <- bd_ids_combinada_analise %>%
  mutate(`dados_demograficos_date2.x` = case_when(
    !is.na(`dados_demograficos_date2.x`) & `dados_demograficos_date2.x` != "" ~
      format(
        suppressWarnings(lubridate::parse_date_time(
          `dados_demograficos_date2.x`,
          orders = c(
            "mdy HM",          # SSENCIAL para 3/26/2026 2:00
            "mdy HMS",
            "dmy", "d-b-y", "d-b-Y",
            "ymd HMS", "ymd HMSz", "ymd HMSOSz",
            "ymd", "mdy"
          )
        )),
        "%d-%m-%Y"
      ),
    TRUE ~ as.character(`dados_demograficos_date2.x`)
  ))


bd_ids_combinada_analise<- bd_ids_combinada_analise %>% mutate(`Dados_demograficos:DATE2` =`dados_demograficos_date2.x`)

names(B_geral_HCA_R)

names(bd_ids_combinada_analise)

base_prelim_analise <- bd_ids_combinada_analise %>% 
  select(codigo_paciente,`Dados_demograficos:DATE2`,"resultado_lab_ins6_sars_cov2","resultado_lab_ins5_resultado_de_influenza")

base_prelim_analise <- base_prelim_analise %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codigo_paciente=as.character(base_prelim_analise$`codigo_paciente`), ""))),
    vigilancia = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Ambiental",
      str_detect(codigo_trim, "^IDSC") ~ "Comunitaria",
      str_detect(codigo_trim, "^IDS")  ~ "Hospitalar",
      TRUE ~ "Outro"
    )
  ) %>%
  filter(vigilancia %in% c("Comunitaria", "Hospitalar", "Ambiental"))


base_prelim_analise <- base_prelim_analise %>%
  mutate(
    # 1. Converter para Date (ajustar formato conforme os teus dados)
    DATE2 = parse_date_time(`Dados_demograficos:DATE2`,
                            orders = c("dmy", "ymd", "mdy")), 
    # se for "dia/mês/ano"
    # DATE2 = ymd(...)  # se for "ano-mês-dia"
    # DATE2 = mdy(...)  # se for "mês/dia/ano"
    
    # 2. Ano epidemiológico correto
    ano = year(DATE2),
    
    # 3. Semana epidemiológica ISO
    Semana_Epi = isoweek(DATE2),
    
    # 4. Ano + semana
    Semana_Epi_ano = ifelse(
      is.na(DATE2),
      NA,
      paste0(ano, "-", sprintf("%02d", Semana_Epi))
    )
  )


base_prelim_analise_1 <- base_prelim_analise %>%  select(codigo_paciente,resultado_lab_ins5_resultado_de_influenza, resultado_lab_ins6_sars_cov2,Semana_Epi,DATE2, vigilancia )


base_prelim_analise_1 <- base_prelim_analise_1 %>%
  mutate(
    DATE2 = case_when(
      !is.na(DATE2) & DATE2 != "" ~
        suppressWarnings(
          parse_date_time(
            DATE2,
            orders = c(
              "mdy HM", "mdy HMS",
              "dmy HM", "dmy HMS",
              "d-b-y", "d-b-Y",
              "ymd HMS", "ymd HMSz", "ymd HMSOSz",
              "ymd", "mdy", "dmy"
            ),
            tz = "UTC"   # ✅ define UTC diretamente
          )
        ),
      TRUE ~ as.POSIXct(NA, tz = "UTC")
    )
  )


write.csv(base_prelim_analise_1, file.path(dir_R, "base_prelim_analise.csv"), row.names = FALSE)

