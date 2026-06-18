
#=========================================================
# 1. BASE DE DADOS PARA DASHBOARD
#=========================================================
cli::cli_alert("Criando base de dados para Dashboard")

#base combinada com base de resultados
BD_cont_cent <- B_HCA_central_corrigida %>% 
  left_join(
    bd_ids_lab_ins_cent_prel %>%
      select(
        resultado_lab_ins_codigo_paciente, 
        resultado_lab_ins5_resultado_de_influenza,
        resultado_lab_ins6_sars_cov2,
        resultado_lab_ins11_resultado_hemocultura
      ),
    by = c("codigo_paciente" = "resultado_lab_ins_codigo_paciente")
  ) %>%
  # Seleciona, renomeia as principais e traz TODO o resto automaticamente
  select(
    # Identificação e Tempo
    codigo_paciente,
    DATE2 = dados_demograficos_date2,
    
    # Variáveis Geográficas Renomeadas
    provincia_casos = provincia_residencia,
    distrito_casos = distrito_residencia,
    bairro = bairro_residencia,
    unidade_sanitaria = hospital1_unidade_sanitaria,
    
    # Resultados do Laboratório Renomeados
    Influenza = resultado_lab_ins5_resultado_de_influenza,
    SARS_CoV_2 = resultado_lab_ins6_sars_cov2,
    resul_tifoide = resultado_lab_ins11_resultado_hemocultura,
    
    # O "everything()" garante que TODAS as outras 30+ variáveis listadas 
    # (como as da vigilância ambiental, comunitária, idade, sintomas, etc.) 
    # entrem no banco final sem precisar digitar os nomes de uma por uma.
    everything() 
  )


B_HCA_R_cent <- B_HCA_central_corrigida %>% 
  inner_join(
    bd_ids_lab_ins_cent_prel %>%
      select(
        resultado_lab_ins_codigo_paciente, 
        resultado_lab_ins5_resultado_de_influenza,
        resultado_lab_ins6_sars_cov2,
        resultado_lab_ins11_resultado_hemocultura
      ),
    by = c("codigo_paciente" = "resultado_lab_ins_codigo_paciente")
  ) %>%
  # Seleciona, renomeia as principais e traz TODO o resto automaticamente
  select(
    # Identificação e Tempo
    codigo_paciente,
    DATE2 = dados_demograficos_date2,
    
    # Variáveis Geográficas Renomeadas
    provincia_casos = provincia_residencia,
    distrito_casos = distrito_residencia,
    bairro = bairro_residencia,
    unidade_sanitaria = hospital1_unidade_sanitaria,
    
    # Resultados do Laboratório Renomeados
    Influenza = resultado_lab_ins5_resultado_de_influenza,
    SARS_CoV_2 = resultado_lab_ins6_sars_cov2,
    resul_tifoide = resultado_lab_ins11_resultado_hemocultura,
    
    # O "everything()" garante que TODAS as outras 30+ variáveis listadas 
    # (como as da vigilância ambiental, comunitária, idade, sintomas, etc.) 
    # entrem no banco final sem precisar digitar os nomes de uma por uma.
    everything() 
  )



#combinar variaveis

B_HCA_R_limpa <- B_HCA_R_cent %>%
  dplyr::mutate(
    
    # Identificação
    codigo_paciente = codigo_paciente,
    
    # Datas
    data_recolha = coalesce(
      DATE2,
      hospital1_date22,
      comunitaria_date23
    ),
    
    data_inclusao = hospital1_data_inclusao,
    
    data_inicio_sintomas = hospital2_data_inic_sint,
    
    data_nascimento_final = data_nascimento,
    
    # Localização
    provincia = coalesce(
      provincia_casos,
      ambiental_provincia_residencia1,
      comunitaria_provincia
    ),
    
    distrito = coalesce(
      distrito_casos,
      hospital1_outro_distrito_residencia,
      ambiental_distrito_residencia1,
      comunitaria_distrito
    ),
    
    bairro_final = coalesce(
      bairro,
      ambiental_bairro_residencia1,
      outro_bairro
    ),
    
    unidade_sanitaria_final = coalesce(
      unidade_sanitaria,
      comunitaria_us
    ),
    
    # Idade
    idade_final = coalesce(
      suppressWarnings(as.numeric(idade)),
      suppressWarnings(as.numeric(idade2)),
      suppressWarnings(as.numeric(hospital1_child_age_months))
    ),
    
    tipo_idade_final = tipo_idade,
    
    # Dados demográficos
    sexo_final = sexo,
    escolaridade_final = escolaridade,
    estado_civil_final = estado_civil,
    profissao_final = profissao,
    residencia_tipo_final = residencia_tipo,
    conhece_data_nasc_final = conhece_data_nasc,
    
    # Sintomas
    sintomas_final = sintomas,
    outro_sintoma_final = outro_sintoma,
    
    # Hospitalização
    motivo_hospitalizacao_final = coalesce(
      motivo_hospitalizacao,
      hospital5_motivo_hospitalizado_other
    ),
    
    # Resultados laboratoriais
    influenza_resultado = Influenza,
    sars_cov2_resultado = SARS_CoV_2,
    tifoide_resultado = resul_tifoide,
    
    # Vigilância hospitalar
    local_inclusao = hospital1_local_de_inclusao,
    
    # Vigilância ambiental
    local_colheita = ambiental_lo_colheita,
    tecnico_responsavel_colheita = ambiental_tec_res_colheita,
    
    # Vigilância comunitária
    cluster_comunitario = comunitaria_clusters,
    
    # Outros
    formulario_demografico = dados_demograficos_select_form
    
  ) %>%
  
  dplyr::select(
    
    # Variáveis harmonizadas
    codigo_paciente,
    data_recolha,
    data_inclusao,
    data_nascimento_final,
    provincia,
    distrito,
    bairro_final,
    unidade_sanitaria_final,
    local_inclusao,
    local_colheita,
    idade_final,
    tipo_idade_final,
    sexo_final,
    estado_civil_final,
    profissao_final,
    residencia_tipo_final,
    conhece_data_nasc_final,
    sintomas_final,
    outro_sintoma_final,
    motivo_hospitalizacao_final,
    influenza_resultado,
    sars_cov2_resultado,
    cluster_comunitario,
  
   
    
    
  )



#======================================================================================


write.csv(B_geral_HCA_R, file = file.path(dir_dashboard, "B_geral_HCA_R_simp.csv"),row.names = FALSE, fileEncoding = "UTF-8")


#base do agregate com a base do central
B_HCA_p <- B_geral_HCA_R %>%
  full_join(B_HCA_R_cent, by = "codigo_paciente") %>% 
  select(-starts_with("WHOTA"))


BD_Cont_colheita <- BD_Cot_colheita %>%
  full_join(BD_cont_cent, by = "codigo_paciente") %>% 
  select(-starts_with("WHOTA"))

rm(BD_Cot_colheita, BD_Cont_central)

#corregir formato da data para ano-mes-dia da variavel DATE2 da base B_HCA_p

B_HCA_p <- B_HCA_p %>%
  dplyr::mutate(
    # 1. Converte apenas onde há dados, ignorando os vazios/NAs
    DATE2 = ymd_hms(DATE2, quiet = TRUE),
    
    # 2. Transforma em formato de Data puro (Ano-Mês-Dia)
    DATE2 = as.Date(DATE2)
  )

rm(B_geral_HCA_R)
#======================================================================================
B_HCA_final <- B_HCA_p %>%
 dplyr::mutate(
    # 1. Extrair estritamente os primeiros 10 caracteres (YYYY-MM-DD) de cada coluna,
    # ignorando as horas e tratando os campos vazios "" como NA.
    data_origem1 = na_if(substr(as.character(`Dados_demograficos:DATE2`), 1, 10), ""),
    data_origem2 = na_if(substr(as.character(DATE2), 1, 10), ""),
    
    # 2. Combinar ambas, mantendo o formato puro de texto Y-M-D
    DATE2 = coalesce(data_origem1, data_origem2)
  ) %>%
  # Limpar as colunas de suporte criadas acima
  select(-data_origem1, -data_origem2) %>%
  
  # 3. Continuar com o resto do seu mutate original...
  dplyr::mutate(
    sexo = coalesce(sexo, `Dados_demograficos:sexo`),
    data_nascimento = coalesce(data_nascimento, `Dados_demograficos:data_nascimento`),
    idade = coalesce(idade, `Dados_demograficos:idade`,`Dados_demograficos:idade2`,idade2),
    tipo_idade = coalesce(tipo_idade, `Dados_demograficos:tipo_idade`),
    
    provincia_casos = coalesce(provincia_casos, `Dados_demograficos:provincia_de_residencia`,`ambiental_provincia_residencia1`),
    distrito_casos = coalesce(distrito_casos, `Dados_demograficos:distrito_residencia`,`ambiental_distrito_residencia1`),
    unidade_sanitaria = coalesce(unidade_sanitaria, Unidade_sanitaria),
    local_inclusao = coalesce(hospital1_local_de_inclusao, `Dados_demograficos:Local_de_inclusao`),
    
    sintomas = coalesce(sintomas, Sintomas),
    motivo_hospitalizacao = coalesce(motivo_hospitalizacao, Motivo_Hospitalizado),
    bairro = coalesce(bairro, bairro_comb),
    
    Influenza = coalesce(Influenza, resultado_influenza),
    SARS_CoV_2 = coalesce(SARS_CoV_2, resultado_sars_cov2),
    resul_tifoide = coalesce(resul_tifoide, resultado_hemocultura),
    resul_colera = coalesce(resultado_colera)
  ) %>%
  dplyr::select(
    -`Dados_demograficos:DATE2`,
    -`Dados_demograficos:sexo`,
    -`Dados_demograficos:data_nascimento`,
    -`Dados_demograficos:idade`,
    -`Dados_demograficos:tipo_idade`,
    -`Dados_demograficos:idade2`,
    -`Dados_demograficos:provincia_de_residencia`,
    -`Dados_demograficos:distrito_residencia`,
    -Unidade_sanitaria,
    -`Dados_demograficos:Local_de_inclusao`, -hospital1_local_de_inclusao,
    -Sintomas,
    -bairro,
    -Motivo_Hospitalizado,
    -resultado_influenza,
    -resultado_sars_cov2,
    -resultado_hemocultura,
    -resultado_colera,
    
    codigo_paciente, DATE2, provincia_casos, distrito_casos, unidade_sanitaria,
    sexo, data_nascimento, idade, sintomas, motivo_hospitalizacao,
    Influenza, SARS_CoV_2, resul_tifoide, resul_colera,
    everything()
  )



#BD_Cont_colheita
#corregir base de Contagem de casos auxiliar
BD_Cont_colheita_f <- BD_Cont_colheita %>%
  dplyr::mutate(
    # 1. Extrair estritamente os primeiros 10 caracteres (YYYY-MM-DD) de cada coluna,
    # ignorando as horas e tratando os campos vazios "" como NA.
    data_origem1 = na_if(substr(as.character(`Dados_demograficos:DATE2`), 1, 10), ""),
    data_origem2 = na_if(substr(as.character(DATE2), 1, 10), ""),
    
    # 2. Combinar ambas, mantendo o formato puro de texto Y-M-D
    DATE2 = coalesce(data_origem1, data_origem2)
  ) %>%
  # Limpar as colunas de suporte criadas acima
  select(-data_origem1, -data_origem2) %>%
  
  # 3. Continuar com o resto do seu mutate original...
  dplyr::mutate(
    sexo = coalesce(sexo, `Dados_demograficos:sexo`),
    data_nascimento = coalesce(data_nascimento, `Dados_demograficos:data_nascimento`),
    idade = coalesce(idade, `Dados_demograficos:idade`,`Dados_demograficos:idade2`,idade2),
    tipo_idade = coalesce(tipo_idade, `Dados_demograficos:tipo_idade`),
    
    provincia_casos = coalesce(provincia_casos, `Dados_demograficos:provincia_de_residencia`,`ambiental_provincia_residencia1`),
    distrito_casos = coalesce(distrito_casos, `Dados_demograficos:distrito_residencia`,`ambiental_distrito_residencia1`),
    unidade_sanitaria = coalesce(unidade_sanitaria, Unidade_sanitaria),
    local_inclusao = coalesce(hospital1_local_de_inclusao, `Dados_demograficos:Local_de_inclusao`),
    
    sintomas = coalesce(sintomas, Sintomas),
    motivo_hospitalizacao = coalesce(motivo_hospitalizacao, Motivo_Hospitalizado),
    bairro = coalesce(bairro, bairro_comb),
    
    Influenza = coalesce(Influenza, resultado_influenza),
    SARS_CoV_2 = coalesce(SARS_CoV_2, resultado_sars_cov2),
    resul_tifoide = coalesce(resul_tifoide, resultado_hemocultura),
    resul_colera = coalesce(resultado_colera)
  ) %>%
  dplyr::select(
    -`Dados_demograficos:DATE2`,
    -`Dados_demograficos:sexo`,
    -`Dados_demograficos:data_nascimento`,
    -`Dados_demograficos:idade`,
    -`Dados_demograficos:tipo_idade`,
    -`Dados_demograficos:idade2`,
    -`Dados_demograficos:provincia_de_residencia`,
    -`Dados_demograficos:distrito_residencia`,
    -Unidade_sanitaria,
    -`Dados_demograficos:Local_de_inclusao`, -hospital1_local_de_inclusao,
    -Sintomas,
    -bairro,
    -Motivo_Hospitalizado,
    -resultado_influenza,
    -resultado_sars_cov2,
    -resultado_hemocultura,
    -resultado_colera,
    
    codigo_paciente, DATE2, provincia_casos, distrito_casos, unidade_sanitaria,
    sexo, data_nascimento, idade, sintomas, motivo_hospitalizacao,
    Influenza, SARS_CoV_2, resul_tifoide, resul_colera,
    everything()
  )



#remover da base BD_Cont_colheita_f 1221 linhas onde o resultado de influenza e SARS_CoV_2, resultado_hemocultura, resultado_colera, sao NA ou null

linhas_para_remover <- which(
  is.na(BD_Cont_colheita_f$Influenza) & 
    is.na(BD_Cont_colheita_f$SARS_CoV_2) & 
    is.na(BD_Cont_colheita_f$resul_tifoide) & 
    is.na(BD_Cont_colheita_f$resul_colera)
)

# 2. Limitar estritamente às primeiras 1221 ocorrências encontradas
linhas_para_remover_exato <- head(linhas_para_remover, 1221)

# 3. Remover apenas essas posições exatas da base de dados
BD_Cont_colheita_f <- BD_Cont_colheita_f[-linhas_para_remover_exato, ]


rm(B_HCA_central_corrigida, bd_ids_lab_ins_cent_prel,BD_Cont_colheita, BD_cont_cent,BD_Cot_colheita)

B_HCA_final <- B_HCA_final %>%
  dplyr::mutate(
    modulo_ras = case_when(
      (stringr::str_trim(dplyr::coalesce(Influenza, "")) != "") | 
        (stringr::str_trim(dplyr::coalesce(SARS_CoV_2, "")) != "") ~ "IRAS",
      TRUE ~ NA_character_  # Se não for IRAS, deixa vazio (NA tipo texto)
    )
  )


B_HCA_final <- B_HCA_final %>%
  dplyr::mutate(
    modulo_tifoide = case_when(
        (stringr::str_trim(dplyr::coalesce(resul_tifoide, "")) != "") ~ "TIFOIDE",
      TRUE ~ NA_character_  # Se não for IRAS, deixa vazio (NA tipo texto)
    )
  )



B_HCA_final <- B_HCA_final %>%
  dplyr::mutate(
    modulo_colera = case_when(
      (stringr::str_trim(dplyr::coalesce(resultado_colera, "")) != "") ~ "COLERA",
      TRUE ~ NA_character_  # Se não for IRAS, deixa vazio (NA tipo texto)
    )
  )


B_HCA_final <- B_HCA_final %>%
  dplyr::mutate(
    modulo_rsv = case_when(
      (stringr::str_trim(dplyr::coalesce(resultado_rsv, "")) != "") ~ "RSV",
      TRUE ~ NA_character_  # Se não for IRAS, deixa vazio (NA tipo texto)
    )
  )


rm(B_HCA_p)

#======================================================================================




#======================================================================================

#
#
#

#======================================================================================

#criar uma variavel de idade_comp copiar todas idades da variavel idade mas onde na variavel tipo de idade for meses atribuir idade 0 na nova vairiavel idade_complementar


B_HCA_final <- B_HCA_final %>%
  mutate(
    idade_complement = case_when(
      tipo_idade == "meses" ~ 1,
      tipo_idade == "dias"  ~ 1,
      TRUE                  ~ idade  # Mantém a idade original para os outros casos (ex: "anos")
    )
  )

#================================================================================================================================================
base_intermediate <- B_HCA_final %>%
  dplyr::select("codigo_paciente", modulo_ras,modulo_tifoide, modulo_colera,modulo_rsv, "Amostras_colhidas","modulo_reporte","Unidade_sanitaria",
          "local_colheita",   "DATE2", "provincia_casos", "distrito_casos", "hospital1_outro_distrito_residencia","bairro", "unidade_sanitaria", 
          "data_nascimento", "conhece_data_nasc", "idade", "tipo_idade", "idade_complement", 
          "escolaridade", "estado_civil", "profissao","sexo","hospital1_data_inclusao","hospital1_child_age_months", 
          "Influenza", "SARS_CoV_2", "tipo_influenza", "subtipo_influenza",  "resul_tifoide", "resul_colera", 
         "resultado_cultura", "outro_resultado_cultura", "resultado_rsv", "tipo_rsv", "resultado_salmonella_typhi",
         "hospital5_motivo_hospitalizado_other", "ambiental_provincia_residencia1",
         "ambiental_distrito_residencia1", "ambiental_bairro_residencia1", 
         "ambiental_lo_colheita", "comunitaria_clusters", "comunitaria_provincia", "comunitaria_distrito", 
         "comunitaria_us", "outro_bairro", "residencia_tipo",  
         "sintomas","hospital2_data_inic_sint", "outro_sintoma",
          "Hospitalizado", "motivo_hospitalizacao", "local_inclusao",
         "instanceID","meta:instanceID"
         )



rm(B_HCA_final, BD_Cot_colheita)
#adicionar variavel vigilancia na base combinada B_geral_HCA apartir do codigo do paciente
B_geral_HCA_R<- base_intermediate %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codig_paciente=as.character(codigo_paciente), ""))),
    vigilancia = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Ambiental",
      str_detect(codigo_trim, "^IDSC") ~ "Comunitaria",
      str_detect(codigo_trim, "^IDS")  ~ "Hospitalar",
      TRUE ~ "Outro"
    ), 
    modulo_reporte=toupper(modulo_reporte)
  ) %>%
  filter(vigilancia %in% c("Comunitaria", "Hospitalar", "Ambiental"))

#aTRIBUIR CRITERIO DE FILTROS A BASE 
BD_Cont_colheita_f<- BD_Cont_colheita_f %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codig_paciente=as.character(codigo_paciente), ""))),
    vigilancia = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Ambiental",
      str_detect(codigo_trim, "^IDSC") ~ "Comunitaria",
      str_detect(codigo_trim, "^IDS")  ~ "Hospitalar",
      TRUE ~ "Outro"
    ), 
    modulo_reporte=toupper(modulo_reporte)
  ) %>%
  filter(vigilancia %in% c("Comunitaria", "Hospitalar", "Ambiental"))
  
  

cli::cli_alert_success("Base de dado combinada com sucesso para analises")


# Definir os formatos comuns que vêm no seu banco B_geral_HCA_R
formatos_data <- c("ymd HMS", "ymd", "dmy HMS", "dmy", "mdy")

B_geral_HCA_R <- B_geral_HCA_R %>%
  dplyr::mutate(
    # 1. Trata múltiplos formatos e padroniza para classe Date
    DATE2 = parse_date_time(DATE2, orders = formatos_data),
    DATE2 = as.Date(DATE2),
    
    # 2. Semana epidemiológica (Se precisar começar no Domingo, mude para epiweek)
    Semana_Epi = isoweek(DATE2),
    
    # 3. Ano epidemiológico correto (isoyear garante que o ano combine com a isoweek)
    # Se mudar para epiweek acima, use 'epiyear(DATE2)' aqui.
    ano = isoyear(DATE2),
    
    # 4. Combinar Ano + semana formatada (Ex: 2026-05)
    Semana_Epi_ano = ifelse(
      is.na(DATE2),
      NA,
      paste0(ano, "-", sprintf("%02d", Semana_Epi))
    )
  )




#======================================================================================
#bases combinadas
B_geral_Colera <- B_geral_HCA_R %>%
  dplyr::filter(str_detect(modulo_reporte, regex("Coler", ignore_case = TRUE)))



#======================================================================================

  bd_conjunta <- B_geral_HCA_R  %>%
    dplyr::select(
      codigo_paciente,
      Influenza = Influenza,,
      `SARS-CoV-2` = SARS_CoV_2,
       DATE2,
      #v3rificar diferentes formatos de data e trazer unic formato da DATE2 para analise de semana epidemiologica
      
      

    ) %>%
    dplyr::mutate(
      # 1. Converter data com segurança
      # 2. Semana epidemiológica correta (ISO)
      ano = isoyear(DATE2),
      Semana_Epi = isoweek(DATE2),
      
      # 3. Formato combinado (muito usado em vigilância)
      Semana_Epi_ano = ifelse(
        is.na(DATE2),
        NA,
        paste0(ano, "-", sprintf("%02d", Semana_Epi))
      )
    )
  #view(bd_conjunta)
  
  
  
  Base_positividade<-bd_conjunta %>% dplyr::select(codigo_paciente,Influenza,`SARS-CoV-2`,Week=Semana_Epi,DATE2)
  
  
  
  #=================================================================
  #fim base para positividade e genomica
  #=================================================================

BD_genomica_lined_p <- BD_genomica_lined %>%
  rename(week = week)

Base_positividade_p <- Base_positividade %>%
  rename(week = Week)

Base_genomica_positividade <- merge(
  BD_genomica_lined_p,
  Base_positividade_p,
  by = "week",
  all.x = TRUE
)

positivity_genomica<- Base_genomica_positividade %>% dplyr::select(Ano, week, lineage="Subtype.Lineage.Clade.and.Pagolin.",Patogeno, vigilancia)

rm(BD_genomica_lined_p, Base_positividade_p,Base_genomica_positividade)
rm(bd_conjunta)



# mutete semana epi
B_geral_Colera <- B_geral_Colera %>%
  dplyr::mutate(
    # 1. Converter data com segurança
    DATE2 = DATE2,
    
    # 2. Ano epidemiológico correto (ISO)
    ano = isoyear(DATE2),
    
    # 3. Semana epidemiológica
    Semana_Epi = isoweek(DATE2),
    
    # 4. Ano + semana formatado
    Semana_Epi_ano = ifelse(
      is.na(DATE2),
      NA,
      paste0(ano, "-", sprintf("%02d", Semana_Epi))
    )
  )




#calcular faixa etaria de acordo com

B_geral_Colera <- B_geral_Colera %>%
  dplyr::mutate(
    faixa_etaria = case_when(
      idade_complement >= 0  & idade_complement < 2  ~ "00 < 02",
      idade_complement >= 2  & idade_complement < 5  ~ "02 < 05",
      idade_complement >= 5  & idade_complement <= 15 ~ "05 < 15",
      idade_complement > 15  & idade_complement < 50 ~ "15 < 50",
      idade_complement >= 50 & idade_complement < 65 ~ "50 < 65",
      idade_complement >= 65                      ~ "65+",
      TRUE ~ NA_character_
    )
  )



B_geral_HCA_R <- B_geral_HCA_R %>%
  dplyr::mutate(
    faixa_etaria = case_when(
      idade_complement >= 0  & idade_complement < 2  ~ "00 < 02",
      idade_complement >= 2  & idade_complement < 5  ~ "02 < 05",
      idade_complement >= 5  & idade_complement <= 15 ~ "05 < 15",
      idade_complement > 15  & idade_complement < 50 ~ "15 < 50",
      idade_complement >= 50 & idade_complement < 65 ~ "50 < 65",
      idade_complement >= 65                      ~ "65+",
      TRUE ~ NA_character_
    )
  )




#===================================================================
 #fim da base de dados de colera
#===================================================================




#definicao de provincia de testagem


B_geral_HCA_R<- B_geral_HCA_R %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codig_paciente=as.character(codigo_paciente), ""))),
    prov_testagem = case_when(
      str_detect(codigo_trim, "^[IL]DS(0|W|C)") ~ "MAPUTO",
      str_detect(codigo_trim, "^IDST") ~ "TETE",
      str_detect(codigo_trim, "^IDSQ")  ~ "QUELIMANE",
      TRUE ~ "Outro"
    ), 
    modulo_reporte=toupper(modulo_reporte)
  ) %>%
  filter(prov_testagem %in% c("MAPUTO", "TETE", "QUELIMANE"))



#corregir provincias na variavel provincia_casos da base B_geral_HCA_R, onde for maputo_cidade corregir para Maputo Cidade, onde for maputo_provincia/Maputo_Provincia corregir para Maputo Provincia, tete para Tete,quelimane par Quelinmane etc

B_geral_HCA_R <- B_geral_HCA_R %>%
  mutate(
    provincia_casos = case_when(
      # Detecta "MAPUTO CIDADE" ou "MAPUTO_CIDADE"
      str_detect(toupper(provincia_casos), "MAPUTO[ _]CIDADE") ~ "Maputo Cidade",
      
      # Detecta "MAPUTO PROVINCIA" ou "MAPUTO_PROVINCIA"
      str_detect(toupper(provincia_casos), "MAPUTO[ _]PROVINCIA") ~ "Maputo Provincia",
      
      # Detecta "TETE"
      str_detect(toupper(provincia_casos), "TETE") ~ "Tete",
      
      # Detecta "QUELIMANE"
      str_detect(toupper(provincia_casos), "QUELIMANE") ~ "Quelimane",
      
      # Mantém o valor original caso não entre em nenhuma das condições acima
      TRUE ~ provincia_casos
    )
  )

## 

# =========================================================
# 2. BASE ORIGINAL (MANTIDA)
# =========================================================

#if (FALSE) {
  
Data_map_inicial <- B_geral_HCA_R %>% 
  dplyr::select(
    DATE2 = DATE2,
    vigilancia      = vigilancia,
    modulo_ras,
    modulo_colera,
    modulo_tifoide,
    provincia_casos = provincia_casos,
    distrito_casos  = distrito_casos,
    bairro          = bairro,
    unidade_sanitaria = unidade_sanitaria,
    Influenza       = Influenza,
    SARS_CoV_2      = SARS_CoV_2,
    resul_colera,
    resul_tifoide
  ) %>%
  mutate(
    # 1. Conversão segura da data
    DATE2,
    # 2. Semana epidemiológica correta
    ano = isoyear(DATE2),
    Semana_Epi = isoweek(DATE2),
    
    # 3. Identificador completo (RECOMENDADO)
    Semana_Epi_ano = ifelse(
      is.na(DATE2),
      NA,
      paste0(ano, "-", sprintf("%02d", Semana_Epi))
    )
  )
#}

rm(base_intermediate,bd_ids_lab_us)

#filtrar na base base_geral_HCA_R os resultado da vigilancia comunitaria
BD_Final_VC_R <-B_geral_HCA_R %>% dplyr::filter(vigilancia == "Comunitaria")
BD_Final_VH_R <-B_geral_HCA_R %>% dplyr::filter(vigilancia == "Hospitalar")
BD_Final_VA_R <-B_geral_HCA_R %>% dplyr::filter(vigilancia == "Ambiental")

