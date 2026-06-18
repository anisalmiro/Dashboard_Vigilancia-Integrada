
cli::cli_alert_info("Iniciando processo de combinação de bases de dados para análises")

#combinar base vigilancia hospitalar, comunitaria e ambiental usando o o codigo_paciente para full join
Base_v_HCA_total_aggre <- full_join(vigilancia_hospitalar, vigilancia_comunitaria, by = "codigo_paciente") %>%
  full_join(vigilancia_ambiental, by = "codigo_paciente")


Base_v_HCA_total_aggre_1 <- Base_v_HCA_total_aggre %>%
  select(
    "start.x", "end.x", "deviceid.x", "Unidade_sanitaria.x", "Codigo_do_tecnico.x", 
    "Dados_demograficos:coordenadas_IDS:Latitude.x","Dados_demograficos:coordenadas_IDS:Longitude.x",
    "Dados_demograficos:DATE1.x", "Dados_demograficos:DATE2.x",  
    "Dados_demograficos:Local_de_inclusao",
    "Dados_demograficos:sexo.x", "Dados_demograficos:conhece_nascimento_data.x", 
    "Dados_demograficos:data_nascimento.x", "Dados_demograficos:idade.x", 
    "Dados_demograficos:tipo_idade.x", "Dados_demograficos:idade2.x", 
    "Dados_demograficos:Escolaridade.x", "Dados_demograficos:Escolaridade_other.x", 
    "Dados_demograficos:Marital.x", "Dados_demograficos:Profissao.x", 
    "Dados_demograficos:Profissao_other.x", "Dados_demograficos:Relegiao.x", 
    "Dados_demograficos:Relegiao_other.x", "Dados_demograficos:provincia_de_residencia.x", 
    "Dados_demograficos:distrito_residencia.x", "Dados_demograficos:bairro.x", 
    "Dados_demograficos:Gravidez.x",
    "nota111:Tensao_arterial.x", "nota111:Sintomas.x",
    "nota111:nest_mom_febre.x", "nota111:outro_sintoma.x","nota111:Sintomas.y",
    "meta:instanceID.x", "codigo_paciente", "start.y", "end.y", "deviceid.y", 
    "Unidade_sanitaria.y", "Codigo_do_tecnico.y", "Dados_demograficos:DATE1.y", 
    "Dados_demograficos:DATE2.y", 
    "Dados_demograficos:sexo.y", "Dados_demograficos:conhece_nascimento_data.y", 
    "Dados_demograficos:data_nascimento.y", "Dados_demograficos:idade.y", 
    "Dados_demograficos:tipo_idade.y", "Dados_demograficos:idade2.y", 
    "Dados_demograficos:Escolaridade.y", "Dados_demograficos:Escolaridade_other.y", 
    "Dados_demograficos:Marital.y", "Dados_demograficos:Profissao.y", "Dados_demograficos:Esta_em_TARV.x","Dados_demograficos:Esta_em_TARV.y",
    "Dados_demograficos:Profissao_other.y",
    "Dados_demograficos:Relegiao_other.y", "Dados_demograficos:provincia_de_residencia.y", 
    "Dados_demograficos:distrito_residencia.y", "Dados_demograficos:bairro.y", "Dados_demograficos:Gravidez.y", "Dados_demograficos:ssim_meses.y", 
    "nota124:Hospitalizado.y", "nota124:Data30.y", "nota124:Motivo_Hospitalizado.y", 
    "nota124:Motivo_Hospitalizado_other.y", "nota126:doenca_cronica.y", 
    "nota126:Doen_repirat.y", "nota126:Doen_repirat_other.y", "nota126:Doe_Hepatica_cronica.y", 
    "nota126:Doe_Renal_Cronica.y", "nota126:Doe_Neuromuscular.y", "nota126:Diabetes.y", 
    "nota126:Data40.y", "nota126:Tratamen_ultimas_48h.y", "nota126:Tratamen_ult_48h_other.y", "nota124:Motivo_Hospitalizado.x",
    "nota126:colheu_amos.y", "Salmonella:Pac_resp_si_mesmo.y", "Salmonella:Q_rel_paciente.y","nota126:Diabetes.x", 
    "Salmonella:Q_rel_paciente_other.y", "note5:Tom_med_US.y", "note5:Ssim_ind_med.y", 
    "note5:antibioticos_toma_tomou.y", "note5:antibioticos_toma_tomou_other.y", 
    "note5:antibioticos_toma_t_data.y", "note5:Opac_rec_al_medic.y", "note5:Ssim_ind_medic.y", 
    "note5:Ssim_ind_medic_other.y", "note5:antibioticos_toma_tomou1.y", "note6:Foi_encam_US.y", 
    "note6:DIAG_FEITO.y", "note6:DIAG_FEITO_other.y", "note6:ATFAD_pacient.y", 
    "note6:Repeat_medic_pacient.y", "note6:APOS_CONS_HOJE.y", "note6:HA_INFOR_REPOT.y", "nota124:Hospitalizado.x",
    "note6:colheu_amostra_febre.y", "Dados_demograficos:DATE2", "WHOTA:provincia_colheita", 
    "WHOTA:dist_colheita", "WHOTA:LO_COLHEITA", "WHOTA:ptdcol", "WHOTA:tipmast", 
    "WHOTA:teccol", "WHOTA:tembie", "WHOTA:temag", "WHOTA:humreab", "WHOTA:ph", 
    "WHOTA:Turbidez", "WHOTA:condelec", "WHOTA:conamon", "WHOTA:conortf", "WHOTA:oxigenio", 
    "WHOTA:descfras", "WHOTA:consdtr",
    "WHOTA:Poliovirus", "WHOTA:Colera", "WHOTA:Febre_tifoide", "WHOTA:Outros", 
    "WHOTA:Outros_other", "WHOTA:TEC_RES_COLHEITA","Dados_demograficos:bairro.x","Dados_demograficos:bairro.y" 
  )



Base_v_HCA_total_aggre_sel <- Base_v_HCA_total_aggre_1 %>%
  
  # =========================================================
# CONSOLIDAR VARIÁVEIS DUPLICADAS (.x -> .y -> original)
# =========================================================
mutate(
  
  # ---------------- DATAS ----------------
  `Dados_demograficos:DATE2` = coalesce(
    `Dados_demograficos:DATE2.x`,
    `Dados_demograficos:DATE2.y`,
    `Dados_demograficos:DATE2`
  ),
  
  # ---------------- LOCAL ----------------
  Unidade_sanitaria = coalesce(
    `Unidade_sanitaria.x`,
    `Unidade_sanitaria.y`
  ),
  
  `Dados_demograficos:provincia_de_residencia` = coalesce(
    `Dados_demograficos:provincia_de_residencia.x`,
    `Dados_demograficos:provincia_de_residencia.y`
  ),
  
  `Dados_demograficos:data_nascimento` = coalesce(
    `Dados_demograficos:data_nascimento.x`,
    `Dados_demograficos:data_nascimento.y`
  ),
  
  `Dados_demograficos:distrito_residencia` = coalesce(
    `Dados_demograficos:distrito_residencia.x`,
    `Dados_demograficos:distrito_residencia.y`
  ),
  
  # ---------------- DEMOGRAFIA ----------------
  `Dados_demograficos:sexo` = coalesce(
    `Dados_demograficos:sexo.x`,
    `Dados_demograficos:sexo.y`
  ),
  
  `Dados_demograficos:idade` = coalesce(
    `Dados_demograficos:idade.x`,
    `Dados_demograficos:idade.y`
  ),
  
  `Dados_demograficos:tipo_idade` = coalesce(
    `Dados_demograficos:tipo_idade.x`,
    `Dados_demograficos:tipo_idade.y`
  ),
  
  `Dados_demograficos:idade2` = coalesce(
    `Dados_demograficos:idade2.x`,
    `Dados_demograficos:idade2.y`
  ),
  
  # ---------------- SINTOMAS ----------------
  `nota111:Sintomas` = coalesce(
    `nota111:Sintomas.x`,
    `nota111:Sintomas.y`
  ),
  
  
  `Dados_demograficos:Esta_em_TARV` = coalesce(
    `Dados_demograficos:Esta_em_TARV.x`,
    `Dados_demograficos:Esta_em_TARV.y`
  ),
  
  `nota126:Diabetes` = coalesce(
    `nota126:Diabetes.x`,
    `nota126:Diabetes.y`
  ),
  
  # ---------------- HOSPITALIZAÇÃO ----------------
  `nota124:Hospitalizado` = coalesce(
    `nota124:Hospitalizado.x`,
    `nota124:Hospitalizado.y`
  ),
  # ---------------- Bairro ----------------
  `bairro_comb` = coalesce(
    `Dados_demograficos:bairro.x`,
    `Dados_demograficos:bairro.y`
  ),
  
  `nota124:Motivo_Hospitalizado` = coalesce(
    `nota124:Motivo_Hospitalizado.x`,
    `nota124:Motivo_Hospitalizado.y`
  )
  
) %>%
  
  # =========================================================
# REMOVER TODAS VARIÁVEIS .x E .y
# =========================================================
select(
  -matches("\\.x$"),
  -matches("\\.y$"),
    Latitude = `Dados_demograficos:coordenadas_IDS:Latitude.x`,
    Longitude = `Dados_demograficos:coordenadas_IDS:Longitude.x`
  #pegar todas outras variaveis
) %>%
  
  # =========================================================
# REMOVER PREFIXOS
# =========================================================
rename_with(
  ~ gsub(
    "^(nota111:|nota123:|nota124:|nota126:|note5:|note6:|meta:)",
    "",
    .
  )
)



Base_v_HCA_total_aggre_sel_1 <- Base_v_HCA_total_aggre_sel %>%
  # 1. Filtramos valores nulos, vacíos y que empiecen por "IDS"
  filter(!is.na(codigo_paciente), 
         codigo_paciente != "", 
         str_detect(codigo_paciente, "^IDS")) %>%
  # 2. Eliminamos duplicados basados en el ID del paciente
  distinct(codigo_paciente, .keep_all = TRUE)


BD_VH_VC_VA_Intermidiaria <- Base_v_HCA_total_aggre_sel_1 %>% left_join(vig_laboratorial, by = c("codigo_paciente" = "cod_amostra"))





#resultado da base de daods do AGGREGATE

resultado_testagem_sel <- resultado_testagem %>%
  select(
    "detalhes:data_informe", 
    "detalhes:codido_do_teste2", 
    "detalhes:modulo", 
    "local_colheita:tipo_local_colheita", 
    "testes_resultados:Resultado_de_Cultura", 
    "testes_resultados:Outro_Resultado_de_Cultura",
    "TIFOIDE:Resultado_de_Influenza", 
    "TIFOIDE:Tipo_de_Influenza", 
    "TIFOIDE:Subtipo_de_Influenza_A",
    "TIFOIDE:Subtipo_de_Influenza_B", 
    "group_jz9ln80:SARSCov2",
    "group_hg1cx57:Resultado_RSV", 
    "group_hg1cx57:Tipo_de_RSV", 
    "Ident:Resultado_hemocultura",
    "meta:instanceID"
    # Adicione aqui as colunas de ID de paciente e Doença se não estiverem acima
  ) %>%
  # 1. Criar um marcador de prioridade: Positivo = 1, Outros = 2
  mutate(prioridade_resultado = if_any(
    c(
      "TIFOIDE:Resultado_de_Influenza",
      "group_jz9ln80:SARSCov2",
      "group_hg1cx57:Resultado_RSV",
      "Ident:Resultado_hemocultura"
      # Nota: Salmonella costuma estar na cultura ou hemocultura neste novo esquema
    ),
    ~ .x == "positivo" | .x == "Detectável"
  )) %>%
  # 2. Ordenar para que os 'TRUE' (Positivos) fiquem no topo, agrupando por paciente e doença
  arrange(
    `detalhes:codido_do_teste2`, # Substitua pelo seu ID de paciente real
    desc(prioridade_resultado),
    `detalhes:modulo` # Substitua pelo seu ID de doença real, se aplicável
  ) %>%
  # 3. Remover duplicados mantendo a primeira linha (a Positiva, se houver)
#  distinct(`detalhes:codido_do_teste2`,`detalhes:modulo`, .keep_all = TRUE) %>%
  # 4. Remover a coluna auxiliar
  select(-prioridade_resultado)



# 1. Seleção das colunas (mantendo sua estrutura original)
bd_ids_lab_ins_cent_prel <- bd_ids_lab_ins_cent %>%
  select(
    DATE2="dados_demograficos_date2",
    "resultado_lab_ins_codigo_paciente", 
    "resultado_lab_ins_selecao_da_doenca", 
    "resultado_lab_ins1_tipo_local_colheita",
    "resultado_lab_ins3_resultado_de_cultura", 
    "resultado_lab_ins3_outro_resultado_de_cultura",
    "resultado_lab_ins5_resultado_de_influenza", 
    "resultado_lab_ins5_tipo_de_influenza", 
    "resultado_lab_ins5_influenza_subtipada", 
    "resultado_lab_ins6_sars_cov2", 
    "resultado_lab_ins6_resultado_salmonella_typhi", 
    "resultado_lab_ins7_resultado_rsv", 
    "resultado_lab_ins11_resultado_hemocultura"
  ) %>%
  # 2. Criamos uma coluna temporária de pontuação/prioridade
  # Se houver "Positivo" em qualquer uma das colunas-alvo, atribuímos 1, caso contrário 2
  mutate(prioridade = if_any(
    c(
      "resultado_lab_ins5_resultado_de_influenza",
      "resultado_lab_ins6_sars_cov2",
      "resultado_lab_ins6_resultado_salmonella_typhi",
      "resultado_lab_ins7_resultado_rsv",
      "resultado_lab_ins11_resultado_hemocultura"
    ),
    ~ .x == "positivo" # Ajuste para "S" ou "Detectável" se for o caso do seu BD
  )) %>%
  # 3. Ordenamos: TRUE (positivos) vem antes de FALSE
  arrange(resultado_lab_ins_codigo_paciente, resultado_lab_ins_selecao_da_doenca, desc(prioridade)) %>%
  # 4. Removemos duplicados mantendo a primeira ocorrência (que será a positiva, se existir)
  distinct(resultado_lab_ins_codigo_paciente, resultado_lab_ins_selecao_da_doenca, .keep_all = TRUE) %>%
  # 5. Removemos a coluna de prioridade para limpar o dataset
  select(-prioridade)


bd_ids_lab_ins_cent_prel <- bd_ids_lab_ins_cent_prel %>%
  mutate(DATE2 = as.POSIXct(DATE2), # Garante que está em formato de data
         DATE2 = format(DATE2, "%Y-%m-%d"))


#combinar os resultados do agregate e os resultados do odkcentral usando o codigo do paciente como chave de junção

resultado_testagem_prelim_comb <- resultado_testagem_sel %>%
  full_join(bd_ids_lab_ins_cent_prel, by = c("detalhes:codido_do_teste2" = "resultado_lab_ins_codigo_paciente"))



resultado_testagem_comb_fn <- resultado_testagem_prelim_comb %>%
  dplyr::mutate(
    # 1. Local de Colheita
    local_colheita = coalesce(`local_colheita:tipo_local_colheita`, 
                              resultado_lab_ins1_tipo_local_colheita),
    
    # 2. Resultados de Cultura
    resultado_cultura = coalesce(`testes_resultados:Resultado_de_Cultura`, 
                                 resultado_lab_ins3_resultado_de_cultura),
    
    outro_resultado_cultura = coalesce(`testes_resultados:Outro_Resultado_de_Cultura`, 
                                       resultado_lab_ins3_outro_resultado_de_cultura),
    
    # 3. Influenza (Resultado, Tipo e Subtipos)
    resultado_influenza = coalesce(`TIFOIDE:Resultado_de_Influenza`, 
                                   resultado_lab_ins5_resultado_de_influenza),
    
    tipo_influenza = coalesce(`TIFOIDE:Tipo_de_Influenza`, 
                              resultado_lab_ins5_tipo_de_influenza),
    
    # Aqui consolidamos os subtipos A e B com a coluna 'influenza_subtipada'
    subtipo_influenza = coalesce(`TIFOIDE:Subtipo_de_Influenza_A`, 
                                 `TIFOIDE:Subtipo_de_Influenza_B`, 
                                 resultado_lab_ins5_influenza_subtipada),
    
    # 4. SARS-CoV-2
    resultado_sars_cov2 = coalesce(`group_jz9ln80:SARSCov2`, 
                                   resultado_lab_ins6_sars_cov2),
    
    # 5. RSV (Resultado e Tipo)
    resultado_rsv = coalesce(`group_hg1cx57:Resultado_RSV`, 
                             resultado_lab_ins7_resultado_rsv),
    
    tipo_rsv = `group_hg1cx57:Tipo_de_RSV`, # Não havia correspondente 'ins', mas incluída para não perder dados
    
    # 6. Hemocultura e Outros
    resultado_hemocultura = coalesce(`Ident:Resultado_hemocultura`, 
                                     resultado_lab_ins11_resultado_hemocultura),
    data_reporte = coalesce(resultado_testagem_prelim_comb$`detalhes:data_informe`, DATE2),
    modulo_reporte = coalesce(resultado_testagem_prelim_comb$`detalhes:modulo`, resultado_testagem_prelim_comb$`resultado_lab_ins_selecao_da_doenca`),
    
    resultado_salmonella_typhi = resultado_lab_ins6_resultado_salmonella_typhi,
  ) %>% 
  dplyr::select(
    # Metadados e IDs
    codigo_paciente="detalhes:codido_do_teste2",
    modulo_reporte = toupper(resultado_testagem_prelim_comb$modulo_reporte),
    modulo_reporte,
    data_reporte,
    
    # Variáveis Consolidadas
    local_colheita,
    resultado_cultura,
    outro_resultado_cultura,
    resultado_influenza,
    tipo_influenza,
    subtipo_influenza,
    resultado_sars_cov2,
    resultado_rsv,
    tipo_rsv,
    resultado_salmonella_typhi,
    resultado_hemocultura,
    "meta:instanceID"
  )
 
 
# Supondo que seu dataframe se chame 'dados'
BD_VH_VC_VA_Interm_fn <- BD_VH_VC_VA_Intermidiaria %>%
  select(
    # 1. Identificadores do Sistema
     codigo_paciente,
    
    # 2. Dados Demográficos
    `Dados_demograficos:sexo`,
    `Dados_demograficos:data_nascimento`,
    `Dados_demograficos:idade`,
    `Dados_demograficos:tipo_idade`,
    `Dados_demograficos:idade2`,
    `Dados_demograficos:provincia_de_residencia`,
    `Dados_demograficos:distrito_residencia`,
    `bairro_comb`,
    
    # 3. Informações de Inclusão e Local de Coleta (WHOTA)
    `Dados_demograficos:Local_de_inclusao`,
    `Dados_demograficos:DATE2`,
     Unidade_sanitaria,
    `WHOTA:provincia_colheita`,
    `WHOTA:dist_colheita`,
    `WHOTA:LO_COLHEITA`,
    
    # 5. Condições Clínicas e Comorbidades
    Sintomas,
    Hospitalizado,
    Motivo_Hospitalizado,
    
    # 6. Descrição da Doença e Resultados
     Amostras_colhidas,
    resultado_colera,
     instanceID
  )



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#combinar a base de dados BD_VH_VC_VA_Intermidiaria com a base de dados resultado_testagem_comb_fn usando o codigo do paciente como chave de junção
B_geral_HCA_R <- BD_VH_VC_VA_Interm_fn %>%
  inner_join(resultado_testagem_comb_fn, by = "codigo_paciente")%>%
  filter(str_detect(codigo_paciente, "^IDS"))


BD_Cot_colheita <- BD_VH_VC_VA_Interm_fn %>%
  left_join(resultado_testagem_comb_fn, by = "codigo_paciente")%>%
  filter(str_detect(codigo_paciente, "^IDS"))

#remover onde lab_us_resultado for Na
bd_ids_lab_us <- bd_ids_lab_us %>%
  dplyr::filter(!is.na(lab_us_resultado_colera))
  

rm(
  vigilancia_hospitalar,vigilancia_comunitaria,vigilancia_ambiental,
   vig_laboratorial,Base_v_HCA_total_aggre_1,bd_ids_lab_ins_cent,
   resultado_testagem_sel,df,bd_ids_HCA_central,
   BD_VH_VC_VA_Intermidiaria,left_df,right_df,df,VA_R,
   BD_VH_VC_VA_Interm_fn, resultado_testagem_comb_fn,resultado_testagem_comb_fn,resultado_testagem_prelim_comb,
  Base_v_HCA_total_aggre_sel_1,Base_v_HCA_total_aggre_sel_1,Base_v_HCA_total_aggre_sel,Base_v_HCA_total_aggre_sel,Base_v_HCA_total_aggre,Base_v_HCA_total_aggre
   
)



cli::cli_alert_success("Base de dados combinada e limpa para análises!")



#### Combinar a base da dadso  do agregate com a base Central


