
cli::cli_alert_info("Lendo base de dados do ODK Central e extraindo componentes para análise...")

#estrair dados das diferentes componentes de vigilancia para formar bases separadas

# Hospitalar,comunitaria, ambiental
bd_ids_HCA_central <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form %in% c(
    "Form_hospital_d_demograficos", 
    "Form_Comunitaria_d_demograficos", 
    "Form_Ambiental_d_demograficos"
  ))


# Laboratório US
bd_ids_lab_us <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Formulario_laboratorio_US")

# Laboratório INS Saúde Pública
bd_ids_lab_ins <- bd_ids_combinada %>% 
  dplyr::filter(dados_demograficos_select_form == "Form_laborat_INS_Saude_publica")

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




#base ids_lab_us selecionar as colunas que contem dados_demograficos12_
bd_ids_lab_us <- bd_ids_lab_us %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("dados_demograficos12_"))

# base de dados bd_ids_lab_ins selecionar as colunas que contem detalhes_
bd_ids_lab_ins <- bd_ids_lab_ins %>%
  dplyr::select(1:dados_demograficos_select_form, starts_with("detalhes_"))


vars_genomica <- c(
  "ident_caixa_e_localizacao",
  "nota111yrtre_tipo_de_patogino1",
  "nota111yrtre_tipo_de_patogino",
  "nota111yrtre_tipo_de_patogino_othter",
  "nota111yrtre_tipo_de_especie1",
  "nota111yrtre_tipo_de_serotipo",
  "nota111yrtre_tipo_de_genotipo1",
  "nota111yrtre_tipo_de_especie2",
  "nota111yrtre_tipo_de_genotipo2",
  "nota111yrtre_tipo_de_especie3",
  "nota111yrtre_tipo_de_genotipo3",
  "nota111yrtre_tipo_de_genotipo3_clade",
  "nota111yrtre_clade_h3n2",
  "nota111yrtre_clade_h3n2_other",
  "nota111yrtre_clade_h1n1",
  "nota111yrtre_clade_h1n1_other",
  "nota111yrtre_tipo_de_especie4",
  "nota111yrtre_tipo_de_genotipo4",
  "nota111yrtre_alfa_1",
  "nota111yrtre_beta_1",
  "nota111yrtre_delta_1",
  "nota111yrtre_omicrom_1",
  "nota111yrtre_alfa",
  "nota111yrtre_beta",
  "nota111yrtre_delta",
  "nota111yrtre_omicrom",
  "nota111yrtre_tipo_de_genotipo4_othter",
  "nota111yrtre_tipo_de_especie5",
  "nota111yrtre_tipo_de_serotipo5",
  "nota111yrtre_tipo_de_genotipo5",
  "nota111yrtre_tipo_de_poliovirus",
  "nota111yrtre_metricas_de_qualida",
  "nota111yrtre_tipo_de_coverage",
  "nota111yrtre_tipo_de_depth",
  "nota111yrtre_tipo_de_read_length",
  "nota111yrtre_tipo_de_q30",
  "nota111yrtre_tipo_de_gc_content",
  "nota111yrtre_resiste_antimicrobiana",
  "nota111yrtre_tipo_resistance_genes",
  "nota111yrtre_tipo_amr_classes",
  "nota111yrtre_tipo_amr_classes_other",
  "nota111yrtre_f_virolencia_mutacoes",
  "nota111yrtre_plasmidio",
  "nota111yrtre_mutacoes_chaves",
  "nota111yrtre_cepas",
  "nota111yrtre_serotipo",
  "nota111yrtre_plasmideos",
  "nota111yrtre_resistancia",
  "nota111yrtre_genes_de_virulencia",
  "nota111yrtre_nota4trtrcdc",
  "nota111yrtre_nome_do_repositories",
  "nota111yrtre_data_submissao",
  "nota111yrtre_fastq_link_localizacao",
  "nota111yrtre_fasta_link_localizacao"
)

bd_ids_genomica <- bd_ids_genomica %>%
  dplyr::select(
    1:dados_demograficos_select_form,
    dplyr::all_of(vars_genomica)
  )



#sabe all into /raw/cental_database csv files

readr::write_csv(bd_ids_combinada, file.path(dir_backup_raw_combined, "bd_ids_completa_HCLAG.csv"))
readr::write_csv(bd_ids_hospitalar, file.path(dir_backup_raw_combined, "bd_ids_hospitalar.csv"))
readr::write_csv(bd_ids_comunitaria, file.path(dir_backup_raw_combined, "bd_ids_comunitaria.csv"))
readr::write_csv(bd_ids_ambiental, file.path(dir_backup_raw_combined, "bd_ids_ambiental.csv"))
readr::write_csv(bd_ids_lab_us, file.path(dir_backup_raw_combined, "bd_ids_lab_us.csv"))
readr::write_csv(bd_ids_lab_ins, file.path(dir_backup_raw_combined, "bd_ids_lab_ins.csv"))
readr::write_csv(bd_ids_genomica, file.path(dir_backup_raw_combined, "bd_ids_genomica.csv"))
readr::write_csv(bd_ids_seguimento, file.path(dir_backup_raw_combined, "bd_ids_seguimento.csv"))
readr::write_csv(bd_ids_agregados, file.path(dir_backup_raw_combined, "bd_ids_agregados.csv"))
readr::write_csv(bd_ids_casos_perdidos, file.path(dir_backup_raw_combined, "bd_ids_casos_perdidos.csv"))


cli::cli_alert_success("Bases de dados extraídas e salvas com sucesso")

