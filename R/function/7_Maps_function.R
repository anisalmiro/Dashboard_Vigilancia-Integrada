# =========================================================
# 1. PACOTES
# =========================================================

Data_map <- Data_map_inicial %>%
  filter(modulo == "IRAS")




# =========================================================
# 3. FUNÇÃO DE NORMALIZAÇÃO (PADRÃO GIS)
# =========================================================
normalizar_bairro <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}





# =========================================================
# 3. TABELA COMPLETA DE EQUIVALÊNCIA
# =========================================================
equivalencia_bairros <- tribble(
  ~bairro_raw, ~bairro_geo,
  
  # ---------------- MAPUTO CIDADE ----------------
  "25 de junho", "25 de Junho",
  "25 de junho b", "25 de Junho",
  "25_de_junho_b", "25 de Junho",
  
  "abel jafar", "Abel Jafar",
  
  "aeroporto", "Aeroporto",
  "aeroporto a", "Aeroporto",
  
  "albazim", "Albazine",
  "albazine", "Albazine",
  
  "b central", "B Central",
  
  "bairro ferroviario", "Bairro Ferroviário",
  "bairro ferroviário", "Bairro Ferroviário",
  "ferroviario", "Bairro Ferroviário",
  
  "bairro zimpeto", "Zimpeto",
  
  "boquisso", "Boquisso",
  
  "chamanculo", "Chamanculo",
  "chiango", "Chiango",
  "choupal", "Choupal",
  
  "costa de sol", "Costa do Sol",
  "costa do sol", "Costa do Sol",
  "costa de sol mapulene", "Costa do Sol",
  
  "cumbeza", "Cumbeza",
  
  "fplm", "FPLM",
  
  "george de mitrov", "George Dimitrov",
  "jorge de mitrove", "George Dimitrov",
  "george dimitrov", "George Dimitrov",
  "george_dimitrov", "George Dimitrov",
  
  "grande maputo", "Grande Maputo",
  
  "guava", "Guava",
  
  "hulene", "Hulene",
  "hulene a", "Hulene A",
  "hulene b", "Hulene B",
  "hulene d", "Hulene D",
  "huleneb", "Hulene B",
  
  "infulene", "Infulene",
  "inhagoi", "Inhagoi",
  
  "katembe", "Katembe",
  
  "laulane", "Laulane",
  "laulane b", "Laulane B",
  
  "luis cabral", "Luís Cabral",
  "luis  cabral", "Luís Cabral",
  
  "mafalala", "Mafalala",
  
  "mag c", "Magazine C",
  "magazine c", "Magazine C",
  
  "magoanine", "Magoanine",
  "magoamine", "Magoanine",
  "magonine b", "Magoanine B",
  "mgoanine b", "Magoanine B",
  "magoanine a", "Magoanine A",
  "magoanine b", "Magoanine B",
  "magoanine c", "Magoanine C",
  "magoanine cmc", "Magoanine CMC",
  "magoanine 11 5", "Magoanine 11,5",
  "magoanine 13 5", "Magoanine 13,5",
  
  "mahotas", "Mahotas",
  "mahotas albazine", "Mahotas",
  
  "malhangalene", "Malhangalene",
  "malhazine", "Malhazine",
  
  "matedene", "Matedene",
  "matendene", "Matedene",
  
  "mateque", "Mateque",
  "matibwana", "Matibwana",
  
  "mavalane", "Mavalane",
  "mavalane a", "Mavalane A",
  "mavalane b", "Mavalane B",
  
  "maxaquene", "Maxaquene",
  "maxaquene a", "Maxaquene A",
  "maxaquene b", "Maxaquene B",
  "maxaquene c", "Maxaquene C",
  "maxaquene d", "Maxaquene D",
  "maxaquened", "Maxaquene D",
  
  "muhalaze", "Muhalaze",
  "mulumbela", "Mulumbela",
  "muntanhane", "Muntanhane",
  
  "pcb", "PCB",
  "p c b", "PCB",
  "pca", "PCB",
  
  "pescador", "Pescadores",
  "pescadores", "Pescadores",
  
  "polana canico", "Polana Caniço",
  "polana canico a", "Polana Caniço A",
  "polana canico b", "Polana Caniço B",
  
  "romao", "Romão",
  
  "urbanizacao", "Urbanização",
  "urbanisation", "Urbanização",
  
  "xipamanine", "Xipamanine",
  "zimpeto", "Zimpeto",
  "Nkobe", "cobe",
  
  # ---------------- MAPUTO PROVÍNCIA ----------------
  "1 de maio", "1º de Maio",
  "1o de maio", "1º de Maio",
  
  "3 de fevereiro", "3 de Fevereiro",
  "3 de feverreiro", "3 de Fevereiro",
  
  "agostinho neto", "Agostinho Neto",
  
  "benfica", "Benfica",
  "boane", "Boane",
  "bobole", "Bobole",
  
  "chiboene", "Chiboene",
  "cocole", "Cocole",
  
  "dona verde", "Dona Verde",
  "eduardo mondlane", "Eduardo Mondlane",
  
  "intaka", "Intaka",
  "intaka 1", "Intaka",
  "intaka 2", "Intaka",
  "intaka i", "Intaka",
  "intaka2", "Intaka",
  
  "inhaca", "Inhaca",
  
  "khongolote", "Khongolote",
  "kongolote", "Khongolote",
  
  "macaneta", "Macaneta",
  "madeira", "Madeira",
  
  "marracuene", "Marracuene",
  
  "matola", "Matola",
  "matola gare", "Matola Gare",
  "matola gar", "Matola Gare",
  
  "michafutene", "Michafutene",
  
  "mumemo", "Mumemo",
  "mumemo 1", "Mumemo",
  
  "ndavela", "Ndavela",
  "ndjavela", "Ndavela",
  "ndlavela", "Ndavela",
  
  "patrice lumumba", "Patrice Lumumba",
  "patricia lumumba", "Patrice Lumumba",
  
  "santa isabel", "Santa Isabel",
  "santander isabel", "Santa Isabel",
  
  "txumene", "Txumene",
  "tchumene 2", "Txumene",
  
  "zona verde", "Zona Verde",
  "zona_verde", "Zona Verde",
  "zonal verde", "Zona Verde",
  
  # ---------------- TETE ----------------
  "chiuta", "Chiuta",
  "filipe samuel magaia", "Filipe Samuel Magaia",
  "josina machel", "Josina Machel",
  "mateus sansao muthemba", "Mateus Sansão Muthemba",
  "matundo", "Matundo",
  "mpadue", "Mpadue",
  "samora machel", "Samora Machel"
)


# =========================================================
# 4. PREPARAR TABELA DE EQUIVALÊNCIA (SEM DISTINCT)
# =========================================================
# Estrutura esperada:
# equivalencia_bairros:
# - bairro_raw  -> como aparece na base
# - bairro_geo  -> nome oficial do shapefile

equivalencia_bairros_norm <- equivalencia_bairros %>%
  mutate(
    bairro_norm = normalizar_bairro(bairro_raw)
  ) %>%
  select(bairro_norm, bairro_geo)


# =========================================================
# 2. PREPARAR TABELA DE EQUIVALÊNCIA (COM DISTINCT)
# =========================================================

equivalencia_bairros_clean <- equivalencia_bairros %>%
  mutate(
    bairro_norm = normalizar_bairro(bairro_raw)
  ) %>%
  distinct(bairro_norm, .keep_all = TRUE) %>%
  select(bairro_norm, bairro_geo)


# =========================================================

Data_map_corrigido <- Data_map %>%
  mutate(
    bairro_original = bairro,
    bairro_norm = normalizar_bairro(bairro)
  ) %>%
  left_join(
    equivalencia_bairros_clean,
    by = "bairro_norm"
  ) %>%
  mutate(
    bairro_geo = if_else(
      is.na(bairro_geo) | bairro_geo == "",
      bairro_original,
      bairro_geo
    )
  )



# =========================================================
# 5. CORRIGIR BAIRROS (SEM PERDER LINHAS)
# =========================================================
B_Map_flu_sars <- Data_map %>%
  mutate(
    bairro_original = bairro,
    bairro_norm = normalizar_bairro(bairro)
  ) %>%
  left_join(
    equivalencia_bairros_norm,
    by = "bairro_norm"
  ) %>%
  mutate(
    bairro_geo = if_else(
      !is.na(bairro_geo),
      bairro_geo,
      bairro_original
    )
  ) %>%
  select(-bairro_norm)


rm(Data_map,data_map_inicial,equivalencia_bairros,equivalencia_bairros_norm,equivalencia_bairros_clean,bairros_sf)



# =========================================================
# PACOTES
# =========================================================


# =========================================================
# SHAPEFILE DOS BAIRROS
# =========================================================
bairros_sf <- st_read("shape/Cinco Cidades/Maputo/Bairrosd Maputo e Matola.shx")

# Definir CRS caso esteja ausente
if (is.na(st_crs(bairros_sf))) {
  st_crs(bairros_sf) <- 32736   # UTM 36S
}

bairros_sf <- bairros_sf %>%
  dplyr::rename(bairro_geo = BAIRRO)


bairros_sf <- bairros_sf %>%
  mutate(
    bairro_geo = bairro_geo %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all("[^a-z0-9 ]", "")
  )

Data_map_corrigido <- Data_map_corrigido %>%
  mutate(
    bairro_geo = bairro_geo %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all("[^a-z0-9 ]", "")
  )

# =========================================================
# AGREGAÇÃO DOS DADOS POR BAIRRO
# =========================================================
dados_bairro <- Data_map_corrigido %>%
  dplyr::group_by(bairro_geo) %>%
  dplyr::summarise(
    total_testados = dplyr::n(),
    positivos_influenza = sum(Influenza == "positivo", na.rm = TRUE),
    positivos_sarscov2 = sum(SARS_CoV_2 == "positivo", na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# JUNÇÃO COM O SHAPEFILE
# =========================================================


mapa_dados <- bairros_sf %>%
  left_join(dados_bairro, by = "bairro_geo") %>%
  mutate(
    total_testados = ifelse(is.na(total_testados), 0, total_testados),
    positivos_influenza = ifelse(is.na(positivos_influenza), 0, positivos_influenza)
  )

# contar todos casos positivos de influenza


# =========================================================
# CLASSIFICAÇÃO PARA A LEGENDA
# =========================================================
mapa_dados <- mapa_dados %>%
  mutate(
    classe_testados = case_when(
      total_testados <= 25 ~ "1 - 25",
      total_testados <= 50 ~ "26 - 50",
      total_testados <= 75 ~ "51 - 75",
      total_testados > 75  ~ "> 75",
      TRUE ~ NA_character_
    )
  )

dim(mapa_dados)

# exportar para dir_dashboard a base mapa_dados csv
write.csv(mapa_dados, file.path(dir_dashboard, "mapa_dados_influenza_sarsc.csv"), row.names = FALSE)

#gravar em rds para C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/mapa_dados_influenza_sarsc.rds
save(mapa_dados, file = paste0("C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/mapa_dados_influenza_sarsc.rda"))

save(mapa_dados, file = paste0("C:/Github/IDS_API/IDS_Genomica/data/DB_Dashboard/mapa_dados_influenza_sarsc.rda"))



# =========================================================

