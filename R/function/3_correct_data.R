Sys.setlocale("LC_TIME", "pt_PT.UTF-8")

cli::cli_alert_info("Correcao da data de Nascimento Hospitalar")

#data de nascimento
cli::cli_alert_info("Correcao da data de Nascimento na base Comunitaria")

#converter_data(vigilancia_hospitalar, "Dados_demograficos:data_nascimento")
#converter_data(vigilancia_comunitaria, "Dados_demograficos:data_nascimento")
#converter_data(vigilancia_ambiental, "WHOTA:DATE")

corrigir_data <- function(x) {
  suppressWarnings(
    as.Date(parse_date_time(
      x,
      orders = c(
        "ymd HMS","ymd HMSz","ymd HMSOSz",
        "ymd","dmy","mdy",
        "ymd HM","dmy HM","mdy HM"
      )
    ))
  )
}

cli::cli_alert_info("Correcao da data de Nascimento na base Hospitalar")
vigilancia_hospitalar <- vigilancia_hospitalar %>%
  mutate(`Dados_demograficos:data_nascimento` = corrigir_data(`Dados_demograficos:data_nascimento`))

vigilancia_hospitalar <- vigilancia_hospitalar %>%
  mutate(`Dados_demograficos:DATE2` = case_when(
    !is.na(`Dados_demograficos:DATE2`) & `Dados_demograficos:DATE2` != "" ~
      format(
        suppressWarnings(lubridate::parse_date_time(
          `Dados_demograficos:DATE2`,
          orders = c(
            "dmy", "d-b-y", "d-b-Y",   # para 18-fev-25
            "ymd HMS", "ymd HMSz", "ymd HMSOSz",
            "ymd", "mdy"
          ),
          locale = "pt_PT.UTF-8"  # importante para meses em português
        )),
        "%d-%m-%Y"  # formato final desejado
      ),
    TRUE ~ as.character(`Dados_demograficos:DATE2`)
  ))



cli::cli_alert_info("Correcao da data de Nascimento na base Comunitaria")
vigilancia_comunitaria <- vigilancia_comunitaria %>%
  mutate(`Dados_demograficos:data_nascimento` = corrigir_data(`Dados_demograficos:data_nascimento`))

vigilancia_comunitaria <- vigilancia_comunitaria %>%
  mutate(`Dados_demograficos:DATE2` = case_when(
    !is.na(`Dados_demograficos:DATE2`) & `Dados_demograficos:DATE2` != "" ~
      format(
        suppressWarnings(lubridate::parse_date_time(
          `Dados_demograficos:DATE2`,
          orders = c(
            "dmy", "d-b-y", "d-b-Y",   # para 18-fev-25
            "ymd HMS", "ymd HMSz", "ymd HMSOSz",
            "ymd", "mdy"
          ),
          locale = "pt_PT.UTF-8"  # importante para meses em português
        )),
        "%d-%m-%Y"  # formato final desejado
      ),
    TRUE ~ as.character(`Dados_demograficos:DATE2`)
  ))



cli::cli_alert_info("Correcao da data de Testagem")

resultado_testagem <- resultado_testagem %>%
  mutate(`detalhes:data_informe` = case_when(
    !is.na(`detalhes:data_informe`) & `detalhes:data_informe` != "" ~
      format(
        suppressWarnings(lubridate::parse_date_time(
          `detalhes:data_informe`,
          orders = c(
            "dmy", "d-b-y", "d-b-Y",   # para 18-fev-25
            "ymd HMS", "ymd HMSz", "ymd HMSOSz",
            "ymd", "mdy"
          ),
          locale = "pt_PT.UTF-8"  # importante para meses em português
        )),
        "%d-%m-%Y"  # formato final desejado
      ),
    TRUE ~ as.character(`detalhes:data_informe`)
  ))


cli::cli_alert_info("Correcao da data de colheita de dados na base ambiental")
vigilancia_ambiental <- vigilancia_ambiental %>%
  mutate(`WHOTA:DATE` = corrigir_data(`WHOTA:DATE`))

cli::cli_alert_info( 'correcao do ID do teste na base de resultados de testagem')

#Limpeza na base de Resultados de Vigilancia
cli::cli_alert_success("Limpeza da base de resultados")
#BASE DE RESULTADOS
resultado_testagem<- resultado_testagem %>% 
  mutate(`detalhes:codido_do_teste2` = case_when(
    `meta:instanceID` == "uuid:e9c0bf0c-d695-4528-a95d-0eb1ae1cde83" ~ "IDS0100020_1",
    `meta:instanceID` == "uuid:77810663-056c-4f13-89f7-f2ea5fc9897d" ~ "IDS0100205_1",
    `meta:instanceID` == "uuid:e500126d-105c-4640-a192-e579182492ad" ~ "IDS0100022_1",
    `meta:instanceID` == "uuid:3b30eab9-9f35-46b6-95e5-d85cf302ca6f" ~ "IDS0100021_1",
    `meta:instanceID` == "uuid:90fc9ae4-889c-402b-87bc-b665b47caa51" ~ "IDS0100019_1",
    `meta:instanceID` == "uuid:e444e32a-a5a8-443b-b365-177d3713627f" ~ "IDS0100018_1",
    `meta:instanceID` == "uuid:7f3d641e-1d39-4043-baae-0f9dba17236f" ~ "IDS0100002",
    `meta:instanceID` == "uuid:88cb5eec-5971-49be-bda4-c659346f6849" ~ "IDS0100332",
    `meta:instanceID` == "uuid:f56e2a2d-5a29-4de3-98d2-4a68bf6360eb" ~ "IDS0100145_1",
    `meta:instanceID` == "uuid:dfb9d61d-7c9a-4bef-b7c8-bac0f1a8adb8" ~ "IDST100163",
    `meta:instanceID` == "uuid:3aa454a9-fc52-4113-9f27-1996f3a0c091" ~ "IDST100162_1",
    `meta:instanceID` == "uuid:da3069d7-0881-4ade-a68d-0d09717b7d0f" ~ "IDS0200003_1",
    `meta:instanceID` == "uuid:abc21b3b-e98d-4c09-b43a-d4a1e1afba95" ~ "IDS0200001_1",
    `meta:instanceID` == "uuid:868c3a31-0f6f-4211-971f-f73995af9f3f" ~ "IDS0200002_1",

     
    TRUE ~ `detalhes:codido_do_teste2`  # mantém os valores originais se não corresponder
  ))




#Limpeza na base de Vigilancia Hospitalar
cli::cli_alert_info("Limpeza da base hospitalar")
#VIGILANCIA HOSPITALAR
vigilancia_hospitalar<- vigilancia_hospitalar %>% 
  mutate(`Dados_demograficos:codigo_paciente` = case_when(
    `meta:instanceID` == "uuid:8bbfa1a9-bc4c-4900-8b8b-0079228161ad" ~ "IDS0100340",
    `meta:instanceID` == "uuid:32a54a4c-f51f-4924-9f75-7794d62b2cb4" ~ "IDS0100386",
    `meta:instanceID` == "uuid:7ea2fb97-2abb-4a08-ae02-b07d023c39e6" ~ "IDS0100274_1",
    `meta:instanceID` == "uuid:b8dde8a2-87e6-4eab-b785-deb0aac79675" ~ "IDS0100488",
    `meta:instanceID` == "uuid:f7ee9771-75a1-472d-b327-252a35aca379" ~ "IDS0100012",
    `meta:instanceID` == "uuid:e08bb71f-7be6-4677-bc98-e2f8788d7bb3" ~ "IDS0100084",
    `meta:instanceID` == "uuid:1f8ff1da-04dd-4ca4-9fac-7009d6717604" ~ "IDS0100173",
    `meta:instanceID` == "uuid:8bbfa1a9-bc4c-4900-8b8b-0079228161ad" ~ "IDS0100340",
    `meta:instanceID` == "uuid:82ee4810-1140-4651-9fc9-03420dfa60be" ~ "IDS0100044",
    `meta:instanceID` == "uuid:31516e65-4e79-489b-b136-2d6e81f36338" ~ "IDS0100045",
    `meta:instanceID` == "uuid:60663b8a-a315-4762-aa28-c832e55f14fc" ~ "IDS0100941",
    `meta:instanceID` == "uuid:b0354815-4dfd-421a-9bcf-1d732b6df753" ~ "IDS0100953",
    `meta:instanceID` == "uuid:8272d00d-319c-4d15-b4f9-41231a611cab" ~ "IDST100009",
    `meta:instanceID` == "uuid:606164d6-8e2f-4900-9b2d-a7a5fa7ca2a4" ~ "IDS0200215",
    `meta:instanceID` == "uuid:8272d00d-319c-4d15-b4f9-41231a611cab" ~ "IDST100009",
    `meta:instanceID` == "uuid:9d5fe6de-ae4e-48b6-923b-e10232859dd3" ~ "IDST100036",
    `meta:instanceID` == "uuid:148386f7-e6b5-47c5-92cc-b690436db754" ~ "IDST100073_1",
    `meta:instanceID` == "uuid:19e06eff-90f5-4f9d-a018-812dd7082360" ~ "IDST100107",
    `meta:instanceID` == "uuid:b0bd66db-68d5-4eb8-b5e2-f90bd49f81be" ~ "IDST100108",
    `meta:instanceID` == "uuid:dd85f66d-b9ea-4fda-8cb2-d83b7e9db807" ~ "IDST100132",
    `meta:instanceID` == "uuid:7458c3fc-1d1d-408d-a28b-9792e590bcbb" ~ "IDST100181",
    `meta:instanceID` == "uuid:5e78f7e8-fe98-453e-9bc3-2ec5be0f5995" ~ "IDST100188_1",
    
    TRUE ~ `Dados_demograficos:codigo_paciente`  # mantém os valores originais se não corresponder
  ))


cli::cli_alert_info("Limpeza da base Laboratorio")
#LABORATORIAL
vig_laboratorial <- vig_laboratorial %>% 
  mutate(`cod_amostra_iras` = case_when(
    `meta:instanceID` == "uuid:ffd1d46f-5cea-4fa1-a418-5ff290e08128" ~ "IDS0100011",
    `meta:instanceID` == "uuid:aecb799c-6af5-469b-9c68-f17665efe73e" ~ "IDS0100039",
    `meta:instanceID` == "uuid:ee7634a2-b0ed-4ca4-a5f9-6e6fc576a1dd" ~ "IDS0100063",
    `meta:instanceID` == "uuid:a1321aa3-bb82-4821-a775-b1931c498f8e" ~ "IDS0100065",
    `meta:instanceID` == "uuid:adb09246-30b0-4331-b345-e9f550aa2640" ~ "IDS0100091",
    `meta:instanceID` == "uuid:c9185d9b-8d69-4773-bc18-0b3c63c44915" ~ "IDS0100092",
    `meta:instanceID` == "uuid:046f6b08-ea0c-4904-9d3c-32543fdc9e08" ~ "IDS0100148",
    `meta:instanceID` == "uuid:ffd1d46f-5cea-4fa1-a418-5ff290e08128" ~ "IDS0100011",
    `meta:instanceID` == "uuid:aecb799c-6af5-469b-9c68-f17665efe73e" ~ "IDS0100039",
    `meta:instanceID` == "uuid:ee7634a2-b0ed-4ca4-a5f9-6e6fc576a1dd" ~ "IDS0100063",
    `meta:instanceID` == "uuid:a1321aa3-bb82-4821-a775-b1931c498f8e" ~ "IDS0100065",
    `meta:instanceID` == "uuid:c9185d9b-8d69-4773-bc18-0b3c63c44915" ~ "IDS0100092",
    `meta:instanceID` == "uuid:adb09246-30b0-4331-b345-e9f550aa2640" ~ "IDS0100091",
    `meta:instanceID` == "uuid:f19523a3-0d77-40c0-b27e-2a38fc3d30da" ~ "IDST100057",
    `meta:instanceID` == "uuid:b4fe03cf-82fe-4f40-a086-6cae09c9450b" ~  "IDST100056",
    `meta:instanceID` == "uuid:206f563b-9bb3-4a45-980b-8b5e80f5d399" ~  "IDST100110",
    `meta:instanceID` == "uuid:43c7addd-fc2c-4efd-b3db-b15f19ef9bfd" ~  "IDST100156",
    `meta:instanceID` == "uuid:acb6ddf9-89ad-4f35-831a-e36bac986c57" ~  "IDST100123",
    
    
    
    TRUE ~ `cod_amostra_iras`
  ))

#para variavel Dados_demograficos:codigo_amostra_colera


cli::cli_alert_info("Limpeza da base Laboratorio para variavel Dados_demograficos:codigo_amostra_colera")

vig_laboratorial <- vig_laboratorial %>% 
  mutate(`codigo_amostra_colera` = case_when(
    `meta:instanceID` == "uuid:ffd1d46f-5cea-4fa1-a418-5ff290e08128" ~ "IDS0100011",
    `meta:instanceID` == "uuid:aecb799c-6af5-469b-9c68-f17665efe73e" ~ "IDS0100039",
    `meta:instanceID` == "uuid:ee7634a2-b0ed-4ca4-a5f9-6e6fc576a1dd" ~ "IDS0100063",
    `meta:instanceID` == "uuid:a1321aa3-bb82-4821-a775-b1931c498f8e" ~ "IDS0100065",
    `meta:instanceID` == "uuid:adb09246-30b0-4331-b345-e9f550aa2640" ~ "IDS0100091",
    `meta:instanceID` == "uuid:c9185d9b-8d69-4773-bc18-0b3c63c44915" ~ "IDS0100092",
    `meta:instanceID` == "uuid:046f6b08-ea0c-4904-9d3c-32543fdc9e08" ~ "IDS0100148",
    `meta:instanceID` == "uuid:ffd1d46f-5cea-4fa1-a418-5ff290e08128" ~ "IDS0100011",
    `meta:instanceID` == "uuid:aecb799c-6af5-469b-9c68-f17665efe73e" ~ "IDS0100039",
    `meta:instanceID` == "uuid:ee7634a2-b0ed-4ca4-a5f9-6e6fc576a1dd" ~ "IDS0100063",
    `meta:instanceID` == "uuid:a1321aa3-bb82-4821-a775-b1931c498f8e" ~ "IDS0100065",
    `meta:instanceID` == "uuid:c9185d9b-8d69-4773-bc18-0b3c63c44915" ~ "IDS0100092",
    `meta:instanceID` == "uuid:adb09246-30b0-4331-b345-e9f550aa2640" ~ "IDS0100091",
    `meta:instanceID` == "uuid:f19523a3-0d77-40c0-b27e-2a38fc3d30da" ~ "IDST100057",
    `meta:instanceID` == "uuid:b4fe03cf-82fe-4f40-a086-6cae09c9450b" ~  "IDST100056",
    `meta:instanceID` == "uuid:206f563b-9bb3-4a45-980b-8b5e80f5d399" ~  "IDST100110",
    `meta:instanceID` == "uuid:43c7addd-fc2c-4efd-b3db-b15f19ef9bfd" ~  "IDST100156",
    `meta:instanceID` == "uuid:acb6ddf9-89ad-4f35-831a-e36bac986c57" ~  "IDST100123",
    
    
    
    TRUE ~ `codigo_amostra_colera`
  ))



#correct vigilancia laboratorial
vig_laboratorial <- vig_laboratorial %>% 
  dplyr::mutate(
    cod_amostra = coalesce(
      na_if(trimws(cod_amostra_iras), ""),
      na_if(trimws(codigo_amostra_colera), ""),
      na_if(trimws(Codigo_da_amostra_febre), "")
    )
  ) %>% 
  dplyr::select(
    cod_amostra,
    Amostras_colhidas,
    resultado_colera,
    instanceID = `meta:instanceID`
  )

#remover duplicados na base de vig_labora
vig_laboratorial <- vig_laboratorial %>%
  # Remove linhas onde cod_amostra é NA (sem informação)
  dplyr::filter(!is.na(cod_amostra)) %>%
  # Remove duplicados mantendo a primeira ocorrência
  dplyr::distinct(cod_amostra, Amostras_colhidas, .keep_all = TRUE)
#view(vigilancia_hospitalar %>% filter(vigilancia_hospitalar$`Dados_demograficos:codigo_paciente`=='57421186'))


cli::cli_alert_info("Limpeza da base Ambiental")
#Ambiental
vigilancia_ambiental<- vigilancia_ambiental %>% 
  mutate(`WHOTA:codigo_paciente` = case_when(
    `meta:instanceID` == "uuid:6db16d74-7f65-4a03-b32d-1806b281ec53" ~ "IDSW100008",
    `meta:instanceID` == "uuid:d4488fde-f7c7-4e18-8b50-02553d8c713c" ~ "IDSW100127",
    `meta:instanceID` == "uuid:5bebd392-daaf-4ec4-bac6-ca47d9fc9df3" ~ "IDSW100129",
    TRUE ~ `WHOTA:codigo_paciente`  # mantém os valores originais se não corresponder
  ))



cli::cli_alert_info("Limpeza de provincia de colhita")
vigilancia_ambiental$`WHOTA:provincia_colheita` <- plyr::revalue(
  vigilancia_ambiental$`WHOTA:provincia_colheita`,
  c("Maputo_Cidade" = "Maputo Cidade"),
  warn_missing = FALSE
)

cli::cli_alert_info("Limpeza da Unidade Sanitaria")
vigilancia_hospitalar$Unidade_sanitaria <- plyr::revalue(
  vigilancia_hospitalar$Unidade_sanitaria,
  c("CS_Mavalane" = "CS Mavalane",
    "H_geral_de_Mavalane" = "Hospital Geral Mavalane",
    "CS_Zimpeto" = "CS Zimpeto"),
  warn_missing = FALSE
) 

cli::cli_alert_info("Limpeza da local de colhita")
resultado_testagem$`local_colheita:us_colheita` <- plyr::revalue(
  resultado_testagem$`local_colheita:us_colheita`,
  c("CS_Zimpeto" = "CS Zimpeto"),
  warn_missing = FALSE
)


cli::cli_alert_info("Limpeza da unidade sanitaria")
vigilancia_comunitaria$Unidade_sanitaria <- plyr::revalue(
  vigilancia_comunitaria$Unidade_sanitaria,
  c("Clusters_de_Zimpeto" = "Cluster de Zimpeto"),
  warn_missing = FALSE
)



cli::cli_alert_info("Limpeza da tipo colheita")
resultado_testagem$`local_colheita:tipo_local_colheita` <- plyr::revalue(
  resultado_testagem$`local_colheita:tipo_local_colheita`,
  c(
    "Comunidade" = "Comunitario",
    "US" = "Hospitalar",
    "Laboratorio" = "Hospitalar"
  ),
  warn_missing = FALSE
)


cli::cli_alert_info("Limpeza das provincias")
vigilancia_hospitalar$`Dados_demograficos:provincia_de_residencia` <- plyr::revalue(
  vigilancia_hospitalar$`Dados_demograficos:provincia_de_residencia`,
  c(
    "Maputo_Cidade" = "Maputo Cidade",
    "Maputo_provincia" = "Maputo Provincia",
    "tete" = "Tete"
  ),
  warn_missing = FALSE
)


#Renomeando variaveis 
cli::cli_alert_info("Limpeza da variavel do codigo do paciente")
# Renomeia se a coluna existir

vigilancia_ambiental <- dplyr::rename(
  vigilancia_ambiental,
  `Dados_demograficos:codigo_paciente` = `WHOTA:codigo_paciente`
)

# Lista de renomeações: old_name = new_name
rename_map <- c(
  "WHOTA:coordenadas_IDS:Latitude"  = "Dados_demograficos:coordenadas_IDS:Latitude",
  "WHOTA:coordenadas_IDS:Longitude" = "Dados_demograficos:coordenadas_IDS:Longitude",
  "WHOTA:DATE"                       = "Dados_demograficos:DATE2"#,
 # "WHOTA:provincia_colheita"         = "local_colheita:provincia_colheita"
)

# Loop para renomear apenas se a coluna existir
for (old_name in names(rename_map)) {
  new_name <- rename_map[[old_name]]
  if (old_name %in% names(vigilancia_ambiental)) {
    vigilancia_ambiental <- dplyr::rename(
      vigilancia_ambiental,
      !!new_name := all_of(old_name)
    )
  }
}


cli::cli_alert_info("Limpeza da data de colhita")
vigilancia_ambiental <- vigilancia_ambiental %>%
  mutate(`Dados_demograficos:DATE2` = case_when(
    !is.na(`Dados_demograficos:DATE2`) & `Dados_demograficos:DATE2` != "" ~
      format(
        suppressWarnings(lubridate::parse_date_time(
          `Dados_demograficos:DATE2`,
          orders = c("ymd HMS", "ymd HMSz", "ymd HMSOSz", "ymd", "dmy", "mdy")
        )),
        "%d-%b-%y"
      ),
    TRUE ~ as.character(`Dados_demograficos:DATE2`)
  ))



cli::cli_alert_info("Limpeza da Unidade Subtipo A e B")
# 1. Normalizar NA / vazio

resultado_testagem <- resultado_testagem %>%
  mutate(`TIFOIDE:Subtipo_de_Influenza_A` =
           case_when(
             is.na(`TIFOIDE:Subtipo_de_Influenza_A`) ~ "Nao Tipado",
             `TIFOIDE:Subtipo_de_Influenza_A` == "" ~ "Nao Tipado",
             `TIFOIDE:Subtipo_de_Influenza_A` == "a_h3n2" ~ "A(H3N2)",
             `TIFOIDE:Subtipo_de_Influenza_A` == "a_pdm_h1n1_09" ~ "A(H1N1) pdm09",
             `TIFOIDE:Subtipo_de_Influenza_A` %in% c("a_pdm_h1n1_09 a_h3n2", "a_h3n2 a_pdm_h1n1_09") ~ "A(H1N1) pdm09",
             TRUE ~ `TIFOIDE:Subtipo_de_Influenza_A`
           )
  )


resultado_testagem <- resultado_testagem %>%
  mutate(`TIFOIDE:Subtipo_de_Influenza_B` =
           case_when(
             is.na(`TIFOIDE:Subtipo_de_Influenza_B`) ~ "Nao Tipado",
             `TIFOIDE:Subtipo_de_Influenza_B` == "" ~ "Nao Tipado",
             `TIFOIDE:Subtipo_de_Influenza_B` == "NA" ~ "Nao Tipado",
             `TIFOIDE:Subtipo_de_Influenza_B` == "b_vic" ~ "B(Victoria)",
             `TIFOIDE:Subtipo_de_Influenza_B` == "b_yamagata" ~ "B(Yamagata)",
             `TIFOIDE:Subtipo_de_Influenza_B` == "sim" ~ "Nao Tipado",
             TRUE ~ `TIFOIDE:Subtipo_de_Influenza_B`
           )
  )


#corregir para caracter a variavel nota111:FRE_RESPIRATORIA na vigilancia comunitaria e hospitalar
vigilancia_comunitaria$`nota111:FRE_RESPIRATORIA` <- as.character(vigilancia_comunitaria$`nota111:FRE_RESPIRATORIA`)
vigilancia_hospitalar$`nota111:FRE_RESPIRATORIA` <- as.character(vigilancia_hospitalar$`nota111:FRE_RESPIRATORIA`)

#corregir para caracter a variavel  nota111:Frequencia_cardiaca na base vigilancia hospitalar e comunitaria
vigilancia_comunitaria$`nota111:Frequencia_cardiaca` <- as.character(vigilancia_comunitaria$`nota111:Frequencia_cardiaca`)
vigilancia_hospitalar$`nota111:Frequencia_cardiaca` <- as.character(vigilancia_hospitalar$`nota111:Frequencia_cardiaca`)  




# corregir a base de dados B_HCA_central combinando todas variaveis semelhantes entre as bases de dados de vigilancia hospitalar, comunitaria e ambiental em uma variavel

B_HCA_central_corrigida <- B_HCA_central_prel %>%
  mutate(
    # 1. Identificação e Códigos
    codigo_paciente = coalesce(hospital1_codigo_paciente, comunitaria_codigo_paciente, ambiental_codigo_paciente),
    
    # 2. Localização / Residência
    provincia_residencia = coalesce(hospital1_provincia_residencia, comunitaria_provincia_residencia2),
    distrito_residencia  = coalesce(hospital1_distrito_residencia, comunitaria_distrito_residencia2),
    bairro_residencia    = coalesce(hospital1_bairro_residencia, comunitaria_bairro_residencia2),
    outro_bairro         = coalesce(hospital1_outro_bairro, ambiental_outro_bairro),
    residencia_tipo      = coalesce(hospital1_residencia, ambiental_residencia),
    
    # 3. Dados Demográficos
    sexo                 = coalesce(hospital1_sexo, comunitaria_sexo1),
    data_nascimento      = coalesce(hospital1_data_nascimento, comunitaria_data_nascimento1),
    conhece_data_nasc    = coalesce(hospital1_conhece_nascimento_data, comunitaria_conhece_nascimento_data1),
    idade                = coalesce(hospital1_idade, comunitaria_idade12),
    tipo_idade           = coalesce(hospital1_tipo_idade, comunitaria_tipo_idade1),
    idade2               = coalesce(hospital1_idade2, comunitaria_idade2tt),
    
    # 4. Socioeconómicos
    escolaridade         = coalesce(hospital1_escolaridade, comunitaria_escolaridade1),
    estado_civil         = coalesce(hospital1_marital, comunitaria_marital),
    profissao            = coalesce(hospital1_profissao, comunitaria_profissao1),
    
    # 5. Clínicos e Sintomas
    sintomas             = coalesce(hospital2_sintomas, comunitaria1_sintomas1),
    outro_sintoma        = coalesce(hospital2_outro_sintoma, comunitaria1_outro_sintoma1),
    motivo_hospitalizacao= coalesce(hospital5_motivo_hospitalizado, comunitaria4_motivo_hospitalizado2)
  ) %>%
  # Opcional: Remover as colunas antigas que já foram combinadas para limpar a base
  select(
    -hospital1_codigo_paciente, -comunitaria_codigo_paciente, -ambiental_codigo_paciente,
    -hospital1_provincia_residencia, -comunitaria_provincia_residencia2,
    -hospital1_distrito_residencia, -comunitaria_distrito_residencia2,
    -hospital1_bairro_residencia, -comunitaria_bairro_residencia2,
    -hospital1_outro_bairro, -ambiental_outro_bairro,
    -hospital1_residencia, -ambiental_residencia,
    -hospital1_sexo, -comunitaria_sexo1,
    -hospital1_data_nascimento, -comunitaria_data_nascimento1,
    -hospital1_conhece_nascimento_data, -comunitaria_conhece_nascimento_data1,
    -hospital1_idade, -comunitaria_idade12,
    -hospital1_tipo_idade, -comunitaria_tipo_idade1,
    -hospital1_idade2, -comunitaria_idade2tt,
    -hospital1_escolaridade, -comunitaria_escolaridade1,
    -hospital1_marital, -comunitaria_marital,
    -hospital1_profissao, -comunitaria_profissao1,
    -hospital2_sintomas, -comunitaria1_sintomas1,
    -hospital2_outro_sintoma, -comunitaria1_outro_sintoma1,
    -hospital5_motivo_hospitalizado, -comunitaria4_motivo_hospitalizado2
  )

rm(bd_ids_combinada,B_HCA_central_prel)

