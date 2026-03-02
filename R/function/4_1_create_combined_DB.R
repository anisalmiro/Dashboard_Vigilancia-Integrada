

#combinar base COMUNITARI, HOSPITALAR com a base de resulatados de testagem usando osseguintes codigos left_key <- "Dados_demograficos:codigo_paciente" e right_key <- "detalhes:codido_do_teste2"

cli::cli_alert_info("Combinando base de dado Vig e resultados de testagem para analises")
left_df <- BD_VH_VC_Preliminar
right_df <- resultado_testagem

left_key <- "Dados_demograficos:codigo_paciente"
right_key <- "detalhes:codido_do_teste2"

if (!left_key %in% names(left_df)) stop(paste("Key", left_key, "not found in `BD_VH_VC_Preliminar`"))
if (!right_key %in% names(right_df)) stop(paste("Key", right_key
, "not found in `resultado_testagem`"))
codes_left <- unique(left_df[[left_key]])
codes_right <- unique(right_df[[right_key]])

not_found_codes <- setdiff(codes_left, codes_right)
n_not_found <- length(not_found_codes)

cat("Codigos na basede dados combinada de nome  `BD_VH_VC_Preliminar` nao foram encontrados na base de resultados nomeado `resultado_testagem`:", n_not_found, "\n")

BD_C_H_R <- left_df %>%
  left_join(right_df, by = setNames(right_key, left_key))


rm(left_df,right_df,left_key,right_key)
cli::cli_alert_success("Base de dado combinada com sucesso para analises")


#combinar base Ambiental com a base de resulatados de testagem usando osseguintes codigos left_key <- "Dados_demograficos:codigo_paciente" e right_key <- "detalhes:codido_do_teste2"


cli::cli_alert_info("Combinando base de dado Vigilancia Ambiental e resultados de testagem")
left_df <- vigilancia_ambiental
right_df <- resultado_testagem

left_key <- "Dados_demograficos:codigo_paciente"
right_key <- "detalhes:codido_do_teste2"

if (!left_key %in% names(left_df)) stop(paste("Key", left_key, "not found in `vigilancia_ambiental`"))
if (!right_key %in% names(right_df)) stop(paste("Key", right_key, "not found in `resultado_testagem`"))

codes_left <- unique(left_df[[left_key]])
codes_right <- unique(right_df[[right_key]])

not_found_codes <- setdiff(codes_left, codes_right)
n_not_found <- length(not_found_codes)

cat("Codigos na basede dados combinada de nome  `vigilancia_ambiental` nao foram encontrados na base de resultados nomeado `resultado_testagem`:", n_not_found, "\n")

VA_R <- left_df %>%
  left_join(right_df, by = setNames(right_key, left_key))

rm(left_df,right_df,left_key,right_key)
cli::cli_alert_success("Base de dado combinada com sucesso para analises")



#bse geral Hospital, comunitariaeambiental

cli::cli_alert_info("Combinando base de dado Vig e resultados de testagem para analises")
left_df <- BD_VH_VC_VA_Intermidiaria
right_df <- resultado_testagem

left_key <- "Dados_demograficos:codigo_paciente"
right_key <- "detalhes:codido_do_teste2"

if (!left_key %in% names(left_df)) stop(paste("Key", left_key, "not found in `BD_VH_VC_Preliminar`"))
if (!right_key %in% names(right_df)) stop(paste("Key", right_key
                                                , "not found in `resultado_testagem`"))
codes_left <- unique(left_df[[left_key]])
codes_right <- unique(right_df[[right_key]])

not_found_codes <- setdiff(codes_left, codes_right)
n_not_found <- length(not_found_codes)

cat("Codigos na basede dados combinada de nome  `BD_VH_VC_VA_Intermidiaria` nao foram encontrados na base de resultados nomeado `resultado_testagem`:", n_not_found, "\n")

B_geral_HCA_R_Final<- inner_join(BD_VH_VC_VA_Intermidiaria,resultado_testagem, by = c("Dados_demograficos:codigo_paciente" = "detalhes:codido_do_teste2"))

B_geral_HCA_R <- B_geral_HCA_R_Final

rm(B_geral_HCA_R_Final)


#adicionar variavel vigilancia na base combinada B_geral_HCA apartir do codigo do paciente
B_geral_HCA_R <- B_geral_HCA_R %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codig_paciente=as.character(B_geral_HCA_R$`Dados_demograficos:codigo_paciente`), ""))),
    vigilancia = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Ambiental",
      str_detect(codigo_trim, "^IDSC") ~ "Comunitaria",
      str_detect(codigo_trim, "^IDS")  ~ "Hospitalar",
      TRUE ~ "Outro"
    )
  ) %>%
  filter(vigilancia %in% c("Comunitaria", "Hospitalar", "Ambiental"))


cli::cli_alert_success("Base de dado combinada com sucesso para analises")

# mutete semana epi
B_geral_HCA_R <- B_geral_HCA_R %>%
  mutate(
    DATE2 = lubridate::parse_date_time(`Dados_demograficos:DATE2`, orders = c("Ymd", "Y-m-d", "dmY", "d/m/Y", "mdY", "m/d/Y", "Ymd HMS", "dmY HMS")),
    Semana_Epi_ano = paste0(lubridate::year(DATE2), "-", sprintf("%02d", lubridate::isoweek(DATE2))),
    ano = lubridate::year(DATE2),
    Semana_Epi = lubridate::isoweek(DATE2)
  )



# combinar bse base BD_VH_VC_VA_Intermidiaria com base vig_laboratorial
cli::cli_alert_info("Combinando base de dado BD_VH_VC_VA_Intermidiaria e vig_laboratorial para analises")



# full join na base hospitalar e laboratorial

vig_laboratorialp <- vig_laboratorial %>%
  mutate(
    `Dados_demograficos:codigo_paciente` =
      `Dados_demograficos:codigo_amostra_colera`
  )

vig_laboratorialp <- vig_laboratorialp %>%
  filter(str_detect(`Dados_demograficos:Amostras_colhidas`, regex("Coler", ignore_case = TRUE)))


B_geral_HC_Lab <- inner_join(
  vigilancia_hospitalar,
  vig_laboratorialp,
  by = "Dados_demograficos:codigo_paciente"
)


#esta base seleciona as variaveis de interesse para analise preliminar de colera
BD_Prelim_lab_analise<- B_geral_HC_Lab %>% select(start_date=start.x, data_reorte1="Dados_demograficos:DATE2.x",`Dados_demograficos:codigo_paciente`,Unidade_sanitaria,
                                                        "Dados_demograficos:codigo_amostra_colera",
                                                        "Dados_demograficos:Tipo_d_amostra_iras",
                                                        `Dados_demograficos:Amostras_colhidas`,
                                                        "Dados_demograficos:provincia_de_residencia",
                                                        "Dados_demograficos:distrito_residencia",
                                                        "Dados_demograficos:bairro" ,
                                                        "Dados_demograficos:amostra_testada" ,
                                                        "Dados_demograficos:tipo_amostra",
                                                        "Dados_demograficos:resultado_colera",
                                                        "Dados_demograficos:bairro","Dados_demograficos:data_nascimento",
                                                        "Dados_demograficos:idade2","Dados_demograficos:idade","Dados_demograficos:sexo","Dados_demograficos:distrito_residencia",
                                                        "nota111:Sintomas","Dados_demograficos:Tipo_d_amostra_iras",
                                                        "Dados_demograficos:bairro","nota111:Sintomas","nota111:outro_sintoma" ,"nota124:Hospitalizado","nota124:Motivo_Hospitalizado_other",
                                                        "meta:instanceID.y")



#criar idade unica usando `Dados_demograficos:idade` e `Dados_demograficos:idade2` d Base BD_Prelim_lab_analise
BD_Prelim_lab_analise <- BD_Prelim_lab_analise %>%
  mutate(
    idade_complement = coalesce(`Dados_demograficos:idade`,
                             `Dados_demograficos:idade2`)
  )

rm(vig_laboratorialp,B_geral_HC_Lab)
#remove(B_geral_HCA_Lab,B_geral_HCA_LabP)

names(BD_Prelim_lab_analise)

#combinar base BD_Prelim_lab_analise com base de resultado_testagem usando innerjoin nos codigos left_key <- "Dados_demograficos:codigo_amostra_colera" e right_key <- "detalhes:codido_do_teste2"
B_geral_Colera<- inner_join(BD_Prelim_lab_analise,resultado_testagem, by = c("Dados_demograficos:codigo_paciente" = "detalhes:codido_do_teste2"))

#dim(B_geral_Colera)

#adicionar variavel vigilancia na base combinada B_geral_Colera apartir do codigo do colera
B_geral_Colera <- B_geral_Colera %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codig_paciente=as.character(B_geral_Colera$`Dados_demograficos:codigo_amostra_colera`), ""))),
    vigilancia = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Ambiental",
      str_detect(codigo_trim, "^IDSC") ~ "Comunitaria",
      str_detect(codigo_trim, "^IDS")  ~ "Hospitalar",
      TRUE ~ "Outro"
    )
  ) %>%
  filter(vigilancia %in% c("Comunitaria", "Hospitalar", "Ambiental"))



# mutete semana epi
B_geral_Colera <- B_geral_Colera %>%
  mutate(
    DATE2 = lubridate::parse_date_time(data_reorte1, orders = c("Ymd", "Y-m-d", "dmY", "d/m/Y", "mdY", "m/d/Y", "Ymd HMS", "dmY HMS")),
    Semana_Epi_ano = paste0(lubridate::year(DATE2), "-", sprintf("%02d", lubridate::isoweek(DATE2))),
    ano = lubridate::year(DATE2),
    Semana_Epi = lubridate::isoweek(DATE2)
  )

#calcular faixa etaria de acordo com

  B_geral_Colera <- B_geral_Colera %>%
  mutate(
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



#view(B_geral_Colera)

#se tudo estiver bem remover a base B_geral_HCA_LabP, B_geral_HCA_Lab



#combinar base B_geral_HCA_LabP com base de resultado_testagem usando innerjoin nos codigos left_key <- "Dados_demograficos:codigo_paciente" e right_key <- "detalhes:codido_do_teste2"
rm(left_df,right_df,df,VA_R)

#filtrar na base combinada BD_C_H_R apartir da variavel modulo apenas casosque pertencem a vidilancia comunitaria  e outa base vigilancia hospitalar
cli::cli_alert_info("Filtrando base de dado combinada Vigilancia Comunitaria")
BD_Final_VC_R <- B_geral_HCA_R %>%
  filter(vigilancia == "Comunitaria")
cli::cli_alert_success("Base de dado combinada Vigilancia Comunitaria filtrada com sucesso para analises")

cli::cli_alert_info("Filtrando base de dado combinada Vigilancia Hospitalar")
BD_Final_VH_R <- B_geral_HCA_R %>%
  filter(vigilancia == "Hospitalar")
cli::cli_alert_success("Base de dado combinada Vigilancia Hospitalar filtrada com sucesso para analises")


cli::cli_alert_info("Filtrando base de dado combinada Vigilancia Ambiental")
BD_Final_VA_R <- B_geral_HCA_R %>%
  filter(vigilancia == "Ambiental")
cli::cli_alert_success("Base de dado combinada Vigilancia Ambiental filtrada com sucesso para analises")





