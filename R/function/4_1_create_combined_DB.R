

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





