


library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(plyr)
library(cli)
library(readr)
library(tidyr)
library(here)
library(lubridate)

#====================#
# CARREGAR BASE DE DADOS
#====================#
dbll <- file.path("raw", "BASE_CONJUNTA.xlsx")
raw <- read_xlsx(dbll, sheet = "Sheet1")

# Padronizar nomes das colunas
colnames(raw) <- tolower(colnames(raw))


#============================================================#
# FUNÇÃO PARA ENCONTRAR COLUNAS IMPORTANTES
#============================================================#
find_col <- function(possible_names) {
  found <- intersect(possible_names, colnames(raw))
  if (length(found) == 0) return(NA_character_) else return(found[1])
}

col_id       <- find_col(c("codigo_paciente","codigo paciente","codigo","id","sample","codigo_pac"))
col_week     <- find_col(c("week","epiweek","epidemiologicalweek","epiw"))
col_influenza<- find_col(c("influenza","influenza_result","influenza resultado"))
col_sars     <- find_col(c("sars-cov-2","sars_cov_2","sarscov2","sars","sars_cov2","sars_cov-2"))
col_date     <- find_col(c("data","date","date2","data_coleta","data_informe","dados_demograficos:date2"))

if (any(is.na(c(col_id, col_week, col_influenza, col_sars)))) {
  stop("❌ ERROR: Missing required columns (codigo_paciente, Week, Influenza, SARS-CoV-2)")
}

#====================#
# RENOMEAR COLUNAS
#====================#
df <- raw %>%
  dplyr::rename(
    codigo_paciente = !!sym(col_id),
    Week = !!sym(col_week),
    Influenza = !!sym(col_influenza),
    SARS_CoV_2 = !!sym(col_sars)
  ) %>%
  mutate(
    codigo_paciente = as.character(codigo_paciente),
    Week = as.character(Week),
    Influenza = as.character(Influenza),
    SARS_CoV_2 = as.character(SARS_CoV_2)
  )

#====================#
# PADRONIZAR SEMANA
#====================#
df <- df %>%
  mutate(
    Week = str_trim(Week),
    Week = str_replace_all(Week, "^0+", ""),
    Week = ifelse(Week == "", NA, Week),
    Week_num = as.integer(Week)  # transforma string/número da semana em inteiro
  )

#====================#
# EXTRAR ANO DA COLUNA DE DATA
#====================#
if (!is.na(col_date) && col_date %in% colnames(df)) {
  df$data_evento <- suppressWarnings(
    parse_date_time(df[[col_date]], orders = c("ymd","dmy","mdy","Ymd","dmY"))
  ) %>% as.Date()
  df$ano <- year(df$data_evento)
} else {
  df$ano <- year(Sys.Date())
}

#====================#
# CRIAR COLUNA ANO_SEMANA (ex: 2024_W43)
#====================#
df <- df %>%
  mutate(
    Ano_Semana = paste0(
      ano, "_W", stringr::str_pad(Week_num, width = 2, pad = "0", side = "left")
    )
  )

#====================#
# CRIAR FATOR ORDENADO
#====================#
week_levels <- df %>%
  distinct(Ano_Semana) %>%
  arrange(Ano_Semana) %>%
  pull(Ano_Semana)

df <- df %>%
  mutate(
    Ano_Semana_f = factor(Ano_Semana, levels = week_levels)
  )

#====================#
# DEFINIR TIPO DE VIGILÂNCIA
#====================#
df <- df %>%
  mutate(
    codigo_trim = str_trim(toupper(coalesce(codigo_paciente, ""))),
    setting = case_when(
      str_detect(codigo_trim, "^[IL]DSW") ~ "Wastewater",
      str_detect(codigo_trim, "^IDSC")    ~ "Community",
      str_detect(codigo_trim, "^IDS")     ~ "Health Care Facility",
      TRUE ~ "Other"
    )
  ) %>%
  filter(setting %in% c("Community", "Health Care Facility", "Wastewater"))

#====================#
# LIMPAR RESULTADOS
#====================#
clean_result <- function(x) {
  x2 <- tolower(str_trim(coalesce(x, "")))
  x2[x2 %in% c("", "-", "na", "n/a")] <- NA_character_
  return(x2)
}

df <- df %>%
  mutate(
    influenza_clean = clean_result(Influenza),
    sars_clean      = clean_result(SARS_CoV_2)
  )

#====================#
# FUNÇÃO – CALCULAR POSITIVIDADE
#====================#

compute_positivity <- function(df, result_col) {
  
  # Garantir que coluna Ano_Semana_f existe
  if (!"Ano_Semana_f" %in% colnames(df)) {
    stop("❌A coluna 'Ano_Semana_f' não existe. Crie-a antes de calcular positividade.")
  }
  
  df %>%
    dplyr::filter(!is.na(.data[[result_col]])) %>%
    dplyr::group_by(setting, Ano_Semana_f, ano, Week_num) %>%
    dplyr::summarise(
      tests = dplyr::n(),
      positives = sum(.data[[result_col]] == "positivo", na.rm = TRUE),
      positivity = ifelse(tests > 0, 100 * positives / tests, 0),
      .groups = "drop"
    ) %>%
    dplyr::ungroup()
}



influenza_pos <- compute_positivity(df, "influenza_clean")
sars_pos      <- compute_positivity(df, "sars_clean")


#====================#
# REGRA WASTEWATER
#====================#
influenza_pos <- influenza_pos %>%
  mutate(positivity = ifelse(setting=="Wastewater" & positives>0, 100, positivity))

sars_pos <- sars_pos %>%
  mutate(positivity = ifelse(setting=="Wastewater" & positives>0, 100, positivity))

#====================#
# FUNÇÃO – PLOTAR
#====================#
plot_by_pathogen <- function(pos_df, title_text, out_file) {
  
  pos_df <- pos_df %>%
    arrange(setting, Week_num)
  
  colors <- c(
    "Community" = "#f39c12",
    "Health Care Facility" = "#7fc8ff",
    "Wastewater" = "#c41f1f"
  )
  
  p <- ggplot(pos_df, aes(x = Ano_Semana_f, y = positivity, color = setting)) +
    geom_line(group = 1, size = 1) +
    geom_point(size = 2) +
    facet_grid(rows = vars(setting)) +
    scale_color_manual(values = colors) +
    scale_y_continuous(limits = c(0,100)) +
    labs(
      x = "Epidemiological Week / Year",
      y = "Positivity Rate (%)",
      title = title_text
    ) +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(out_file, plot = p, dpi = 300, width = 18, height = 9)
  message("Saved plot: ", out_file)
  
  return(p)
}

#====================#
# PLOTAR GRÁFICOS
#====================#
plot_by_pathogen(influenza_pos, "Influenza Positivity Surveillance", "Influenza_Pos.png")
plot_by_pathogen(sars_pos, "SARS-CoV-2 Positivity Surveillance", "SARS-CoV-2_Pos.png")

#====================#
# EXPORTAÇÃO PARA DASHBOARD
#====================#
influenza_pos_v <- influenza_pos %>% mutate(tipo="Influenza")
sars_pos_v      <- sars_pos %>% mutate(tipo="SARS-CoV-2")

combined_pos_sars_inf <- bind_rows(influenza_pos_v, sars_pos_v)

combined_pos_sars_inf$setting <- plyr::revalue(
  combined_pos_sars_inf$setting,
  c("Community" = "COMUNIDADE",
    "Health Care Facility" = "HOSPITAL",
    "Wastewater" = "AGUAS RESIDUAIS")
)

# mostrar os graficos gerados de positividade



write_csv(combined_pos_sars_inf, "Sars_Influenza_Positivity_Data.csv")
