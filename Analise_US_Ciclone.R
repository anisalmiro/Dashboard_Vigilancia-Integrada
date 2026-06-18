# ============================================================
# ANÁLISE DA MÉDIA DA PROPORÇÃO DE UNIDADES SANITÁRIAS
# AFECTADAS/DESTRUÍDAS POR CICLONES EM SOFALA, MOÇAMBIQUE
# Período: 2017–2024
# ============================================================


# ============================================================
# 1. DEFINIR DIRECTÓRIO DE TRABALHO
# ============================================================

setwd("C:/Users/rgdti/OneDrive - INS - Instituto Nacional de Saúde/Academia/Mestrado/Ms final")


# ============================================================
# 2. PACOTES
# ============================================================

library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(stringi)
library(readr)
library(writexl)
library(ggplot2)
library(forcats)
library(tidyr)
library(sf)
library(geodata)
library(terra)
library(patchwork)
library(tibble)
library(scales)


# ============================================================
# 3. FUNÇÕES AUXILIARES
# ============================================================

limpar_nome <- function(x) {
  x %>%
    as.character() %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("_", " ") %>%
    str_replace_all("-", " ") %>%
    str_squish()
}


padronizar_distrito <- function(x) {
  x <- limpar_nome(x)
  
  case_when(
    x %in% c("cidade da beira", "beira cidade", "beira city", "beira municipio", "beira") ~ "beira",
    x %in% c("buzi", "busi", "buzzi") ~ "buzi",
    x %in% c("caia") ~ "caia",
    x %in% c("chemba") ~ "chemba",
    x %in% c("cheringoma") ~ "cheringoma",
    x %in% c("chibabava") ~ "chibabava",
    x %in% c("dondo") ~ "dondo",
    x %in% c("gorongosa", "gorongoza") ~ "gorongosa",
    x %in% c("machanga") ~ "machanga",
    x %in% c("marromeu") ~ "marromeu",
    x %in% c("maringue") ~ "maringue",
    x %in% c("muanza") ~ "muanza",
    x %in% c("nhamatanda") ~ "nhamatanda",
    TRUE ~ x
  )
}


normalizar_01 <- function(x) {
  if (all(is.na(x))) {
    return(rep(0, length(x)))
  }
  
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    rep(0, length(x))
  } else {
    (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
}


tema_mapa <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right"
    )
}


tema_grafico <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(size = 10),
      legend.position = "bottom"
    )
}


# ============================================================
# 4. DATA FRAME: TOTAL DE US POR DISTRITO - SOFALA
# ============================================================
# Esta tabela é o denominador para calcular a proporção.
# ============================================================

us_sofala_distrito <- data.frame(
  distrito = c(
    "Beira",
    "Buzi",
    "Caia",
    "Chemba",
    "Cheringoma",
    "Chibabava",
    "Dondo",
    "Gorongosa",
    "Machanga",
    "Maringue",
    "Marromeu",
    "Muanza",
    "Nhamatanda"
  ),
  total_us = c(
    19,
    14,
    12,
    8,
    7,
    15,
    15,
    14,
    9,
    10,
    8,
    6,
    19
  )
) %>%
  mutate(
    distrito_limpo = padronizar_distrito(distrito)
  ) %>%
  dplyr::select(distrito_limpo, distrito, total_us)

print(us_sofala_distrito)


# ============================================================
# 5. LER BASE EXCEL COM TODAS AS PLANILHAS
# ============================================================

ficheiro <- "cyclone_historical-data-2017-2024.xlsx"

planilhas <- excel_sheets(ficheiro)

base_ciclones <- map_dfr(planilhas, function(nome_planilha) {
  
  read_excel(
    ficheiro,
    sheet = nome_planilha,
    col_types = "text"
  ) %>%
    mutate(
      planilha_origem = nome_planilha,
      
      ano = str_extract(nome_planilha, "\\d{4}"),
      ano = as.numeric(ano),
      
      ciclone = nome_planilha,
      ciclone = str_remove(ciclone, regex("Cyclone", ignore_case = TRUE)),
      ciclone = str_remove(ciclone, "\\s*-?\\s*\\d{4}"),
      ciclone = str_squish(ciclone),
      ciclone = str_to_title(ciclone)
    )
})


# ============================================================
# 6. FILTRAR SOFALA E LIMPAR VARIÁVEIS PRINCIPAIS
# ============================================================

base_sofala <- base_ciclones %>%
  filter(Provínce == "Sofala") %>%
  mutate(
    ano = as.numeric(ano),
    
    distrito_original = District,
    distrito_limpo = padronizar_distrito(District),
    
    us_afectadas_destruidas = parse_number(
      as.character(Health facilities affected)
    ),
    
    us_afectadas_destruidas = ifelse(
      is.na(us_afectadas_destruidas),
      0,
      us_afectadas_destruidas
    ),
    
    ciclone_ano = paste0(ciclone, " - ", ano)
  )


# Conferir ciclones existentes na base
table(base_sofala$ciclone)

# Conferir distritos
table(base_sofala$distrito_limpo)


# ============================================================
# 7. TABELA DE VELOCIDADE E CATEGORIA DOS CICLONES
# ============================================================

categoria_ciclones <- tibble::tribble(
  ~ciclone,                 ~ano, ~vento_kmh,
  "Idai And Ts",             2019, 213,
  "Tropical Storm Chalane",  2020, 124,
  "Eloise",                 2021, 157,
  "Gombe",                  2022, 183,
  "Tropical Storm Ana",      2022,  93,
  "Freddy",                 2023, 183,
  "Tropical Storm Filipo",   2024, 111
) %>%
  mutate(
    categoria_ciclone = case_when(
      is.na(vento_kmh) ~ NA_character_,
      vento_kmh <= 153 ~ "Categoria 1",
      vento_kmh >= 154 & vento_kmh <= 177 ~ "Categoria 2",
      vento_kmh >= 178 & vento_kmh <= 208 ~ "Categoria 3",
      vento_kmh >= 209 & vento_kmh <= 251 ~ "Categoria 4",
      vento_kmh >= 252 ~ "Categoria 5"
    ),
    
    categoria_ciclone = ifelse(
      categoria_ciclone %in% c(
        "Categoria 1",
        "Categoria 2",
        "Categoria 3",
        "Categoria 4"
      ),
      categoria_ciclone,
      NA
    ),
    
    categoria_ciclone = factor(
      categoria_ciclone,
      levels = c(
        "Categoria 1",
        "Categoria 2",
        "Categoria 3",
        "Categoria 4"
      )
    ),
    
    peso_categoria_ciclone = case_when(
      categoria_ciclone == "Categoria 1" ~ 0.40,
      categoria_ciclone == "Categoria 2" ~ 0.55,
      categoria_ciclone == "Categoria 3" ~ 0.75,
      categoria_ciclone == "Categoria 4" ~ 0.90,
      TRUE ~ NA_real_
    )
  )

print(categoria_ciclones)


# ============================================================
# 8. JUNTAR CATEGORIA DOS CICLONES À BASE DE SOFALA
# ============================================================

base_sofala <- base_sofala %>%
  left_join(
    categoria_ciclones,
    by = c("ciclone", "ano")
  )


# Verificar se algum ciclone ficou sem categoria
ciclones_sem_categoria <- base_sofala %>%
  filter(is.na(categoria_ciclone)) %>%
  distinct(ciclone, ano, ciclone_ano)

print(ciclones_sem_categoria)


# ============================================================
# 9. RESUMO DISTRITO × CICLONE
# ============================================================
# Aqui calculamos a proporção em cada evento ciclónico.
# Depois a média destas proporções será usada como indicador principal.
# ============================================================

resumo_distrito_ciclone <- base_sofala %>%
  group_by(
    distrito_limpo,
    ciclone,
    ano,
    ciclone_ano,
    categoria_ciclone,
    vento_kmh,
    peso_categoria_ciclone
  ) %>%
  summarise(
    nr_us_afectadas_destruidas = sum(us_afectadas_destruidas, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    us_sofala_distrito,
    by = "distrito_limpo"
  ) %>%
  mutate(
    proporcao_us_evento = ifelse(
      is.na(total_us) | total_us == 0,
      NA_real_,
      nr_us_afectadas_destruidas / total_us
    ),
    
    percent_us_evento = 100 * proporcao_us_evento,
    
    proporcao_us_evento_ajustada = ifelse(
      is.na(total_us) | total_us == 0,
      NA_real_,
      pmin(nr_us_afectadas_destruidas, total_us) / total_us
    ),
    
    percent_us_evento_ajustada = 100 * proporcao_us_evento_ajustada
  ) %>%
  arrange(ano, desc(proporcao_us_evento_ajustada))

print(resumo_distrito_ciclone)


# ============================================================
# 10. RESUMO POR DISTRITO: MÉDIA DA PROPORÇÃO POR EVENTO
# ============================================================
# Indicador principal:
# média da proporção de US afectadas/destruídas por evento ciclónico.
# ============================================================

resumo_distrito <- resumo_distrito_ciclone %>%
  group_by(distrito_limpo) %>%
  summarise(
    distrito = first(distrito),
    total_us = first(total_us),
    
    nr_us_afectadas_destruidas_acumulado = sum(
      nr_us_afectadas_destruidas,
      na.rm = TRUE
    ),
    
    media_proporcao_us_afectadas = mean(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_medio_us_afectadas = 100 * media_proporcao_us_afectadas,
    
    max_proporcao_us_afectadas = max(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_max_us_afectadas = 100 * max_proporcao_us_afectadas,
    
    proporcao_us_acumulada = ifelse(
      is.na(total_us) | total_us == 0,
      NA_real_,
      nr_us_afectadas_destruidas_acumulado / total_us
    ),
    
    percent_us_acumulada = 100 * proporcao_us_acumulada,
    
    nr_ciclones_com_impacto = n_distinct(
      ciclone_ano[nr_us_afectadas_destruidas > 0]
    ),
    
    .groups = "drop"
  ) %>%
  arrange(desc(media_proporcao_us_afectadas))


print(resumo_distrito)


# Verificar distritos sem total de US
distritos_sem_total_us <- resumo_distrito %>%
  filter(is.na(total_us)) %>%
  dplyr::select(distrito_limpo, nr_us_afectadas_destruidas_acumulado)

print(distritos_sem_total_us)


# ============================================================
# 11. RESUMO POR CICLONE
# ============================================================
# Indicador principal:
# média da proporção de US afectadas/destruídas entre distritos no evento.
# ============================================================

resumo_ciclone <- resumo_distrito_ciclone %>%
  group_by(
    ano,
    ciclone,
    ciclone_ano,
    categoria_ciclone
  ) %>%
  summarise(
    nr_distritos_afectados = sum(nr_us_afectadas_destruidas > 0, na.rm = TRUE),
    
    nr_us_afectadas_destruidas = sum(
      nr_us_afectadas_destruidas,
      na.rm = TRUE
    ),
    
    total_us_distritos_evento = sum(
      total_us,
      na.rm = TRUE
    ),
    
    media_proporcao_us_afectadas = mean(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_medio_us_afectadas = 100 * media_proporcao_us_afectadas,
    
    max_proporcao_us_afectadas = max(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_max_us_afectadas = 100 * max_proporcao_us_afectadas,
    
    proporcao_us_acumulada_evento = ifelse(
      total_us_distritos_evento == 0,
      NA_real_,
      nr_us_afectadas_destruidas / total_us_distritos_evento
    ),
    
    percent_us_acumulada_evento = 100 * proporcao_us_acumulada_evento,
    
    vento_kmh = first(vento_kmh),
    peso_categoria_ciclone = first(peso_categoria_ciclone),
    
    .groups = "drop"
  ) %>%
  arrange(ano)

print(resumo_ciclone)


# ============================================================
# 12. RESUMO POR CATEGORIA DO CICLONE
# ============================================================
# Indicador principal:
# média da proporção de US afectadas/destruídas por evento-distrito.
# ============================================================

resumo_categoria <- resumo_distrito_ciclone %>%
  filter(!is.na(categoria_ciclone)) %>%
  group_by(categoria_ciclone) %>%
  summarise(
    nr_eventos = n_distinct(ciclone_ano),
    
    nr_us_afectadas_destruidas = sum(
      nr_us_afectadas_destruidas,
      na.rm = TRUE
    ),
    
    media_proporcao_us_afectadas = mean(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_medio_us_afectadas = 100 * media_proporcao_us_afectadas,
    
    max_proporcao_us_afectadas = max(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_max_us_afectadas = 100 * max_proporcao_us_afectadas,
    
    .groups = "drop"
  )

print(resumo_categoria)


# ============================================================
# 13. GRÁFICO 1: MÉDIA DA PROPORÇÃO POR CICLONE
# ============================================================

grafico1_ciclone <- resumo_ciclone %>%
  mutate(
    ciclone_ano = factor(ciclone_ano, levels = ciclone_ano[order(ano)])
  ) %>%
  ggplot(
    aes(
      x = ciclone_ano,
      y = media_proporcao_us_afectadas,
      fill = categoria_ciclone
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = percent(media_proporcao_us_afectadas, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(resumo_ciclone$media_proporcao_us_afectadas, na.rm = TRUE) * 1.20)
  ) +
  scale_fill_manual(
    values = c(
      "Categoria 1" = "#9ECAE1",
      "Categoria 2" = "#6BAED6",
      "Categoria 3" = "#2171B5",
      "Categoria 4" = "#08306B"
    ),
    drop = FALSE
  ) +
  labs(
    #title = "Média da proporção de US afectadas/destruídas por ciclone em Sofala",
    #subtitle = "Média das proporções distritais observadas em cada evento ciclónico",
    x = "Ciclone e ano",
    y = "Média da proporção de US afectadas/destruídas",
    fill = "Categoria"
  ) +
  tema_grafico() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grafico1_ciclone


# ============================================================
# 14. GRÁFICO 2: MÉDIA DA PROPORÇÃO POR DISTRITO
# ============================================================

grafico2_distrito <- resumo_distrito %>%
  ggplot(
    aes(
      x = fct_reorder(distrito_limpo, media_proporcao_us_afectadas),
      y = media_proporcao_us_afectadas
    )
  ) +
  geom_col(width = 0.7, fill = "#2C7FB8") +
  geom_text(
    aes(label = percent(media_proporcao_us_afectadas, accuracy = 0.1)),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(resumo_distrito$media_proporcao_us_afectadas, na.rm = TRUE) * 1.20)
  ) +
  labs(
    #title = "Média da proporção de US afectadas/destruídas por distrito",
    #subtitle = "Média da proporção observada por evento ciclónico; denominador: total de US do distrito",
    x = "Distrito",
    y = "Média da proporção de US afectadas/destruídas"
  ) +
  tema_grafico() +
  theme(
    legend.position = "none"
  )

grafico2_distrito


# ============================================================
# 15. GRÁFICO 3: PARETO DA MÉDIA DA PROPORÇÃO POR DISTRITO
# ============================================================

pareto_distrito <- resumo_distrito %>%
  arrange(desc(media_proporcao_us_afectadas)) %>%
  mutate(
    distrito_limpo = factor(distrito_limpo, levels = distrito_limpo),
    acumulado = cumsum(media_proporcao_us_afectadas),
    total = sum(media_proporcao_us_afectadas, na.rm = TRUE),
    perc_acumulado = 100 * acumulado / total
  )

grafico3_pareto <- ggplot(
  pareto_distrito,
  aes(
    x = distrito_limpo,
    y = media_proporcao_us_afectadas
  )
) +
  geom_col(fill = "#3182BD", width = 0.7) +
  geom_line(
    aes(
      y = perc_acumulado * max(media_proporcao_us_afectadas, na.rm = TRUE) / 100,
      group = 1
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      y = perc_acumulado * max(media_proporcao_us_afectadas, na.rm = TRUE) / 100
    ),
    size = 2
  ) +
  scale_y_continuous(
    name = "Média da proporção de US afectadas/destruídas",
    labels = percent_format(accuracy = 1),
    sec.axis = sec_axis(
      ~ . * 100 / max(pareto_distrito$media_proporcao_us_afectadas, na.rm = TRUE),
      name = "Percentagem acumulada (%)"
    )
  ) +
  labs(
    # title = "Pareto da média da proporção de US afectadas/destruídas por distrito",
    # subtitle = "Mostra os distritos com maior impacto médio proporcional por evento",
    x = "Distrito"
  ) +
  tema_grafico() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grafico3_pareto


# ============================================================
# 16. GRÁFICO 4: HEATMAP DISTRITO × CICLONE
# ============================================================

heatmap_distrito_ciclone <- resumo_distrito_ciclone %>%
  mutate(
    ciclone_ano = factor(
      ciclone_ano,
      levels = resumo_ciclone$ciclone_ano[order(resumo_ciclone$ano)]
    ),
    distrito_limpo = fct_reorder(
      distrito_limpo,
      proporcao_us_evento_ajustada,
      .fun = sum
    )
  )

grafico4_heatmap <- ggplot(
  heatmap_distrito_ciclone,
  aes(
    x = ciclone_ano,
    y = distrito_limpo,
    fill = proporcao_us_evento_ajustada
  )
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(
      label = ifelse(
        proporcao_us_evento_ajustada > 0,
        percent(proporcao_us_evento_ajustada, accuracy = 0.1),
        ""
      )
    ),
    size = 3
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    labels = percent_format(accuracy = 1),
    name = "Proporção de US\nafectadas/destruídas"
  ) +
  labs(
    #title = "Proporção de US afectadas/destruídas por distrito e ciclone",
    #subtitle = "Cada célula representa a proporção da rede distrital afectada no evento",
    x = "Ciclone e ano",
    y = "Distrito"
  ) +
  tema_grafico() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grafico4_heatmap


# ============================================================
# 17. GRÁFICO 5: MÉDIA DA PROPORÇÃO POR CATEGORIA
# ============================================================

grafico5_categoria <- resumo_categoria %>%
  ggplot(
    aes(
      x = categoria_ciclone,
      y = media_proporcao_us_afectadas,
      fill = categoria_ciclone
    )
  ) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = percent(media_proporcao_us_afectadas, accuracy = 0.1)),
    vjust = -0.3,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, max(resumo_categoria$media_proporcao_us_afectadas, na.rm = TRUE) * 1.20)
  ) +
  scale_fill_manual(
    values = c(
      "Categoria 1" = "#9ECAE1",
      "Categoria 2" = "#6BAED6",
      "Categoria 3" = "#2171B5",
      "Categoria 4" = "#08306B"
    ),
    drop = FALSE
  ) +
  labs(
    # title = "Média da proporção de US afectadas/destruídas por categoria do ciclone",
    # subtitle = "Tempestades tropicais incorporadas na Categoria 1",
    x = "Categoria do ciclone",
    y = "Média da proporção de US afectadas/destruídas",
    fill = "Categoria"
  ) +
  tema_grafico() +
  theme(
    legend.position = "none"
  )

grafico5_categoria


# ============================================================
# 18. BAIXAR E PREPARAR SHAPEFILE DE SOFALA
# ============================================================

moz_adm2 <- geodata::gadm(
  country = "MOZ",
  level = 2,
  path = "shapefiles_mocambique"
)

moz_adm2_sf <- st_as_sf(moz_adm2)

sofala_shape <- moz_adm2_sf %>%
  filter(NAME_1 == "Sofala") %>%
  mutate(
    distrito_limpo = padronizar_distrito(NAME_2)
  ) %>%
  dplyr::select(NAME_1, NAME_2, distrito_limpo, geometry)


# Verificar distritos que não cruzam
distritos_base_sem_shape <- anti_join(
  resumo_distrito,
  sofala_shape %>% st_drop_geometry(),
  by = "distrito_limpo"
)

distritos_shape_sem_base <- anti_join(
  sofala_shape %>% st_drop_geometry(),
  resumo_distrito,
  by = "distrito_limpo"
)

print(distritos_base_sem_shape)
print(distritos_shape_sem_base)


# ============================================================
# 19. ELEVAÇÃO MÉDIA POR DISTRITO
# ============================================================

elev_moz <- geodata::elevation_30s(
  country = "MOZ",
  path = "dados_elevacao_mocambique"
)

sofala_vect <- terra::vect(sofala_shape)

elev_sofala <- terra::crop(elev_moz, sofala_vect)
elev_sofala <- terra::mask(elev_sofala, sofala_vect)

elev_distrito <- terra::extract(
  elev_sofala,
  sofala_vect,
  fun = mean,
  na.rm = TRUE
)

elev_distrito <- as.data.frame(elev_distrito)

names(elev_distrito)[2] <- "elevacao_media"

elev_distrito <- elev_distrito %>%
  mutate(
    distrito_limpo = sofala_shape$distrito_limpo,
    Distrito = sofala_shape$NAME_2
  ) %>%
  dplyr::select(
    distrito_limpo,
    Distrito,
    elevacao_media
  )


# ============================================================
# 20. BASE FINAL PARA MAPAS E ÍNDICE DE RISCO
# ============================================================
# Indicador principal no risco:
# media_proporcao_us_afectadas
# ============================================================

risco_distrito <- resumo_distrito %>%
  left_join(
    base_sofala %>%
      group_by(distrito_limpo) %>%
      summarise(
        nr_ciclones = n_distinct(
          ciclone_ano[us_afectadas_destruidas > 0]
        ),
        
        peso_categoria_max = ifelse(
          all(is.na(peso_categoria_ciclone[us_afectadas_destruidas > 0])),
          NA_real_,
          max(peso_categoria_ciclone[us_afectadas_destruidas > 0], na.rm = TRUE)
        ),
        
        vento_max_kmh = ifelse(
          all(is.na(vento_kmh[us_afectadas_destruidas > 0])),
          NA_real_,
          max(vento_kmh[us_afectadas_destruidas > 0], na.rm = TRUE)
        ),
        
        .groups = "drop"
      ),
    by = "distrito_limpo"
  ) %>%
  mutate(
    peso_categoria_max = ifelse(is.na(peso_categoria_max), 0, peso_categoria_max),
    vento_max_kmh = ifelse(is.na(vento_max_kmh), 0, vento_max_kmh),
    nr_ciclones = ifelse(is.na(nr_ciclones), 0, nr_ciclones)
  )


mapa_risco <- sofala_shape %>%
  left_join(
    risco_distrito,
    by = "distrito_limpo"
  ) %>%
  left_join(
    elev_distrito,
    by = "distrito_limpo"
  ) %>%
  mutate(
    total_us = ifelse(is.na(total_us), 0, total_us),
    
    nr_us_afectadas_destruidas_acumulado = ifelse(
      is.na(nr_us_afectadas_destruidas_acumulado),
      0,
      nr_us_afectadas_destruidas_acumulado
    ),
    
    media_proporcao_us_afectadas = ifelse(
      is.na(media_proporcao_us_afectadas),
      0,
      media_proporcao_us_afectadas
    ),
    
    percent_medio_us_afectadas = 100 * media_proporcao_us_afectadas,
    
    max_proporcao_us_afectadas = ifelse(
      is.na(max_proporcao_us_afectadas),
      0,
      max_proporcao_us_afectadas
    ),
    
    percent_max_us_afectadas = 100 * max_proporcao_us_afectadas,
    
    proporcao_us_acumulada = ifelse(
      is.na(proporcao_us_acumulada),
      0,
      proporcao_us_acumulada
    ),
    
    percent_us_acumulada = 100 * proporcao_us_acumulada,
    
    nr_ciclones = ifelse(is.na(nr_ciclones), 0, nr_ciclones),
    peso_categoria_max = ifelse(is.na(peso_categoria_max), 0, peso_categoria_max),
    vento_max_kmh = ifelse(is.na(vento_max_kmh), 0, vento_max_kmh)
  )


# ============================================================
# 21. CLASSIFICAR FREQUÊNCIA E ELEVAÇÃO
# ============================================================

mapa_risco <- mapa_risco %>%
  mutate(
    categoria_frequencia = case_when(
      nr_ciclones == 0 ~ "0 ciclones",
      nr_ciclones == 1 ~ "1 ciclone",
      nr_ciclones == 2 ~ "2 ciclones",
      nr_ciclones >= 3 ~ "3 ou mais ciclones"
    ),
    
    categoria_frequencia = factor(
      categoria_frequencia,
      levels = c(
        "0 ciclones",
        "1 ciclone",
        "2 ciclones",
        "3 ou mais ciclones"
      )
    )
  )


cortes_elevacao <- quantile(
  mapa_risco$elevacao_media,
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

mapa_risco <- mapa_risco %>%
  mutate(
    categoria_elevacao = case_when(
      elevacao_media <= cortes_elevacao[1] ~ "Baixa elevação",
      elevacao_media <= cortes_elevacao[2] ~ "Elevação moderada",
      elevacao_media > cortes_elevacao[2] ~ "Alta elevação",
      TRUE ~ NA_character_
    ),
    
    categoria_elevacao = factor(
      categoria_elevacao,
      levels = c(
        "Baixa elevação",
        "Elevação moderada",
        "Alta elevação"
      )
    )
  )


# ============================================================
# 22. ÍNDICE FINAL DE RISCO
# ============================================================
# Componentes:
# 40% média da proporção de US afectadas/destruídas
# 25% frequência de ciclones
# 20% categoria/intensidade do ciclone
# 15% baixa elevação
# ============================================================

mapa_risco <- mapa_risco %>%
  mutate(
    us_normalizado = normalizar_01(media_proporcao_us_afectadas),
    ciclones_normalizado = normalizar_01(nr_ciclones),
    categoria_ciclone_normalizada = peso_categoria_max,
    
    elevacao_normalizada = normalizar_01(elevacao_media),
    elevacao_risco = 1 - elevacao_normalizada,
    
    indice_risco = 0.40 * us_normalizado +
      0.25 * ciclones_normalizado +
      0.20 * categoria_ciclone_normalizada +
      0.15 * elevacao_risco
  )


cortes_risco <- quantile(
  mapa_risco$indice_risco[mapa_risco$indice_risco > 0],
  probs = c(1/3, 2/3),
  na.rm = TRUE
)

mapa_risco <- mapa_risco %>%
  mutate(
    categoria_risco = case_when(
      indice_risco == 0 ~ "Sem risco observado",
      indice_risco <= cortes_risco[1] ~ "Baixo risco",
      indice_risco <= cortes_risco[2] ~ "Risco moderado",
      indice_risco > cortes_risco[2] ~ "Alto risco"
    ),
    
    categoria_risco = factor(
      categoria_risco,
      levels = c(
        "Sem risco observado",
        "Baixo risco",
        "Risco moderado",
        "Alto risco"
      )
    )
  )


# ============================================================
# 23. TABELA FINAL DE RISCO
# ============================================================

tabela_risco_final <- mapa_risco %>%
  st_drop_geometry() %>%
  dplyr::select(
    Distrito = NAME_2,
    distrito_limpo,
    total_us,
    nr_us_afectadas_destruidas_acumulado,
    media_proporcao_us_afectadas,
    percent_medio_us_afectadas,
    max_proporcao_us_afectadas,
    percent_max_us_afectadas,
    proporcao_us_acumulada,
    percent_us_acumulada,
    nr_ciclones,
    categoria_frequencia,
    vento_max_kmh,
    peso_categoria_max,
    elevacao_media,
    categoria_elevacao,
    us_normalizado,
    ciclones_normalizado,
    categoria_ciclone_normalizada,
    elevacao_risco,
    indice_risco,
    categoria_risco
  ) %>%
  arrange(desc(indice_risco))

print(tabela_risco_final)


# ============================================================
# 24. MAPA 1: MÉDIA DA PROPORÇÃO DE US AFECTADAS/DESTRUÍDAS
# ============================================================

mapa1_us <- ggplot(mapa_risco) +
  geom_sf(
    aes(fill = media_proporcao_us_afectadas),
    color = "black",
    linewidth = 0.30
  ) +
  geom_sf_text(
    aes(label = NAME_2),
    size = 2.5
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    labels = percent_format(accuracy = 1),
    name = "US afectadas"
  ) +
  labs(
    # title = "Mapa 1. Média da proporção de US afectadas/destruídas",
    #subtitle = "Indicador médio por evento ciclónico; denominador: total de US do distrito"
    x=NULL, y=NULL) +
  tema_mapa()

mapa1_us


# ============================================================
# 25. MAPA 2: NÚMERO DE CICLONES COM IMPACTO POR DISTRITO
# ============================================================

mapa2_ciclones <- ggplot(mapa_risco) +
  geom_sf(
    aes(fill = nr_ciclones),
    color = "black",
    linewidth = 0.30
  ) +
  geom_sf_text(
    aes(label = NAME_2),
    size = 2.5
  ) +
  scale_fill_gradient(
    low = "#FFF7EC",
    high = "#7F0000",
    name = "Nº de ciclones"
  ) +
  labs(
    ##title = "Mapa 2. Número de ciclones com impacto nas US",
    #subtitle = "Número de ciclones que afectaram/destruíram US por distrito"
    x=NULL, y=NULL) +
  tema_mapa()

mapa2_ciclones


# ============================================================
# 26. MAPA 3: MÉDIA DA PROPORÇÃO POR CATEGORIA DO CICLONE
# ============================================================

base_categoria_ciclone_distrito <- resumo_distrito_ciclone %>%
  filter(!is.na(categoria_ciclone)) %>%
  group_by(
    distrito_limpo,
    categoria_ciclone
  ) %>%
  summarise(
    total_us = first(total_us),
    
    nr_us_afectadas_destruidas_categoria = sum(
      nr_us_afectadas_destruidas,
      na.rm = TRUE
    ),
    
    media_proporcao_us_afectadas_categoria = mean(
      proporcao_us_evento_ajustada,
      na.rm = TRUE
    ),
    
    percent_medio_us_afectadas_categoria =
      100 * media_proporcao_us_afectadas_categoria,
    
    nr_eventos_categoria = n_distinct(
      ciclone_ano[nr_us_afectadas_destruidas > 0]
    ),
    
    .groups = "drop"
  )


categorias_ciclone_ordem <- data.frame(
  categoria_ciclone = factor(
    c(
      "Categoria 1",
      "Categoria 2",
      "Categoria 3",
      "Categoria 4"
    ),
    levels = c(
      "Categoria 1",
      "Categoria 2",
      "Categoria 3",
      "Categoria 4"
    )
  )
)


mapa_categoria_ciclone_facet <- categorias_ciclone_ordem %>%
  tidyr::crossing(
    sofala_shape %>%
      st_drop_geometry() %>%
      dplyr::select(NAME_2, distrito_limpo)
  ) %>%
  left_join(
    base_categoria_ciclone_distrito,
    by = c("distrito_limpo", "categoria_ciclone")
  ) %>%
  mutate(
    media_proporcao_us_afectadas_categoria = ifelse(
      is.na(media_proporcao_us_afectadas_categoria),
      0,
      media_proporcao_us_afectadas_categoria
    ),
    
    percent_medio_us_afectadas_categoria =
      100 * media_proporcao_us_afectadas_categoria,
    
    nr_eventos_categoria = ifelse(
      is.na(nr_eventos_categoria),
      0,
      nr_eventos_categoria
    ),
    
    categoria_ciclone = factor(
      categoria_ciclone,
      levels = c(
        "Categoria 1",
        "Categoria 2",
        "Categoria 3",
        "Categoria 4"
      )
    )
  ) %>%
  left_join(
    sofala_shape %>%
      dplyr::select(distrito_limpo, geometry),
    by = "distrito_limpo"
  ) %>%
  st_as_sf()


mapa3_cat_ciclone <- ggplot(mapa_categoria_ciclone_facet) +
  geom_sf(
    aes(fill = media_proporcao_us_afectadas_categoria),
    color = "black",
    linewidth = 0.25
  ) +
  geom_sf_text(
    aes(
      label = ifelse(
        media_proporcao_us_afectadas_categoria > 0,
        percent(media_proporcao_us_afectadas_categoria, accuracy = 0.1),
        ""
      )
    ),
    size = 2.5,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    labels = percent_format(accuracy = 1),
    name = "US afectadas"
  ) +
  facet_wrap(
    ~ categoria_ciclone,
    ncol = 4,
    drop = FALSE
  ) +
  labs(
    #title = "Mapa 3. Média da proporção de US afectadas/destruídas por categoria do ciclone",
    #subtitle = "Tempestades tropicais incorporadas na Categoria 1",
    #caption = "O preenchimento representa a média da proporção da rede distrital afectada por categoria."
    x=NULL, y=NULL) +
  tema_mapa()

mapa3_cat_ciclone


# ============================================================
# 27. MAPA 4: CLASSIFICAÇÃO FINAL DE RISCO
# ============================================================

mapa4_risco_final <- ggplot(mapa_risco) +
  geom_sf(
    aes(fill = categoria_risco),
    color = "black",
    linewidth = 0.30
  ) +
  geom_sf_text(
    aes(label = NAME_2),
    size = 2.5
  ) +
  scale_fill_manual(
    values = c(
      "Sem risco observado" = "grey90",
      "Baixo risco" = "#FFFFB2",
      "Risco moderado" = "#FECC5C",
      "Alto risco" = "#E31A1C"
    ),
    name = "Risco final",
    drop = FALSE
  ) +
  labs(
    #title = "Mapa 4. Classificação final de risco de afectação/destruição de US",
    #subtitle = "Índice baseado na média da proporção de US afectadas, frequência, categoria do ciclone e baixa elevação"
    x=NULL, y=NULL) +
  tema_mapa()

mapa4_risco_final


# ============================================================
# 28. MAPA 5: ELEVAÇÃO MÉDIA POR DISTRITO
# ============================================================

mapa5_elevacao <- ggplot(mapa_risco) +
  geom_sf(
    aes(fill = elevacao_media),
    color = "black",
    linewidth = 0.30
  ) +
  geom_sf_text(
    aes(label = NAME_2),
    size = 2.5
  ) +
  scale_fill_gradient(
    low = "#D9F0A3",
    high = "#004529",
    name = "Elevação média\n(m)"
  ) +
  labs(
    #title = "Mapa 5. Elevação média por distrito",
    #subtitle = "Distritos de baixa elevação podem ter maior vulnerabilidade a inundações associadas a ciclones"
    x=NULL, y=NULL ) +
  tema_mapa()

mapa5_elevacao


# ============================================================
# 29. MAPA 6: FACET POR CICLONE COM PROPORÇÃO POR EVENTO
# ============================================================

lista_ciclones <- resumo_distrito_ciclone %>%
  distinct(ciclone, ano, ciclone_ano) %>%
  arrange(ano)

lista_distritos <- sofala_shape %>%
  st_drop_geometry() %>%
  distinct(NAME_2, distrito_limpo)

base_completa_distrito_ciclone <- lista_distritos %>%
  crossing(lista_ciclones) %>%
  left_join(
    resumo_distrito_ciclone %>%
      dplyr::select(
        distrito_limpo,
        ciclone,
        ano,
        ciclone_ano,
        nr_us_afectadas_destruidas,
        total_us,
        proporcao_us_evento_ajustada,
        percent_us_evento_ajustada
      ),
    by = c("distrito_limpo", "ciclone", "ano", "ciclone_ano")
  ) %>%
  mutate(
    nr_us_afectadas_destruidas = ifelse(
      is.na(nr_us_afectadas_destruidas),
      0,
      nr_us_afectadas_destruidas
    ),
    
    total_us = ifelse(is.na(total_us), 0, total_us),
    
    proporcao_us_evento_ajustada = ifelse(
      is.na(proporcao_us_evento_ajustada),
      0,
      proporcao_us_evento_ajustada
    ),
    
    percent_us_evento_ajustada = 100 * proporcao_us_evento_ajustada
  )

mapa_us_distrito_ciclone <- sofala_shape %>%
  dplyr::select(NAME_2, distrito_limpo, geometry) %>%
  left_join(
    base_completa_distrito_ciclone,
    by = c("NAME_2", "distrito_limpo")
  ) %>%
  mutate(
    ciclone_ano = factor(
      ciclone_ano,
      levels = lista_ciclones$ciclone_ano
    )
  )


mapa6_ciclone_facet <- ggplot(mapa_us_distrito_ciclone) +
  geom_sf(
    aes(fill = proporcao_us_evento_ajustada),
    color = "black",
    linewidth = 0.25
  ) +
  scale_fill_gradient(
    low = "#F7FBFF",
    high = "#08306B",
    labels = percent_format(accuracy = 1),
    name = "Proporção de US\nafectadas"
  ) +
  facet_wrap(~ ciclone_ano) +
  labs(
    title = "Mapa 6. Proporção de US afectadas/destruídas por distrito e ciclone",
    subtitle = "Cada mapa mostra a proporção da rede distrital afectada no evento",
    caption = "Fonte: base de ciclones consolidada; limites administrativos: GADM via geodata"
  ) +
  tema_mapa()

mapa6_ciclone_facet


# ============================================================
# 30. PAINEL COM GRÁFICOS PRINCIPAIS
# ============================================================

painel_graficos <- (grafico1_ciclone / grafico2_distrito) |
  (grafico3_pareto / grafico4_heatmap)

painel_graficos <- painel_graficos +
  plot_annotation(
    title = "Média da proporção de Unidades Sanitárias afectadas/destruídas por ciclones em Sofala",
    subtitle = "Resumo por ciclone, distrito, concentração proporcional média e matriz distrito-evento",
    caption = "Fonte: base de ciclones consolidada"
  )

painel_graficos


# ============================================================
# 31. PAINEL COM MAPAS PRINCIPAIS
# ============================================================

painel_mapas <- (mapa1_us + mapa2_ciclones) /
  (mapa3_cat_ciclone + mapa4_risco_final) +
  plot_annotation(
    title = "Risco de afectação/destruição de Unidades Sanitárias por ciclones em Sofala",
    subtitle = "Indicador principal: média da proporção de US afectadas/destruídas por evento ciclónico",
    caption = "Fonte: base de ciclones consolidada; limites administrativos e elevação: geodata"
  )

painel_mapas


# ============================================================
# 32. EXPORTAR TABELAS
# ============================================================

write_xlsx(
  list(
    "Total US por distrito" = us_sofala_distrito,
    "Categoria ciclones" = categoria_ciclones,
    "Resumo distrito ciclone" = resumo_distrito_ciclone,
    "Resumo por distrito" = resumo_distrito,
    "Resumo por ciclone" = resumo_ciclone,
    "Resumo por categoria" = resumo_categoria,
    "Tabela risco final" = tabela_risco_final
  ),
  "tabelas_resumo_media_proporcao_us_sofala.xlsx"
)

write.csv(
  tabela_risco_final,
  "tabela_risco_final_media_proporcao_us_sofala.csv",
  row.names = FALSE
)


# ============================================================
# 33. EXPORTAR GRÁFICOS
# ============================================================

ggsave(
  filename = "grafico1_media_proporcao_us_por_ciclone_sofala.png",
  plot = grafico1_ciclone,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "grafico2_media_proporcao_us_por_distrito_sofala.png",
  plot = grafico2_distrito,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "grafico3_pareto_media_proporcao_us_sofala.png",
  plot = grafico3_pareto,
  width = 12,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "grafico4_heatmap_proporcao_evento_distrito_ciclone_sofala.png",
  plot = grafico4_heatmap,
  width = 13,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "grafico5_media_proporcao_us_por_categoria_ciclone_sofala.png",
  plot = grafico5_categoria,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "painel_graficos_media_proporcao_us_sofala.png",
  plot = painel_graficos,
  width = 16,
  height = 12,
  dpi = 300
)


# ============================================================
# 34. EXPORTAR MAPAS
# ============================================================

ggsave(
  filename = "mapa1_media_proporcao_us_afectadas_destruidas_sofala.png",
  plot = mapa1_us,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "mapa2_nr_ciclones_sofala.png",
  plot = mapa2_ciclones,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "mapa3_media_proporcao_us_por_categoria_ciclone_sofala.png",
  plot = mapa3_cat_ciclone,
  width = 14,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "mapa4_risco_final_media_proporcao_us_sofala.png",
  plot = mapa4_risco_final,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "mapa5_elevacao_media_sofala.png",
  plot = mapa5_elevacao,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "mapa6_proporcao_us_por_distrito_e_ciclone_sofala.png",
  plot = mapa6_ciclone_facet,
  width = 16,
  height = 10,
  dpi = 300
)

ggsave(
  filename = "painel_mapas_risco_media_proporcao_us_sofala.png",
  plot = painel_mapas,
  width = 16,
  height = 11,
  dpi = 300
)


# ============================================================
# 35. VERIFICAÇÃO ESPECÍFICA: DONDO E IDAI
# ============================================================

verificar_dondo_idai <- resumo_distrito_ciclone %>%
  filter(
    distrito_limpo == "dondo",
    ano == 2019,
    str_detect(str_to_lower(ciclone), "idai")
  ) %>%
  dplyr::select(
    ano,
    ciclone,
    ciclone_ano,
    distrito_limpo,
    total_us,
    nr_us_afectadas_destruidas,
    proporcao_us_evento,
    percent_us_evento,
    proporcao_us_evento_ajustada,
    percent_us_evento_ajustada,
    categoria_ciclone,
    vento_kmh
  )

print(verificar_dondo_idai)


# ============================================================
# 36. VERIFICAÇÃO FINAL
# ============================================================

summary(resumo_distrito$nr_us_afectadas_destruidas_acumulado)
summary(resumo_distrito$media_proporcao_us_afectadas)
summary(resumo_distrito$percent_medio_us_afectadas)
summary(resumo_distrito$proporcao_us_acumulada)
summary(resumo_distrito$percent_us_acumulada)

print(tabela_risco_final)