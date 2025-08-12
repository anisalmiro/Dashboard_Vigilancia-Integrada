library(tidyverse)
library(janitor)
library(readxl)
library(sp)
library(openxlsx)
library(ggrepel)
library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)

# Carregar o shapefile dos bairros
moz_shape <- st_read("shape/maputo/Bairros_Maputo.shp")

# Carregar dados de casos COVID-19
# Assumindo que BD_comulativo_casos é um arquivo Excel ou CSV
# Ajuste o caminho conforme necessário
casos_covid <- read_excel("BD_comulativo_casos.xlsx") # ou read.csv("BD_comulativo_casos.csv")

# Filtrar casos positivos de SARS-CoV-2 onde modulo_testagem = IRAS
casos_positivos <- casos_covid %>%
  filter(
    modulo_testagem == "IRAS",
    # Assumindo que existe uma coluna para resultado do teste
    # Ajuste o nome da coluna conforme sua base de dados
    resultado_teste == "Positivo" | 
    str_detect(tolower(resultado_teste), "positiv") |
    # Ou se houver coluna específica para SARS-CoV-2
    sars_cov2 == "Positivo" |
    str_detect(tolower(sars_cov2), "positiv")
  ) %>%
  # Limpar nomes das colunas
  clean_names()

# Agregar casos por bairro
# Assumindo que existe uma coluna com nome do bairro
# Ajuste o nome da coluna conforme sua base de dados
casos_por_bairro <- casos_positivos %>%
  group_by(bairro) %>% # ou nome_bairro, localidade, etc.
  summarise(
    total_casos = n(),
    .groups = 'drop'
  ) %>%
  # Criar categorias para tamanhos dos círculos
  mutate(
    categoria_casos = case_when(
      total_casos >= 3 ~ ">2",
      total_casos >= 1 ~ "1-2",
      TRUE ~ "0"
    )
  )

# Coordenadas da CS Zimpeto
latitude <- -25.83118
longitude <- 32.57754

# Criar ponto como sf
ponto <- st_as_sf(
  data.frame(longitude = longitude, latitude = latitude),
  coords = c("longitude", "latitude"),
  crs = 4326
)

# Transformar shapefile para o mesmo CRS do ponto (WGS84), se necessário
if (st_crs(moz_shape) != st_crs(ponto)) {
  moz_shape <- st_transform(moz_shape, st_crs(ponto))
}

# Juntar dados de casos com shapefile
# Assumindo que o campo de nome no shapefile é NOME ou NOME_BAIR
moz_shape_casos <- moz_shape %>%
  left_join(casos_por_bairro, by = c("NOME" = "bairro")) %>% # Ajuste os nomes das colunas
  # Substituir NA por 0
  mutate(
    total_casos = ifelse(is.na(total_casos), 0, total_casos),
    categoria_casos = ifelse(is.na(categoria_casos), "0", categoria_casos)
  )

# Calcular centroides dos bairros para posicionar os círculos
centroides <- st_centroid(moz_shape_casos)
coords_centroides <- st_coordinates(centroides)

# Adicionar coordenadas ao dataframe
moz_shape_casos$centroid_lng <- coords_centroides[,1]
moz_shape_casos$centroid_lat <- coords_centroides[,2]

# Filtrar apenas bairros com casos para mostrar círculos
bairros_com_casos <- moz_shape_casos %>%
  filter(total_casos > 0) %>%
  st_drop_geometry()

# Definir tamanhos dos círculos baseado no número de casos
tamanhos_circulos <- function(casos) {
  case_when(
    casos >= 3 ~ 200,  # Círculos maiores para >2 casos
    casos >= 1 ~ 100,  # Círculos menores para 1-2 casos
    TRUE ~ 0
  )
}

# Criar mapa com casos de SARS-CoV-2
mapa_covid <- leaflet() %>%
  addTiles() %>%
  # Adicionar polígonos dos bairros
  addPolygons(
    data = moz_shape,
    fillColor = "lightblue", 
    weight = 1, 
    color = "black",
    fillOpacity = 0.3, 
    label = ~as.character(NOME)
  ) %>%
  # Adicionar marcador da CS Zimpeto
  addMarkers(
    lng = longitude, 
    lat = latitude, 
    popup = "CS Zimpeto"
  ) %>%
  # Adicionar círculo de área de captação (15 km)
  addCircles(
    lng = longitude, 
    lat = latitude, 
    radius = 15000, 
    color = "red", 
    weight = 2, 
    fillOpacity = 0.1, 
    label = "Área de Captação da US (15 Km)"
  )

# Adicionar círculos para casos positivos
if (nrow(bairros_com_casos) > 0) {
  mapa_covid <- mapa_covid %>%
    addCircleMarkers(
      data = bairros_com_casos,
      lng = ~centroid_lng,
      lat = ~centroid_lat,
      radius = ~tamanhos_circulos(total_casos) / 20, # Ajustar escala
      color = "red",
      fillColor = "red",
      weight = 2,
      fillOpacity = 0.7,
      popup = ~paste0(
        "<b>Bairro:</b> ", NOME, "<br>",
        "<b>Casos Positivos SARS-CoV-2:</b> ", total_casos
      ),
      label = ~paste0(NOME, ": ", total_casos, " casos")
    )
}

# Finalizar mapa
mapa_covid <- mapa_covid %>%
  setView(lng = longitude, lat = latitude, zoom = 12)

# Mostrar mapa
print(mapa_covid)

# Criar resumo dos dados
cat("=== RESUMO DOS CASOS POSITIVOS DE SARS-COV-2 ===\n")
cat("Filtro aplicado: modulo_testagem = IRAS\n")
cat("Total de casos positivos:", nrow(casos_positivos), "\n")
cat("Bairros com casos:", nrow(bairros_com_casos), "\n\n")

if (nrow(casos_por_bairro) > 0) {
  cat("Distribuição por bairro:\n")
  print(casos_por_bairro %>% arrange(desc(total_casos)))
}

# Salvar dados processados (opcional)
write.xlsx(casos_por_bairro, "casos_covid_por_bairro.xlsx")
cat("\nDados salvos em: casos_covid_por_bairro.xlsx\n")

