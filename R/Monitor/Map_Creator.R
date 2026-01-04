
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

#moz_shape <- st_read("shape/marracuene/Marracuene.shp")

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

# Mapa com raio de 1 km ao redor da CS Zimpeto
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = moz_shape,
    fillColor = "lightblue", weight = 1, color = "black",
    fillOpacity = 0.3, label = ~as.character(NOME) # ajustar ao nome real do campo
  ) %>%
  addMarkers(lng = longitude, lat = latitude, popup = "CS Zimpeto") %>%
  addCircles(lng = longitude, lat = latitude, radius = 15008, color = "red", weight = 2, fillOpacity = 0.1, label = "Raio 15.8 km") %>%
  setView(lng = longitude, lat = latitude, zoom = 14)




###########################

# Lista de bairros a destacar
bairros_interesse <- c("Zimpeto", "Magoanine C", "Mavalane B", "Hulene B")

# Verifique o nome correto da coluna com os nomes dos bairros:
names(moz_shape) # talvez seja "NOME_BAIR", "Bairro", "NOME", etc.

# Criar coluna de destaque
moz_shape <- moz_shape %>%
  mutate(destaque = ifelse(NOME %in% bairros_interesse, "selecionado", "outros"))

# Paleta de cores: verde para selecionados, cinza para outros
pal <- colorFactor(
  palette = c("lightgray", "darkgreen"),
  domain = c("outros", "selecionado")
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

# Garantir que CRS é o mesmo
if (st_crs(moz_shape) != st_crs(ponto)) {
  moz_shape <- st_transform(moz_shape, st_crs(ponto))
}

# Mapa com bairros pintados conforme a lista
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = moz_shape,
    fillColor = ~pal(destaque),
    weight = 1,
    color = "black",
    fillOpacity = 0.6,
    label = ~NOME_BAIR
  ) %>%
  addMarkers(lng = longitude, lat = latitude, popup = "CS Zimpeto") %>%
  addCircles(lng = longitude, lat = latitude, radius = 1000, color = "red", weight = 2, fillOpacity = 0.1, label = "Raio 1 km") %>%
  setView(lng = longitude, lat = latitude, zoom = 14)
