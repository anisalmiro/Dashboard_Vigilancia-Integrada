
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
library(htmltools)

# ============================================================================
# SCRIPT PARA VISUALIZAÇÃO DE CASOS POSITIVOS DE SARS-COV-2
# Filtro: modulo_testagem = IRAS
# Baseado na representação gráfica de casos por bairro
# ============================================================================

# Carregar o shapefile dos bairros
cat("Carregando shapefile dos bairros...\n")
moz_shape <- st_read("shape/maputo/Bairros_Maputo.shp")

# Carregar dados de casos COVID-19
cat("Carregando dados de casos COVID-19...\n")
# Assumindo que BD_comulativo_casos é um arquivo Excel
# Ajuste o caminho e formato conforme necessário
casos_vigilancia <- read.csv("R/Dashboard/BD_Vig_Hosp&Com_Final.csv") 
# Alternativa para CSV: casos_covid <- read.csv("BD_comulativo_casos.csv")
resultado_teste<-'Base_Resultados.group_jz9ln80.SARSCov2'
# Filtrar casos positivos de SARS-CoV-2 onde modulo_testagem = IRAS
cat("Filtrando casos positivos SARS-CoV-2 com modulo_testagem = IRAS...\n")
casos_positivos <- casos_vigilancia %>%
  filter(
    casos_vigilancia$Base_Resultados.detalhes.modulo== "IRAS",
    # Filtrar casos positivos - ajuste os nomes das colunas conforme sua base
    (casos_vigilancia$Base_Resultados.group_jz9ln80.SARSCov2 == "Positivo" | 
     str_detect(tolower(casos_vigilancia$Base_Resultados.group_jz9ln80.SARSCov2), "positivo"))
  ) %>%
  clean_names()

# Agregar casos por bairro
cat("Agregando casos por bairro...\n")
casos_por_bairro <- casos_positivos %>%
  group_by('bairro'=casos_positivos$dados_demograficos_bairro) %>% # Ajuste o nome da coluna conforme sua base
  summarise(
    total_casos = n(),
    .groups = 'drop'
  ) %>%
  # Criar categorias para visualização (baseado na imagem de referência)
  mutate(
    categoria_casos = case_when(
      total_casos > 2 ~ ">2",
      total_casos >= 1 ~ "1-2",
      TRUE ~ "0"
    ),
    # Definir tamanhos dos círculos
    tamanho_circulo = case_when(
      total_casos > 2 ~ 15,   # Círculos grandes
      total_casos >= 1 ~ 8,   # Círculos pequenos
      TRUE ~ 0
    )
  )

# Coordenadas da CS Zimpeto
latitude <- -25.83118
longitude <- 32.57754

# Transformar shapefile para WGS84 se necessário
if (st_crs(moz_shape)$input != "EPSG:4326") {
  moz_shape <- st_transform(moz_shape, 4326)
}

# Juntar dados de casos com shapefile
moz_shape_casos <- moz_shape %>%
  left_join(casos_por_bairro, by = c("NOME" = "bairro")) %>%
  mutate(
    total_casos = ifelse(is.na(total_casos), 0, total_casos),
    categoria_casos = ifelse(is.na(categoria_casos), "0", categoria_casos),
    tamanho_circulo = ifelse(is.na(tamanho_circulo), 0, tamanho_circulo)
  )

# Calcular centroides dos bairros
centroides <- st_centroid(moz_shape_casos)
coords_centroides <- st_coordinates(centroides)

# Adicionar coordenadas dos centroides
moz_shape_casos$centroid_lng <- coords_centroides[,1]
moz_shape_casos$centroid_lat <- coords_centroides[,2]

# Filtrar bairros com casos para visualização
bairros_com_casos <- moz_shape_casos %>%
  filter(total_casos > 0) %>%
  st_drop_geometry()

# Criar HTML para legenda personalizada
legenda_html <- '
<div style="position: fixed; 
            top: 10px; right: 10px; width: 200px; height: auto; 
            background-color: white; border: 2px solid grey; z-index:1000; 
            font-size:12px; padding: 10px">
<p><strong>Legenda</strong></p>
<p><span style="color:blue;">🏥</span> CS Zimpeto</p>
<p><span style="color:red;">⭕</span> Área de Captação da US (15 Km)</p>
<p><strong>Positivos para SARS-CoV-2 (geral)</strong></p>
<p><span style="color:red;">●</span> 1 - 2</p>
<p><span style="color:red; font-size:16px;">●</span> > 2</p>
</div>
'

# Criar mapa principal
cat("Criando mapa de visualização...\n")
mapa_covid <- leaflet() %>%
  addTiles() %>%
  # Adicionar polígonos dos bairros
  addPolygons(
    data = moz_shape,
    fillColor = "lightgray", 
    weight = 1, 
    color = "black",
    fillOpacity = 0.2, 
    label = ~as.character(NOME),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    )
  ) %>%
  # Adicionar marcador da CS Zimpeto
  addMarkers(
    lng = longitude, 
    lat = latitude, 
    popup = "<b>CS Zimpeto</b><br>Centro de Saúde",
    label = "CS Zimpeto"
  ) %>%
  # Adicionar círculo de área de captação (15 km)
  addCircles(
    lng = longitude, 
    lat = latitude, 
    radius = 12000, 
    color = "red", 
    weight = 2, 
    fillOpacity = 0.05,
    fillColor = "red",
    label = "Área de Captação da US (15 Km)",
    labelOptions = labelOptions(
      style = list("font-weight" = "bold"),
      textsize = "14px"
    )
  )

# Adicionar círculos para casos positivos
if (nrow(bairros_com_casos) > 0) {
  # Casos categoria 1-2
  casos_pequenos <- bairros_com_casos %>% filter(categoria_casos == "1-2")
  if (nrow(casos_pequenos) > 0) {
    mapa_covid <- mapa_covid %>%
      addCircleMarkers(
        data = casos_pequenos,
        lng = ~centroid_lng,
        lat = ~centroid_lat,
        radius = 8,
        color = "darkred",
        fillColor = "red",
        weight = 2,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Bairro:</b> ", NOME, "<br>",
          "<b>Casos Positivos SARS-CoV-2:</b> ", total_casos, "<br>",
          "<b>Categoria:</b> 1-2 casos"
        ),
        label = ~paste0(NOME, ": ", total_casos, " caso(s)")
      )
  }
  
  # Casos categoria >2
  casos_grandes <- bairros_com_casos %>% filter(categoria_casos == ">2")
  if (nrow(casos_grandes) > 0) {
    mapa_covid <- mapa_covid %>%
      addCircleMarkers(
        data = casos_grandes,
        lng = ~centroid_lng,
        lat = ~centroid_lat,
        radius = 5,
        color = "darkred",
        fillColor = "red",
        weight = 2,
        fillOpacity = 0.8,
        popup = ~paste0(
          "<b>Bairro:</b> ", NOME, "<br>",
          "<b>Casos Positivos SARS-CoV-2:</b> ", total_casos, "<br>",
          "<b>Categoria:</b> >2 casos"
        ),
        label = ~paste0(NOME, ": ", total_casos, " casos")
      )
  }
}

# Adicionar legenda HTML personalizada
mapa_Sarscov2 <- mapa_covid %>%
  addControl(html = legenda_html, position = "topright") %>%
  setView(lng = longitude, lat = latitude, zoom = 11)

# Mostrar mapa
print(mapa_Sarscov2)

# ============================================================================
# RELATÓRIO DE RESULTADOS
# ============================================================================

cat("\n" %R% rep("=", 60) %R% "\n")
cat("RELATÓRIO - CASOS POSITIVOS DE SARS-COV-2\n")
cat("Filtro aplicado: modulo_testagem = IRAS\n")
cat(rep("=", 60) %R% "\n")

cat("Total de casos positivos encontrados:", nrow(casos_positivos), "\n")
cat("Número de bairros com casos:", nrow(bairros_com_casos), "\n")
cat("Número de bairros sem casos:", nrow(moz_shape) - nrow(bairros_com_casos), "\n\n")

if (nrow(casos_por_bairro) > 0) {
  cat("DISTRIBUIÇÃO POR BAIRRO:\n")
  cat(rep("-", 40) %R% "\n")
  
  casos_ordenados <- casos_por_bairro %>% 
    arrange(desc(total_casos))
  
  for (i in 1:nrow(casos_ordenados)) {
    cat(sprintf("%-20s: %2d casos (%s)\n", 
                casos_ordenados$bairro[i], 
                casos_ordenados$total_casos[i],
                casos_ordenados$categoria_casos[i]))
  }
  
  cat("\nRESUMO POR CATEGORIA:\n")
  cat(rep("-", 30) %R% "\n")
  resumo_categoria <- casos_por_bairro %>%
    group_by(categoria_casos) %>%
    summarise(
      bairros = n(),
      total_casos = sum(total_casos),
      .groups = 'drop'
    )
  
  print(resumo_categoria)
}

# Salvar dados processados
cat("\nSalvando dados processados...\n")
write.xlsx(list(
  "Casos_por_Bairro" = casos_por_bairro,
  "Resumo_Geral" = data.frame(
    Indicador = c("Total Casos", "Bairros com Casos", "Bairros sem Casos"),
    Valor = c(nrow(casos_positivos), nrow(bairros_com_casos), 
              nrow(moz_shape) - nrow(bairros_com_casos))
  )
), "relatorio_casos_covid_IRAS.xlsx")

cat("Dados salvos em: relatorio_casos_covid_IRAS.xlsx\n")
cat("Mapa interativo criado com sucesso!\n")

# ============================================================================
# INSTRUÇÕES DE USO
# ============================================================================
cat("\n" %R% rep("=", 60) %R% "\n")
cat("INSTRUÇÕES DE USO:\n")
cat(rep("=", 60) %R% "\n")
cat("1. Certifique-se de que o arquivo 'BD_comulativo_casos.xlsx' está no diretório de trabalho\n")
cat("2. Ajuste os nomes das colunas nas linhas 25-30 conforme sua base de dados\n")
cat("3. Verifique se o shapefile está no caminho correto: 'shape/maputo/Bairros_Maputo.shp'\n")
cat("4. Execute o script completo para gerar o mapa e o relatório\n")
cat("5. O mapa será exibido no viewer do RStudio ou navegador\n")
cat("6. Os dados processados serão salvos em 'relatorio_casos_covid_IRAS.xlsx'\n")

