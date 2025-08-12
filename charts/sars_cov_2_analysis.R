
# Carregar pacotes necessários
if (!require(sf)) install.packages('sf')
if (!require(dplyr)) install.packages('dplyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(rgeos)) install.packages('rgeos')

library(sf)
library(dplyr)
library(ggplot2)
library(rgeos)

# 1. Simular dados geográficos dos bairros (polígonos)
# Para simplificar, vamos criar alguns polígonos aleatórios que representam bairros.
# Em um cenário real, você carregaria um shapefile ou GeoJSON.

# Criar pontos para formar polígonos
set.seed(123)
bairros_coords <- list(
  list(x = c(0, 0, 2, 2, 0), y = c(0, 2, 2, 0, 0), name = 'Bairro A'),
  list(x = c(1, 1, 3, 3, 1), y = c(1, 3, 3, 1, 1), name = 'Bairro B'),
  list(x = c(3, 3, 5, 5, 3), y = c(0, 2, 2, 0, 0), name = 'Bairro C'),
  list(x = c(0, 0, 1, 1, 0), y = c(3, 4, 4, 3, 3), name = 'Bairro D'),
  list(x = c(2, 2, 4, 4, 2), y = c(3, 5, 5, 3, 3), name = 'Bairro E')
)

bairros_sf <- lapply(bairros_coords, function(b) {
  polygon <- st_polygon(list(cbind(b$x, b$y)))
  st_sf(name = b$name, geometry = polygon)
})

bairros_sf <- do.call(rbind, bairros_sf)

# 2. Simular dados de casos de SARS-CoV-2
# Pontos aleatórios dentro dos limites dos bairros simulados
casos_sars_cov_2 <- data.frame(
  id = 1:5,
  x = c(0.5, 1.5, 3.5, 0.8, 2.5),
  y = c(0.5, 2.5, 1.0, 3.5, 4.0),
  status = sample(c('Positivo', 'Testado'), 5, replace = TRUE),
  bairro = c('Bairro A', 'Bairro B', 'Bairro C', 'Bairro D', 'Bairro E')
)

casos_sf <- st_as_sf(casos_sars_cov_2, coords = c('x', 'y'), crs = st_crs(bairros_sf))

# 3. Simular dados de unidades de saúde e população por bairro
# Para a unidade de saúde CS Zimpeto, vamos definir uma localização.
# A população e a percentagem que frequenta a US serão simuladas.

us_zimpeto <- st_point(c(2.5, 1.5)) # Localização simulada da US Zimpeto
us_zimpeto_sf <- st_sf(name = 'CS Zimpeto', geometry = us_zimpeto)

# Dados simulados de população e frequência da US por bairro
populacao_us <- data.frame(
  bairro = c('Bairro A', 'Bairro B', 'Bairro C', 'Bairro D', 'Bairro E'),
  populacao_total = c(1000, 1500, 800, 1200, 900),
  populacao_us = c(850, 1000, 500, 950, 700) # População que frequenta a US
)

populacao_us <- populacao_us %>%
  mutate(perc_us = (populacao_us / populacao_total) * 100)

# 4. Implementar a lógica para definir as áreas com 80% da população que vai à unidade sanitária
# Vamos considerar um buffer ao redor da US como área de captação e selecionar bairros.

# Área de captação da US (15 Km - simulado como um raio de 2 unidades para este exemplo)
area_captacao_us <- st_buffer(us_zimpeto_sf, dist = 2)

# Identificar bairros dentro da área de captação e com >80% de população frequentando a US
bairros_selecionados <- bairros_sf %>%
  st_join(area_captacao_us, join = st_intersects) %>%
  filter(!is.na(name.y)) %>% # Bairros que intersectam a área de captação
  left_join(populacao_us, by = c('name.x' = 'bairro')) %>%
  filter(perc_us >= 80) # Bairros com 80% ou mais da população frequentando a US

# Renomear a coluna do nome do bairro para clareza
names(bairros_selecionados)[names(bairros_selecionados) == 'name.x'] <- 'bairro'

# 5. Realizar a análise de sobreposição para identificar casos dentro das áreas definidas
casos_dentro_area <- casos_sf %>%
  st_join(bairros_selecionados, join = st_intersects) %>%
  filter(!is.na(bairro)) # Casos que estão dentro dos bairros selecionados

# Adicionar uma coluna para indicar se o caso está na área selecionada
casos_sf$na_area_selecionada <- casos_sf$id %in% casos_dentro_area$id

# Preparar dados para visualização no Power BI (exemplo: um dataframe simples)
# Power BI pode consumir dataframes diretamente de scripts R.

# Exemplo de dataframe de saída para Power BI
output_data <- data.frame(
  bairro = bairros_sf$name,
  is_selected_area = bairros_sf$name %in% bairros_selecionados$bairro,
  pop_total = populacao_us$populacao_total[match(bairros_sf$name, populacao_us$bairro)],
  pop_us = populacao_us$populacao_us[match(bairros_sf$name, populacao_us$bairro)],
  perc_us = populacao_us$perc_us[match(bairros_sf$name, populacao_us$bairro)]
)

# Adicionar informações dos casos ao output_data, se necessário
# Para visualização no Power BI, pode ser melhor ter um dataframe separado para os casos
casos_output_data <- casos_sf %>%
  as.data.frame() %>%
  select(id, x, y, status, bairro, na_area_selecionada)

# Para o Power BI, você pode retornar múltiplos dataframes ou um único dataframe consolidado.
# Por exemplo, para um visual de mapa, você pode retornar os dados geoespaciais e os pontos dos casos.

# Para simplificar a integração com Power BI, vamos criar um dataframe final com tudo.
# Em um cenário real, você pode querer exportar shapefiles ou GeoJSONs e importá-los no Power BI
# usando visuais personalizados ou o recurso de mapa do Power BI.

# Exemplo de saída para Power BI: um dataframe com os bairros e seus atributos
# e outro com os casos.

# Para Power BI, é comum que o script R retorne um dataframe que será usado como tabela.
# Vamos criar um dataframe que combine as informações necessárias para o mapa.

# Converter os objetos sf para dataframes para Power BI (perde a geometria, mas pode ser recriada no Power BI com visuais de mapa)
# Ou, o Power BI pode usar o visual R para renderizar o gráfico diretamente.

# Para o visual R no Power BI, o script R deve gerar um gráfico.
# Vamos gerar um gráfico usando ggplot2 e ele será exibido no Power BI.

# Visualização (para ser exibida no Power BI via Visual R)
map_plot <- ggplot() +
  geom_sf(data = bairros_sf, aes(fill = name %in% bairros_selecionados$bairro), color = 'black', alpha = 0.6) +
  geom_sf(data = us_zimpeto_sf, color = 'blue', size = 4, shape = 8) + # US Zimpeto
  geom_sf(data = area_captacao_us, fill = NA, color = 'red', linetype = 'dashed', size = 1) + # Área de captação
  geom_sf(data = casos_sf, aes(color = status, size = na_area_selecionada), show.legend = 'point') +
  scale_fill_manual(values = c('FALSE' = 'lightgray', 'TRUE' = 'lightblue'), labels = c('Outros Bairros', 'Bairros Selecionados')) +
  scale_color_manual(values = c('Positivo' = 'red', 'Testado' = 'orange')) +
  scale_size_manual(values = c('TRUE' = 3, 'FALSE' = 1), labels = c('Na Área Selecionada', 'Fora da Área Selecionada')) +
  labs(
    title = 'Casos de SARS-CoV-2 e Áreas de Captação da US',
    fill = 'Área de Interesse',
    color = 'Status do Caso',
    size = 'Localização do Caso'
  ) +
  theme_minimal()

print(map_plot)

# Se o Power BI precisar de um dataframe de saída, você pode retornar um.
# Por exemplo, um dataframe com os casos e se eles estão na área selecionada.
final_cases_data <- casos_sf %>%
  as.data.frame() %>%
  select(id, status, bairro, na_area_selecionada, geometry)

# Para Power BI, você pode precisar de coordenadas para os pontos se não usar um visual R.
final_cases_data$longitude <- st_coordinates(final_cases_data$geometry)[,1]
final_cases_data$latitude <- st_coordinates(final_cases_data$geometry)[,2]
final_cases_data <- select(final_cases_data, -geometry)

# O Power BI pode usar este dataframe para criar um mapa de pontos.
# print(final_cases_data) # Descomente para ver o dataframe de saída

# Para integrar no Power BI, você pode usar o Visual R para exibir o 'map_plot'
# ou carregar 'final_cases_data' como uma tabela e usar os visuais de mapa nativos do Power BI.

# Salvar o plot como imagem (opcional, para depuração ou uso externo)
# ggsave("sars_cov_2_map.png", plot = map_plot, width = 10, height = 8, dpi = 300)

# Salvar o dataframe de casos para Power BI (opcional, para depuração ou uso externo)
# write.csv(final_cases_data, "sars_cov_2_cases_for_powerbi.csv", row.names = FALSE)


