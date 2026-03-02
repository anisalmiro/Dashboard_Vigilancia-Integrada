
# =========================================================
# CENTRÓIDES (PARA OS CÍRCULOS VERDES)
# =========================================================
centroides <- st_centroid(mapa_dados)

# =========================================================
# MAPA FINAL
# =========================================================
ggplot() +
  
  # ---------------- POLÍGONOS DOS BAIRROS ----------------
geom_sf(
  data = mapa_dados,
  aes(fill = classe_testados),
  color = "grey40",
  size = 0.2
) +
  
  # ---------------- PONTOS (CENTRÓIDES) ----------------
geom_sf(
  data = centroides,
  aes(size = positivos_influenza),
  color = "darkgreen",
  alpha = 0.8
) +
  
  # ---------------- LABELS DOS BAIRROS ----------------
geom_sf_text(
  data = mapa_dados,
  aes(label = bairro_geo),
  size = 2.5,
  color = "black",
  check_overlap = TRUE
) +
  
  # ---------------- ESCALA DE CORES ----------------
scale_fill_manual(
  name = "Total de testados",
  values = c(
    "1 - 25" = "#f2e6f5",
    "26 - 50" = "#d7a1b0",
    "51 - 75" = "#a25364",
    "> 75"   = "#6b0000"
  ),
  drop = FALSE
) +
  
  # ---------------- ESCALA DOS PONTOS ----------------
scale_size_continuous(
  name = "Positivos para Influenza",
  breaks = c(1, 15, 30),
  range = c(2, 8)
) +
  
  # ---------------- TEMA ----------------
theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "left"
  ) +
  
  # ---------------- TÍTULOS ----------------
labs(
  title = "Distribuição Espacial dos Casos de Influenza",
  subtitle = "Cidade e Província de Maputo",
  caption = "Fonte: Vigilância Epidemiológica / EWARS"
)





#sarscov2

ggplot() +
  
  # ---------------- POLÍGONOS DOS BAIRROS ----------------
geom_sf(
  data = mapa_dados,
  aes(fill = classe_testados),
  color = "grey40",
  size = 0.2
) +
  
  # ---------------- PONTOS (CENTRÓIDES – SARS-CoV-2) ----------------
geom_sf(
  data = centroides,
  aes(size = positivos_sarscov2),
  color = "darkred",
  alpha = 0.8
) +
  
  # ---------------- LABELS DOS BAIRROS ----------------
geom_sf_text(
  data = mapa_dados,
  aes(label = bairro_geo),
  size = 2.3,
  color = "black",
  check_overlap = TRUE
) +
  
  # ---------------- ESCALA DE CORES ----------------
scale_fill_manual(
  name = "Total de testados",
  values = c(
    "1 - 25" = "#f2e6f5",
    "26 - 50" = "#d7a1b0",
    "51 - 75" = "#a25364",
    "> 75"   = "#6b0000"
  ),
  drop = FALSE
) +
  
  # ---------------- ESCALA DOS PONTOS (SARS-CoV-2) ----------------
scale_size_continuous(
  name = "Positivos para SARS-CoV-2",
  breaks = c(1, 10, 25, 50),
  range = c(2, 9)
) +
  
  # ---------------- TEMA ----------------
theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "left",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  ) +
  
  # ---------------- TÍTULOS ----------------
labs(
  title = "Distribuição Espacial dos Casos de SARS-CoV-2",
  subtitle = "Cidade e Província de Maputo",
  caption = "Fonte: Vigilância Epidemiológica / EWARS"
)

