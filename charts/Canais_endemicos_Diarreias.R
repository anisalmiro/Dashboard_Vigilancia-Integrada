library(haven)
library(ggplot2) 
library(zoo)
library(reshape2)
library(readxl)
library(dplyr)
library(forecast)



#---------------------------------------------------------------
# 1. Leitura da base principal e seleccionacao do distrito kamubukwana
#---------------------------------------------------------------
#carregar casos de diareia DiarreiaZimpeto.dta


DB_2024 <- read_dta("C:/Github/IDS_API/raw/DiarreiaZimpeto.dta")

AllDistrit <- DB_2024 %>%
  filter(Distrito_R == "KAMUBUKWANA") %>%
  group_by(Semana) %>%
  summarise(
    cases = sum(Diarreia_R2, na.rm = TRUE),
    start_date = min(date, na.rm = TRUE)
  ) %>%
  filter(!is.na(cases))

#---------------------------------------------------------------
# 2. Leitura da base de dados retrospectiva e calculo de percentis semanais
#---------------------------------------------------------------
DB_retrospectivo <- read_dta("C:/Github/IDS_API/raw/Retrospectivos3.dta")

summary_data <- DB_retrospectivo %>%
  group_by(Semana) %>%
  summarise(
    p5  = quantile(Diarreia_R2, probs = 0.05, na.rm = TRUE),
    p25 = quantile(Diarreia_R2, probs = 0.25, na.rm = TRUE),
    p50 = quantile(Diarreia_R2, probs = 0.50, na.rm = TRUE),
    p75 = quantile(Diarreia_R2, probs = 0.75, na.rm = TRUE),
    p95 = quantile(Diarreia_R2, probs = 0.95, na.rm = TRUE)
  )

#---------------------------------------------------------------
# 3. Criacao do gráfico com faixas de percentis
#---------------------------------------------------------------

p <- ggplot(summary_data, aes(x = Semana)) +
  
  # Faixa 1: muito baixo (abaixo do p5)
  geom_ribbon(aes(ymin = 0, ymax = p5, fill = "Muito baixo (<p5)"), alpha = 0.5) +
  
  # Faixa 2: baixo (p5–p25)
  geom_ribbon(aes(ymin = p5, ymax = p25, fill = "Baixo (p5–p25)"), alpha = 0.5) +
  
  # Faixa 3: normal (p25–p75)
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Normal (p25–p75)"), alpha = 0.5) +
  
  # Faixa 4: alto (p75–p95)
  geom_ribbon(aes(ymin = p75, ymax = p95, fill = "Alto (p75–p95)"), alpha = 0.5) +
  
  # Faixa 5: muito alto (>p95)
  geom_ribbon(aes(ymin = p95, ymax = max(p95, na.rm = TRUE)*1.1, fill = "Muito alto (>p95)"), alpha = 0.4) +
  
  # Linha da mediana (percentil 50)
  geom_line(aes(y = p50, color = "Mediana histórica (p50)"), size = 1.2) +
  
  # Linha dos casos reais (2025)
  geom_line(data = AllDistrit, aes(x = Semana, y = cases, color = "Casos reportados (2025)"), size = 1.3) +
  
  # Escalas e cores
  scale_x_continuous(breaks = seq(1, 52, by = 2)) +
  
  scale_fill_manual(
    name = "Níveis de ocorrência",
    values = c(
      "Muito baixo (<p5)"  = "#2ECC71",  # verde
      "Baixo (p5–p25)"     = "#F7DC6F",  # amarelo
      "Normal (p25–p75)"   = "#F39C12",  # laranja
      "Alto (p75–p95)"     = "#E74C3C",  # vermelho
      "Muito alto (>p95)"  = "#8E44AD"   # bordô / roxo escuro
    )
  ) +
  
  scale_color_manual(
    name = "Linhas de referência",
    values = c(
      "Mediana histórica (p50)" = "black",
      "Casos reportados (2025)" = "darkblue"
    )
  ) +
  

  labs(
    title = "Monitoria dos casos de diarreia – Distrito Kamubukwana",
    x = "Semanas epidemiológicas",
    y = "Número de casos reportados"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.box = "vertical"
  )


#---------------------------------------------------------------
# 4. Exportacao  do gráfico
#---------------------------------------------------------------
ggsave(
  filename = "canal_endemico_zimpeto_OMS.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


#---------------------------------------------------------------
# 1. Leitura da base principal e seleccao do distrito
#---------------------------------------------------------------
DB_2024 <- read_dta("C:/Github/IDS_API/raw/DiarreiaZimpetocomunid.dta")

AllDistrit <- DB_2024 %>%
  # filter(Distrito_R == "KAMUBUKWANA") %>%
  group_by(Semana) %>%
  summarize(
    cases = sum(Diarreia_R2, na.rm = TRUE)
    #start_date = min(date)
  ) %>%
  filter(!is.na(cases))


#---------------------------------------------------------------
# 2. Leitura da base retrospectiva e calcular percentis semanais
#---------------------------------------------------------------
DB_retrospectivo <- read_dta("C:/Github/IDS_API/raw/Retrospectivos3.dta")

summary_data <- DB_retrospectivo %>%
  group_by(Semana) %>%
  summarise(
    p5  = quantile(Diarreia_R2, probs = 0.05, na.rm = TRUE),
    p25 = quantile(Diarreia_R2, probs = 0.25, na.rm = TRUE),
    p50 = quantile(Diarreia_R2, probs = 0.50, na.rm = TRUE),
    p75 = quantile(Diarreia_R2, probs = 0.75, na.rm = TRUE),
    p95 = quantile(Diarreia_R2, probs = 0.95, na.rm = TRUE)
  )

#---------------------------------------------------------------
# 3. Criacao do gráfico com faixas de percentis 
#---------------------------------------------------------------

p <- ggplot(summary_data, aes(x = Semana)) +
  
  # Faixa 1: muito baixo (abaixo do p5)
  geom_ribbon(aes(ymin = 0, ymax = p5, fill = "Muito baixo (<p5)"), alpha = 0.5) +
  
  # Faixa 2: baixo (p5–p25)
  geom_ribbon(aes(ymin = p5, ymax = p25, fill = "Baixo (p5–p25)"), alpha = 0.5) +
  
  # Faixa 3: normal (p25–p75)
  geom_ribbon(aes(ymin = p25, ymax = p75, fill = "Normal (p25–p75)"), alpha = 0.5) +
  
  # Faixa 4: alto (p75–p95)
  geom_ribbon(aes(ymin = p75, ymax = p95, fill = "Alto (p75–p95)"), alpha = 0.5) +
  
  # Faixa 5: muito alto (>p95)
  geom_ribbon(aes(ymin = p95, ymax = max(p95, na.rm = TRUE)*1.1, fill = "Muito alto (>p95)"), alpha = 0.4) +
  
  # Linha da mediana (percentil 50)http://127.0.0.1:16968/graphics/plot_zoom_png?width=969&height=959
  geom_line(aes(y = p50, color = "Mediana histórica (p50)"), size = 1.2) +
  
  # Linha dos casos reais (2025)
  geom_line(data = AllDistrit, aes(x = Semana, y = cases, color = "Casos reportados (2025)"), size = 1.3) +
  
  # Escalas e cores
  scale_x_continuous(breaks = seq(1, 52, by = 2)) +
  
  scale_fill_manual(
    name = "Níveis de ocorrência",
    values = c(
      "Muito baixo (<p5)"  = "#2ECC71",  # verde
      "Baixo (p5–p25)"     = "#F7DC6F",  # amarelo
      "Normal (p25–p75)"   = "#F39C12",  # laranja
      "Alto (p75–p95)"     = "#E74C3C",  # vermelho
      "Muito alto (>p95)"  = "#8E44AD"   # bordô / roxo escuro
    )
  ) +
  
  scale_color_manual(
    name = "Linhas de referência",
    values = c(
      "Mediana histórica (p50)" = "black",
      "Casos reportados (2025)" = "darkblue"
    )
  ) +
  

  labs(
    title = "Monitoria dos casos de diarreia – Distrito Kamubukwana-comunidade",
    x = "Semanas epidemiológicas",
    y = "Número de casos reportados"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "bottom",
    legend.box = "vertical"
  )

print(p)

#---------------------------------------------------------------
# 4. Exportacao  do gráfico
#---------------------------------------------------------------
ggsave(
  filename = "canal_endemico_zimpetocomunidade_OMS.png",
  plot = p,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)


