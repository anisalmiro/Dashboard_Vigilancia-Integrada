
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)
library(janitor)
library(forcats)



dados <- B_geral_HCA_R %>% 
  dplyr::filter(vigilancia =="Comunitaria") 


dados <- dados %>%
  rename(
    provincia = provincia_casos,
    idade1 = idade_complement,
    sintomas = sintomas
  )

# 4. PADRONIZAR TEXTO

dados <- dados %>%
  mutate(
    sintomas = str_to_lower(sintomas)
  )


# 5. CRIAR GRUPOS ETÁRIOS


dados <- dados %>%
  mutate(
    grupo_idade = case_when(
      idade <= 1 ~ "0-1",
      idade >= 2 & idade <= 4 ~ "2-4",
      idade >= 5 & idade <= 14 ~ "5-14",
      idade >= 15 & idade <= 49 ~ "15-49",
      idade >= 50 & idade <= 64 ~ "50-64",
      idade >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  )

# 6. PADRONIZAÇÃO DOS SINTOMAS


dados <- dados %>%
  mutate(
    
    tosse =
      ifelse(str_detect(sintomas, "tosse"), 1, 0),
    
    rinorreia =
      ifelse(str_detect(sintomas, "rinorreia"), 1, 0),
    
    febre =
      ifelse(str_detect(sintomas, "febre"), 1, 0),
    
    dificuldade_respiratoria =
      ifelse(
        str_detect(
          sintomas,
          "dificuldade_respiratoria"
        ),
        1,0
      ),
    
    inflamacao_garganta =
      ifelse(
        str_detect(
          sintomas,
          "inflamacao_da_garganta|irritacao_da_garganta"
        ),
        1,0
      ),
    
    cefaleia =
      ifelse(
        str_detect(sintomas,"cefaleia"),
        1,0
      ),
    
    fraqueza =
      ifelse(
        str_detect(sintomas,"fraqueza"),
        1,0
      ),
    
    anorexia =
      ifelse(
        str_detect(sintomas,"anorexia"),
        1,0
      ),
    
    calafrios =
      ifelse(
        str_detect(sintomas,"calafrios"),
        1,0
      ),
    
    dor_abdominal =
      ifelse(
        str_detect(
          sintomas,
          "dor abdominal|dor do abdomen|dores de barriga|dor de barriga"
        ),
        1,0
      ),
    
    dor_lombar =
      ifelse(
        str_detect(
          sintomas,
          "dor lombar|dor lombal"
        ),
        1,0
      ),
    
    dor_dentaria =
      ifelse(
        str_detect(
          sintomas,
          "dor no dente|dor do dente"
        ),
        1,0
      ),
    
    dor_ouvido =
      ifelse(
        str_detect(
          sintomas,
          "dor do ouvido|otite"
        ),
        1,0
      ),
    
    dores_articulares =
      ifelse(
        str_detect(
          sintomas,
          "dores_articulares"
        ),
        1,0
      ),
    
    dor_coluna =
      ifelse(
        str_detect(
          sintomas,
          "dores de coluna"
        ),
        1,0
      ),
    
    hipertensao =
      ifelse(
        str_detect(
          sintomas,
          "hipertensao|hipertensão|hipertensao arterial|hipertensao_arterial"
        ),
        1,0
      ),
    
    malaria =
      ifelse(
        str_detect(
          sintomas,
          "malaria"
        ),
        1,0
      ),
    
    asma =
      ifelse(
        str_detect(
          sintomas,
          "asma"
        ),
        1,0
      ),
    
    diarreia =
      ifelse(
        str_detect(
          sintomas,
          "diarreia"
        ),
        1,0
      ),
    
    vomitos =
      ifelse(
        str_detect(
          sintomas,
          "vomitos"
        ),
        1,0
      )
  )


# 7. TABELA DE PROVÍNCIA


freq_provincia <- dados %>%
  tabyl(provincia) %>%
  adorn_pct_formatting()

freq_provincia


# 8. TABELA DE SEXO


freq_sexo <- dados %>%
  tabyl(sexo) %>%
  adorn_pct_formatting()

freq_sexo


# 9. TABELA DE GRUPO ETÁRIO


freq_idade <- dados %>%
  tabyl(grupo_idade) %>%
  adorn_pct_formatting()

freq_idade


# 10. BASE LONGA DE SINTOMAS


sintomas_long <- dados %>%
  
  select(
    provincia,
    sexo,
    grupo_idade,
    
    tosse,
    rinorreia,
    febre,
    dificuldade_respiratoria,
    inflamacao_garganta,
    cefaleia,
    fraqueza,
    anorexia,
    calafrios,
    dor_abdominal,
    dor_lombar,
    dor_dentaria,
    dor_ouvido,
    dores_articulares,
    dor_coluna,
    hipertensao,
    malaria,
    asma,
    diarreia,
    vomitos
  ) %>%
  
  pivot_longer(
    cols = c(
      tosse,
      rinorreia,
      febre,
      dificuldade_respiratoria,
      inflamacao_garganta,
      cefaleia,
      fraqueza,
      anorexia,
      calafrios,
      dor_abdominal,
      dor_lombar,
      dor_dentaria,
      dor_ouvido,
      dores_articulares,
      dor_coluna,
      hipertensao,
      malaria,
      asma,
      diarreia,
      vomitos
    ),
    names_to = "sintoma",
    values_to = "presente"
  ) %>%
  
  filter(presente == 1)


# 11. FREQUÊNCIA DOS SINTOMAS


freq_sintomas <- sintomas_long %>%
  
  dplyr::count(sintoma) %>%
  
  arrange(desc(n))

freq_sintomas


# 12. EXPORTAR TABELAS


write.csv(
  freq_provincia,
  "frequencia_provincia.csv",
  row.names = FALSE
)

write.csv(
  freq_sexo,
  "frequencia_sexo.csv",
  row.names = FALSE
)

write.csv(
  freq_idade,
  "frequencia_grupo_idade.csv",
  row.names = FALSE
)

write.csv(
  freq_sintomas,
  "frequencia_sintomas.csv",
  row.names = FALSE
)


# 13. TOP 10 SINTOMAS


top10 <- freq_sintomas %>%
  slice_max(
    n,
    n = 10
  )


# 14. GRÁFICO GERAL


geral<- ggplot(
  top10,
  aes(
    x = reorder(sintoma, n),
    y = n
  )
) +
  
  geom_col(
    fill = "#2C7FB8"
  ) +
  
  coord_flip() +
  
  labs(
    title = "10 sintomas mais frequentes",
    x = "",
    y = "Frequência"
  ) +
  
  theme_bw()


# 15. SINTOMAS POR PROVÍNCIA



# FREQUÊNCIA DOS SINTOMAS POR PROVÍNCIA


freq_provincia_sintoma <- sintomas_long %>%
  
  dplyr::count(
    provincia,
    sintoma
  ) %>%
  
  group_by(provincia) %>%
  
  mutate(
    perc = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  
  ungroup()


# GRÁFICO


ggplot(
  freq_provincia_sintoma,
  aes(
    x = fct_reorder(sintoma, n),
    y = n
  )
) +
  
  geom_col(
    fill = "#2C7FB8",
    width = 0.8
  ) +
  
  geom_text(
    aes(
      label = paste0(
        n,
        " (",
        perc,
        "%)"
      )
    ),
    hjust = -0.15,
    size = 3.2,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  facet_wrap(
    ~ provincia,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.20)
    )
  ) +
  
  labs(
    title = "Frequência dos sintomas reportados por província",
    x = "",
    y = "Número de casos"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      size = 16,
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    axis.title = element_text(
      face = "bold"
    )
  )


# EXPORTAR


ggsave(
  "Sintomas_por_Provincia.png",
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# FREQUÊNCIA DOS SINTOMAS POR SEXO E PROVÍNCIA


freq_sexo_provincia <- sintomas_long %>%
  
  dplyr::count(
    provincia,
    sintoma,
    sexo
  ) %>%
  
  group_by(
    provincia,
    sintoma
  ) %>%
  
  mutate(
    perc = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  
  ungroup()


# GRÁFICO


ggplot(
  freq_sexo_provincia,
  aes(
    x = fct_reorder(sintoma, n),
    y = n,
    fill = sexo
  )
) +
  
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  geom_text(
    aes(
      label = paste0(
        n,
        " (",
        perc,
        "%)"
      )
    ),
    position = position_dodge(width = 0.8),
    hjust = -0.15,
    size = 2.8,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  facet_wrap(
    ~ provincia,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(0, 0.25)
    )
  ) +
  
  labs(
    title = "Distribuição dos sintomas por sexo e província",
    subtitle = "Percentagem de homens e mulheres dentro de cada sintoma",
    x = "",
    y = "Número de casos",
    fill = "Sexo"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    legend.position = "bottom"
  )

# EXPORTAR


ggsave(
  "Sintomas_Sexo_Provincia.png",
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)



freq_sexo_provincia <- sintomas_long %>%
  
  dplyr::count(
    provincia,
    sintoma,
    sexo
  ) %>%
  
  group_by(
    provincia,
    sintoma
  ) %>%
  
  mutate(
    perc = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  
  ungroup()



sintomassexo <- ggplot(
  freq_sexo_provincia,
  aes(
    x = sintoma,
    y = perc,
    fill = sexo
  )
) +
  
  geom_col(
    width = 0.8
  ) +
  
  geom_text(
    aes(
      label = paste0(
        n,
        "\n(",
        perc,
        "%)"
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 2.8,
    fontface = "bold"
  ) +
  
  coord_flip() +
  
  facet_wrap(
    ~ provincia,
    scales = "free_y",
    ncol = 2
  ) +
  
  labs(
    title = "Distribuição dos sinais e sintomas por sexo e província",
    x = "",
    y = "Percentagem (%)",
    fill = "Sexo"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 16
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    legend.position = "bottom"
  )

sintomassexo


# EXPORTAR


ggsave(
  "Sintomas_Sexo_Provincia_Empilhado.png",
  plot = sintomassexo,
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)









# 17. SINTOMAS POR GRUPO ETÁRIO


freq_idade_sintoma <- sintomas_long %>%
  
  dplyr::count(
    grupo_idade,
    sintoma
  )

ggplot(
  freq_idade_sintoma,
  aes(
    x = reorder(sintoma, n),
    y = n
  )
) +
  
  geom_col(
    fill = "#41AB5D"
  ) +
  
  coord_flip() +
  
  facet_wrap(
    ~ grupo_idade,
    scales = "free_y"
  ) +
  
  labs(
    title = "Frequência dos sintomas por grupo etário",
    x = "",
    y = "Número de casos"
  ) +
  
  theme_bw()


# 18. TOP 10 SINTOMAS POR PROVÍNCIA



# FREQUÊNCIA DOS SINTOMAS POR IDADE E PROVÍNCIA



# FREQUÊNCIA DOS SINTOMAS POR IDADE E PROVÍNCIA


freq_idade_provincia <- sintomas_long %>%
  
  dplyr::count(
    provincia,
    sintoma,
    grupo_idade
  ) %>%
  
  group_by(
    provincia,
    sintoma
  ) %>%
  
  mutate(
    perc = round(
      100 * n / sum(n),
      1
    )
  ) %>%
  
  ungroup()


# ORDENAR FAIXAS ETÁRIAS


freq_idade_provincia <- freq_idade_provincia %>%
  dplyr::mutate(
    grupo_idade = factor(
      grupo_idade,
      levels = c(
        "0-1",
        "2-4",
        "5-14",
        "15-49",
        "50-64",
        "65+"
      ),
      ordered = TRUE
    )
  )


# CRIAR LABELS


freq_idade_provincia <- freq_idade_provincia %>%
  
  dplyr::mutate(
    label = ifelse(
      perc >= 5,
      paste0(
        n,
        "\n(",
        perc,
        "%)"
      ),
      ""
    )
  )


# GRÁFICO


sintomasidade <- ggplot(
  freq_idade_provincia,
  aes(
    x = sintoma,
    y = perc,
    fill = grupo_idade
  )
) +
  
  geom_col(
    width = 0.8
  ) +
  
  geom_text(
    aes(
      label = label
    ),
    position = position_stack(vjust = 0.5),
    size = 2.7,
    fontface = "bold",
    colour = "black"
  ) +
  
  coord_flip() +
  
  facet_wrap(
    ~ provincia,
    scales = "free_y",
    ncol = 2
  ) +
  
  scale_fill_manual(
    values = c(
      "0-1"   = "#d73027",
      "2-4"   = "#fc8d59",
      "5-14"  = "#fee090",
      "15-49" = "#91bfdb",
      "50-64" = "#4575b4",
      "65+"   = "#313695"
    ),
    breaks = c(
      "0-1",
      "2-4",
      "5-14",
      "15-49",
      "50-64",
      "65+"
    ),
    drop = FALSE
  ) +
  
  labs(
    title = "Distribuição dos sinais e sintomas por grupo etário e província",
    subtitle = "Percentagem dos grupos etários dentro de cada sintoma",
    x = "",
    y = "Percentagem (%)",
    fill = "Grupo etário"
  ) +
  
  theme_bw(base_size = 13) +
  
  theme(
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 16
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA
    ),
    
    panel.grid.minor = element_blank(),
    
    axis.text.y = element_text(
      size = 9
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    )
  )


# VISUALIZAR


sintomasidade


# EXPORTAR


ggsave(
  "Sintomas_Grupo_Idade_Provincia.png",
  plot = sintomasidade,
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)

# 19. EXPORTAR BASE PADRONIZADA


write.csv(
  dados,
  "base_padronizada_sintomas.csv",
  row.names = FALSE
)

