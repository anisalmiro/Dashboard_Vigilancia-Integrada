
setwd("C")


library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

df <- read_excel("dados_dercio_influenza_convertido.xlsx", sheet = "Sheet1")


df <- df %>%
  mutate(
    H1N1pdm = as.numeric(ifelse(H1N1pdm %in% c("Negativo", "N/A", "NA"), NA_character_, H1N1pdm)),
    H3N2 = as.numeric(ifelse(H3N2 %in% c("Negativo", "N/A", "NA"), NA_character_, H3N2)),
    Victoria = as.numeric(ifelse(Victoria %in% c("Negativo", "N/A", "NA"), NA_character_, Victoria)),
    Yamagata = as.numeric(ifelse(Yamagata %in% c("Negativo", "N/A", "NA"), NA_character_, Yamagata))
  )


df <- df %>%
  mutate(
    type = case_when(
      Site == "Wastewater" ~ "Wastewater",
      grepl("^IDSC", Site) ~ "Community",
      grepl("^IDS", Site) ~ "Health Care Facility",
      TRUE ~ NA_character_
    )
  )


df <- df %>%
  mutate(
    positive_subtype = case_when(
      !is.na(H1N1pdm) ~ "H1N1pdm",
      !is.na(H3N2) ~ "H3N2",
      !is.na(Victoria) ~ "Victoria",
      !is.na(Yamagata) ~ "Yamagata",
      type == "Wastewater" & grepl("^6B", Clade) ~ "H1N1pdm",
      type == "Wastewater" & grepl("^3C", Clade) ~ "H3N2",
      type == "Wastewater" & grepl("^V", Clade) ~ "Victoria",
      type == "Wastewater" & grepl("^Y", Clade) ~ "Yamagata",
      TRUE ~ NA_character_
    ),
    Ct = case_when(
      positive_subtype == "H1N1pdm" ~ H1N1pdm,
      positive_subtype == "H3N2" ~ H3N2,
      positive_subtype == "Victoria" ~ Victoria,
      positive_subtype == "Yamagata" ~ Yamagata,
      TRUE ~ NA_real_
    )
  )


df <- df %>%
  mutate(
    influenza_type = case_when(
      positive_subtype == "H1N1pdm" ~ "A/H1N1pdm",
      positive_subtype == "H3N2" ~ "A/H3N2",
      positive_subtype == "Victoria" ~ "B/Victoria",
      positive_subtype == "Yamagata" ~ "B/Yamagata",
      TRUE ~ NA_character_
    ),
    display_label = case_when(
      is.na(Clade) | Clade == "" | Clade %in% c("N/A", "NA", "Negativo") ~ influenza_type,
      TRUE ~ paste(influenza_type, "(", Clade, ")", sep = "")
    )
  )



df <- df %>% 
  filter(
    !is.na(positive_subtype) & 
      !is.na(type) & 
      (type == "Wastewater" | !is.na(Ct))
  )


df_summary <- df %>%
  group_by(`Epi week`, type, display_label) %>%
  summarise(count = n(), .groups = 'drop')


all_weeks <- range(read_excel("dados_dercio_influenza_convertido.xlsx", sheet = "Sheet1")$`Epi week`, na.rm = TRUE)


all_combinations <- expand.grid(
  `Epi week` = unique(df$`Epi week`), 
  type = unique(df_summary$type),
  display_label = unique(df_summary$display_label)
)


df_summary_complete <- all_combinations %>%
  left_join(df_summary, by = c("Epi week", "type", "display_label")) %>%
  mutate(count = ifelse(is.na(count), 0, count))


unique_labels <- unique(df_summary_complete$display_label)
print(unique_labels)


unique_labels <- unique(df_summary_complete$display_label)

color_palette <- setNames(
  viridis(length(unique_labels), option = "turbo"),  
  unique_labels
)


p <- ggplot(df_summary_complete, aes(x = `Epi week`, y = count, fill = display_label)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  facet_grid(type ~ ., scales = "free_y") +  
  scale_fill_manual(values = color_palette) + 
  labs(
    title = "",
    subtitle = "",
    x = "Epidemiological Week",
    y = "Number of Detections",
    fill = "Influenza Type (Clade)"
  ) +
  theme_minimal(base_size = 14) +  
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  scale_x_continuous(
    breaks = seq(all_weeks[1], all_weeks[2], by = 1),  
    limits = c(all_weeks[1] - 0.5, all_weeks[2] + 0.5)  
  )


print(p)


ggsave("influenza_trends_plot.png", p, width = 12, height = 8, dpi = 300)