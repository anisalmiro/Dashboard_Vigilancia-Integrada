

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(stringr)

#data <- read.table("dados_nuro_sars_cov_2.txt", 
#                   sep = "\t", 
#                   header = TRUE, 
#                   stringsAsFactors = FALSE,
#                   quote = "")


#READ CSV
data <- read.csv("dados_nuro_sars_cov_2.csv", 
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     quote = "")

#write as dados_nuro_sars_cov_2.xlsx
#write_csv(data, "dados_nuro_sars_cov_2.csv")



data$Surveillance_Type <- case_when(
  grepl("^IDS\\d", data$Site) ~ "Health Care facility",
  grepl("^IDSC", data$Site) ~ "Community", 
  data$Site == "Wastewater" ~ "Wastewater",
  TRUE ~ "Other"
)

data$Epi.week <- as.numeric(data$Epi.week)
data$Year <- as.numeric(data$Year)


data <- data %>%
  mutate(Sort_Key = Year * 100 + Epi.week)

expanded_data <- data.frame()

for(i in 1:nrow(data)) {
  row_data <- data[i, ]
  
  if(is.na(row_data$Clade..Pango.lineage.) || row_data$Clade..Pango.lineage. == "") next
  
  lineages <- str_split(row_data$Clade..Pango.lineage., ";")[[1]]
  lineages <- str_trim(lineages)
  
  if(row_data$Surveillance_Type == "Wastewater" && 
     !is.na(row_data$Proportion) && 
     row_data$Proportion != "") {
    
    prop_str <- gsub('"', '', row_data$Proportion)
    props <- str_split(prop_str, "[;,]")[[1]]
    props <- as.numeric(str_trim(props))
    props <- props[!is.na(props)]
    
    if(length(props) == length(lineages)) {
      counts <- props
    } else if(length(props) > 0) {
      counts <- rep(sum(props) / length(lineages), length(lineages))
    } else {
      counts <- rep(1, length(lineages))
    }
  } else {
    counts <- rep(1, length(lineages))
  }
  
  for(j in 1:length(lineages)) {
    expanded_data <- rbind(expanded_data, data.frame(
      Epi.week = row_data$Epi.week,
      Year = row_data$Year,
      Sort_Key = row_data$Sort_Key,
      Surveillance_Type = row_data$Surveillance_Type,
      Lineage_Full = lineages[j],  
      Count = counts[j],
      stringsAsFactors = FALSE
    ))
  }
}

expanded_data <- expanded_data %>% filter(!is.na(Epi.week))


week_year_data <- data %>%
  filter(!is.na(Epi.week)) %>%
  select(Epi.week, Year, Sort_Key) %>%
  distinct() %>%
  arrange(Sort_Key)

actual_weeks <- week_year_data$Epi.week
actual_years <- week_year_data$Year


week_labels <- paste0(actual_weeks, " (", actual_years, ")")

cat("Weeks in chronological order:", paste(week_labels, collapse = ", "), "\n")

all_lineages <- sort(unique(expanded_data$Lineage_Full))
cat("Total unique lineages:", length(all_lineages), "\n")


professional_colors <- c(
  "Not detected" = "#000000",   # 
  "Pending" = "#FFD700",        # 
  "Not tested" = "#8B4513",     # 
  "#5B2C6F",  # 
  "#3E885B", "#4DA8DA", "#F18F01", "#C73E1D", 
  "#6A994E", "#A7C957", "#F2E8CF", "#BC6C25", "#DDA15E",
  "#7D5A50",  
  "#606C38", "#FEFAE0", "#BC4749", "#F2CC8F",
  "#81B29A", "#F07167", "#0081A7", "#00AFB9", "#FDFCDC",
  "#FF9F1C", "#FFB627", "#FF6B35", "#F7931E", "#C9ADA7",
  "#6B4C9A",  
  "#4A4E69", "#9A8C98", "#F2E9E4", "#264653",
  "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51", "#8ECAE6",
  "#219EBC", 
  "#B8860B",  
  "#FFB3BA", "#FFDFBA", "#6D8299",
  "#8B5FBF", "#D4A5A5", "#9B59B6", "#E74C3C", "#3498DB",
  "#1ABC9C", "#F39C12", "#95A5A6", "#34495E"
)

n_lineages <- length(all_lineages)
if(n_lineages <= length(professional_colors)) {
  lineage_colors <- professional_colors[1:n_lineages]
} else {
  stop("Not enough unique colors for all lineages. Please add more colors to professional_colors.")
}
names(lineage_colors) <- all_lineages

plot_data <- expanded_data %>%
  group_by(Epi.week, Year, Sort_Key, Surveillance_Type, Lineage_Full) %>%
  summarise(Total_Count = sum(Count, na.rm = TRUE), .groups = "drop")


plot_data <- plot_data %>%
  mutate(Week_Label = paste0(Epi.week, " (", Year, ")"))

p <- ggplot(plot_data, aes(x = factor(Week_Label, levels = week_labels), 
                           y = Total_Count, 
                           fill = Lineage_Full)) +
  geom_col(position = "stack", width = 0.6, alpha = 0.9) +
  facet_wrap(~factor(Surveillance_Type, levels = c("Community", "Health Care facility", "Wastewater")), 
             scales = "free_y", ncol = 1, strip.position = "right") +
  scale_fill_manual(values = lineage_colors, name = "SARS-CoV-2 Lineage") +
  scale_x_discrete(
    breaks = week_labels,
    labels = actual_weeks,  #
    expand = expansion(add = 0.5)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = scales::number_format(accuracy = 1)) +
  labs(
    x = "Epidemiological Week (2024-2025)",
    y = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#2E4057"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50"),
    strip.text = element_text(size = 16, face = "bold", color = "#2E4057"),
    strip.background = element_rect(fill = "#F8F9FA", color = "#DEE2E6"),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 14, color = "#2E4057"),
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "#ADB5BD", fill = NA, size = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "#495057"),
    axis.text.y = element_text(size = 12, color = "#495057"),
    axis.title = element_text(size = 14, face = "bold", color = "#2E4057"),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = guide_legend(
    ncol = 2, 
    override.aes = list(alpha = 1),
    title.position = "top",
    byrow = TRUE
  ))

print(p)

ggsave("SARS_CoV2_Exact_Surveillance.png", p, 
       width = 20, height = 8, dpi = 300, bg = "white")

ggsave("SARS_CoV2_Exact_Surveillance.pdf", p, 
       width = 20, height = 8, bg = "white")

cat("\n=== EXACT DATA SUMMARY ===\n")
cat("Epidemiological weeks shown (chronologically):", paste(week_labels, collapse = ", "), "\n")
cat("Total unique lineages:", n_lineages, "\n")
cat("Total data points:", nrow(plot_data), "\n")

week_summary <- plot_data %>%
  group_by(Epi.week, Year, Surveillance_Type) %>%
  summarise(
    Lineages = n_distinct(Lineage_Full),
    Total_Detections = sum(Total_Count),
    .groups = "drop"
  ) %>%
  arrange(Year, Epi.week, Surveillance_Type)

print(week_summary)

cat("\n✓ Files saved: SARS_CoV2_Exact_Surveillance.png & .pdf\n")
cat("🎯 Exact data representation complete! Professional visualization ready! 🎯\n")