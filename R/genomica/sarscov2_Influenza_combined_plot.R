# ===============================
# Libraries
# ===============================
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

# ===============================
# Load data
# ===============================
file_path <- "C:/Github/IDS_API/R/Dashboard/BD_GENOMICA_LINED.csv"
if (!file.exists(file_path)) stop(paste("File not found:", file_path))

df <- read.csv(
  file_path,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

# ===============================
# Filter pathogen
# ===============================
var_Influenza <- "Influenza"
df <- df %>% filter(Patogeno == var_Influenza)

# ===============================
# Normalize column names
# ===============================
orig_names <- names(df)
norm_names <- tolower(gsub("[^[:alnum:]_]", "_", orig_names))
names(df) <- norm_names

# ===============================
# Helper to find columns
# ===============================
find_col <- function(patterns) {
  cols <- grep(
    paste(patterns, collapse = "|"),
    names(df),
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(cols) == 0) return(NA_character_)
  cols[1]
}

week_col    <- find_col(c("^week$", "^week_", "\\bweek\\b"))
year_col    <- find_col(c("^year$", "^ano$", "collection_year"))
count_col   <- find_col(c("^count$", "^n$", "^number$"))
ids_col     <- find_col(c("^ids_id$", "^ids$", "^id[_]?s"))
subtype_col <- find_col(c("subtype", "subclade"))

if (is.na(week_col)) stop("No week column found")
if (is.na(year_col)) stop("No year column found")
if (is.na(subtype_col)) stop("No subtype/subclade column found")

if (is.na(count_col)) {
  df$count <- NA_real_
  count_col <- "count"
}

# ===============================
# Clean & convert
# ===============================
df[[week_col]]  <- suppressWarnings(as.integer(df[[week_col]]))
df[[year_col]]  <- suppressWarnings(as.integer(df[[year_col]]))
df[[count_col]] <- suppressWarnings(as.numeric(df[[count_col]]))
# make column names unique
names(df) <- make.unique(names(df))


df <- df %>%
  filter(
    !is.na(.data[[week_col]]),
    !is.na(.data[[year_col]])
  )

# ===============================
# Rename canonical columns
# ===============================
names(df)[names(df) == week_col]  <- "week"
names(df)[names(df) == year_col]  <- "year"
names(df)[names(df) == count_col] <- "count"
if (!is.na(ids_col)) names(df)[names(df) == ids_col] <- "ids_id"
names(df)[names(df) == subtype_col] <- "subtype_and_subclade"

# ===============================
# Define surveillance type
# ===============================
if ("ids_id" %in% names(df)) {
  df$type <- case_when(
    df$ids_id == "IDSW" ~ "Wastewater",
    grepl("^idsc", df$ids_id, ignore.case = TRUE) ~ "Community",
    grepl("^ids", df$ids_id, ignore.case = TRUE) ~ "Health Care Facility",
    TRUE ~ NA_character_
  )
} else {
  df$type <- NA_character_
}

# ===============================
# Display label
# ===============================
df$display_label <- as.character(df$subtype_and_subclade)
df <- df %>% filter(!is.na(display_label), nzchar(display_label))

# ===============================
# Create Year-Week axis
# ===============================
df$epi_yearweek <- paste0(
  df$year, "-W", sprintf("%02d", df$week)
)

df$epi_yearweek <- factor(
  df$epi_yearweek,
  levels = unique(df$epi_yearweek[order(df$year, df$week)])
)

# ===============================
# Color palette
# ===============================
unique_labels <- sort(unique(df$display_label))
color_palette <- setNames(
  viridis(length(unique_labels), option = "turbo"),
  unique_labels
)
df$display_label <- factor(df$display_label, levels = unique_labels)

# ===============================
# Aggregate data
# ===============================
df_summary <- df %>%
  add_count(
    year,
    week,
    epi_yearweek,
    type,
    display_label,
    name = "count"
  ) %>%
  distinct(
    year,
    week,
    epi_yearweek,
    type,
    display_label,
    count
  )

# ===============================
# Wastewater as proportion
# ===============================
df_summary <- df_summary %>%
  group_by(type, epi_yearweek) %>%
  mutate(
    value_plot = ifelse(
      type == "Wastewater",
      count / sum(count, na.rm = TRUE),
      count
    )
  ) %>%
  ungroup()

# ===============================
# Plot
# ===============================
p <- ggplot(
  df_summary,
  aes(
    x = epi_yearweek,
    y = value_plot,
    fill = display_label
  )
) +
  geom_col(width = 0.7) +
  facet_grid(
    type ~ .,
    scales = "free_y",
    switch = "y"
  ) +
  scale_fill_manual(values = color_palette) +
  labs(
    x = "Epidemiological Week (Year–Week)",
    y = "Detections (Community / HCF) | Proportion (Wastewater)",
    fill = "Influenza Type / Clade"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      size = 10
    ),
    legend.position = "bottom",
    strip.text.y.right = element_text(
      face = "bold",
      size = 14
    ),
    strip.placement = "outside",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA
    )
  )

# ===============================
# Print
# ===============================
print(p)


ggsave("influenza_trends_plot.png", p, width = 12, height = 8, dpi = 300)



##--------------------------------------------------------  SARSCOV2  --------------------------------------------------------##
#sarscov2 script 
# ===============================
# Libraries
# ===============================
library(ggplot2)
library(dplyr)
library(viridis)

# ===============================
# Load data
# ===============================
file_path <- "C:/Github/IDS_API/R/Dashboard/BD_GENOMICA_LINED.csv"
if (!file.exists(file_path)) stop(paste("File not found:", file_path))

df <- read.csv(
  file_path,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

# ===============================
# Filter pathogen
# ===============================
var_sars_cov2 <- "Sars_C0v2"
df <- df %>% filter(Patogeno == var_sars_cov2)

# ===============================
# Normalize column names
# ===============================
names(df) <- tolower(gsub("[^[:alnum:]_]", "_", names(df)))
names(df) <- make.unique(names(df))   # CRÍTICO: evita erro no dplyr

# ===============================
# Helper to find columns
# ===============================
find_col <- function(patterns) {
  for (p in patterns) {
    exact <- grep(paste0("^", p, "$"), names(df), ignore.case = TRUE, value = TRUE)
    if (length(exact) > 0) return(exact[1])
  }
  cols <- grep(paste(patterns, collapse = "|"), names(df), ignore.case = TRUE, value = TRUE)
  if (length(cols) == 0) return(NA_character_)
  cols[1]
}

week_col    <- find_col(c("week"))
year_col    <- find_col(c("year", "ano", "collection_year"))
count_col   <- find_col(c("count", "n", "number"))
ids_col     <- find_col(c("ids_id", "ids", "id_s"))
subtype_col <- find_col(c("subtype", "subclade"))

if (is.na(week_col))    stop("No week column found")
if (is.na(year_col))    stop("No year column found")
if (is.na(subtype_col)) stop("No subtype column found")

if (is.na(count_col)) {
  df$count <- NA_real_
  count_col <- "count"
}

# ===============================
# Convert types
# ===============================
df[[week_col]]  <- suppressWarnings(as.integer(df[[week_col]]))
df[[year_col]]  <- suppressWarnings(as.integer(df[[year_col]]))
df[[count_col]] <- suppressWarnings(as.numeric(df[[count_col]]))

df <- df %>%
  filter(
    !is.na(.data[[week_col]]),
    !is.na(.data[[year_col]])
  )

# ===============================
# Rename canonical columns
# ===============================
names(df)[names(df) == week_col]  <- "week"
names(df)[names(df) == year_col]  <- "year"
names(df)[names(df) == count_col] <- "count"
if (!is.na(ids_col)) names(df)[names(df) == ids_col] <- "ids_id"
names(df)[names(df) == subtype_col] <- "subtype_and_subclade"

# ===============================
# Define surveillance type
# ===============================
if ("ids_id" %in% names(df)) {
  df$type <- case_when(
    df$ids_id == "IDSW" ~ "Wastewater",
    grepl("^idsc", df$ids_id, ignore.case = TRUE) ~ "Community",
    grepl("^ids", df$ids_id, ignore.case = TRUE) ~ "Health Care Facility",
    TRUE ~ NA_character_
  )
} else {
  df$type <- NA_character_
}

# ===============================
# Display labels
# ===============================
df$display_label <- as.character(df$subtype_and_subclade)
df <- df %>% filter(!is.na(display_label), nzchar(display_label))

# ===============================
# Create Year–Week axis
# ===============================
df$year_week <- paste0(df$year, "-W", sprintf("%02d", df$week))

df$year_week <- factor(
  df$year_week,
  levels = unique(df$year_week[order(df$year, df$week)])
)

# ===============================
# Color palette
# ===============================
unique_labels <- sort(unique(df$display_label))
color_palette <- setNames(
  viridis(length(unique_labels), option = "turbo"),
  unique_labels
)

df$display_label <- factor(df$display_label, levels = unique_labels)

# ===============================
# Aggregate data
# ===============================
df_summary <- df %>%
  add_count(
    year,
    week,
    year_week,
    type,
    display_label,
    name = "count"
  ) %>%
  distinct(
    year,
    week,
    year_week,
    type,
    display_label,
    count
  )

# ===============================
# Plot
# ===============================
p <- ggplot(
  df_summary,
  aes(x = year_week, y = count, fill = display_label)
) +
  geom_col(width = 0.7) +
  facet_grid(
    type ~ .,
    scales = "free_y",
    switch = "y"
  ) +
  scale_fill_manual(values = color_palette) +
  labs(
    x = "Epidemiological Week (Year–Week)",
    y = "Number of Detections",
    fill = "SARS-CoV-2"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      size = 8
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 8),
    legend.text  = element_text(size = 6.5),
    strip.text.y.right = element_text(face = "bold", size = 14),
    strip.placement = "outside",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

# ===============================
# Display
# ===============================
print(p)

# Ajuste final: expandir tamanho da figura

ggsave("grafico_corrigido.png", p, width = 12, height = 6)

