library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)

file_path <- "genomica_sequency.csv"
if (!file.exists(file_path)) stop(paste("File not found:", file_path))
df <- read.csv(file_path, fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)

# normalize column names to simple lowercase identifiers
orig_names <- names(df)
norm_names <- tolower(gsub("[^[:alnum:]_]", "_", orig_names))
names(df) <- norm_names

# helper to find columns
find_col <- function(patterns) {
  cols <- grep(paste(patterns, collapse="|"), names(df), ignore.case = TRUE, value = TRUE)
  if (length(cols) == 0) return(NA_character_)
  cols[1]
}

week_col <- find_col(c("^week$","^week_","^week[0-9]","\\bweek\\b"))
count_col <- find_col(c("^count$","^n$","^number$"))
ids_col <- find_col(c("^ids_id$","^ids$","^id[_]?s"))
subtype_col <- find_col(c("subtype","subclade","subtype_and_subclade","subtype_and_subclade","subtype\\.and\\.subclade"))

if (is.na(week_col)) stop("No week column found in `genomica_sequency.csv`")
if (is.na(subtype_col)) stop("No subtype/subclade column found in `genomica_sequency.csv`")
if (is.na(count_col)) {
  df$count <- NA_real_
  count_col <- "count"
}

# Convert & clean using base R (avoid .data outside dplyr masks)
df[[week_col]] <- suppressWarnings(as.integer(as.character(df[[week_col]])))
df[[count_col]] <- suppressWarnings(as.numeric(as.character(df[[count_col]])))

# keep only finite week rows
df <- df[!is.na(df[[week_col]]) & is.finite(df[[week_col]]), , drop = FALSE]

# rename canonical columns for simpler downstream code
names(df)[names(df) == week_col] <- "week"
names(df)[names(df) == count_col] <- "count"
if (!is.na(ids_col)) names(df)[names(df) == ids_col] <- "ids_id"
names(df)[names(df) == subtype_col] <- "subtype_and_subclade"

# create type from ids_id (if present)
if ("ids_id" %in% names(df)) {
  df$type <- dplyr::case_when(
    df$ids_id == "Wastewater" ~ "Wastewater",
    grepl("^idsc", df$ids_id, ignore.case = TRUE) ~ "Community",
    grepl("^ids", df$ids_id, ignore.case = TRUE) ~ "Health Care Facility",
    TRUE ~ NA_character_
  )
} else {
  df$type <- NA_character_
}

# display label and remove missing/empty
df$display_label <- as.character(df$subtype_and_subclade)
df <- df[!is.na(df$display_label) & nzchar(df$display_label), , drop = FALSE]

# palette and factor levels
unique_labels <- sort(unique(df$display_label))
if (length(unique_labels) == 0) stop("No display labels found for plotting.")
color_palette <- setNames(viridis(length(unique_labels), option = "turbo"), unique_labels)
df$display_label <- factor(df$display_label, levels = unique_labels)

# week range
week_range <- range(df$week, na.rm = TRUE)
if (!all(is.finite(week_range))) stop("No finite week values available for plotting.")

# plot
df_summary <- df %>%
  add_count(week, type, display_label, name = "count") %>%
  distinct(week, type, display_label, count)



# Intervalo de semanas
week_range <- range(df_summary$week, na.rm = TRUE)

# Criar gráfico
p <- ggplot(df_summary, aes(x = week, y = count, fill = display_label)) +
  geom_col(position = "stack", width = 0.7) +
  facet_grid(type ~ ., scales = "free_y", drop = TRUE) +
  scale_fill_manual(values = color_palette, na.value = "grey50") +
  scale_x_continuous(
    breaks = seq(week_range[1], week_range[2], by = 1),
    limits = c(week_range[1] - 0.5, week_range[2] + 0.5),
    expand = c(0, 0)
  ) +
  labs(
    x = "Epidemiological Week",
    y = "Number of Detections",
    fill = "Influenza Type (Clade)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p)
ggsave("influenza_trends_plot.png", p, width = 12, height = 8, dpi = 300)

