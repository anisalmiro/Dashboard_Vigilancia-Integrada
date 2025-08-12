


# === Load packages ===
if (!require(dotenv)) install.packages("dotenv")
if (!require(readr)) install.packages("readr")
if (!require(fs)) install.packages("fs")

library(dotenv)
library(readr)
library(fs)

# === Load .env config ===
dotenv::load_dot_env("config/.env")
Sys.getenv("FORM_IDS")

briefcase_path <- Sys.getenv("BRIEFCASE_PATH")
storage_dir    <- Sys.getenv("STORAGE_DIR")
export_dir     <- Sys.getenv("EXPORT_DIR")
data_dir       <- Sys.getenv("DATA_DIR")
aggregate_url  <- Sys.getenv("AGGREGATE_URL")
username       <- Sys.getenv("USERNAME")
password       <- Sys.getenv("PASSWORD")

# Form IDs
form_ids <- unlist(strsplit(Sys.getenv("FORM_IDS"), ","))
form_ids <- trimws(gsub('^"|"$', '', form_ids))  # remove quotes

# === Create folders if not exist ===
dir_create(storage_dir)
dir_create(export_dir)
dir_create(data_dir)

# === Helper: Run command ===
run_cmd <- function(cmd) {
  message("➡️ Running: ", cmd)
  res <- system(cmd, intern = TRUE)
  cat(paste(res, collapse = "\n"), "\n")
}

# === Main Loop ===
for (form_id in form_ids) {
  message("\n📥 Pulling + Exporting: ", form_id)
  
  # PULL
  pull_cmd <- paste(
    "java -jar", shQuote(briefcase_path),
    "pull",
    "--storage_directory", shQuote(storage_dir),
    "--aggregate_url", shQuote(aggregate_url),
    "--form_id", shQuote(form_id),
    "--odk_username", shQuote(username),
    "--odk_password", shQuote(password)
  )
  run_cmd(pull_cmd)
  
  # EXPORT
  export_cmd <- paste(
    "java -jar", shQuote(briefcase_path),
    "export",
    "--storage_directory", shQuote(storage_dir),
    "--form_id", shQuote(form_id),
    "--export_format=CSV",
    "--export_directory", shQuote(export_dir)
  )
  run_cmd(export_cmd)
  
  # Find the exported CSV
  exported_csv <- list.files(export_dir, pattern = paste0("^", form_id, ".*\\.csv$"), full.names = TRUE)
  
  if (length(exported_csv) == 0) {
    warning("❌ No exported CSV found for ", form_id)
    next
  }
  
  # Load into R and save to /data
  data <- read_csv(exported_csv[1], show_col_types = FALSE)
  
  save_path <- file.path(data_dir, paste0(form_id, ".csv"))
  write_csv(data, save_path)
  
  message("✅ Saved: ", save_path)
}

message("\n🎉 All forms pulled, loaded to R, and saved to: ", data_dir)




