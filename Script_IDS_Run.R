


# === CONFIGURATION ===


briefcase_path <- "ODK-Briefcase-v1.18.0.jar"             # Path to ODK Briefcase JAR
storage_dir <- "data"            # Local storage for forms
export_dir <- "ODK Briefcase Storage"                 # Where CSVs will be saved
aggregate_url <- "https://inqueritos.ins.gov.mz/inqueritos" # Your ODK Aggregate URL
username <- "Anisio"
password <- "Anisio@2022"

# List of form IDs to download and export
form_ids <- c("hospitalar", "hospitalar_v1", "hospitalar_ib", "hospitalar_LB", "Comunidade", "VIGILÂNCIA AMBIENTAL")

# === SETUP DIRECTORIES ===
if (!dir.exists(storage_dir)) dir.create(storage_dir)
if (!dir.exists(export_dir)) dir.create(export_dir)

# === FUNCTION TO RUN SYSTEM COMMAND ===
run_cmd <- function(cmd) {
  message("Running: ", cmd)
  system(cmd)
}

# === LOOP THROUGH FORMS ===
for (form_id in form_ids) {
  message("Processing form: ", form_id)
  
  # PULL command
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
  
  # EXPORT command
  export_cmd <- paste(
    "java -jar", shQuote(briefcase_path),
    "export",
    "--storage_directory", shQuote(storage_dir),
    "--form_id", shQuote(form_id),
    "--export_format=CSV",
    "--export_directory", shQuote(export_dir)
  )
  run_cmd(export_cmd)
}

message("✅ All forms processed and exported.")
