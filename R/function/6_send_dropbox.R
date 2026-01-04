
library(rdrop2)

# Seu token Dropbox
token <- readRDS("token.rds")


token <- drop_auth(new_user = TRUE)

# Pasta no Dropbox
db_folder <- "/IDS"

# Verificar autenticação
drop_dir(db_folder, dtoken = token)


project_path <- getwd()

# Diretório local onde os arquivos estão salvos
local_dir <- file.path(project_path, "IDS_Monitoria","data", "DB_Dashboard")

# Criar lista de arquivos locais usando o caminho correto
files <- list(
  file.path(local_dir, "B_geral.rda"),
  file.path(local_dir, "B_Comunitaria.rda"),
  file.path(local_dir, "B_Hospitalar.rda"),
  file.path(local_dir, "B_Ambiental.rda")
)



for (file in files) {
  drop_upload(
    file,
    path = db_folder,
    mode = "overwrite",
    dtoken = token
  )
}

message("✔️ Upload concluído com sucesso!")




names(B_geral_HCA_R)

