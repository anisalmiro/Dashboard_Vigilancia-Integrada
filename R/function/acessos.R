
#Exportando base para Dashboard


#Base de dados de acessos
library(RSQLite)

users_db <- data.frame(
  user = c("lab","tete","zambezia","com","hosp","amb","anisio"),
  password = c("lab@123","tete@123","zam@123","com@123","hosp@123","amb@123","anisio@123"),
  admin = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE
)

# Define the path to the SQLite database

db_path <- "users.sqlite"

# Connect to the SQLite database
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Create a table in the database
dbWriteTable(con, "credentials", users_db, overwrite = TRUE)
# write to this patth "C:/Github/IDS_API/IDS_Monitoria/data/DB_Dashboard/users.sqlite"

db_path <- "C:/Github/IDS_API/IDS_Monitoria/users.sqlite"

# Abrir ligação
con <- dbConnect(RSQLite::SQLite(), db_path)

# Gravar tabela "credentials" dentro do SQLite
dbWriteTable(con, "credentials", users_db, overwrite = TRUE)

# Disconnect from the database
dbDisconnect(con)
write.csv(users_db, file = paste0("users_db.csv"), row.names = FALSE)
cli::cli_alert_success("Base de dados de acessos criada com sucesso!")



#ACESSO PARA DROPBOX

#token <- drop_auth(
#  new_user = TRUE,
#  key = "gxmmd64forfyfql",
#  secret = "1uh0zctfpwtxqdq",
#  cache = TRUE,
#  rdstoken = NA
#) # gets credentials


