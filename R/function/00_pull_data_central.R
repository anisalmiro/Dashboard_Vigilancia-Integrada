## Setting ODK Central Access Information 

Sys.setenv(ODKC_SVC="https://estatisticas.ins.gov.mz/v1/projects/11/forms/IDS_v1.svc")
Sys.setenv(ODKC_UN="anisio.bule@ins.gov.mz") ## ADD YOUR INFO HERE
Sys.setenv(ODKC_PW="An1s1@1990$") ## ADD YOUR INFO HERE


##--run ruODK setup to load your credentials into ruODK
ruODK::ru_setup(
  svc = Sys.getenv("ODKC_SVC"),
  un = Sys.getenv("ODKC_UN"),
  pw = Sys.getenv("ODKC_PW"),
  tz = "Africa/Maputo",
  verbose = TRUE
)

##--manually create a folder in your R project called 'raw' and link to it
loc <- here::here("raw", "ids_central")

##--list the 'repeat groups' available in the submitted data
fq_svc <- ruODK::odata_service_get()
fq_svc %>% knitr::kable(.)
##--read in the data from ODK Central, including repeat groups



bd_ids_combinada <- ruODK::odata_submission_get(
  table = fq_svc$name[1],
  local_dir = loc,
  wkt=TRUE)


# save to file.path(dir_raw, "bd_ids_combinada.rds")
write_rds(bd_ids_combinada, file.path(dir_raw, "bd_ids_combinada.rds"))

write_rds(bd_ids_combinada, file.path(dir_raw, paste0("bd_ids_combinada_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")))

##-- 3. Asignar nombres a la lista para fácil acceso
#names(bd_ids_list) <- fq_svc$name

##-- 4. Extraer la tabla principal (generalmente la primera)
#bd_ids_combinada <- bd_ids_list[[1]]

##-- Ejemplo: Si tienes un repeat group llamado "mi_grupo_repetido"
# puedes acceder a él con: bd_ids_list$mi_grupo_repetido


