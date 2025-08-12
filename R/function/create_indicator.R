

cli::cli_alert_info("Lendo Baseses de dados para Dashboard")
r_Base<- rio::import(bdgeral)
BD_3_meses <- rio::import(bd3m)
BD_15_dias <- rio::import(bd30d)
BD_diaria <- rio::import(bd_diar)
BD_positivos <- rio::import(bd_pos)


#bade dos tres meses
cli::cli_alert_info("Criando indicadores para Dashboard")


#Fasendo as bases para dashboard
#Positivos_15_dias<-Positivos_15_dias_P  %>%  group_by(Provincia,Distrito) %>%  summarise(testados=sum(as.numeric(resultado_testagem=="Negativo",na.rm = T)), Positivos=sum(as.numeric(resultado_testagem=="Positivo",na.rm = T))) %>% mutate(Taxa_positividade=Positivos/testados*100)

#casos_diarios_15_dias<-Positivos_15_dias_P  %>%  group_by(Provincia,Distrito,Data_reporte) %>%  summarise(testados=sum(as.numeric(resultado_testagem=="Negativo",na.rm = T)), Positivos=sum(as.numeric(resultado_testagem=="Positivo",na.rm = T))) %>% mutate(Taxa_positividade=Positivos/testados*100) %>% arrange(., Data_reporte)
#View(casos_diarios_15_dias)
