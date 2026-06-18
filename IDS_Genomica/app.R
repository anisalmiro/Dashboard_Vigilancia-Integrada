# app.R -------------------------------------------------------------
# Dashboard clássico (shinydashboard) - Versão reorganizada e responsiva
# Autor: Gerado por ChatGPT (Anisio Bule - adaptação)
# ------------------------------------------------------------------



# libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(htmltools)
library(htmlwidgets)
library(shinythemes)
library(shinymanager)   # mantido (secure_app)
library(dplyr)
library(reactable)
library(reactablefmtr)
library(highcharter)
library(rpivotTable)
library(googleVis)
library(plotly)
library(stringr)
library(ggplot2)
library(plyr)
library(cli)
library(readr)
library(tidyr)
library(here)
library(lubridate)
library(fontawesome)
library(rsconnect) 
library(sf)
library(leaflet)


#rsconnect::deployApp(
 # appFiles = c("app.R", "www/","data")
#)


# Instalar rdrop2 a partir do GitHub
#remotes::install_github("karthik/rdrop2")


Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")


# ---------------- Carregar bases locais (se já baixadas) --------------

load(file = 'data/DB_Dashboard/B_HCAR.rda')        # B_geral_HCA_R
load(file = 'data/DB_Dashboard/BD_Genomica_Final.rda')  # BD_Final_VH_R
load(file = 'data/DB_Dashboard/mapa_dados_influenza_sarsc.rda')  # BD_genomica_lined


ultima_data_reporte <- B_geral_HCA_R %>%
  dplyr::mutate(
    DATA2 = suppressWarnings(
      lubridate::parse_date_time(`DATE2`,
                                 orders = c("ymd", "dmy", "mdy", "Ymd", "dmY"))
    ) %>% as.Date()
  ) %>%
  dplyr::summarise(
    ULTIMA_DATA = max(DATA2, na.rm = TRUE)
  ) %>%
  dplyr::pull(ULTIMA_DATA)


# ---------------- UI ---------------------------------------------
# --- helpers simples (ex.: ultima data) ---------------------------
ultima_data <- tryCatch({
  ultima_data_reporte
}, error = function(e) NA)

# --- UI interno (dashboardPage) -------------------------------------
ui_inner <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = span(img(src = "INS.png", height = "40px"), "VIGILÂNCIA GENÔMICA"),
    titleWidth = 420
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Sindrome Respiratorio", tabName = "sfb", icon = icon("lungs-virus")),
      menuItem("Síndrome Diarreica", tabName = "diarrheaT", icon = icon("disease"))
    )
  ),
  dashboardBody(
    # meta viewport para mobile / responsividade
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      # DataTables Buttons CSS/JS (garante que os botões apareçam)
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/buttons/2.4.1/css/buttons.dataTables.min.css"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/dataTables.buttons.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/pdfmake.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/vfs_fonts.js"),
      tags$script(src = "https://cdn.datatables.net/buttons/2.4.1/js/buttons.html5.min.js"),
      # CSS custom para garantir largura e alinhamentos
      tags$style(HTML("
        /* Full width container */
        .content { max-width: 1400px; margin: 0 auto; }
        /* Make boxes fit nicely and be responsive */
        .box { margin-bottom: 15px; }
        /* Download button spacing */
        .download-btn { margin-bottom: 10px; display: inline-block; }
        /* Ensure DataTables fill container */
        .dataTables_wrapper { width: 100% !important; }
        /* Align panel titles */
        .box-title { font-size: 16px; font-weight: 600; }
        /* Make nav tabs good-looking within boxes */
        .nav-tabs-custom .nav > li > a { color: #00688B; }
      "))
    ),
    
    tabItems(
      # ---------------- Dashboard principal ----------------
      tabItem(tabName = "sfb",
              
              tabBox(width = 12, id = "dashboard_tabs",
                     fluidRow(
                       column(width = 3,
                              p(strong("Última atualização:")),
                              p(as.character(ultima_data))
                       )),
                  
                     # ---- Tab 1: Indicadores e Filtros ----
                     tabPanel("Positividade",
                              # ---- Tab 1: Indicadores e Filtros ----
                              fluidRow(
                                box(
                                  width = 12,
                                  title = "Alertas",
                                  status = "danger",
                                  solidHeader = TRUE,
                                  column(width = 12,
                                         
                                         column(
                                           width = 3,
                                           valueBoxOutput("alert_sars", width = 6),
                                           valueBoxOutput("alert_inf", width = 6)
                                         ),
                                         
                                         br(),
                                         
                                         highchartOutput("tend_alerta", height = "300px")
                                         
                                  )
                                )
                              ),
                              
                              fluidRow(
                                box(width = 12, status = "primary", solidHeader = TRUE,
                                    title = tagList(icon("chart-line"), "Resumo"),
                                    fluidRow(
                                      column(width = 12,
                                             div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                                 "Influenza Positivity Surveillance"
                                             ),
                                            
                                             br(),
                                             
                                             plotOutput("tend_casos_positivos_inf", height = "600px")
                                            
                                             
                                        )
                                      ),
                                    fluidRow(
                                      column(width = 12,
                                             div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                                 "SARS-CoV-2 Positivity Surveillance"
                                             ),
                                             
                                             br(),
                                             
                                             plotOutput("tend_casos_positivos_sarsc", height = "600px")
                                             
                                      )
                                    )
                                )
                              )
                     ),
                     
                     # ---- Tab 2: Influenza Surveillance ----
                     tabPanel("Influenza",
                              div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                  "Sequency Influenza Surveillance"
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       box(title = "Health Care Facility", width = 12,
                                           highchartOutput("graf_influenza_hospitalar", height = "300px")),
                                       box(title = "Community", width = 12,
                                           highchartOutput("graf_influenza_comunitaria", height = "300px")),
                                       box(title = "Wastewater", width = 12,
                                           highchartOutput("graf_influenza_ambiental", height = "600px"))
                                )
                              )
                     ),
                     
                     # ---- Tab 3: SARS-CoV-2 Surveillance ----
                     tabPanel("SARS-CoV-2",
                              div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                  "Sequency SARS-CoV-2 Surveillance"
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       box(title = "Health Care Facility", width = 12,
                                           highchartOutput("graf_sars_Cov2_hospitalar", height = "300px")),
                                       box(title = "Community", width = 12,
                                           highchartOutput("graf_sars_Cov2_comunitaria", height = "300px")),
                                       box(title = "Wastewater", width = 12,
                                           highchartOutput("graf_sars_Cov2_ambiental", height = "600px"))
                                )
                              )
                     ),
                     
                     tabPanel("Mapas",
                              div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                  "Proveniencia de casos de Influenza"
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       box( width = 12,
                                            leafletOutput(
                                              outputId = "mapa_influenza",
                                              height = "700px"
                                            )
                                          
                                            )
                                    
                                )
                              ),
                              div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                  "Proveniencia de casos de Sarscov2"
                              ),
                              fluidRow(
                                column(width = 12,
                                       
                                       box( width = 12,
                                           
                                            leafletOutput(
                                              outputId = "mapa_sarscov2",
                                              height = "700px"
                                            ))
                                )
                              )
                     )
                     
                     
              )
      ),
      
      

      tabItem(tabName ="diarrheaT",
               div(
                 style = "
      background-color:#005c99;color:white;padding:12px;
      margin-bottom:15px;border-radius:6px;
      font-weight:bold;font-size:20px;",
                 "Proveniência de casos de Cólera"
               ),
               
               fluidRow(
                 column(
                   width = 12,
                   box(
                     width = 12,
                     highchartOutput("coleumap", height = "500px")
                   )
                 )
               )
      )
    )
  )
) 

ui <- ui_inner

# ---------------- SERVER ---------------------------------------------
server <- function(input, output, session) {
 # base positividade

  #====================#
  output$tend_casos_positivos_inf <- renderPlot({
    
    #============================================================#
    # 1. SELEÇÃO E PREPARAÇÃO DA BASE
    #============================================================#
    bd_conjunta <- B_geral_HCA_R %>%
      dplyr::select(
        codigo_paciente = `Dados_demograficos:codigo_paciente`,
        influenza = `TIFOIDE:Resultado_de_Influenza`,
        sars_cov_2 = `group_jz9ln80:SARSCov2`,
        date2 = `DATE2`
      ) %>%
      dplyr::mutate(
        date2 = lubridate::parse_date_time(
          date2,
          orders = c("Ymd","Y-m-d","dmY","d/m/Y","mdY","m/d/Y","Ymd HMS","dmY HMS")
        ),
        week = lubridate::week(date2)
      )
    
    raw <- bd_conjunta %>%
      dplyr::select(codigo_paciente, influenza, sars_cov_2, week, date2)
    
    #============================================================#
    # 2. NORMALIZAR NOMES DAS COLUNAS
    #============================================================#
    colnames(raw) <- tolower(colnames(raw))
    
    #============================================================#
    # 3. FUNÇÃO PARA ENCONTRAR COLUNAS
    #============================================================#
    find_col <- function(possible_names) {
      found <- intersect(possible_names, colnames(raw))
      if (length(found) == 0) NA_character_ else found[1]
    }
    
    col_id        <- find_col(c("codigo_paciente","codigo","id"))
    col_week      <- find_col(c("week"))
    col_influenza <- find_col(c("influenza"))
    col_sars      <- find_col(c("sars_cov_2","sarscov2","sars"))
    
    if (any(is.na(c(col_id, col_week, col_influenza, col_sars)))) {
      stop("❌ Colunas obrigatórias em falta")
    }
    
    #============================================================#
    # 4. RENOMEAR COLUNAS (FORMA CORRETA)
    #============================================================#
    df <- raw %>%
      dplyr::rename(
        codigo_paciente = all_of(col_id),
        week            = all_of(col_week),
        influenza       = all_of(col_influenza),
        sars_cov_2      = all_of(col_sars)
      )
    
    #============================================================#
    # 5. CRIAR VARIÁVEIS DE TEMPO
    #============================================================#
    df <- df %>%
      dplyr::mutate(
        week_num = as.integer(week),
        data_evento = as.Date(date2),
        ano = lubridate::year(data_evento),
        ano_semana = paste0(
          ano, "_W",
          stringr::str_pad(week_num, 2, pad = "0")
        )
      )
    
    #============================================================#
    # 6. DEFINIR CONTEXTO DE VIGILÂNCIA
    #============================================================#
    df <- df %>%
      dplyr::mutate(
        codigo_trim = toupper(trimws(codigo_paciente)),
        setting = dplyr::case_when(
          stringr::str_detect(codigo_trim, "^[IL]DSW") ~ "Wastewater",
          stringr::str_detect(codigo_trim, "^IDSC")    ~ "Community",
          stringr::str_detect(codigo_trim, "^IDS")     ~ "Health Care Facility",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(setting))
    
    #============================================================#
    # 7. LIMPAR RESULTADOS LABORATORIAIS
    #============================================================#
    clean_result <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x[x %in% c("", "na", "n/a", "-")] <- NA
      x
    }
    
    df <- df %>%
      dplyr::mutate(
        influenza_clean = clean_result(influenza),
        sars_clean      = clean_result(sars_cov_2)
      )
    
    #============================================================#
    # 8. FUNÇÃO PARA CALCULAR POSITIVIDADE
    #============================================================#
    compute_positivity <- function(df, result_col) {
      df %>%
        dplyr::filter(!is.na(.data[[result_col]])) %>%
        dplyr::group_by(setting, ano_semana) %>%
        dplyr::summarise(
          tests = dplyr::n(),
          positives = sum(.data[[result_col]] == "positivo"),
          positivity = ifelse(tests > 0, 100 * positives / tests, 0),
          .groups = "drop"
        )
    }
    
    influenza_pos <- compute_positivity(df, "influenza_clean")
    
    #============================================================#
    # 9. REGRA ESPECIAL WASTEWATER
    #============================================================#
    influenza_pos <- influenza_pos %>%
      dplyr::mutate(
        positivity = ifelse(setting == "Wastewater" & positives > 0, 100, positivity)
      )
    
    #============================================================#
    # 10. GRÁFICO FINAL (RETORNO DO renderPlot)
    #============================================================#
    ggplot(
      influenza_pos,
      aes(x = ano_semana, y = positivity, color = setting, group = setting)
    ) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      facet_grid(setting ~ .) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(
        title = "Influenza – Positivity Surveillance",
        x = "Epidemiological Week / Year",
        y = "Positivity Rate (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  
  
  output$tend_casos_positivos_sarsc <- renderPlot({
    
    #============================================================#
    # 1. SELEÇÃO E PREPARAÇÃO DA BASE
    #============================================================#
    bd_conjunta <- B_geral_HCA_R %>%
      dplyr::select(
        codigo_paciente = `Dados_demograficos:codigo_paciente`,
        influenza = `TIFOIDE:Resultado_de_Influenza`,
        sars_cov_2 = `group_jz9ln80:SARSCov2`,
        date2 = `DATE2`
      ) %>%
      dplyr::mutate(
        date2 = lubridate::parse_date_time(
          date2,
          orders = c("Ymd","Y-m-d","dmY","d/m/Y","mdY","m/d/Y","Ymd HMS","dmY HMS")
        ),
        week = lubridate::week(date2)
      )
    
    raw <- bd_conjunta %>%
      dplyr::select(codigo_paciente, influenza, sars_cov_2, week, date2)
    
    colnames(raw) <- tolower(colnames(raw))
    
    #============================================================#
    # 2. FUNÇÃO PARA ENCONTRAR COLUNAS
    #============================================================#
    find_col <- function(possible_names) {
      found <- intersect(possible_names, colnames(raw))
      if (length(found) == 0) NA_character_ else found[1]
    }
    
    col_id   <- find_col(c("codigo_paciente","codigo","id"))
    col_week <- find_col(c("week"))
    col_sars <- find_col(c("sars_cov_2","sarscov2","sars"))
    
    if (any(is.na(c(col_id, col_week, col_sars)))) {
      stop("❌ Colunas obrigatórias em falta para SARS-CoV-2")
    }
    
    #============================================================#
    # 3. RENOMEAR COLUNAS
    #============================================================#
    df <- raw %>%
      dplyr::rename(
        codigo_paciente = all_of(col_id),
        week            = all_of(col_week),
        sars_cov_2      = all_of(col_sars)
      )
    
    #============================================================#
    # 4. VARIÁVEIS DE TEMPO
    #============================================================#
    df <- df %>%
      dplyr::mutate(
        week_num = as.integer(week),
        data_evento = as.Date(date2),
        ano = lubridate::year(data_evento),
        ano_semana = paste0(
          ano, "_W",
          stringr::str_pad(week_num, 2, pad = "0")
        )
      )
    
    #============================================================#
    # 5. DEFINIR CONTEXTO DE VIGILÂNCIA
    #============================================================#
    df <- df %>%
      dplyr::mutate(
        codigo_trim = toupper(trimws(codigo_paciente)),
        setting = dplyr::case_when(
          stringr::str_detect(codigo_trim, "^[IL]DSW") ~ "Wastewater",
          stringr::str_detect(codigo_trim, "^IDSC")    ~ "Community",
          stringr::str_detect(codigo_trim, "^IDS")     ~ "Health Care Facility",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(setting))
    
    #============================================================#
    # 6. LIMPAR RESULTADOS SARS-CoV-2
    #============================================================#
    clean_result <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x[x %in% c("", "na", "n/a", "-")] <- NA
      x
    }
    
    df <- df %>%
      dplyr::mutate(
        sars_clean = clean_result(sars_cov_2)
      )
    
    #============================================================#
    # 7. CALCULAR POSITIVIDADE SARS-CoV-2
    #============================================================#
    sars_pos <- df %>%
      dplyr::filter(!is.na(sars_clean)) %>%
      dplyr::group_by(setting, ano_semana) %>%
      dplyr::summarise(
        tests = dplyr::n(),
        positives = sum(sars_clean == "positivo"),
        positivity = ifelse(tests > 0, 100 * positives / tests, 0),
        .groups = "drop"
      )
    
    #============================================================#
    # 8. REGRA ESPECIAL – WASTEWATER
    #============================================================#
    sars_pos <- sars_pos %>%
      dplyr::mutate(
        positivity = ifelse(setting == "Wastewater" & positives > 0, 100, positivity)
      )
    
    #============================================================#
    # 9. GRÁFICO FINAL (RETORNO)
    #============================================================#
    ggplot(
      sars_pos,
      aes(x = ano_semana, y = positivity, color = setting, group = setting)
    ) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      facet_grid(setting ~ .) +
      scale_y_continuous(limits = c(0, 100)) +
      labs(
        title = "SARS-CoV-2 – Positivity Surveillance",
        x = "Epidemiological Week / Year",
        y = "Positivity Rate (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
  })
  

#Fim dos graficos de positividade
  
  output$graf_influenza_hospitalar <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        tolower(as.character(Patogeno)) == "influenza",
        as.character(vigilancia) == "Hospitalar"
      )
    
    if (nrow(df) == 0) {
      return(
        highchart() %>%
          hc_title(text = "No influenza genomic records for Hospitalar")
      )
    }
    
    # Contagem simples por semana e subtipo
    df_count <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(
        semana,
        `Subtype.Lineage.Clade.and.Pagolin.`
      ) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Influenza genomic lineages – Hospitalar") %>%
      hc_xAxis(categories = df_count$semana) %>%
      hc_yAxis(
        title = list(text = "Number of samples"),
        allowDecimals = FALSE
      ) %>%
      hc_plotOptions(
        column = list(stacking = "normal")
      )
    
    # Adicionar séries (valores absolutos)
    for (col in setdiff(names(df_count), "semana")) {
      hc <- hc %>% hc_add_series(
        name = col,
        data = df_count[[col]]
      )
    }
    
    hc %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}</b>: {point.y}<br/>"
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  output$graf_influenza_comunitaria <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        tolower(as.character(Patogeno)) == "influenza",
        as.character(vigilancia) == "Comunitaria"
      )
    
    if (nrow(df) == 0) {
      return(
        highchart() %>%
          hc_title(text = "No influenza genomic records for Comunitaria")
      )
    }
    
    # Contagem absoluta por semana e subtipo
    df_count <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(
        semana,
        `Subtype.Lineage.Clade.and.Pagolin.`
      ) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Influenza genomic lineages – Comunitaria") %>%
      hc_xAxis(categories = df_count$semana) %>%
      hc_yAxis(
        title = list(text = "Number of samples"),
        allowDecimals = FALSE
      ) %>%
      hc_plotOptions(
        column = list(stacking = "normal")
      )
    
    for (col in setdiff(names(df_count), "semana")) {
      hc <- hc %>% hc_add_series(
        name = col,
        data = df_count[[col]]
      )
    }
    
    hc %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}</b>: {point.y}<br/>"
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  output$graf_influenza_ambiental <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        tolower(as.character(Patogeno)) == "influenza",
        as.character(vigilancia) == "Wastwater"
      )
    
    if (nrow(df) == 0) {
      return(highchart() %>%
               hc_title(text = "No influenza genomic records for Wastewater"))
    }
    
    df_prop <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(semana, `Subtype.Lineage.Clade.and.Pagolin.`) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::mutate(
        total = rowSums(dplyr::across(-semana)),
        dplyr::across(-c(semana, total), ~ ifelse(total > 0, .x / total * 100, 0))
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Influenza genomic lineages – Proportion (%) (Wastewater)") %>%
      hc_xAxis(categories = df_prop$semana) %>%
      hc_yAxis(max = 100, labels = list(format = "{value}%")) %>%
      hc_plotOptions(column = list(stacking = "normal"))
    
    for (col in setdiff(names(df_prop), c("semana", "total"))) {
      hc <- hc %>% hc_add_series(name = col, data = round(df_prop[[col]], 1))
    }
    
    hc %>% hc_tooltip(shared = TRUE) %>% hc_exporting(enabled = TRUE)
  })
  
  
  
  
  #Sars-cov_2
  #sARSCov2_h_lab, sARSCov2_com_lab, sARSCov2_w_lab
  
  output$graf_sars_Cov2_hospitalar <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        Patogeno == "Sars_C0v2",
        as.character(vigilancia) == "Hospitalar"
      )
    
    if (nrow(df) == 0) {
      return(
        highchart() %>%
          hc_title(text = "No Sars_C0v2 genomic records for Hospitalar")
      )
    }
    
    # Contagem absoluta por semana e subtipo
    df_count <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(
        semana,
        `Subtype.Lineage.Clade.and.Pagolin.`
      ) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Sars_C0v2 genomic lineages – Hospitalar") %>%
      hc_xAxis(categories = df_count$semana) %>%
      hc_yAxis(
        title = list(text = "Number of samples"),
        allowDecimals = FALSE
      ) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_legend(
        align = "right",
        verticalAlign = "middle",
        layout = "vertical",
        maxHeight = 300
      )
    
    # Adicionar séries (valores absolutos)
    for (col in setdiff(names(df_count), "semana")) {
      hc <- hc %>% hc_add_series(
        name = col,
        data = df_count[[col]]
      )
    }
    
    hc %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}</b>: {point.y}<br/>"
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  #Sars-Cov2 comunitária
  
  output$graf_sars_Cov2_comunitaria <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        Patogeno == "Sars_C0v2",
        as.character(vigilancia) == "Comunitaria"
      )
    
    if (nrow(df) == 0) {
      return(
        highchart() %>%
          hc_title(text = "No Sars_C0v2 genomic records for Comunitaria")
      )
    }
    
    # Contagem absoluta por semana e subtipo
    df_count <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(
        semana,
        `Subtype.Lineage.Clade.and.Pagolin.`
      ) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Sars_C0v2 genomic lineages – Comunitaria") %>%
      hc_xAxis(categories = df_count$semana) %>%
      hc_yAxis(
        title = list(text = "Number of samples"),
        allowDecimals = FALSE
      ) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_legend(
        align = "right",
        verticalAlign = "middle",
        layout = "vertical",
        maxHeight = 300
      )
    
    # Adicionar séries (valores absolutos)
    for (col in setdiff(names(df_count), "semana")) {
      hc <- hc %>% hc_add_series(
        name = col,
        data = df_count[[col]]
      )
    }
    
    hc %>%
      hc_tooltip(
        shared = TRUE,
        pointFormat = "<b>{series.name}</b>: {point.y}<br/>"
      ) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  #######_---------------------------------- GRAFICOSARS-COV2 AMBIENTAL ----------------------------------
  
  output$graf_sars_Cov2_ambiental <- renderHighchart({
    
    df <- BD_genomica_lined %>%
      dplyr::filter(
        Patogeno == "Sars_C0v2",
        as.character(vigilancia) == "Wastwater"
      )
    
    if (nrow(df) == 0) {
      return(highchart() %>%
               hc_title(text = "No Sars_C0v2 genomic records for Wastwater"))
    }
    
    df_prop <- df %>%
      dplyr::mutate(
        semana = paste0(Ano, "-W", sprintf("%02d", as.integer(week)))
      ) %>%
      dplyr::count(
        semana,
        `Subtype.Lineage.Clade.and.Pagolin.`
      ) %>%
      tidyr::pivot_wider(
        names_from = `Subtype.Lineage.Clade.and.Pagolin.`,
        values_from = n,
        values_fill = 0
      ) %>%
      dplyr::mutate(
        total = rowSums(dplyr::across(-semana)),
        dplyr::across(-c(semana, total), ~ ifelse(total > 0, .x / total * 100, 0))
      ) %>%
      dplyr::arrange(semana)
    
    hc <- highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Sars_C0v2 genomic lineages – Proportion (%) (Wastwater)") %>%
      hc_xAxis(categories = df_prop$semana) %>%
      hc_yAxis(
        max = 100,
        labels = list(format = "{value}%"),
        title = list(text = "Proportion (%)")
      ) %>%
      hc_plotOptions(column = list(stacking = "normal"))
    
    
    
    for (col in setdiff(names(df_prop), c("semana", "total"))) {
      hc <- hc %>% hc_add_series(
        name = col,
        data = round(df_prop[[col]], 1)
      )
    }

    
    hc %>%
      hc_tooltip(shared = TRUE,
                 pointFormat = "<b>{series.name}</b>: {point.y:.1f}%<br/>") %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  # Função para calcular centróides ou pontos dentro do polígono
  get_centroids <- function(sf_data, method = c("centroid", "point_on_surface")) {
    method <- match.arg(method)
    
    if(method == "centroid") {
      return(st_centroid(sf_data))
    } else if(method == "point_on_surface") {
      return(st_point_on_surface(sf_data))
    }
  }
  
  
  ########## --------- Grafico de mapa de influenza -------------
  
  # Render do mapa
  output$mapa_influenza <- renderLeaflet({
    
    # =========================================================
    # 1. PREPARAÇÃO DOS DADOS
    # =========================================================
    mapa_dados2 <- mapa_dados %>%
      mutate(
        classe_testados = ifelse(
          classe_testados %in% c("0", "0 testados"),
          NA,
          classe_testados
        )
      )
    
    # =========================================================
    # 2. CENTRÓIDES
    # =========================================================
    centroides_pos <- get_centroids(
      mapa_dados2,
      method = "point_on_surface"
    ) %>%
      filter(!is.na(positivos_influenza) & positivos_influenza > 0)
    
    centroides_labels <- get_centroids(
      mapa_dados2,
      method = "point_on_surface"
    )
    
    # =========================================================
    # 3. PALETA DE CORES – TESTADOS (POLÍGONOS)
    # =========================================================
    pal_testados <- colorFactor(
      palette = c(
        "01 - 25"  = "#f2e6f5",
        "26 - 50" = "#d7a1b0",
        "51 - 75" = "#a25364",
        "> 75"    = "#6b0000"
      ),
      levels = c("01 - 25", "26 - 50", "51 - 75", "> 75"),
      domain = mapa_dados2$classe_testados,
      na.color = "#f0f0f0"
    )
    
    # =========================================================
    # 4. FUNÇÃO PARA TAMANHO DOS PONTOS (POSITIVOS)
    # =========================================================
    raio_pos <- function(x) {
      dplyr::case_when(
        x < 05   ~ 04,
        x < 15  ~ 06,
        x < 30  ~ 08,
        TRUE    ~ 11
      )
    }
    
    # =========================================================
    # 5. MAPA LEAFLET
    # =========================================================
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # ---------------------------------------------------------
    # POLÍGONOS – TESTADOS
    # ---------------------------------------------------------
    addPolygons(
      data = mapa_dados2,
      fillColor = ~pal_testados(classe_testados),
      fillOpacity = 0.75,
      color = "grey40",
      weight = 0.6,
      label = ~paste0(
        "<b>Bairro:</b> ", bairro_geo, "<br>",
        "<b>Classe de testados:</b> ",
        ifelse(is.na(classe_testados), "Sem dados", classe_testados)
      ),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#000000",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
      
      # ---------------------------------------------------------
    # LABELS FIXOS – NOMES DOS BAIRROS
    # ---------------------------------------------------------
    addLabelOnlyMarkers(
      data = centroides_labels,
      label = ~bairro_geo,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list(
          "font-size"   = "10px",
          "font-weight" = "bold",
          "color"       = "#2b2b2b",
          "text-shadow" = "1px 1px 2px white"
        )
      )
    ) %>%
      
      # ---------------------------------------------------------
    # PONTOS – POSITIVOS INFLUENZA
    # ---------------------------------------------------------
    addCircleMarkers(
      data = centroides_pos,
      radius = ~raio_pos(positivos_influenza),
      color = "darkgreen",
      fillColor = "darkgreen",
      fillOpacity = 0.85,
      stroke = FALSE,
      label = ~paste0(
        "<b>Bairro:</b> ", bairro_geo, "<br>",
        "<b>Positivos Influenza:</b> ", positivos_influenza
      )
    ) %>%
      
      # ---------------------------------------------------------
    # LEGENDA – TESTADOS
    # ---------------------------------------------------------
    addLegend(
      position = "bottomleft",
      pal = pal_testados,
      values = mapa_dados2$classe_testados,
      title = htmltools::HTML(
        "<b>Total de testados</b><br>
         <span style='font-size:11px'>(por bairro)</span>"
      ),
      opacity = 1,
      na.label = "Sem dados"
    ) %>%
      
      # ---------------------------------------------------------
    # LEGENDA – POSITIVOS INFLUENZA
    # ---------------------------------------------------------
    addLegend(
      position = "bottomright",
      colors = rep("darkgreen", 4),
      labels = c("1–4", "5–14", "15–29", "≥30"),
      title = htmltools::HTML(
        "<b>Casos positivos</b><br>
         <span style='font-size:11px'>Influenza</span>"
      ),
      opacity = 0.9
    )%>%
      addControl(
        html = htmltools::HTML("
      <div style='
        background: rgba(255,255,255,0.9);
        padding: 10px 14px;
        border-radius: 6px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.3);
        text-align: center;
      '>
        <div style='font-size:16px; font-weight:bold;'>
          Distribuição Espacial dos Casos de Influenza
        </div>
        <div style='font-size:13px; color:#555;'>
          Cidade e Província de Maputo
        </div>
        <div style='font-size:11px; color:#777; margin-top:4px;'>
          Fonte: IDS
        </div>
      </div>
    "),
        position = "topright"
      )
    
    
    
  })
  
  
  
  
  ######## ----------- Gra

  
  output$mapa_sarscov2 <- renderLeaflet({
    
    # =========================================================
    # 1. PREPARAÇÃO DOS DADOS
    # =========================================================
    mapa_dados2 <- mapa_dados %>%
      mutate(
        classe_testados = ifelse(
          classe_testados %in% c("0", "0 testados"),
          NA,
          classe_testados
        )
      )
    
    # =========================================================
    # 2. CENTRÓIDES
    # =========================================================
    centroides_pos <- get_centroids(mapa_dados2) %>%
      filter(!is.na(positivos_sarscov2) & positivos_sarscov2 > 0)
    
    centroides_labels <- get_centroids(mapa_dados2)
    
    # =========================================================
    # 3. PALETA DE CORES – TESTADOS (POLÍGONOS)
    # =========================================================
    pal_testados <- colorFactor(
      palette = c(
        "01 - 25"  = "#edf8e9",
        "26 - 50" = "#bae4b3",
        "51 - 75" = "#74c476",
        "> 75"    = "#238b45"
      ),
      levels = c("01 - 25", "26 - 50", "51 - 75", "> 75"),
      domain = mapa_dados2$classe_testados,
      na.color = "#f0f0f0"
    )
    
    # =========================================================
    # 4. FUNÇÃO PARA RAIO DOS POSITIVOS
    # =========================================================
    raio_pos <- function(x) {
      dplyr::case_when(
        x < 05   ~ 04,
        x < 10  ~ 06,
        x < 25  ~ 08,
        x < 50  ~ 10,
        TRUE    ~ 12
      )
    }
    
    # =========================================================
    # 5. MAPA LEAFLET
    # =========================================================
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      # ---------------------------------------------------------
    # POLÍGONOS – TESTADOS
    # ---------------------------------------------------------
    addPolygons(
      data = mapa_dados2,
      fillColor = ~pal_testados(classe_testados),
      fillOpacity = 0.75,
      color = "#525252",
      weight = 0.6,
      label = ~paste0(
        "<b>Bairro:</b> ", bairro_geo, "<br>",
        "<b>Classe de testados:</b> ",
        ifelse(is.na(classe_testados), "Sem dados", classe_testados)
      ),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#000000",
        fillOpacity = 0.9,
        bringToFront = TRUE
      )
    ) %>%
      
      # ---------------------------------------------------------
    # LABELS FIXOS – NOMES DOS BAIRROS
    # ---------------------------------------------------------
    addLabelOnlyMarkers(
      data = centroides_labels,
      label = ~bairro_geo,
      labelOptions = labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        style = list(
          "font-size"   = "10px",
          "font-weight" = "bold",
          "color"       = "#2b2b2b",
          "text-shadow" = "1px 1px 2px white"
        )
      )
    ) %>%
      
      # ---------------------------------------------------------
    # PONTOS – POSITIVOS SARS-CoV-2
    # ---------------------------------------------------------
    addCircleMarkers(
      data = centroides_pos,
      radius = ~raio_pos(positivos_sarscov2),
      color = "#b30000",
      fillColor = "#b30000",
      fillOpacity = 0.85,
      stroke = FALSE,
      label = ~paste0(
        "<b>Bairro:</b> ", bairro_geo, "<br>",
        "<b>Positivos SARS-CoV-2:</b> ", positivos_sarscov2
      )
    ) %>%
      
      # ---------------------------------------------------------
    # LEGENDA – TESTADOS
    # ---------------------------------------------------------
    addLegend(
      position = "bottomleft",
      pal = pal_testados,
      values = mapa_dados2$classe_testados,
      title = htmltools::HTML(
        "<b>Total de testados</b><br>
         <span style='font-size:11px'>(por bairro)</span>"
      ),
      opacity = 1,
      na.label = "Sem dados"
    ) %>%
      
      
      # ---------------------------------------------------------
    # LEGENDA – POSITIVOS
    # ---------------------------------------------------------
    addLegend(
      position = "bottomright",
      colors = rep("#b30000", 5),
      labels = c("1–4", "5–9", "10–24", "25–49", "≥50"),
      title = htmltools::HTML(
        "<b>Casos positivos</b><br>
         <span style='font-size:11px'>SARS-CoV-2</span>"
      ),
      opacity = 0.9
    ) %>%
      addControl(
        html = htmltools::HTML("
      <div style='
        background: rgba(255,255,255,0.9);
        padding: 10px 14px;
        border-radius: 6px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.3);
        text-align: center;
      '>
        <div style='font-size:16px; font-weight:bold;'>
          Distribuição Espacial dos Casos de SARS-CoV-2
        </div>
        <div style='font-size:13px; color:#555;'>
          Cidade e Província de Maputo
        </div>
        <div style='font-size:11px; color:#777; margin-top:4px;'>
          Fonte: IDS
        </div>
      </div>
    "),
        position = "topright"
      )
    
  })
  
  
  
  alertas <- reactiveValues(
    nivel_sars = NA,
    tend_sars  = NA,
    nivel_inf  = NA,
    tend_inf   = NA
  )
  
  
  
  output$tend_alerta <- renderHighchart({
    
    # ---------------- PREPARAÇÃO DOS DADOS ----------------
    df_f <- B_geral_HCA_R %>%
      dplyr::mutate(
        DATA2 = suppressWarnings(
          lubridate::parse_date_time(
            `DATE2`,
            orders = c("ymd", "dmy", "mdy", "Ymd", "dmY")
          )
        ) %>% as.Date()
      ) %>%
      dplyr::filter(!is.na(DATA2)) %>%
      dplyr::mutate(
        ano = lubridate::year(DATA2),
        sem_epi = lubridate::epiweek(DATA2),
        influenza_pos = ifelse(`TIFOIDE:Resultado_de_Influenza` == "positivo", 1, 0),
        sarscov2_pos  = ifelse(`group_jz9ln80:SARSCov2` == "positivo", 1, 0)
      ) %>%
      dplyr::group_by(ano, sem_epi) %>%
      dplyr::summarise(
        total_testados = n(),
        Influenza = sum(influenza_pos, na.rm = TRUE),
        SARSCoV2  = sum(sarscov2_pos, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        taxa_influenza = round((Influenza / total_testados) * 100, 1),
        taxa_sarscov2  = round((SARSCoV2  / total_testados) * 100, 1)
      ) %>%
      dplyr::arrange(ano, sem_epi)
    
    #list(head(df_f))
    
    req(nrow(df_f) >= 2)
    
    # ---------------- ALERTAS ----------------
    ult    <- df_f %>% dplyr::slice_tail(n = 1)
    penult <- df_f %>% dplyr::slice_tail(n = 2) %>% dplyr::slice(1)
    
    media_hist <- df_f %>%
      dplyr::slice_tail(n = min(52, nrow(df_f))) %>%
      dplyr::summarise(
        media_sars = mean(taxa_sarscov2, na.rm = TRUE),
        media_inf  = mean(taxa_influenza, na.rm = TRUE)
      )
    
    margem <- 0.10
    
    nivel_sars <- dplyr::case_when(
      ult$taxa_sarscov2 > media_hist$media_sars * (1 + margem) ~ "Alto",
      ult$taxa_sarscov2 < media_hist$media_sars * (1 - margem) ~ "Baixo",
      TRUE ~ "Médio"
    )
    
    nivel_inf <- dplyr::case_when(
      ult$taxa_influenza > media_hist$media_inf * (1 + margem) ~ "Alto",
      ult$taxa_influenza < media_hist$media_inf * (1 - margem) ~ "Baixo",
      TRUE ~ "Médio"
    )
    
    tend_sars <- ifelse(ult$taxa_sarscov2 > penult$taxa_sarscov2, "Subindo", "Diminuindo")
    tend_inf  <- ifelse(ult$taxa_influenza > penult$taxa_influenza, "Subindo", "Diminuindo")
    
    alertas$nivel_sars <- nivel_sars
    alertas$tend_sars  <- tend_sars
    alertas$nivel_inf  <- nivel_inf
    alertas$tend_inf   <- tend_inf
    
    cor_nivel <- function(nivel) {
      dplyr::case_when(
        nivel == "Alto"  ~ "#d9534f",
        nivel == "Médio" ~ "#5bc0de",
        nivel == "Baixo" ~ "#5cb85c"
      )
    }
    
    # ---------------- EIXO X ----------------
    categorias <- paste0("WK ", sprintf("%02d", df_f$sem_epi), "<br>", df_f$ano)
    
    # ---------------- GRÁFICO (SEM TESTADOS) ----------------
    highchart() %>%
      hc_chart(type = "line", zoomType = "x") %>%
      hc_title(text = "SARS-CoV-2 e Influenza – Taxa de Positividade (%)") %>%
      hc_subtitle(
        useHTML = TRUE,
        text = paste0(
          "<b>SARS-CoV-2:</b> <span style='color:", cor_nivel(nivel_sars), "'>",
          nivel_sars, " – ", tend_sars, "</span> | ",
          "<b>Influenza:</b> <span style='color:", cor_nivel(nivel_inf), "'>",
          nivel_inf, " – ", tend_inf, "</span>"
        )
      ) %>%
      hc_xAxis(categories = categorias, labels = list(useHTML = TRUE)) %>%
      hc_yAxis(
        title = list(text = "Taxa de Positividade (%)"),
        min = 0,
        max = 100
      ) %>%
      hc_add_series(
        name = "Influenza (%)",
        data = df_f$taxa_influenza,
        dashStyle = "ShortDot",
        marker = list(enabled = TRUE)
      ) %>%
      hc_add_series(
        name = "SARS-CoV-2 (%)",
        data = df_f$taxa_sarscov2,
        dashStyle = "Dash",
        marker = list(enabled = TRUE),
        dataLabels = list(enabled = TRUE)
      ) %>%
      hc_tooltip(shared = TRUE, valueSuffix = " %") %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
  })
  
  
  
  output$alert_sars <- renderValueBox({
    
    req(alertas$nivel_sars, alertas$tend_sars)
    
    cor <- ifelse(
      alertas$nivel_sars == "Alto", "red",
      ifelse(alertas$nivel_sars == "Médio", "light-blue", "green")
    )
    
    icon_dir <- ifelse(alertas$tend_sars == "Subindo", "arrow-up", "arrow-down")
    
    valueBox(
      value = alertas$nivel_sars,
      subtitle = paste("SARS-CoV-2 |", alertas$tend_sars),
      icon = icon(icon_dir),
      color = cor
    )
  })
  
  
  output$alert_inf <- renderValueBox({
    
    req(alertas$nivel_inf, alertas$tend_inf)
    
    cor <- ifelse(
      alertas$nivel_inf == "Alto", "red",
      ifelse(alertas$nivel_inf == "Médio", "light-blue", "green")
    )
    
    icon_dir <- ifelse(alertas$tend_inf == "Subindo", "arrow-up", "arrow-down")
    
    valueBox(
      value = alertas$nivel_inf,
      subtitle = paste("Influenza |", alertas$tend_inf),
      icon = icon(icon_dir),
      color = cor
    )
  })
  
  # secao de mapas de positivos e nehativos de influenza por bairos usando a variavel  que compoe os bairros usando a base de dados B_geral_HCA_R
  
} # end server



# ---------------- Run App ---------------------------------------------
shinyApp(ui, server)
