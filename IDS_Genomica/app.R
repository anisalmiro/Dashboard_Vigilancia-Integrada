# app.R -------------------------------------------------------------
# Dashboard clássico (shinydashboard) - Versão reorganizada e responsiva
# Autor: Gerado por ChatGPT (Anisio Bule - adaptação)
# ------------------------------------------------------------------

# libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(htmltools)
library(htmlwidgets)
library(shinythemes)
library(shinymanager)   # mantido (secure_app)
library(dplyr)
library(reactable)
library(reactablefmtr)
library(highcharter)
library(rpivotTable)
library(DBI)
library(googleVis)
library(plotly)
library(readxl)
library(stringr)
library(ggplot2)
library(plyr)
library(cli)
library(readr)
library(tidyr)
library(here)
library(lubridate)
library(fontawesome)





# Instalar rdrop2 a partir do GitHub
#remotes::install_github("karthik/rdrop2")


Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")


# ---------------- Carregar bases locais (se já baixadas) --------------

load(file = 'data/DB_Dashboard/B_geral.rda')        # B_geral_HCA_R
load(file = 'data/DB_Dashboard/BD_Genomica_Final.rda')  # BD_Final_VH_R


ultima_data_reporte <- B_geral_HCA_R %>%
  dplyr::mutate(
    DATA2 = suppressWarnings(
      lubridate::parse_date_time(`Dados_demograficos:DATE2`,
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
      menuItem("Síndrome Diarreica", tabName = "diarrheaT", icon = icon("disease")),
      menuItem("Sobre", tabName = "sobre", icon = icon("info-circle"))
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
                     
                     # ---- Tab 1: Indicadores e Filtros ----
                     tabPanel("Indicadores",
                              fluidRow(
                                column(width = 3,
                                       p(strong("Última atualização:")),
                                       p(as.character(ultima_data))
                                )),
                             
                        
                              
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
                                  "Influenza Positivity Surveillance"
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
                                  "SARS-CoV-2 Positivity Surveillance"
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
                     
                     tabPanel("Maps",
                              div(style="
                           background-color:#005c99;color:white;padding:12px;
                           margin-bottom:15px;border-radius:6px;font-weight:bold;font-size:20px;",
                                  "Proveniencia de casos de Influenza"
                              ),
                              
                              fluidRow(
                                column(width = 12,
                                       box( width = 12,
                                           highchartOutput("influmap", height = "500px"))
                                    
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
                                           highchartOutput("sarscmap", height = "500px"))
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
      ),
      
      
      tabItem(tabName = "sobre",
              fluidRow(
                box(width = 6, title = "Sobre este Dashboard", status = "info", solidHeader = TRUE,
                    p("Dashboard de Monitoria das Vigilâncias Hospitalar, Comunitária e Ambiental - INS"),
                    p("Desenvolvido para visualização e análise rápida de dados de vigilância.")
                )
              ),
              fluidRow(
                box(width = 12, title = "Informações Gerais / Links", status = "success", solidHeader = TRUE,
                    tags$a(href = "https://ins.gov.mz/", "INS - site oficial", target = "_blank"),
                    br()
                )
              )
      )
    ),
    # tabItems end
    
    
    
    tags$footer(
      div(style = "text-align:center; padding:10px;",
          "Instituto Nacional de Saúde - Todos Direitos Reservados - DATICGD")
    )
  )
) # end dashboardPage

# Wrap UI with secure_app (mantendo compatibilidade com seu código)

# Customização do cabeçalho de login
#ui <- secure_app(ui_inner,
#                 # customização do cabeçalho de login opcional
#                 theme = shinythemes::shinytheme("flatly"),
##                 tags_top = tags$div(
#                   style = "text-align:center; padding:20px;",
#                   tags$img(src = "INS.png", width = "120px"),
#                   tags$h3("INS - Instituto Nacional de Saúde", style = "margin-top:10px; font-weight:bold; color:#003366;")
#                 )
#                 
#)
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
        date2 = `Dados_demograficos:DATE2`
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
        date2 = `Dados_demograficos:DATE2`
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

  

  
} # end server



# ---------------- Run App ---------------------------------------------
shinyApp(ui, server)
