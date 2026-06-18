# ===================== LIBRARIES =====================
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(dplyr)
library(lubridate)
library(ggplot2)
library(highcharter)
library(reactable)

# ===================== LOAD DATA =====================
load('data/DB_Dashboard/B_geral.rda')
load('data/DB_Dashboard/B_Comunitaria.rda')
load('data/DB_Dashboard/B_Hospitalar.rda')
load('data/DB_Dashboard/B_Ambiental.rda')
load('data/DB_Dashboard/BD_Genomica_Final.rda')
load('data/DB_Dashboard/mapa_dados_influenza_sarsc.rda')

# ===================== DATA PROCESS =====================
ultima_data_reporte <- B_geral_HCA_R %>%
  mutate(
    DATE2 = DATE2
  ) %>%
  summarise(ULTIMA_DATA = max(DATE2)) %>%
  pull(ULTIMA_DATA)

# ===================== UI =====================
ui <- navbarPage(
  title = "Sindrome Respiratoria",
  #theme = shinytheme("flatly"),
  theme = shinythemes::shinytheme("flatly"),
  
  header = tagList(
    useShinydashboard(), #Garante que fornece estruturas e estilos adicionais para criar painéis de controle bonitos segundo a nossa escolha
    uiOutput("navbar_alertas")
    ),
  
  tags$head(
    tags$style(HTML("
      
      /* Corrigir cores das valueBox */
      .small-box.bg-primary { background-color: #2C7BE5 !important; color: white !important; }
      .small-box.bg-success { background-color: #00A65A !important; color: white !important; }
      .small-box.bg-warning { background-color: #F39C12 !important; color: white !important; }
      .small-box.bg-danger  { background-color: #E74C3C !important; color: white !important; }
      
      /* Ícones mais visíveis */
      .small-box .icon-large {
        font-size: 60px !important;
        opacity: 0.3;
      }
    "))
  ),
  
     h4(paste("Date:", ultima_data_reporte)),
    hr(),
    
    
    fluidRow(
      box(
        width = 12,
        title = "Principais Indicadores",
        status = "info",
        solidHeader = TRUE,
        splitLayout(
          cellWidths = c("30%", "30%", "30%", "30%"),  # <- ajuste aqui
          valueBoxOutput("vb_total_registros"),
          valueBoxOutput("vb_hospitalar"),
          valueBoxOutput("vb_comunitaria"),
          valueBoxOutput("vb_ambiental")
        )
      ),
      
    ), 
  tabPanel(
    "Influenza",
    icon = icon("head-side-cough"),
    

    
    hr(),
    
    fluidRow(
      box(width = 12, status = "primary", solidHeader = TRUE,
          title = tagList(icon("chart-line"), "Dados do SISMA"),
      box(
        title = "CS zimpeto",
        width = 6,
        plotOutput("graf_casodiario")
      ),
      box(
        title = "CS Mavalane",
        width = 6,
        plotOutput("tendencia_casos")
      )
    ),
    
    box(width = 12, status = "primary", solidHeader = TRUE,
        title = tagList(icon("chart-bar"), "Influenza Positivity Surveillance"),
      fluidRow(
        column(width = 12,
               box(title = "Health Care Facility", width = 6,
                   highchartOutput("graf_influenza_hospitalar", height = "300px")),
               box(title = "Community", width = 6,
                   highchartOutput("graf_influenza_comunitaria", height = "300px")),
               box(title = "Wastewater", width = 12,
                   highchartOutput("graf_influenza_ambiental", height = "300px"))
        )
      )
    )

    ),
    
  ),
  
  tabPanel(
    "Sars-CoV-2",
    icon = icon("virus-covid"),
    
    fluidRow(
      box(
        title = "Base de Dados",
        width = 12,
        reactableOutput("Toda_BD")
      )
    )
  ),
  
  tabPanel(
    "Maps",
    icon = icon("map"),
    
    fluidRow(
      box(
        title = "Testagem por Província",
        width = 6,
        plotOutput("testagem_provincia")
      ),
      box(
        title = "Taxa de Positividade",
        width = 6,
        highchartOutput("taxa_pos_prov")
      )
    )
  )
)

# ===================== SERVER =====================
server <- function(input, output, session) {
  
  
  #metodo para filtrar na base de dados
  safe_filter <- function(df, col, value) {
    if (!is.null(value) && value != "Todas" && col %in% names(df)) {
      df <- df %>% dplyr::filter(as.character(.data[[col]]) == value)
    }
    df
  }
  
  
  
  # ---------- Value boxes (exemplos) ----------------
  output$vb_total_registros <- renderValueBox({
    
    df <- B_geral_HCA_R %>%
      safe_filter(provincia_casos, input$filt_prov) %>%
      safe_filter("vigilancia", input$filt_vigilancia)%>%
      safe_filter(Unidade_sanitaria, input$filt_post) 
    
    valueBox(
      value = nrow(df),
      subtitle = "Sample Tested",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  
  
  output$vb_hospitalar <- renderValueBox({
    df <- BD_Final_VH_R %>%
      safe_filter(provincia_casos, input$filt_prov) %>%
      safe_filter(Unidade_sanitaria, input$filt_vigilancia)%>%
      safe_filter(Unidade_sanitaria, input$filt_post) 
    
    n = nrow(df)
    valueBox(value = n, subtitle = "Health Care Facility", icon = icon("hospital"), color = "green")
  })
  output$vb_comunitaria <- renderValueBox({
    df <- BD_Final_VC_R %>%
      safe_filter(provincia_casos, input$filt_prov) %>%
      safe_filter("vigilancia", input$filt_vigilancia)%>%
      safe_filter("Unidade_sanitaria", input$filt_post) 
    
    n = nrow(df)
    valueBox(value = n, subtitle = "Community", icon = icon("users"), color = "yellow")
  })
  
  output$vb_ambiental <- renderValueBox({

    BD_Final_VA_R <- B_geral_HCA_R %>% 
      filter(vigilancia == "Ambiental") %>%
      select(
        "codigo_paciente", "modulo_ras", "modulo_tifoide", "modulo_colera",                       
        "modulo_rsv", "Amostras_colhidas", "modulo_reporte", "Unidade_sanitaria",                   
        "local_colheita", "DATE2", "provincia_casos", "distrito_casos",                      
        "hospital1_outro_distrito_residencia", "bairro", "unidade_sanitaria", "data_nascimento",                     
        "conhece_data_nasc", "idade", "tipo_idade", "idade_complement",                    
        "escolaridade", "estado_civil", "profissao", "sexo",                                
        "hospital1_data_inclusao", "hospital1_child_age_months", "Influenza", "SARS_CoV_2",                          
        "tipo_influenza", "subtipo_influenza", "resul_tifoide", "resul_colera",                        
        "resultado_cultura", "outro_resultado_cultura", "resultado_rsv", "tipo_rsv",                            
        "resultado_salmonella_typhi", "hospital5_motivo_hospitalizado_other", "ambiental_provincia_residencia1", "ambiental_distrito_residencia1",      
        "ambiental_bairro_residencia1", "ambiental_lo_colheita", "comunitaria_clusters", "comunitaria_provincia",               
        "comunitaria_distrito", "comunitaria_us", "outro_bairro", "residencia_tipo",                     
        "sintomas", "hospital2_data_inic_sint", "outro_sintoma", "Hospitalizado",                       
        "motivo_hospitalizacao", "local_inclusao", "instanceID", "meta:instanceID",                     
        "vigilancia", "ano", "Semana_Epi", "Semana_Epi_ano"
      )
    
    df <- BD_Final_VA_R %>%
      safe_filter(provincia_casos, input$filt_prov) %>%
      safe_filter("vigilancia", input$filt_vigilancia)%>%
      safe_filter("Unidade_sanitaria", input$filt_post) 
    
    n = nrow(df)
    valueBox(value = n, subtitle = "Wastwater", icon = icon("water"), color = "red")
  })
  
  
  # ---------- Highcharts placeholders ----------------
  # Função auxiliar para gerar gráfico por vigilância
  
  grafico_influenza <- function(df, tipo_vigilancia) {
    
    df_vig <- df %>%
     # safe_filter(provincia_casos, input$filt_prov) %>%
     # safe_filter("vigilancia", input$filt_vigilancia) %>%
     # safe_filter("Unidade_sanitaria", input$filt_post) %>%
      dplyr::filter(vigilancia == tipo_vigilancia) %>%
      dplyr::mutate(
        ano = lubridate::year(DATE2),
        sem_epi = lubridate::epiweek(DATE2)
      ) %>%
      dplyr::group_by(ano, sem_epi) %>%
      dplyr::summarise(
        total_testados = n(),
        positivos = sum(
          ifelse(Influenza == "positivo", 1, 0),
          na.rm = TRUE
        ),
        taxa_positividade = round((positivos / total_testados) * 100, 1),
        .groups = "drop"
      ) %>%
      dplyr::arrange(ano, sem_epi)
    
    categorias <- paste0(
      "S", sprintf("%02d", df_vig$sem_epi),
      "<br>", df_vig$ano
    )
    
    highchart() %>%
      
      hc_chart(zoomType = "xy") %>%
      
      hc_title(
        text = paste(
          "Disease reported, samples tested and positivity rate -",
          toupper(
            ifelse(tipo_vigilancia == "Comunitaria", "Community",
                   ifelse(tipo_vigilancia == "Hospitalar", "Health Facility",
                          ifelse(tipo_vigilancia == "Ambiental", "Wastewater",
                                 tipo_vigilancia)))
          )
        )
      ) %>%
      
      hc_xAxis(
        categories = categorias,
        title = list(text = "Epi Week / Year"),
        labels = list(useHTML = TRUE)
      ) %>%
      
      hc_yAxis_multiples(
        list(
          title = list(text = "Positive Cases / Sample Tested"),
          opposite = FALSE
        ),
        list(
          title = list(text = "Positivity rate (%)"),
          opposite = TRUE,
          labels = list(format = "{value}%")
        )
      ) %>%
      
      # ------------------- SAMPLE TESTED -------------------
    hc_add_series(
      name = "Sample Tested",
      data = df_vig$total_testados,
      type = "column",
      color = "steelblue",
      yAxis = 0,
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "function () {
              if (this.y === 0) { return null; }
              return this.y;
           }"
        )
      )
    ) %>%
      
      # ------------------- POSITIVE CASES -------------------
    hc_add_series(
      name = "Influenza Positive Cases",
      data = df_vig$positivos,
      type = "column",
      color = "orange",
      yAxis = 0,
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "function () {
              if (this.y === 0) { return null; }
              return this.y;
           }"
        )
      )
    ) %>%
      
      # ------------------- POSITIVITY RATE -------------------
    hc_add_series(
      name = "Positivity rate (%)",
      data = df_vig$taxa_positividade,
      type = "line",
      yAxis = 1,
      dashStyle = "ShortDash",
      color = "darkgreen",
      marker = list(enabled = TRUE),
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "function () {
              if (this.y === 0) { return null; }
              return this.y + '%';
           }"
        )
      )
    ) %>%
      
      hc_plotOptions(
        column = list(
          grouping = TRUE,
          pointPadding = 0.1,
          groupPadding = 0.2
        )
      ) %>%
      
      hc_tooltip(
        shared = TRUE,
        crosshairs = TRUE
      ) %>%
      
      hc_add_theme(hc_theme_google()) %>%
      
      hc_exporting(enabled = TRUE)
  }
  
  
  
  
  
  # Preparar base de dados
  df_f <- B_geral_HCA_R %>%
    dplyr::mutate(
      DATE2 
    ) %>%
    dplyr::filter(!is.na(DATE2))
  
  
  
  # Renderizar cada gráfico de influenza
  output$graf_influenza_hospitalar <- renderHighchart({
    grafico_influenza(df_f, "Hospitalar")
  })
  
  output$graf_influenza_comunitaria <- renderHighchart({
    grafico_influenza(df_f, "Comunitaria")
  })
  
  output$graf_influenza_ambiental <- renderHighchart({
    grafico_influenza(df_f, "Ambiental")
  })
  
  
  
  #ALERTAS
  
  output$navbar_alertas <- renderUI({
    
    # ---------------- PREPARAÇÃO DOS DADOS ----------------
    df_f <- B_geral_HCA_R %>%
      dplyr::mutate(
        DATE2 
      ) %>%
      dplyr::filter(!is.na(DATE2)) %>%
      dplyr::mutate(
        ano = lubridate::year(DATE2),
        sem_epi = lubridate::epiweek(DATE2),
        influenza_pos = ifelse(Influenza == "positivo", 1, 0),
        sarscov2_pos  = ifelse(SARSCov2 == "positivo", 1, 0)
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
    
    req(nrow(df_f) >= 2)
    
    # ---------------- CÁLCULO DE ALERTAS ----------------
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
    
    # ---------------- FUNÇÃO COR ----------------
    cor_nivel <- function(nivel) {
      dplyr::case_when(
        nivel == "Alto"  ~ "#d9534f",
        nivel == "Médio" ~ "#f0ad4e",
        nivel == "Baixo" ~ "#5cb85c"
      )
    }
    
    # ---------------- NAVBAR HTML + PULSO ----------------
    tagList(
      tags$style(HTML("
      @keyframes pulse {
        0% { transform: scale(1); opacity: 1; }
        50% { transform: scale(1.3); opacity: 0.5; }
        100% { transform: scale(1); opacity: 1; }
      }
      .pulse-red { animation: pulse 1s infinite; }
      .arrow-blink { animation: pulse 1s infinite; font-weight: bold; }
      .circle { 
        display:inline-block; 
        width:12px; 
        height:12px; 
        border-radius:50%; 
        margin-right:5px; 
      }
    ")),
      
      tags$ul(
        class = "nav navbar-nav navbar-right",
        style = "padding-right: 20px;",
        
        # SARS-CoV-2
        tags$li(
          tags$span(
            style = "font-weight:bold; padding-right:15px;",
            tags$span(
              class = paste("circle", ifelse(nivel_sars=="Alto" & tend_sars=="Subindo","pulse-red","")),
              style = paste0("background-color:", cor_nivel(nivel_sars), ";")
            ),
            "SARS-CoV-2: ", nivel_sars, " ",
            tags$span(
              class = ifelse(tend_sars=="Subindo","arrow-blink",""),
              ifelse(tend_sars=="Subindo","\u2191","\u2193")
            )
          )
        ),
        
        # Influenza
        tags$li(
          tags$span(
            style = "font-weight:bold; padding-right:15px;",
            tags$span(
              class = paste("circle", ifelse(nivel_inf=="Alto" & tend_inf=="Subindo","pulse-red","")),
              style = paste0("background-color:", cor_nivel(nivel_inf), ";")
            ),
            "Influenza: ", nivel_inf, " ",
            tags$span(
              class = ifelse(tend_inf=="Subindo","arrow-blink",""),
              ifelse(tend_inf=="Subindo","\u2191","\u2193")
            )
          )
        )
      )
    )
    
  })
  
  
}

# ===================== RUN APP =====================
shinyApp(ui, server)