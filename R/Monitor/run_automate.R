# Script para baixar dados do ODK Aggregate em R
# Similar ao ODK Briefcase

# Instalar pacotes necessários (se ainda não estiver instalado)
if (!require("httr")) install.packages("httr")
if (!require("xml2")) install.packages("xml2")
if (!require("dplyr")) install.packages("dplyr")

library(httr)
library(xml2)
library(dplyr)

# ==============================================================================
# FUNÇÃO PRINCIPAL PARA BAIXAR DADOS DO ODK AGGREGATE
# ==============================================================================

baixar_dados_odk <- function(base_url, username, password, form_id = NULL, 
                             output_dir = "odk_data") {
  
  # Remover barra final da URL se existir
  base_url <- gsub("/$", "", base_url)
  
  # Criar diretório de saída se não existir
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Configurar autenticação
  auth <- authenticate(username, password)
  
  # ==============================================================================
  # 1. LISTAR FORMULÁRIOS DISPONÍVEIS
  # ==============================================================================
  
  listar_formularios <- function() {
    cat("Buscando lista de formulários...\n")
    
    url_formlist <- paste0(base_url, "/formList")
    
    response <- GET(url_formlist, auth)
    
    if (status_code(response) != 200) {
      stop("Erro ao buscar formulários. Status: ", status_code(response))
    }
    
    # Parse XML
    content_xml <- content(response, "text", encoding = "UTF-8")
    xml_data <- read_xml(content_xml)
    
    # Extrair informações dos formulários
    forms <- xml_find_all(xml_data, ".//xform")
    
    form_list <- data.frame(
      formID = xml_text(xml_find_first(forms, ".//formID")),
      name = xml_text(xml_find_first(forms, ".//name")),
      downloadUrl = xml_text(xml_find_first(forms, ".//downloadUrl")),
      stringsAsFactors = FALSE
    )
    
    cat("Encontrados", nrow(form_list), "formulários\n")
    print(form_list[, c("formID", "name")])
    
    return(form_list)
  }
  
  # ==============================================================================
  # 2. BAIXAR SUBMISSÕES DE UM FORMULÁRIO
  # ==============================================================================
  
  baixar_submissions <- function(form_id) {
    cat("\n========================================\n")
    cat("Baixando dados do formulário:", form_id, "\n")
    cat("========================================\n")
    
    # URL para visualização das submissões
    url_view <- paste0(base_url, "/view/submissionList")
    
    # Parâmetros para obter dados em CSV
    params <- list(
      formId = form_id
    )
    
    response <- GET(url_view, auth, query = params)
    
    if (status_code(response) != 200) {
      warning("Erro ao baixar dados do formulário ", form_id, 
              ". Status: ", status_code(response))
      return(NULL)
    }
    
    # Salvar como CSV
    filename <- paste0(output_dir, "/", 
                       gsub("[^A-Za-z0-9_-]", "_", form_id), 
                       "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    
    writeBin(content(response, "raw"), filename)
    
    cat("✓ Dados salvos em:", filename, "\n")
    
    # Tentar ler e retornar os dados
    tryCatch({
      dados <- read.csv(filename, stringsAsFactors = FALSE)
      cat("✓ Total de registros:", nrow(dados), "\n")
      return(dados)
    }, error = function(e) {
      cat("Arquivo salvo, mas houve erro ao ler CSV:", e$message, "\n")
      return(NULL)
    })
  }
  
  # ==============================================================================
  # EXECUÇÃO PRINCIPAL
  # ==============================================================================
  
  # Listar formulários disponíveis
  forms <- listar_formularios()
  
  # Se form_id foi especificado, baixar apenas esse
  if (!is.null(form_id)) {
    dados <- baixar_submissions(form_id)
    return(list(forms = forms, data = dados))
  }
  
  # Caso contrário, perguntar qual baixar
  cat("\nDeseja baixar dados de qual formulário?\n")
  cat("Digite o número (ou 0 para baixar todos):\n")
  
  for (i in 1:nrow(forms)) {
    cat(i, "-", forms$name[i], "\n")
  }
  
  escolha <- readline(prompt = "Opção: ")
  escolha <- as.integer(escolha)
  
  if (escolha == 0) {
    # Baixar todos os formulários
    all_data <- list()
    for (i in 1:nrow(forms)) {
      dados <- baixar_submissions(forms$formID[i])
      all_data[[forms$formID[i]]] <- dados
    }
    return(list(forms = forms, data = all_data))
  } else if (escolha >= 1 && escolha <= nrow(forms)) {
    # Baixar formulário específico
    dados <- baixar_submissions(forms$formID[escolha])
    return(list(forms = forms, data = dados))
  } else {
    stop("Opção inválida")
  }
}

# ==============================================================================
# EXEMPLO DE USO
# ==============================================================================

# Configurar suas credenciais
BASE_URL <- "https://inqueritos.ins.gov.mz/inqueritos"
USERNAME <- "Anisio"
PASSWORD <- "Anisio@2022"

# Opção 1: Baixar todos os formulários (interativo)
resultado <- baixar_dados_odk(
  base_url = BASE_URL,
  username = USERNAME,
  password = PASSWORD,
  output_dir = "dados_odk"
)
