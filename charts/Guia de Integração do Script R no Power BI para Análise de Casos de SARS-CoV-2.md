# Guia de Integração do Script R no Power BI para Análise de Casos de SARS-CoV-2

## Introdução

Este guia detalha o processo de integração de um script R no Microsoft Power BI para visualizar e analisar casos de SARS-CoV-2. O script R foi desenvolvido para identificar áreas geográficas específicas (bairros) com base na densidade populacional que frequenta uma unidade de saúde e, em seguida, mapear os casos de SARS-CoV-2 dentro dessas áreas. A integração com o Power BI permitirá a criação de dashboards interativos e dinâmicos, combinando a capacidade analítica do R com as poderosas ferramentas de visualização do Power BI.

## Pré-requisitos

Antes de iniciar a integração, certifique-se de que os seguintes pré-requisitos estejam instalados e configurados em seu ambiente:

1.  **Microsoft Power BI Desktop**: A versão mais recente do Power BI Desktop deve estar instalada em seu computador. Você pode baixá-lo gratuitamente no site oficial da Microsoft.
2.  **R e RStudio (Opcional, mas Recomendado)**: Embora o Power BI possa executar scripts R diretamente, ter o R e o RStudio instalados localmente é altamente recomendado para desenvolvimento, depuração e teste do script R. O Power BI utilizará a instalação do R em seu sistema.
    *   **Instalação do R**: Baixe e instale a versão mais recente do R a partir do [CRAN (The Comprehensive R Archive Network)](https://cran.r-project.org/).
    *   **Instalação do RStudio**: Baixe e instale o RStudio Desktop a partir do [site oficial do RStudio](https://www.rstudio.com/products/rstudio/download/).
3.  **Pacotes R Necessários**: O script R utiliza os seguintes pacotes para manipulação de dados geoespaciais e visualização:
    *   `sf`: Para manipulação de dados espaciais (Simple Features).
    *   `dplyr`: Para manipulação e transformação de dados.
    *   `ggplot2`: Para criação de gráficos e visualizações.
    *   `rgeos`: Para operações geoespaciais (algumas funções podem ser substituídas por `sf` em versões mais recentes).

    Você pode instalar esses pacotes no R ou RStudio executando os seguintes comandos:
    ```R
    install.packages("sf")
    install.packages("dplyr")
    install.packages("ggplot2")
    install.packages("rgeos")
    ```

## Configuração do R no Power BI Desktop

Para que o Power BI Desktop possa executar scripts R, você precisa configurar o caminho para a sua instalação do R:

1.  Abra o **Power BI Desktop**.
2.  Vá para **Arquivo > Opções e configurações > Opções**.
3.  No painel esquerdo, selecione **Scripting R**.
4.  Certifique-se de que o caminho para o seu diretório base do R esteja selecionado. Se não estiver, clique em **Outro** e navegue até a pasta onde o R está instalado (por exemplo, `C:\Program Files\R\R-x.x.x`).
5.  Clique em **OK**.

## Preparação dos Dados no Power BI

O script R espera certos tipos de dados para realizar a análise. Em um cenário real, você importaria seus dados de casos de SARS-CoV-2, dados geográficos dos bairros e informações sobre unidades de saúde e população para o Power BI. Para este guia, assumiremos que você tem acesso a esses dados em um formato que o Power BI pode consumir (por exemplo, CSV, Excel, bancos de dados).

### Estrutura de Dados Esperada pelo Script R

O script R `sars_cov_2_analysis.R` simula os seguintes conjuntos de dados:

*   **Dados Geográficos dos Bairros**: Polígonos que representam os limites dos bairros. No script, isso é simulado com coordenadas `x` e `y` para criar polígonos simples. Em um ambiente real, você precisaria de um arquivo GeoJSON ou Shapefile com as geometrias dos bairros e um identificador único para cada bairro.
    *   **Exemplo de Colunas Necessárias**: `Bairro_Nome`, `Geometria` (ou colunas de coordenadas para cada vértice do polígono).

*   **Dados de Casos de SARS-CoV-2**: Pontos que representam a localização dos casos, juntamente com seu status (positivo/testado).
    *   **Exemplo de Colunas Necessárias**: `ID_Caso`, `Longitude`, `Latitude`, `Status_Caso` (e.g., 'Positivo', 'Testado'), `Bairro_Associado`.

*   **Dados de Unidades de Saúde e População**: Informações sobre a localização da unidade de saúde (CS Zimpeto) e dados populacionais por bairro, incluindo a população total e a população que frequenta a unidade de saúde.
    *   **Exemplo de Colunas Necessárias**: `Bairro_Nome`, `Populacao_Total`, `Populacao_US`.

### Importando Dados para o Power BI

1.  No Power BI Desktop, clique em **Obter Dados** na faixa de opções **Página Inicial**.
2.  Escolha a fonte de dados apropriada (por exemplo, **Texto/CSV**, **Excel**, **Banco de Dados SQL Server**, etc.).
3.  Importe seus conjuntos de dados para o Power BI. Certifique-se de que os nomes das colunas e os tipos de dados correspondam ao que o script R espera.
4.  Se necessário, use o **Editor do Power Query** para transformar e limpar seus dados, garantindo que estejam no formato correto para o script R.

## Integrando o Script R como um Visual no Power BI

O Power BI permite que você use scripts R para criar visuais personalizados. O script `sars_cov_2_analysis.R` gera um gráfico `ggplot2` que pode ser exibido diretamente no Power BI.

1.  Na tela de relatório do Power BI Desktop, clique no ícone **Visual do script R** no painel **Visualizações**.
2.  Uma caixa de texto para o script R aparecerá. Copie e cole o conteúdo do arquivo `sars_cov_2_analysis.R` nesta caixa de texto.
3.  No painel **Campos**, arraste os campos necessários dos seus conjuntos de dados importados para a seção **Valores** do visual do script R. Por exemplo, se você tiver uma tabela de bairros, arraste o campo `Bairro_Nome` e, se tiver uma tabela de casos, arraste `Longitude`, `Latitude` e `Status_Caso`.
    *   **Importante**: O script R receberá esses campos como um `dataframe` chamado `dataset`. Você precisará adaptar a parte inicial do script R para ler esses dados do `dataset` em vez de simulá-los. Por exemplo, se você arrastar `Bairro_Nome`, `Longitude`, `Latitude`, `Status_Caso` para o visual R, eles estarão disponíveis no dataframe `dataset`.

    **Exemplo de Adaptação do Script R para Power BI (dentro do Power BI):**

    ```R
    # Carregar pacotes necessários (já feito no script original)
    library(sf)
    library(dplyr)
    library(ggplot2)
    library(rgeos)

    # Adaptar para usar os dados do Power BI
    # O Power BI passa os dados selecionados para um dataframe chamado 'dataset'

    # Exemplo de como você pode reestruturar seus dados do Power BI para o script R
    # Supondo que 'dataset' contenha as colunas: Bairro_Nome, Longitude, Latitude, Status_Caso, Populacao_Total, Populacao_US

    # Dados Geográficos dos Bairros (você precisaria de um shapefile ou GeoJSON real)
    # Para este exemplo, vamos simular novamente, mas em um cenário real, você carregaria seus dados geoespaciais.
    # Power BI não passa geometrias complexas diretamente para o script R Visual.
    # Você pode precisar de um visual de mapa personalizado ou pré-processar as geometrias.
    # Para o propósito deste guia, o script R visualiza o mapa, então as geometrias são geradas internamente ou simplificadas.

    # Simulação de bairros (se você não tiver um shapefile carregado no Power BI)
    # Em um cenário real, você carregaria um shapefile ou GeoJSON aqui, ou passaria os dados de geometria de outra forma.
    # Para o Visual R, é mais fácil se as geometrias forem geradas ou simplificadas.
    set.seed(123)
    bairros_coords <- list(
      list(x = c(0, 0, 2, 2, 0), y = c(0, 2, 2, 0, 0), name = 'Bairro A'),
      list(x = c(1, 1, 3, 3, 1), y = c(1, 3, 3, 1, 1), name = 'Bairro B'),
      list(x = c(3, 3, 5, 5, 3), y = c(0, 2, 2, 0, 0), name = 'Bairro C'),
      list(x = c(0, 0, 1, 1, 0), y = c(3, 4, 4, 3, 3), name = 'Bairro D'),
      list(x = c(2, 2, 4, 4, 2), y = c(3, 5, 5, 3, 3), name = 'Bairro E')
    )

    bairros_sf <- lapply(bairros_coords, function(b) {
      polygon <- st_polygon(list(cbind(b$x, b$y)))
      st_sf(name = b$name, geometry = polygon)
    })

    bairros_sf <- do.call(rbind, bairros_sf)

    # Dados de Casos de SARS-CoV-2 do Power BI
    # Certifique-se de que as colunas 'Longitude', 'Latitude', 'Status_Caso', 'Bairro_Associado' existam no seu 'dataset'
    casos_sf <- st_as_sf(dataset, coords = c('Longitude', 'Latitude'), crs = st_crs(bairros_sf))
    names(casos_sf)[names(casos_sf) == 'Status_Caso'] <- 'status' # Renomear para corresponder ao script original
    names(casos_sf)[names(casos_sf) == 'Bairro_Associado'] <- 'bairro' # Renomear para corresponder ao script original

    # Dados de Unidades de Saúde e População do Power BI
    # Certifique-se de que as colunas 'Bairro_Nome', 'Populacao_Total', 'Populacao_US' existam no seu 'dataset'
    populacao_us <- dataset %>%
      select(Bairro_Nome, Populacao_Total, Populacao_US) %>%
      distinct() # Remover duplicatas se os dados de população estiverem repetidos por caso
    names(populacao_us)[names(populacao_us) == 'Bairro_Nome'] <- 'bairro' # Renomear

    populacao_us <- populacao_us %>%
      mutate(perc_us = (Populacao_US / Populacao_Total) * 100)

    # Localização da US Zimpeto (pode ser um ponto fixo ou vir de um dado)
    us_zimpeto <- st_point(c(2.5, 1.5)) # Localização simulada da US Zimpeto
    us_zimpeto_sf <- st_sf(name = 'CS Zimpeto', geometry = us_zimpeto)

    # Área de captação da US (15 Km - simulado como um raio de 2 unidades para este exemplo)
    area_captacao_us <- st_buffer(us_zimpeto_sf, dist = 2)

    # Identificar bairros dentro da área de captação e com >80% de população frequentando a US
    bairros_selecionados <- bairros_sf %>%
      st_join(area_captacao_us, join = st_intersects) %>%
      filter(!is.na(name.y)) %>% # Bairros que intersectam a área de captação
      left_join(populacao_us, by = c('name' = 'bairro')) %>% # Usar 'name' para bairros_sf
      filter(perc_us >= 80) # Bairros com 80% ou mais da população frequentando a US

    # Realizar a análise de sobreposição para identificar casos dentro das áreas definidas
    casos_dentro_area <- casos_sf %>%
      st_join(bairros_selecionados, join = st_intersects) %>%
      filter(!is.na(bairro)) # Casos que estão dentro dos bairros selecionados

    # Adicionar uma coluna para indicar se o caso está na área selecionada
    casos_sf$na_area_selecionada <- casos_sf$id %in% casos_dentro_area$id

    # Visualização (para ser exibida no Power BI via Visual R)
    map_plot <- ggplot() +
      geom_sf(data = bairros_sf, aes(fill = name %in% bairros_selecionados$name), color = 'black', alpha = 0.6) + # Usar name para bairros_selecionados
      geom_sf(data = us_zimpeto_sf, color = 'blue', size = 4, shape = 8) + # US Zimpeto
      geom_sf(data = area_captacao_us, fill = NA, color = 'red', linetype = 'dashed', size = 1) + # Área de captação
      geom_sf(data = casos_sf, aes(color = status, size = na_area_selecionada), show.legend = 'point') +
      scale_fill_manual(values = c('FALSE' = 'lightgray', 'TRUE' = 'lightblue'), labels = c('Outros Bairros', 'Bairros Selecionados')) +
      scale_color_manual(values = c('Positivo' = 'red', 'Testado' = 'orange')) +
      scale_size_manual(values = c('TRUE' = 3, 'FALSE' = 1), labels = c('Na Área Selecionada', 'Fora da Área Selecionada')) +
      labs(
        title = 'Casos de SARS-CoV-2 e Áreas de Captação da US',
        fill = 'Área de Interesse',
        color = 'Status do Caso',
        size = 'Localização do Caso'
      ) +
      theme_minimal()

    print(map_plot)
    ```

4.  Clique no botão **Executar script** (o ícone de reprodução) no canto superior direito do editor de script R.
5.  O visual do mapa será renderizado no seu relatório do Power BI.

## Integrando o Script R para Transformação de Dados

Além de visuais, você pode usar scripts R para transformar dados no Power Query, o que é útil para pré-processar dados geoespaciais ou realizar cálculos complexos antes de carregar os dados no modelo do Power BI.

1.  No Power BI Desktop, clique em **Transformar dados** na faixa de opções **Página Inicial** para abrir o **Editor do Power Query**.
2.  Selecione a consulta (tabela) que contém os dados que você deseja transformar.
3.  Na faixa de opções **Transformar**, clique em **Executar script R**.
4.  Uma caixa de diálogo para o script R aparecerá. O dataframe de entrada será chamado `dataset`.
5.  Você pode escrever um script R que manipule este `dataset` e retorne um novo dataframe. Por exemplo, você pode usar o script R para calcular a distância de cada caso à unidade de saúde ou para enriquecer os dados com informações geoespaciais.

    **Exemplo de Script R para Transformação de Dados no Power Query:**

    ```R
    # Exemplo: Adicionar uma coluna indicando se o caso está próximo à US
    # Supondo que 'dataset' tenha 'Longitude' e 'Latitude' dos casos

    library(sf)
    library(dplyr)

    # Localização da US Zimpeto (pode ser um ponto fixo ou vir de um parâmetro)
    us_zimpeto_coords <- c(2.5, 1.5) # Coordenadas simuladas
    us_zimpeto_sf <- st_sf(name = 'CS Zimpeto', geometry = st_point(us_zimpeto_coords))

    # Converter o dataset do Power BI para um objeto sf
    casos_sf_transform <- st_as_sf(dataset, coords = c('Longitude', 'Latitude'), crs = 4326) # Use o CRS apropriado

    # Calcular a distância de cada caso à US
    casos_sf_transform$dist_to_us <- st_distance(casos_sf_transform, us_zimpeto_sf)

    # Adicionar uma coluna booleana para proximidade (exemplo: dentro de 1 unidade de distância)
    casos_sf_transform$is_near_us <- casos_sf_transform$dist_to_us < units::set_units(1, m) # Ajuste a unidade conforme necessário

    # Retornar o dataframe modificado para o Power BI
    # O Power BI espera um dataframe como saída.
    output <- as.data.frame(casos_sf_transform) %>%
      select(-geometry) # Remover a coluna de geometria se não for necessária no Power BI

    # O dataframe 'output' será carregado de volta no Power BI
    ```

6.  Clique em **OK**. O Power Query executará o script R e aplicará as transformações aos seus dados.

## Atualização dos Dados e do Visual

Quando os dados subjacentes no Power BI são atualizados (por exemplo, novos casos de SARS-CoV-2 são adicionados à sua fonte de dados), o visual do script R e as transformações de dados R serão automaticamente atualizados quando você atualizar o relatório no Power BI Desktop ou quando o relatório for atualizado no serviço do Power BI.

## Considerações Finais

*   **Desempenho**: Scripts R complexos ou que processam grandes volumes de dados podem afetar o desempenho do seu relatório do Power BI. Otimize seu script R para eficiência.
*   **Limitações do Visual R**: O visual do script R no Power BI tem algumas limitações, como a incapacidade de interagir diretamente com outros visuais do Power BI (por exemplo, filtros cruzados). Para interatividade total, considere usar os recursos de mapa nativos do Power BI com dados geoespaciais pré-processados no R ou Power Query.
*   **Segurança**: Certifique-se de que os scripts R que você executa no Power BI são de fontes confiáveis, pois eles podem acessar recursos do sistema.

Este guia fornece uma base para integrar seu script R no Power BI. Para análises mais avançadas ou personalizações, consulte a documentação oficial do Power BI e do R.

---

**Autor**: Manus AI
**Data**: 14 de Julho de 2025


