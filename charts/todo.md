## Tarefas para o script R e integração com Power BI

### Fase 1: Análise da imagem e requisitos
- [x] Extrair informações relevantes da imagem e do prompt.
- [x] Identificar os tipos de dados necessários (geográficos, casos de SARS-CoV-2, dados populacionais, dados de unidades de saúde).
- [x] Definir a estrutura dos dados de entrada para o script R.

### Fase 2: Criação do script R para análise geoespacial
- [x] Pesquisar e identificar pacotes R para manipulação de dados geoespaciais (e.g., `sf`, `sp`, `rgeos`, `leaflet`).
- [x] Simular ou obter dados geográficos dos bairros (polígonos).
- [x] Simular ou obter dados de casos de SARS-CoV-2 (localização, status testado/positivo).
- [x] Simular ou obter dados de unidades de saúde e população por bairro.
- [x] Implementar a lógica para definir as áreas com 80% da população que vai à unidade sanitária.
- [x] Realizar a análise de sobreposição para identificar casos dentro das áreas definidas.

### Fase 3: Desenvolvimento da visualização de mapa
- [x] Criar um mapa base usando os dados geográficos.
- [x] Plotar as áreas definidas e os casos de SARS-CoV-2 no mapa.
- [x] Adicionar legendas e elementos visuais conforme a imagem de referência.
- [x] Exportar o mapa como uma imagem ou objeto gráfico compatível com Power BI.

### Fase 4: Criação do script de integração com Power BI
- [x] Pesquisar como scripts R são integrados no Power BI (visual R, transformação de dados R).
- [x] Adaptar o script R para ser executado no ambiente do Power BI.
- [x] Garantir que o script R produza um dataframe ou visual que o Power BI possa consumir.

### Fase 5: Documentação e instruções de integração
- [x] Escrever um guia passo a passo sobre como usar o script R no Power BI.
- [x] Incluir informações sobre a preparação dos dados no Power BI.
- [x] Explicar como atualizar os dados e o visual.

### Fase 6: Entrega dos resultados ao usuário
- [ ] Empacotar o script R, dados de exemplo e documentação.
- [ ] Enviar os arquivos ao usuário.

