# Introdu&ccedil;&atilde;o 

Balanço de potência: A ferramenta tem como objetivo avaliar os montantes de potência necessários nos momentos em que o sistema apresenta demanda máxima instantânea com o objetivo de verificar as condições de seu atendimento. Isto é, verificando a possibilidade de déficits e a auxiliando na avaliação de contratação de potência para o sistema. A ferramenta mantém a compatibilidade com a análise energética previamente efetuada.

# Instalando o BP 

1.  Para utilizar a ferramenta do Balanço de Potência é necessário que o R esteja instalado. Para tal, basta baixar a instalação e seguir as instruções localizadas, por exemplo, no "mirror" do CRAN da Fiocruz em https://cran.fiocruz.br/ 

2.  Com o R já instalado, execute o R e clique em Pacotes > Instalar pacote(s)... Selecione um "mirror" para baixar os pacotes. Dê preferência para os que estão no Brasil, pois o download será mais rápido. Após isso marque e instale os seguintes pacotes que serão necessários para execução do BP: readr, readxl, writexl, dplyr, stringr, tidyr, clpAPI, DBI, RSQLite, shiny, shinythemes, shinybusy, tictoc, ggplot2, zoo, scales, showtext, jsonlite, parallel, foreach, doParallel, numbers. Outra maneira de instalar as dependências é usando a instrução abaixo no *command* do R. 

`install.packages(c("readr", "readxl", "writexl", "dplyr", "stringr", "tidyr", "clpAPI", "DBI", "RSQLite", "shiny", "shinythemes", "shinybusy", "tictoc", "ggplot2", "zoo", "scales", "showtext", "jsonlite", "parallel", "foreach", "doParallel", "numbers", "devtools"), repos = "https://https://cran.fiocruz.br")`

3. Após a instalação das dependências que estão no CRAN, você deve instalar o pacote do leitor de arquivos dos modelos de planejanemto energético do CEPEL. Este também é uma dependência, contudo, ele foi desenvolvido pela EPE e ainda não está no CRAN. Para instalar esse pacote há duas opções: 

   1. Usar as instruções no *command* do R: `library(devtools)`    

     `install_github("equipesge/leitorrmpe")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip recém baixado.

4. Finalmente, podemos instalar o pacote do BP. Para instalar o pacote há duas opções: 

   1. Usar as instruções no *command* do R: `library(devtools)`    

     `install_github("equipesge/epebpr")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip recém baixado. 

# Executando o BP 

Após ter o pacote instalado, basta usar as instruções no *command* do R: `library(epebpr)`    

     `aplicacaoBalanco()` 

# Instruções de uso da ferramenta 

Após ter a ferramenta em execução basta clicar no ícone do livro para obter ajuda. 

# Contribuição 

Caso desejem fazer contribuições no código, basta seguir o procedimento padrão do GitHub. Nós avaliaremos os *commits* e os incorporaremos se verificarmos as devidas melhorias. 