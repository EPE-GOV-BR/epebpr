# Introdu&ccedil;&atilde;o 

Balan&ccedil;o de pot&ecirc;ncia: A ferramenta tem como objetivo avaliar os montantes de pot&ecirc;ncia necess&aacute;rios nos momentos em que o sistema apresenta demanda m&aacute;xima instant&acirc;nea com o objetivo de verificar as condi&ccedil;&otilde;es de seu atendimento. Isto &eacute;, verificando a possibilidade de d&eacute;ficits e a auxiliando na avalia&ccedil;&atilde;o de contrata&ccedil;&atilde;o de pot&ecirc;ncia para o sistema. A ferramenta mant&eacute;m a compatibilidade com a an&aacute;lise energ&eacute;tica previamente efetuada.

# Instalando o BP 

1.  Para utilizar a ferramenta do Balan&ccedil;o de Pot&ecirc;ncia &eacute; necess&aacute;rio que o R esteja instalado. Para tal, basta baixar a instala&ccedil;&atilde;o e seguir as instru&ccedil;&otilde;es localizadas, por exemplo, no "mirror" do CRAN da Fiocruz em https://cran.fiocruz.br/ 

2.  Com o R j&aacute; instalado, execute o R e clique em Pacotes > Instalar pacote(s)... Selecione um "mirror" para baixar os pacotes. D&ecirc; prefer&ecirc;ncia para os que est&atilde;o no Brasil, pois o download ser&aacute; mais r&aacute;pido. Ap&oacute;s isso marque e instale os seguintes pacotes que ser&atilde;o necess&aacute;rios para execu&ccedil;&atilde;o do BP: readr, readxl, writexl, dplyr, stringr, tidyr, clpAPI, DBI, RSQLite, shiny, shinythemes, shinybusy, tictoc, ggplot2, zoo, scales, showtext, jsonlite, parallel, foreach, doParallel, numbers. Outra maneira de instalar as depend&ecirc;ncias &eacute; usando a instru&ccedil;&atilde;o abaixo no *command* do R. 

`install.packages(c("readr", "readxl", "writexl", "dplyr", "stringr", "tidyr", "clpAPI", "DBI", "RSQLite", "shiny", "shinythemes", "shinybusy", "tictoc", "ggplot2", "zoo", "scales", "showtext", "jsonlite", "parallel", "foreach", "doParallel", "numbers", "devtools"), repos = "https://https://cran.fiocruz.br")`

3. Ap&oacute;s a instala&ccedil;&atilde;o das depend&ecirc;ncias que est&atilde;o no CRAN, voc&ecirc; deve instalar o pacote do leitor de arquivos dos modelos de planejanemto energ&eacute;tico do CEPEL. Este tamb&eacute;m &eacute; uma depend&ecirc;ncia, contudo, ele foi desenvolvido pela EPE e ainda n&atilde;o est&aacute; no CRAN. Para instalar esse pacote h&aacute; duas op&ccedil;&otilde;es: 

   1. Usar as instru&ccedil;&otilde;es no *command* do R: `library(devtools)`    

     `install_github("equipesge/leitorrmpe")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip rec&eacute;m baixado.

4. Finalmente, podemos instalar o pacote do BP. Para instalar o pacote h&aacute; duas op&ccedil;&otilde;es: 

   1. Usar as instru&ccedil;&otilde;es no *command* do R: `library(devtools)`    

     `install_github("equipesge/epebpr")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip rec&eacute;m baixado. 

# Executando o BP 

Ap&oacute;s ter o pacote instalado, basta usar as instru&ccedil;&otilde;es no *command* do R: `library(epebpr)`    

     `aplicacaoBalanco()` 

# Instru&ccedil;&otilde;es de uso da ferramenta 

Ap&oacute;s ter a ferramenta em execu&ccedil;&atilde;o basta clicar no &iacute;cone do livro para obter ajuda. 

# Contribui&ccedil;&atilde;o 

Caso desejem fazer contribui&ccedil;&otilde;es no c&oacute;digo, basta seguir o procedimento padr&atilde;o do GitHub. N&oacute;s avaliaremos os *commits* e os incorporaremos se verificarmos as devidas melhorias. 