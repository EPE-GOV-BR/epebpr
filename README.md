# Introdu&ccedil;&atilde;o 

Balan�o de pot�ncia: A ferramenta tem como objetivo avaliar os montantes de pot�ncia necess�rios nos momentos em que o sistema apresenta demanda m�xima instant�nea com o objetivo de verificar as condi��es de seu atendimento. Isto �, verificando a possibilidade de d�ficits e a auxiliando na avalia��o de contrata��o de pot�ncia para o sistema. A ferramenta mant�m a compatibilidade com a an�lise energ�tica previamente efetuada.

# Instalando o BP 

1.  Para utilizar a ferramenta do Balan�o de Pot�ncia � necess�rio que o R esteja instalado. Para tal, basta baixar a instala��o e seguir as instru��es localizadas, por exemplo, no "mirror" do CRAN da Fiocruz em https://cran.fiocruz.br/ 

2.  Com o R j� instalado, execute o R e clique em Pacotes > Instalar pacote(s)... Selecione um "mirror" para baixar os pacotes. D� prefer�ncia para os que est�o no Brasil, pois o download ser� mais r�pido. Ap�s isso marque e instale os seguintes pacotes que ser�o necess�rios para execu��o do BP: readr, readxl, writexl, dplyr, stringr, tidyr, clpAPI, DBI, RSQLite, shiny, shinythemes, shinybusy, tictoc, ggplot2, zoo, scales, showtext, jsonlite, parallel, foreach, doParallel, numbers. Outra maneira de instalar as depend�ncias � usando a instru��o abaixo no *command* do R. 

`install.packages(c("readr", "readxl", "writexl", "dplyr", "stringr", "tidyr", "clpAPI", "DBI", "RSQLite", "shiny", "shinythemes", "shinybusy", "tictoc", "ggplot2", "zoo", "scales", "showtext", "jsonlite", "parallel", "foreach", "doParallel", "numbers", "devtools"), repos = "https://https://cran.fiocruz.br")`

3. Ap�s a instala��o das depend�ncias que est�o no CRAN, voc� deve instalar o pacote do leitor de arquivos dos modelos de planejanemto energ�tico do CEPEL. Este tamb�m � uma depend�ncia, contudo, ele foi desenvolvido pela EPE e ainda n�o est� no CRAN. Para instalar esse pacote h� duas op��es: 

   1. Usar as instru��es no *command* do R: `library(devtools)`    

     `install_github("equipesge/leitorrmpe")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip rec�m baixado.

4. Finalmente, podemos instalar o pacote do BP. Para instalar o pacote h� duas op��es: 

   1. Usar as instru��es no *command* do R: `library(devtools)`    

     `install_github("equipesge/epebpr")` 

   2. Ou efetuar o download do pacote em zip em www.epe.gov.br/xxxxx e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip rec�m baixado. 

# Executando o BP 

Ap�s ter o pacote instalado, basta usar as instru��es no *command* do R: `library(epebpr)`    

     `aplicacaoBalanco()` 

# Instru��es de uso da ferramenta 

Ap�s ter a ferramenta em execu��o basta clicar no �cone do livro para obter ajuda. 

# Contribui��o 

Caso desejem fazer contribui��es no c�digo, basta seguir o procedimento padr�o do GitHub. N�s avaliaremos os *commits* e os incorporaremos se verificarmos as devidas melhorias. 