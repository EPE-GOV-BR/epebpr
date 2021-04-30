# Introdu&ccedil;&atilde;o 

Balan&ccedil;o de pot&ecirc;ncia: A ferramenta tem como objetivo avaliar os montantes de pot&ecirc;ncia necess&aacute;rios nos momentos em que o sistema apresenta demanda m&aacute;xima instant&acirc;nea com o objetivo de verificar as condi&ccedil;&otilde;es de seu atendimento. Isto &eacute;, verificando a possibilidade de d&eacute;ficits e a auxiliando na avalia&ccedil;&atilde;o de contrata&ccedil;&atilde;o de pot&ecirc;ncia para o sistema. A ferramenta mant&eacute;m a compatibilidade com a an&aacute;lise energ&eacute;tica previamente efetuada.

A ferramenta de balan&ccedil;o de pot&ecirc;ncia foi desenvolvida em linguagem de programa&ccedil;&atilde;o R e sua interface foi desenvolvida com Shiny, que &eacute; um pacote R que facilita a cria&ccedil;&atilde;o de aplica&ccedil;&otilde;es web diretamente do R e utiliza CSS, widgets, html e a&ccedil;&otilde;es JavaScript para maior flexibilidade. A ferramenta tamb&eacute;m usa o pacote clpAPI, que &eacute; uma Interface R para a API em C do COIN-OR Clp.

# Instalando o BP 

1.  Para utilizar a ferramenta do Balan&ccedil;o de Pot&ecirc;ncia &eacute; necess&aacute;rio que o R esteja instalado. Para tal, basta baixar a instala&ccedil;&atilde;o e seguir as instru&ccedil;&otilde;es localizadas, por exemplo, no "mirror" do CRAN da Fiocruz em https://cran.fiocruz.br

2.  Al&eacute;m do R, h&aacute; a necessidade da instala&ccedil;&atilde;o do Rtools, que &eacute; um conjunto de ferramentas importantes para a constru&ccedil;&atilde;o de novos pacotes. Para instalar basta acessar https://cran.r-project.org/bin/windows/Rtools/ para baixar o Rtools 4.0 para windows e depois executar o arquivo. 

2.  Com o R e o Rtools j&aacute; instalados, execute o R e clique em Pacotes > Instalar pacote(s)... Selecione um "mirror" para baixar os pacotes. D&ecirc; prefer&ecirc;ncia para os que est&atilde;o no Brasil, pois o download ser&aacute; mais r&aacute;pido. Ap&oacute;s isso marque e instale os seguintes pacotes que ser&atilde;o necess&aacute;rios para execu&ccedil;&atilde;o do BP: readr, readxl, writexl, dplyr, stringr, tidyr, clpAPI, DBI, RSQLite, shiny, shinythemes, shinybusy, tictoc, plotly, zoo, scales, showtext, jsonlite, parallel, foreach, doParallel, numbers. Outra maneira de instalar as depend&ecirc;ncias &eacute; usando a instru&ccedil;&atilde;o abaixo no *command* do R. 

`install.packages(c("readr", "readxl", "writexl", "dplyr", "stringr", "tidyr", "clpAPI", "DBI",`
`"RSQLite", "shiny", "shinythemes", "shinybusy", "tictoc", "plotly", "zoo", "scales", "showtext",`
`"jsonlite", "parallel", "foreach", "doParallel", "numbers"), repos = "https://cran.fiocruz.br")`

3. Ap&oacute;s a instala&ccedil;&atilde;o das depend&ecirc;ncias que est&atilde;o no CRAN, voc&ecirc; deve instalar o pacote do leitor de arquivos dos modelos de planejamento energ&eacute;tico do CEPEL. Este tamb&eacute;m &eacute; uma depend&ecirc;ncia, contudo, ele foi desenvolvido pela EPE e ainda n&atilde;o est&aacute; no CRAN. Para instalar esse pacote basta efetuar o download do pacote em zip mais recente em https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/leitor-dos-arquivos-de-entrada-e-saida-dos-modelos-do-planejamento-energetico-do-cepel e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip rec&eacute;m baixado.

4. Finalmente, podemos instalar o pacote do BP. Para instalar o pacote basta efetuar o download do pacote em tar.gz na parte de arquivos desta p&aacute;gina e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o tar.gz rec&eacute;m baixado. 

# Executando o BP

Ap&oacute;s ter o pacote instalado, basta usar as instru&ccedil;&otilde;es no *command* do R:

`library(epebpr)`    
`aplicacaoBalanco()` 

# Instru&ccedil;&otilde;es de uso da ferramenta 

Ap&oacute;s ter a ferramenta em execu&ccedil;&atilde;o basta clicar no &iacute;cone do livro ![](inst/appBalanco/www/imagens/logo-wiki.png){width=30px} para obter ajuda e toda a documenta&ccedil;&atilde;o necess&aacute;ria para entender a metodologia do BP e seu uso. 

Para a execu&ccedil;&atilde;o do balan&ccedil;o de pot&ecirc;ncia para o caso do PDE 29, foi disponibilizado na &aacute;rea de Arquivos o deck do PDE com os arquivos extras necess&aacute;rios ao BP (PDE29 - deck NEWAVE e entradas BP.zip) e as sa&iacute;das do Newave (PDE29 - nwlistop.zip). 

# Contato
Para coment&aacute;rios sobre a ferramenta, favor enviar e-mail para modelos.sgr@epe.gov.br.

# Arquivos 

# Vers&otilde;es

### v.0.11.0
Teste com inclus&atilde;o de funcionalidade que distribui o d&eacute;ficit entre os subsistemas por faixas.

### v.0.11.0
Inclus&atilde;o de funcionalidade que distribui o d&eacute;ficit entre os subsistemas. Essa funcionalidade n&atilde;o altera a quantidade total de d&eacute;ficit do SIN, somente tenta encontrar uma solu&ccedil;&atilde;o &oacute;tima equivalente a atual onde h&aacute; a possibilidade de n&atilde;o concentrar os d&eacute;ficits em um &uacute;nico subsistema.

### v.0.10.1
Corrige o download dos dados dos gr&aacute;ficos de GF.

### v.0.10.0
Corrige valor de LOLP para gr&aacute;ficos de GF.

Inclus&atilde;o de funcionalidade que l&ecirc; e passa a usar o percentual de reserva de carga por subsistema e m&ecirc;s e percentual de reserva por causa da incerteza na gera&ccedil;&atilde;o das renov&aacute;veis. Esse percentual tem como refer&ecirc;ncia a expectativa de gera&ccedil;&atilde;o por tipo de renov&aacute;vel, subsistema e m&ecirc;s. Inclui a tabela BPO_A21_RESERVA para gravar essas informa&ccedil;&otilde;es.

Passa a ler o arquivo infoMDI.txt para pegar o in&iacute;cio e fim do horizonte de estudo do MDI.

Passa a gravar o horizonte da demanda de acordo com o horizonte do caso NEWAVE.

### v.0.9.2
Verifica se os dados de mercado e patamar possuem os mesmo subsistemas e avisa caso negativo. Esse problema acontece em casos montados manualmente.

Apresenta novos gr&aacute;ficos de CVaR e VaR seguindo os crit&eacute;rios de contabiliza&ccedil;&atilde;o de Garantia F&iacute;sica.

### v.0.9.1
Corrige erro ocorrido na atualiza&ccedil;&atilde;o de submotoriza&ccedil;&atilde;o quando n&atilde;o havia expans&atilde;o hidr&aacute;ulica no horizonte do estudo.

Passa a contabilizar o n&uacute;mero de s&eacute;ries hist&oacute;ricas hidr&aacute;ulicas para simula&ccedil;&atilde;o a partir do in&iacute;cio de varredura da s&eacute;rie hist&oacute;rica.

Corrige gr&aacute;fico de LOLP para exibir anos que possuem risco anual exatamente igual a zero.

### v.0.9.0

Primeira vers&atilde;o divulgada 