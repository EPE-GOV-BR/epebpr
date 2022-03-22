# Introdução 

Balanço de potência: A ferramenta tem como objetivo avaliar os montantes de potência necessários nos momentos em que o sistema apresenta demanda máxima instant&acirc;nea com o objetivo de verificar as condiç&otilde;es de seu atendimento. Isto é, verificando a possibilidade de déficits e a auxiliando na avaliação de contratação de potência para o sistema. A ferramenta mantém a compatibilidade com a análise energética previamente efetuada.

A ferramenta de balanço de potência foi desenvolvida em linguagem de programação R e sua interface foi desenvolvida com Shiny, que é um pacote R que facilita a criação de aplicaç&otilde;es web diretamente do R e utiliza CSS, widgets, html e aç&otilde;es JavaScript para maior flexibilidade. A ferramenta também usa o pacote clpAPI, que é uma Interface R para a API em C do COIN-OR Clp.

# Instalando o BP 

1.  Para utilizar a ferramenta do Balanço de Potência é necessário que o R esteja instalado. Para tal, basta baixar a instalação e seguir as instruç&otilde;es localizadas, por exemplo, no "mirror" do CRAN da Fiocruz em https://cran.fiocruz.br

2.  Além do R, há a necessidade da instalação do Rtools, que é um conjunto de ferramentas importantes para a construção de novos pacotes. Para instalar basta acessar https://cran.r-project.org/bin/windows/Rtools/ para baixar o Rtools 4.0 para windows e depois executar o arquivo. 

2.  Com o R e o Rtools já instalados, execute o R e clique em Pacotes > Instalar pacote(s)... Selecione um "mirror" para baixar os pacotes. Dê preferência para os que estão no Brasil, pois o download será mais rápido. Após isso marque e instale os seguintes pacotes que serão necessários para execução do BP: readr, readxl, writexl, dplyr, stringr, tidyr, clpAPI, DBI, RSQLite, shiny, shinythemes, shinybusy, tictoc, plotly, zoo, scales, showtext, jsonlite, parallel, foreach, doParallel, numbers. Outra maneira de instalar as dependências é usando a instrução abaixo no *command* do R. 

`install.packages(c("readr", "readxl", "writexl", "dplyr", "stringr", "tidyr", "clpAPI", "DBI",`
`"RSQLite", "shiny", "shinythemes", "shinybusy", "tictoc", "plotly", "zoo", "scales", "showtext",`
`"jsonlite", "parallel", "foreach", "doParallel", "numbers"), repos = "https://cran.fiocruz.br")`

3. Após a instalação das dependências que estão no CRAN, você deve instalar o pacote do leitor de arquivos dos modelos de planejamento energético do CEPEL. Este também é uma dependência, contudo, ele foi desenvolvido pela EPE e ainda não está no CRAN. Para instalar esse pacote basta efetuar o download do pacote em zip mais recente em https://www.epe.gov.br/pt/publicacoes-dados-abertos/publicacoes/leitor-dos-arquivos-de-entrada-e-saida-dos-modelos-do-planejamento-energetico-do-cepel e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o zip recém baixado.

4. Finalmente, podemos instalar o pacote do BP. Para instalar o pacote basta efetuar o download do pacote em tar.gz na parte de arquivos desta página e, depois, no R, ir em Pacotes > Install package(s) from local files... e escolher o tar.gz recém baixado. 

# Executando o BP

Após ter o pacote instalado, basta usar as instruç&otilde;es no *command* do R:

`library(epebpr)`    
`aplicacaoBalanco()` 

# Instruç&otilde;es de uso da ferramenta 

Após ter a ferramenta em execução basta clicar no ícone do livro ![](inst/appBalanco/www/imagens/logo-wiki.png){width=30px} para obter ajuda e toda a documentação necessária para entender a metodologia do BP e seu uso. 

Para a execução do balanço de potência para o caso do PDE 29, foi disponibilizado na área de Arquivos o deck do PDE com os arquivos extras necessários ao BP (PDE29 - deck NEWAVE e entradas BP.zip) e as saídas do Newave (PDE29 - nwlistop.zip). 

# Contato
Para comentários sobre a ferramenta, favor enviar e-mail para modelos.sgr@epe.gov.br.

# Arquivos 

# Vers&otilde;es

### v.0.11.2
Correção da disponibilidade por step de déficit que antes não estava com mesma disponibilidade em cada step. Disponibilização de versão beta com leitura de PDISP_QMIN do SUISHI. A sa&iacute;da do SUISHI apresenta problemas com o valor de altura l&iacute;quida (QUED) e precisa ser revisto na fonte.

### v.0.11.1
Teste com inclusão de funcionalidade que distribui o déficit entre os subsistemas por faixas.

### v.0.11.0
Inclusão de funcionalidade que distribui o déficit entre os subsistemas. Essa funcionalidade não altera a quantidade total de déficit do SIN, somente tenta encontrar uma solução ótima equivalente a atual onde há a possibilidade de não concentrar os déficits em um &uacute;nico subsistema.

### v.0.10.1
Corrige o download dos dados dos gráficos de GF.

### v.0.10.0
Corrige valor de LOLP para gráficos de GF.

Inclusão de funcionalidade que l&ecirc; e passa a usar o percentual de reserva de carga por subsistema e m&ecirc;s e percentual de reserva por causa da incerteza na geração das renováveis. Esse percentual tem como refer&ecirc;ncia a expectativa de geração por tipo de renovável, subsistema e m&ecirc;s. Inclui a tabela BPO_A21_RESERVA para gravar essas informaç&otilde;es.

Passa a ler o arquivo infoMDI.txt para pegar o in&iacute;cio e fim do horizonte de estudo do MDI.

Passa a gravar o horizonte da demanda de acordo com o horizonte do caso NEWAVE.

### v.0.9.2
Verifica se os dados de mercado e patamar possuem os mesmo subsistemas e avisa caso negativo. Esse problema acontece em casos montados manualmente.

Apresenta novos gráficos de CVaR e VaR seguindo os critérios de contabilização de Garantia F&iacute;sica.

### v.0.9.1
Corrige erro ocorrido na atualização de submotorização quando não havia expansão hidráulica no horizonte do estudo.

Passa a contabilizar o n&uacute;mero de séries históricas hidráulicas para simulação a partir do in&iacute;cio de varredura da série histórica.

Corrige gráfico de LOLP para exibir anos que possuem risco anual exatamente igual a zero.

### v.0.9.0

Primeira versão divulgada 
