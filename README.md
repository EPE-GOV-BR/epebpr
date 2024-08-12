
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Ferramenta de Balanço de Potência - Pacote epebpr

<!-- badges: start -->
<!-- badges: end -->

<img src="man/figures/logo.png" align="right" height="100" />

A ferramenta tem como objetivo avaliar os montantes de potência
necessários nos momentos em que o sistema apresenta demanda máxima
instantânea com o objetivo de verificar as condições de seu atendimento.
Isto é, verificando a possibilidade de déficits e a auxiliando na
avaliação de contratação de potência para o sistema. A ferramenta mantém
a compatibilidade com a análise energética previamente efetuada.

A ferramenta de balanço de potência foi desenvolvida em linguagem de
programação R e sua interface foi desenvolvida com Shiny, que é um
pacote R que facilita a criação de aplicações web diretamente do R e
utiliza CSS, widgets, html e ações JavaScript para maior flexibilidade.
A ferramenta também usa o pacote highs, que é uma Interface R para o
[HiGHS - high performance software for linear
optimization](https://highs.dev/).

## Instalação

Você pode instalar o pacote através do repositório da EPE no GitHub:

``` r
devtools::install_github("EPE-GOV-BR/epebpr")
```

### Dependências

Os seguintes pacotes do CRAN são necessários para a execução do BP:

``` r
install.packages(shiny, shinythemes, shinybusy, readr, readxl, writexl, dplyr,  stringr, stringi, tidyr, highs, DBI, RSQLite, tictoc, plotly, zoo, scales, parallel, foreach, doParallel, magrittr, ggplot2, lubridate, cellranger)
```

Além disso, é necessária a instalação do pacote `leitorrmpe`, também
desenvolvido pela EPE, para a leitura de arquivos dos modelos de
planejanemto energético do CEPEL. Ele pode ser baixado através deste
[link](https://github.com/EPE-GOV-BR/leitorrmpe).

### Excutando o BP

``` r
library(epebpr)

aplicacaoBalanco()
```

### Dúvidas e Sugestões

Para dúvidas e sugestões, entrar em contato através do e-mail
<modelos.sgr@epe.gov.br>
