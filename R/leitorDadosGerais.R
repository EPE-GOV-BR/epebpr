#' Leitor dos dados gerais
#'
#' Faz a leitura do arquivo do NEWAVE com dados gerais (gder.*) e recupera dados necessarios para o balanco.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 25.0.1 de setembro/2019 - paginas 17 - 25
#'
#' @param arquivo nome e localizacao do arquivo do NEWAVE com dados gerais
#'
#' @return \code{df.dadosGerais} data frame com informacao sobre o estudo
#' \itemize{
#' \item numero de anos de duracao do estudo(\code{$duracaoEstudo})
#' \item mes do inicio do estudo (\code{$mesInicio})
#' \item ano do inicio do estudo (\code{$anoInicio})
#' \item numero de anos iniciais para fins de estabilizacao no calculo da politica (\code{$anosPre})
#' \item numero de anos finais para fins de estabilizacao no calculo da politica (\code{$anosPos})
#' \item numero de series sinteticas (\code{$seriesSinteticas})
#' \item valor definindo tipo de simuacao final (0 nao simula; 1 serie sintetica; 2 serie historica; 3 consistencia de dados) (\code{$tipoSimulacao})
#' }
#'
#' @importFrom readr read_lines
#' @importFrom stringr str_sub
#' @importFrom magrittr %>%
#'
#' @examples
#' leituraDadosGerais("C:/PDE2027_Caso080/dger.d27")
#'
#' @export
leituraDadosGerais <- function(arquivo) {
  if (!file.exists(arquivo)) {
    stop(paste0("arquivo ", arquivo, " n\u00E3o encontrado!"))
  }

  # le o arquivo dger como um vetor de caracteres nx1
  dadosBrutos <- read_lines(arquivo, locale = locale(encoding = "latin1"), n_max = 100)

  # data frame
  df.dadosGerais <- data.frame(duracaoEstudo = 0)
  df.dadosGerais$duracaoEstudo <- str_sub(dadosBrutos[4], 22, 25) %>% as.numeric()
  df.dadosGerais$mesInicio <- str_sub(dadosBrutos[6], 22, 25) %>% as.numeric()
  df.dadosGerais$anoInicio <- str_sub(dadosBrutos[7], 22, 25) %>% as.numeric()
  df.dadosGerais$anosPre <- str_sub(dadosBrutos[8], 22, 25) %>% as.numeric()
  df.dadosGerais$anosPos <- str_sub(dadosBrutos[9], 22, 25) %>% as.numeric()
  df.dadosGerais$seriesSinteticas <- str_sub(dadosBrutos[19], 22, 25) %>% as.numeric()
  df.dadosGerais$tipoSimulacao <- str_sub(dadosBrutos[27], 22, 25) %>% as.numeric()

  return(df.dadosGerais)
}
