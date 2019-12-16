#' Leitor dos dados de submotorizacao por REE
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de submotorizacao por REE (relat.d**) e recupera esses valores por ano e mes
#'
#' @param arquivo nome e localizacao do arquivo do NEWAVE com dados de submotorizacao
#'
#' @return \code{df.submotorizacao} data frame com os valores de submotorizacao por REE
#' \itemize{
#' \item nome da REE (\code{$ree})
#' \item valor de ano e mes (\code{$anoMes})
#' }
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @examples
#' leituraSubmotorizacaoREE("C:/PDE2027_Caso080/relat.d27")
#'
#' @export
leituraSubmotorizacaoREE <- function(arquivo) {
  if (!file.exists(arquivo)) {
    stop(paste0("arquivo ", arquivo, " n\u00E3o encontrado!"))
  }

  # le o arquivo de entrada como um vetor de caracteres nx1
  dadosBrutos <- read_lines(arquivo, locale = locale(encoding = "latin1"))

  # encontra o inicio da informacao de submotorizacao
  inicioDados <- which(str_detect(dadosBrutos, "SUBMOTORIZACAO TOTAL POR REE"))
  if (length(inicioDados) == 0) {
    stop("localiza\u00E7\u00E3o da SUBMOTORIZACAO TOTAL POR REE n\u00E3o encontrada!")
  }
  # encontra todos o fins de paginas
  fimPagina <- which(str_detect(dadosBrutos, "\f"))
  # encontra o fim da submotorizacao para casos com restricao por REE
  fimPMO <- which(str_detect(dadosBrutos, "SUBMOTORIZACAO TOTAL COM RESTRICAO ELETRICA POR REE"))
  # encontra o fim da informacao de submotorizacao
  if (length(fimPMO) == 0) {
    fimDados <- min(fimPagina[fimPagina > inicioDados])
  } else {
    fimDados <- fimPMO
  }
  # filtra somente a parte do vetor que tem os dados de interesse
  submotorizacaoTXT <- dadosBrutos[inicioDados:fimDados]
  # encontra as REEs
  ree <- submotorizacaoTXT[which(str_detect(submotorizacaoTXT, "REE:"))] %>% str_remove("REE:") %>% str_trim()
  # localiza a posicao do inicio de dados de submotorizacao de cada REE
  inicioREE <- which(str_detect(submotorizacaoTXT, "ANO"))
  # localiza a posicao do fim de dados de submotorizacao de cada REE
  fimREE <- which(str_detect(submotorizacaoTXT, "X------------------------------------------------------------------------------------------X")) - 2

  # cria data frame de base para armazenar os dados de submotorizacao de todas as REEs
  df.submotorizacao <- tibble(ree = character(), anoMes = numeric(), submotorizacao = numeric())

  for (andaRee in 1:length(ree)) {
    # recupera dados, limpa e faz o "pivot" da tabela para dados normalizados (tidy)
    df.submotorizacaoRee <- submotorizacaoTXT[inicioREE[andaRee]:fimREE[andaRee]] %>% str_squish() %>% read_delim(" ") %>%
      pivot_longer(cols = -ANO, names_to = "mes", values_to = "submotorizacao") %>% mutate(ree = ree[andaRee], anoMes = (ANO * 100 + as.numeric(mes))) %>%
      select(ree, anoMes, submotorizacao)
    # concatena dados num data frame unico
    df.submotorizacao <- rbind(df.submotorizacao, df.submotorizacaoRee)
  }
  return(df.submotorizacao)
}
