#' Leitor dos dados dos Reservatorios Equivalentes de Energia (REE)
#'
#' Faz a leitura do arquivo do NEWAVE com informacao dos REE (ree.dat) e recupera o codigo, nome e sistema de cada REE cadastrada
#'
#' @param arquivo nome e localizacao do arquivo do NEWAVE com dados de REE
#' @return \code{df.ree} data frame com os dados dos REE
#' \itemize{
#'   \item codigo do REE (\code{$codREE})
#'   \item nome (\code{$nomeREE})
#'   \item codigo do sistema do REE (\code{$codSistema})
#'   }
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @examples
#' leituraREE("C:/PDE2027_Caso080/ree.dat")
#'
#' @export
leituraREE <- function(arquivo) {
  if (!file.exists(arquivo)) {
    stop(paste0("arquivo ", arquivo, " n\u00E3o encontrado!"))
  }

  # le o arquivo como um vetor de caracteres nx1
  dadosBrutos <- read_lines(arquivo, locale = locale(encoding = "latin1"))
  # encontra o inicio da informacao
  inicioDados <- which(str_detect(dadosBrutos, "XXX\\|XXXXXXXXXX\\|  XXX"))
  if (length(inicioDados) == 0) {
    stop("localiza\u00E7\u00E3o de in\u00EDcio de dados n\u00E3o encontrada!")
  }
  # encontra o fim da informacao
  fimDados <- which(str_detect(dadosBrutos, "999"))
  # filtra somente a parte do vetor que tem os dados de interesse
  dados <- dadosBrutos[(inicioDados+1):(fimDados-1)]
  # recupera a estrutura de dados
  df.ree <- data.frame(codREE = as.numeric(str_sub(dados, 1, 4)),
                       nomeREE = str_squish(str_sub(dados, 6, 15)),
                       codSistema = as.numeric(str_sub(dados, 17, 21)))
  return(df.ree)
}
