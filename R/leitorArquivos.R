#' Leitor dos nomes dos arquivos do NEWAVE
#'
#' Procura o arquivo caso.dat (nos formatos Caso.dat, caso.dat e CASO.DAT) e faz a leitura do arquivo do CEPEL
#' indicado nele (geralmente arquivos.dat) para recuperar a lista de arquivos de entrada e resultados utilizados pelo NEWAVE.
#'
#' @param pastaCaso caracter com localizacao do arquivo caso.dat.
#'
#' @return \code{df.arquivos} data frame com os arquivos utilizados pelo NEWAVE.
#' \itemize{
#' \item descricao (\code{$descricao})
#' \item nomes dos arquivos (\code{$arquivo})
#' }
#'
#' @importFrom readr read_lines read_delim
#' @import dplyr
#' @importFrom stringr str_squish
#'
#' @examples
#' leituraArquivos("C:/PDE2027_Caso080")
#'
#' @export
leituraArquivos <- function(pastaCaso) {
  if (!dir.exists(pastaCaso)) {
    stop(paste0("pasta ", pastaCaso, " n\u00E3o encontrada!"))
  }

  caso <- list.files(path = pastaCaso, pattern = "[cC]aso\\.dat|CASO\\.DAT")
  if (length(caso) != 1) {
    stop("caso.dat n\u00E3o encontrado!")
  }
  nomeArquivo <- read_lines(paste(pastaCaso, caso, sep = "/"), locale = locale(encoding = "latin1")) %>% str_squish()
  arquivo <- list.files(path = pastaCaso, pattern = nomeArquivo)
  if (length(arquivo) != 1) {
    stop(paste0(nomeArquivo, " n\u00E3o encontrado!"))
  }

  df.arquivos <- read_delim(paste(pastaCaso, arquivo, sep = "/"), delim = ":",
                            col_names = F, locale = locale(encoding = "latin1"), col_types = "cc")
  if (nrow(df.arquivos) == 0) {
    stop(nomeArquivo, " vazio")
  }
  colnames(df.arquivos) <- c("descricao","arquivo")
  # remove os espacos no inicio e fim da descricao e garante que estao em maiusculo
  df.arquivos <- df.arquivos %>% mutate(descricao = str_squish(toupper(descricao)),
                                        arquivo = str_squish(arquivo))

  return(df.arquivos)
}
