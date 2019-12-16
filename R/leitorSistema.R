#' Leitor dos dados de sistema
#'
#' Faz a leitura do arquivo do NEWAVE com informacao de deficit do sistema (sistema.d**)
#'
#' @param arquivo nome e localizacao do arquivo do NEWAVE com dados de sistema
#' @return \code{df.sistema} data frame com informacoes de sistema
#' \itemize{
#'   \item codigo do sistema (\code{$codSistema})
#'   \item nome do sistema (\code{$nomeSistema})
#'   \item tipo de sistema (\code{$tipoFicticio}) - pode assumir 1 para ficticio, ou 0 para real
#'   \item custo de deficit em R$/MWh (\code{$deficit})
#'   }
#'
#' @import readr
#' @import dplyr
#' @import stringr
#' @import tidyr
#'
#' @examples
#' leituraSistema("C:/PDE2027_Caso080/sistema.d27")
#'
#' @export
leituraSistema <- function(arquivo) {
  if (!file.exists(arquivo)) {
    stop(paste0("arquivo ", arquivo, " n\u00E3o encontrado!"))
  }

  # le o arquivo sistema como um vetor de caracteres nx1
  sistema <- read_lines(arquivo, locale = locale(encoding = "latin1"), n_max = 100)
  # encontra o inicio da informacao
  inicioSistema <- which(str_detect(sistema, "XXX\\|XXXXXXXXXX\\|..\\|XXXX.XX XXXX.XX XXXX.XX XXXX.XX\\|X.XXX X.XXX X.XXX X.XXX\\|"))
  # encontra o fim da informacao
  fimSistema <- which(str_detect(sistema, "LIMITES DE INTERCAMBIO"))
  # filtra somente a parte do vetor que tem os dados de interesse
  sistemaTXT <- sistema[(inicioSistema+1):(fimSistema-2)]
  # recupera os tipos de sistema - 0 real, 1 ficticio
  tipoSistema <- str_sub(sistemaTXT, 16, 18) %>% as.numeric()
  # recupera a estrutura de dados quando sistema real (com campo de custo de defict)
  df.sistema <- data.frame(codSistema = as.numeric(str_sub(sistemaTXT[tipoSistema == 0], 1, 4)),
                           nomeSistema = str_sub(sistemaTXT[tipoSistema == 0], 6, 15),
                           tipoFicticio = as.numeric(str_sub(sistemaTXT[tipoSistema == 0], 16, 18)),
                           deficit = as.numeric(str_sub(sistemaTXT[tipoSistema == 0], 19, 26)))
  # recupera a estrutura de dados quando sistema ficticio
  df.sistemaFicticio <- data.frame(codSistema = as.numeric(str_sub(sistemaTXT[tipoSistema == 1], 1, 4)),
                                   nomeSistema = str_sub(sistemaTXT[tipoSistema == 1], 6, 15),
                                   tipoFicticio = as.numeric(str_sub(sistemaTXT[tipoSistema == 1], 16, 18)))
  df.sistemaFicticio$deficit <- NA
  # junta os data frames em um unico
  df.sistema <- rbind(df.sistema, df.sistemaFicticio)

  return(df.sistema)
}
