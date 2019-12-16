#' Leitor de dados de configuracao hidroeletrica
#'
#' Faz a leitura do arquivo do NEWAVE com dados de configuracao hidroeletrica (confhd.*) e recupera dados necessarios para o balanco.
#' Usa como referencia para a leitura do arquivo as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 25.0.1 de setembro/2019 - paginas 67 e 68
#'
#' @param arquivo nome e localizacao do arquivo do NEWAVE com dados de configuracao hidroeletrica
#'
#' @return \code{df.dadosConfiguracaoHidro} data frame com dados de configuracao hidroeletrica
#' \itemize{
#' \item codigo da usina no cadastro de usinas hidroeletricas (\code{$codUsina})
#' \item nome da usina (\code{$nomeUsina})
#' \item numero do posto de vazoes da usina (\code{$posto})
#' \item codigo da usina jusante no cadastro de usinas hidroeletricas (\code{$codUsinaJusante})
#' \item codigo do REE (\code{$codREE})
#' \item volume armazenado inicial em percentagem do volume util (\code{$volumeInical})
#' \item indicador de usina existente e/ou em expansao (EX existente; EE existente com expansao; NE nao existente; NC nao considerada)
#' (\code{$idUsinaExistente})
#' \item indice de modificacao de dados da usina (0 nao modifica, 1 modifica) (\code{$idModficacaoUsina})
#' \item primeiro ano do historico de vazoes do posto correspondente a usina (\code{$inicioHistorico})
#' \item ultimo ano do historico de vazoes do posto correspondente a usina (\code{$fimHistorico})
#' \item tecnologia da usina para efeito de calculo de emissoes de GEE (\code{$tecnologia})
#' }
#'
#' @importFrom readr read_fwf
#'
#' @examples
#' leituraConfiguracaoHidro("C:/PDE2027_Caso080/confhd.d27")
#'
#' @export
leituraConfiguracaoHidro <- function(arquivo) {
  if (!file.exists(arquivo)) {
    stop(paste0("arquivo ", arquivo, " n\u00E3o encontrado!"))
  }

  # posicoes e nomes de acordo com manual do NEWAVE
  df.dadosConfiguracaoHidro <- read_fwf(arquivo,
                                        col_positions = fwf_positions(c(2, 7, 20, 26, 31, 36, 45, 50, 59, 68, 74), # vetor com as posicoes iniciais de cada campo
                                                                      c(5, 18, 23, 29, 34, 41, 46, 53, 62, 71, 76), # vetor com as posicoes finais de cada campo
                                                                      c("codUsina", "nomeUsina", "posto", "codUsinaJusante", "codREE", "volumeInical",
                                                                        "idUsinaExistente", "idModficacaoUsina", "inicioHistorico", "fimHistorico", "tecnologia")),
                                        col_types = cols(codUsina = col_double(), nomeUsina = col_character(), posto = col_double(), codUsinaJusante = col_double(),
                                                         codREE = col_double(), volumeInical = col_double(), idUsinaExistente = col_character(),
                                                         idModficacaoUsina = col_double(), inicioHistorico = col_double(), fimHistorico = col_double(),
                                                         tecnologia = col_character()),
                                        skip = 2)


  return(df.dadosConfiguracaoHidro)
}
