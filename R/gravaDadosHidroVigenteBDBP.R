#' Gravacao dos dados das usinas hidreletricas ao longo do horizonte de simulacao
#'
#' Faz a gravacao dos dados das usinas hidreletricas ao longo do horizonte de simulacao do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A05_DADOS_VIGENTES_UHE do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE e auxliares do BP.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' 
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A05_DADOS_VIGENTES_UHE
#'
#' @examples
#' \dontrun{
#' gravacaoDadosHidroVigenteBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)
#' }
#'
#' @export
gravacaoDadosHidroVigenteBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do BP")
  }
  if (missing(conexao)) {
    stop("favor indicar a conex\u00E3o com o banco de dados")
  }
  if (missing(tipoCaso)) {
    stop("favor indicar tipo do caso")
  }
  if (missing(numeroCaso)) {
    stop("favor indicar o n\u00FAmero do caso")
  }
  if (missing(codModelo)) {
    stop("favor indicar o c\u00F3digo do modelo")
  }
  
  # executa query para apagar da tabela BPO_A05_DADOS_VIGENTES_UHE os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  DBI::dbExecute(conexao, paste0("DELETE FROM BPO_A05_DADOS_VIGENTES_UHE
                              WHERE A01_TP_CASO = ", tipoCaso, 
                                 " AND A01_NR_CASO = ", numeroCaso, 
                                 " AND A01_CD_MODELO = ", codModelo))
  
  # executa as funcoes de leitutra do pacote leitorrmpe para o carregamento dos dados das termeletricas
  # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
  
  df.consolidadoUHE <- leitorrmpe::consolidaHidreletricasnoHorizonte(pastaCaso) %>% 
    dplyr::mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo, potenciaEfetivaUsinaExp = 0) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    dplyr::select(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A03_CD_USINA = codUsina,
                  A05_NR_MES = anoMes,
                  A02_NR_REE = codREE,
                  A05_NR_CANAL_FUGA_MEDIO = canalFugaFinal,
                  A05_VL_VOL_MAX = volumeMaximoFinal,
                  A05_VL_VOL_MIN = volumeMinimoFinal,
                  A05_VL_VAZAO_MINIMA = vazaoMinimaFinal,
                  A05_NR_CONJUNTOS = numeroConjuntos,
                  A05_VL_TEIF = TEIF,
                  A05_VL_IP = IP,
                  A05_VL_POTENCIA = potenciaEfetivaUsina,
                  A05_VL_POTENCIA_EXPANSAO = potenciaEfetivaUsinaExp)
  
  # executa query para gravar os dados referentes aos conjuntos e maquinas das usinas hidreletricas na tabela BPO_A05_DADOS_VIGENTES_UHE do BDBP
  DBI::dbWriteTable(conexao, "BPO_A05_DADOS_VIGENTES_UHE", df.consolidadoUHE, append = TRUE)
  
  mensagem <- "tabela BPO_A05_DADOS_VIGENTES_UHE gravada com sucesso!"
  
  return(mensagem)
}
