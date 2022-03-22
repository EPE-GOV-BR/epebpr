#' Gravacao dos dados dos limites dos agrupamentos de intercambios ao longo do horizonte de simulacao
#'
#' Faz a gravacao dos dados dos limites dos agrupamento de intercambios ao longo do horizonte de simulacao do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO
#'
#' @examples
#' \dontrun{
#' gravacaoDadosLimitesAgrupIntercambioBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)}
#'
#' @export
gravacaoDadosLimitesAgrupIntercambioBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  if (missing(conexao)) {
    stop("favor indicar a conexão com o banco de dados")
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
  
  # executa query para apagar da tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  dbSendQuery(conexao, paste0("DELETE FROM BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO
                                WHERE A01_TP_CASO = ", tipoCaso, 
                              " AND A01_NR_CASO = ", numeroCaso, 
                              " AND A01_CD_MODELO = ", codModelo))
  
  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados dos limites dos agrupamentos de intercambios do patamar de ponta (1)
  # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
  df.LimitesAgrInt <- filter(leituraLimiteAgrupamentoInterligacoes(pastaCaso), patamar == 1) %>% 
    mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    select(A01_TP_CASO = tipoCaso, 
           A01_NR_CASO = numeroCaso, 
           A01_CD_MODELO = codModelo,
           A12_NR_MES = anoMes,
           A12_CD_AGRUPAMENTO = codAgrup,
           A12_VL_LIMITE_INTERCAMBIO = limiteAgrup)

  # executa query para gravar os dados de limites dos agrupamentos de intercambios na tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO do BDBP
  dbWriteTable(conexao, "BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO", df.LimitesAgrInt, append = TRUE)
  
  mensagem <- "tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO gravada com sucesso!"
  
  return(mensagem)
}
