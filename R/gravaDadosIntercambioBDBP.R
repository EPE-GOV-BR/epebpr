#' Gravacao dos dados dos limites de intercambio ao longo do horizonte de simulacao
#'
#' Faz a gravacao dos dados dos limites de intercambio ao longo do horizonte de simulacao do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A11_INTERCAMBIOS do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A11_INTERCAMBIOS
#'
#' @examples
#' \dontrun{
#' gravacaoDadosIntercambioBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)}
#'
#' @export
gravacaoDadosIntercambioBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo) {
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
    stop("favor indicar o código do modelo")
  }
  
  # executa query para apagar da tabela BPO_A11_INTERCAMBIOS os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  dbExecute(conexao, paste0("DELETE FROM BPO_A11_INTERCAMBIOS
                              WHERE A01_TP_CASO = ", tipoCaso, 
                            " AND A01_NR_CASO = ", numeroCaso, 
                            " AND A01_CD_MODELO = ", codModelo))

  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados dos limites e profundidade do patamar de intercambio
  # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
  # define o limite de intercambio para o patamar de ponta (limiteInterligacao * profundidadeIntercambio)
  df.DadosIntercambio <- inner_join(leituraLimiteInterligacoes(pastaCaso), 
                                    filter(leituraDadosProfundidadePatamarIntercambio(pastaCaso), patamar == 1), 
                                    by = c("anoMes", "codSubsistemaOrigem", "codSubsistemaDestino")) %>% 
    mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo, 
           limiteInterligacaoPonta = limiteInterligacao * profundidadeIntercambio) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    select(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A11_NR_SUBSISTEMA_ORIGEM = codSubsistemaOrigem,
           A11_NR_SUBSISTEMA_DESTINO = codSubsistemaDestino,
           A11_NR_MES = anoMes,
           A11_VL_LIMITE_INTERCAMBIO = limiteInterligacaoPonta)
  
  # executa query para gravar os dados dos limites de intercambio na tabela BPO_A11_INTERCAMBIOS do BDBP
  dbWriteTable(conexao, "BPO_A11_INTERCAMBIOS", df.DadosIntercambio, append = TRUE)
  
  mensagem <- "tabela BPO_A11_INTERCAMBIOS gravada com sucesso!"
  
  return(mensagem)
}
