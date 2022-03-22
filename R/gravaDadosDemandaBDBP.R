#' Gravacao dos dados de demanda de ponta
#'
#' Faz a gravacao dos dados de demanda de ponta do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A10_DEMANDA do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A10_DEMANDA 
#'
#' @examples
#' \dontrun{
#' gravacaoDadosDemandaBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)}
#'
#' @export
gravacaoDadosDemandaBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo) {
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
  
  # executa query para apagar da tabela BPO_A10_DEMANDA os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  
  dbExecute(conexao, paste0("DELETE FROM BPO_A10_DEMANDA
                              WHERE A01_TP_CASO = ", tipoCaso, 
                            " AND A01_NR_CASO = ", numeroCaso, 
                            " AND A01_CD_MODELO = ", codModelo))
  
  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados da demanda de ponta (patamar = 1)
  # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
  # define a demanda para o patamar de ponta (demandaPonta = limiteInterligacao * profundidadeIntercambio)
  df.mercado <- leituraMercadoEnergia(pastaCaso)
  df.patamar <- filter(leituraDadosProfundidadePatamarCarga(pastaCaso), patamar == 1)
  
  # verifica se mercado e patamar estao com o mesmo horizonte de meses
  if (length(setdiff(unique(df.patamar$anoMes), unique(df.mercado$anoMes))) != 0 & tipoCaso != 2) {
    dbDisconnect(conexao)
    stop("Horizonte de mercado inferior ao horizonte dos patamares de carga!")
  }
  if (length(setdiff(unique(df.mercado$anoMes), unique(df.patamar$anoMes))) != 0) {
    dbDisconnect(conexao)
    stop("Horizonte dos patamares de carga inferior ao horizonte de mercado!")
  }
  
  # verifica se mercado e patamar estao com os mesmos subsistemas
  subsistemasConjuntos <- length(intersect(unique(df.mercado$codSubsistema), unique(df.patamar$codSubsistema)))
  if (length(unique(df.mercado$codSubsistema)) != subsistemasConjuntos | length(unique(df.patamar$codSubsistema)) != subsistemasConjuntos) {
    dbDisconnect(conexao)
    stop("Patamares de carga e mercado não possuem os mesmos subsistemas!")
  }
  
  # horizonte de simulacao, no formato anoMes (AAAAMM)
  horizonte <- definePeriodo(pastaCaso) %>% pull(anoMes)
  
  df.Demanda <- inner_join(df.mercado, df.patamar, by = c("anoMes", "codSubsistema")) %>% 
    mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo, numSequencialFrequencia = 1, valorFrequencia = 1, 
           demandaPonta = energiaMercado * profundidadeCarga) %>% 
    filter(between(anoMes, min(horizonte), max(horizonte))) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    select(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A02_NR_SUBSISTEMA = codSubsistema,
           A10_NR_MES = anoMes,
           A10_NR_SEQ_FREQUENCIA = numSequencialFrequencia,
           A10_VL_FREQUENCIA = valorFrequencia,
           A10_VL_DEMANDA = demandaPonta)
  
  # executa query para gravar os dados da demanda de ponta na tabela BPO_A10_DEMANDA do BDBP
  dbWriteTable(conexao, "BPO_A10_DEMANDA", df.Demanda, append = TRUE)
  
  mensagem <- "tabela BPO_A10_DEMANDA gravada com sucesso!"
  
  return(mensagem)
}
