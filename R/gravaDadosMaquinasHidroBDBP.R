#' Gravacao dos dados referentes aos conjuntos e maquinas das usinas hidreletricas
#'
#' Faz a gravacao dos dados referentes aos conjuntos e maquinas das usinas hidreletricas do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A04_MAQUINAS_UHE do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A04_MAQUINAS_UHE 
#'
#' @examples
#' \dontrun{
#' gravacaoDadosMaquinasHidroBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)}
#'
#' @export
gravacaoDadosMaquinasHidroBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo) {
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
  
  # executa query para apagar da tabela BPO_A04_MAQUINAS_UHE os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  DBI::dbExecute(conexao, paste0("DELETE FROM BPO_A04_MAQUINAS_UHE
                              WHERE A01_TP_CASO = ", tipoCaso, 
                                 " AND A01_NR_CASO = ", numeroCaso, 
                                 " AND A01_CD_MODELO = ", codModelo))
  
  # executa as funcoes de leitura do pacote leitorrcepel para o carregamento dos dados das usinas hidreletricas
  lt.dadosUsinasHidro <- leitorrmpe::leituraDadosUsinasHidro(pastaCaso) 
  df.dadosConfiguracao <- lt.dadosUsinasHidro$df.dadosConfiguracao %>% 
    dplyr::select(-nomeUsina)
  
  df.configuracaoHidro <- leitorrmpe::leituraConfiguracaoHidro(pastaCaso) %>% 
    dplyr::select(codUsina, nomeUsina, codREE, idUsinaExistente, idModificacaoUsina)
  
  lt.alteracaoDadosUsinasHidro <- leitorrmpe::leituraAlteracaoDadosUsinasHidro(pastaCaso)
  df.alteracaoConjunto <- lt.alteracaoDadosUsinasHidro$df.alteracaoConjunto %>% 
    dplyr::rename(potenciaUnitaria = potenciaEfetiva)
  
  lt.dadosExpansaoHidro <- leitorrmpe::leituraDadosExpansaoUsinasHidro(pastaCaso)
  df.dadosExpansaoHidroTempo <- lt.dadosExpansaoHidro$df.dadosExpansaoHidroTempo %>% 
    dplyr::select(-nomeUsina)
  
  df.dadosGerais <- leitorrmpe::leituraDadosGerais(pastaCaso)
  
  # define o dataframe com as informacoes das maquinas por conjunto de cada hidreletrica
  # as informacoes sao definidas apenas para o inicio do horizonte de simulacao
  df.MaqUHE <- dplyr::inner_join(df.dadosConfiguracao, df.configuracaoHidro, by = c("codUsina")) %>% 
    dplyr::filter((!stringr::str_detect(nomeUsina, "FIC ") & !stringr::str_detect(nomeUsina, "FICT"))) %>% 
    dplyr::select(codUsina,idUsinaExistente,idModificacaoUsina,conjunto,numeroMaquinas,potenciaUnitaria,quedaEfetiva) %>% 
    dplyr::mutate(aux = 1) %>% dplyr::inner_join(dplyr::mutate(leitorrmpe::definePeriodo(pastaCaso), aux = 1), by = c("aux")) %>% 
    dplyr::left_join(df.alteracaoConjunto, by = c("codUsina","conjunto","anoMes")) %>% 
    dplyr::left_join(df.dadosExpansaoHidroTempo, by = c("codUsina","conjunto","anoMes")) %>% 
    dplyr::mutate(numeroMaquinasFinal = ifelse((idUsinaExistente=="NC"), 
                                               0, 
                                               ifelse(!is.na(numeroMaquinas.y) & idModificacaoUsina==1 & is.na(numeroMaquinas), 
                                                      numeroMaquinas.y, 
                                                      ifelse((idUsinaExistente=="EE" | idUsinaExistente=="NE"), 
                                                             ifelse(!is.na(numeroMaquinas), 
                                                                    numeroMaquinas, 
                                                                    0),
                                                             numeroMaquinas.x)))) %>% 
    dplyr::mutate(potenciaUnitariaFinal = ifelse((idUsinaExistente=="NC"), 
                                                 0, 
                                                 ifelse(!is.na(potenciaUnitaria.y) & idModificacaoUsina==1 & is.na(potenciaUnitaria), 
                                                        potenciaUnitaria.y, 
                                                        ifelse((idUsinaExistente=="EE" | idUsinaExistente=="NE"), 
                                                               ifelse(!is.na(potenciaUnitaria), 
                                                                      potenciaUnitaria, 
                                                                      potenciaUnitaria.x),
                                                               potenciaUnitaria.x)))) %>% 
    dplyr::filter(anoMes ==df.dadosGerais$anoInicio*100+df.dadosGerais$mesInicio) %>% 
    dplyr::mutate(potenciaUnitariaFinal=ifelse(numeroMaquinasFinal==0,0,potenciaUnitariaFinal)) %>% 
    dplyr::filter(!(numeroMaquinasFinal == 0 & conjunto != 1)) %>% 
    dplyr::mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    dplyr::select(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A03_CD_USINA = codUsina,
                  A04_NR_CONJUNTO = conjunto,
                  A04_NR_MAQUINAS = numeroMaquinasFinal,
                  A04_VL_POTENCIA = potenciaUnitariaFinal,
                  A04_VL_ALTURA_REFERENCIA = quedaEfetiva)
  
  # executa query para gravar os dados referentes aos conjuntos e maquinas das usinas hidreletricas na tabela BPO_A04_MAQUINAS_UHE do BDBP
  DBI::dbWriteTable(conexao, "BPO_A04_MAQUINAS_UHE", df.MaqUHE, append = TRUE)
  
  mensagem <- "tabela BPO_A04_MAQUINAS_UHE gravada com sucesso!"
  
  return(mensagem)
}
