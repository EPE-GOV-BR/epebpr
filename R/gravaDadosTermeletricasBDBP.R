#' Gravacao dos dados das usinas termeletricas ao longo do horizonte de simulacao
#'
#' Faz a gravacao dos dados das usinas termeletricas ao longo do horizonte de simulacao do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A14_DISPONIBILIDADE_UTE do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param pastaSaidas localizacao dos arquivos de saida do modulo NWLISTOP
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A14_DISPONIBILIDADE_UTE
#'
#' @examples
#' \dontrun{
#' gravacaoDadosTermeletricasBDBP("C:/PDE2027_Caso080", "C:/PDE2027_Caso080/nwlistop", conexao, 1, 80, 1)}
#'
#' @export
gravacaoDadosTermeletricasBDBP <- function(pastaCaso, pastaSaidas, conexao, tipoCaso, numeroCaso, codModelo) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  if (missing(pastaSaidas)) {
    stop("favor indicar a pasta com os arquivos do NWLISTOP")
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
  
  # executa query para apagar da tabela BPO_A14_DISPONIBILIDADE_UTE os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  DBI::dbExecute(conexao, paste0("DELETE FROM BPO_A14_DISPONIBILIDADE_UTE
                              WHERE A01_TP_CASO = ", tipoCaso, 
                                 " AND A01_NR_CASO = ", numeroCaso, 
                                 " AND A01_CD_MODELO = ", codModelo))
  
  DBI::dbExecute(conexao, paste0("DELETE FROM BPO_A14B_DISPONIBILIDADE_UTE_GNL
                              WHERE A01_TP_CASO = ", tipoCaso, 
                              " AND A01_NR_CASO = ", numeroCaso, 
                              " AND A01_CD_MODELO = ", codModelo))
  
  # executa as funcoes de leitutra do pacote leitorrmpe para o carregamento dos dados das termeletricas
  # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
  df.consolidadoUTE <- leitorrmpe::consolidaTermeletricasnoHorizonte(pastaCaso) %>% 
    dplyr::mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    dplyr::select(A01_TP_CASO = tipoCaso, 
                  A01_NR_CASO = numeroCaso, 
                  A01_CD_MODELO = codModelo,
                  A02_NR_SUBSISTEMA = codSubsistema,
                  A14_NR_MES = anoMes,
                  A14_CD_USINA = codUsinaTermica,
                  A14_VL_POTENCIA = capacidaInstalada,
                  A14_VL_FATOR_CAPACIDADE = FCMaximo,
                  A14_VL_PERC_TEIF = TEIF,
                  A14_VL_PERC_IP = IP,
                  A14_VL_INFLEXIBILIDADE = GTMin,
                  A14_VL_DISPONIBILIDADE_MAXIMA_PONTA = PDISP,
                  A14_VL_CVU = CVU)
  
  # verifica quais UTEs tem despacho antecipado GNL
  uteGnl <- unique(leitorrmpe::leituraAntecipacaoGnl(pastaCaso)$codUsina)
  
  df.consolidadoUTEGnl <- df.consolidadoUTE %>% 
    dplyr::filter(A14_CD_USINA %in% uteGnl)
  
  # retira as UTEs com despacho antecipado GNL do df
  df.consolidadoUTE <- df.consolidadoUTE %>% 
    dplyr::filter(!A14_CD_USINA %in% uteGnl)
  
  # executa query para gravar os dados das termeletricas na tabela BPO_A14_DISPONIBILIDADE_UTE do BDBP
  DBI::dbWriteTable(conexao, "BPO_A14_DISPONIBILIDADE_UTE", df.consolidadoUTE, append = TRUE)
  
  # leitura dos arquivos gtert - geracao termeletrica
  gterm <- leitorrmpe::leituraGeracaoTermicaClassesTotal(pastaSaidas)
  
  # selecao dos cenarios de geracao termeletrica das usinas GNL no NEWAVE
  # se a geracao for maior que zero, tem disponibilidade, senao disponibilidade fica zerada
  df.consolidadoUTEGnl <- gterm %>% 
    dplyr::filter(codUsina %in% uteGnl) %>% 
    dplyr::group_by(codUsina, anoMes, serie) %>% 
    dplyr::reframe(geracao_mes = sum(geracao)) %>% 
    dplyr::left_join(df.consolidadoUTEGnl, by = c("codUsina" = "A14_CD_USINA", "anoMes" = "A14_NR_MES")) %>% 
    dplyr::mutate(A14_VL_DISPONIBILIDADE_MAXIMA_PONTA = dplyr::if_else(geracao_mes > 0, A14_VL_DISPONIBILIDADE_MAXIMA_PONTA, 0),
                  A14_VL_INFLEXIBILIDADE = dplyr::if_else(geracao_mes > 0, A14_VL_INFLEXIBILIDADE, 0)) %>% 
    dplyr::select(-geracao_mes) %>% 
    dplyr::rename(A14_CD_USINA = codUsina, A14_NR_MES = anoMes, A14_NR_SERIE = serie)
    
  # executa query para gravar os dados das termeletricas na tabela BPO_A14B_DISPONIBILIDADE_UTE_GNL do BDBP
  DBI::dbWriteTable(conexao, "BPO_A14B_DISPONIBILIDADE_UTE_GNL", df.consolidadoUTEGnl, append = TRUE)
  
  mensagem <- "tabelas BPO_A14_DISPONIBILIDADE_UTE e BPO_A14B_DISPONIBILIDADE_UTE_GNL gravadas com sucesso!"
  
  return(mensagem)
}
