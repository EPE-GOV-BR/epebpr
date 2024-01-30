#' Exporta os dados dos graficos de risco de defict
#'
#' Exporta os dados dos graficos de risco de defict de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#'
#' @return tib.resultadosCvarMes tibble com os dados do grafico de CVaR por subsistema
#'
#' @export
dadosGraficoCVARSubsistema <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT 
                      A02.A02_TX_DESCRICAO_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES,
                      SUM(A16_VL_DESPACHO) AS DEFICIT,
                      A10.DEMANDA 
                    FROM 
                      BPO_A16_BALANCO AS A16,
                      ( SELECT 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO,
                          SUM(A10_VL_DEMANDA) AS DEMANDA
                        FROM BPO_A10_DEMANDA
                        GROUP BY 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO
                      ) AS A10,
                      BPO_A02_SUBSISTEMAS AS A02
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT' AND
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND
                      A10.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA AND
                      A10.A10_NR_MES = A16.A09_NR_MES AND
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND
                      A10.A01_NR_CASO = A16.A01_NR_CASO AND
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A02.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A02.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA
                    GROUP BY
                      A16.A02_NR_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES;")
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% 
    dplyr::mutate(PROFUNDIDADE = ifelse(is.nan(DEFICIT/DEMANDA), 0, DEFICIT/DEMANDA))
  
  # filtra subsistemas sem demanda
  filtroSubsistemaSemDemanda <- tib.resultados %>% 
    dplyr::group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% 
    dplyr::summarise(demanda = sum(DEMANDA), .groups = "drop") %>% 
    dplyr::filter(demanda > 0) %>% 
    dplyr::pull(A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.resultados <- tib.resultados %>% 
    dplyr::filter(A02_TX_DESCRICAO_SUBSISTEMA %in% filtroSubsistemaSemDemanda)
  
  # calcula o CVAR por subsistema, ano e mes
  tib.resultadosCvarMes <- tib.resultados %>%
    dplyr::group_by(A02_TX_DESCRICAO_SUBSISTEMA, A09_NR_MES) %>%
    dplyr::summarise(cvar = cvar(PROFUNDIDADE, 0.05), .groups = 'drop')
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
    dplyr::mutate(anoMes = as.character(A09_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
    dplyr::select(-anoMes)
  
  return(tib.resultadosCvarMes)
}
