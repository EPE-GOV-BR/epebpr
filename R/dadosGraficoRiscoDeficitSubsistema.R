#' Exporta os dados dos graficos de risco de defict por subsistema
#'
#' Exporta os dados dos graficos de risco de defict por subsistema de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGrafico vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return tib.resultadosRisco tibble com os dados do grafico de risco
#'
#' @export
dadosGraficoRiscoDeficitSubs <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico, 
                                         tituloGrafico = paste0("Risco de Déficit por Subsistema - Caso ", numeroCaso)) {
  
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
                      A16.A16_TP_GERACAO = 'DEFICIT_R' AND
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
  
  # filtra subsistemas sem demanda
  filtroSubsistemaSemDemanda <- tib.resultados %>% 
    dplyr::group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% 
    dplyr::summarise(demanda = sum(DEMANDA), .groups = "drop") %>% 
    dplyr::filter(demanda > 0) %>% 
    dplyr::pull(A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.resultados <- tib.resultados %>% 
    dplyr::filter(A02_TX_DESCRICAO_SUBSISTEMA %in% filtroSubsistemaSemDemanda)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultados <- tib.resultados %>% 
    dplyr::mutate(anoMes = as.character(A09_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date(),
                  mes = A09_NR_MES %% 100, ano = A09_NR_MES %/% 100) %>% 
    dplyr::rename(subsistema = A02_TX_DESCRICAO_SUBSISTEMA) %>% 
    dplyr::filter(dplyr::between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # calcula o risco mensal para os meses com defict
  tib.resultadosMes <- tib.resultados %>% 
    dplyr::mutate(DEFICIT = ifelse(DEFICIT > 0, 1,0)) %>% 
    dplyr::group_by(subsistema, ano, mes, anoMes) %>% 
    dplyr::reframe(riscoMensal = sum(DEFICIT)/n())
  
  # calcula o risco de defict anual
  tib.resultadosAno <- tib.resultados %>% 
    dplyr::mutate(DEFICIT = ifelse(DEFICIT > 0, 1,0)) %>%
    dplyr::group_by(subsistema, ano) %>% 
    dplyr::reframe(riscoAnual = sum(DEFICIT)/n())
  
  # efetua join entre as tibbles de mes e ano para se ter o risco anual na tibble mensal
  tib.resultadosRisco <- dplyr::inner_join(tib.resultadosMes, tib.resultadosAno, by = c("ano", "subsistema"))
  
  return(tib.resultadosRisco)
}
