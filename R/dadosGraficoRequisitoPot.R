#' Exporta os dados do grafico de requisitos de potencia
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#'
#' @return tib.requisitosPot tibble com os dados do grafico de Requisitos de Potencia
#'
#' @export
dadosRequisitoPot <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                              inicioHorizonteGrafico, fimHorizonteGrafico) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  squery <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT,
                    A10.DEMANDA,
                    A21.RESERVA_F,
                    A21.RESERVA_C
                  FROM 
                    BPO_A16_BALANCO AS A16,
                    ( SELECT 
                        A10_NR_MES,
                        A01_TP_CASO,
                        A01_NR_CASO,
                        A01_CD_MODELO,
                        SUM(A10_VL_DEMANDA) AS DEMANDA
                      FROM 
                        BPO_A10_DEMANDA
                      GROUP BY 
                        A10_NR_MES,
                        A01_TP_CASO,
                        A01_NR_CASO,
                        A01_CD_MODELO
                    ) AS A10,
                    ( SELECT 
                        A21_NR_MES,
                        A01_TP_CASO,
                        A01_NR_CASO,
                        A01_CD_MODELO,
                        SUM(A21_VL_RESERVA_CARGA) AS RESERVA_C,
                        SUM(A21_VL_RESERVA_FONTES) AS RESERVA_F
                      FROM 
                        BPO_A21_RESERVA
                      GROUP BY 
                        A21_NR_MES,
                        A01_TP_CASO,
                        A01_NR_CASO,
                        A01_CD_MODELO
                    ) AS A21
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT' AND
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND
                      A10.A10_NR_MES = A16.A09_NR_MES AND
                      A21.A21_NR_MES = A16.A09_NR_MES AND
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND
                      A21.A01_NR_CASO = A16.A01_NR_CASO AND
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A21.A01_CD_MODELO = A16.A01_CD_MODELO
                    GROUP BY
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES;")
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # calcula o requisito por mes
  tib.resultadosCvarMes <- tib.resultados %>%
    dplyr::group_by(A09_NR_MES, DEMANDA, RESERVA_F, RESERVA_C) %>%
    dplyr::summarise(cvar5 = cvar(DEFICIT, 0.05)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(limiteCriterio = (DEMANDA +  RESERVA_F + RESERVA_C)* 0.05, 
                  violacaoCriterio = ifelse(cvar5 > limiteCriterio, 
                                            cvar5 - limiteCriterio, 
                                            0), 
                  ano = A09_NR_MES%/%100) %>% 
    dplyr::select(A09_NR_MES, ano, violacaoCriterio)
  
  # calcula o VAR 5 por ano
  tib.resultadosVarAno <- tib.resultados %>% 
    dplyr::mutate(ano = A09_NR_MES%/%100) %>% 
    dplyr::group_by(ano) %>%
    dplyr::summarise(var5 = var(DEFICIT, 0.05), .groups = "drop")
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
    dplyr::mutate(anoMes = as.character(A09_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # cria tibble de requisitos fazendo o join
  tib.requisitosPot <- dplyr::left_join(tib.resultadosCvarMes, tib.resultadosVarAno, by = "ano")
  
  return(tib.requisitosPot)
}
