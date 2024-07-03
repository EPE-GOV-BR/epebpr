#' Exporta os dados dos graficos de VaR
#'
#' Exporta os dados dos graficos de Profundidade de Deficit por VAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 1: Mensal por patamar; 2: Mensal em linha; 3:Anual
#'
#' @return tib.dadosGraficoVAR tibble com os dados do grafico de CVaR
#'
#' @export
dadosGraficoVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                            inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  squery <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT
                    FROM BPO_A16_BALANCO AS A16
                    WHERE 
                    A16.A16_TP_GERACAO = 'DEFICIT' AND
                    A16.A01_TP_CASO = ", tipoCaso," AND
                    A16.A01_NR_CASO = ", numeroCaso," AND
                    A16.A01_CD_MODELO = ", codModelo, " 
                    GROUP BY
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # calcula o CVAR por ano e mes
  tib.resultadosVarMes <- tib.resultados %>%
    dplyr::group_by(A09_NR_MES) %>%
    dplyr::summarise(#"var 1%" = var(DEFICIT, 0.01),
      "1var 1,5%" = var(DEFICIT, 0.015),
      #"var 2%" = var(DEFICIT, 0.02),
      "2var 2,5%" = var(DEFICIT, 0.025),
      "3var 5%" = var(DEFICIT, 0.05),
      "4var 10%" = var(DEFICIT, 0.1)) %>% 
    dplyr::ungroup()
  
  # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
  tib.resultadosVarMes <- tib.resultadosVarMes %>% 
    tidyr::gather(key = "tamanhoVAR", value = "var", -A09_NR_MES)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosVarMes <- tib.resultadosVarMes %>% 
    dplyr::mutate(anoMes = as.character(A09_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo var por tipo para possibilitar a identificacao dos maximos no grafico
  tib.resultadosVarMesMax <- tib.resultadosVarMes %>% 
    dplyr::group_by(tamanhoVAR) %>% 
    dplyr::summarise(maxVAR = max(var)) %>% 
    dplyr::ungroup()
  
  tib.resultadosVarMes <- dplyr::inner_join(tib.resultadosVarMes, tib.resultadosVarMesMax, by = "tamanhoVAR")
  
  if (tipoGrafico %in% c(5,6)) {
    tib.dadosGraficoVAR <- tib.resultadosVarMes
    
  } else {
    # var anual
    # cria coluna de ano e calcula o CVAR por ano
    tib.resultadosVarAno <- tib.resultados %>% 
      dplyr::mutate(ano = A09_NR_MES%/%100) %>% 
      dplyr::group_by(ano) %>%
      dplyr::summarise(# "var 1%" = var(DEFICIT, 0.01),
        "1var 1,5%" = var(DEFICIT, 0.015),
        # "var 2%" = var(DEFICIT, 0.02),
        "2var 2,5%" = var(DEFICIT, 0.025),
        "3var 5%" = var(DEFICIT, 0.05),
        "4var 10%" = var(DEFICIT, 0.1))
    
    # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
    tib.resultadosVarAno <- tib.resultadosVarAno %>% 
      tidyr::gather(key = "tamanhoVAR", value = "var", -ano) 
    
    # filtra o horizonte
    tib.resultadosVarAno <- tib.resultadosVarAno %>% 
      dplyr::filter(dplyr::between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
    
    tib.dadosGraficoVAR <- tib.resultadosVarAno
  }
  return(tib.dadosGraficoVAR)
}
