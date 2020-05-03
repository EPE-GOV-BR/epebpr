#' Exporta os dados dos graficos de CVaR
#'
#' Exporta os dados dos graficos de Profundidade de Deficit por CVAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 1: Mensal por patamar; 2: Mensal em linha; 3:Anual
#'
#' @return tib.dadosGraficoCVAR tibble com os dados do grafico de CVaR
#'
#' @export
dadosGraficoCVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                             inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  squery <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT,
                    A10.DEMANDA 
                    FROM BPO_A16_BALANCO AS A16,
                    ( SELECT A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO,
                      SUM(A10_VL_DEMANDA) AS DEMANDA
                      FROM BPO_A10_DEMANDA
                      GROUP BY A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO
                    ) AS A10
                    WHERE 
                    A16.A16_TP_GERACAO = 'DEFICIT' AND
                    A16.A01_TP_CASO = ", tipoCaso," AND
                    A16.A01_NR_CASO = ", numeroCaso," AND
                    A16.A01_CD_MODELO = ", codModelo, " AND
                    A10.A10_NR_MES = A16.A09_NR_MES AND
                    A10.A01_TP_CASO = A16.A01_TP_CASO AND
                    A10.A01_NR_CASO = A16.A01_NR_CASO AND
                    A10.A01_CD_MODELO = A16.A01_CD_MODELO
                    GROUP BY
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as.tbl()
  dbDisconnect(conexao)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% mutate(PROFUNDIDADE = DEFICIT/DEMANDA)
  
  # calcula o CVAR por ano e mes
  tib.resultadosCvarMes <- tib.resultados %>%
    group_by(A09_NR_MES) %>%
    summarise(#"cvar 1%" = cvar(PROFUNDIDADE, 0.01),
      "1cvar 1,5%" = cvar(PROFUNDIDADE, 0.015),
      #"cvar 2%" = cvar(PROFUNDIDADE, 0.02),
      "2cvar 2,5%" = cvar(PROFUNDIDADE, 0.025),
      "3cvar 5%" = cvar(PROFUNDIDADE, 0.05),
      "4cvar 10%" = cvar(PROFUNDIDADE, 0.1)) %>% ungroup()
  
  # faz a transposicao dos dados de cvar por coluna para um campo de identificacao do cvar (1%; 1,5%; 2%; 5%...) e outro de valor de cvar
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% gather(key = "tamanhoCVAR", value = "cvar", -A09_NR_MES)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% mutate(anoMes = as.character(A09_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    filter(between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo cvar por tipo para possibilitar a identificacao dos maximos no grafico
  tib.resultadosCvarMesMax <- tib.resultadosCvarMes %>% group_by(tamanhoCVAR) %>% summarise(maxCVAR = max(cvar)) %>% ungroup()
  tib.resultadosCvarMes <- inner_join(tib.resultadosCvarMes, tib.resultadosCvarMesMax, by = "tamanhoCVAR")
  
  if (tipoGrafico %in% c(1,2)) {
    tib.dadosGraficoCVAR <- tib.resultadosCvarMes
    
  } else {
    # cvar anual
    # cria coluna de ano e calcula o CVAR por ano
    tib.resultadosCvarAno <- tib.resultados %>% mutate(ano = A09_NR_MES%/%100) %>% 
      group_by(ano) %>%
      summarise(# "cvar 1%" = cvar(PROFUNDIDADE, 0.01),
        "1cvar 1,5%" = cvar(PROFUNDIDADE, 0.015),
        # "cvar 2%" = cvar(PROFUNDIDADE, 0.02),
        "2cvar 2,5%" = cvar(PROFUNDIDADE, 0.025),
        "3cvar 5%" = cvar(PROFUNDIDADE, 0.05),
        "4cvar 10%" = cvar(PROFUNDIDADE, 0.1))
    
    # faz a transposicao dos dados de cvar por coluna para um campo de identificacao do cvar (1%; 1,5%; 2%; 5%...) e outro de valor de cvar
    tib.resultadosCvarAno <- tib.resultadosCvarAno %>% gather(key = "tamanhoCVAR", value = "cvar", -ano) 
    
    # filtra o horizonte
    tib.resultadosCvarAno <- tib.resultadosCvarAno %>% filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
    
    tib.dadosGraficoCVAR <- tib.resultadosCvarAno
  }
  return(tib.dadosGraficoCVAR)
}