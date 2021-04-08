#' Exibe graficos de CVaR com criterios de GF
#'
#' Monta graficos de Profundidade de Deficit por CVAR de um caso especifico com criterios de GF
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 10: CVAR mensal; 11: VAR mensal; 12: VAR anual
#'
#' @return objeto do tipo plotly
#'
#' @export
dadosGraficosGF <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, tipoGrafico) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT
                      A16.A09_NR_SERIE as serie,
                      A16.A09_NR_MES as anoMes,
                      A02.A02_TX_DESCRICAO_SUBSISTEMA as subsistema,
                      A16_VL_DESPACHO AS defict,
                      A10.A10_VL_DEMANDA AS demanda
                    FROM 
                      BPO_A16_BALANCO AS A16,
                      BPO_A10_DEMANDA AS A10,
                      BPO_A02_SUBSISTEMAS AS A02
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT' AND 
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND 
                      A10.A10_NR_MES = A16.A09_NR_MES AND 
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A10.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A16.A02_NR_SUBSISTEMA = A10.A02_NR_SUBSISTEMA AND
                      A02.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A02.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A02.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # calcula risco Anual (LOLP)
  lolp <- tib.resultados %>% mutate(defict = ifelse(defict > 0, 1,0)) %>%
    summarise(lolp = sum(defict)/n(), .groups = "drop") %>% pull(lolp) %>% 
    percent(accuracy = 0.01, scale = 100, decimal.mark = ",", suffix = "%")
  
  # cria SIN e adiciona
  tib.resultadosSIN <- tib.resultados %>% group_by(serie, anoMes) %>% 
    summarise(defict = sum(defict), demanda = sum(demanda), .groups = "drop") %>% 
    mutate(subsistema = "SIN") %>% select(serie, anoMes, subsistema, defict, demanda)
  
  tib.resultados <- bind_rows(tib.resultados, tib.resultadosSIN)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% 
    mutate(profundidade = ifelse(is.nan(defict/demanda), 0, defict/demanda),
           mes = anoMes %% 100)
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  nomeMes <- (1:12 * 100 + 20000001) %>% as.character() %>% as.Date("%Y%m%d") %>% months(abbreviate = T)
  
  if (tipoGrafico == 10) {
    # calcula o CVaR por subsistema e mes
    tib.resultados <- tib.resultados %>%
      group_by(subsistema, mes) %>%
      summarise(cvar = cvar(profundidade, 0.05), .groups = "drop")
    
  } else if (tipoGrafico == 11) {
    # calcula o VaR por subsistema e mes
    tib.resultados <- tib.resultados %>%
      group_by(subsistema, mes) %>%
      summarise(var = var(defict, 0.05), .groups = "drop")
    
  } else {
    # calcula o VaR por subsistema
    tib.resultados <- tib.resultados %>%
      group_by(subsistema) %>%
      summarise(var = var(defict, 0.05), .groups = "drop")
  }
  
  return(tib.resultados)
}