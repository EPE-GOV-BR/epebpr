#' Exibe graficos de risco de defict anual
#'
#' Monta graficos de risco de defict de um caso especifico (LOLP)
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGrafico vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoRiscoDeficitAnual <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico, 
                                     tituloGrafico = paste0("Risco de Déficit (LOLP) - Caso ", numeroCaso)) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  query <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT,
                    A10.DEMANDA,
                    A01.SERIES
                   FROM 
                    BPO_A16_BALANCO AS A16,
                    (SELECT A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO,
                      SUM(A10_VL_DEMANDA) AS DEMANDA
                      FROM BPO_A10_DEMANDA
                      GROUP BY A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO
                    ) AS A10,
                    (SELECT A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO,
                      A01_NR_SERIES_HIDRO AS SERIES FROM
                      BPO_A01_CASOS_ANALISE
                    ) AS A01
                   WHERE 
                    A16.A16_TP_GERACAO = 'DEFICIT' AND
                    A16.A01_TP_CASO = ", tipoCaso," AND
                    A16.A01_NR_CASO = ", numeroCaso," AND
                    A16.A01_CD_MODELO = ", codModelo, " AND
                    A10.A10_NR_MES = A16.A09_NR_MES AND
                    A10.A01_TP_CASO = A16.A01_TP_CASO AND
                    A10.A01_NR_CASO = A16.A01_NR_CASO AND
                    A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                    A01.A01_TP_CASO = A16.A01_TP_CASO AND
                    A01.A01_NR_CASO = A16.A01_NR_CASO AND
                    A01.A01_CD_MODELO = A16.A01_CD_MODELO
                   GROUP BY 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, query) %>% as_tibble()
  dbDisconnect(conexao)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultados <- tib.resultados %>% 
    mutate(anoMes = as.character(A09_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date(),
           mes = A09_NR_MES %% 100, ano = A09_NR_MES %/% 100) %>% 
    filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # calcula o risco de defict anual (LOLP)
  tib.resultados <- tib.resultados %>% 
    mutate(DEFICIT = ifelse(DEFICIT > 0, 1,0)) %>%
    group_by(ano) %>% summarise(riscoAnual = sum(DEFICIT)/max(SERIES)/12, .groups = "drop")
  
  # exibe grafico de risco
  graficoRisco <- plot_ly(data = tib.resultados, x = ~ano, y = ~riscoAnual, name = "", type = "bar", showlegend = F,
                          textposition = 'outside', texttemplate = "<b>%{y:.1%}</b>",
                          # <extra></extra> remove o trece do hover
                          hovertemplate = "<b>Risco de Déficit</b>: %{y:.1%}<br><b>Ano</b>: %{x}<extra></extra>") %>%
    add_trace(tib.resultados, x = ~ano, y = 0.05, type = 'scatter', mode = 'lines', color = I("red"),
              hovertemplate = "<b>Limite de critério de suprimento: %{y:.0%}<extra></extra>") %>%
    layout( 
      title = paste0("<b>", tituloGrafico, "</b>"),
      yaxis = list( 
        title = "<b>Risco de Déficit</b>", 
        tickformat = "%"),
      xaxis = list(
        type = 'category')
    )
}
