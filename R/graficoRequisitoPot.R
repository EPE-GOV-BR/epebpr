#' Exibe grafico de Requisito de Potencia
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoRequisitosPot <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  
  squery <- paste0("SELECT 
                    A29_NR_MES, 
                    A29_VL_VIOLACAO_CRITERIO, 
                    A29_VL_LIMITE_CRITERIO 
                    FROM BPO_A29_REQUISITOS_POTENCIA
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.requisitosPot <- tib.resultados %>% 
    dplyr::mutate(anoMes = as.character(A29_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.requisitosPot %>% 
    dplyr::filter(months(anoMes) %in% c("janeiro","julho")) %>% 
    dplyr::pull(anoMes) %>% c(., max(tib.requisitosPot$anoMes))
  
  # cria sequencia de datas para desenhar a linha de limite de criterio de sup. tem um mes antes e um depois para atravessar todo o grafico
  primeiroMes <- min(tib.requisitosPot$anoMes) %>% zoo::as.yearmon()
  ultimoMes <- max(tib.requisitosPot$anoMes) %>% zoo::as.yearmon()
  mesesLinha <- seq(primeiroMes - 1/12, ultimoMes + 1/12, 1/12) %>% format("%Y-%m-%d")
  
  graficoReqPot <- plotly::plot_ly(data = tib.requisitosPot, x = ~anoMes, y = ~A29_VL_VIOLACAO_CRITERIO, name = "", type = "bar", color = I("gray"), showlegend = T,
                                   hovertemplate = "<b>Requisitos de pot\u00EAncia calculados para m\u00E9tricas CVaR5%(PNS) <= 5 [%Dem]</b>: %{y:.2f} MW<extra></extra>") %>% 
    plotly::add_trace(tib.requisitosPot, x = ~anoMes, y = ~A29_VL_LIMITE_CRITERIO, type = 'scatter', mode = 'lines', color = I("orange"), line = list(dash = "dash"),
                      hovertemplate = "<b>Requisito para LOLP <= 5%</b>: %{y:.2f} MW<extra></extra>") %>% 
    plotly::layout( 
      title = "<b>Requisito de Pot\u00EAncia</b>",
      legend = list(orientation = 'h', x = "0.3"),
      yaxis = list( 
        title = "<b>Requisito de Pot\u00EAncia para o SIN [MW]</b>", 
        tickformat = "d" 
      ), 
      xaxis = list( 
        title = "<b>M\u00EDs</b>", 
        ticktext = as.list(as.character(zoo::as.yearmon(marcasEixoMes))), 
        tickvals = as.list(marcasEixoMes)#,
      )
    ) %>%
    plotly::style(name = "Requisito para CVaR5%(PNS)) <= 5 [%Dem] ", traces = 1) %>%
    plotly::style(name = "Requisito para LOLP <= 5%", traces = 2)
  
  return(graficoReqPot)
}
