#' Exibe graficos de CVaR
#'
#' Monta graficos de Profundidade de Deficit por CVAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 1: Mensal por patamar; 2: Mensal em linha; 3:Anual
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoCVARAno vetor de caracteres com o titulo do grafico de CVAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoCVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                        inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico,
                        tituloGraficoCVARMes = paste0("Profundidade de Déficit - CVAR Mensal 5% - Caso ", numeroCaso),
                        tituloGraficoCVARAno = paste0("Profundidade de Déficit - CVAR Anual - Caso ", numeroCaso)) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  
  if (tipoGrafico == 1) {
    # cvar mensal
    # query no banco
    squery <- paste0("SELECT 
                    A22_NR_MES,
                    A22_TX_PERCENT_CVAR,
                    A22_VL_CVAR 
                    FROM BPO_A22_CVAR_MENSAL_SIN
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
    
    tib.resultadosCvarMes <- DBI::dbGetQuery(conexao, squery) %>% 
      tidyr::as_tibble()
    
    DBI::dbDisconnect(conexao)
    
    # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
    tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
      dplyr::mutate(anoMes = as.character(A22_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
      dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
      dplyr::filter(A22_TX_PERCENT_CVAR == "5%")
    
    # inclusao de maximo cvar por tipo para possibilitar a identificacao dos maximos no grafico
    tib.resultadosCvarMesMax <- tib.resultadosCvarMes %>% 
      dplyr::group_by(A22_TX_PERCENT_CVAR) %>% 
      dplyr::summarise(maxCVAR = max(A22_VL_CVAR)) %>% 
      dplyr::ungroup()
    
    tib.resultadosCvarMes <- dplyr::inner_join(tib.resultadosCvarMes, tib.resultadosCvarMesMax, by = "A22_TX_PERCENT_CVAR")
    
    # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
    marcasEixoMes <- tib.resultadosCvarMes %>% 
      dplyr::filter(months(anoMes) %in% c("janeiro","julho")) %>% 
      dplyr::pull(anoMes) %>% c(., max(tib.resultadosCvarMes$anoMes))
    
    # exibe grafico mensal de cvar separado por tipo de cvar
    tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
      dplyr::mutate(textoCVaR = ifelse(A22_VL_CVAR == maxCVAR, paste0(round(A22_VL_CVAR * 100, 1), "%"), ""))
    
    # cria sequencia de datas para desenhar a linha de limite de criterio de sup. tem um mes antes e um depois para atravessar todo o grafico
    primeiroMes <- min(tib.resultadosCvarMes$anoMes) %>% zoo::as.yearmon()
    ultimoMes <- max(tib.resultadosCvarMes$anoMes) %>% zoo::as.yearmon()
    mesesLinha <- seq(primeiroMes - 1/12, ultimoMes + 1/12, 1/12) %>% format("%Y-%m-%d")
    
    graficoCVaR <- plotly::plot_ly(data = tib.resultadosCvarMes, x = ~anoMes, y = ~A22_VL_CVAR, name = "", type = "bar", showlegend = F,
                                   hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Mês</b>: %{x|%Y-%m}<extra></extra>") %>% 
      plotly::add_trace(tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, type = 'scatter',
                        mode = 'text', text = ~textoCVaR, textposition = 'top center', texttemplate = "<b>%{text}</b>") %>%
      plotly::add_trace(tib.resultadosCvarMes, x = mesesLinha, y = 0.05, type = 'scatter', mode = 'lines', color = I("red"),
                        hovertemplate = "<b>Limite de critério de suprimento: %{y:.0%}<extra></extra>") %>% 
      plotly::layout( 
        title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "p" 
        ), 
        xaxis = list( 
          title = "<b>Mês</b>", 
          ticktext = as.list(as.character(zoo::as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)
        )
      )
    
  } else if (tipoGrafico == 3){
    # cvar anual
    
    # query no banco
    squery <- paste0("SELECT 
                    A23_NR_ANO,
                    A23_TX_PERCENT_CVAR,
                    A23_VL_CVAR 
                    FROM BPO_A23_CVAR_ANUAL_SIN
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
    
    tib.resultadosCvarAno <- DBI::dbGetQuery(conexao, squery) %>% 
      tidyr::as_tibble()
    
    DBI::dbDisconnect(conexao)
    
    # filtra resultados por anos
    tib.resultadosCvarAno <- tib.resultadosCvarAno %>% dplyr::filter(dplyr::between(A23_NR_ANO, inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
      # faz um dplyr::mutate para corrigir a ordem do grafico
      dplyr::mutate(A23_TX_PERCENT_CVAR = ifelse(A23_TX_PERCENT_CVAR == "1,5%", "1 1,5%",
                                                 ifelse(A23_TX_PERCENT_CVAR == "2,5%", "2 2,5%",
                                                        ifelse(A23_TX_PERCENT_CVAR == "5%", "3 5%",
                                                               ifelse(A23_TX_PERCENT_CVAR == "10%", "4 10%", "")))))
    
    # exibe grafico anual de cvar
    graficoCVaR <- plotly::plot_ly(data = tib.resultadosCvarAno, x = ~A23_NR_ANO, y = ~A23_VL_CVAR, color = ~A23_TX_PERCENT_CVAR, 
                                   colors = "Set3", type = "bar",
                                   hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Ano</b>: %{x}") %>% 
      plotly::layout( 
        title = paste0("<b>", tituloGraficoCVARAno, "</b>"),
        legend = list(orientation = 'h', x = "0.3"),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "p"), 
        xaxis = list(
          title = "<b>Ano</b>",
          type = 'category')) %>% 
      plotly::style(name = "CVaR 1.5%", traces = 1) %>% 
      plotly::style(name = "CVaR 2.5%", traces = 2) %>% 
      plotly::style(name = "CVaR 5%", traces = 3) %>% 
      plotly::style(name = "CVaR 10%", traces = 4)
  }
  return(graficoCVaR)
}
