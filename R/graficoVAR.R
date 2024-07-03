#' Exibe graficos de VaR
#'
#' Monta graficos de Profundidade de Deficit por VAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 5: Mensal por patamar; 6: Mensal em linha; 7:Anual
#' @param tituloGraficoVARMes vetor de caracteres com o titulo do grafico de VAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoVARAno vetor de caracteres com o titulo do grafico de VAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                       inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico,
                       tituloGraficoVARMes = paste0("Profundidade de D\u00E9ficit - VaR Mensal - Caso ", numeroCaso),
                       tituloGraficoVARAno = paste0("Profundidade de D\u00E9ficit - VaR Anual - Caso ", numeroCaso)) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  
  if (tipoGrafico == 6) {
    # exibe grafico mensal de var
    # query no banco
    squery <- paste0("SELECT 
                    A25_NR_MES,
                    A25_TX_PERCENT_VAR,
                    A25_VL_VAR 
                    FROM BPO_A25_VAR_MENSAL_SIN
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
    
    tib.resultadosVarMes <- DBI::dbGetQuery(conexao, squery) %>% 
      tidyr::as_tibble()
    
    DBI::dbDisconnect(conexao)
    
    # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
    tib.resultadosVarMes <- tib.resultadosVarMes %>% 
      dplyr::mutate(anoMes = as.character(A25_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
      dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
      # faz um dplyr::mutate para corrigir a ordem do grafico
      dplyr::mutate(A25_TX_PERCENT_VAR = ifelse(A25_TX_PERCENT_VAR == "1,5%", "1 1,5%",
                                                ifelse(A25_TX_PERCENT_VAR == "2,5%", "2 2,5%",
                                                       ifelse(A25_TX_PERCENT_VAR == "5%", "3 5%",
                                                              ifelse(A25_TX_PERCENT_VAR == "10%", "4 10%", "")))))
    
    # inclusao de maximo var por tipo para possibilitar a identificacao dos maximos no grafico
    tib.resultadosVarMesMax <- tib.resultadosVarMes %>% 
      dplyr::group_by(A25_TX_PERCENT_VAR) %>% 
      dplyr::summarise(maxVAR = max(A25_VL_VAR)) %>% 
      dplyr::ungroup()
    
    tib.resultadosVarMes <- dplyr::inner_join(tib.resultadosVarMes, tib.resultadosVarMesMax, by = "A25_TX_PERCENT_VAR") 
    
    # tibble criado para possibilitar marcacao da area do grafico onde se encontra o maximo
    tib.localMaxVar <- rbind(tib.resultadosVarMes %>% dplyr::filter(A25_VL_VAR == maxVAR), 
                             tib.resultadosVarMes %>% dplyr::filter(A25_VL_VAR == maxVAR) %>% dplyr::mutate(anoMes = zoo::as.Date(zoo::as.yearmon(anoMes)+1/12)))
    
    # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
    marcasEixoMes <- tib.resultadosVarMes %>% 
      dplyr::filter(months(anoMes) %in% c("janeiro","julho")) %>% 
      dplyr::pull(anoMes) %>% c(., max(tib.resultadosVarMes$anoMes))
    
    graficoVaR <- plotly::plot_ly(data = tib.resultadosVarMes, x = ~anoMes, y = ~A25_VL_VAR, color = ~A25_TX_PERCENT_VAR, type = "bar",
                                  hovertemplate = "<b>D\u00E9ficit em MW</b>: %{y:.0f}<br><b>M\u00EDs</b>: %{x|%Y-%m}<extra></extra>") %>% 
      plotly::layout( 
        title = paste0("<b>", tituloGraficoVARMes, "</b>"),
        legend = list(orientation = 'h', x = "0.3", y = "-0.15"),
        yaxis = list( 
          title = "<b>D\u00E9ficit em MW</b>", 
          tickformat = ".0f"
        ), 
        xaxis = list( 
          title = "",
          ticktext = as.list(as.character(zoo::as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)
        )
      ) %>% 
      plotly::style(visible = "legendonly", name = "VaR 1.5%", traces = 1) %>% 
      plotly::style(visible = "legendonly", name = "VaR 2.5%", traces = 2) %>% 
      plotly::style(name = "VaR 5%", traces = 3) %>% 
      plotly::style(visible = "legendonly", name = "VaR 10%", traces = 4)
    
  }else if (tipoGrafico == 7){
    # var anual
    squery <- paste0("SELECT 
                    A26_NR_ANO,
                    A26_TX_PERCENT_VAR,
                    A26_VL_VAR 
                    FROM BPO_A26_VAR_ANUAL_SIN
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
    
    tib.resultadosVarAno <- DBI::dbGetQuery(conexao, squery) %>% 
      tidyr::as_tibble()
    
    DBI::dbDisconnect(conexao)
    
    # filtra o horizonte para exibicao no grafico
    tib.resultadosVarAno <- tib.resultadosVarAno %>% 
      dplyr::filter(dplyr::between(A26_NR_ANO, inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
      # faz um dplyr::mutate para corrigir a ordem do grafico
      dplyr::mutate(A26_TX_PERCENT_VAR = ifelse(A26_TX_PERCENT_VAR == "1,5%", "1 1,5%",
                                                ifelse(A26_TX_PERCENT_VAR == "2,5%", "2 2,5%",
                                                       ifelse(A26_TX_PERCENT_VAR == "5%", "3 5%",
                                                              ifelse(A26_TX_PERCENT_VAR == "10%", "4 10%", "")))))
    
    # exibe grafico anual de var
    graficoVaR <- plotly::plot_ly(data = tib.resultadosVarAno, x = ~A26_NR_ANO, y = ~A26_VL_VAR, color = ~A26_TX_PERCENT_VAR, 
                                  colors = "Set3", type = "bar",
                                  hovertemplate = "<b>D\u00E9ficit em MW</b>: %{y:.0f}<br><b>Ano</b>: %{x}") %>% 
      plotly::layout( 
        title = paste0("<b>", tituloGraficoVARAno, "</b>"),
        legend = list(orientation = 'h', x = "0.3"),
        yaxis = list( 
          title = "<b>D\u00E9ficit em MW</b>", 
          tickformat = ".0f"), 
        xaxis = list(
          title = "<b>Ano</b>",
          type = 'category')) %>% 
      plotly::style(name = "VaR 1.5%", traces = 1) %>% 
      plotly::style(name = "VaR 2.5%", traces = 2) %>% 
      plotly::style(name = "VaR 5%", traces = 3) %>% 
      plotly::style(name = "VaR 10%", traces = 4)
    
  }
  return(graficoVaR)
}
