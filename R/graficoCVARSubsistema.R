#' Exibe graficos de CVaR por subsistema
#'
#' Monta graficos de Profundidade de Deficit por CVAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoCVARSubsistema <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                                  inicioHorizonteGrafico, fimHorizonteGrafico, 
                                  tituloGraficoCVARMes = paste0("Profundidade de D\u00E9ficit - CVAR Mensal 5% - Caso ", numeroCaso)) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT 
                    A24_NR_MES,
                    A02_TX_DESCRICAO_SUBSISTEMA,
                    A24_VL_CVAR_5 
                    FROM BPO_A24_CVAR_MENSAL_SUBS
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultadosCvarMes <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
    dplyr::mutate(anoMes = as.character(A24_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo cvar por SUBSISTEMA para possibilitar a identificacao dos maximos no grafico
  tib.resultadosCvarMesMax <- tib.resultadosCvarMes %>% 
    dplyr::group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% 
    dplyr::summarise(maxCVAR = max(A24_VL_CVAR_5), .groups = 'drop')
  
  tib.resultadosCvarMes <- dplyr::inner_join(tib.resultadosCvarMes, tib.resultadosCvarMesMax, by = "A02_TX_DESCRICAO_SUBSISTEMA")
  
  # tibble criado para possibilitar marcacao da area do grafico onde se encontra o maximo
  tib.localMaxCvar <- rbind(tib.resultadosCvarMes %>% dplyr::filter(A24_VL_CVAR_5 == maxCVAR), 
                            tib.resultadosCvarMes %>% dplyr::filter(A24_VL_CVAR_5 == maxCVAR) %>% dplyr::mutate(anoMes = zoo::as.Date(zoo::as.yearmon(anoMes)+1/12)))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosCvarMes %>% 
    dplyr::filter(months(anoMes) %in% c("janeiro","julho")) %>% 
    dplyr::pull(anoMes) %>% c(., max(tib.resultadosCvarMes$anoMes))
  
  grafico <- plotly::plot_ly(data = tib.resultadosCvarMes, x = ~anoMes, y = ~A24_VL_CVAR_5, color = ~A02_TX_DESCRICAO_SUBSISTEMA, type = "bar",
                             hovertemplate = "<b>D\u00E9ficit % da Demanda</b>: %{y:.1%}<br><b>M\u00EAs</b>: %{x|%Y-%m}") %>% 
    plotly::layout( 
      title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
      legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
      yaxis = list( 
        title = "<b>D\u00E9ficit % da Demanda</b>", 
        tickformat = "p" 
      ), 
      xaxis = list( 
        title = "<b>M\u00EAs</b>", 
        ticktext = as.list(as.character(zoo::as.yearmon(marcasEixoMes))), 
        tickvals = as.list(marcasEixoMes)
      )
    ) %>% 
    # deixa aparecer somente o primeiro subsistema
    plotly::style(visible = "legendonly", traces = 2:length(unique(tib.resultadosCvarMes$A02_TX_DESCRICAO_SUBSISTEMA)))
  
  return(grafico)
}
