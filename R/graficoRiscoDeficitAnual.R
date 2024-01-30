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
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco
  query <- paste0("SELECT 
                    A28_NR_ANO,
                    A28_VL_LOLP 
                    FROM BPO_A28_LOLP_ANUAL
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultados <- DBI::dbGetQuery(conexao, query) %>% tidyr::as_tibble()
  DBI::dbDisconnect(conexao)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultados <- tib.resultados %>% 
    dplyr::filter(dplyr::between(A28_NR_ANO, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # exibe grafico de risco
  graficoRisco <- plotly::plot_ly(data = tib.resultados, x = ~A28_NR_ANO, y = ~A28_VL_LOLP, name = "", type = "bar", showlegend = F,
                                  textposition = 'outside', texttemplate = "<b>%{y:.1%}</b>",
                                  # <extra></extra> remove o trece do hover
                                  hovertemplate = "<b>Risco de Déficit</b>: %{y:.1%}<br><b>Ano</b>: %{x}<extra></extra>") %>%
    plotly::add_trace(tib.resultados, x = ~A28_NR_ANO, y = 0.05, type = 'scatter', mode = 'lines', color = I("red"),
                      hovertemplate = "<b>Limite de critério de suprimento: %{y:.0%}<extra></extra>") %>%
    plotly::layout( 
      title = paste0("<b>", tituloGrafico, "</b>"),
      yaxis = list( 
        title = "<b>Risco de Déficit</b>", 
        tickformat = "p"),
      xaxis = list(
        title = "<b>Ano</b>",
        type = 'category')
    )
}
