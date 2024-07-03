#' Exibe grafico de Requisito de Potencia Quadrimestral
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
graficoRequisitosPotQuad <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  
  squery <- paste0("SELECT 
                    A30_NR_ANO,
                    A30_NR_QUADRIMESTRE,
                    A30_VL_REQUISITO 
                    FROM BPO_A30_REQUISITOS_POTENCIA_QUAD
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # cria coluna de quadrimestre e filtra o horizonte para exibicao no grafico
  tib.requisitosPotQuad <- tib.resultados %>%
    dplyr::filter(dplyr::between(A30_NR_ANO, inicioHorizonteGrafico, fimHorizonteGrafico)) %>% 
    dplyr::mutate(anoQuad = paste0(A30_NR_ANO, "-", A30_NR_QUADRIMESTRE))
  
  graficoReqPotQuad <- plotly::plot_ly(data = tib.requisitosPotQuad, x = ~anoQuad, y = ~A30_VL_REQUISITO, name = "", type = "bar", color = I("gray"), showlegend = F,
                                       hovertemplate = "<b>Requisito de pot\u00EDncia</b>: %{y:.2f} MW<extra></extra>") %>% 
    plotly::layout( 
      title = "<b>Requisito de Pot\u00EDncia Quadrimestral</b>",
      legend = list(orientation = 'h', x = "0.3"),
      yaxis = list( 
        title = "<b>Requisito de Pot\u00EDncia para o SIN [MW]</b>", 
        tickformat = "d" 
      ), 
      xaxis = list( 
        title = "<b>Quadrimestre</b>"
      )
    )
  
  return(graficoReqPotQuad)
}
