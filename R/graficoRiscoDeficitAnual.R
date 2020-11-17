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
                                     tituloGrafico = paste0("Risco de D\u00E9ficit (LOLP) - Caso ", numeroCaso)) {
  
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
  tib.resultados <- tib.resultados %>% filter(DEFICIT > 0) %>% 
    group_by(ano) %>% summarise(riscoAnual = n()/max(SERIES)/12)
  
  # exibe grafico de risco
  graficoRisco <- plot_ly(data = tib.resultados, x = ~ano, y = ~riscoAnual, name = "", type = "bar", 
                          textposition = 'outside', texttemplate = "<b>%{y:.1%}</b>",
                          hovertemplate = "<b>Risco de D\u00E9ficit</b>: %{y:.1%}<br><b>Ano</b>: %{x}<extra></extra>") %>% 
                          # <extra></extra> remove o trece do hover
    layout( 
      title = paste0("<b>", tituloGrafico, "</b>"),
      yaxis = list( 
        title = "<b>Risco de D\u00E9ficit</b>", 
        tickformat = "%"),
      xaxis = list(
        type = 'category')
      )
  
  
  # graficoRisco <- ggplot(tib.resultados, aes(x = as.factor(ano), y = riscoAnual)) + 
  #   geom_col(fill = "steelblue") +
  #   # geom_line(size = 1.5, color = "red2") +
  #   geom_text(aes(label = percent(riscoAnual, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ",")), 
  #             nudge_y = (ceiling(max(tib.resultados$riscoAnual)*10)/10 * 0.05),
  #             hjust = 0.4, show.legend = FALSE, fontface = "bold", size = 5, family = "sans") +
  #   scale_x_discrete(name = "Ano") +
  #   # scale_x_continuous(name = "Ano", labels = percent_format(accuracy = 1, scale = 1), 
  #   #                    expand = c(0,0), breaks = seq(min(tib.resultados$ano), max(tib.resultados$ano), 1)) + 
  #   scale_y_continuous(name = "Risco de D\u00E9ficit", expand = c(0,0), labels = percent_format(accuracy = 1, scale = 100, suffix = "%", decimal.mark = ","),
  #                      # breaks = seq(0, ceiling(max(tib.resultados$riscoAnual)*10)/10, ceiling(max(tib.resultados$riscoAnual)*10)/10/16),
  #                      limits = c(0, ceiling(max(tib.resultados$riscoAnual)*10)/10)) +
  #   # scale_fill_manual(name = NULL, values = "steelblue") +
  #   # scale_colour_manual(name = NULL, values = "red2") +
  #   ggtitle(label = tituloGrafico) + 
  #   # expand_limits(x = max(tib.resultadosMes$anoMes) + 20) + # dar uma folga no grafico
  #   theme(text = element_text(size = 20, family = "sans"),
  #         plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)),
  #         strip.background = element_blank(),
  #         panel.background = element_rect(fill = "white"),
  #         panel.grid.major = element_line(color = "gray92"),
  #         panel.grid.major.x = element_blank(),
  #         axis.line = element_line(colour = "black"),
  #         axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
  #         axis.text.y = element_text(color = "black"),
  #         axis.title = element_text(face = "bold"),
  #         legend.position = "bottom",
  #         legend.key = element_blank(),
  #         strip.text.x = element_blank(),
  #         legend.box.background = element_blank()) +
  #   guides(colour = guide_legend(nrow = 1))
}