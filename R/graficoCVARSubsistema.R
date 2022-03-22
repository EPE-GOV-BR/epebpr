#' Exibe graficos de CVaR por subsistema
#'
#' Monta graficos de Profundidade de Deficit por CVAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoCVARAno vetor de caracteres com o titulo do grafico de CVAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoCVARSubsistema <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                                  inicioHorizonteGrafico, fimHorizonteGrafico, 
                                  tituloGraficoCVARMes = paste0("Profundidade de Déficit - CVAR Mensal 5% - Caso ", numeroCaso)) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT 
                      A02.A02_TX_DESCRICAO_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES,
                      SUM(A16_VL_DESPACHO) AS DEFICIT,
                      A10.DEMANDA 
                    FROM 
                      BPO_A16_BALANCO AS A16,
                      ( SELECT 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO,
                          SUM(A10_VL_DEMANDA) AS DEMANDA
                        FROM BPO_A10_DEMANDA
                        GROUP BY 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO
                      ) AS A10,
                      BPO_A02_SUBSISTEMAS AS A02
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT' AND
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND
                      A10.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA AND
                      A10.A10_NR_MES = A16.A09_NR_MES AND
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND
                      A10.A01_NR_CASO = A16.A01_NR_CASO AND
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A02.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A02.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA
                    GROUP BY
                      A16.A02_NR_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% mutate(PROFUNDIDADE = ifelse(is.nan(DEFICIT/DEMANDA), 0, DEFICIT/DEMANDA))
  
  # filtra subsistemas sem demanda
  filtroSubsistemaSemDemanda <- tib.resultados %>% group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% summarise(demanda = sum(DEMANDA), .groups = "drop") %>% 
    filter(demanda > 0) %>% pull(A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.resultados <- tib.resultados %>% filter(A02_TX_DESCRICAO_SUBSISTEMA %in% filtroSubsistemaSemDemanda)
  
  # calcula o CVAR por subsistema, ano e mes
  tib.resultadosCvarMes <- tib.resultados %>%
    group_by(A02_TX_DESCRICAO_SUBSISTEMA, A09_NR_MES) %>%
    summarise(cvar = cvar(PROFUNDIDADE, 0.05), .groups = 'drop')
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% mutate(anoMes = as.character(A09_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    filter(between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo cvar por SUBSISTEMA para possibilitar a identificacao dos maximos no grafico
  tib.resultadosCvarMesMax <- tib.resultadosCvarMes %>% group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% summarise(maxCVAR = max(cvar), .groups = 'drop')
  tib.resultadosCvarMes <- inner_join(tib.resultadosCvarMes, tib.resultadosCvarMesMax, by = "A02_TX_DESCRICAO_SUBSISTEMA")
  
  # tibble criado para possibilitar marcacao da area do grafico onde se encontra o maximo
  tib.localMaxCvar <- rbind(tib.resultadosCvarMes %>% filter(cvar == maxCVAR), 
                            tib.resultadosCvarMes %>% filter(cvar == maxCVAR) %>% mutate(anoMes = zoo::as.Date(as.yearmon(anoMes)+1/12)))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosCvarMes %>% filter(months(anoMes) %in% c("janeiro","julho")) %>% pull(anoMes) %>% c(., max(tib.resultadosCvarMes$anoMes))
  
  grafico <- plot_ly(data = tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, color = ~A02_TX_DESCRICAO_SUBSISTEMA, type = "bar",
          hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Mês</b>: %{x|%Y-%m}") %>% 
    layout( 
      title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
      legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
      yaxis = list( 
        title = "<b>Déficit % da Demanda</b>", 
        tickformat = "%" 
      ), 
      xaxis = list( 
        title = "<b>Mês</b>", 
        ticktext = as.list(as.character(as.yearmon(marcasEixoMes))), 
        tickvals = as.list(marcasEixoMes)
        )
      ) %>% 
    # deixa aparecer somente o primeiro subsistema
    style(visible = "legendonly", traces = 2:length(filtroSubsistemaSemDemanda))
  
  # exibe grafico mensal de cvar separado por tipo de cvar
  # graficoCVaR <- ggplotly(ggplot(tib.resultadosCvarMes, aes(x = anoMes, y = cvar, colour = A02_TX_DESCRICAO_SUBSISTEMA, fill = A02_TX_DESCRICAO_SUBSISTEMA)) + 
  #   # geom_area(data = tib.localMaxCvar, show.legend = FALSE) + 
  #   # scale_fill_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue")) +
  #   # scale_fill_manual(name = NULL, values = c("steelblue"), labels = NULL) +
  #   geom_col(size = 1.5, show.legend = FALSE, position = "dodge") + 
  #   # geom_text(aes(label = ifelse(cvar == maxCVAR, percent(cvar, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
  #   #           nudge_y = (ceiling(max(tib.resultadosCvarMes$cvar)*10)/10 * 0.05), 
  #   #           hjust = 0.2,
  #   #           show.legend = FALSE, 
  #   #           fontface = "bold", size = 5, family = "sans") +
  #   scale_x_date(name = "Mês", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) +
  #   scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","))#,
  #                      #breaks = seq(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10/5),
  #                      #limits = c(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10)) +
  #   # scale_color_manual(name = NULL, values = c("steelblue"), labels = NULL) +
  #   # scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("cvar 1,5%", "cvar 2,5%", "cvar 5%", "cvar 10%")) + 
  #   # ggtitle(label = tituloGraficoCVARMes) + 
  #   # expand_limits(x = max(tib.resultadosCvarMes$anoMes) + 20) + # dar uma folga no grafico
  #   theme(# text = element_text(size = 20, family = "sans"),
  #         plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)),
  #         # plot.background = element_rect(fill = "gray98"),
  #         strip.background = element_blank(),
  #         panel.background = element_rect(fill = "white"),
  #         panel.grid.major = element_line(color = "gray92"),
  #         panel.grid.major.x = element_blank(),
  #         panel.spacing = unit(2, "lines"),
  #         axis.line = element_line(colour = "black"),
  #         axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
  #         axis.text.y = element_text(color = "black"),  
  #         axis.title = element_text(face = "bold"),
  #         legend.position = "bottom",
  #         legend.key = element_blank(),
  #         strip.text.x = element_blank(),
  #         legend.box.background = element_blank()) +
  #   guides(colour = guide_legend(nrow = 1))) # +
  #   # facet_wrap(~A02_TX_DESCRICAO_SUBSISTEMA, ncol = 1)
  
  return(grafico)
}
