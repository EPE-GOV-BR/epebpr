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
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  squery <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT,
                    A10.DEMANDA 
                    FROM BPO_A16_BALANCO AS A16,
                    ( SELECT A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO,
                      SUM(A10_VL_DEMANDA) AS DEMANDA
                      FROM BPO_A10_DEMANDA
                      GROUP BY A10_NR_MES,
                      A01_TP_CASO,
                      A01_NR_CASO,
                      A01_CD_MODELO
                    ) AS A10
                    WHERE 
                    A16.A16_TP_GERACAO = 'DEFICIT' AND
                    A16.A01_TP_CASO = ", tipoCaso," AND
                    A16.A01_NR_CASO = ", numeroCaso," AND
                    A16.A01_CD_MODELO = ", codModelo, " AND
                    A10.A10_NR_MES = A16.A09_NR_MES AND
                    A10.A01_TP_CASO = A16.A01_TP_CASO AND
                    A10.A01_NR_CASO = A16.A01_NR_CASO AND
                    A10.A01_CD_MODELO = A16.A01_CD_MODELO
                    GROUP BY
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% mutate(PROFUNDIDADE = DEFICIT/DEMANDA)
  
  # calcula o CVAR por ano e mes
  tib.resultadosCvarMes <- tib.resultados %>%
    group_by(A09_NR_MES) %>%
    summarise(#"cvar 1%" = cvar(PROFUNDIDADE, 0.01),
      #"1cvar 1,5%" = cvar(PROFUNDIDADE, 0.015),
      #"cvar 2%" = cvar(PROFUNDIDADE, 0.02),
      #"2cvar 2,5%" = cvar(PROFUNDIDADE, 0.025),
      #"4cvar 10%" = cvar(PROFUNDIDADE, 0.1),
      "3cvar 5%" = cvar(PROFUNDIDADE, 0.05)) %>% ungroup()
  
  # faz a transposicao dos dados de cvar por coluna para um campo de identificacao do cvar (1%; 1,5%; 2%; 5%...) e outro de valor de cvar
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% gather(key = "tamanhoCVAR", value = "cvar", -A09_NR_MES)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosCvarMes <- tib.resultadosCvarMes %>% mutate(anoMes = as.character(A09_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    filter(between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo cvar por tipo para possibilitar a identificacao dos maximos no grafico
  tib.resultadosCvarMesMax <- tib.resultadosCvarMes %>% group_by(tamanhoCVAR) %>% summarise(maxCVAR = max(cvar)) %>% ungroup()
  tib.resultadosCvarMes <- inner_join(tib.resultadosCvarMes, tib.resultadosCvarMesMax, by = "tamanhoCVAR")
  
  # tibble criado para possibilitar marcacao da area do grafico onde se encontra o maximo
  # tib.localMaxCvar <- rbind(tib.resultadosCvarMes %>% filter(cvar == maxCVAR), 
  #                           tib.resultadosCvarMes %>% filter(cvar == maxCVAR) %>% mutate(anoMes = zoo::as.Date(as.yearmon(anoMes)+1/12)))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosCvarMes %>% filter(months(anoMes) %in% c("janeiro","julho")) %>% pull(anoMes) %>% c(., max(tib.resultadosCvarMes$anoMes))
  
  # carrega fontes para o grafico
  # font_add_google("Montserrat", "Montserrat")
  # showtext_auto()
  
  if (tipoGrafico == 1) {
    # exibe grafico mensal de cvar separado por tipo de cvar
    tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
      mutate(textoCVaR = ifelse(cvar == maxCVAR, paste0(round(cvar * 100, 1), "%"), ""))
    
    # cria sequencia de datas para desenhar a linha de limite de criterio de sup. tem um mes antes e um depois para atravessar todo o grafico
    primeiroMes <- min(tib.resultadosCvarMes$anoMes) %>% as.yearmon()
    ultimoMes <- max(tib.resultadosCvarMes$anoMes) %>% as.yearmon()
    mesesLinha <- seq(primeiroMes - 1/12, ultimoMes + 1/12, 1/12) %>% format("%Y-%m-%d")
    
    graficoCVaR <- plot_ly(data = tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, name = "", type = "bar", showlegend = F,
                           hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Mês</b>: %{x|%Y-%m}<extra></extra>") %>% 
      add_trace(tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, type = 'scatter',
                mode = 'text', text = ~textoCVaR, textposition = 'top center', texttemplate = "<b>%{text}</b>") %>%
      add_trace(tib.resultadosCvarMes, x = mesesLinha, y = 0.05, type = 'scatter', mode = 'lines', color = I("red"),
                hovertemplate = "<b>Limite de critério de suprimento: %{y:.0%}<extra></extra>") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
        # legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "%" 
        ), 
        xaxis = list( 
          title = "<b>Mês</b>", 
          ticktext = as.list(as.character(as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)#,
          #range = c(min(tib.resultadosCvarMes$anoMes), max(tib.resultadosCvarMes$anoMes))
        )
      )
    
    # graficoCVaR <- ggplot(tib.resultadosCvarMes, aes(x = anoMes, y = cvar, colour = tamanhoCVAR, fill = tamanhoCVAR)) + 
    #   geom_area(data = tib.localMaxCvar, show.legend = FALSE) + 
    #   # scale_fill_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue")) +
    #   scale_fill_manual(name = NULL, values = c("steelblue"), labels = NULL) +
    #   geom_step(size = 1.5, show.legend = FALSE) + 
    #   geom_text(aes(label = ifelse(cvar == maxCVAR, percent(cvar, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
    #             nudge_y = (ceiling(max(tib.resultadosCvarMes$cvar)*10)/10 * 0.05), 
    #             hjust = 0.2,
    #             show.legend = FALSE, 
    #             fontface = "bold") + #, size = 5, family = "sans") +
    #   scale_x_date(name = "Mês", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) +
    #   scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
    #                      breaks = seq(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10/5),
    #                      limits = c(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10)) +
    #   scale_color_manual(name = NULL, values = c("steelblue"), labels = NULL) +
    #   # scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("cvar 1,5%", "cvar 2,5%", "cvar 5%", "cvar 10%")) + 
    #   ggtitle(label = tituloGraficoCVARMes) + 
    #   expand_limits(x = max(tib.resultadosCvarMes$anoMes) + 20) + # dar uma folga no grafico
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
    #   guides(colour = guide_legend(nrow = 1)) 
    
  } else if (tipoGrafico == 2){
    
    # exibe grafico mensal de cvar
    tib.resultadosCvarMes <- tib.resultadosCvarMes %>% 
      mutate(textoCVaR = ifelse(cvar == maxCVAR, paste0(round(cvar * 100, 1), "%"), ""))
    
    graficoCVaR <- plot_ly(data = tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, name = "", type = 'scatter', mode = 'lines', showlegend = F,
                           hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Mês</b>: %{x|%Y-%m}<extra></extra>") %>% 
      add_trace(tib.resultadosCvarMes, x = ~anoMes, y = ~cvar, type = 'scatter', name = "",
                mode = 'text', text = ~textoCVaR, textposition = 'top center', texttemplate = "<b>%{text}</b>") %>%
      layout( 
        title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
        # legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "%" 
        ), 
        xaxis = list( 
          title = "<b>Mês</b>", 
          ticktext = as.list(as.character(as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)
        )
      )
    
    # graficoCVaR <- ggplot(tib.resultadosCvarMes, aes(x = anoMes, y = cvar, colour = tamanhoCVAR)) + 
    #   geom_line(size = 1.5, show.legend = FALSE) +
    #   scale_x_date(name = "Mês", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) +
    #   scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
    #                      breaks = seq(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10/20),
    #                      limits = c(0, ceiling(max(tib.resultadosCvarMes$cvar)*10)/10)) +
    #   scale_color_manual(name = NULL, labels = NULL, 
    #                      values = c("steelblue", "black", "darkgreen", "red2")) + 
    #   ggtitle(label = tituloGraficoCVARMes) + 
    #   expand_limits(x = max(tib.resultadosCvarMes$anoMes) + 20) + # dar uma folga no grafico
    #   theme(text = element_text(size = 20, family = "sans"),
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
    #   guides(colour = guide_legend(nrow = 1))
    
  } else {
    # cvar anual
    # cria coluna de ano e calcula o CVAR por ano
    tib.resultadosCvarAno <- tib.resultados %>% mutate(ano = A09_NR_MES%/%100) %>% 
      group_by(ano) %>%
      summarise(# "cvar 1%" = cvar(PROFUNDIDADE, 0.01),
        "1cvar 1,5%" = cvar(PROFUNDIDADE, 0.015),
        # "cvar 2%" = cvar(PROFUNDIDADE, 0.02),
        "2cvar 2,5%" = cvar(PROFUNDIDADE, 0.025),
        "3cvar 5%" = cvar(PROFUNDIDADE, 0.05),
        "4cvar 10%" = cvar(PROFUNDIDADE, 0.1))
    
    # faz a transposicao dos dados de cvar por coluna para um campo de identificacao do cvar (1%; 1,5%; 2%; 5%...) e outro de valor de cvar
    tib.resultadosCvarAno <- tib.resultadosCvarAno %>% gather(key = "tamanhoCVAR", value = "cvar", -ano) 
    
    # filtra o horizonte para exibicao no grafico
    tib.resultadosCvarAno <- tib.resultadosCvarAno %>% filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
    
    # exibe grafico anual de cvar
    graficoCVaR <- plot_ly(data = tib.resultadosCvarAno, x = ~ano, y = ~cvar, color = ~tamanhoCVAR, 
                           colors = "Set3", type = "bar",
                           hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Ano</b>: %{x}") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoCVARAno, "</b>"),
        legend = list(orientation = 'h', x = "0.3"),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "%"), 
        xaxis = list(
          type = 'category')) %>% 
      style(name = "CVaR 1.5%", traces = 1) %>% 
      style(name = "CVaR 2.5%", traces = 2) %>% 
      style(name = "CVaR 5%", traces = 3) %>% 
      style(name = "CVaR 10%", traces = 4)
    
    
    # graficoCVaR <- ggplot(tib.resultadosCvarAno, aes(x = ano, y = cvar, colour = tamanhoCVAR)) + 
    #   geom_step(size = 1.5) + 
    #   scale_x_continuous(name = "Ano", expand = c(0,0), breaks = seq(inicioHorizonteGrafico, fimHorizonteGrafico)) +
    #   scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), 
    #                      breaks = seq(0, ceiling(max(tib.resultadosCvarAno$cvar)*10)/10, ceiling(max(tib.resultadosCvarAno$cvar)*10)/10/20),
    #                      limits = c(0, ceiling(max(tib.resultadosCvarAno$cvar)*10)/10)) + 
    #   scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("cvar 1,5%", "cvar 2,5%", "cvar 5%", "cvar 10%")) + 
    #   ggtitle(label = tituloGraficoCVARAno) +
    #   expand_limits(x = max(tib.resultadosCvarAno$ano) + 0.2) + # dar uma folga no grafico
    #   theme(text = element_text(size = 20, family = "sans"),
    #         plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)), 
    #         # plot.background = element_rect(fill = "gray98"),
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
    #         strip.text.x = element_blank()) +
    #   guides(colour = guide_legend(nrow = 1))
    
  }
  return(graficoCVaR)
}
