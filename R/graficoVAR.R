#' Exibe graficos de VaR
#'
#' Monta graficos de Profundidade de Deficit por VAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 5: Mensal por patamar; 6: Mensal em linha; 7:Anual
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoCVARAno vetor de caracteres com o titulo do grafico de CVAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo ggplot
#'
#' @export
graficoVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                       inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico,
                       tituloGraficoCVARMes = paste0("Profundidade de D\u00E9ficit - VaR Mensal - Caso ", numeroCaso),
                       tituloGraficoCVARAno = paste0("Profundidade de D\u00E9ficit - VaR Anual - Caso ", numeroCaso)) {
  
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
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as.tbl()
  dbDisconnect(conexao)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% mutate(PROFUNDIDADE = DEFICIT/DEMANDA)
  
  # calcula o CVAR por ano e mes
  tib.resultadosVarMes <- tib.resultados %>%
    group_by(A09_NR_MES) %>%
    summarise(#"var 1%" = var(PROFUNDIDADE, 0.01),
      "1var 1,5%" = var(PROFUNDIDADE, 0.015),
      #"var 2%" = var(PROFUNDIDADE, 0.02),
      "2var 2,5%" = var(PROFUNDIDADE, 0.025),
      "3var 5%" = var(PROFUNDIDADE, 0.05),
      "4var 10%" = var(PROFUNDIDADE, 0.1)) %>% ungroup()
  
  # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
  tib.resultadosVarMes <- tib.resultadosVarMes %>% gather(key = "tamanhoVAR", value = "var", -A09_NR_MES)
  
  # cria coluna do tipo data a partir do campo anoMes e filtra o horizonte para exibicao no grafico
  tib.resultadosVarMes <- tib.resultadosVarMes %>% mutate(anoMes = as.character(A09_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    filter(between(as.integer(format(anoMes, "%Y")), inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # inclusao de maximo var por tipo para possibilitar a identificacao dos maximos no grafico
  tib.resultadosVarMesMax <- tib.resultadosVarMes %>% group_by(tamanhoVAR) %>% summarise(maxVAR = max(var)) %>% ungroup()
  tib.resultadosVarMes <- inner_join(tib.resultadosVarMes, tib.resultadosVarMesMax, by = "tamanhoVAR")
  
  # tibble criado para possibilitar marcacao da area do grafico onde se encontra o maximo
  tib.localMaxVar <- rbind(tib.resultadosVarMes %>% filter(var == maxVAR), 
                           tib.resultadosVarMes %>% filter(var == maxVAR) %>% mutate(anoMes = zoo::as.Date(as.yearmon(anoMes)+1/12)))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosVarMes %>% filter(months(anoMes) %in% c("janeiro","julho")) %>% pull(anoMes) %>% c(., max(tib.resultadosVarMes$anoMes))
  
  # carrega fontes para o grafico
  # font_add_google("Montserrat", "Montserrat")
  # showtext_auto()
  
  if (tipoGrafico == 5) {
    # exibe grafico mensal de var separado por tipo de var
    graficoVaR <- ggplot(tib.resultadosVarMes, aes(x = anoMes, y = var, colour = tamanhoVAR, fill = tamanhoVAR)) + 
      geom_area(data = tib.localMaxVar, show.legend = FALSE) + 
      scale_fill_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue")) +
      geom_step(size = 1.5) + 
      geom_text(aes(label = ifelse(var == maxVAR, percent(var, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
                nudge_y = (ceiling(max(tib.resultadosVarMes$var)*10)/10 * 0.1), 
                hjust = 0.2,
                show.legend = FALSE, 
                fontface = "bold", size = 5, family = "sans") +
      scale_x_date(name = "M\u00EAs", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) +
      scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
                         breaks = seq(0, ceiling(max(tib.resultadosVarMes$var)*10)/10, ceiling(max(tib.resultadosVarMes$var)*10)/10/5),
                         limits = c(0, ceiling(max(tib.resultadosVarMes$var)*10)/10)) +
      scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("var 1,5%", "var 2,5%", "var 5%", "var 10%")) + 
      ggtitle(label = tituloGraficoCVARMes) + 
      expand_limits(x = max(tib.resultadosVarMes$anoMes) + 20) + # dar uma folga no grafico
      theme(text = element_text(size = 20, family = "sans"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)),
            # plot.background = element_rect(fill = "gray98"),
            strip.background = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray92"),
            panel.grid.major.x = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
            axis.text.y = element_text(color = "black"),  
            axis.title = element_text(face = "bold"),
            legend.position = "bottom",
            legend.key = element_blank(),
            strip.text.x = element_blank(),
            legend.box.background = element_blank()) +
      guides(colour = guide_legend(nrow = 1)) +
      facet_wrap(~tamanhoVAR, ncol = 1)
    
  } else if (tipoGrafico == 6){
    
    # exibe grafico mensal de var
    graficoVaR <- ggplot(tib.resultadosVarMes, aes(x = anoMes, y = var, colour = tamanhoVAR)) + 
      geom_line(size = 1.5) +
      scale_x_date(name = "M\u00EAs", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) +
      scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
                         breaks = seq(0, ceiling(max(tib.resultadosVarMes$var)*10)/10, ceiling(max(tib.resultadosVarMes$var)*10)/10/20),
                         limits = c(0, ceiling(max(tib.resultadosVarMes$var)*10)/10)) +
      scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("var 1,5%", "var 2,5%", "var 5%", "var 10%")) + 
      ggtitle(label = tituloGraficoCVARMes) + 
      expand_limits(x = max(tib.resultadosVarMes$anoMes) + 20) + # dar uma folga no grafico
      theme(text = element_text(size = 20, family = "sans"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)),
            # plot.background = element_rect(fill = "gray98"),
            strip.background = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray92"),
            panel.grid.major.x = element_blank(),
            panel.spacing = unit(2, "lines"),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
            axis.text.y = element_text(color = "black"),  
            axis.title = element_text(face = "bold"),
            legend.position = "bottom",
            legend.key = element_blank(),
            strip.text.x = element_blank(),
            legend.box.background = element_blank()) +
      guides(colour = guide_legend(nrow = 1))
    
  } else {
    # var anual
    # cria coluna de ano e calcula o CVAR por ano
    tib.resultadosVarAno <- tib.resultados %>% mutate(ano = A09_NR_MES%/%100) %>% 
      group_by(ano) %>%
      summarise(# "var 1%" = var(PROFUNDIDADE, 0.01),
        "1cvar 1,5%" = var(PROFUNDIDADE, 0.015),
        # "var 2%" = var(PROFUNDIDADE, 0.02),
        "2cvar 2,5%" = var(PROFUNDIDADE, 0.025),
        "3cvar 5%" = var(PROFUNDIDADE, 0.05),
        "4cvar 10%" = var(PROFUNDIDADE, 0.1))
    
    # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
    tib.resultadosVarAno <- tib.resultadosVarAno %>% gather(key = "tamanhoVAR", value = "var", -ano) 
    
    # filtra o horizonte para exibicao no grafico
    tib.resultadosVarAno <- tib.resultadosVarAno %>% filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
    
    # exibe grafico anual de var
    graficoVaR <- ggplot(tib.resultadosVarAno, aes(x = ano, y = var, colour = tamanhoVAR)) + 
      geom_step(size = 1.5) + 
      scale_x_continuous(name = "Ano", expand = c(0,0), breaks = seq(inicioHorizonteGrafico, fimHorizonteGrafico)) +
      scale_y_continuous(name = "% da Demanda", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), 
                         breaks = seq(0, ceiling(max(tib.resultadosVarAno$var)*10)/10, ceiling(max(tib.resultadosVarAno$var)*10)/10/20),
                         limits = c(0, ceiling(max(tib.resultadosVarAno$var)*10)/10)) + 
      scale_color_manual(name = NULL, values = c("black", "darkgreen", "red2", "steelblue"), labels = c("var 1,5%", "var 2,5%", "var 5%", "var 10%")) + 
      ggtitle(label = tituloGraficoCVARAno) +
      expand_limits(x = max(tib.resultadosVarAno$ano) + 0.2) + # dar uma folga no grafico
      theme(text = element_text(size = 20, family = "sans"),
            plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)), 
            # plot.background = element_rect(fill = "gray98"),
            strip.background = element_blank(),
            panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(color = "gray92"),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, color = "black"),
            axis.text.y = element_text(color = "black"), 
            axis.title = element_text(face = "bold"),
            legend.position = "bottom",
            legend.key = element_blank(),
            strip.text.x = element_blank()) +
      guides(colour = guide_legend(nrow = 1))
    
  }
  return(graficoVaR)
}