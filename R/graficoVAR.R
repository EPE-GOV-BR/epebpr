#' Exibe graficos de VaR
#'
#' Monta graficos de Profundidade de Deficit por VAR de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 5: Mensal por patamar; 6: Mensal em linha; 7:Anual
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoCVARAno vetor de caracteres com o titulo do grafico de CVAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficoVAR <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, 
                       inicioHorizonteGrafico, fimHorizonteGrafico, tipoGrafico,
                       tituloGraficoVARMes = paste0("Profundidade de D\u00E9ficit - VaR Mensal - Caso ", numeroCaso),
                       tituloGraficoVARAno = paste0("Profundidade de D\u00E9ficit - VaR Anual - Caso ", numeroCaso)) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade (informacao pelo SIN)
  squery <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT
                    FROM BPO_A16_BALANCO AS A16
                    WHERE 
                    A16.A16_TP_GERACAO = 'DEFICIT' AND
                    A16.A01_TP_CASO = ", tipoCaso," AND
                    A16.A01_NR_CASO = ", numeroCaso," AND
                    A16.A01_CD_MODELO = ", codModelo, " 
                    GROUP BY
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # calcula o CVAR por ano e mes
  tib.resultadosVarMes <- tib.resultados %>%
    group_by(A09_NR_MES) %>%
    summarise(#"var 1%" = var(DEFICIT, 0.01),
      "1var 1,5%" = var(DEFICIT, 0.015),
      #"var 2%" = var(DEFICIT, 0.02),
      "2var 2,5%" = var(DEFICIT, 0.025),
      "3var 5%" = var(DEFICIT, 0.05),
      "4var 10%" = var(DEFICIT, 0.1), .groups = "drop")
  
  # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
  tib.resultadosVarMes <- tib.resultadosVarMes %>% pivot_longer(cols = -A09_NR_MES, names_to = "tamanhoVAR", values_to = "var")

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
  
  if (tipoGrafico == 5) {
    # exibe grafico mensal de var separado por tipo de var
    graficoVaR <- plot_ly(data = tib.resultadosVarMes, x = ~anoMes, y = ~var, color = ~tamanhoVAR, colors = "Set3", type = "bar",
                       hovertemplate = "<b>D\u00E9ficit em MW</b>: %{y:.0f}<br><b>M\u00EAs</b>: %{x|%Y-%m}<extra></extra>") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoVARMes, "</b>"),
        legend = list(orientation = 'h', x = "0.3", y = "-0.15"),
        yaxis = list( 
          title = "<b>D\u00E9ficit em MW</b>", 
          tickformat = ".0f"
        ), 
        xaxis = list( 
          title = "",
          ticktext = as.list(as.character(as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)
        )
      ) %>% 
      style(visible = "legendonly", name = "CVaR 1.5%", traces = 1) %>% 
      style(visible = "legendonly", name = "CVaR 2.5%", traces = 2) %>% 
      style(name = "CVaR 5%", traces = 3) %>% 
      style(visible = "legendonly", name = "CVaR 10%", traces = 4)

  } else if (tipoGrafico == 6){
    
    # exibe grafico mensal de var
    graficoVaR <- plot_ly(data = tib.resultadosVarMes, x = ~anoMes, y = ~var, color = ~tamanhoVAR, type = "scatter", mode = "lines",
                          hovertemplate = "<b>D\u00E9ficit em MW</b>: %{y:.0f}<br><b>M\u00EAs</b>: %{x|%Y-%m}<extra></extra>") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoVARMes, "</b>"),
        legend = list(orientation = 'h', x = "0.3", y = "-0.15"),
        yaxis = list( 
          title = "<b>D\u00E9ficit em MW</b>", 
          tickformat = ".0f"
        ), 
        xaxis = list( 
          title = "",
          ticktext = as.list(as.character(as.yearmon(marcasEixoMes))), 
          tickvals = as.list(marcasEixoMes)
        )
      ) %>% 
      style(visible = "legendonly", name = "CVaR 1.5%", traces = 1) %>% 
      style(visible = "legendonly", name = "CVaR 2.5%", traces = 2) %>% 
      style(name = "CVaR 5%", traces = 3) %>% 
      style(visible = "legendonly", name = "CVaR 10%", traces = 4)
    
  } else {
    # var anual
    # cria coluna de ano e calcula o CVAR por ano
    tib.resultadosVarAno <- tib.resultados %>% mutate(ano = A09_NR_MES%/%100) %>% 
      group_by(ano) %>%
      summarise(# "var 1%" = var(DEFICIT, 0.01),
        "1cvar 1,5%" = var(DEFICIT, 0.015),
        # "var 2%" = var(DEFICIT, 0.02),
        "2cvar 2,5%" = var(DEFICIT, 0.025),
        "3cvar 5%" = var(DEFICIT, 0.05),
        "4cvar 10%" = var(DEFICIT, 0.1), .groups = "drop")
    
    # faz a transposicao dos dados de var por coluna para um campo de identificacao do var (1%; 1,5%; 2%; 5%...) e outro de valor de var
    tib.resultadosVarAno <- tib.resultadosVarAno %>% pivot_longer(cols = -ano, names_to = "tamanhoVAR", values_to = "var")

    # filtra o horizonte para exibicao no grafico
    tib.resultadosVarAno <- tib.resultadosVarAno %>% filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
    
    # exibe grafico anual de var
    graficoVaR <- plot_ly(data = tib.resultadosVarAno, x = ~ano, y = ~var, color = ~tamanhoVAR, 
                          colors = "Set3", type = "bar",
                          hovertemplate = "<b>D\u00E9ficit em MW</b>: %{y:.0f}<br><b>Ano</b>: %{x}") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoVARAno, "</b>"),
        legend = list(orientation = 'h', x = "0.3"),
        yaxis = list( 
          title = "<b>D\u00E9ficit em MW</b>", 
          tickformat = ".0f"), 
        xaxis = list(
          type = 'category')) %>% 
      style(name = "CVaR 1.5%", traces = 1) %>% 
      style(name = "CVaR 2.5%", traces = 2) %>% 
      style(name = "CVaR 5%", traces = 3) %>% 
      style(name = "CVaR 10%", traces = 4)

  }
  return(graficoVaR)
}