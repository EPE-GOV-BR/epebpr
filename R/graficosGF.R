#' Exibe graficos de CVaR com criterios de GF
#'
#' Monta graficos de Profundidade de Deficit por CVAR de um caso especifico com criterios de GF
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param tipoGrafico valor numerico identificando o tipo de grafico. 10: CVAR mensal; 11: VAR mensal; 12: VAR anual
#' @param tituloGraficoCVARMes vetor de caracteres com o titulo do grafico de CVAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoVARMes vetor de caracteres com o titulo do grafico de VAR mensal - Nao obrigatorio - valor padrao com numero do caso
#' @param tituloGraficoVARAno vetor de caracteres com o titulo do grafico de VAR anual - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo plotly
#'
#' @export
graficosGF <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, tipoGrafico,
                       tituloGraficoCVARMes = paste0("Profundidade de Déficit - CVaR Mensal 5% - Caso ", numeroCaso),
                       tituloGraficoVARMes = paste0("Profundidade de Déficit - VaR Mensal 5% - Caso ", numeroCaso),
                       tituloGraficoVARAno = paste0("Profundidade de Déficit - VaR Anual 5% - Caso ", numeroCaso)) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT
                      A16.A09_NR_SERIE as serie,
                      A16.A09_NR_MES as anoMes,
                      A02.A02_TX_DESCRICAO_SUBSISTEMA as subsistema,
                      A16_VL_DESPACHO AS defict,
                      A10.A10_VL_DEMANDA AS demanda
                    FROM 
                      BPO_A16_BALANCO AS A16,
                      BPO_A10_DEMANDA AS A10,
                      BPO_A02_SUBSISTEMAS AS A02
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT' AND 
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND 
                      A10.A10_NR_MES = A16.A09_NR_MES AND 
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A10.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A16.A02_NR_SUBSISTEMA = A10.A02_NR_SUBSISTEMA AND
                      A02.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A02.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A02.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # calcula risco Anual (LOLP)
  lolp <- tib.resultados %>% group_by(anoMes, serie) %>% 
    summarise(defict = sum(defict), .groups = "drop") %>% 
    mutate(defict = ifelse(defict > 0, 1,0)) %>%
    summarise(lolp = sum(defict)/n(), .groups = "drop") %>% pull(lolp) %>% 
    percent(accuracy = 0.01, scale = 100, decimal.mark = ",", suffix = "%")
  
  # cria SIN e adiciona
  tib.resultadosSIN <- tib.resultados %>% group_by(serie, anoMes) %>% 
    summarise(defict = sum(defict), demanda = sum(demanda), .groups = "drop") %>% 
    mutate(subsistema = "SIN") %>% select(serie, anoMes, subsistema, defict, demanda)
  
  tib.resultados <- bind_rows(tib.resultados, tib.resultadosSIN)
  
  # calcula a profundidade dos deficts
  tib.resultados <- tib.resultados %>% 
    mutate(profundidade = ifelse(is.nan(defict/demanda), 0, defict/demanda),
           mes = anoMes %% 100)
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  nomeMes <- (1:12 * 100 + 20000001) %>% as.character() %>% as.Date("%Y%m%d") %>% months(abbreviate = T)
  
  if (tipoGrafico == 10) {
    # calcula o CVaR por subsistema e mes
    tib.resultadosCvarMes <- tib.resultados %>%
      group_by(subsistema, mes) %>%
      summarise(cvar = cvar(profundidade, 0.05), .groups = "drop") 
    
    # verifica os subsistemas para o grafico
    subsistemas <- tib.resultadosCvarMes %>% pull(subsistema) %>% unique() %>% sort()
    subsistemasEsconder <- setdiff((1:length(subsistemas)), which(subsistemas == "SIN"))
    
    grafico <- plot_ly(data = tib.resultadosCvarMes, x = ~mes, y = ~cvar, color = ~subsistema, type = "bar",
                       hovertemplate = "<b>Déficit % da Demanda</b>: %{y:.1%}<br><b>Mês</b>: %{x}") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoCVARMes, "</b>"),
        legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
        yaxis = list( 
          title = "<b>Déficit % da Demanda</b>", 
          tickformat = "p" 
        ), 
        xaxis = list( 
          title = "<b>Mês</b>", 
          ticktext = as.list(nomeMes), 
          tickvals = as.list(1:12)
        )
      ) %>% 
      # deixa aparecer somente o primeiro subsistema
      style(visible = "legendonly", traces = subsistemasEsconder) %>% 
      add_annotations(
        x = 0.5,
        y = 1.02,
        xref = "paper", # referencia paper o x e y ficam variando entre 0 e 1 e independente do valor do eixo
        yref = "paper",
        text = paste0("<b>LOLP Anual SIN: ", lolp, "</b>"),
        showarrow = F
      )
    
  } else if (tipoGrafico == 11) {
    # calcula o VaR por subsistema e mes
    tib.resultadosVarMes <- tib.resultados %>%
      group_by(subsistema, mes) %>%
      summarise(var = var(defict, 0.05), .groups = "drop")
    
    # verifica os subsistemas para o grafico
    subsistemas <- tib.resultadosVarMes %>% pull(subsistema) %>% unique() %>% sort()
    subsistemasEsconder <- setdiff((1:length(subsistemas)), which(subsistemas == "SIN"))
    
    grafico <- plot_ly(data = tib.resultadosVarMes, x = ~mes, y = ~var, color = ~subsistema, type = "bar",
                       hovertemplate = "<b>Déficit em MW</b>: %{y:.0f}<br><b>Mês</b>: %{x}") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoVARMes, "</b>"),
        legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
        yaxis = list( 
          title = "<b>Déficit em MW</b>", 
          tickformat = "" 
        ), 
        xaxis = list( 
          title = "<b>Mês</b>", 
          ticktext = as.list(nomeMes), 
          tickvals = as.list(1:12)
        )
      ) %>% 
      # deixa aparecer somente o primeiro subsistema
      style(visible = "legendonly", traces = subsistemasEsconder)
    
  } else {
    # calcula o VaR por subsistema
    tib.resultadosVarAno <- tib.resultados %>%
      group_by(subsistema) %>%
      summarise(var = var(defict, 0.05), .groups = "drop")
    
    # verifica os subsistemas para o grafico
    subsistemas <- tib.resultadosVarAno %>% pull(subsistema) %>% unique() %>% sort()
    subsistemasEsconder <- setdiff((1:length(subsistemas)), which(subsistemas == "SIN"))
    
    grafico <- plot_ly(data = tib.resultadosVarAno, x = 1, y = ~var, color = ~subsistema, type = "bar",
                       hovertemplate = "<b>Déficit em MW</b>: %{y:.0f}") %>% 
      layout( 
        title = paste0("<b>", tituloGraficoVARAno, "</b>"),
        legend = list(title = list(text='<b> Subsistemas </b>')), #orientation = 'h'),
        yaxis = list( 
          title = "<b>Déficit em MW</b>", 
          tickformat = "" 
        ), 
        xaxis = list( 
          title = "<b>Anual</b>", 
          ticktext = as.list(""), 
          tickvals = as.list(1)
        )
      )
  }
  
  return(grafico)
}
