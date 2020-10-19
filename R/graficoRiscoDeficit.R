#' Exibe graficos de risco de defict
#'
#' Monta graficos de risco de defict de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGrafico vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo ggplot
#'
#' @export
graficoRiscoDeficit <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico, 
                                tituloGrafico = paste0("Risco de D\u00E9ficit - Caso ", numeroCaso)) {
  
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
  
  # calcula o risco mensal para os meses com defict
  tib.resultadosMes <- tib.resultados %>% filter(DEFICIT > 0) %>% 
    group_by(ano, mes, anoMes) %>% summarise(riscoMensal = n()/max(SERIES))
  
  # separa os dados com risco 0 para compor o grafico
  tib.resultadoDeficit0 <- tib.resultados %>% group_by(ano, mes, anoMes) %>% 
    summarise(riscoMensal = sum(DEFICIT)) %>% filter(riscoMensal == 0)
  
  # une as informacoes de deficit e ordena por data de forma crescente
  tib.resultadosMes <- rbind(tib.resultadosMes, tib.resultadoDeficit0) %>% 
    arrange(anoMes)
  
  # calcula o risco de defict anual
  tib.resultadosAno <- tib.resultados %>% filter(DEFICIT > 0) %>% 
    group_by(ano) %>% summarise(riscoAnual = n()/max(SERIES)/12)
  
  # efetua join entre as tibbles de mes e ano para se ter o risco anual na tibble mensal
  tib.resultadosMes <- inner_join(tib.resultadosMes, tib.resultadosAno, by = "ano")
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosMes %>% filter(months(anoMes) %in% c("janeiro","julho")) %>% pull(anoMes) %>% c(.,max(tib.resultadosMes$anoMes))
  
  # cria vetor auxiliar com as datas em que ocorreu o maior risco mensal em cada ano do horizonte
  maximosAnuais <- tib.resultadosMes %>% group_by(ano) %>% summarise(riscoMensal = max(riscoMensal)) %>% 
    semi_join(tib.resultadosMes, ., by = c("ano", "riscoMensal")) %>% pull (anoMes)
  filtroDuplicados <- maximosAnuais %>% format("%Y") %>% duplicated() %>% not()
  maximosAnuais <- maximosAnuais[filtroDuplicados]
  
  # carrega fontes de texto para o grafico
  # font_add_google("Montserrat", "Montserrat")
  # showtext_auto()
  
  # exibe grafico mensal de risco
  graficoRisco <- ggplot(tib.resultadosMes, aes(x = anoMes, y = riscoMensal)) + 
    geom_col(aes(fill = "Risco Mensal")) +
    geom_line(aes(y = riscoAnual, colour = "Risco Anual"), size = 1.5) +
    geom_text(aes(label = ifelse(anoMes %in% maximosAnuais, percent(riscoMensal, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
              nudge_y = (ceiling(max(tib.resultadosMes$riscoMensal)*10)/10 * 0.05),
              hjust = 0.4, show.legend = FALSE, fontface = "bold", size = 5, family = "sans") +
    scale_x_date(name = "M\u00EAs", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) + 
    scale_y_continuous(name = "Risco de D\u00E9ficit", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
                       breaks = seq(0, ceiling(max(tib.resultadosMes$riscoMensal)*10)/10, ceiling(max(tib.resultadosMes$riscoMensal)*10)/10/16),
                       limits = c(0, ceiling(max(tib.resultadosMes$riscoMensal)*10)/10)) +
    scale_fill_manual(name = NULL, values = "steelblue") +
    scale_colour_manual(name = NULL, values = "red2") +
    ggtitle(label = tituloGrafico) + 
    expand_limits(x = max(tib.resultadosMes$anoMes) + 20) + # dar uma folga no grafico
    theme(text = element_text(size = 20, family = "sans"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1)),
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
          strip.text.x = element_blank(),
          legend.box.background = element_blank()) +
    guides(colour = guide_legend(nrow = 1))
}