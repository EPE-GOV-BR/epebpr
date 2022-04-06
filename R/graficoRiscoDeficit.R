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
                                tituloGrafico = paste0("Risco de Déficit - Caso ", numeroCaso)) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco
  query <- paste0("SELECT 
                    A27_NR_MES,
                    A27_VL_LOLP 
                    FROM BPO_A27_LOLP_MENSAL
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultadosMes <- dbGetQuery(conexao, query) %>% as_tibble()
  
  query <- paste0("SELECT 
                    A28_NR_ANO,
                    A28_VL_LOLP 
                    FROM BPO_A28_LOLP_ANUAL
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultadosAno <- dbGetQuery(conexao, query) %>% as_tibble()
  
  dbDisconnect(conexao)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultadosMes <- tib.resultadosMes %>% 
    mutate(anoMes = as.character(A27_NR_MES) %>% as.yearmon("%Y%m") %>% zoo::as.Date(),
           mes = A27_NR_MES %% 100, ano = A27_NR_MES %/% 100) %>% 
    filter(between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # efetua join entre as tibbles de mes e ano para se ter o risco anual na tibble mensal
  tib.resultadosMes <- inner_join(tib.resultadosMes, tib.resultadosAno, by = c("ano" = "A28_NR_ANO"))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosMes %>% filter(months(anoMes) %in% c("janeiro","julho")) %>% pull(anoMes) %>% c(.,max(tib.resultadosMes$anoMes))
  
  # cria vetor auxiliar com as datas em que ocorreu o maior risco mensal em cada ano do horizonte
  maximosAnuais <- tib.resultadosMes %>% group_by(ano) %>% summarise(A27_VL_LOLP = max(A27_VL_LOLP), .groups = "drop") %>% 
    semi_join(tib.resultadosMes, ., by = c("ano", "A27_VL_LOLP")) %>% pull (anoMes)
  filtroDuplicados <- maximosAnuais %>% format("%Y") %>% duplicated() %>% not()
  maximosAnuais <- maximosAnuais[filtroDuplicados]
  
  # exibe grafico mensal de risco
  graficoRisco <- ggplot(tib.resultadosMes, aes(x = anoMes, y = A27_VL_LOLP)) + 
    geom_col(aes(fill = "Risco Mensal")) +
    geom_line(aes(y = A28_VL_LOLP, colour = "Risco Anual"), size = 1.5) +
    geom_text(aes(label = ifelse(anoMes %in% maximosAnuais, percent(A27_VL_LOLP, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
              nudge_y = (ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10 * 0.05),
              hjust = 0.4, show.legend = FALSE, fontface = "bold", size = 5, family = "sans") +
    scale_x_date(name = "Mês", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) + 
    scale_y_continuous(name = "Risco de Déficit", expand = c(0,0), labels = percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
                       breaks = seq(0, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10/16),
                       limits = c(0, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10)) +
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
