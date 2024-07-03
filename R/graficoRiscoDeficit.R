#' Exibe graficos de risco de defict
#'
#' Monta graficos de risco de defict de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonteGrafico valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonteGrafico valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGrafico vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return objeto do tipo ggplot
#'
#' @export
graficoRiscoDeficit <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico, 
                                tituloGrafico = paste0("Risco de D\u00E9ficit - Caso ", numeroCaso)) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco
  query <- paste0("SELECT 
                    A27_NR_MES,
                    A27_VL_LOLP 
                    FROM BPO_A27_LOLP_MENSAL
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultadosMes <- DBI::dbGetQuery(conexao, query) %>% 
    tidyr::as_tibble()
  
  query <- paste0("SELECT 
                    A28_NR_ANO,
                    A28_VL_LOLP 
                    FROM BPO_A28_LOLP_ANUAL
                    WHERE 
                    A01_TP_CASO = ", tipoCaso," AND
                    A01_NR_CASO = ", numeroCaso," AND
                    A01_CD_MODELO = ", codModelo, ";")
  
  tib.resultadosAno <- DBI::dbGetQuery(conexao, query) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultadosMes <- tib.resultadosMes %>% 
    dplyr::mutate(anoMes = as.character(A27_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date(), mes = A27_NR_MES %% 100, ano = A27_NR_MES %/% 100) %>% 
    dplyr::filter(dplyr::between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # efetua join entre as tibbles de mes e ano para se ter o risco anual na tibble mensal
  tib.resultadosMes <- dplyr::inner_join(tib.resultadosMes, tib.resultadosAno, by = c("ano" = "A28_NR_ANO"))
  
  # cria vetor auxiliar com os marcadores que aparecerao no eixo de tempo no grafico
  marcasEixoMes <- tib.resultadosMes %>% 
    dplyr::filter(months(anoMes) %in% c("janeiro","julho")) %>% 
    dplyr::pull(anoMes) %>% c(.,max(tib.resultadosMes$anoMes))
  
  # cria vetor auxiliar com as datas em que ocorreu o maior risco mensal em cada ano do horizonte
  maximosAnuais <- tib.resultadosMes %>% 
    dplyr::group_by(ano) %>% 
    dplyr::summarise(A27_VL_LOLP = max(A27_VL_LOLP), .groups = "drop") %>% 
    dplyr::semi_join(tib.resultadosMes, ., by = c("ano", "A27_VL_LOLP")) %>% 
    dplyr::pull (anoMes)
  
  filtroDuplicados <- maximosAnuais %>% 
    format("%Y") %>% 
    duplicated() %>% 
    not()
  
  maximosAnuais <- maximosAnuais[filtroDuplicados]
  
  # exibe grafico mensal de risco
  graficoRisco <- ggplot2::ggplot(tib.resultadosMes, ggplot2::aes(x = anoMes, y = A27_VL_LOLP)) + 
    ggplot2::geom_col(ggplot2::aes(fill = "Risco Mensal")) +
    ggplot2::geom_line(ggplot2::aes(y = A28_VL_LOLP, colour = "Risco Anual"), size = 1.5) +
    ggplot2::geom_text(ggplot2::aes(label = ifelse(anoMes %in% maximosAnuais, scales::percent(A27_VL_LOLP, accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","), "")), 
                       nudge_y = (ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10 * 0.05),
                       hjust = 0.4, show.legend = FALSE, fontface = "bold", size = 5, family = "sans") +
    ggplot2::scale_x_date(name = "M\u00EDs", date_labels = "%b-%y", expand = c(0,0), breaks = marcasEixoMes) + 
    ggplot2::scale_y_continuous(name = "Risco de D\u00E9ficit", expand = c(0,0), labels = scales::percent_format(accuracy = 0.1, scale = 100, suffix = "%", decimal.mark = ","),
                                breaks = seq(0, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10/16),
                                limits = c(0, ceiling(max(tib.resultadosMes$A27_VL_LOLP)*10)/10)) +
    ggplot2::scale_fill_manual(name = NULL, values = "steelblue") +
    ggplot2::scale_colour_manual(name = NULL, values = "red2") +
    ggplot2::ggtitle(label = tituloGrafico) + 
    ggplot2::expand_limits(x = max(tib.resultadosMes$anoMes) + 20) + # dar uma folga no grafico
    ggplot2::theme(text = ggplot2::element_text(size = 20, family = "sans"),
                   plot.title = ggplot2::element_text(face = "bold", hjust = 0.5, size = rel(1)),
                   strip.background = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "white"),
                   panel.grid.major = ggplot2::element_line(color = "gray92"),
                   panel.grid.major.x = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = "black"),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, color = "black"),
                   axis.text.y = ggplot2::element_text(color = "black"),
                   axis.title = ggplot2::element_text(face = "bold"),
                   legend.position = "bottom",
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = ggplot2::element_blank(),
                   legend.box.background = ggplot2::element_blank()) +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1))
}
