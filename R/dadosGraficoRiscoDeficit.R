#' Exporta os dados dos graficos de risco de defict
#'
#' Exporta os dados dos graficos de risco de defict de um caso especifico
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param tituloGrafico vetor de caracteres com o titulo do grafico de risco - Nao obrigatorio - valor padrao com numero do caso
#'
#' @return tib.resultadosRisco tibble com os dados do grafico de risco
#'
#' @export
dadosGraficoRiscoDeficit <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonteGrafico, fimHorizonteGrafico, 
                                     tituloGrafico = paste0("Risco de Déficit - Caso ", numeroCaso)) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco para buscar defict por serie para calculo do risco
  query <- paste0("SELECT 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES,
                    SUM(A16_VL_DESPACHO) AS DEFICIT,
                    A01.SERIES
                   FROM 
                    BPO_A16_BALANCO AS A16,
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
                    A01.A01_TP_CASO = A16.A01_TP_CASO AND
                    A01.A01_NR_CASO = A16.A01_NR_CASO AND
                    A01.A01_CD_MODELO = A16.A01_CD_MODELO
                   GROUP BY 
                    A16.A09_NR_SERIE,
                    A16.A09_NR_MES;")
  
  tib.resultados <- DBI::dbGetQuery(conexao, query) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # cria coluna de data anoMes, mes e ano e filtra o horizonte para exibicao no grafico
  tib.resultados <- tib.resultados %>% 
    dplyr::mutate(anoMes = as.character(A09_NR_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date(),
                  mes = A09_NR_MES %% 100, ano = A09_NR_MES %/% 100) %>% 
    dplyr::filter(dplyr::between(ano, inicioHorizonteGrafico, fimHorizonteGrafico))
  
  # calcula o risco mensal para os meses com defict
  tib.resultadosMes <- tib.resultados %>% 
    dplyr::mutate(DEFICIT = ifelse(DEFICIT > 0, 1,0)) %>% 
    dplyr::group_by(ano, mes, anoMes) %>% 
    dplyr::reframe(riscoMensal = sum(DEFICIT)/dplyr::n())
  
  # calcula o risco de defict anual
  tib.resultadosAno <- tib.resultados %>% 
    dplyr::mutate(DEFICIT = ifelse(DEFICIT > 0, 1,0)) %>%
    dplyr::group_by(ano) %>% 
    dplyr::reframe(riscoAnual = sum(DEFICIT)/dplyr::n())
  
  # efetua join entre as tibbles de mes e ano para se ter o risco anual na tibble mensal
  tib.resultadosRisco <- dplyr::inner_join(tib.resultadosMes, tib.resultadosAno, by = "ano")
}
