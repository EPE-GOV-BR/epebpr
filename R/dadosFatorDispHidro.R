#' Exporta os dados de fator de disponibilidade hidro
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param inicioHorizonte valor numerico do ano de inicio do horizonte para o grafico. Formato: AAAA. Ex: 2020
#' @param fimHorizonte valor numerico do ano de fim do horizonte para o grafico. Formato: AAAA. Ex:2029
#' @param pctCenarios porcentagem de cenarios para serem considerados no calculo da sobra de disponibilidade
#'
#' @return tib.dadosSobraDispHidro tibble com os dados de fator de disponibilidade hidro
#'
#' @export
dadosFatorDispHidro <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, inicioHorizonte, fimHorizonte, pctCenarios = 5) {
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # querys no banco
  
  # leitura dos dados de pdisp das UHE por cenario
  dispHidroSubsis <- DBI::dbReadTable(conexao, name = 'BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(A09_NR_SERIE, A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, ANO_MES = A09_NR_MES, A02_NR_SUBSISTEMA) %>% 
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte))
  
  # pdisp do SIN por cenario
  dispHidroSin <- dispHidroSubsis %>% 
    dplyr::group_by(A09_NR_SERIE, ANO_MES) %>%
    dplyr::reframe(UHE = sum(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA))
  
  # leitura pdisp termica
  dispTerm <- DBI::dbReadTable(conexao, name = 'BPO_A14_DISPONIBILIDADE_UTE') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select (ANO_MES = A14_NR_MES, A14_VL_DISPONIBILIDADE_MAXIMA_PONTA) %>%
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::group_by(ANO_MES) %>%
    dplyr::reframe(UTE = sum(A14_VL_DISPONIBILIDADE_MAXIMA_PONTA))
  
  # leitura dos dados de pdisp das UTE GNL por cenario
  dispTermGnl <- DBI::dbReadTable(conexao, name = 'BPO_A31_DISPONIBILIDADE_UTE_GNL') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(A14_NR_SERIE, A14_VL_DISPONIBILIDADE_MAXIMA_PONTA, ANO_MES = A14_NR_MES, A02_NR_SUBSISTEMA) %>% 
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::group_by(ANO_MES, A14_NR_SERIE) %>%
    dplyr::reframe(UTE_GNL = sum(A14_VL_DISPONIBILIDADE_MAXIMA_PONTA))
  
  # leitura pdisp renovaveis
  dispRenov <- DBI::dbReadTable(conexao, name = 'BPO_A13_DISPONIBILIDADE_OFR') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(ANO_MES = A13_NR_MES, A13_VL_DISPONIBILIDADE_MAXIMA_PONTA) %>%
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::group_by(ANO_MES) %>%
    dplyr::reframe(OFR = sum(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA, na.rm = TRUE))
  
  # leitura dados de demanda
  demanda <- DBI::dbReadTable(conexao, name = 'BPO_A10_DEMANDA') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(ANO_MES = A10_NR_MES, A10_VL_DEMANDA) %>%
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::group_by(ANO_MES) %>%
    dplyr::reframe(DEMANDA = sum(A10_VL_DEMANDA))
  
  # leitura reserva das fontes e de carga
  reserva <- DBI::dbReadTable(conexao, name = 'BPO_A21_RESERVA') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(ANO_MES = A21_NR_MES, A21_VL_RESERVA_CARGA, A21_VL_RESERVA_FONTES) %>%
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::group_by(ANO_MES) %>%
    dplyr::reframe(RESERVA_C = sum(A21_VL_RESERVA_CARGA),
                   RESERVA_F = sum(A21_VL_RESERVA_FONTES))
  
  # leitura tabela ree/subsistema
  ree <- DBI::dbReadTable(conexao, name = 'BPO_A02_REES') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(A02_NR_REE, A02_NR_SUBSISTEMA)
  
  # leitura do somatorio da potencia das UHE por periodo
  dadosUhe <- DBI::dbReadTable(conexao, name = 'BPO_A05_DADOS_VIGENTES_UHE') %>%
    dplyr::filter(A01_TP_CASO == tipoCaso, A01_NR_CASO == numeroCaso, A01_CD_MODELO == codModelo) %>% 
    dplyr::select(ANO_MES = A05_NR_MES, A05_VL_POTENCIA, A02_NR_REE) %>%
    dplyr::mutate(anoMes = as.character(ANO_MES) %>% zoo::as.yearmon("%Y%m") %>% zoo::as.Date()) %>% 
    dplyr::filter(dplyr::between(as.integer(format(anoMes, "%Y")), inicioHorizonte, fimHorizonte)) %>% 
    dplyr::left_join(ree, by = "A02_NR_REE") %>% 
    dplyr::group_by(ANO_MES, A02_NR_SUBSISTEMA) %>% 
    dplyr::reframe(SOMATORIO_POTENCIA = sum(A05_VL_POTENCIA))
  
  DBI::dbDisconnect(conexao)
  
  # faz o balanço e pega os piores cenarios
  balanco <- dplyr::left_join(dispHidroSin, dispTermGnl, by = c("ANO_MES", "A09_NR_SERIE" = "A14_NR_SERIE")) %>%
    dplyr::left_join(dispTerm, by = "ANO_MES") %>% 
    dplyr::left_join(dispRenov, by = "ANO_MES") %>% 
    dplyr::left_join(demanda, by = "ANO_MES") %>% 
    dplyr::left_join(reserva, by = "ANO_MES") %>% 
    dplyr::mutate(sobra = UHE + UTE + OFR - DEMANDA - RESERVA_C - RESERVA_F,
                  ano = ANO_MES%/%100) %>% 
    dplyr::group_by(ano) %>% 
    dplyr::slice_min(order_by = sobra, prop = pctCenarios/100) %>% 
    dplyr::ungroup()
  
  # cenarios onde ocorreram os piores cenarios
  cenariosSobra <- balanco %>% 
    dplyr::select(A09_NR_SERIE, ANO_MES)
  
  # pega a disp hidro dos piores cenarios
  fatorDisp <- dplyr::left_join(cenariosSobra, dispHidroSubsis, by = c("A09_NR_SERIE", "ANO_MES")) %>% 
    dplyr::left_join(dadosUhe, by = c("ANO_MES", "A02_NR_SUBSISTEMA")) %>%
    dplyr::mutate(ANO = ANO_MES%/%100) %>% 
    dplyr::group_by(ANO, A02_NR_SUBSISTEMA) %>% 
    dplyr::reframe(DISP_HIDRO = mean(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA), POT_HIDRO = mean(SOMATORIO_POTENCIA)) %>% 
    #dplyr::mutate(FATOR_CONTRIBUICAO = DISP_HIDRO/POT_HIDRO) %>% 
    dplyr::select(ANO, SUBSISTEMA = A02_NR_SUBSISTEMA, DISP_HIDRO, POT_HIDRO)#, FATOR_CONTRIBUICAO)
  
  return(fatorDisp)
}
