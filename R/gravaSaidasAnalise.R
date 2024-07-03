#' Gravacao dos principais dados de saida
#'
#' Faz a gravacao dos dados das principais saidas de analise do balanCo de ponta em arquivo excel e no banco de dados do Balanco de Potencia (BDBP).
#' Os dados sao gravados nas tabelas a partir da BPO_A22 do BDBP
#'
#' @param baseSQLite caracter com a localizacao da base de dados do BP.
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param df.dadosGerais data frame com os dados gerais do caso utilizado
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao dos dados de saida de analise
#' 
#' @examples
#' \dontrun{
#' gravacaoSaidasAnalises("C:/PDE2027_Caso080.sqlite3", 1, 80, 1)
#' }
#'
#' @export
gravacaoSaidasAnalises <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, df.dadosGerais) {
  if (missing(baseSQLite)) {
    stop("favor indicar o caminho da BDBP")
  }
  if (missing(tipoCaso)) {
    stop("favor indicar tipo do caso")
  }
  if (missing(numeroCaso)) {
    stop("favor indicar o n\u00FAmero do caso")
  }
  if (missing(codModelo)) {
    stop("favor indicar o c\u00F3digo do modelo")
  }
  
  # arquivo excel sera salvo na mesma pasta da BD
  pastaSaidaExcel <- dirname(baseSQLite)
  
  # CVaR mensal
  dadosCvarMensal <- dadosGraficoCVAR(baseSQLite, 
                                      tipoCaso,
                                      numeroCaso,
                                      codModelo, 
                                      df.dadosGerais$anoInicio, 
                                      (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                      1) %>% 
    dplyr::mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100, percentualCvar = stringr::str_sub(tamanhoCVAR, 7)) %>% 
    dplyr::select(ano, mes, percentualCvar, tidyr::everything(), -tamanhoCVAR, -maxCVAR, -anoMes, -A09_NR_MES)
  
  # CVaR anual
  dadosCvarAnual <- dadosGraficoCVAR(baseSQLite, 
                                     tipoCaso,
                                     numeroCaso,
                                     codModelo, 
                                     df.dadosGerais$anoInicio, 
                                     (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                     3) %>% 
    dplyr::mutate(percentualCvar = stringr::str_sub(tamanhoCVAR, 7)) %>% 
    dplyr::select(ano, percentualCvar, tidyr::everything(), -tamanhoCVAR)
  
  # CVaR mensal subsistema
  dadosCvarMensalSubs <- dadosGraficoCVARSubsistema(baseSQLite, 
                                                    tipoCaso,
                                                    numeroCaso,
                                                    codModelo, 
                                                    df.dadosGerais$anoInicio, 
                                                    (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1)) %>% 
    dplyr::mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100) %>% 
    dplyr::rename(subsistema = A02_TX_DESCRICAO_SUBSISTEMA, cvar5 = cvar) %>% 
    dplyr::select(ano, mes, subsistema, cvar5, -A09_NR_MES)
  
  # VaR mensal
  dadosVarMensal <- dadosGraficoVAR(baseSQLite, 
                                    tipoCaso,
                                    numeroCaso,
                                    codModelo, 
                                    df.dadosGerais$anoInicio, 
                                    (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                    5) %>% 
    dplyr::mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100, percentualVar = stringr::str_sub(tamanhoVAR, 6)) %>% 
    dplyr::select(ano, mes, percentualVar, tidyr::everything(), -tamanhoVAR, -maxVAR, -anoMes, -A09_NR_MES)
  
  # VaR anual
  dadosVarAnual <- dadosGraficoVAR(baseSQLite, 
                                   tipoCaso,
                                   numeroCaso,
                                   codModelo, 
                                   df.dadosGerais$anoInicio, 
                                   (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                   1) %>% 
    dplyr::mutate(percentualVar = stringr::str_sub(tamanhoVAR, 6)) %>% 
    dplyr::select(ano, percentualVar, tidyr::everything(), -tamanhoVAR)
  
  # Risco SIN
  dadosRisco <- dadosGraficoRiscoDeficit(baseSQLite, 
                                         tipoCaso,
                                         numeroCaso,
                                         codModelo, 
                                         df.dadosGerais$anoInicio, 
                                         (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1))
  dadosRiscoMensal <- dadosRisco %>% 
    dplyr::select(-anoMes, -riscoAnual)
  
  # LOLP Anual
  dadosRiscoAnual <- dadosRisco %>% 
    dplyr::ungroup() %>% 
    dplyr::select(ano, riscoAnual) %>% 
    dplyr::distinct()
  
  # Risco Subs
  dadosRiscoSubs <- dadosGraficoRiscoDeficitSubs(baseSQLite, 
                                                 tipoCaso,
                                                 numeroCaso,
                                                 codModelo, 
                                                 df.dadosGerais$anoInicio, 
                                                 (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1))
  dadosRiscoMensalSubs <- dadosRiscoSubs %>% 
    dplyr::select(-anoMes, -riscoAnual)
  
  # LOLP Anual Subs
  dadosRiscoAnualSubs <- dadosRiscoSubs %>% 
    dplyr::select(subsistema, ano, riscoAnual) %>% 
    dplyr::distinct()
  
  # Requisitos de Potencia
  dadosRequisitoPot <- dadosRequisitoPot(baseSQLite, 
                                         tipoCaso,
                                         numeroCaso,
                                         codModelo, 
                                         df.dadosGerais$anoInicio, 
                                         (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1) )%>% 
    dplyr::mutate(mes = A09_NR_MES%%100) %>% 
    dplyr::select(ano, mes, tidyr::everything(), -anoMes, -A09_NR_MES)
  
  # Requisitos de Potencia quadrimestral
  dadosRequisitoPotQuad <- dadosRequisitoPotQuad(baseSQLite, 
                                                 tipoCaso,
                                                 numeroCaso,
                                                 codModelo, 
                                                 df.dadosGerais$anoInicio, 
                                                 (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1))
  
  # Dados de distribuicao do Deficit e CVaR
  dadosDistDef <- dadosDistDeficit(baseSQLite, 
                                   tipoCaso,
                                   numeroCaso,
                                   codModelo)
  
  # Dados de disponibilidade hidro
  dadosFatorDisp <- dadosFatorDispHidro(baseSQLite, 
                                        tipoCaso, 
                                        numeroCaso, 
                                        codModelo, 
                                        df.dadosGerais$anoInicio, 
                                        (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1), 
                                        5)
  
  # grava todos os df em uma planilha excel
  writexl::write_xlsx(list("CVaR Mensal" = dadosCvarMensal,
                           "CVaR Anual" = dadosCvarAnual, 
                           "CVaR Mensal Subs" = dadosCvarMensalSubs, 
                           "VaR Mensal" = dadosVarMensal, 
                           "VaR Anual" = dadosVarAnual, 
                           "LOLP Mensal SIN" = dadosRiscoMensal,
                           "LOLP Anual SIN" = dadosRiscoAnual,
                           "LOLP Mensal Subs" = dadosRiscoMensalSubs,
                           "LOLP Anual Subs" = dadosRiscoAnualSubs,
                           "Requisito de Pot\u00EAncia" = dadosRequisitoPot,
                           "Requisito de Pot\u00EAncia Quadr" = dadosRequisitoPotQuad,
                           "Distribui\u00E7\u00E3o do D\u00E9ficit" = dadosDistDef[["distDef"]],
                           "Varia\u00E7\u00E3o do CVaR Mensal" = dadosDistDef[["distCvar"]],
                           "Dist D\u00E9ficit x CVaR Max" = dadosDistDef[["distDefxCvarMax"]],
                           "Disp Hidro Anual" = dadosFatorDisp),
                      path = paste0(pastaSaidaExcel, "//resumoSaidasBP_", stringr::str_remove(basename(baseSQLite), "\\.sqlite3"), ".xlsx"))
  
  # abre conexao
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  
  # modifica dfs para compatibilziar com a base
  
  dadosCvarMensalSQL <- dadosCvarMensal %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A22_NR_MES = ano*100 + mes) %>% 
    dplyr::rename(A22_TX_PERCENT_CVAR = percentualCvar, A22_VL_CVAR = cvar) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A22_NR_MES, A22_TX_PERCENT_CVAR, A22_VL_CVAR)
  
  dadosCvarAnualSQL <- dadosCvarAnual %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo) %>% 
    dplyr::rename(A23_NR_ANO = ano, A23_TX_PERCENT_CVAR = percentualCvar, A23_VL_CVAR = cvar) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A23_NR_ANO, A23_TX_PERCENT_CVAR, A23_VL_CVAR)
  
  dadosCvarMensalSubsSQL <- dadosCvarMensalSubs %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A24_NR_MES = ano*100 + mes) %>% 
    dplyr::rename(A02_TX_DESCRICAO_SUBSISTEMA = subsistema, A24_VL_CVAR_5 = cvar5) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A24_NR_MES, A02_TX_DESCRICAO_SUBSISTEMA, A24_VL_CVAR_5)
  
  dadosVarMensalSQL <- dadosVarMensal %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A25_NR_MES = ano*100 + mes) %>% 
    dplyr::rename(A25_TX_PERCENT_VAR = percentualVar, A25_VL_VAR = var) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A25_NR_MES, A25_TX_PERCENT_VAR, A25_VL_VAR)
  
  dadosVarAnualSQL <- dadosVarAnual %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo) %>% 
    dplyr::rename(A26_NR_ANO = ano, A26_TX_PERCENT_VAR = percentualVar, A26_VL_VAR = var) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A26_NR_ANO, A26_TX_PERCENT_VAR, A26_VL_VAR)
  
  dadosRiscoMensalSQL <- dadosRiscoMensal %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A27_NR_MES = ano*100 + mes) %>% 
    dplyr::rename(A27_VL_LOLP = riscoMensal) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A27_NR_MES, A27_VL_LOLP)
  
  dadosRiscoAnualSQL <- dadosRiscoAnual %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo) %>% 
    dplyr::rename(A28_NR_ANO = ano, A28_VL_LOLP = riscoAnual) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A28_NR_ANO, A28_VL_LOLP)
  
  dadosRequisitoPotSQL <- dadosRequisitoPot %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A29_NR_MES = ano*100 + mes) %>% 
    dplyr::rename(A29_VL_VIOLACAO_CRITERIO = violacaoCriterio, A29_VL_LIMITE_CRITERIO = var5) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A29_NR_MES, A29_VL_VIOLACAO_CRITERIO, A29_VL_LIMITE_CRITERIO)
  
  dadosRequisitoPotQuadSQL <- dadosRequisitoPotQuad %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo) %>% 
    dplyr::rename(A30_NR_ANO = ano, A30_NR_QUADRIMESTRE = quad, A30_VL_REQUISITO = reqPot) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A30_NR_ANO, A30_NR_QUADRIMESTRE, A30_VL_REQUISITO)
  
  # limpa as tabelas de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A22_CVAR_MENSAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A22_CVAR_MENSAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A23_CVAR_ANUAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A23_CVAR_ANUAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A24_CVAR_MENSAL_SUBS 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A24_CVAR_MENSAL_SUBS 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A25_VAR_MENSAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A25_VAR_MENSAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A26_VAR_ANUAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A26_VAR_ANUAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A27_LOLP_MENSAL 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A27_LOLP_MENSAL 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A28_LOLP_ANUAL 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A28_LOLP_ANUAL 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A29_REQUISITOS_POTENCIA 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A29_REQUISITOS_POTENCIA 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A30_REQUISITOS_POTENCIA_QUAD
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  
  apagar <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A30_REQUISITOS_POTENCIA_QUAD 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  # salva no BDBP
  DBI::dbWriteTable(conexao, "BPO_A22_CVAR_MENSAL_SIN", dadosCvarMensalSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A23_CVAR_ANUAL_SIN", dadosCvarAnualSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A24_CVAR_MENSAL_SUBS", dadosCvarMensalSubsSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A25_VAR_MENSAL_SIN", dadosVarMensalSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A26_VAR_ANUAL_SIN", dadosVarAnualSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A27_LOLP_MENSAL", dadosRiscoMensalSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A28_LOLP_ANUAL", dadosRiscoAnualSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A29_REQUISITOS_POTENCIA", dadosRequisitoPotSQL, append = T)
  DBI::dbWriteTable(conexao, "BPO_A30_REQUISITOS_POTENCIA_QUAD", dadosRequisitoPotQuadSQL, append = T)
  
  mensagem <- "saidas de analise gravadas com sucesso!"
  
  return(mensagem)
}
