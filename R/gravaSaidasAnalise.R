#' Gravacao dos principais dados de saida
#'
#' Faz a gravacao dos dados das principais saidas de analise do balanço de ponta em arquivo excel e no banco de dados do Balanco de Potencia (BDBP).
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
#' gravacaoSaidasAnalises("C:/PDE2027_Caso080.sqlite3", 1, 80, 1)}
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
    stop("favor indicar o código do modelo")
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
    mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100, percentualCvar = str_sub(tamanhoCVAR, 7)) %>% 
    select(ano, mes, percentualCvar, everything(), -tamanhoCVAR, -maxCVAR, -anoMes, -A09_NR_MES)
  
  # CVaR anual
  dadosCvarAnual <- dadosGraficoCVAR(baseSQLite, 
                                     tipoCaso,
                                     numeroCaso,
                                     codModelo, 
                                     df.dadosGerais$anoInicio, 
                                     (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                     3) %>% 
    mutate(percentualCvar = str_sub(tamanhoCVAR, 7)) %>% 
    select(ano, percentualCvar, everything(), -tamanhoCVAR)
  
  # CVaR mensal subsistema
  dadosCvarMensalSubs <- dadosGraficoCVARSubsistema(baseSQLite, 
                                                    tipoCaso,
                                                    numeroCaso,
                                                    codModelo, 
                                                    df.dadosGerais$anoInicio, 
                                                    (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1)) %>% 
    mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100) %>% 
    rename(subsistema = A02_TX_DESCRICAO_SUBSISTEMA, cvar5 = cvar) %>% 
    select(ano, mes, subsistema, cvar5, -A09_NR_MES)
  
  # VaR mensal
  dadosVarMensal <- dadosGraficoVAR(baseSQLite, 
                                    tipoCaso,
                                    numeroCaso,
                                    codModelo, 
                                    df.dadosGerais$anoInicio, 
                                    (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                    5) %>% 
    mutate(ano = A09_NR_MES%/%100, mes = A09_NR_MES%%100, percentualVar = str_sub(tamanhoVAR, 6)) %>% 
    select(ano, mes, percentualVar, everything(), -tamanhoVAR, -maxVAR, -anoMes, -A09_NR_MES)
  
  # VaR anual
  dadosVarAnual <- dadosGraficoVAR(baseSQLite, 
                                   tipoCaso,
                                   numeroCaso,
                                   codModelo, 
                                   df.dadosGerais$anoInicio, 
                                   (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1),
                                   1) %>% 
    mutate(percentualVar = str_sub(tamanhoVAR, 6)) %>% 
    select(ano, percentualVar, everything(), -tamanhoVAR)
  
  # Risco
  dadosRisco <- dadosGraficoRiscoDeficit(baseSQLite, 
                                         tipoCaso,
                                         numeroCaso,
                                         codModelo, 
                                         df.dadosGerais$anoInicio, 
                                         (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1))
  dadosRiscoMensal <- dadosRisco %>% select(-anoMes, -riscoAnual)
  
  # LOLP Anual
  dadosRiscoAnual <- dadosRisco %>% ungroup() %>% select(ano, riscoAnual) %>% distinct()
  
  # Risco
  dadosRequisitoPot <- dadosRequisitoPot(baseSQLite, 
                                         tipoCaso,
                                         numeroCaso,
                                         codModelo, 
                                         df.dadosGerais$anoInicio, 
                                         (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1) )%>% 
    mutate(mes = A09_NR_MES%%100) %>% 
    select(ano, mes, everything(), -anoMes, -A09_NR_MES)
  
  # grava todos os df em uma planilha excel
  write_xlsx(list("CVaR Mensal" = dadosCvarMensal,
                  "CVaR Anual" = dadosCvarAnual, 
                  "CVaR Mensal Subs" = dadosCvarMensalSubs, 
                  "VaR Mensal" = dadosVarMensal, 
                  "VaR Anual" = dadosVarAnual, 
                  "LOLP Mensal" = dadosRiscoMensal,
                  "LOLP Anual" = dadosRiscoAnual,
                  "Requisito de Potência" = dadosRequisitoPot),
             path = paste0(pastaSaidaExcel, "//resumoSaidasBP.xlsx"))
  
  # abre conexao
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  
  # modifica dfs para compatibilziar com a base
  
  dadosCvarMensalSQL <- dadosCvarMensal %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A22_NR_MES = ano*100 + mes) %>% 
    rename(A22_TX_PERCENT_CVAR = percentualCvar, A22_VL_CVAR = cvar) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A22_NR_MES, A22_TX_PERCENT_CVAR, A22_VL_CVAR)
  
  dadosCvarAnualSQL <- dadosCvarAnual %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo) %>% 
    rename(A23_NR_ANO = ano, A23_TX_PERCENT_CVAR = percentualCvar, A23_VL_CVAR = cvar) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A23_NR_ANO, A23_TX_PERCENT_CVAR, A23_VL_CVAR)
  
  dadosCvarMensalSubsSQL <- dadosCvarMensalSubs %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A24_NR_MES = ano*100 + mes) %>% 
    rename(A02_TX_DESCRICAO_SUBSISTEMA = subsistema, A24_VL_CVAR_5 = cvar5) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A24_NR_MES, A02_TX_DESCRICAO_SUBSISTEMA, A24_VL_CVAR_5)
  
  dadosVarMensalSQL <- dadosVarMensal %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A25_NR_MES = ano*100 + mes) %>% 
    rename(A25_TX_PERCENT_VAR = percentualVar, A25_VL_VAR = var) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A25_NR_MES, A25_TX_PERCENT_VAR, A25_VL_VAR)
  
  dadosVarAnualSQL <- dadosVarAnual %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo) %>% 
    rename(A26_NR_ANO = ano, A26_TX_PERCENT_VAR = percentualVar, A26_VL_VAR = var) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A26_NR_ANO, A26_TX_PERCENT_VAR, A26_VL_VAR)
  
  dadosRiscoMensalSQL <- dadosRiscoMensal %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A27_NR_MES = ano*100 + mes) %>% 
    rename(A27_VL_LOLP = riscoMensal) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A27_NR_MES, A27_VL_LOLP)
  
  dadosRiscoAnualSQL <- dadosRiscoAnual %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo) %>% 
    rename(A28_NR_ANO = ano, A28_VL_LOLP = riscoAnual) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A28_NR_ANO, A28_VL_LOLP)
  
  dadosRequisitoPotSQL <- dadosRequisitoPot %>% 
    mutate(A01_TP_CASO = tipoCaso,
           A01_NR_CASO = numeroCaso,
           A01_CD_MODELO = codModelo,
           A29_NR_MES = ano*100 + mes) %>% 
    rename(A29_VL_VIOLACAO_CRITERIO = violacaoCriterio, A29_VL_LIMITE_CRITERIO = var5) %>% 
    select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A29_NR_MES, A29_VL_VIOLACAO_CRITERIO, A29_VL_LIMITE_CRITERIO)
  
  # limpa as tabelas de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A22_CVAR_MENSAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A22_CVAR_MENSAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A23_CVAR_ANUAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A23_CVAR_ANUAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A24_CVAR_MENSAL_SUBS 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A24_CVAR_MENSAL_SUBS 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A25_VAR_MENSAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A25_VAR_MENSAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A26_VAR_ANUAL_SIN 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A26_VAR_ANUAL_SIN 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A27_LOLP_MENSAL 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A27_LOLP_MENSAL 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A28_LOLP_ANUAL 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A28_LOLP_ANUAL 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                   BPO_A29_REQUISITOS_POTENCIA 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A29_REQUISITOS_POTENCIA 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  # salva no BDBP
  dbWriteTable(conexao, "BPO_A22_CVAR_MENSAL_SIN", dadosCvarMensalSQL, append = T)
  dbWriteTable(conexao, "BPO_A23_CVAR_ANUAL_SIN", dadosCvarAnualSQL, append = T)
  dbWriteTable(conexao, "BPO_A24_CVAR_MENSAL_SUBS", dadosCvarMensalSubsSQL, append = T)
  dbWriteTable(conexao, "BPO_A25_VAR_MENSAL_SIN", dadosVarMensalSQL, append = T)
  dbWriteTable(conexao, "BPO_A26_VAR_ANUAL_SIN", dadosVarAnualSQL, append = T)
  dbWriteTable(conexao, "BPO_A27_LOLP_MENSAL", dadosRiscoMensalSQL, append = T)
  dbWriteTable(conexao, "BPO_A28_LOLP_ANUAL", dadosRiscoAnualSQL, append = T)
  dbWriteTable(conexao, "BPO_A29_REQUISITOS_POTENCIA", dadosRequisitoPotSQL, append = T)
  
  mensagem <- "saidas de analise gravadas com sucesso!"
  
  return(mensagem)
}
