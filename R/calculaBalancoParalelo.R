#' Calcula um balanco de ponta
#'
#' Monta e resolve o problema linear de um balanco de ponta. Funcao criada para poder executar processamento paralelo.
#'
#' @param baseSQLite vetor de caracteres com nome e localizacao da base de dados. Ex: "bp.slqlite3"
#' @param tipoCaso variavel com o tipo de caso
#' @param numeroCaso variavel com o numero do caso
#' @param codModelo variavel com o codigo do modelo
#' @param cvuTransmissao  valor de "CVU" para uso da transmissao. Valor declarado baixo para dar prioridade. Ex. 1e-10
#' @param cvuHidro valor de "CVU" para uso das hidraulicas. Valor declarado baixo para dar 3a prioridade. Ex. 2e-8
#' @param cvuRenovaveis valor de "CVU" para uso das outras renovaveis. Valor declarado baixo para dar 2a prioridade. Ex. 1e-8
#' @param cvuOutrasTermicas valor de "CVU" para uso das termicas com CVU decalaro de 0. Valor declarado baixo para dar 4a prioridade. Ex. 0.1
#' @param balancoResumido variavel binaria para decidir se vai gravar somente o balanco resumido (\code{BPO_A16_BALANCO}) ou tambem o por gerador (\code{BPO_A17_BALANCO_GERADOR}).
#' Valor padrao T.
#'
#' @return Grava resultados dos balancos nas tabelas BPO_A16_BALANCO, BPO_A17_BALANCO_GERADOR e BPO_A20_BALANCO_SUBSISTEMA
#'
#' @examples
#' \dontrun{
#' calculaBalancoParalelo("C:/PDE2027_Caso080/bp.slqlite3", 1, 80, 1, 1e-10, 2e-8, 1e-8, 0.1, F)}
#' 
#' @export
calculaBalancoParalelo <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, cvuTransmissao, cvuHidro, 
                                   cvuRenovaveis, cvuOutrasTermicas, balancoResumido = T) {
  # abre conexao
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  
  # seleciona os sistemas do balanco
  query <- paste0("SELECT A02_NR_SUBSISTEMA AS subsistema, A02_TP_FICTICIO AS tipoSistema ", 
                  "FROM BPO_A02_SUBSISTEMAS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.subsistemas <- dbGetQuery(conexao, query)
  if(nrow(df.subsistemas) == 0) {
    dbDisconnect(conexao)
    stop("N\u00E3o h\u00E1 dados de sistemas (BPO_A02_SUBSISTEMAS) para o caso!")
  }
  
  # seleciona o percentual de reserva e o numero total de series hidrologicas
  query <- paste0("SELECT A01_VL_RESERVA_OPERATIVA AS reserva, A01_NR_SERIES_HIDRO AS numSeriesHidro, ",
                  "A01_NR_MES_INICIO AS inicioHorizonte, A01_NR_MES_FIM AS fimHorizonte ",
                  "FROM BPO_A01_CASOS_ANALISE WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.casosAnalise <- dbGetQuery(conexao, query)
  
  # custo do defict
  # na tabela BPO_A02_SUBSISTEMAS so ha custo de deficit para os subsistemas reais (A02_TP_FICTICIO = 0)
  query <- paste0("SELECT 'DEFICIT' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, 0 AS transmissao, ", 
                  "0 AS inflexibilidade, 0 AS disponibilidade, A02_VL_CUSTO_DEFICIT as cvu ",
                  "FROM BPO_A02_SUBSISTEMAS WHERE A02_TP_FICTICIO = 0 AND A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " ORDER BY A02_NR_SUBSISTEMA")
  df.custoDefict <- dbGetQuery(conexao, query)
  df.custoDefict$disponibilidade <- Inf
  
  # horizonte do balanco
  quantidadeMesesHorizonte <- ((df.casosAnalise$fimHorizonte %/% 100) * 12 + df.casosAnalise$fimHorizonte %% 100) - 
    ((df.casosAnalise$inicioHorizonte %/% 100) * 12 + df.casosAnalise$inicioHorizonte %% 100)
  horizonte <- ((zoo::as.yearmon(as.character(df.casosAnalise$inicioHorizonte), "%Y%m")) + seq(0, (quantidadeMesesHorizonte/12), (1/12))) %>% 
    format("%Y%m") %>%  as.integer()
  
  # geracao termica
  query <- paste0("SELECT A14_NR_MES AS anoMes, 'TERMICA' AS tipoUsina, A14_CD_USINA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, ",
                  "0 AS transmissao, A14_VL_INFLEXIBILIDADE AS inflexibilidade, A14_VL_DISPONIBILIDADE_MAXIMA_PONTA AS disponibilidade, A14_VL_CVU as cvu ",
                  "FROM BPO_A14_DISPONIBILIDADE_UTE WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " ORDER BY A14_NR_MES, A02_NR_SUBSISTEMA, A14_CD_USINA")
  df.geracaoTermicaTotal <- dbGetQuery(conexao, query)
  df.geracaoTermicaTotal$cvu[df.geracaoTermicaTotal$cvu <= 0] <- cvuOutrasTermicas # atribui CVU para termicas com CVU zero
  
  # geracao outras renovaveis
  query <- paste0("SELECT A13_NR_MES AS anoMes, 'RENOVAVEIS' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, ", 
                  "0 AS transmissao, SUM(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA) AS inflexibilidade, ",
                  "SUM(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA) AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " GROUP BY A13_NR_MES, A02_NR_SUBSISTEMA")
  df.geracaoRenovaveisTotal <- dbGetQuery(conexao, query)
  df.geracaoRenovaveisTotal$cvu <- cvuRenovaveis
  
  # modela limites de transmissao como geradores
  query <- paste0("SELECT A11_NR_MES AS anoMes, 'TRANSMISSAO' AS tipoUsina, A11_NR_SUBSISTEMA_ORIGEM AS codUsina, A11_NR_SUBSISTEMA_ORIGEM AS subsistema, ",
                  " (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) * -1 AS transmissao, ", 
                  "0 AS inflexibilidade, A11_VL_LIMITE_INTERCAMBIO AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A11_INTERCAMBIOS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.geracaoTransmissaoTotal <- dbGetQuery(conexao, query)
  query <- paste0("SELECT A11_NR_MES AS anoMes, 'TRANSMISSAO' AS tipoUsina, A11_NR_SUBSISTEMA_DESTINO AS codUsina, ",
                  "A11_NR_SUBSISTEMA_DESTINO AS subsistema, (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) AS transmissao, ", 
                  "0 AS inflexibilidade, A11_VL_LIMITE_INTERCAMBIO AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A11_INTERCAMBIOS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.geracaoTransmissaoTotal <- rbind(dbGetQuery(conexao, query), df.geracaoTransmissaoTotal)
  df.geracaoTransmissaoTotal$cvu <- cvuTransmissao
  
  # agrupamentos de linhas de transmissao
  query <- paste0("SELECT A12_CD_AGRUPAMENTO AS agrupamento, (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) AS transmissao ",
                  "FROM BPO_A15_AGRUPAMENTOS_INTERCAMBIO ",
                  "WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.agrupamentoLinhas <- dbGetQuery(conexao, query)
  
  # limites de agrupamentos de linhas de transmissao
  query <- paste0("SELECT A12_NR_MES as anoMes, A12_CD_AGRUPAMENTO AS agrupamento, A12_VL_LIMITE_INTERCAMBIO AS limite ",
                  "FROM BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO ",
                  "WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.limitesAgrupamentoLinhasTotal <- dbGetQuery(conexao, query)
  
  # geracao hidro
  query <- paste0("SELECT A09_NR_MES AS anoMes, A09_NR_SERIE AS serieHidro, 'HIDRO' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, ",
                  "A02_NR_SUBSISTEMA AS subsistema, 0 AS transmissao, ",
                  "A09_VL_GERACAO_HIDRO_MINIMA AS inflexibilidade, A09_VL_DISPONIBILIDADE_MAXIMA_PONTA AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.geracaoHidroTotal <- dbGetQuery(conexao, query)
  
  # demanda
  query <- paste0("SELECT A10_NR_MES AS anoMes, A02_NR_SUBSISTEMA AS subsistema, A10_NR_SEQ_FREQUENCIA as id, A10_VL_FREQUENCIA as probOcorrencia, ",
                  "A10_VL_DEMANDA as demanda FROM BPO_A10_DEMANDA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.demanda <- dbGetQuery(conexao, query)
  
  dbExecute(conexao, "BEGIN TRANSACTION;")
  # limpa a base de uma eventual rodada anterior
  # BPO_A16_BALANCO
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A16_BALANCO WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query)
  if (apagar$TOTAL > 0) {
    query <- paste0("DELETE FROM BPO_A16_BALANCO WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  # BPO_A20_BALANCO_SUBSISTEMA
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A20_BALANCO_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query)
  if (apagar$TOTAL > 0) {
    query <- paste0("DELETE FROM BPO_A20_BALANCO_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  if (balancoResumido == F) {
    # BPO_A17_BALANCO_GERADOR
    query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A17_BALANCO_GERADOR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    apagar <- dbGetQuery(conexao, query)
    if (apagar$TOTAL > 0) {
      query <- paste0("DELETE FROM BPO_A17_BALANCO_GERADOR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                      " AND A01_CD_MODELO = ", codModelo)
      dbExecute(conexao, query)
    }
  }
  
  # processamento paralelo
  coresParaUso <- detectCores() - 2 # disponibiliza todos os cores da CPU menos 2, para nao travar a maquina do usuario
  clusterBalanco <- makeCluster(coresParaUso)
  registerDoParallel(clusterBalanco)
  clusterExport(clusterBalanco, "balancoPeriodo")
  
  if (balancoResumido == F) {
    # for criado para solucionar problema de memoria em relacao ao data frame criado para inclusao no banco 
    anosHorizonte <- as.integer(unique(substr(horizonte, 1,4)))
    for (andaAnos in anosHorizonte) {
      lt.resultado <- foreach(periodo = horizonte[substr(horizonte, 1,4) == andaAnos],
                              .combine = "subRBind",
                              .packages = c("dplyr", "lpSolveAPI", "DBI", "RSQLite"))  %dopar%  balancoPeriodo(periodo, 
                                                                                                               balancoResumido, 
                                                                                                               conexao,
                                                                                                               df.custoDefict,
                                                                                                               df.geracaoTermicaTotal, 
                                                                                                               df.geracaoTransmissaoTotal, 
                                                                                                               df.geracaoRenovaveisTotal, 
                                                                                                               df.limitesAgrupamentoLinhasTotal,
                                                                                                               df.demanda,
                                                                                                               df.casosAnalise,
                                                                                                               df.geracaoHidroTotal,
                                                                                                               df.agrupamentoLinhas,
                                                                                                               tipoCaso, numeroCaso, codModelo,
                                                                                                               df.subsistemas,
                                                                                                               cvuHidro)
      # salva os resultados na base
      # BPO_A16_BALANCO
      dbWriteTable(conexao, "BPO_A16_BALANCO", lt.resultado$df.resultado, append = T)
      # BPO_A17_BALANCO_GERADOR
      dbWriteTable(conexao, "BPO_A17_BALANCO_GERADOR", lt.resultado$df.resultadoGerador, append = T)
      # BPO_A20_BALANCO_SUBSISTEMA
      dbWriteTable(conexao, "BPO_A20_BALANCO_SUBSISTEMA", lt.resultado$df.resultadoCMO, append = T)
      
    }
  } else{
    lt.resultado <- foreach(periodo = horizonte,
                            .combine = "subRBind",
                            .packages = c("dplyr", "lpSolveAPI", "DBI", "RSQLite"))  %dopar%  balancoPeriodo(periodo, 
                                                                                                             balancoResumido, 
                                                                                                             conexao,
                                                                                                             df.custoDefict,
                                                                                                             df.geracaoTermicaTotal, 
                                                                                                             df.geracaoTransmissaoTotal, 
                                                                                                             df.geracaoRenovaveisTotal, 
                                                                                                             df.limitesAgrupamentoLinhasTotal,
                                                                                                             df.demanda,
                                                                                                             df.casosAnalise,
                                                                                                             df.geracaoHidroTotal,
                                                                                                             df.agrupamentoLinhas,
                                                                                                             tipoCaso, numeroCaso, codModelo,
                                                                                                             df.subsistemas,
                                                                                                             cvuHidro)
    # salva os resultados na base
    # BPO_A16_BALANCO
    dbWriteTable(conexao, "BPO_A16_BALANCO", lt.resultado$df.resultado, append = T)
    # BPO_A20_BALANCO_SUBSISTEMA
    dbWriteTable(conexao, "BPO_A20_BALANCO_SUBSISTEMA", lt.resultado$df.resultadoCMO, append = T)
  }
  
  # para cluster
  stopCluster(clusterBalanco)
  # efetua commit no banco de dados
  dbExecute(conexao, "COMMIT TRANSACTION;")
  # fecha conexao
  dbDisconnect(conexao)
}