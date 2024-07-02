#' Calcula todo o processo de balanco de potencia
#'
#' Monta e resolve o problema linear de um balanco de potencia. Funcao criada para poder executar processamento paralelo.
#'
#' @param baseSQLite vetor de caracteres com nome e localizacao da base de dados. Ex: "bp.slqlite3"
#' @param tipoCaso variavel com o tipo de caso
#' @param numeroCaso variavel com o numero do caso
#' @param codModelo variavel com o codigo do modelo
#' @param cvuTransmissao  valor de "CVU" para uso da transmissao. Valor declarado baixo para dar prioridade. Ex. 1e-10
#' @param cvuHidro valor de "CVU" para uso das hidraulicas. Valor declarado baixo para dar 3a prioridade. Ex. 2e-8
#' @param cvuRenovaveis valor de "CVU" para uso das outras renovaveis. Valor declarado baixo para dar 2a prioridade. Ex. 1e-8
#' @param cvuOutrasTermicas valor de "CVU" para uso das termicas com CVU decalaro de 0. Valor declarado baixo para dar 4a prioridade. Ex. 0.1
#' @param balancoResumido variavel binaria para decidir se vai gravar somente o balanco resumido (\code{BPO_A16_BALANCO}) ou 
#' tambem o por gerador (\code{BPO_A17_BALANCO_GERADOR}). Valor padrao T.
#' @param distribuicaoDeficit variavel com valor (percentual) da demanda para ser o limite de disponibilidade do deficit distribuido. Valor padrao 0.05
#' @param execShiny booleano que indica se a função está sendo executada em um contexto reativo, para atualização da barra de progresso
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao dos resultados dos balancos 
#' nas tabelas BPO_A16_BALANCO, BPO_A17_BALANCO_GERADOR e BPO_A20_BALANCO_SUBSISTEMA
#'
#' @examples
#' \dontrun{
#' calculaBalancoParalelo("C:/PDE2027_Caso080/bp.slqlite3", 1, 80, 1, 2e-6, 3e-5, 1e-5, 0.1, T, 1, F)}
#' 
#' @importFrom foreach %dopar%
#' 
#' @export
calculaBalancoParalelo <- function(baseSQLite, tipoCaso, numeroCaso, codModelo, cvuTransmissao, cvuHidro, 
                                   cvuRenovaveis, cvuOutrasTermicas, balancoResumido = T, distribuicaoDeficit = 1, execShiny = F) {
  # barra de progresso
  if(execShiny){incProgress(0.05, detail = "Caregando Dados")}
  
  # abre conexao
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # fecha conexao com a base SQLite na saida da funcao, seja por erro ou normalmente
  on.exit(DBI::dbDisconnect(conexao))
  # seleciona os sistemas do balanco
  query <- paste0("SELECT A02_NR_SUBSISTEMA AS subsistema, A02_TP_FICTICIO AS tipoSistema ", 
                  "FROM BPO_A02_SUBSISTEMAS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.subsistemas <- DBI::dbGetQuery(conexao, query)
  if(nrow(df.subsistemas) == 0) {
    DBI::dbDisconnect(conexao)
    stop("Não h\u00E1 dados de sistemas (BPO_A02_SUBSISTEMAS) para o caso!")
  }
  
  # seleciona o numero total de series hidrologicas
  query <- paste0("SELECT A01_NR_SERIES_HIDRO AS numSeriesHidro, ",
                  "A01_NR_MES_INICIO AS inicioHorizonte, A01_NR_MES_FIM AS fimHorizonte ",
                  "FROM BPO_A01_CASOS_ANALISE WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.casosAnalise <- DBI::dbGetQuery(conexao, query)
  
  # custo do defict
  # na tabela BPO_A02_SUBSISTEMAS so ha custo de deficit para os subsistemas reais (A02_TP_FICTICIO = 0)
  query <- paste0("SELECT 'DEFICIT' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, 0 AS transmissao, ", 
                  "0 AS inflexibilidade, 0 AS disponibilidade, A02_VL_CUSTO_DEFICIT as cvu ",
                  "FROM BPO_A02_SUBSISTEMAS WHERE A02_TP_FICTICIO = 0 AND A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " ORDER BY A02_NR_SUBSISTEMA")
  df.custoDefict <- DBI::dbGetQuery(conexao, query) %>% 
    dplyr::mutate(disponibilidade = 999999,
                  cvu = cvu * (1.01 + distribuicaoDeficit)) # aumenta em 1% o custo do deficit para usar o deficit realocado antes
  
  # deficit realocado por subsistema
  # cria uma primeira "geracao" de deficit limitada a um percentual da energia de cada substema com custo igual ao custo de deficit, 
  # assim "forca" o PL a distribuir o deficit ate esse maximo em cada subsistema antes de alocar o deficit aleatoriamente dado que o problema tem multiplas
  # solucoes otimas
  query <- paste0("SELECT 
                      'DEFICITREALOCADO' AS tipoUsina,
                      A.A02_NR_SUBSISTEMA AS codUsina,
                      A.A02_NR_SUBSISTEMA AS subsistema,
                      A.A10_NR_MES AS anoMes,
                      0 AS transmissao,
                      0 AS inflexibilidade,
                      A.A10_VL_DEMANDA AS disponibilidade,
                      B.A02_VL_CUSTO_DEFICIT AS cvu
                    FROM 
                      BPO_A10_DEMANDA A,
                      BPO_A02_SUBSISTEMAS B
                   WHERE 
                      A.A01_TP_CASO = B.A01_TP_CASO AND 
                      A.A01_NR_CASO = B.A01_NR_CASO AND 
                      A.A01_CD_MODELO = B.A01_CD_MODELO AND 
                      A.A02_NR_SUBSISTEMA = B.A02_NR_SUBSISTEMA AND 
                      A02_TP_FICTICIO = 0 AND 
                      A.A01_TP_CASO = ", tipoCaso, " AND 
                      A.A01_NR_CASO = ", numeroCaso, " AND 
                      A.A01_CD_MODELO = ", codModelo)
  df.defictRealocado <- DBI::dbGetQuery(conexao, query)
  
  df.faixasDeficit <- data.frame(tipoUsina = "DEFICITREALOCADO", faixas = seq(0, distribuicaoDeficit, 0.005)) %>% 
    dplyr::mutate(tipoUsinaFaixa = paste0(tipoUsina, stringr::str_replace(faixas, "\\.", "_")))
  
  df.defictRealocado <- dplyr::inner_join(df.defictRealocado, df.faixasDeficit, by = "tipoUsina") %>% 
    dplyr::mutate(tipoUsina = tipoUsinaFaixa, 
                  disponibilidade = ifelse(faixas > 0, 
                                           disponibilidade * 0.005,
                                           0),
                  cvu = cvu * (1 + faixas)) %>% 
    dplyr::filter(disponibilidade > 0) %>% 
    dplyr::select(-tipoUsinaFaixa, -faixas) %>% 
    dplyr::arrange(tipoUsina, codUsina, subsistema, anoMes)
  
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
  df.geracaoTermicaTotal <- DBI::dbGetQuery(conexao, query)
  df.geracaoTermicaTotal$cvu[df.geracaoTermicaTotal$cvu <= 0] <- cvuOutrasTermicas # atribui CVU para termicas com CVU zero
  
  # geracao termica GNL
  query <- paste0("SELECT A31_NR_MES AS anoMes, A31_NR_SERIE AS serieGnl, 'TERMICA' AS tipoUsina, A31_CD_USINA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, ",
                  "0 AS transmissao, A31_VL_INFLEXIBILIDADE AS inflexibilidade, A31_VL_DISPONIBILIDADE_MAXIMA_PONTA AS disponibilidade, A31_VL_CVU as cvu ",
                  "FROM BPO_A31_DISPONIBILIDADE_UTE_GNL WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " ORDER BY A31_NR_MES, A31_NR_SERIE, A02_NR_SUBSISTEMA, A31_CD_USINA")
  df.geracaoTermicaGnl <- DBI::dbGetQuery(conexao, query)
  
  # geracao outras renovaveis
  query <- paste0("SELECT A13_NR_MES AS anoMes, 'RENOVAVEIS' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, ", 
                  "0 AS transmissao, SUM(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA) AS inflexibilidade, ",
                  "SUM(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA) AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo, " GROUP BY A13_NR_MES, A02_NR_SUBSISTEMA")
  df.geracaoRenovaveisTotal <- DBI::dbGetQuery(conexao, query)
  df.geracaoRenovaveisTotal$cvu <- cvuRenovaveis
  
  # geracao fontes de armazenamento
  query <- paste0("SELECT A32_NR_MES AS anoMes, 'ARMAZENAMENTO' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, A02_NR_SUBSISTEMA AS subsistema, ", 
                  "0 AS transmissao, A32_VL_DISPONIBILIDADE_PONTA AS inflexibilidade, ",
                  "A32_VL_DISPONIBILIDADE_PONTA AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.geracaoArmazenamentoTotal <- DBI::dbGetQuery(conexao, query)
  # considera o CVU das tecnologias de armazenamento igual ao cvuHidro
  if(nrow(df.geracaoArmazenamentoTotal) > 0){
    df.geracaoArmazenamentoTotal$cvu <- cvuHidro
  }
  
  # modela limites de transmissao como geradores
  query <- paste0("SELECT A11_NR_MES AS anoMes, 'TRANSMISSAO' AS tipoUsina, A11_NR_SUBSISTEMA_ORIGEM AS codUsina, A11_NR_SUBSISTEMA_ORIGEM AS subsistema, ",
                  " (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) * -1 AS transmissao, ", 
                  "0 AS inflexibilidade, A11_VL_LIMITE_INTERCAMBIO AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A11_INTERCAMBIOS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.geracaoTransmissaoTotal <- DBI::dbGetQuery(conexao, query)
  query <- paste0("SELECT A11_NR_MES AS anoMes, 'TRANSMISSAO' AS tipoUsina, A11_NR_SUBSISTEMA_DESTINO AS codUsina, ",
                  "A11_NR_SUBSISTEMA_DESTINO AS subsistema, (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) AS transmissao, ", 
                  "0 AS inflexibilidade, A11_VL_LIMITE_INTERCAMBIO AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A11_INTERCAMBIOS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.geracaoTransmissaoTotal <- rbind(DBI::dbGetQuery(conexao, query), df.geracaoTransmissaoTotal)
  df.geracaoTransmissaoTotal$cvu <- cvuTransmissao
  
  # agrupamentos de linhas de transmissao
  query <- paste0("SELECT A12_CD_AGRUPAMENTO AS agrupamento, (A11_NR_SUBSISTEMA_ORIGEM * 1000 + A11_NR_SUBSISTEMA_DESTINO) AS transmissao ",
                  "FROM BPO_A15_AGRUPAMENTOS_INTERCAMBIO ",
                  "WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.agrupamentoLinhas <- DBI::dbGetQuery(conexao, query)
  
  # limites de agrupamentos de linhas de transmissao
  query <- paste0("SELECT A12_NR_MES as anoMes, A12_CD_AGRUPAMENTO AS agrupamento, A12_VL_LIMITE_INTERCAMBIO AS limite ",
                  "FROM BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO ",
                  "WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  df.limitesAgrupamentoLinhasTotal <- DBI::dbGetQuery(conexao, query)
  
  # geracao hidro
  query <- paste0("SELECT A09_NR_MES AS anoMes, A09_NR_SERIE AS serieHidro, 'HIDRO' AS tipoUsina, A02_NR_SUBSISTEMA AS codUsina, ",
                  "A02_NR_SUBSISTEMA AS subsistema, 0 AS transmissao, ",
                  "A09_VL_GERACAO_HIDRO_MINIMA AS inflexibilidade, A09_VL_DISPONIBILIDADE_MAXIMA_PONTA AS disponibilidade, 0 as cvu ",
                  "FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  df.geracaoHidroTotal <- DBI::dbGetQuery(conexao, query)
  
  # demanda e reserva operativa
  query <- paste0("SELECT 
                    A.A10_NR_MES AS anoMes,
                    A.A02_NR_SUBSISTEMA AS subsistema,
                    A.A10_NR_TIPO_DEMANDA AS id,
                    A.A10_VL_DEMANDA AS demanda,
                    A21_VL_RESERVA_CARGA as reservaCarga,
                    A21_VL_RESERVA_FONTES as reservaFontes
                  FROM 
                    BPO_A10_DEMANDA A,
                    BPO_A21_RESERVA B
                  WHERE 
                    A.A01_TP_CASO = B.A01_TP_CASO AND 
                    A.A01_NR_CASO = B.A01_NR_CASO AND 
                    A.A01_CD_MODELO = B.A01_CD_MODELO AND 
                    A.A10_NR_MES = B.A21_NR_MES AND 
                    A.A02_NR_SUBSISTEMA = B.A02_NR_SUBSISTEMA AND 
                    A.A10_NR_TIPO_DEMANDA = B.A10_NR_TIPO_DEMANDA AND
                    A.A01_TP_CASO = ", tipoCaso, " AND 
                    A.A01_NR_CASO = ", numeroCaso, " AND 
                    A.A01_CD_MODELO = ", codModelo)
  
  df.demanda <- DBI::dbGetQuery(conexao, query)
  df.demanda <- df.demanda %>% dplyr::mutate(demanda = demanda + reservaCarga + reservaFontes)
  
  # monta data frame com a combinacao de horizonte de estudo e quantidade de series hidro
  df.demandasAnoMesSerie <- tidyr::crossing(data.frame(anoMes = df.demanda$anoMes), data.frame(serie = seq(1, df.casosAnalise$numSeriesHidro)))
  
  DBI::dbExecute(conexao, "BEGIN TRANSACTION;")
  
  # limpa a base de uma eventual rodada anterior
  # BPO_A16_BALANCO
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A16_BALANCO WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- DBI::dbGetQuery(conexao, query)
  if (apagar$TOTAL > 0) {
    query <- paste0("DELETE FROM BPO_A16_BALANCO WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  # BPO_A20_BALANCO_SUBSISTEMA
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A20_BALANCO_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- DBI::dbGetQuery(conexao, query)
  if (apagar$TOTAL > 0) {
    query <- paste0("DELETE FROM BPO_A20_BALANCO_SUBSISTEMA WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  if (balancoResumido == F) {
    # BPO_A17_BALANCO_GERADOR
    query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A17_BALANCO_GERADOR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    apagar <- DBI::dbGetQuery(conexao, query)
    if (apagar$TOTAL > 0) {
      query <- paste0("DELETE FROM BPO_A17_BALANCO_GERADOR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                      " AND A01_CD_MODELO = ", codModelo)
      DBI::dbExecute(conexao, query)
    }
  }
  
  # processamento paralelo
  coresParaUso <- parallel::detectCores() - 1 # disponibiliza todos os cores da CPU menos 1, para nao travar a maquina do usuario
  clusterBalanco <- parallel::makeCluster(coresParaUso)
  doParallel::registerDoParallel(clusterBalanco)
  parallel::clusterExport(clusterBalanco, "balancoPeriodoClp")
  
  # para cluster na saida da funcao, seja por erro ou normalmente
  on.exit(parallel::stopCluster(clusterBalanco), add = T, after = F)
  
  quantidadeCenarios <- nrow(df.demandasAnoMesSerie)
  # calcula as janelas de 1000 registros para inserir em lote
  tamanhoJanela <- 1000
  janelaCenarios <- c(seq(1, quantidadeCenarios, tamanhoJanela), (quantidadeCenarios + 1))
  quantidadeJanela <- length(janelaCenarios)
  
  # se nao for balanco resumido grava BPO_A16_BALANCO, BPO_A17_BALANCO_GERADOR e BPO_A20_BALANCO_SUBSISTEMA
  if (balancoResumido == F) {
    # for criado para nao estourar a alocacao de memoria e ao mesmo tempo nao compromenter desempenho na gravacao na base (gravacao em disco)
    for (andaJanela in 1:(quantidadeJanela - 1)) {
      # barra de progresso
      if(execShiny){incProgress((1/(quantidadeJanela - 1))*0.45, detail = paste(round(andaJanela*100/(quantidadeJanela - 1), 0),"%"))}
      lt.resultado <- foreach::foreach(cenario = seq(janelaCenarios[andaJanela], (janelaCenarios[andaJanela + 1] - 1)),
                                       .combine = "subRBind",
                                       .packages = c("dplyr", "highs", "DBI", "RSQLite"))  %dopar%  balancoPeriodoClp(df.demandasAnoMesSerie$anoMes[cenario],
                                                                                                                       df.demandasAnoMesSerie$serie[cenario],
                                                                                                                       balancoResumido, 
                                                                                                                       conexao,
                                                                                                                       df.custoDefict,
                                                                                                                       df.geracaoTermicaTotal,
                                                                                                                       df.geracaoTermicaGnl,
                                                                                                                       df.geracaoTransmissaoTotal, 
                                                                                                                       df.geracaoRenovaveisTotal,
                                                                                                                       df.geracaoArmazenamentoTotal,
                                                                                                                       df.limitesAgrupamentoLinhasTotal,
                                                                                                                       df.demanda,
                                                                                                                       df.geracaoHidroTotal,
                                                                                                                       df.agrupamentoLinhas,
                                                                                                                       tipoCaso, numeroCaso, codModelo,
                                                                                                                       df.subsistemas,
                                                                                                                       cvuHidro,
                                                                                                                       df.defictRealocado)
      # salva os resultados na base
      # BPO_A16_BALANCO
      DBI::dbWriteTable(conexao, "BPO_A16_BALANCO", lt.resultado$df.resultado, append = T)
      # BPO_A17_BALANCO_GERADOR
      DBI::dbWriteTable(conexao, "BPO_A17_BALANCO_GERADOR", lt.resultado$df.resultadoGerador, append = T)
      # BPO_A20_BALANCO_SUBSISTEMA
      DBI::dbWriteTable(conexao, "BPO_A20_BALANCO_SUBSISTEMA", lt.resultado$df.resultadoCMO, append = T)
    }
    # se for balanco resumido grava BPO_A16_BALANCO e BPO_A20_BALANCO_SUBSISTEMA
  } else {
    for (andaJanela in 1:(quantidadeJanela - 1)) {
      # barra de progresso
      if(execShiny){incProgress((1/(quantidadeJanela - 1))*0.45, detail = paste(round(andaJanela*100/(quantidadeJanela - 1), 0),"%"))}
      lt.resultado <- foreach::foreach(cenario = seq(janelaCenarios[andaJanela], (janelaCenarios[andaJanela + 1] - 1)),
                                       .combine = "subRBind",
                                       .packages = c("dplyr", "highs", "DBI", "RSQLite")) %dopar% balancoPeriodoClp(df.demandasAnoMesSerie$anoMes[cenario],
                                                                                                                     df.demandasAnoMesSerie$serie[cenario], 
                                                                                                                     balancoResumido, 
                                                                                                                     conexao,
                                                                                                                     df.custoDefict,
                                                                                                                     df.geracaoTermicaTotal,
                                                                                                                     df.geracaoTermicaGnl,
                                                                                                                     df.geracaoTransmissaoTotal, 
                                                                                                                     df.geracaoRenovaveisTotal,
                                                                                                                     df.geracaoArmazenamentoTotal,
                                                                                                                     df.limitesAgrupamentoLinhasTotal,
                                                                                                                     df.demanda,
                                                                                                                     df.geracaoHidroTotal,
                                                                                                                     df.agrupamentoLinhas,
                                                                                                                     tipoCaso, numeroCaso, codModelo,
                                                                                                                     df.subsistemas,
                                                                                                                     cvuHidro,
                                                                                                                     df.defictRealocado)
      # salva os resultados na base
      # BPO_A16_BALANCO
      DBI::dbWriteTable(conexao, "BPO_A16_BALANCO", lt.resultado$df.resultado, append = T)
      # BPO_A20_BALANCO_SUBSISTEMA
      DBI::dbWriteTable(conexao, "BPO_A20_BALANCO_SUBSISTEMA", lt.resultado$df.resultadoCMO, append = T)
    }
  }
  
  # # para cluster
  # stopCluster(clusterBalanco)
  # efetua commit no banco de dados
  DBI::dbExecute(conexao, "COMMIT TRANSACTION;")
  # # fecha conexao
  # DBI::dbDisconnect(conexao)
  
  return("Balanço de potência executado e gravado com sucesso!")
}
