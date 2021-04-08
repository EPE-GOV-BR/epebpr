#' Calcula um balanco de potencia usando o solver Coin
#'
#' Monta e resolve o problema linear de um balanco de potencia usando Clp (Coin-or linear programming). Funcao criada para poder executar processamento paralelo.
#'
#' @param periodo vetor com o anoMes a ser ser processado. Ex. 201805
#' @param balancoResumido variavel binaria para decidir se vai calcular somente o balanco resumido (\code{BPO_A16_BALANCO}) ou 
#' tambem o por gerador (\code{BPO_A17_BALANCO_GERADOR})
#' @param conexao conexao com o banco de dados (classe SQLiteConnection)
#' @param df.custoDefict data frame com custos de deficit
#' @param df.geracaoTermicaTotal data frame com os dados das geracoes termicas
#' @param df.geracaoTransmissaoTotal data frame com os dados de transmissao
#' @param df.geracaoRenovaveisTotal data frame com os dados de renovaveis
#' @param df.limitesAgrupamentoLinhasTotal data frame com os dados de agrupamento de transmissao
#' @param df.demanda data frame com os dados de demanda
#' @param df.geracaoHidroTotal data frame com os dados de hidro
#' @param df.agrupamentoLinhas data frame com os dados de agrupamento de linhas
#' @param tipoCaso variavel com o tipo de caso
#' @param numeroCaso variavel com o numero do caso
#' @param codModelo variavel com o codigo do modelo
#' @param df.subsistemas data frame com os dados de subsistemas
#' @param cvuHidro valor de CVU das hidro
#'
#' @return \code{lt.resultado} lista com data frames com as estruturas das tabelas com os resultados dos balancos. 
#' A lista pode ter 2 ou 3 data frames conforme definido pela variavel \code{balancoResumido}.
#' \itemize{
#' \item Para \code{balancoResumido} == \code{F}: lista com 3 data frames com as estruturas das tabelas \code{BPO_A16_BALANCO} (\code{df.resultado}),
#' \code{BPO_A17_BALANCO_GERADOR} (\code{df.resultadoGerador}) e \code{BPO_A20_BALANCO_SUBSISTEMA} (\code{df.resultadoCMO}) para cada periodo (anoMes)
#' \item Para \code{balancoResumido} == \code{T}: lista com 2 data frames com as estruturas das tabelas \code{BPO_A16_BALANCO} (\code{df.resultado}) e
#' \code{BPO_A20_BALANCO_SUBSISTEMA} (\code{df.resultadoCMO})
#' }
#'
#' @examples
#' \dontrun{
#' balancoPeriodo(201901, T, conexao, df.custoDefict, df.geracaoTermicaTotal,
#' df.geracaoTransmissaoTotal, df.geracaoRenovaveisTotal,
#' df.limitesAgrupamentoLinhasTotal, df.demanda, df.geracaoHidroTotal,
#' df.agrupamentoLinhas, tipoCaso, numeroCaso, codModelo,
#' df.subsistemas, cvuHidro)}
#'
#' @export
balancoPeriodoClp <- function(periodo,
                              idDemanda,
                              idSerieHidro,
                              balancoResumido,
                              conexao,
                              df.custoDefict,
                              df.geracaoTermicaTotal,
                              df.geracaoTransmissaoTotal,
                              df.geracaoRenovaveisTotal,
                              df.limitesAgrupamentoLinhasTotal,
                              df.demanda,
                              df.geracaoHidroTotal,
                              df.agrupamentoLinhas,
                              tipoCaso, numeroCaso, codModelo,
                              df.subsistemas,
                              cvuHidro) {
  
  # filtrando geracao termica para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoTermicaTotal %>% filter(anoMes == periodo)) == 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 gera\u00E7\u00E3o t\u00E9rmica (BPO_A14_DISPONIBILIDADE_UTE) para o per\u00EDodo de ", periodo))
    
  }
  # filtrando geracao termica
  df.geracaoTermica <- df.geracaoTermicaTotal %>% filter(anoMes == periodo) %>%
    select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando limites das linhas de transmissao para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoTransmissaoTotal %>% filter(anoMes == periodo)) == 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 transmiss\u00E3o (BPO_A11_INTERCAMBIOS) para o per\u00EDodo de ", periodo))
  }
  # filtrando limites das linhas de transmissao
  df.geracaoTransmissao <- df.geracaoTransmissaoTotal %>% filter(anoMes == periodo) %>%
    select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando geracao revnovavel para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoRenovaveisTotal %>% filter(anoMes == periodo)) == 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 renov\u00E1veis (BPO_A13_DISPONIBILIDADE_OFR) para o per\u00EDodo de ", periodo))
  }
  # filtrando geracao revnovavel
  df.geracaoRenovaveis <- df.geracaoRenovaveisTotal %>% filter(anoMes == periodo) %>%
    select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando limites dos grupos de linhas de transmissao para o mes especifico
  # critica de existencia de dados caso nao seja caso de GF
  if(nrow(df.limitesAgrupamentoLinhasTotal %>% filter(anoMes == periodo)) == 0 & tipoCaso != 3) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 limites de agrupamentos de linhas (BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO) para o per\u00EDodo de ", periodo))
  }
  # filtrando limites dos grupos de linhas de transmissao
  df.limitesAgrupamentoLinhas <- df.limitesAgrupamentoLinhasTotal %>% filter(anoMes == periodo) %>% select(agrupamento, limite)
  
  # geracao hidro
  df.geracaoHidro <- df.geracaoHidroTotal %>% filter(anoMes == periodo, serieHidro == idSerieHidro) %>%
    select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  # critica de existencia de dados
  if(nrow(df.geracaoHidro) == 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 h\u00EDdricas (BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA) para o per\u00EDodo de ", 
                periodo, " e s\u00E9rie hidro ", idSerieHidro))
  }
  df.geracaoHidro$cvu <- cvuHidro
  
  # geracao total
  df.geracao <- rbind(df.geracaoHidro, df.geracaoRenovaveis, df.geracaoTermica, df.geracaoTransmissao, df.custoDefict)
  # corrige pequenas distorcoes
  df.geracao <- df.geracao %>% mutate(disponibilidade = ifelse(((disponibilidade - inflexibilidade) < 0.0001 & (disponibilidade - inflexibilidade) > -0.0001),
                                                               inflexibilidade, disponibilidade))
  # geracao total para balanco sem restricao de transmissao
  df.geracaoSemTransmissao <- df.geracao %>% mutate(disponibilidade = replace(disponibilidade, tipoUsina == 'TRANSMISSAO', Inf))
  
  # filtra demanda especifica (uso particular para carga liquida)
  df.demandaLiquida <- df.demanda %>% filter(anoMes == periodo & id == idDemanda) %>% select(subsistema, probOcorrencia, demanda)
  # critica de existencia de dados
  if(nrow(df.demandaLiquida) == 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 demanda (BPO_A10_DEMANDA) para o per\u00EDodo de ", periodo, " e demanda ", idDemanda))
  }
  
  # verifica inconsistencia de limites das variaveis
  if (any(is.na(df.geracao$disponibilidade))) {
    dbDisconnect(conexao)
    stop(paste0("Problema na disponibilidade da gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  if (any(is.na(df.geracao$inflexibilidade))) {
    dbDisconnect(conexao)
    stop(paste0("Problema na inflexibilidade da gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  inconsistenciaLimites <- df.geracao$disponibilidade - df.geracao$inflexibilidade
  inconsistenciaLimites <- inconsistenciaLimites < 0
  inconsistenciaLimites <- any(inconsistenciaLimites == T)
  if(inconsistenciaLimites) {
    dbDisconnect(conexao)
    stop(paste0("Problema de limites na gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  
  # Balanco
  # cria modelo para ser resolvido no solver do Coin-or Linear Programming
  # min c'x
  # onde: 
  # limiteInferiorLinha <= Ax <= limiteSuperiorLinha
  # limiteInferiorColuna <= x <= limiteSuperiorColuna
  lpBalanco <- initProbCLP()
  
  # define sentido da otimizacao (minimizacao = 1, maximizacao = -1)
  setObjDirCLP(lpBalanco, 1)

  # define restricoes
  # sinais das variaveis - as variaveis podem ser geradores ou linhas de transmissao. 
  # os geradores recebem valor 0 na coluna transmissao do data frame de geracao. As linhas transmissao recebem um codigo indicando os subsistemas 
  # ligados por ela como cada linha e modelada como 2 geradores virtuais nos 2 sistemas que ela liga, um deles deve ter sinal negativo 
  # (retira energia de um sistema e alimenta outro)
  sinalVariavel <- ifelse(df.geracao$transmissao >= 0, 1, -1) # sinais de operacao
  
  # demanda
  identificaSubsistema <- function(x) ifelse(df.geracao$subsistema == x, 1, 0)
  # define a parte da matriz de restricoes A referente a demanda
  matrizRestricoesDemanda <- sapply(df.subsistemas$subsistema, identificaSubsistema)   
  matrizRestricoesDemanda <- (matrizRestricoesDemanda * sinalVariavel) %>% t() 
  # define os limites superior e inferior de cada linha de restricao para demanda. Como e uma igualdade, os limites superior e inferior sao iguais
  ubDemanda <- left_join(df.subsistemas, df.demandaLiquida, by = "subsistema") %>% 
    mutate(demanda = ifelse(is.na(demanda), 0, demanda)) %>% pull(demanda)
  # ifelse(is.na(demanda), 0, demanda * (1 + df.casosAnalise$reserva))
  lbDemanda <- ubDemanda
  
  # tranmissao (geradores virtuais)
  # seleciona variaveis de transmissao, excluindo a duplicidade com sinal negativo
  varTransmissao <- df.geracao$transmissao[df.geracao$transmissao > 0]
  # funcao para identificar a tranmissao
  identificaTransmissao <- function(x) ifelse(abs(df.geracao$transmissao) == x, 1, 0)
  # define a parte da matriz de restricoes A referente as linhas transmissao
  matrizRestricoesTransmissao <- sapply(varTransmissao, identificaTransmissao)
  matrizRestricoesTransmissao <- (matrizRestricoesTransmissao * sinalVariavel) %>% t()
  # define os limites superior e inferior de cada linha de restricao para as linhas de transmissao. 
  # como sao geradores virtuiais alocados em 2 subsistemas, a 'soma' deve ser 0. E como e uma igualdade, os limites sao iguais
  ubTransmissao <- rep(0, nrow(matrizRestricoesTransmissao))
  lbTransmissao <- ubTransmissao

  # agrupamento transmissao
  # nao considera agrupamento caso seja caso de GF
  if (tipoCaso == 3) {
    matrizRestricoesAgrupamento <- numeric()
  } else {
    # seleciona os agrupamentos de transmissao
    varAgrupamento <- df.limitesAgrupamentoLinhas %>% pull(agrupamento)
    # funcao para identificar os agrupamentos de transmissao
    identificaAgrupamento <- function(x) ifelse(df.geracao$transmissao %in% df.agrupamentoLinhas$transmissao[df.agrupamentoLinhas$agrupamento == x], 1, 0)
    # define a parte da matriz de restricoes A referente aos agrupamentos de transmissao
    matrizRestricoesAgrupamento <- sapply(varAgrupamento, identificaAgrupamento)
    matrizRestricoesAgrupamento <- (matrizRestricoesAgrupamento * sinalVariavel) %>% t()
  }
  # define os limites superior e inferior de cada linha de restricao para os agrupamentos de transmissao
  ubAgrupamento <- df.limitesAgrupamentoLinhas %>% pull(limite)
  lbAgrupamento <- rep(0, length(ubAgrupamento))
  
  # junta todas as retricoes e cria o problema a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao, matrizRestricoesAgrupamento) %>% t()
  nlinhas <- ncol(matrizRestricoes)
  # vetor indicando a posicao i da matriz de restricoes A(i,j). No Clp a primeira posicao recebe o valor 0
  ia <- which(matrizRestricoes != 0, arr.ind = T) %>% .[order(.[,1]),] %>% .[,2] - 1
  # vetor indicando a posicao j da matriz de restricoes A(i,j). Como a matriz A possui muitos 0, ela e passada para o Clp como um vetor sem os valores 0.
  # logo esse vetor ja indica a posicao de cada inicio de coluna nesse vetor linha sem 0 e O ultimo valor deve ser o tamanho do vetor linha
  ja <- which(matrizRestricoes != 0, arr.ind = T) %>% .[order(.[,1]),] %>% .[,1] %>% table() %>% as.vector() %>% cumsum() %>% c(0, .)
  # vetor linha contendo os valores diferente de 0 da matriz de restricao A
  ra <- matrizRestricoes %>% t() %>% as.vector()  %>% .[which(. != 0)]
  # limites superiores das linhas da matriz de restricao A
  rub <- c(ubDemanda, ubTransmissao, ubAgrupamento)
  # limites inferiores das linhas da matriz de restricao A
  rlb <- c(lbDemanda, lbTransmissao, lbAgrupamento)
  
  # exemplo para entender o Clp
  # Minimize ou maximize Z = x1 + 2x5 - x8
  # 
  # Sujeito a:
  #  2.5 <=   3x1 +  x2         -  2x4 - x5               -    x8
  #                 2x2 + 1.1x3                                   <=  2.1
  #                          x3              +  x6                ==  4.0
  #  1.8 <=                      2.8x4             -1.2x7         <=  5.0
  #  3.0 <= 5.6x1                      + x5               + 1.9x8 <= 15.0
  # 
  # onde:
  #  2.5 <= x1
  #    0 <= x2 <= 4.1
  #    0 <= x3
  #    0 <= x4
  #  0.5 <= x5 <= 4.0
  #    0 <= x6
  #    0 <= x7
  #    0 <= x8 <= 4.3
  #
  # funcao objetivo
  # obj <- c(1, 0, 0, 0, 2, 0, 0, -1)
  #
  # limites superior e inferior das linhas
  # rlower <- c(2.5, -1000, 4, 1.8, 3)
  # rupper <- c(1000, 2.1, 4, 5, 15)
  #
  # limites superior e inferior das colunas
  # clower <- c(2.5, 0, 0, 0, 0.5, 0, 0, 0)
  # cupper <- c(1000, 4.1, 1, 1, 4, 1000, 1000, 4.3)
  #
  # matriz de restricoes
  # ia <- c(0, 4, 0, 1, 1, 2, 0, 3, 0, 4, 2, 3, 0, 4)
  # ja <- c(0, 2, 4, 6, 8, 10, 11, 12, 14)
  # ra <- c(3.0, 5.6, 1.0, 2.0, 1.1, 1.0, -2.0, 2.8,-1.0, 1.0, 1.0, -1.2, -1.0, 1.9)
  
  loadProblemCLP(lp = lpBalanco, 
                 ncols = nrow(df.geracao), 
                 nrows = nlinhas, 
                 ia = ia, 
                 ja = ja, 
                 ra = ra,
                 lb = df.geracao$inflexibilidade, # define limite inferior das variaveis
                 ub = df.geracao$disponibilidade, # define limite superior das variaveis
                 obj_coef = df.geracao$cvu, # funcao objetivo
                 rlb = rlb, 
                 rub = rub)

  # resolve o modelo linear
  solucao <- solveInitialCLP(lpBalanco)
  # critica
  if(solucao != 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", status_codeCLP(solucao),") para execu\u00E7\u00E3o de ", 
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  # solucao primal das variaveis
  primalBalanco <- getColPrimCLP(lpBalanco)
  
  # solucao dual das restricoes
  dualBalanco <- getRowDualCLP(lpBalanco)
  
  # libera o problema da memoria
  delProbCLP(lpBalanco)
  # Fim Balanco
  
  
  # Balanco sem limite de transmissao
  # cria modelo para ser resolvido no solver Coin-or Linear Programming
  lpBalancoSemTransmissao <- initProbCLP()
  
  # define sentido da otimizacao (minimizacao = 1)
  setObjDirCLP(lpBalancoSemTransmissao, 1)
  
  # junta todas as retricoes definidas no balanco e cria o problema sem limites de transmissao a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao) %>% t()
  nlinhas <- ncol(matrizRestricoes)
  ia <- which(matrizRestricoes != 0, arr.ind = T) %>% .[order(.[,1]),] %>% .[,2] - 1
  ja <- which(matrizRestricoes != 0, arr.ind = T) %>% .[order(.[,1]),] %>% .[,1] %>% table() %>% as.vector() %>% cumsum() %>% c(0, .)
  ra <- matrizRestricoes %>% t() %>% as.vector()  %>% .[which(. != 0)]
  rub <- c(ubDemanda, ubTransmissao)
  rlb <- c(lbDemanda, lbTransmissao)
  
  loadProblemCLP(lp = lpBalancoSemTransmissao, 
                 ncols = nrow(df.geracaoSemTransmissao), 
                 nrows = nlinhas, 
                 ia = ia, 
                 ja = ja, 
                 ra = ra,
                 lb = df.geracaoSemTransmissao$inflexibilidade, # define limite inferior das variaveis
                 ub = df.geracaoSemTransmissao$disponibilidade, # define limite superior das variaveis
                 obj_coef = df.geracaoSemTransmissao$cvu, # funcao objetivo
                 rlb = rlb, 
                 rub = rub)
  
  # resolve o modelo linear
  solucao <- solveInitialCLP(lpBalancoSemTransmissao)
  
  # critica
  if(solucao != 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", 
                status_codeCLP(solucao),") para execu\u00E7\u00E3o com transmiss\u00E3o ilimitada de ", 
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  
  # solucao primal das variaveis
  primalBalancoTransmissao <- getColPrimCLP(lpBalancoSemTransmissao)
  
  # libera o problema da memoria
  delProbCLP(lpBalancoSemTransmissao)
  # Fim Balanco sem limite de transmissao
  
  
  # gera resultado
  df.geracao$balanco <- round(primalBalanco, 2)
  df.geracao$balancoRedeIlimitada <- round(primalBalancoTransmissao, 2)
  df.resultado <- df.geracao %>% group_by(tipoUsina, subsistema) %>%
    summarise(A16_VL_GMIN = round(sum(inflexibilidade),2), A16_VL_DESPACHO = round(sum(balanco * ifelse(sign(transmissao) == -1,-1,1)),2),
              A16_VL_DESPACHO_REDE_ILIMITADA = round(sum(balancoRedeIlimitada * ifelse(sign(transmissao) == -1,-1,1)),2),
              A16_VL_NAO_DESPACHADO = round((sum(disponibilidade) - A16_VL_DESPACHO),2))
  df.resultado <- ungroup(df.resultado) # remove atributos do df criados pelo group by
  df.resultado$A01_TP_CASO <- tipoCaso
  df.resultado$A01_NR_CASO <- numeroCaso
  df.resultado$A01_CD_MODELO <- codModelo
  df.resultado$A09_NR_MES <- periodo
  df.resultado$A09_NR_SERIE <- idSerieHidro
  df.resultado$A10_NR_SEQ_FREQUENCIA <- idDemanda
  colnames(df.resultado) <- c("A16_TP_GERACAO", "A02_NR_SUBSISTEMA", "A16_VL_GMIN", "A16_VL_DESPACHO", "A16_VL_DESPACHO_REDE_ILIMITADA",
                              "A16_VL_NAO_DESPACHADO", "A01_TP_CASO", "A01_NR_CASO", "A01_CD_MODELO", "A09_NR_MES",
                              "A09_NR_SERIE", "A10_NR_SEQ_FREQUENCIA")
  
  # gera resultados de CMO
  subsistemasReais <- df.subsistemas %>% filter(tipoSistema == 0) %>% pull(subsistema)
  idSubsistemasReais <- df.subsistemas %>% mutate(linha = row_number()) %>% filter(tipoSistema == 0) %>% pull(linha)
  cmo <- dualBalanco[idSubsistemasReais] %>% round(2)
  df.resultadoCMO <- data.frame(A20_VL_CMO = cmo)
  df.resultadoCMO$A01_TP_CASO <- tipoCaso
  df.resultadoCMO$A01_NR_CASO <- numeroCaso
  df.resultadoCMO$A01_CD_MODELO <- codModelo
  df.resultadoCMO$A20_NR_MES <- periodo
  df.resultadoCMO$A20_NR_SERIE <- idSerieHidro
  df.resultadoCMO$A10_NR_SEQ_FREQUENCIA <- idDemanda
  df.resultadoCMO$A02_NR_SUBSISTEMA <- subsistemasReais
  
  # gera resultado por gerador
  if (balancoResumido == F) {
    df.resultadoGerador <- df.geracao
    indicesTransmissao <- which(df.geracao$tipoUsina == 'TRANSMISSAO')
    df.resultadoGerador$codUsina[indicesTransmissao] <- abs(df.resultadoGerador$transmissao[indicesTransmissao])
    df.resultadoGerador$A17_VL_NAO_DESPACHADO <- round((df.resultadoGerador$disponibilidade - df.resultadoGerador$balanco),2)
    df.resultadoGerador$balanco[indicesTransmissao] <-
      df.resultadoGerador$balanco[indicesTransmissao] * sign(df.resultadoGerador$transmissao[indicesTransmissao])
    df.resultadoGerador$balancoRedeIlimitada[indicesTransmissao] <-
      df.resultadoGerador$balancoRedeIlimitada[indicesTransmissao] * sign(df.resultadoGerador$transmissao[indicesTransmissao])
    df.resultadoGerador <- df.resultadoGerador %>% select(-transmissao, -disponibilidade, -cvu)
    df.resultadoGerador$A01_TP_CASO <- tipoCaso
    df.resultadoGerador$A01_NR_CASO <- numeroCaso
    df.resultadoGerador$A01_CD_MODELO <- codModelo
    df.resultadoGerador$A09_NR_MES <- periodo
    df.resultadoGerador$A09_NR_SERIE <- idSerieHidro
    df.resultadoGerador$A10_NR_SEQ_FREQUENCIA <- idDemanda
    colnames(df.resultadoGerador) <- c("A16_TP_GERACAO", "A17_CD_USINA", "A02_NR_SUBSISTEMA", "A17_VL_GMIN", "A17_VL_DESPACHO",
                                       "A17_VL_DESPACHO_REDE_ILIMITADA", "A17_VL_NAO_DESPACHADO", "A01_TP_CASO", "A01_NR_CASO",
                                       "A01_CD_MODELO", "A09_NR_MES", "A09_NR_SERIE", "A10_NR_SEQ_FREQUENCIA")
  }
  # limpa o valor infinito do valor nao dispachado dos deficts
  df.resultado <- df.resultado %>% mutate(A16_VL_NAO_DESPACHADO = replace(A16_VL_NAO_DESPACHADO, A16_TP_GERACAO %in% c('DEFICIT','TRANSMISSAO'), NA))
  if (balancoResumido == F) {
    df.resultadoGerador <- df.resultadoGerador %>% mutate(A17_VL_NAO_DESPACHADO = replace(A17_VL_NAO_DESPACHADO, A16_TP_GERACAO == 'DEFICIT', NA))
    lt.resultado <- list(df.resultadoGerador = df.resultadoGerador, df.resultado = df.resultado, df.resultadoCMO = df.resultadoCMO)
  } else {
    lt.resultado <- list(df.resultado = df.resultado, df.resultadoCMO = df.resultadoCMO)
  }
  return(lt.resultado)
}
