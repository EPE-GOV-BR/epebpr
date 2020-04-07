#' Calcula um balanco de ponta usando ROI
#'
#' Monta e resolve o problema linear de um balanco de ponta usando R Optimization Infrastructure (ROI). 
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
#' @param df.casosAnalise data frame com os dados dos casos em analise
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
#' df.limitesAgrupamentoLinhasTotal, df.demanda, df.casosAnalise,
#' df.geracaoHidroTotal, df.agrupamentoLinhas, tipoCaso, numeroCaso, codModelo,
#' df.subsistemas, cvuHidro)}
#'
#' @export
balancoPeriodoROI <- function(periodo,
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
                              df.casosAnalise,
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
  # critica de existencia de dados
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
    stop(paste0("N\u00E3o h\u00E1 h\u00EDdricas (BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA) para o per\u00EDodo de ", periodo, 
                " e s\u00E9rie hidro ", idSerieHidro))
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
    dbExecute(conexao, "ROLLBACK;")
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 demanda (BPO_A10_DEMANDA) para o per\u00EDodo de ", periodo, " e demanda ", idDemanda))
  }
  
  # verifica inconsistencia de limites das variaveis
  inconsistenciaLimites <- df.geracao$disponibilidade - df.geracao$inflexibilidade 
  inconsistenciaLimites <- inconsistenciaLimites < 0
  inconsistenciaLimites <- any(inconsistenciaLimites == T)
  if(inconsistenciaLimites) {
    dbDisconnect(conexao)
    stop(paste0("Problema de limites na gerac\u00E7\u00E3o para execu\u00E7\u00E3o de ", 
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  
  # Balanco
  # cria modelo para ser resolvido no ROI (R Optimization Infrastructure) que administra uma vasta gama de solvers
  # min c'x
  # onde: 
  # limiteInferiorLinha <= Ax <= limiteSuperiorLinha
  # limiteInferiorColuna <= x <= limiteSuperiorColuna

  # define solver usado
  solverBalanco <- "glpk" # lpsolve, glpk
  
  # define restricoes
  # sinais das variaeis - as variaveis podem ser geradores ou linhas de transmissao. 
  # os geradores recebem valor 0 na coluna transmissao do data frame de geracao. As linhas transmissao recebem um codigo indicando os subsistemas 
  # ligados por ela como cada linha e modelada como 2 geradores virtuais nos 2 sistemas que ela liga, um deles deve ter sinal negativo 
  # (retira energia de um sistema e alimenta outro)
  sinalVariavel <- ifelse(df.geracao$transmissao >= 0, 1, -1) # sinais de operacao
  
  # demanda
  identificaSubsistema <- function(x) ifelse(df.geracao$subsistema == x, 1, 0)
  # define a parte da matriz de restricoes A referente a demanda
  matrizRestricoesDemanda <- sapply(df.subsistemas$subsistema, identificaSubsistema) 
  matrizRestricoesDemanda <- (matrizRestricoesDemanda * sinalVariavel) %>% t()
  # define o sinal da restricao
  direcaoResticoesDemanda <- rep("==", nrow(matrizRestricoesDemanda))
  # define os valores de cada linha de restricao para demanda
  rhsRestricaoDemanda <- left_join(df.subsistemas, df.demandaLiquida, by = "subsistema") %>% 
    mutate(demanda = ifelse(is.na(demanda), 0, demanda * (1 + df.casosAnalise$reserva))) %>% pull(demanda)
  # nomes
  # nomesRestricoesDemanda <- paste0("subsistema", df.subsistemas$subsistema)
  
  # tranmissao (geradores virtuais)
  # seleciona variaveis de transmissao, excluindo a duplicidade com sinal negativo
  varTransmissao <- df.geracao$transmissao[df.geracao$transmissao > 0]
  # funcao para identificar a tranmissao
  identificaTransmissao <- function(x) ifelse(abs(df.geracao$transmissao) == x, 1, 0)
  # define a parte da matriz de restricoes A referente as linhas transmissao
  matrizRestricoesTransmissao <- sapply(varTransmissao, identificaTransmissao)
  matrizRestricoesTransmissao <- (matrizRestricoesTransmissao * sinalVariavel) %>% t()
  # define o sinal da restricao
  direcaoResticoesTransmissao <- rep("==", nrow(matrizRestricoesTransmissao))
  # define os valores de cada linha de restricao para as linhas de transmissao. 
  rhsRestricaoTransmissao <- rep(0, nrow(matrizRestricoesTransmissao))

  # agrupamento transmissao
  # seleciona os agrupamentos de transmissao
  varAgrupamento <- df.limitesAgrupamentoLinhas %>% pull(agrupamento)
  # funcao para identificar os agrupamentos de transmissao
  identificaAgrupamento <- function(x) ifelse(df.geracao$transmissao %in% df.agrupamentoLinhas$transmissao[df.agrupamentoLinhas$agrupamento == x], 1, 0)
  # define a parte da matriz de restricoes A referente aos agrupamentos de transmissao
  matrizRestricoesAgrupamento <- sapply(varAgrupamento, identificaAgrupamento)
  matrizRestricoesAgrupamento <- (matrizRestricoesAgrupamento * sinalVariavel) %>% t()
  # define o sinal da restricao
  direcaoResticoesAgrupamento <- rep("<=", nrow(matrizRestricoesAgrupamento))
  # define os valores de cada linha de restricao para as linhas de agrupamento da transmissao. 
  rhsRestricaoAgrupamento <- df.limitesAgrupamentoLinhas %>% pull(limite)

  # junta todas as retricoes e cria o problema a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao, matrizRestricoesAgrupamento)
  direcaoResticoes <- c(direcaoResticoesDemanda, direcaoResticoesTransmissao, direcaoResticoesAgrupamento)
  rhsRestricao <- c(rhsRestricaoDemanda, rhsRestricaoTransmissao, rhsRestricaoAgrupamento)
  
  # define as entradas de acordo com o ROI
  restricoesLinha <- L_constraint(L = matrizRestricoes, dir = direcaoResticoes, rhs = rhsRestricao)
  
  restricoesColuna <- V_bound(li = 1:ncol(matrizRestricoes), ui = 1:ncol(matrizRestricoes), 
                              lb = df.geracao$inflexibilidade, ub = df.geracao$disponibilidade)
  
  # funcao objetivo
  funcaoObjetivo <- L_objective(L = df.geracao$cvu)
  
  # modelo linear
  lpBalanco <- OP(objective = funcaoObjetivo, constraints = restricoesLinha, bounds = restricoesColuna)
  
  # resolve o modelo linear
  solucao <- ROI_solve(lpBalanco, solver = solverBalanco)
  
  # critica
  if(solution(solucao, "status_code") != 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", ROI_plugin_solution_msg(solucao),") para execu\u00E7\u00E3o de ", 
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  
  # solucao primal das variaveis
  primalBalanco <- solution(solucao, "primal")
  
  # solucao dual das restricoes
  dualBalanco <- solution(solucao, "dual")
  if (solverBalanco == "lpsolve") {
    dualBalanco <- dualBalanco[2:length(dualBalanco)]
  }
  if (solverBalanco == "glpk") {
    dualBalanco <- dualBalanco * -1
  }
  
  # Fim Balanco
  
  
  # Balanco sem limite de transmissao
  # junta todas as retricoes definidas no balanco e cria o problema sem limites de transmissao a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao)
  direcaoResticoes <- c(direcaoResticoesDemanda, direcaoResticoesTransmissao)
  rhsRestricao <- c(rhsRestricaoDemanda, rhsRestricaoTransmissao)
  
  # define as entradas de acordo com o ROI
  restricoesLinha <- L_constraint(L = matrizRestricoes, dir = direcaoResticoes, rhs = rhsRestricao)
  
  restricoesColuna <- V_bound(li = 1:ncol(matrizRestricoes), ui = 1:ncol(matrizRestricoes), 
                              lb = df.geracaoSemTransmissao$inflexibilidade, ub = df.geracaoSemTransmissao$disponibilidade)
  
  # funcao objetivo
  funcaoObjetivo <- L_objective(L = df.geracaoSemTransmissao$cvu)
  
  # modelo linear
  lpBalancoSemTransmissao <- OP(objective = funcaoObjetivo, constraints = restricoesLinha, bounds = restricoesColuna)
  
  # resolve o modelo linear
  solucao <- ROI_solve(lpBalancoSemTransmissao, solver = solverBalanco)
  
  # critica
  if(solution(solucao, "status_code") != 0) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", 
                ROI_plugin_solution_msg(solucao),") para execu\u00E7\u00E3o com transmiss\u00E3o ilimitada de ", 
                periodo, ", s\u00E9rie hidro ", idSerieHidro, ", demanda ", idDemanda))
  }
  
  # solucao primal das variaveis
  primalBalancoTransmissao <- solution(solucao, "primal")
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
