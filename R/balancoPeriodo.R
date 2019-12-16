#' Calcula um balanco de ponta
#'
#' Monta e resolve o problema linear de um balanco de ponta. Funcao criada para poder executar processamento paralelo.
#'
#' @param periodo vetor com o anoMes a ser ser processado. Ex. 201805
#' @param balancoResumido variavel binaria para decidir se vai calcular somente o balanco resumido (\code{BPO_A16_BALANCO}) ou tambem o por gerador (\code{BPO_A17_BALANCO_GERADOR})
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
#' @return \code{lt.resultado} lista com data frames com as estruturas das tabelas com os resultados dos balancos. A lista pode ter 2 ou 3 data frames conforme definido
#' pela variavel \code{balancoResumido}.
#' \itemize{
#' \item Para \code{balancoResumido} == \code{F}: lista com 3 data frames com as estruturas das tabelas \code{BPO_A16_BALANCO} (\code{df.resultado}),
#' \code{BPO_A17_BALANCO_GERADOR} (\code{df.resultadoGerador}) e \code{BPO_A20_BALANCO_SUBSISTEMA} (\code{df.resultadoCMO}) para cada periodo (anoMes)
#' \item Para \code{balancoResumido} == \code{T}: lista com 2 data frames com as estruturas das tabelas \code{BPO_A16_BALANCO} (\code{df.resultado}) e
#' \code{BPO_A20_BALANCO_SUBSISTEMA} (\code{df.resultadoCMO})
#' }
#'
#' @import lpSolveAPI
#' @import dplyr
#' @import DBI
#'
#' @examples
#' balancoPeriodo(201901, T, conexao, df.custoDefict, df.geracaoTermicaTotal,
#' df.geracaoTransmissaoTotal, df.geracaoRenovaveisTotal,
#' df.limitesAgrupamentoLinhasTotal, df.demanda, df.casosAnalise,
#' df.geracaoHidroTotal, df.agrupamentoLinhas, tipoCaso, numeroCaso, codModelo,
#' df.subsistemas, cvuHidro)
#'
#' @export
balancoPeriodo <- function(periodo,
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

  # filtando numero de demandas liquidas
  numeroDemandas <- df.demanda %>% filter(anoMes == periodo) %>% summarise(nDemandas = max(id, na.rm = T)) %>% pull(nDemandas)
  # critica de existencia de dados
  if(numeroDemandas == -Inf) {
    dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 demandas (BPO_A10_DEMANDA) para o per\u00EDodo de ", periodo))
  }

  # cria estrutura de data frame para armazenar resultado
  df.resultado <- data.frame(A16_TP_GERACAO = character(), A02_NR_SUBSISTEMA = integer(), A16_VL_GMIN = double(), A16_VL_DESPACHO = double(),
                             A16_VL_DESPACHO_REDE_ILIMITADA = double(), A16_VL_NAO_DESPACHADO = double(), A01_TP_CASO = double(),
                             A01_NR_CASO = double(), A01_CD_MODELO = double(), A09_NR_MES = double(), A09_NR_SERIE = integer(),
                             A10_NR_SEQ_FREQUENCIA = integer())

  df.resultadoGerador <- data.frame(A16_TP_GERACAO = character(), A17_CD_USINA = integer(), A02_NR_SUBSISTEMA = integer(), A17_VL_GMIN = double(),
                                    A17_VL_DESPACHO = double(), A17_VL_DESPACHO_REDE_ILIMITADA = double(), A17_VL_NAO_DESPACHADO = double(),
                                    A01_TP_CASO = double(), A01_NR_CASO = double(), A01_CD_MODELO = double(), A09_NR_MES = double(),
                                    A09_NR_SERIE = integer(), A10_NR_SEQ_FREQUENCIA = integer())

  df.resultadoCMO <- data.frame(A20_VL_CMO = double(), A01_TP_CASO = double(), A01_NR_CASO = double(), A01_CD_MODELO = double(), A20_NR_MES = double(),
                                A20_NR_SERIE = integer(), A10_NR_SEQ_FREQUENCIA = integer(), A02_NR_SUBSISTEMA = integer())

  for (andaNumSerie in 1:df.casosAnalise$numSeriesHidro) {
    # geracao hidro
    df.geracaoHidro <- df.geracaoHidroTotal %>% filter(anoMes == periodo & serieHidro == andaNumSerie) %>%
      select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
    # critica de existencia de dados
    if(nrow(df.geracaoHidro) == 0) {
      dbDisconnect(conexao)
      stop(paste0("N\u00E3o h\u00E1 h\u00EDdricas (BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA) para o per\u00EDodo de ", periodo))
    }
    df.geracaoHidro$cvu <- cvuHidro

    # geracao total
    df.geracao <- rbind(df.geracaoHidro, df.geracaoRenovaveis, df.geracaoTermica, df.geracaoTransmissao, df.custoDefict)
    # geracao total para balanco sem restricao de transmissao
    df.geracaoSemTransmissao <- df.geracao %>% mutate(disponibilidade = replace(disponibilidade, tipoUsina == 'TRANSMISSAO', Inf))

    for (andaDemandas in 1:numeroDemandas) {
      # filtra demanda especifica (uso particular para carga liquida)
      df.demandaLiquida <- df.demanda %>% filter(anoMes == periodo & id == andaDemandas) %>% select(subsistema, probOcorrencia, demanda)
      # critica de existencia de dados
      if(nrow(df.demandaLiquida) == 0) {
        dbDisconnect(conexao)
        stop(paste0("N\u00E3o h\u00E1 demanda (BPO_A10_DEMANDA) para o per\u00EDodo de ", periodo))
      }

      # Balanco
      # cria modelo de otimizacao linear para ser resolvido no solver tipo Mixed Integer Linear Programming (MILP)
      lpBalanco <- make.lp(nrow = 0, ncol = nrow(df.geracao))

      # define funcao objetivo
      set.objfn(lpBalanco, df.geracao$cvu)

      # define sentido da otimizacao (minimizacao)
      lp.control(lpBalanco, sense = "min")

      # define limites das variaveis
      set.bounds(lpBalanco, lower = df.geracao$inflexibilidade, upper = df.geracao$disponibilidade)

      # define restricoes
      # restricao de demanda
      xtDemanda <- ifelse(df.geracao$transmissao >= 0, 1, -1) # sinais de operacao
      for (andaSubsistema in df.subsistemas$subsistema) {
        indicesDemanda <- which(df.geracao$subsistema == andaSubsistema)
        valorDemanda <- df.demandaLiquida$demanda[df.demandaLiquida$subsistema == andaSubsistema]
        if(df.subsistemas$tipoSistema[df.subsistemas$subsistema == andaSubsistema] == 1) {
          valorDemanda <- 0
        }
        add.constraint(lpBalanco,
                       xt = xtDemanda[indicesDemanda], # define o sinal antes de cada valor (soma ou subtracao)
                       type = "=",
                       rhs = valorDemanda * (1 + df.casosAnalise$reserva),
                       indices = indicesDemanda)
      }
      # restricao de transmissao
      varTransmissao <- df.geracao$transmissao[df.geracao$transmissao > 0]
      for (andaTransmissao in varTransmissao) {
        indicesTransmissao <- which(abs(df.geracao$transmissao) == andaTransmissao)
        add.constraint(lpBalanco,
                       xt = xtDemanda[indicesTransmissao],
                       type = "=",
                       rhs = 0,
                       indices = indicesTransmissao)
      }
      # restricao de grupo de transmissao
      for (andaAgrupamento in df.agrupamentoLinhas %>% select(agrupamento) %>% distinct() %>% pull()) {
        indicesTransmissao <- df.geracao %>% mutate(linha = row_number()) %>% left_join(., df.agrupamentoLinhas, by = "transmissao") %>%
          filter(agrupamento == andaAgrupamento) %>% pull(linha)
        add.constraint(lpBalanco,
                       xt = xtDemanda[indicesTransmissao],
                       type = "<=",
                       rhs = df.limitesAgrupamentoLinhas %>% filter(agrupamento == andaAgrupamento) %>% pull(limite),
                       indices = indicesTransmissao)
      }
      # grava modelo para resolver o problema do solver de se perder ao executar o solver em loop
      write.lp(lpBalanco,
               paste0("C:/CacheRBalanco/", periodo, "sh",andaNumSerie, "d", andaDemandas, "modelo.lp"), type="lp")
      delete.lp(lpBalanco)
      rm(lpBalanco)
      lpBalanco <- read.lp(paste0("C:/CacheRBalanco/", periodo, "sh",andaNumSerie, "d", andaDemandas, "modelo.lp"), type = "lp")

      # escalonamento
      # O algoritmo simplex e um processo de iteracao tipico em que milhares de calculos flutuantes sao feitos para encontrar a solucao ideal.
      # A chance de instabilidade numerica e tambem bastante grande. O escalonamento nao apenas melhora a estabilidade numerica e minimiza erros
      # de arredondamento, mas tambem melhora o desempenho. Quando um modelo nao e bem dimensionado, o algoritmo pode rejeitar certos elementos de pivo
      # porque eles sao muito pequenos e, por causa disso, o solver nao escolhe o caminho mais curto para a solucao.
      lp.control(lpBalanco, scaling = c("extreme", "quadratic", "power2"))

      # resolve o modelo linear
      solucao <- solve(lpBalanco)
      # critica
      if(solucao != 0) {
        dbDisconnect(conexao)
        stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (problema ", solucao ,") para execu\u00E7\u00E3o de ", periodo, ", s\u00E9rie hidro ",
                    andaNumSerie, ", demanda ", andaDemandas))
        # 0: "optimal solution found"
        # 1: "the model is sub-optimal"
        # 2: "the model is infeasible"
        # 3: "the model is unbounded"
        # 4: "the model is degenerate"
        # 5: "numerical failure encountered"
        # 6: "process aborted"
        # 7: "timeout"
        # 9: "the model was solved by presolve"
        # 10: "the branch and bound routine failed"
        # 11: "the branch and bound was stopped because of a break-at-first or break-at-value"
        # 12: "a feasible branch and bound solution was found"
        # 13: "no feasible branch and bound solution was found"
      }
      # exibe solucao
      # write.csv(get.objective(lpBalanco), paste0(tipoCaso, numeroCaso, codModelo, periodo, andaNumSerie, andaDemandas, 'resultado.csv'))
      # Fim Balanco

      # Balanco sem limite de transmissao
      # cria modelo de otimizacao linear para ser resolvido no solver tipo Mixed Integer Linear Programming (MILP)
      lpBalancoSemTransmissao <- make.lp(nrow = 0, ncol = nrow(df.geracaoSemTransmissao))

      # define funcao objetivo
      set.objfn(lpBalancoSemTransmissao, df.geracaoSemTransmissao$cvu)

      # define sentido da otimizacao (minimizacao)
      lp.control(lpBalancoSemTransmissao, sense = "min")

      # define limites das variaveis
      set.bounds(lpBalancoSemTransmissao, lower = df.geracaoSemTransmissao$inflexibilidade, upper = df.geracaoSemTransmissao$disponibilidade)

      # define restricoes
      # restricao de demanda
      xtDemanda <- ifelse(df.geracaoSemTransmissao$transmissao >= 0, 1, -1) # sinais de operacao
      for (andaSubsistema in df.subsistemas$subsistema) {
        indicesDemanda <- which(df.geracaoSemTransmissao$subsistema == andaSubsistema)
        valorDemanda <- df.demandaLiquida$demanda[df.demandaLiquida$subsistema == andaSubsistema]
        if(df.subsistemas$tipoSistema[df.subsistemas$subsistema == andaSubsistema] == 1) {
          valorDemanda <- 0
        }
        add.constraint(lpBalancoSemTransmissao,
                       xt = xtDemanda[indicesDemanda], # define o sinal antes de cada valor (soma ou subtracao)
                       type = "=",
                       rhs = valorDemanda * (1 + df.casosAnalise$reserva),
                       indices = indicesDemanda)
      }
      # restricao de transmissao
      varTransmissao <- df.geracaoSemTransmissao$transmissao[df.geracaoSemTransmissao$transmissao > 0]
      for (andaTransmissao in varTransmissao) {
        indicesTransmissao <- which(abs(df.geracaoSemTransmissao$transmissao) == andaTransmissao)
        add.constraint(lpBalancoSemTransmissao,
                       xt = xtDemanda[indicesTransmissao],
                       type = "=",
                       rhs = 0,
                       indices = indicesTransmissao)
      }
      # grava modelo para resolver o problema do solver de se perder ao executar o solver em loop
      write.lp(lpBalancoSemTransmissao,
               paste0("C:/CacheRBalanco/", periodo, "sh",andaNumSerie, "d", andaDemandas, "modeloBU.lp"), type="lp")
      delete.lp(lpBalancoSemTransmissao)
      rm(lpBalancoSemTransmissao)
      lpBalancoSemTransmissao <- read.lp(paste0("C:/CacheRBalanco/", periodo, "sh",andaNumSerie, "d", andaDemandas, "modeloBU.lp"), type = "lp")

      # escalonamento
      lp.control(lpBalancoSemTransmissao, scaling = c("extreme", "logarithmic", "power2"))

      # resolve o modelo linear
      solucaoSemTransmissao <- solve(lpBalancoSemTransmissao)
      # critica
      if(solucaoSemTransmissao != 0) {
        dbDisconnect(conexao)
        stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel para execu\u00E7\u00E3o com transmiss\u00E3o ilimitada de ", periodo, ",
                    s\u00E9rie hidro ", andaNumSerie, ", demanda ", andaDemandas))

      }
      # Fim Balanco sem limite de transmissao

      # gera resultado
      df.geracao$balanco <- round(get.variables(lpBalanco),2)
      df.geracao$balancoRedeIlimitada <- round(get.variables(lpBalancoSemTransmissao),2)
      df.resultadoIteracao <- df.geracao %>% group_by(tipoUsina, subsistema) %>%
        summarise(A16_VL_GMIN = round(sum(inflexibilidade),2), A16_VL_DESPACHO = round(sum(balanco * ifelse(sign(transmissao) == -1,-1,1)),2),
                  A16_VL_DESPACHO_REDE_ILIMITADA = round(sum(balancoRedeIlimitada * ifelse(sign(transmissao) == -1,-1,1)),2),
                  A16_VL_NAO_DESPACHADO = round((sum(disponibilidade) - A16_VL_DESPACHO),2))
      df.resultadoIteracao <- ungroup(df.resultadoIteracao) # remove atributos do df criados pelo group by
      df.resultadoIteracao$A01_TP_CASO <- tipoCaso
      df.resultadoIteracao$A01_NR_CASO <- numeroCaso
      df.resultadoIteracao$A01_CD_MODELO <- codModelo
      df.resultadoIteracao$A09_NR_MES <- periodo
      df.resultadoIteracao$A09_NR_SERIE <- andaNumSerie
      df.resultadoIteracao$A10_NR_SEQ_FREQUENCIA <- andaDemandas
      colnames(df.resultadoIteracao) <- c("A16_TP_GERACAO", "A02_NR_SUBSISTEMA", "A16_VL_GMIN", "A16_VL_DESPACHO", "A16_VL_DESPACHO_REDE_ILIMITADA",
                                          "A16_VL_NAO_DESPACHADO", "A01_TP_CASO", "A01_NR_CASO", "A01_CD_MODELO", "A09_NR_MES",
                                          "A09_NR_SERIE", "A10_NR_SEQ_FREQUENCIA")

      # concatena resultados agregados do balanco
      df.resultado <- rbind(df.resultado, df.resultadoIteracao)

      # gera resultados de CMO
      subsistemasReais <- df.subsistemas %>% filter(tipoSistema == 0) %>% select(subsistema) %>% pull()
      cmo <- get.dual.solution(lpBalanco) %>% round(2)
      # o primeiro valor de cmo e sempre 1, os calculados vao de 2 ate o numero de restricoes e limites de variaveis
      cmo <- cmo[2:(length(subsistemasReais)+1)]
      df.resultadoCMOIteracao <- data.frame(A20_VL_CMO = cmo)
      df.resultadoCMOIteracao$A01_TP_CASO <- tipoCaso
      df.resultadoCMOIteracao$A01_NR_CASO <- numeroCaso
      df.resultadoCMOIteracao$A01_CD_MODELO <- codModelo
      df.resultadoCMOIteracao$A20_NR_MES <- periodo
      df.resultadoCMOIteracao$A20_NR_SERIE <- andaNumSerie
      df.resultadoCMOIteracao$A10_NR_SEQ_FREQUENCIA <- andaDemandas
      df.resultadoCMOIteracao$A02_NR_SUBSISTEMA <- subsistemasReais

      # concatena resultados agregados de CMO
      df.resultadoCMO <- rbind(df.resultadoCMO, df.resultadoCMOIteracao)

      # gera resultado por gerador
      if (balancoResumido == F) {
        df.resultadoGeradorIteracao <- df.geracao
        indicesTransmissao <- which(df.geracao$tipoUsina == 'TRANSMISSAO')
        df.resultadoGeradorIteracao$codUsina[indicesTransmissao] <- abs(df.resultadoGeradorIteracao$transmissao[indicesTransmissao])
        df.resultadoGeradorIteracao$A17_VL_NAO_DESPACHADO <- round((df.resultadoGeradorIteracao$disponibilidade - df.resultadoGeradorIteracao$balanco),2)
        df.resultadoGeradorIteracao$balanco[indicesTransmissao] <-
          df.resultadoGeradorIteracao$balanco[indicesTransmissao] * sign(df.resultadoGeradorIteracao$transmissao[indicesTransmissao])
        df.resultadoGeradorIteracao$balancoRedeIlimitada[indicesTransmissao] <-
          df.resultadoGeradorIteracao$balancoRedeIlimitada[indicesTransmissao] * sign(df.resultadoGeradorIteracao$transmissao[indicesTransmissao])
        df.resultadoGeradorIteracao <- df.resultadoGeradorIteracao %>% select(-transmissao, -disponibilidade, -cvu)
        df.resultadoGeradorIteracao$A01_TP_CASO <- tipoCaso
        df.resultadoGeradorIteracao$A01_NR_CASO <- numeroCaso
        df.resultadoGeradorIteracao$A01_CD_MODELO <- codModelo
        df.resultadoGeradorIteracao$A09_NR_MES <- periodo
        df.resultadoGeradorIteracao$A09_NR_SERIE <- andaNumSerie
        df.resultadoGeradorIteracao$A10_NR_SEQ_FREQUENCIA <- andaDemandas
        colnames(df.resultadoGeradorIteracao) <- c("A16_TP_GERACAO", "A17_CD_USINA", "A02_NR_SUBSISTEMA", "A17_VL_GMIN", "A17_VL_DESPACHO",
                                                   "A17_VL_DESPACHO_REDE_ILIMITADA", "A17_VL_NAO_DESPACHADO", "A01_TP_CASO", "A01_NR_CASO",
                                                   "A01_CD_MODELO", "A09_NR_MES", "A09_NR_SERIE", "A10_NR_SEQ_FREQUENCIA")
        df.resultadoGerador <- rbind(df.resultadoGerador, df.resultadoGeradorIteracao)
      }
    }
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
