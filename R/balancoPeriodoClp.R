#' Calcula um balanco de potencia usando o solver Coin
#'
#' Monta e resolve o problema linear de um balanco de potencia usando Clp (Coin-or linear programming). 
#' Funcao criada para poder executar processamento paralelo.
#'
#' @param periodo vetor com o anoMes a ser ser processado. Ex. 201805
#' @param idSerieHidro variavel com o valor da serie hidrologica para a qual sera calculada o balanco
#' @param balancoResumido variavel binaria para decidir se vai calcular somente o balanco resumido (\code{BPO_A16_BALANCO}) ou 
#' tambem o por gerador (\code{BPO_A17_BALANCO_GERADOR})
#' @param conexao conexao com o banco de dados (classe SQLiteConnection)
#' @param df.custoDefict data frame com custos de deficit
#' @param df.geracaoTermicaTotal data frame com os dados das geracoes termicas
#' @param df.geracaoTermicaGnlTotal data frame com os dados das geracoes termicas com despacho antecipado GNL
#' @param df.geracaoTransmissaoTotal data frame com os dados de transmissao
#' @param df.geracaoRenovaveisTotal data frame com os dados de renovaveis
#' @param df.geracaoArmazenamentoTotal data frame com os dados de projetos de armazenamento
#' @param df.limitesAgrupamentoLinhasTotal data frame com os dados de agrupamento de transmissao
#' @param df.demanda data frame com os dados de demanda
#' @param df.geracaoHidroTotal data frame com os dados de hidro
#' @param df.agrupamentoLinhas data frame com os dados de agrupamento de linhas
#' @param tipoCaso variavel com o tipo de caso
#' @param numeroCaso variavel com o numero do caso
#' @param codModelo variavel com o codigo do modelo
#' @param df.subsistemas data frame com os dados de subsistemas
#' @param cvuHidro valor de CVU das hidro
#' @param df.defictRealocadoTotal data frame com os dados do limite de deficit a ser realocado por subsistema
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
#' balancoPeriodoClp(201901, 1, T, conexao, df.custoDefict, df.geracaoTermicaTotal,
#' df.geracaoTermicaGnlTotal, df.geracaoTransmissaoTotal, df.geracaoRenovaveisTotal,
#' df.geracaoArmazenamentoTotal, df.limitesAgrupamentoLinhasTotal, df.demanda,
#' df.geracaoHidroTotal,df.agrupamentoLinhas, tipoCaso, numeroCaso, codModelo,
#' df.subsistemas, cvuHidro, df.defictRealocadoTotal)
#' }
#'
#' @export
balancoPeriodoClp <- function(periodo,
                              idSerieHidro,
                              balancoResumido,
                              conexao,
                              df.custoDefict,
                              df.geracaoTermicaTotal,
                              df.geracaoTermicaGnlTotal,
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
                              df.defictRealocadoTotal) {
  
  # filtra demanda especifica (uso particular para carga liquida)
  df.demandaLiquida <- df.demanda %>% 
    dplyr::filter(anoMes == periodo) %>% 
    dplyr::select(subsistema, demanda)
  
  # critica de existencia de dados
  if(nrow(df.demandaLiquida) == 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 demanda (BPO_A10_DEMANDA) definida para o per\u00EDodo de ", periodo))
  }
  
  # filtrando geracao termica para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoTermicaTotal %>% dplyr::filter(anoMes == periodo)) == 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 gera\u00E7\u00E3o t\u00E9rmica (BPO_A14_DISPONIBILIDADE_UTE) para o per\u00EDodo de ", periodo))
    
  }
  # filtrando geracao termica
  df.geracaoTermica <- df.geracaoTermicaTotal %>% 
    dplyr::filter(anoMes == periodo) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando geracao termica GNL
  df.geracaoTermicaGnl <- df.geracaoTermicaGnlTotal %>% 
    dplyr::filter(anoMes == periodo, serieGnl == idSerieHidro) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando limites das linhas de transmissao para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoTransmissaoTotal %>% dplyr::filter(anoMes == periodo)) == 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 transmiss\u00E3o (BPO_A11_INTERCAMBIOS) para o per\u00EDodo de ", periodo))
  }
  
  # filtrando limites das linhas de transmissao
  df.geracaoTransmissao <- df.geracaoTransmissaoTotal %>% dplyr::filter(anoMes == periodo) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando geracao revnovavel para o mes especifico
  # critica de existencia de dados
  if(nrow(df.geracaoRenovaveisTotal %>% dplyr::filter(anoMes == periodo)) == 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 renov\u00E1veis (BPO_A13_DISPONIBILIDADE_OFR) para o per\u00EDodo de ", periodo))
  }
  
  # filtrando geracao renovavel
  df.geracaoRenovaveis <- df.geracaoRenovaveisTotal %>% dplyr::filter(anoMes == periodo) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando geracao de projetos de armazenamento
  df.geracaoArmazenamento <- df.geracaoArmazenamentoTotal %>% dplyr::filter(anoMes == periodo) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # filtrando limites dos grupos de linhas de transmissao para o mes especifico
  # critica de existencia de dados caso nao seja caso de GF
  if(nrow(df.limitesAgrupamentoLinhasTotal %>% dplyr::filter(anoMes == periodo)) == 0 & tipoCaso != 3) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 limites de agrupamentos de linhas (BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO) para o per\u00EDodo de ", periodo))
  }
  
  # filtrando limites dos grupos de linhas de transmissao
  df.limitesAgrupamentoLinhas <- df.limitesAgrupamentoLinhasTotal %>% 
    dplyr::filter(anoMes == periodo) %>% 
    dplyr::select(agrupamento, limite)
  
  # geracao hidro
  df.geracaoHidro <- df.geracaoHidroTotal %>% 
    dplyr::filter(anoMes == periodo, serieHidro == idSerieHidro) %>%
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # critica de existencia de dados
  if(nrow(df.geracaoHidro) == 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("N\u00E3o h\u00E1 h\u00EDdricas (BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA) para o per\u00EDodo de ", 
                periodo, " e s\u00E9rie hidro ", idSerieHidro))
  }
  df.geracaoHidro$cvu <- cvuHidro
  
  # filtra deficit realocado especifico da execucao
  df.defictRealocado <- df.defictRealocadoTotal %>% dplyr::filter(anoMes == periodo) %>% 
    dplyr::select(tipoUsina, codUsina, subsistema, transmissao, inflexibilidade, disponibilidade, cvu)
  
  # geracao total
  df.geracao <- rbind(df.geracaoHidro, df.geracaoRenovaveis, df.geracaoArmazenamento, df.geracaoTermica, df.geracaoTermicaGnl, df.geracaoTransmissao, df.custoDefict, df.defictRealocado)
  
  # corrige pequenas distorcoes
  df.geracao <- df.geracao %>% 
    dplyr::mutate(disponibilidade = ifelse(((disponibilidade - inflexibilidade) < 0.0001 & (disponibilidade - inflexibilidade) > -0.0001),
                                           inflexibilidade, 
                                           disponibilidade))
  # geracao total para balanco sem restricao de transmissao
  df.geracaoSemTransmissao <- df.geracao %>% 
    dplyr::mutate(disponibilidade = replace(disponibilidade, tipoUsina == 'TRANSMISSAO', Inf))
  
  # verifica inconsistencia de limites das variaveis
  if (any(is.na(df.geracao$disponibilidade))) {
    DBI::dbDisconnect(conexao)
    stop(paste0("Problema na disponibilidade da gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro))
  }
  if (any(is.na(df.geracao$inflexibilidade))) {
    DBI::dbDisconnect(conexao)
    stop(paste0("Problema na inflexibilidade da gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro))
  }
  inconsistenciaLimites <- df.geracao$disponibilidade - df.geracao$inflexibilidade
  inconsistenciaLimites <- inconsistenciaLimites < 0
  inconsistenciaLimites <- any(inconsistenciaLimites == T)
  if(inconsistenciaLimites) {
    DBI::dbDisconnect(conexao)
    stop(paste0("Problema de limites na gera\u00E7\u00E3o para execu\u00E7\u00E3o de ",
                periodo, ", s\u00E9rie hidro ", idSerieHidro))
  }
  
  # Balanco
  # cria modelo para ser resolvido no solver HIGHS
  # min c'x
  # onde: 
  # limiteInferiorLinha <= Ax <= limiteSuperiorLinha
  # limiteInferiorColuna <= x <= limiteSuperiorColuna
  
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
  ubDemanda <- dplyr::left_join(df.subsistemas, df.demandaLiquida, by = "subsistema") %>% 
    dplyr::mutate(demanda = ifelse(is.na(demanda), 0, demanda)) %>% dplyr::pull(demanda)
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
    varAgrupamento <- df.limitesAgrupamentoLinhas %>% dplyr::pull(agrupamento)
    # funcao para identificar os agrupamentos de transmissao
    identificaAgrupamento <- function(x) ifelse(df.geracao$transmissao %in% df.agrupamentoLinhas$transmissao[df.agrupamentoLinhas$agrupamento == x], 1, 0)
    # define a parte da matriz de restricoes A referente aos agrupamentos de transmissao
    matrizRestricoesAgrupamento <- sapply(varAgrupamento, identificaAgrupamento)
    matrizRestricoesAgrupamento <- (matrizRestricoesAgrupamento * sinalVariavel) %>% t()
  }
  # define os limites superior e inferior de cada linha de restricao para os agrupamentos de transmissao
  ubAgrupamento <- df.limitesAgrupamentoLinhas %>% dplyr::pull(limite)
  lbAgrupamento <- rep(0, length(ubAgrupamento))
  
  # junta todas as retricoes e cria o problema a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao, matrizRestricoesAgrupamento)
  # limites superiores das linhas da matriz de restricao
  rub <- c(ubDemanda, ubTransmissao, ubAgrupamento)
  # limites inferiores das linhas da matriz de restricao
  rlb <- c(lbDemanda, lbTransmissao, lbAgrupamento)
  browser()
  # resolve o modelo linear usando o highs
  solucao <- highs::highs_solve(L = df.geracao$cvu, 
                                lower = df.geracao$inflexibilidade, 
                                upper = df.geracao$disponibilidade, 
                                A = matrizRestricoes, 
                                lhs = rlb, 
                                rhs = rub,
                                control = highs::highs_control("random_seed" = 1))
  
  # verifica se a solucao nao foi otima
  if(solucao[["status"]] != 7) {
    # se o solver tiver status unknown, tenta resolver atualizando a semente
    if(solucao[["status"]] == 15){
      seed <- 2
      while(solucao[["status"]] == 15 & seed < 10){
        solucao <- highs::highs_solve(L = df.geracao$cvu, 
                                      lower = df.geracao$inflexibilidade, 
                                      upper = df.geracao$disponibilidade, 
                                      A = matrizRestricoes, 
                                      lhs = rlb, 
                                      rhs = rub,
                                      control = highs::highs_control("random_seed" = seed))
        seed <- seed + 1
      }
    }else{
      DBI::dbDisconnect(conexao)
      stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", solucao[["status_message"]],") para execu\u00E7\u00E3o de ", 
                  periodo, ", s\u00E9rie hidro ", idSerieHidro))
    }
  }
  
  # solucao primal das variaveis
  primalBalanco <- solucao[["primal_solution"]]
  
  # solucao dual das restricoes
  dualBalanco <- solucao[["solver_msg"]][["row_dual"]]
  
  # Fim Balanco
  
  
  # Balanco sem limite de transmissao
  
  # junta todas as retricoes definidas no balanco e cria o problema sem limites de transmissao a ser resolvido
  matrizRestricoes <- rbind(matrizRestricoesDemanda, matrizRestricoesTransmissao)
  # limites superiores das linhas da matriz de restricao
  rub <- c(ubDemanda, ubTransmissao)
  # limites inferiores das linhas da matriz de restricao
  rlb <- c(lbDemanda, lbTransmissao)
  
  # resolve o modelo linear usando o highs
  solucao <- highs::highs_solve(L = df.geracao$cvu, 
                                lower = df.geracao$inflexibilidade, 
                                upper = df.geracao$disponibilidade, 
                                A = matrizRestricoes, 
                                lhs = rlb, 
                                rhs = rub,
                                control = highs::highs_control("random_seed" = 1))
  
  # verifica se a solucao nao foi otima
  if(solucao[["status"]] != 7) {
    # se o solver tiver status unknown, tenta resolver atualizando a semente
    if(solucao[["status"]] == 15){
      seed <- 2
      while(solucao[["status"]] == 15 & seed < 10){
        solucao <- highs::highs_solve(L = df.geracao$cvu, 
                                      lower = df.geracao$inflexibilidade, 
                                      upper = df.geracao$disponibilidade, 
                                      A = matrizRestricoes, 
                                      lhs = rlb, 
                                      rhs = rub,
                                      control = highs::highs_control("random_seed" = seed))
        seed <- seed + 1
      }
    }else{
      DBI::dbDisconnect(conexao)
      stop(paste0("N\u00E3o foi encontrada solu\u00E7\u00E3o vi\u00E1vel (", solucao[["status_message"]],") para execu\u00E7\u00E3o de ", 
                  periodo, ", s\u00E9rie hidro ", idSerieHidro))
    }
  }
  
  # solucao primal das variaveis
  primalBalancoTransmissao <- solucao[["primal_solution"]]
  
  # Fim Balanco sem limite de transmissao
  
  # gera resultado
  df.geracao <- df.geracao %>% dplyr::mutate(balanco = round(primalBalanco, 2),
                                             balancoRedeIlimitada = round(primalBalancoTransmissao, 2))
  df.resultado <- df.geracao %>% dplyr::mutate(tipoUsina = ifelse(startsWith(tipoUsina, "DEFICIT"),
                                                                  "DEFICIT",
                                                                  tipoUsina)) %>% 
    dplyr::group_by(tipoUsina, subsistema) %>%
    dplyr::summarise(A16_VL_GMIN = round(sum(inflexibilidade), 2), 
                     A16_VL_DESPACHO = round(sum(balanco * ifelse(sign(transmissao) == -1, -1, 1)), 2),
                     A16_VL_DESPACHO_REDE_ILIMITADA = round(sum(balancoRedeIlimitada * ifelse(sign(transmissao) == -1, -1, 1)), 2),
                     A16_VL_NAO_DESPACHADO = round((sum(disponibilidade) - A16_VL_DESPACHO), 2), .groups = "drop") %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A01_CD_MODELO = codModelo,
                  A09_NR_MES = periodo,
                  A09_NR_SERIE = idSerieHidro) %>% 
    dplyr::rename(A16_TP_GERACAO = tipoUsina, A02_NR_SUBSISTEMA = subsistema)
  
  # calcula deficit sem reserva
  df.deficitSemReserva <- df.resultado %>% 
    dplyr::filter(A16_TP_GERACAO == "DEFICIT") %>% 
    dplyr::left_join(df.demanda, by = c("A02_NR_SUBSISTEMA" = "subsistema", "A09_NR_MES" = "anoMes")) %>% 
    dplyr::mutate(A16_VL_DESPACHO = A16_VL_DESPACHO - reservaCarga - reservaFontes,
                  A16_VL_DESPACHO = ifelse(A16_VL_DESPACHO < 0,
                                           0,
                                           A16_VL_DESPACHO),
                  A16_TP_GERACAO = "DEFICIT_R") %>% 
    dplyr::select(-id, -demanda, -reservaCarga, -reservaFontes)
  
  # adiciona ao df.resultado
  df.resultado <- rbind(df.resultado, df.deficitSemReserva)
  
  # gera resultados de CMO
  subsistemasReais <- df.subsistemas %>% 
    dplyr::filter(tipoSistema == 0) %>% 
    dplyr::pull(subsistema)
  
  idSubsistemasReais <- df.subsistemas %>% 
    dplyr::mutate(linha = dplyr::row_number()) %>% 
    dplyr::filter(tipoSistema == 0) %>% 
    dplyr::pull(linha)
  
  cmo <- dualBalanco[idSubsistemasReais] %>% round(2) # os primeiros resultados do dualBanco sao os subsistemas na ordem de codigo
  
  df.resultadoCMO <- data.frame(A20_VL_CMO = cmo,
                                A01_TP_CASO = tipoCaso,
                                A01_NR_CASO = numeroCaso,
                                A01_CD_MODELO = codModelo,
                                A20_NR_MES = periodo,
                                A20_NR_SERIE = idSerieHidro,
                                A02_NR_SUBSISTEMA = subsistemasReais)
  
  
  # gera resultado por gerador
  if (balancoResumido == F) {
    
    df.resultadoGerador <- df.geracao %>% dplyr::mutate(A17_CD_USINA = ifelse(tipoUsina == 'TRANSMISSAO',
                                                                              abs(transmissao),
                                                                              codUsina),
                                                        A17_VL_NAO_DESPACHADO = round((disponibilidade - balanco), 2),
                                                        A17_VL_DESPACHO = ifelse(tipoUsina == 'TRANSMISSAO',
                                                                                 balanco * sign(transmissao),
                                                                                 balanco),
                                                        A17_VL_DESPACHO_REDE_ILIMITADA = ifelse(tipoUsina == 'TRANSMISSAO',
                                                                                                balancoRedeIlimitada * sign(transmissao),
                                                                                                balancoRedeIlimitada),
                                                        A01_TP_CASO = tipoCaso,
                                                        A01_NR_CASO = numeroCaso,
                                                        A01_CD_MODELO = codModelo,
                                                        A09_NR_MES = periodo,
                                                        A09_NR_SERIE = idSerieHidro) %>% 
      dplyr::select(-transmissao, -disponibilidade, -cvu, -balanco, -balancoRedeIlimitada, -codUsina) %>% 
      dplyr::rename(A16_TP_GERACAO = tipoUsina, 
                    A17_VL_GMIN = inflexibilidade, 
                    A02_NR_SUBSISTEMA = subsistema)
    
    
  }
  # limpa o valor infinito do valor nao despachado dos deficts
  df.resultado <- df.resultado %>% 
    dplyr::mutate(A16_VL_NAO_DESPACHADO = replace(A16_VL_NAO_DESPACHADO, A16_TP_GERACAO %in% c('DEFICIT','DEFICIT_R','TRANSMISSAO'), NA))
  
  if (balancoResumido == F) {
    df.resultadoGerador <- df.resultadoGerador %>% 
      dplyr::mutate(A17_VL_NAO_DESPACHADO = replace(A17_VL_NAO_DESPACHADO, A16_TP_GERACAO == 'DEFICIT', NA))
    
    lt.resultado <- list(df.resultadoGerador = df.resultadoGerador, df.resultado = df.resultado, df.resultadoCMO = df.resultadoCMO)
  } else {
    lt.resultado <- list(df.resultado = df.resultado, df.resultadoCMO = df.resultadoCMO)
  }
  return(lt.resultado)
}
