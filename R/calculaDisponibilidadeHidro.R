#' Calcula a disponibilidade hidraulica para o Balanco de Potencia
#'
#' Faz os calculos da disponibilidade hidraulica
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param pastaCaso localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta
#' @param pastaSaidas localizacao dos arquivos de saida do modulo NWLISTOP
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE
#' @param lt.dadosTucurui lista com dados referentes a UHE Tucurui para calculo do PDisp
#' @param flagVert booleano que indica se considera ou nao o vertimento para todas as UHE
#' @param flagUHE booleano que indica se as saidas individuais por UHE deverao ou nao ser gravadas na base de dados
#' @param execShiny booleano que indica se a funcao esta sendo executada em um contexto reativo, para atualizacao da barra de progresso
#' @param tipoModulacao valor inteiro. 1:Modulacao por REE; 2:Modulacao por UHE
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export
calculaDisponibilidadeHidro <- function(baseSQLite, pastaCaso, pastaSaidas, tipoCaso, numeroCaso, codModelo, lt.dadosTucurui, flagVert = FALSE, flagUHE = FALSE, execShiny = FALSE, tipoModulacao) {
  # SQLite
  conexaoSQLite <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # fecha conexao com a base SQLite na saida da funcao, seja por erro ou normalmente
  on.exit(DBI::dbDisconnect(conexaoSQLite))
  
  # pega dados da tabela de dados do caso
  sql <- paste0("SELECT A01_NR_MES_INICIO as dataInicioCaso,
                  A01_NR_MES_FIM as dataFimCaso,
                  A01_NR_HORAS_PONTA as horasPonta,
                  A01_NR_SERIES_HIDRO as numeroSeries
                  FROM BPO_A01_CASOS_ANALISE
                  WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  df.dadosCaso <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  # pega dados da tabela de ree
  sql <- paste0("SELECT A02_NR_REE, A02_NR_SUBSISTEMA, A02_TX_DESCRICAO_REE
                  FROM BPO_A02_REES
                  WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  df.ree <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  # barra de progresso
  if(execShiny){incProgress(4/100, detail = "Excluindo outras execu\u00E7\u00F5es de BP para o mesmo caso")}
  
  ##### EXCLUSAO DE DADOS DE EXECUCOES ANTERIORES #####
  
  # limpa base BPO_A08_DADOS_CALCULADOS_UHE de outras execucoes para o mesmo caso
  DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
  DBI::dbExecute(conexaoSQLite, "PRAGMA journal_mode = TRUNCATE;")
  sql <- paste0("DELETE FROM BPO_A08_DADOS_CALCULADOS_UHE
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  DBI::dbExecute(conexaoSQLite, sql)
  
  # limpa BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA de outras execucoes para o mesmo caso
  sql <- paste0("DELETE FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  DBI::dbExecute(conexaoSQLite, sql)
  DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
  
  # limpa BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA de outras execucoes para o mesmo caso
  sql <- paste0("DELETE FROM BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  DBI::dbExecute(conexaoSQLite, sql)
  DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
  
  # barra de progresso
  if(execShiny){incProgress(4/100, detail = "Atualiza\u00E7\u00E3o de Submotoriza\u00E7\u00E3o")}
  
  ##### SUBMOTORIZACAO #####
  
  # atualizacao de submotorizacao
  quantidadeExpansaoHidro <- df.dadosExpansaoHidro <- leitorrmpe::leituraDadosExpansaoUsinasHidro(pastaCaso) %>% 
    magrittr::extract2("df.dadosExpansaoHidro") %>% nrow()
  
  # se houver alguma expansao hidro segue atualizacao 
  if (quantidadeExpansaoHidro > 0) {
    
    df.submotorizacaoREE <- leitorrmpe::leituraSubmotorizacaoREE(pastaCaso) %>%
      dplyr::inner_join(df.ree, by = c("nomeREE" = "A02_TX_DESCRICAO_REE")) %>%
      dplyr::filter(anoMes >= df.dadosCaso$dataInicioCaso, anoMes <= df.dadosCaso$dataFimCaso)
    
    # cria lista para passar os parametros para update
    lt.submotorizacaoREE <- list(ree = df.submotorizacaoREE$A02_NR_REE, anoMes = df.submotorizacaoREE$anoMes,
                                 submotorizacao = df.submotorizacaoREE$submotorizacao)
    
    sqlUpdate <- paste0("UPDATE BPO_A06_SAIDA_HIDRO_NEWAVE
                    SET A06_VL_SUBMOTORIZACAO = $submotorizacao
                    WHERE
                    A02_NR_REE = $ree AND
                    A06_NR_MES = $anoMes AND
                    A01_TP_CASO = ", tipoCaso, " AND
                    A01_NR_CASO = ", numeroCaso, " AND
                    A01_CD_MODELO = ", codModelo, ";")
    DBI::dbExecute(conexaoSQLite, "BEGIN TRANSACTION;")
    DBI::dbExecute(conexaoSQLite, sqlUpdate, param = lt.submotorizacaoREE)
    DBI::dbExecute(conexaoSQLite, "COMMIT TRANSACTION;")
  }
  # fim atualizacao de submotorizacao
  
  ###### LEITURA DE DADOS ######
  
  # barra de progresso
  if(execShiny){incProgress(8/100, detail = "Leitura de dados")}
  
  sql <- paste0("SELECT * FROM BPO_A02_REES
                 WHERE A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo)
  ree <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  sql <- paste0("SELECT 
                  A03_CD_USINA, A02_NR_REE,
                  A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4,
                  A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE 
                 FROM BPO_A03_DADOS_UHE
                 WHERE
                  A03_TX_STATUS <> 'NC' AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo)
  df.dadosUHE <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  sql <- paste0("SELECT A03_CD_USINA,
                  SUM(A04_NR_MAQUINAS * A04_VL_POTENCIA) AS POT_TOTAL
                 FROM BPO_A04_MAQUINAS_UHE
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, " 
                 GROUP BY A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA")
  df.dadosMaquinasUHE <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  sql <- paste0("SELECT A03_CD_USINA, A05_NR_MES, A02_NR_REE,              
                  A05_NR_CANAL_FUGA_MEDIO, A05_VL_VOL_MAX, A05_VL_VOL_MIN, A05_VL_VAZAO_MINIMA, A05_VL_TEIF, A05_VL_IP, A05_VL_POTENCIA
                 FROM BPO_A05_DADOS_VIGENTES_UHE
                 WHERE
                  A05_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo)
  df.dadosVigentesUHE <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  sql <- paste0("SELECT A02_NR_REE, A06_NR_MES, A06_NR_SERIE, A06_VL_PERC_ARMAZENAMENTO, A06_VL_GERACAO_HIDRAULICA, A06_VL_SUBMOTORIZACAO, A06_VL_VERTIMENTO_TURBINAVEL 
               FROM BPO_A06_SAIDA_HIDRO_NEWAVE
               WHERE
                A06_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                A01_TP_CASO = ", tipoCaso, " AND
                A01_NR_CASO = ", numeroCaso, " AND
                A01_CD_MODELO = ", codModelo)
  df.saidasHidro <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  sql <- paste0("SELECT A.A01_TP_CASO,
                  A.A01_NR_CASO,
                  A.A01_CD_MODELO,
                  A.A03_CD_USINA,
                  A04_NR_MAQUINAS,
                  A04_VL_POTENCIA,
                  A04_NR_CONJUNTO,
                  A.A04_VL_ALTURA_REFERENCIA,
                  CASE B.A03_TP_TURBINA WHEN 1 THEN 1.5 WHEN 3 THEN 1.5 ELSE 1.2 END VL_COEF_TURBINA
                 FROM
                  BPO_A04_MAQUINAS_UHE A,
                  BPO_A03_DADOS_UHE B
                 WHERE
                  A.A01_CD_MODELO = B.A01_CD_MODELO AND
                  A.A01_NR_CASO = B.A01_NR_CASO AND
                  A.A01_TP_CASO = B.A01_TP_CASO AND
                  A.A03_CD_USINA = B.A03_CD_USINA AND
                  A.A01_TP_CASO = ", tipoCaso, " AND
                  A.A01_NR_CASO = ", numeroCaso, " AND
                  A.A01_CD_MODELO = ", codModelo, ";")
  df.potMaquinas <- DBI::dbGetQuery(conexaoSQLite, sql)
  
  # filtra REEs com calculo tipo 4
  reeTipo4 <- ree %>% 
    dplyr::filter(A02_TP_CALC_POTENCIA == 4) %>% 
    dplyr::pull(A02_NR_REE)
  df.dadosUHETipo4 <- df.dadosUHE %>% 
    dplyr::filter(A02_NR_REE %in% reeTipo4)
  UHEtipo4 <- unique(df.dadosUHETipo4$A03_CD_USINA)
  df.dadosVigentesUHETipo4 <- df.dadosVigentesUHE %>% 
    dplyr::filter(A02_NR_REE %in% reeTipo4)
  
  # leitura dos dados para calculo do PDisp das UHEs que modulam por tabela
  df.tabelaModulacao <- readRDS("curvaModulacao.rds") %>% 
    dplyr::group_by(codREE, codUsina) %>% 
    dplyr::reframe(funcao = list(approxfun(vazao, potencia, method="linear")))
  
  lt.hidrogramaBM <- readRDS("hidrogramaBM.rds")
  
  df.hidrograma <- lt.hidrogramaBM[["hidrogramas"]] %>% 
    dplyr::filter(tipo == tipoCaso)
  
  # verifica se os REE definidos estao na tabela
  if (length(dplyr::setdiff(reeTipo4, unique(df.tabelaModulacao$codREE))) != 0) {
    DBI::dbDisconnect(conexaoSQLite)
    stop("REE escolhido para modula\u00E7\u00E3o por tabela n\u00E7o possui dados definidos na tabela")
  }
  
  #### METODOLOGIA DE MODULACAO ORIGINAL
  if(tipoModulacao == 1){
    
    # filtra REEs com calculo tipo 1
    reeTipo1 <- ree %>% 
      dplyr::filter(A02_TP_CALC_POTENCIA == 1) %>% 
      dplyr::pull(A02_NR_REE)
    if(length(reeTipo1) == 0){
      DBI::dbDisconnect(conexaoSQLite)
      stop("Nenhum REE definido com A02_TP_CALC_POTENCIA igual a 1 na tabela BPO_A02_REES")
    }
    df.dadosUHETipo1 <- df.dadosUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo1) %>% 
      dplyr::select(-A02_NR_REE)
    df.dadosVigentesUHETipo1 <- df.dadosVigentesUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo1)
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Realizando c\u00E1lculo")}
    
    ##### CALCULO DISPONIBILIDADE TIPO 1 ######
    df.saidasHidroTipo1 <- df.saidasHidro %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo1)
    
    df.dadosCalculadosUHE <- calculaDisponibilidadeTipo1(tipoCaso,
                                                         numeroCaso,
                                                         codModelo,
                                                         df.saidasHidroTipo1, 
                                                         df.dadosUHETipo1, 
                                                         df.dadosVigentesUHETipo1, 
                                                         df.dadosMaquinasUHE, 
                                                         df.potMaquinas, 
                                                         df.dadosCaso, 
                                                         lt.dadosTucurui, 
                                                         flagVert)
    
    # grava dados calculados na BPO_A08_DADOS_CALCULADOS_UHE
    if(flagUHE){
      if(execShiny){incProgress(0, detail = "Gravando Dados Calculados por UHE")}
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      DBI::dbWriteTable(conexaoSQLite, "BPO_A08_DADOS_CALCULADOS_UHE", df.dadosCalculadosUHE, append = T)
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    }
    
    # monta estrutuda de dados para a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
      dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
      dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                       A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = sum(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),
                       A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A08_VL_POTENCIA_MAXIMA_MODULADA),
                       A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
      dplyr::ungroup()
    
    # corrigindo nome das colunas para ficar igual a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    colnames(df.dadosCalculadosSsist)[5:6] <- c("A09_NR_MES", "A09_NR_SERIE")
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Realizando c\u00E1lculo")}
    
    ##### DISPONIBILIDADE TIPO 2 E 3 ######
    sql <- paste0("SELECT
                  A01_CD_MODELO,
                  A01_TP_CASO,
                  A01_NR_CASO,
                  A02_NR_REE,
                  A06_NR_MES AS A09_NR_MES,
                  A06_NR_SERIE AS A09_NR_SERIE,
                  0 AS A09_VL_GERACAO_HIDRO_MINIMA,
                  0 AS A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                  A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO + A06_VL_VERTIMENTO_TURBINAVEL AS A09_VL_DISPONIBILIDADE_MAXIMA_PONTA,
                  0 AS A09_VL_POTENCIA_MAXIMA
                 FROM BPO_A06_SAIDA_HIDRO_NEWAVE
                 WHERE
                  A02_NR_REE IN (SELECT A02_NR_REE FROM BPO_A02_REES
                                 WHERE A02_TP_CALC_POTENCIA IN (2, 3) AND A01_TP_CASO = ", tipoCaso, " AND
                                  A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo, ") AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
    df.dadosSsistNaoModulam <- DBI::dbGetQuery(conexaoSQLite, sql)
    
    ##### CALCULO DISPONIBILIDADE TIPO 4 PELA TABELA ######
    if(length(reeTipo4) > 0){
      
      df.saidasHidroTipo4 <- df.saidasHidro %>% 
        dplyr::filter(A02_NR_REE %in% reeTipo4)
      
      df.dadosUHEModulamTabelaUsina <- calculaDisponibilidadeTabela(tipoCaso,
                                                                    numeroCaso,
                                                                    codModelo,
                                                                    pastaCaso,
                                                                    UHEtipo4,
                                                                    df.saidasHidroTipo4, 
                                                                    lt.hidrogramaBM, 
                                                                    df.hidrograma, 
                                                                    df.tabelaModulacao,
                                                                    flagVert)
      
      df.dadosUHEModulamTabela <- df.dadosUHEModulamTabelaUsina %>% 
        dplyr::group_by(A02_NR_REE, A33_NR_SERIE, A33_NR_MES) %>% #dados por REE
        dplyr::reframe(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA),
                       A33_VL_POTENCIA_MAXIMA = sum(A33_VL_POTENCIA_MAXIMA)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = A33_VL_DISPONIBILIDADE_MAXIMA_PONTA,
                      A01_CD_MODELO = codModelo,
                      A01_TP_CASO = tipoCaso,
                      A01_NR_CASO = numeroCaso,
                      A09_NR_MES = A33_NR_MES,
                      A09_NR_SERIE = A33_NR_SERIE,
                      A09_VL_GERACAO_HIDRO_MINIMA = 0,
                      A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = 0,
                      A09_VL_POTENCIA_MAXIMA = A33_VL_POTENCIA_MAXIMA) %>% 
        dplyr::select(A01_CD_MODELO, 
                      A01_TP_CASO, 
                      A01_NR_CASO, 
                      A02_NR_REE, 
                      A09_NR_MES,
                      A09_NR_SERIE, 
                      A09_VL_GERACAO_HIDRO_MINIMA, 
                      A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                      A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, 
                      A09_VL_POTENCIA_MAXIMA)
      
      # grava dados calculados na BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA
      if(flagUHE){
        if(execShiny){incProgress(0, detail = "Gravando Dados Calculados por UHE")}
        DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
        DBI::dbWriteTable(conexaoSQLite, "BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA", df.dadosUHEModulamTabelaUsina, append = T)
        DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      }
      
      # concatena as REEs que modulam com as que nao modulam e as que modulam por tabela para gravar na base
      df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist, df.dadosSsistNaoModulam, df.dadosUHEModulamTabela)
    }else{
      df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist, df.dadosSsistNaoModulam)
    }
    
    # Para buscar o subsistema
    df.dadosCalculadosSsist <- dplyr::inner_join(df.dadosCalculadosSsist, df.ree, by = c("A02_NR_REE"))
    
    # Nao precisa mais de REE e Descricao
    df.dadosCalculadosSsist <- df.dadosCalculadosSsist %>%
      dplyr::select(-A02_NR_REE, -A02_TX_DESCRICAO_REE)
    
    # Agrupa os dados calculados por Subsistema, totalizando
    df.dadosCalculadosSsistAgrup <- df.dadosCalculadosSsist %>%
      dplyr::group_by(A01_CD_MODELO,A01_NR_CASO,A01_TP_CASO,A02_NR_SUBSISTEMA,A09_NR_MES,A09_NR_SERIE) %>%
      dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(A09_VL_GERACAO_HIDRO_MINIMA),4),
                       A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),4),
                       A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA),4),
                       A09_VL_POTENCIA_MAXIMA = round(sum(A09_VL_POTENCIA_MAXIMA),4)) %>% 
      dplyr::ungroup()
    
    # remove data frame ja utilizado
    rm(df.dadosCalculadosSsist)
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Gravando Disponibilidade Hidro por Subsistema")}
    
    # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
    DBI::dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSsistAgrup, append = T)
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    
    return("Disponibilidade hidro processada com sucesso!")
  }else{
    #### METODOLOGIA DE MODULACAO POR UHE
    
    # leitura da planilha de dados de modulacao por UHE
    planilhaModulacao <- list.files(path = pastaCaso, pattern = "^dadosModulacaoUHE")
    if (length(planilhaModulacao) != 1) {
      DBI::dbDisconnect(conexaoSQLite)
      stop("Planilha de com dados de modula\u00E7\u00E3o por UHE n\u00E3o encontrada ou multiplos arquivos com nome dadosModulacaoUHE em ", pastaCaso)
    }else{
      df.dadosModulacaoUHE <- readxl::read_xlsx(paste(pastaCaso, planilhaModulacao, sep = "/")) %>% 
        dplyr::select(-UHE) %>% 
        tidyr::pivot_longer(!cod, names_to = "mes", values_to = "tipo") %>%
        dplyr::mutate(mes = as.numeric(mes))
      # verifica se os dados de tipo contem apenas os valores 1 e 2
      if(dplyr::setequal(unique(df.dadosModulacaoUHE$tipo), c(1,2)) == FALSE){
        DBI::dbDisconnect(conexaoSQLite)
        stop("Planilha de com dados de modula\u00E7\u00E3o por UHE ", planilhaModulacao, " cont\u00E9m dados para o tipo de modula\u00E7\u00E3o diferentes de 1 e 2.")
      }
    }
    
    # filtra REEs com calculo tipo 5
    reeTipo5 <- ree %>% 
      dplyr::filter(A02_TP_CALC_POTENCIA == 5) %>% 
      dplyr::pull(A02_NR_REE)
    if(length(reeTipo5) == 0){
      DBI::dbDisconnect(conexaoSQLite)
      stop("Nenhum REE definido com A02_TP_CALC_POTENCIA igual a 5 na tabela BPO_A02_REES")
    }
    df.dadosUHETipo5 <- df.dadosUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo5) %>% 
      dplyr::select(-A02_NR_REE)
    df.dadosVigentesUHETipo5 <- df.dadosVigentesUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo5)
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Realizando c\u00E1lculo")}
    
    ##### CALCULO DISPONIBILIDADE TIPO 5 ######
    df.saidasHidroTipo5 <- df.saidasHidro %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo5)
    
    df.dadosCalculadosUHE <- calculaDisponibilidadeTipo1(tipoCaso,
                                                         numeroCaso,
                                                         codModelo,
                                                         df.saidasHidroTipo5, 
                                                         df.dadosUHETipo5, 
                                                         df.dadosVigentesUHETipo5, 
                                                         df.dadosMaquinasUHE, 
                                                         df.potMaquinas, 
                                                         df.dadosCaso, 
                                                         lt.dadosTucurui, 
                                                         flagVert) %>% 
      dplyr::mutate(mes = A08_NR_MES%%100) %>% 
      dplyr::left_join(df.dadosModulacaoUHE, by = c("A03_CD_USINA" = "cod", "mes")) %>%
      dplyr::mutate(tipo = ifelse(is.na(tipo), 1, tipo),
                    A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse(tipo==1, A08_VL_GERACAO_HIDRO_MEDIA, A08_VL_POTENCIA_MAXIMA_MODULADA)) %>% 
      dplyr::select(-mes, -tipo)
    
    # grava dados calculados na BPO_A08_DADOS_CALCULADOS_UHE
    if(flagUHE){
      if(execShiny){incProgress(0, detail = "Gravando Dados Calculados por UHE")}
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      DBI::dbWriteTable(conexaoSQLite, "BPO_A08_DADOS_CALCULADOS_UHE", df.dadosCalculadosUHE, append = T)
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    }
    
    # monta estrutuda de dados para a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
      dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
      dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                       A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = sum(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),
                       A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A08_VL_POTENCIA_MAXIMA_MODULADA),
                       A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
      dplyr::ungroup()
    
    # corrigindo nome das colunas para ficar igual a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    colnames(df.dadosCalculadosSsist)[5:6] <- c("A09_NR_MES", "A09_NR_SERIE")
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Realizando c\u00E1lculo")}
    
    ##### CALCULO DISPONIBILIDADE TIPO 4 PELA TABELA ######
    if(length(reeTipo4) > 0){
      
      df.saidasHidroTipo4 <- df.saidasHidro %>% 
        dplyr::filter(A02_NR_REE %in% reeTipo4)
      
      df.dadosUHEModulamTabelaUsina <- calculaDisponibilidadeTabela(tipoCaso,
                                                                    numeroCaso,
                                                                    codModelo,
                                                                    pastaCaso,
                                                                    UHEtipo4,
                                                                    df.saidasHidroTipo4, 
                                                                    lt.hidrogramaBM, 
                                                                    df.hidrograma, 
                                                                    df.tabelaModulacao,
                                                                    flagVert)
      
      df.dadosUHEModulamTabela <- df.dadosUHEModulamTabelaUsina %>% 
        dplyr::group_by(A02_NR_REE, A33_NR_SERIE, A33_NR_MES) %>% #dados por REE
        dplyr::reframe(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA),
                       A33_VL_POTENCIA_MAXIMA = sum(A33_VL_POTENCIA_MAXIMA)) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = A33_VL_DISPONIBILIDADE_MAXIMA_PONTA,
                      A01_CD_MODELO = codModelo,
                      A01_TP_CASO = tipoCaso,
                      A01_NR_CASO = numeroCaso,
                      A09_NR_MES = A33_NR_MES,
                      A09_NR_SERIE = A33_NR_SERIE,
                      A09_VL_GERACAO_HIDRO_MINIMA = 0,
                      A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = 0,
                      A09_VL_POTENCIA_MAXIMA = A33_VL_POTENCIA_MAXIMA) %>% 
        dplyr::select(A01_CD_MODELO, 
                      A01_TP_CASO, 
                      A01_NR_CASO, 
                      A02_NR_REE, 
                      A09_NR_MES,
                      A09_NR_SERIE, 
                      A09_VL_GERACAO_HIDRO_MINIMA, 
                      A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                      A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, 
                      A09_VL_POTENCIA_MAXIMA)
      
      # grava dados calculados na BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA
      if(flagUHE){
        if(execShiny){incProgress(0, detail = "Gravando Dados Calculados por UHE")}
        DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
        DBI::dbWriteTable(conexaoSQLite, "BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA", df.dadosUHEModulamTabelaUsina, append = T)
        DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      }
      
      # concatena as REEs que modulam com as que nao modulam e as que modulam por tabela para gravar na base
      df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist, df.dadosUHEModulamTabela)
    }else{
      df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist)
    }
    
    # Para buscar o subsistema
    df.dadosCalculadosSsist <- dplyr::inner_join(df.dadosCalculadosSsist, df.ree, by = c("A02_NR_REE"))
    
    # Nao precisa mais de REE e Descricao
    df.dadosCalculadosSsist <- df.dadosCalculadosSsist %>%
      dplyr::select(-A02_NR_REE, -A02_TX_DESCRICAO_REE)
    
    # Agrupa os dados calculados por Subsistema, totalizando
    df.dadosCalculadosSsistAgrup <- df.dadosCalculadosSsist %>%
      dplyr::group_by(A01_CD_MODELO,A01_NR_CASO,A01_TP_CASO,A02_NR_SUBSISTEMA,A09_NR_MES,A09_NR_SERIE) %>%
      dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(A09_VL_GERACAO_HIDRO_MINIMA),4),
                       A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),4),
                       A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA),4),
                       A09_VL_POTENCIA_MAXIMA = round(sum(A09_VL_POTENCIA_MAXIMA),4)) %>% 
      dplyr::ungroup()
    
    # remove data frame ja utilizado
    rm(df.dadosCalculadosSsist)
    
    # barra de progresso
    if(execShiny){incProgress(8/100, detail = "Gravando Disponibilidade Hidro por Subsistema")}
    
    # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
    DBI::dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSsistAgrup, append = T)
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    
    return("Disponibilidade hidro processada com sucesso!")
  }
}
