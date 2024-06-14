#' Calcula a disponibilidade hidraulica para o Balanco de Potencia
#'
#' Faz os calculos da disponibilidade hidraulica atualizando as tabelas BPO_A06_SAIDA_HIDRO_NEWAVE, BPO_A08_DADOS_CALCULADOS_UHE e 
#' BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param pastaCaso localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta
#' @param pastaSaidas localizacao dos arquivos de saida do modulo NWLISTOP e arquivos do SUISHI
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param codTucurui codigo atribuido para a usina de Tucurui
#' @param flagVert booleano que indica se considera ou nao o vertimento para todas as UHE
#' @param execShiny booleano que indica se a função está sendo executada em um contexto reativo, para atualização da barra de progresso
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export
calculaDisponibilidadeHidro <- function(baseSQLite, pastaCaso, pastaSaidas, tipoCaso, numeroCaso, codModelo, codTucurui, flagVert, execShiny = FALSE) {
  # SQLite
  conexaoSQLite <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
  # fecha conexao com a base SQLite na saida da funcao, seja por erro ou normalmente
  on.exit(DBI::dbDisconnect(conexaoSQLite))
  
  # pega dados da tabela de dados do caso
  sql <- paste0("SELECT A01_NR_MES_INICIO as dataInicioCaso,
                  A01_NR_MES_FIM as dataFimCaso,
                  A01_NR_HORAS_PONTA as horasPonta,
                  A01_NR_GERACAO_LIMITE_TUCURUI as gerLimiteTucurui,
                  A01_NR_COTA_LIMITE_TUCURUI as cotaLimiteTucurui,
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
  
  # faz calculos com base em dados do SUISHI (codModelo = 2) ou do NEWAVE (codModelo = 1)
  if (codModelo == 2) {
    # barra de progresso
    if(execShiny){incProgress(0.4, detail = "Disponibilidade Hidro pelo SUISHI")}
    
    # descarta usinas com STATUS = 'NC' (fora do deck)
    sql <- paste0("SELECT 
                    A03_CD_USINA,
                    A03_VL_PRODUTIBILIDADE
                  FROM BPO_A03_DADOS_UHE
                  WHERE
                    A03_TX_STATUS <> 'NC' AND
                    A01_TP_CASO = ", tipoCaso, " AND
                    A01_NR_CASO = ", numeroCaso, " AND
                    A01_CD_MODELO = ", codModelo)
    df.dadosUHE <- DBI::dbGetQuery(conexaoSQLite, sql)
    
    sql <- paste0("SELECT A03_CD_USINA, A05_NR_MES,              
                    A05_VL_VAZAO_MINIMA, A05_VL_TEIF, A05_VL_IP, A05_VL_POTENCIA
                  FROM BPO_A05_DADOS_VIGENTES_UHE
                  WHERE
                    A05_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                    A01_TP_CASO = ", tipoCaso, " AND
                    A01_NR_CASO = ", numeroCaso, " AND
                    A01_CD_MODELO = ", codModelo)
    df.dadosVigentesUHE <- DBI::dbGetQuery(conexaoSQLite, sql)
    
    # dados do SUISHI
    df.operacaoHidroSUISHI <- leitorrmpe::leituraOperacaoHidroSUISHI(pastaSaidas) %>% 
      dplyr::filter(patamar == 1) %>% 
      dplyr::select(codUsina, anoMes, serie, codREE, alturaLiquida = QUED, disponibilidadePonta = PDISP_QMIN)  
    
    # critica falta de dados
    dadosNA <- df.operacaoHidroSUISHI %>% dplyr::filter(is.na(alturaLiquida) | is.na(disponibilidadePonta)) %>% nrow()
    if(dadosNA > 0) {
      usinasComNA <- df.operacaoHidroSUISHI %>% dplyr::filter(is.na(alturaLiquida) | is.na(disponibilidadePonta)) %>% dplyr::pull(codUsina) %>% 
        unique() %>% paste(collapse = ", ")
      stop(paste0("Usinas ", usinasComNA, " sem dados de QUED ou PDISP_QMIN"))
    } 
    
    # junta dados do SUISHI com dados de cadastro   
    df.operacaoHidroSUISHI <- dplyr::left_join(df.operacaoHidroSUISHI, df.dadosUHE, by = c("codUsina" = "A03_CD_USINA")) %>% 
      dplyr::left_join(df.dadosVigentesUHE, by = c("codUsina" = "A03_CD_USINA", "anoMes" = "A05_NR_MES"))
    
    # recupera dados do hidr para casos que nao estao dos dados vigentes (por exemplo, usinas 183 e 184)
    df.dadosHidroComplementares <- leitorrmpe::leituraDadosUsinasHidro(pastaCaso) %>% 
      magrittr::use_series("df.dadosUsinasHidroeletricas") %>% 
      dplyr::select(codUsina, vazaoMinimaHistorico, TEIF, IP)
    
    # substitui os dados vazios pelos dados do hidr. para a potencia, por simplificacao, usa o mesmo valor da disponivel caso nao tenha o dado
    df.operacaoHidroSUISHI <- dplyr::inner_join(df.operacaoHidroSUISHI, df.dadosHidroComplementares, by = "codUsina") %>% 
      dplyr::mutate(A05_VL_VAZAO_MINIMA = ifelse(is.na(A05_VL_VAZAO_MINIMA),
                                                 as.double(vazaoMinimaHistorico),
                                                 A05_VL_VAZAO_MINIMA),
                    A05_VL_TEIF = ifelse(is.na(A05_VL_TEIF),
                                         TEIF/100,
                                         A05_VL_TEIF),
                    A05_VL_IP = ifelse(is.na(A05_VL_IP),
                                       IP/100,
                                       A05_VL_IP),
                    A05_VL_POTENCIA = ifelse(is.na(A05_VL_POTENCIA),
                                             disponibilidadePonta,
                                             A05_VL_POTENCIA)) %>% 
      # caso sobre dados com NA, altera para zero para nao atrapalhar nas contas
      dplyr::mutate(A05_VL_VAZAO_MINIMA = ifelse(is.na(A05_VL_VAZAO_MINIMA),
                                                 0,
                                                 A05_VL_VAZAO_MINIMA),
                    A05_VL_TEIF = ifelse(is.na(A05_VL_TEIF),
                                         0,
                                         A05_VL_TEIF),
                    A05_VL_IP = ifelse(is.na(A05_VL_IP),
                                       0,
                                       A05_VL_IP),
                    A05_VL_POTENCIA = ifelse(is.na(A05_VL_POTENCIA),
                                             0,
                                             A05_VL_POTENCIA),
                    A03_VL_PRODUTIBILIDADE = ifelse(is.na(A03_VL_PRODUTIBILIDADE),
                                                    0,
                                                    A03_VL_PRODUTIBILIDADE))
    
    # calcula geracao minima e desconta taxas de falha e indisponibilidade
    df.operacaoHidroSUISHI <- df.operacaoHidroSUISHI %>% dplyr::mutate(geracaoMinima = alturaLiquida * A05_VL_VAZAO_MINIMA * A03_VL_PRODUTIBILIDADE,
                                                                       disponibilidadePonta = disponibilidadePonta * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                                       potenciaMaxima = A05_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                                       # garante geracao minima igual a disponibilidade pois tem casos que nao respeitam o minimo
                                                                       # e isso causaria inviabilidade na resolucao do PL
                                                                       geracaoMinima = ifelse(geracaoMinima > disponibilidadePonta,
                                                                                              disponibilidadePonta,
                                                                                              geracaoMinima))
    
    
    df.operacaoHidroSUISHI <- dplyr::inner_join(df.operacaoHidroSUISHI, df.ree, by = c("codREE" = "A02_NR_REE"))
    
    # monta estrutuda de dados para a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    df.dadosCalculadosSistema <- df.operacaoHidroSUISHI %>%
      dplyr::group_by(A02_NR_SUBSISTEMA, anoMes, serie) %>%
      dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(geracaoMinima), 4),
                       A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(geracaoMinima), 4),
                       A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(disponibilidadePonta), 4),
                       A09_VL_POTENCIA_MAXIMA = round(sum(potenciaMaxima), 4), .groups = "drop") %>% 
      dplyr::mutate(A01_CD_MODELO = codModelo,
                    A01_NR_CASO = numeroCaso,
                    A01_TP_CASO = tipoCaso,
                    A09_NR_MES = anoMes,
                    A09_NR_SERIE = serie) %>% 
      dplyr::select(A01_CD_MODELO, A01_NR_CASO, A01_TP_CASO, A02_NR_SUBSISTEMA, A09_NR_MES, A09_NR_SERIE, 
                    A09_VL_GERACAO_HIDRO_MINIMA, A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL, A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, A09_VL_POTENCIA_MAXIMA)
    
    # trava conexao com o banco para melhorar desempenho e evitar outros usos da base simultaneos
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
    # limpa BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA de outras execucoes para o mesmo caso
    sql <- paste0("DELETE FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
    DBI::dbExecute(conexaoSQLite, sql)
    
    # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    DBI::dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSistema, append = T)
    DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    
  } else {
    
    # barra de progresso
    if(execShiny){incProgress(4/100, detail = "Excluindo outras execuções de BP para o mesmo caso")}
    
    ##### EXCLUSÃO DE DADOS DE EXECUÇÕES ANTERIORES #####
    
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
    
    # barra de progresso
    if(execShiny){incProgress(3/100, detail = "Atualização de Submotorização")}
    
    ##### SUBMOTORIZAÇÃO #####
    
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
    if(execShiny){incProgress(3/100, detail = "Leitura de dados")}
    
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
    df.potMaquinas<- DBI::dbGetQuery(conexaoSQLite, sql)
    
    # filtra REEs com calculo tipo 1
    reeTipo1 <- ree %>% 
      dplyr::filter(A02_TP_CALC_POTENCIA == 1) %>% 
      dplyr::pull(A02_NR_REE)
    df.dadosUHETipo1 <- df.dadosUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo1) %>% 
      dplyr::select(-A02_NR_REE)
    df.dadosVigentesUHETipo1 <- df.dadosVigentesUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo1)
    
    # filtra REEs com calculo tipo 4
    reeTipo4 <- ree %>% 
      dplyr::filter(A02_TP_CALC_POTENCIA == 4) %>% 
      dplyr::pull(A02_NR_REE)
    df.dadosUHETipo4 <- df.dadosUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo4)
    UHEtipo4 <- unique(df.dadosUHETipo4$A03_CD_USINA)
    df.dadosVigentesUHETipo4 <- df.dadosVigentesUHE %>% 
      dplyr::filter(A02_NR_REE %in% reeTipo4)
    
    df.tabelaModulacao <- readRDS("curvaModulacao.rds") %>% 
      dplyr::group_by(codREE, codUsina) %>% 
      dplyr::reframe(funcao = list(approxfun(vazao, potencia, method="linear")))
    
    lt.hidrogramaBM <- readRDS("hidrogramaBM.rds")
    
    df.hidrograma <- lt.hidrogramaBM[["hidrogramas"]] %>% 
      dplyr::filter(tipo == tipoCaso)
    
    # verifica se os REE definidos estao na tabela
    if (length(dplyr::setdiff(reeTipo4, unique(df.tabelaModulacao$codREE))) != 0) {
      DBI::dbDisconnect(conexao)
      stop("REE escolhido para modulação por tabela não possui dados definidos na tabela")
    }
    
    # for criado para resolver problema de alocacao de memoria. orginalmente era um data frame unico com todos os dados, contudo,
    # trabalalhando com series sinteticas, acabava por exaurir a memoria.
    tamanhoJanela <- 100
    janelaSeries <- c(seq(1, df.dadosCaso$numeroSeries, tamanhoJanela), (df.dadosCaso$numeroSeries + 1))
    quantidadeJanela <- length(janelaSeries)
    df.dadosCalculadosUHETipo4 <- NULL #tabela nova a ser criada com dados por usinas dos ree que modulam por tabela
    for (andaJanela in 1:(quantidadeJanela - 1)) {
      
      # barra de progresso
      if(execShiny){incProgress((1/(quantidadeJanela - 1))*0.3, detail = paste(round(andaJanela*100/(quantidadeJanela - 1), 0),"%"))}
      
      ##### CÁLCULO DISPONIBILIDADE TIPO 1 ######
      df.saidasHidroTipo1 <- df.saidasHidro %>% 
        dplyr::filter(dplyr::between(A06_NR_SERIE, janelaSeries[andaJanela], (janelaSeries[andaJanela + 1] - 1)),
                      A02_NR_REE %in% reeTipo1)
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosVigentesUHETipo1, 
                                                 df.saidasHidroTipo1, 
                                                 by = c("A02_NR_REE", "A05_NR_MES" = "A06_NR_MES")) %>% 
        dplyr::mutate(A08_VL_VOLUME_OPERATIVO = (A05_VL_VOL_MAX - A05_VL_VOL_MIN) * A06_VL_PERC_ARMAZENAMENTO + A05_VL_VOL_MIN,
                      colunaFlagVert = flagVert,
                      # verifica o flag de vertimento, se verdadeiro soma o vertimento na variavel de GH
                      A06_VL_GERACAO_HIDRAULICA = ifelse(colunaFlagVert,
                                                         A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO + A06_VL_VERTIMENTO_TURBINAVEL,
                                                         A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO)
        ) %>% 
        dplyr::select(-colunaFlagVert)
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosMaquinasUHE, by = "A03_CD_USINA") %>% 
        dplyr::mutate(VL_POT_EXP = round(A05_VL_POTENCIA - POT_TOTAL, 2))
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosUHETipo1, by = "A03_CD_USINA") %>% 
        dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
        dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES = A05_NR_MES, A08_NR_SERIE = A06_NR_SERIE, A02_NR_REE, 
                      A08_VL_VOLUME_OPERATIVO, A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4, 
                      A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE, A05_NR_CANAL_FUGA_MEDIO, A05_VL_TEIF, A05_VL_IP, 
                      A05_VL_VAZAO_MINIMA, A06_VL_GERACAO_HIDRAULICA, VL_POT_EXP)
      
      # 2 - COTA OPERATIVA(per,ser) => CALCULADA a partir do VOLUME OPERATIVO(per,ser) e do polinomio cota-volume
      # 3 - ALTURA DE QUEDA LIQUIDA(per,ser) => CALCULADA a partir da COTA OPERATIVA(per,ser), CANAL DE FUGA MEDIO (per) e Perdas
      # 3.1 - Altura de queda liquida = Cota Operativa - Canal de Fuga medio
      # 3.2 - Abate as perdas
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_COTA_OPERATIVA = A03_NR_PCV_0  + (A03_NR_PCV_1 * A08_VL_VOLUME_OPERATIVO) +
                        (A03_NR_PCV_2 * A08_VL_VOLUME_OPERATIVO^2) +
                        (A03_NR_PCV_3 * A08_VL_VOLUME_OPERATIVO^3) +
                        (A03_NR_PCV_4 * A08_VL_VOLUME_OPERATIVO^4),
                      A08_VL_ALTURA_LIQUIDA = ifelse(A03_TP_PERDA == 1,
                                                     (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO) * (1 - (A03_VL_PERDA / 100)),
                                                     (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO)- A03_VL_PERDA))
      
      
      # limpa campos ja usados
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::select(-A03_NR_PCV_0, -A03_NR_PCV_1, -A03_NR_PCV_2, -A03_NR_PCV_3,
                      -A03_NR_PCV_4, -A03_VL_PERDA, -A05_NR_CANAL_FUGA_MEDIO)
      
      # 3.3 - Ajuste caso encontre algum valor negativo
      df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA[df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA < 0.0001] <- 0.0001
      
      # 4 - POTENCIA MaXIMA(per,ser)
      # 4.1) PARA POTENCIA REFERENTE AOS CONJUNTOS Ja EXISTENTES: ALTURA DE REFERENCIA DO CONJUNTO >= ALTURA DE QUEDA
      df.dadosCalculadosUHEMaquinas <- dplyr::inner_join(df.dadosCalculadosUHE, df.potMaquinas,
                                                         by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA"))
      
      # Para os conjuntos onde a altura de queda liquida e menor que a altura de referencia do conjunto, a potencia
      # nao e igual a maxima, mas um percentual da maxima, calculado a partir da razao (HLIQ/HREF)^coef da turbina
      df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
        dplyr::mutate(A08_VL_POTENCIA_MAXIMA = ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                      A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                      A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                        (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA))
      
      # 4.3) PARA POTENCIA EM EXPANSaO e ALTURA DE REFERENCIA DO CONJUNTO 1 >= ALTURA DE QUEDA LiQUIDA(per,ser): APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP
      # 4.4) PARA POTENCIA EM EXPANSAO e ALTURA DE REFERENCIA DO CONJUNTO 1 < ALTURA DE QUEDA LiQUIDA(per,ser):
      # APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP E APLICA AINDA
      # UM FATOR DE REDUcaO DA POTENCIA IGUAL A (ALTURA DE QUEDA LiQUIDA(per,ser)/ALTURA DE REFERENCIA DO CONJUNTO 1)^COEFICIENTE DA TURBINA
      df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
        dplyr::mutate(A08_VL_POTENCIA_MAXIMA =
                        (A08_VL_POTENCIA_MAXIMA + ifelse((A04_NR_CONJUNTO == 1 & VL_POT_EXP > 0) ,
                                                         ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                                VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                                VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                                  (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA ),
                                                         0)))
      
      df.potenciaMaximaUHE <- df.dadosCalculadosUHEMaquinas %>%
        dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A03_CD_USINA, A08_NR_MES, A08_NR_SERIE) %>%
        dplyr::summarise(A08_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>% dplyr::ungroup()
      
      # remove data frame ja utilizado
      rm(df.dadosCalculadosUHEMaquinas)
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.potenciaMaximaUHE,
                                                 by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA", "A08_NR_MES", "A08_NR_SERIE"))
      
      # remove data frame ja utilizado
      rm(df.potenciaMaximaUHE)
      
      ###################################################################################################################################
      # Segundo Passo: Modula a geracao na ponta, respeitando a geracao media (GHTOT(per,res)) durante as horas do mes,
      # o GHMIN(per,ser) fora na hora da ponta e a maxima contribuicao
      # POTENCIA MAXIMA(per,ser) durante o numero de horas em que a ponta ocorre
      ###################################################################################################################################
      # GHMIN CALCULADO ORIGINALMENTE
      
      # Atualiza GHMIN se potencia maxima for inferior ao GHMIN
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL =
                        A08_VL_ALTURA_LIQUIDA * A03_VL_PRODUTIBILIDADE * A05_VL_VAZAO_MINIMA,
                      A08_VL_GERACAO_HIDRO_MINIMA = ifelse(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL > A08_VL_POTENCIA_MAXIMA,
                                                           A08_VL_POTENCIA_MAXIMA,
                                                           A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL))
      
      
      df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
        dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
        dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                         A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
        dplyr::ungroup()
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                                 by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
      
      
      # Calcula GHTOT por usina:
      # Rateia o GHTOT pela potencia maxima, ja abatida do GHMIN*/
      # GHMIN_UHE = GHMIN_UHE X (1 - (GHMIN_SSIS - GHTOT_SSIS)/GHMIN_SSIS)
      # GHMEDIA = GHMIN_UHE + ((GHTOT_SSIS - GHMIN_SSIS) X ((POT_MAX_UHE - GHMIN_UHE)/(POT_MAX_SSIS - GHMIN_SSIS)))
      # Modula o GHTOT, maximizando a geracao na horas de ponta*/
      # POT_MODULADA = ((GHMEDIA X 730.5) - (GHMIN X (730.5 - HORASPONTA))) / HORASPONTA
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_GERACAO_HIDRO_MINIMA = ifelse((A06_VL_GERACAO_HIDRAULICA < A09_VL_GERACAO_HIDRO_MINIMA_TMP),
                                                           (A08_VL_GERACAO_HIDRO_MINIMA * 
                                                              (1 - (A09_VL_GERACAO_HIDRO_MINIMA_TMP - A06_VL_GERACAO_HIDRAULICA) / A09_VL_GERACAO_HIDRO_MINIMA_TMP)),
                                                           A08_VL_GERACAO_HIDRO_MINIMA))
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>% dplyr::select(-A09_VL_GERACAO_HIDRO_MINIMA_TMP)
      
      df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
        dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
        dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA)) %>% dplyr::ungroup()
      
      df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                                 by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_GERACAO_HIDRO_MEDIA = (A08_VL_GERACAO_HIDRO_MINIMA + ((A06_VL_GERACAO_HIDRAULICA - A09_VL_GERACAO_HIDRO_MINIMA_TMP) *
                                                                                     ((A08_VL_POTENCIA_MAXIMA - A08_VL_GERACAO_HIDRO_MINIMA) / 
                                                                                        (A09_VL_POTENCIA_MAXIMA - A09_VL_GERACAO_HIDRO_MINIMA_TMP)))),
                      A08_VL_POTENCIA_MAXIMA_MODULADA = (((A08_VL_GERACAO_HIDRO_MEDIA * 730.5) -
                                                            (A08_VL_GERACAO_HIDRO_MINIMA * (730.5 - df.dadosCaso$horasPonta))) / df.dadosCaso$horasPonta))
      
      # Ajusta potencia maxima de acordo com o limite superior
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse(A08_VL_POTENCIA_MAXIMA_MODULADA > A08_VL_POTENCIA_MAXIMA,
                                                               A08_VL_POTENCIA_MAXIMA,
                                                               A08_VL_POTENCIA_MAXIMA_MODULADA),
                      A08_VL_VAZAO_MAXIMA = 0,
                      A08_VL_VAZAO_MAXIMA_MODULADA = 0,
                      A08_VL_ALTURA_MODULADA = 0)
      
      # Ajusta potencia maxima de tucurui
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse((A03_CD_USINA == codTucurui &
                                                                  A08_VL_POTENCIA_MAXIMA_MODULADA > df.dadosCaso$gerLimiteTucurui &
                                                                  A08_VL_COTA_OPERATIVA < df.dadosCaso$cotaLimiteTucurui),
                                                               df.dadosCaso$gerLimiteTucurui,
                                                               A08_VL_POTENCIA_MAXIMA_MODULADA))
      
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES,
                      A08_NR_SERIE, A02_NR_REE, A08_VL_VOLUME_OPERATIVO,
                      A08_VL_COTA_OPERATIVA, A08_VL_ALTURA_LIQUIDA, A08_VL_VAZAO_MAXIMA,
                      A08_VL_POTENCIA_MAXIMA, A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                      A08_VL_GERACAO_HIDRO_MINIMA, A08_VL_GERACAO_HIDRO_MEDIA,
                      A08_VL_VAZAO_MAXIMA_MODULADA, A08_VL_ALTURA_MODULADA,
                      A08_VL_POTENCIA_MAXIMA_MODULADA)
      
      
      # grava dados calculados na BPO_A08_DADOS_CALCULADOS_UHE
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      DBI::dbWriteTable(conexaoSQLite, "BPO_A08_DADOS_CALCULADOS_UHE", df.dadosCalculadosUHE, append = T)
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      
      
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
                  A06_NR_SERIE BETWEEN ", janelaSeries[andaJanela], " AND ", (janelaSeries[andaJanela + 1] - 1), " AND                
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
      df.dadosSsistNaoModulam <- DBI::dbGetQuery(conexaoSQLite, sql)
      
      ##### CÁLCULO DISPONIBILIDADE TIPO 4 PELA TABELA ######
      browser()
      if(length(reeTipo4) > 0){
        df.saidasHidroTipo4 <- df.saidasHidro %>% 
          dplyr::filter(dplyr::between(A06_NR_SERIE, janelaSeries[andaJanela], (janelaSeries[andaJanela + 1] - 1)), A02_NR_REE %in% reeTipo4)
        
        # calculo da pordutibilidade das usinas dos REE tipo 4
        df.prodREEModulaTabela <- dplyr::full_join(leitorrmpe::leituraAlteracaoDadosUsinasHidro(pastaCaso)[[1]],
                                                   leitorrmpe::leituraDadosUsinasHidro(pastaCaso)[[1]] %>%
                                                     tidyr::crossing(anoMes = unique(df.saidasHidroTipo4$A06_NR_MES)) %>% 
                                                     dplyr::select(codUsina, anoMes, poliCotaVolumeA0, poliCotaVolumeA1, poliCotaVolumeA2, poliCotaVolumeA3, poliCotaVolumeA4, volumeMaximo, volumeReferencia, canalFugaMedio, tipoPerda, perda, tipoTurbina, TEIF, IP, produtibilidade) %>%
                                                     dplyr::mutate(volumeReferencia = ifelse(volumeReferencia < volumeMaximo, volumeMaximo, volumeReferencia)) %>%
                                                     dplyr::select(-volumeMaximo) %>%  
                                                     dplyr::mutate(kturb = ifelse(tipoTurbina == 2, 1.2, 1.5)),
                                                   by=c("codUsina" ,"anoMes")) %>% 
          dplyr::left_join(leitorrmpe::leituraConfiguracaoHidro(pastaCaso) %>% dplyr::select(codREE,codUsina), by=c("codUsina")) %>% 
          dplyr::mutate(volumeMaximo = ifelse(is.na(volumeMaximo) | volumeMaximo >  volumeReferencia ,volumeReferencia,volumeMaximo)) %>%
          dplyr::mutate(nivelMontante = ifelse(is.na(nivelMontante),
                                               poliCotaVolumeA0 + volumeMaximo*poliCotaVolumeA1 + volumeMaximo^2*poliCotaVolumeA2 + volumeMaximo^3*poliCotaVolumeA3+ volumeMaximo^4*poliCotaVolumeA4,
                                               nivelMontante)) %>%
          dplyr::mutate(canalFuga = ifelse(is.na(canalFuga), canalFugaMedio, canalFuga)) %>%
          dplyr::mutate(perda = ifelse(tipoPerda==2, perda, (nivelMontante - canalFuga)*perda/100)) %>%
          dplyr::mutate(Hliq = nivelMontante-canalFuga - perda) %>% 
          dplyr::left_join(leitorrmpe::leituraDadosUsinasHidro(pastaCaso)[[3]], by = c("codUsina"), relationship = "many-to-many") %>%
          dplyr::mutate(potConj = numeroMaquinas * ifelse(Hliq>=quedaEfetiva,potenciaUnitaria,potenciaUnitaria*(Hliq/quedaEfetiva)^kturb),
                        produtibilidade = produtibilidade * (nivelMontante-canalFuga-perda)) %>%
          dplyr::filter(codUsina %in% UHEtipo4,anoMes %in% unique(df.saidasHidroTipo4$A06_NR_MES)) %>%
          dplyr::group_by(codREE, codUsina, anoMes, produtibilidade, TEIF, IP) %>%
          dplyr::summarize(GHmax = sum(potConj)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(GHmax=GHmax * ( 1-TEIF/100) * (1-IP/100)) %>%
          dplyr::select(codREE, codUsina, anoMes, GHmax, produtibilidade) %>%
          dplyr::group_by(codREE, anoMes) %>%
          dplyr::mutate(proporcao = GHmax/sum(GHmax))
        
        df.dadosUHEModulamTabela.usina <- dplyr::left_join(df.saidasHidroTipo4, df.prodREEModulaTabela, by = c("A02_NR_REE" = "codREE", "A06_NR_MES" = "anoMes")) %>% 
          dplyr::left_join(lt.hidrogramaBM[["usinas"]], by = c("codUsina")) %>% 
          dplyr::mutate(mes = A06_NR_MES%%100) %>% 
          dplyr::left_join(df.hidrograma, by = c("mes", "grupo", "codUsina" = "codUsinaHidrograma")) %>% 
          dplyr::select(-mes) %>% 
          dplyr::mutate(flag = flagVert,
                        ghtot_ree = ifelse(flag,
                                           A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO + A06_VL_VERTIMENTO_TURBINAVEL,
                                           A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO),
                        gh = ghtot_ree * proporcao,
                        flagHidrograma = ifelse(!is.na(grupo) & !is.na(vazao), 1, ifelse(!is.na(grupo) & is.na(vazao), 0, NA))) %>% 
          dplyr::left_join(df.tabelaModulacao %>% dplyr::select(-codUsina), by = c("A02_NR_REE" = "codREE")) %>%
          dplyr::group_by(A02_NR_REE, A06_NR_MES, A06_NR_SERIE, grupo) %>% 
          dplyr::mutate(proporcao_hidrograma = (GHmax  * flagHidrograma)/sum(GHmax  * flagHidrograma),
                        proporcao_impactada = (GHmax  * (1-flagHidrograma))/sum(GHmax  * (1-flagHidrograma)),
                        gh_hidrograma = pmin(produtibilidade * vazao, GHmax),
                        gh_impactada = ifelse(flagHidrograma == 0,
                                              pmin(GHmax,pmax(0,sum(gh) - sum(gh_hidrograma,na.rm = T))) * proporcao_impactada,
                                              NA),
                        gh_hidrograma_corrigido = ifelse(flagHidrograma == 1,
                                                         pmin(gh_hidrograma, sum(gh) * proporcao_hidrograma) + pmax(0,(sum(gh) - sum(gh_impactada,na.rm = T) - sum(gh_hidrograma,na.rm = T))) * proporcao_hidrograma,
                                                         NA),
                        gh_corrigido = dplyr::coalesce(gh_hidrograma_corrigido,gh_impactada,gh),
                        vazao = gh_corrigido/produtibilidade) %>% 
          dplyr::ungroup() %>% 
          dplyr::rowwise() %>% 
          dplyr::mutate(pdisph = ifelse(codUsina %in% unique(df.tabelaModulacao$codUsina),
                                        funcao(vazao),
                                        gh_corrigido))
        
        df.dadosUHEModulamTabela <- df.dadosUHEModulamTabela.usina %>% 
          dplyr::group_by(A02_NR_REE,A06_NR_SERIE,A06_NR_MES) %>% #dados por REE
          dplyr::reframe(pdisph = sum(pdisph)) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = pdisph,
                        A01_CD_MODELO = codModelo,
                        A01_TP_CASO = tipoCaso,
                        A01_NR_CASO = numeroCaso,
                        A09_NR_MES = A06_NR_MES,
                        A09_NR_SERIE = A06_NR_SERIE,
                        A09_VL_GERACAO_HIDRO_MINIMA = 0,
                        A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = 0,
                        A09_VL_POTENCIA_MAXIMA = 0) %>% 
          dplyr::select(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A09_NR_MES,
                        A09_NR_SERIE, A09_VL_GERACAO_HIDRO_MINIMA, A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                        A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, A09_VL_POTENCIA_MAXIMA)
        
        #concatena as usinas dos REEs que têm usinas que modulam segundo a curva
        df.dadosCalculadosUHETipo4<- rbind(df.dadosCalculadosUHETipo4, df.dadosUHEModulamTabela.usina)
        
        # concatena as REEs que modulam com as que nao modulam e as que modulam por tabela para gravar na base
        df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist, df.dadosSsistNaoModulam, df.dadosUHEModulamTabela)
      }else{
        df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist, df.dadosSsistNaoModulam)
      }
      
      # Para buscar o subsistema
      df.dadosCalculadosSsist <- dplyr::inner_join(df.dadosCalculadosSsist, df.ree,
                                                   by = c("A02_NR_REE"))
      
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
      
      # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      DBI::dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSsistAgrup, append = T)
      DBI::dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      
    }
    
    writexl::write_xlsx(
      list(
        dados = 
          df.dadosCalculadosUHETipo4 %>% 
          dplyr::select(
            A02_NR_REE,A06_NR_SERIE,A06_NR_MES,grupo,codUsina,
            GHmax ,proporcao,
            ghtot_ree,gh,
            vazao,produtibilidade,gh_hidrograma,
            flagHidrograma,proporcao_hidrograma,proporcao_impactada,
            gh_impactada,gh_hidrograma_corrigido,
            gh_corrigido,vazao,pdisph
          )
      ),
      path = file.path(pastaSaidas,"REE_modulacaoTabelaPdisp.xlsx",fsep = "\\"))
  }
  
  return("Disponibilidade hidro processada com sucesso!")
}
