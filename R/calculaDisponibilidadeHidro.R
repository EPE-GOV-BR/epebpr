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
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export
calculaDisponibilidadeHidro <- function(baseSQLite, pastaCaso, pastaSaidas, tipoCaso, numeroCaso, codModelo, codTucurui) {
  # SQLite
  conexaoSQLite <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # fecha conexao com a base SQLite na saida da funcao, seja por erro ou normalmente
  on.exit(dbDisconnect(conexaoSQLite))

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
  df.dadosCaso <- dbGetQuery(conexaoSQLite, sql)
  
  # pega dados da tabela de ree
  sql <- paste0("SELECT A02_NR_REE, A02_NR_SUBSISTEMA, A02_TX_DESCRICAO_REE
                  FROM BPO_A02_REES
                  WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
  df.ree <- dbGetQuery(conexaoSQLite, sql)
  
  # faz calculos com base em dados do SUISHI (codModelo = 2) ou do NEWAVE (codModelo = 1)
  if (codModelo == 2) {
    # barra de progresso
    incProgress(0.4, detail = "Disponibilidade Hidro pelo SUISHI")
    
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
    df.dadosUHE <- dbGetQuery(conexaoSQLite, sql)
    
    sql <- paste0("SELECT A03_CD_USINA, A05_NR_MES,              
                    A05_VL_VAZAO_MINIMA, A05_VL_TEIF, A05_VL_IP, A05_VL_POTENCIA
                  FROM BPO_A05_DADOS_VIGENTES_UHE
                  WHERE
                    A05_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                    A01_TP_CASO = ", tipoCaso, " AND
                    A01_NR_CASO = ", numeroCaso, " AND
                    A01_CD_MODELO = ", codModelo)
    df.dadosVigentesUHE <- dbGetQuery(conexaoSQLite, sql)
    
    # dados do SUISHI
    df.operacaoHidroSUISHI <- leituraOperacaoHidroSUISHI(pastaSaidas) %>% 
      filter(patamar == 1) %>% 
      select(codUsina, anoMes, serie, codREE, alturaLiquida = QUED, disponibilidadePonta = PDISP_QMIN)  
      
    # critica falta de dados
    dadosNA <- df.operacaoHidroSUISHI %>% filter(is.na(alturaLiquida) | is.na(disponibilidadePonta)) %>% nrow()
    if(dadosNA > 0) {
      usinasComNA <- df.operacaoHidroSUISHI %>% filter(is.na(alturaLiquida) | is.na(disponibilidadePonta)) %>% pull(codUsina) %>% 
        unique() %>% paste(collapse = ", ")
      stop(paste0("Usinas ", usinasComNA, " sem dados de QUED ou PDISP_QMIN"))
    } 

    # junta dados do SUISHI com dados de cadastro   
    df.operacaoHidroSUISHI <- left_join(df.operacaoHidroSUISHI, df.dadosUHE, by = c("codUsina" = "A03_CD_USINA")) %>% 
      left_join(df.dadosVigentesUHE, by = c("codUsina" = "A03_CD_USINA", "anoMes" = "A05_NR_MES"))
    
    # recupera dados do hidr para casos que nao estao dos dados vigentes (por exemplo, usinas 183 e 184)
    df.dadosHidroComplementares <- leituraDadosUsinasHidro(pastaCaso) %>% use_series("df.dadosUsinasHidroeletricas") %>% 
      select(codUsina, vazaoMinimaHistorico, TEIF, IP)
    
    # substitui os dados vazios pelos dados do hidr. para a potencia, por simplificacao, usa o mesmo valor da disponivel caso nao tenha o dado
    df.operacaoHidroSUISHI <- inner_join(df.operacaoHidroSUISHI, df.dadosHidroComplementares, by = "codUsina") %>% 
      mutate(A05_VL_VAZAO_MINIMA = if_else(is.na(A05_VL_VAZAO_MINIMA),
                                           as.double(vazaoMinimaHistorico),
                                           A05_VL_VAZAO_MINIMA),
             A05_VL_TEIF = if_else(is.na(A05_VL_TEIF),
                                   TEIF/100,
                                   A05_VL_TEIF),
             A05_VL_IP = if_else(is.na(A05_VL_IP),
                                 IP/100,
                                 A05_VL_IP),
             A05_VL_POTENCIA = if_else(is.na(A05_VL_POTENCIA),
                                       disponibilidadePonta,
                                 A05_VL_POTENCIA)) %>% 
      # caso sobre dados com NA, altera para zero para nao atrapalhar nas contas
      mutate(A05_VL_VAZAO_MINIMA = if_else(is.na(A05_VL_VAZAO_MINIMA),
                                           0,
                                           A05_VL_VAZAO_MINIMA),
             A05_VL_TEIF = if_else(is.na(A05_VL_TEIF),
                                   0,
                                   A05_VL_TEIF),
             A05_VL_IP = if_else(is.na(A05_VL_IP),
                                 0,
                                 A05_VL_IP),
             A05_VL_POTENCIA = if_else(is.na(A05_VL_POTENCIA),
                                       0,
                                       A05_VL_POTENCIA),
             A03_VL_PRODUTIBILIDADE = if_else(is.na(A03_VL_PRODUTIBILIDADE),
                                       0,
                                       A03_VL_PRODUTIBILIDADE))
    
    # calcula geracao minima e desconta taxas de falha e indisponibilidade
    df.operacaoHidroSUISHI <- df.operacaoHidroSUISHI %>% mutate(geracaoMinima = alturaLiquida * A05_VL_VAZAO_MINIMA * A03_VL_PRODUTIBILIDADE,
                                                                disponibilidadePonta = disponibilidadePonta * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                                potenciaMaxima = A05_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                                # garante geracao minima igual a disponibilidade pois tem casos que nao respeitam o minimo
                                                                # e isso causaria inviabilidade na resolucao do PL
                                                                geracaoMinima = if_else(geracaoMinima > disponibilidadePonta,
                                                                                        disponibilidadePonta,
                                                                                        geracaoMinima))
      

    df.operacaoHidroSUISHI <- inner_join(df.operacaoHidroSUISHI, df.ree, by = c("codREE" = "A02_NR_REE"))
    
    # monta estrutuda de dados para a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    df.dadosCalculadosSistema <- df.operacaoHidroSUISHI %>%
      group_by(A02_NR_SUBSISTEMA, anoMes, serie) %>%
      summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(geracaoMinima), 4),
                A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(geracaoMinima), 4),
                A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(disponibilidadePonta), 4),
                A09_VL_POTENCIA_MAXIMA = round(sum(potenciaMaxima), 4), .groups = "drop") %>% 
      mutate(A01_CD_MODELO = codModelo,
             A01_NR_CASO = numeroCaso,
             A01_TP_CASO = tipoCaso,
             A09_NR_MES = anoMes,
             A09_NR_SERIE = serie) %>% 
      select(A01_CD_MODELO, A01_NR_CASO, A01_TP_CASO, A02_NR_SUBSISTEMA, A09_NR_MES, A09_NR_SERIE, 
             A09_VL_GERACAO_HIDRO_MINIMA, A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL, A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, A09_VL_POTENCIA_MAXIMA)
    
    # trava conexao com o banco para melhorar desempenho e evitar outros usos da base simultaneos
    dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
    # limpa BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA de outras execucoes para o mesmo caso
    sql <- paste0("DELETE FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
    dbExecute(conexaoSQLite, sql)

    # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
    dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSistema, append = T)
    dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    
  } else {
    
    # barra de progresso
    incProgress(3/100, detail = "Atualiza\u00E7\u00E3o de Submotoriza\u00E7\u00E3o")
    
    # atualizacao de submotorizacao
    quantidadeExpansaoHidro <- df.dadosExpansaoHidro <- leituraDadosExpansaoUsinasHidro(pastaCaso) %>% 
      extract2("df.dadosExpansaoHidro") %>% nrow()
    
    # se houver alguma expansao hidro segue atualizacao 
    if (quantidadeExpansaoHidro > 0) {
      
      df.submotorizacaoREE <- leituraSubmotorizacaoREE(pastaCaso) %>%
        inner_join(df.ree, by = c("nomeREE" = "A02_TX_DESCRICAO_REE")) %>%
        filter(anoMes >= df.dadosCaso$dataInicioCaso, anoMes <= df.dadosCaso$dataFimCaso)
      
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
      dbExecute(conexaoSQLite, "BEGIN TRANSACTION;")
      dbExecute(conexaoSQLite, sqlUpdate, param = lt.submotorizacaoREE)
      dbExecute(conexaoSQLite, "COMMIT TRANSACTION;")
    }
    # fim atualizacao de submotorizacao
    
    # barra de progresso
    incProgress(3/100, detail = "REEs que utilizam a GHm\u00E9dia")
    
    # filtro dos ree com calculo tipo 1
    sql <- paste0("SELECT A02_NR_REE FROM BPO_A02_REES
                 WHERE A02_TP_CALC_POTENCIA = 1 AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo)
    reeCalcPotencia1 <- dbGetQuery(conexaoSQLite, sql) %>% pull(A02_NR_REE) %>% paste(collapse = ", ")
    
    sql <- paste0("SELECT 
                  A03_CD_USINA,
                  A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4,
                  A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE 
                 FROM BPO_A03_DADOS_UHE
                 WHERE
                  A03_TX_STATUS <> 'NC' AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, " AND 
                  A02_NR_REE IN (", reeCalcPotencia1, ")")
    df.dadosUHE <- dbGetQuery(conexaoSQLite, sql)
    
    sql <- paste0("SELECT A03_CD_USINA,
                  SUM(A04_NR_MAQUINAS * A04_VL_POTENCIA) AS POT_TOTAL
                 FROM BPO_A04_MAQUINAS_UHE
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, " 
                 GROUP BY A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA")
    df.dadosMaquinasUHE <- dbGetQuery(conexaoSQLite, sql)
    
    
    sql <- paste0("SELECT A03_CD_USINA, A05_NR_MES, A02_NR_REE,              
                  A05_NR_CANAL_FUGA_MEDIO, A05_VL_VOL_MAX, A05_VL_VOL_MIN, A05_VL_VAZAO_MINIMA, A05_VL_TEIF, A05_VL_IP, A05_VL_POTENCIA
                 FROM BPO_A05_DADOS_VIGENTES_UHE
                 WHERE
                  A05_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, " AND 
                  A02_NR_REE IN (", reeCalcPotencia1, ")")
    df.dadosVigentesUHE <- dbGetQuery(conexaoSQLite, sql)
    
    
    sql <- paste0("SELECT A02_NR_REE, A06_NR_MES, A06_NR_SERIE, A06_VL_PERC_ARMAZENAMENTO, A06_VL_GERACAO_HIDRAULICA, A06_VL_SUBMOTORIZACAO 
               FROM BPO_A06_SAIDA_HIDRO_NEWAVE
               WHERE
                A06_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
                A01_TP_CASO = ", tipoCaso, " AND
                A01_NR_CASO = ", numeroCaso, " AND
                A01_CD_MODELO = ", codModelo, " AND 
                A02_NR_REE IN (", reeCalcPotencia1, ")")
    df.saidasHidro <- dbGetQuery(conexaoSQLite, sql)
    
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
    df.potMaquinas<- dbGetQuery(conexaoSQLite, sql)
    
    # barra de progresso
    incProgress(4/100, detail = "Excluindo outras execu\u00E7\u00F5s de BP para o mesmo caso")
    
    # limpa base BPO_A08_DADOS_CALCULADOS_UHE de outras execucoes para o mesmo caso
    dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
    dbExecute(conexaoSQLite, "PRAGMA journal_mode = TRUNCATE;")
    sql <- paste0("DELETE FROM BPO_A08_DADOS_CALCULADOS_UHE
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
    dbExecute(conexaoSQLite, sql)
    
    # limpa BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA de outras execucoes para o mesmo caso
    sql <- paste0("DELETE FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
                 WHERE
                  A01_TP_CASO = ", tipoCaso, " AND
                  A01_NR_CASO = ", numeroCaso, " AND
                  A01_CD_MODELO = ", codModelo, ";")
    dbExecute(conexaoSQLite, sql)
    dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
    
    # for criado para resolver problema de alocacao de memoria. orginalmente era um data frame unico com todos os dados, contudo,
    # trabalalhando com series sinteticas, acabava por exaurir a memoria.
    tamanhoJanela <- 100
    janelaSeries <- c(seq(1, df.dadosCaso$numeroSeries, tamanhoJanela), (df.dadosCaso$numeroSeries + 1))
    quantidadeJanela <- length(janelaSeries)
    
    for (andaJanela in 1:(quantidadeJanela - 1)) {
      # barra de progresso
      incProgress((1/(quantidadeJanela - 1))*0.3, detail = paste(round(andaJanela*100/(quantidadeJanela - 1), 0),"%"))
      
      df.dadosCalculadosUHE <- inner_join(df.dadosVigentesUHE, 
                                          filter(df.saidasHidro, between(A06_NR_SERIE, janelaSeries[andaJanela], (janelaSeries[andaJanela + 1] - 1))), 
                                          by = c("A02_NR_REE", "A05_NR_MES" = "A06_NR_MES")) %>% 
        mutate(A08_VL_VOLUME_OPERATIVO = (A05_VL_VOL_MAX - A05_VL_VOL_MIN) * A06_VL_PERC_ARMAZENAMENTO + A05_VL_VOL_MIN, 
               A06_VL_GERACAO_HIDRAULICA = A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO)
      
      df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosMaquinasUHE, by = "A03_CD_USINA") %>% 
        mutate(VL_POT_EXP = round(A05_VL_POTENCIA - POT_TOTAL, 2))
      
      df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosUHE, by = "A03_CD_USINA") %>% 
        mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
        select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES = A05_NR_MES, A08_NR_SERIE = A06_NR_SERIE, A02_NR_REE, 
               A08_VL_VOLUME_OPERATIVO, A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4, 
               A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE, A05_NR_CANAL_FUGA_MEDIO, A05_VL_TEIF, A05_VL_IP, 
               A05_VL_VAZAO_MINIMA, A06_VL_GERACAO_HIDRAULICA, VL_POT_EXP)
      
      # 2 - COTA OPERATIVA(per,ser) => CALCULADA a partir do VOLUME OPERATIVO(per,ser) e do polinomio cota-volume
      # 3 - ALTURA DE QUEDA LIQUIDA(per,ser) => CALCULADA a partir da COTA OPERATIVA(per,ser), CANAL DE FUGA MEDIO (per) e Perdas
      # 3.1 - Altura de queda liquida = Cota Operativa - Canal de Fuga medio
      # 3.2 - Abate as perdas
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        mutate(A08_VL_COTA_OPERATIVA = A03_NR_PCV_0  + (A03_NR_PCV_1 * A08_VL_VOLUME_OPERATIVO) +
                 (A03_NR_PCV_2 * A08_VL_VOLUME_OPERATIVO^2) +
                 (A03_NR_PCV_3 * A08_VL_VOLUME_OPERATIVO^3) +
                 (A03_NR_PCV_4 * A08_VL_VOLUME_OPERATIVO^4),
               A08_VL_ALTURA_LIQUIDA = ifelse(A03_TP_PERDA == 1,
                                              (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO) * (1 - (A03_VL_PERDA / 100)),
                                              (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO)- A03_VL_PERDA))
      
      
      # limpa campos ja usados
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        select(-A03_NR_PCV_0, -A03_NR_PCV_1, -A03_NR_PCV_2, -A03_NR_PCV_3,
               -A03_NR_PCV_4, -A03_VL_PERDA, -A05_NR_CANAL_FUGA_MEDIO)
      
      # 3.3 - Ajuste caso encontre algum valor negativo
      df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA[df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA < 0.0001] <- 0.0001
      
      # 4 - POTENCIA MaXIMA(per,ser)
      # 4.1) PARA POTENCIA REFERENTE AOS CONJUNTOS Ja EXISTENTES: ALTURA DE REFERENCIA DO CONJUNTO >= ALTURA DE QUEDA
      df.dadosCalculadosUHEMaquinas <- inner_join(df.dadosCalculadosUHE, df.potMaquinas,
                                                  by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA"))
      
      # Para os conjuntos onde a altura de queda liquida e menor que a altura de referencia do conjunto, a potencia
      # nao e igual a maxima, mas um percentual da maxima, calculado a partir da razao (HLIQ/HREF)^coef da turbina
      df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
        mutate(A08_VL_POTENCIA_MAXIMA = ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                               A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                               A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                 (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA))
      
      # 4.3) PARA POTENCIA EM EXPANSaO e ALTURA DE REFERENCIA DO CONJUNTO 1 >= ALTURA DE QUEDA LiQUIDA(per,ser): APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP
      # 4.4) PARA POTENCIA EM EXPANSAO e ALTURA DE REFERENCIA DO CONJUNTO 1 < ALTURA DE QUEDA LiQUIDA(per,ser):
      # APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP E APLICA AINDA
      # UM FATOR DE REDUcaO DA POTENCIA IGUAL A (ALTURA DE QUEDA LiQUIDA(per,ser)/ALTURA DE REFERENCIA DO CONJUNTO 1)^COEFICIENTE DA TURBINA
      df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
        mutate(A08_VL_POTENCIA_MAXIMA =
                 (A08_VL_POTENCIA_MAXIMA + ifelse((A04_NR_CONJUNTO == 1 & VL_POT_EXP > 0) ,
                                                  ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                         VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                         VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                           (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA ),
                                                  0)))
      
      df.potenciaMaximaUHE <- df.dadosCalculadosUHEMaquinas %>%
        group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A03_CD_USINA, A08_NR_MES, A08_NR_SERIE) %>%
        summarise(A08_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>% ungroup()
      
      # remove data frame ja utilizado
      rm(df.dadosCalculadosUHEMaquinas)
      
      df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.potenciaMaximaUHE,
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
        mutate(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL =
                 A08_VL_ALTURA_LIQUIDA * A03_VL_PRODUTIBILIDADE * A05_VL_VAZAO_MINIMA,
               A08_VL_GERACAO_HIDRO_MINIMA = ifelse(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL > A08_VL_POTENCIA_MAXIMA,
                                                    A08_VL_POTENCIA_MAXIMA,
                                                    A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL))
      
      
      df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
        group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
        summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                  A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
        ungroup()
      
      df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                          by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
      
      
      # Calcula GHTOT por usina:
      # Rateia o GHTOT pela potencia maxima, ja abatida do GHMIN*/
      # GHMIN_UHE = GHMIN_UHE X (1 - (GHMIN_SSIS - GHTOT_SSIS)/GHMIN_SSIS)
      # GHMEDIA = GHMIN_UHE + ((GHTOT_SSIS - GHMIN_SSIS) X ((POT_MAX_UHE - GHMIN_UHE)/(POT_MAX_SSIS - GHMIN_SSIS)))
      # Modula o GHTOT, maximizando a geracao na horas de ponta*/
      # POT_MODULADA = ((GHMEDIA X 730.5) - (GHMIN X (730.5 - HORASPONTA))) / HORASPONTA
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        mutate(A08_VL_GERACAO_HIDRO_MINIMA = ifelse((A06_VL_GERACAO_HIDRAULICA < A09_VL_GERACAO_HIDRO_MINIMA_TMP),
                                                    (A08_VL_GERACAO_HIDRO_MINIMA * 
                                                       (1 - (A09_VL_GERACAO_HIDRO_MINIMA_TMP - A06_VL_GERACAO_HIDRAULICA) / A09_VL_GERACAO_HIDRO_MINIMA_TMP)),
                                                    A08_VL_GERACAO_HIDRO_MINIMA))
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>% select(-A09_VL_GERACAO_HIDRO_MINIMA_TMP)
      
      df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
        group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
        summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA)) %>% ungroup()
      
      df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                          by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        mutate(A08_VL_GERACAO_HIDRO_MEDIA = (A08_VL_GERACAO_HIDRO_MINIMA + ((A06_VL_GERACAO_HIDRAULICA - A09_VL_GERACAO_HIDRO_MINIMA_TMP) *
                                                                              ((A08_VL_POTENCIA_MAXIMA - A08_VL_GERACAO_HIDRO_MINIMA) / 
                                                                                 (A09_VL_POTENCIA_MAXIMA - A09_VL_GERACAO_HIDRO_MINIMA_TMP)))),
               A08_VL_POTENCIA_MAXIMA_MODULADA = (((A08_VL_GERACAO_HIDRO_MEDIA * 730.5) -
                                                     (A08_VL_GERACAO_HIDRO_MINIMA * (730.5 - df.dadosCaso$horasPonta))) / df.dadosCaso$horasPonta))
      
      # Ajusta potencia maxima de acordo com o limite superior
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse(A08_VL_POTENCIA_MAXIMA_MODULADA > A08_VL_POTENCIA_MAXIMA,
                                                        A08_VL_POTENCIA_MAXIMA,
                                                        A08_VL_POTENCIA_MAXIMA_MODULADA),
               A08_VL_VAZAO_MAXIMA = 0,
               A08_VL_VAZAO_MAXIMA_MODULADA = 0,
               A08_VL_ALTURA_MODULADA = 0)
      
      # Ajusta potencia maxima de tucurui
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse((A03_CD_USINA == codTucurui &
                                                           A08_VL_POTENCIA_MAXIMA_MODULADA > df.dadosCaso$gerLimiteTucurui &
                                                           A08_VL_COTA_OPERATIVA < df.dadosCaso$cotaLimiteTucurui),
                                                        df.dadosCaso$gerLimiteTucurui,
                                                        A08_VL_POTENCIA_MAXIMA_MODULADA))
      
      
      df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
        select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES,
               A08_NR_SERIE, A02_NR_REE, A08_VL_VOLUME_OPERATIVO,
               A08_VL_COTA_OPERATIVA, A08_VL_ALTURA_LIQUIDA, A08_VL_VAZAO_MAXIMA,
               A08_VL_POTENCIA_MAXIMA, A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
               A08_VL_GERACAO_HIDRO_MINIMA, A08_VL_GERACAO_HIDRO_MEDIA,
               A08_VL_VAZAO_MAXIMA_MODULADA, A08_VL_ALTURA_MODULADA,
               A08_VL_POTENCIA_MAXIMA_MODULADA)
      
      
      # grava dados calculados na BPO_A08_DADOS_CALCULADOS_UHE
      dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      dbWriteTable(conexaoSQLite, "BPO_A08_DADOS_CALCULADOS_UHE", df.dadosCalculadosUHE, append = T)
      dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      
      
      # monta estrutuda de dados para a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
      df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
        group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
        summarise(A09_VL_GERACAO_HIDRO_MINIMA = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                  A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = sum(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),
                  A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A08_VL_POTENCIA_MAXIMA_MODULADA),
                  A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
        ungroup()
      
      # corrigindo nome das colunas para ficar igual a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
      colnames(df.dadosCalculadosSsist)[5:6] <- c("A09_NR_MES", "A09_NR_SERIE")
      
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
      df.dadosSsistNaoModulam <- dbGetQuery(conexaoSQLite, sql)
      
      # concatena as REEs que modulam com as que nao modulam para gravar na base
      df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist,df.dadosSsistNaoModulam)
      
      
      # Para buscar o subsistema
      df.dadosCalculadosSsist <- inner_join(df.dadosCalculadosSsist, df.ree,
                                            by = c("A02_NR_REE"))
      
      # Nao precisa mais de REE e Descricao
      df.dadosCalculadosSsist <- df.dadosCalculadosSsist %>%
        select(-A02_NR_REE, -A02_TX_DESCRICAO_REE)
      
      # Agrupa os dados calculados por Subsistema, totalizando
      df.dadosCalculadosSsistAgrup <- df.dadosCalculadosSsist %>%
        group_by(A01_CD_MODELO,A01_NR_CASO,A01_TP_CASO,A02_NR_SUBSISTEMA,A09_NR_MES,A09_NR_SERIE) %>%
        summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(A09_VL_GERACAO_HIDRO_MINIMA),4),
                  A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),4),
                  A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA),4),
                  A09_VL_POTENCIA_MAXIMA = round(sum(A09_VL_POTENCIA_MAXIMA),4)) %>% ungroup()
      
      # remove data frame ja utilizado
      rm(df.dadosCalculadosSsist)
      
      # grava dados calculados na BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
      dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
      dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSsistAgrup, append = T)
      dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")
      
    }
    
  }

  return("Disponibilidade hidro processada com sucesso!")
}