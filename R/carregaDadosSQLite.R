#' Carrega dados na base SQLite do Balanco de Potencia
#'
#' Faz a leitura dos arquivos do NEWAVE e carrega todos os dados necessarios na base SQLite para execucao do Balanco de Ponto.
#' Usa como referencia para a identificacao dos arquivos as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 25.0.1 de setembro/2019 - paginas 12 - 15
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param pastaCaso localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta
#' @param pastaSaidas localizacao dos arquivos de saida do modulo NWLISTOP
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param descricaoCaso vetor de caracteres indicando a descricao do caso
#' @param horasPonta valor inteiro com o numero de horas de ponta
#' @param idDemandaLiquida identificador de calculo com demanda liquida. 1:Demanda Liquida; 0:Deterministica
#' @param sistemasNaoModulamPonta vetor numerico com codigos dos sitemas que nao modulam na ponta
#' @param sistemasNaoModulamMedia vetor numerico com codigos dos sitemas que nao modulam na media
#' @param codTucurui codigo atribuido para a usina de Tucurui
#' @param cotaLimiteTucurui valor da cota da usina de Tucurui [m]
#' @param geracaoLimiteTucurui valor da geracao limite da usina de Tucurui
#'
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export
carregaDadosSQLite <- function(baseSQLite, pastaCaso, pastaSaidas, tipoCaso, numeroCaso, codModelo, descricaoCaso, horasPonta, 
                               idDemandaLiquida, sistemasNaoModulamPonta, sistemasNaoModulamMedia,
                               codTucurui, cotaLimiteTucurui, geracaoLimiteTucurui) {
  # barra de progresso
  incProgress(3/100, detail = "Leitura de Dados Gerais")
  
  # pega dados gerais do NEWAVE
  df.dadosGerais <- leituraDadosGerais(pastaCaso)
  # pega dados de configuracao hidro
  df.configuracaoHidro <- leituraConfiguracaoHidro(pastaCaso)
  
  # define inicio e fim de caso
  inicioCaso <- df.dadosGerais$anoInicio * 100 + df.dadosGerais$mesInicio
  fimCaso <- (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1) * 100 + 12
  
  # define o numero de series utilizadas no estudo
  if (df.dadosGerais$tipoSimulacao == 1) {
    seriesHidro <- df.dadosGerais$seriesSinteticas
  } else if (df.dadosGerais$tipoSimulacao == 2) {
    # veririfica se todas as series do confhd possuem o mesmo tamanho
    seriesHidro <- df.configuracaoHidro %>%
      mutate(seriesHidro = fimHistorico - inicioHistorico + 1) %>%
      summarise(media = mean(seriesHidro), minimo = min(seriesHidro), maximo = max(seriesHidro))
    if (seriesHidro$media != seriesHidro$minimo | seriesHidro$media != seriesHidro$maximo | seriesHidro$minimo != seriesHidro$maximo) {
      stop("S\u00E9ries hidro n\u00E3o possuem mesmo horizonte cadastrado no arquivo confhd!")
    }
    fimHistorico <- df.configuracaoHidro %>% pull(fimHistorico) %>% max()
    # resgata o valor de inicio de varredura da serie historica para contabilizar quantidade de series
    inicioSimulacaoHistorico <- leituraSeriesHistoricasSimulacaoFinal(pastaCaso) %>% extract2("df.varredura") %>% pull(anoInicio)
    seriesHidro <- fimHistorico - inicioSimulacaoHistorico + 1
  } else {
    stop("Simula\u00E7\u00E3o final ap\u00F3s converg\u00EAncia PDDE do NEWAVE deve ser com s\u00E9ries sint\u00E9ticas ou hist\u00F3ricas!")
  }
  
  # barra de progresso
  incProgress(3/100, detail = "Leitura dos Dados do MDI")  
  
  # somente verifica arquivo do MDI para casos do PDE
  if (tipoCaso == 1) { 
    # define inicio e fim de caso MDI
    arquivoInfoMDI <- paste(pastaCaso, "infoMDI.txt", sep = "/")
    # verifica exitencia do arquivo
    if (!file.exists(arquivoInfoMDI)) {
      stop(paste0("arquivo ", arquivoInfoMDI, " com dados gerais do MDI n\u00E3o encontrado!"))
    }
    df.infoMDI <- read_delim(stri_enc_toutf8(arquivoInfoMDI), 
                             locale = locale(encoding = "latin1"),
                             delim = ";", 
                             col_types = "cc",
                             trim_ws = TRUE)
    
    anoMesInicioMDI <- df.infoMDI %>% filter(variavel == "inicioHorizonteEstudo") %>% pull(valor) %>% as.integer()
    anoMesFimMDI <- df.infoMDI %>% filter(variavel == "fimHorizonteEstudo") %>% pull(valor) %>% as.integer()
    
  } else {
    anoMesInicioMDI <- NA
    anoMesFimMDI <- NA
  }
  
  # barra de progresso
  incProgress(4/100, detail = "Gravando no Banco de Dados")

  # inicio do processo de gravacao das tabelas BPO_A01_CASOS_ANALISE, BPO_A02_SUBSISTEMAS, BPO_A02_REES e BPO_A19_FATOR_PONTA_OFR
  # abre conexao
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  
  # abre transacao com o banco de dados para gravar somente se tudo der certo
  dbExecute(conexao, "BEGIN TRANSACTION;")
  
  # BPO_A01_CASOS_ANALISE
  # limpa a tabela de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A01_CASOS_ANALISE WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A01_CASOS_ANALISE WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  df.casosAnalise <- data.frame(A01_TP_CASO = tipoCaso,
                                A01_NR_CASO = numeroCaso,
                                A01_CD_MODELO = codModelo,
                                A01_TX_DESCRICAO = descricaoCaso,
                                A01_NR_MES_INICIO = inicioCaso,
                                A01_NR_MES_FIM = fimCaso,
                                A01_NR_HORAS_PONTA =  horasPonta,
                                A01_NR_COTA_LIMITE_TUCURUI = cotaLimiteTucurui,
                                A01_NR_GERACAO_LIMITE_TUCURUI = geracaoLimiteTucurui,
                                A01_IN_DEMANDA_LIQUIDA = idDemandaLiquida,
                                A01_NR_SERIES_HIDRO = seriesHidro,
                                A01_NR_MES_INICIO_MDI = anoMesInicioMDI,
                                A01_NR_MES_FIM_MDI = anoMesFimMDI)
  # salva BPO_A01_CASOS_ANALISE
  dbWriteTable(conexao, "BPO_A01_CASOS_ANALISE", df.casosAnalise, append = T)
  
  # BPO_A02_SUBSISTEMAS
  # limpa a tabela de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A02_SUBSISTEMAS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A02_SUBSISTEMAS WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  df.sistema <- leituraDeficitSistema(pastaCaso) %>% filter(patamar %in% c(1, NA)) %>% 
    select(A02_NR_SUBSISTEMA = codSubsistema,
           A02_TX_DESCRICAO_SUBSISTEMA = nomeSubsistema,
           A02_TP_FICTICIO = tipoFicticio,
           A02_VL_CUSTO_DEFICIT = custoDefict) %>% 
    mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo)
  
  # salva BPO_A02_SUBSISTEMAS
  dbWriteTable(conexao, "BPO_A02_SUBSISTEMAS", df.sistema, append = T)
  
  # BPO_A02_REES
  # limpa a tabela de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A02_REES WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A02_REES WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  df.ree <- leituraREE(pastaCaso)
  names(df.ree) <- c("A02_NR_REE", "A02_TX_DESCRICAO_REE", "A02_NR_SUBSISTEMA")
  df.ree$A01_TP_CASO <- tipoCaso
  df.ree$A01_NR_CASO <- numeroCaso
  df.ree$A01_CD_MODELO <- codModelo
  df.ree$A02_TP_CALC_POTENCIA <- 1
  
  # define os valores do calculo de potencia passados pelo usuario
  df.ree <- df.ree %>% mutate(A02_TP_CALC_POTENCIA = ifelse(A02_NR_REE %in% sistemasNaoModulamPonta, 2, A02_TP_CALC_POTENCIA))
  df.ree <- df.ree %>% mutate(A02_TP_CALC_POTENCIA = ifelse(A02_NR_REE %in% sistemasNaoModulamMedia, 3, A02_TP_CALC_POTENCIA))
  
  # salva BPO_A02_REES
  dbWriteTable(conexao, "BPO_A02_REES", df.ree, append = T)
  
  # grava dados das usinas hidreletricas na tabela BPO_A03_DADOS_UHE 
  gravaDadosUsinasHidroBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados referentes aos conjuntos e maquinas das usinas hidreletricas do NEWAVE na tabela BPO_A04_MAQUINAS_UHE
  gravacaoDadosMaquinasHidroBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados das usinas hidreletricas ao longo do horizonte de simulacao do NEWAVE na tabela BPO_A05_DADOS_VIGENTES_UHE
  gravacaoDadosHidroVigenteBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados de armazenamento e geracao das usinas hidraulicas na tabela BPO_A06_SAIDA_HIDRO_NEWAVE
  gravaSaidasNewaveBDBP(pastaCaso, pastaSaidas, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados de demanda de ponta do NEWAVE na tabela BPO_A10_DEMANDA
  gravacaoDadosDemandaBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados dos limites de intercambio ao longo do horizonte de simulacao do NEWAVE na tabela BPO_A11_INTERCAMBIOS
  gravacaoDadosIntercambioBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # nao grava limites e agrupamento dos intercambios se for caso de garantia fisica
  if (tipoCaso != 3) {
    # grava dados dos limites dos agrupamento de intercambios ao longo do horizonte de simulacao do NEWAVE na tabela BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO
    gravacaoDadosLimitesAgrupIntercambioBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
    
    # grava dados de agrupamento dos intercambios do NEWAVE na tabela BPO_A15_AGRUPAMENTOS_INTERCAMBIO
    gravacaoDadosAgrupIntercambioBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  }
  
  # grava dados das usinas termeletricas ao longo do horizonte de simulacao do NEWAVE na tabela BPO_A14_DISPONIBILIDADE_UTE
  gravacaoDadosTermeletricasBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo)
  
  # grava dados de disponilidade das outras fontes renovaveis do NEWAVE na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP. 
  # alem disso, grava as tabelas de apoio BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR
  lt.dadosOutrasFontes <- gravacaoDadosDisponibilidadeOutrasFontesBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, anoMesInicioMDI, anoMesFimMDI)
  
  # calcula e grava dados das reservas de carga e por motivo de renovaveis na tabela BPO_A21_RESERVA
  gravacaoDadosReservaBDBP(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, lt.dadosOutrasFontes$df.energiaOFR) 
  
  # efetua commit no banco de dados confirmando todas as gravacoes com sucesso
  dbExecute(conexao, "COMMIT TRANSACTION;")
  # fecha conexao
  dbDisconnect(conexao)
  
  return("Leitura e grava\u00E7\u00E3o dos dados do NEWAVE efetuadas com sucesso!")
}