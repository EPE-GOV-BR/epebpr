#' Carrega dados na base SQLite do Balanco de Ponta
#'
#' Faz a leitura dos arquivos do NEWAVE e carrega todos os dados necessarios na base SQLite para execucao do Balanco de Ponto.
#' Usa como referencia para a identificacao dos arquivos as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 25.0.1 de setembro/2019 - paginas 12 - 15
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param pastaCaso localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param descricaoCaso vetor de caracteres indicando a descricao do caso
#' @param horasPonta valor inteiro com o numero de horas de ponta
#' @param reservaOperativa valor com a reserva operativa usada no calculo do balanco. Ex. 0.05
#' @param idDemandaLiquida identificador de calculo com demanda liquida. 1:Demanda Liquida; 0:Deterministica
#' @param anosPre numero de anos iniciais para fins de estabilizacao no calculo da politica
#' @param anosPos numero de anos finais para fins de estabilizacao no calculo da politica
#' @param sistemasNaoModulamPonta vetor numerico com codigos dos sitemas que nao modulam na ponta
#' @param sistemasNaoModulamMedia vetor numerico com codigos dos sitemas que nao modulam na media
#' @param codTucurui codigo atribuido para a usina de Tucurui ###############################
#' @param cotaLimiteTucurui valor da cota da usina de Tucurui em metros
#' @param geracaoLimiteTucurui valor da geracao limite da usina de Tucurui
#'
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export
carregaDadosSQLite <- function(baseSQLite, pastaCaso, tipoCaso, numeroCaso, codModelo, descricaoCaso, horasPonta, reservaOperativa, idDemandaLiquida,
                               anosPre, anosPos, sistemasNaoModulamPonta, sistemasNaoModulamMedia,
                               codTucurui, cotaLimiteTucurui, geracaoLimiteTucurui, anoMesInicioMDI = NA, anoMesFimMDI = NA) {
  # identifica os arquivos do NEWAVE para leitura
  df.arquivos <- leituraArquivos(pastaCaso)
  # de acordo com o manual do NEWAVE o arquivo de dados gerais fica na linha 1
  arquivoDger <- df.arquivos %>% filter(row_number() == 1) %>% select(arquivo) %>% pull() %>% paste(pastaCaso, ., sep = "/")
  # de acordo com o manual do NEWAVE o arquivo de sistema fica na linha 2
  arquivoSistema <- df.arquivos %>% filter(row_number() == 2) %>% select(arquivo) %>% pull() %>% paste(pastaCaso, ., sep = "/")
  # de acordo com o manual do NEWAVE o arquivo de sistema fica na linha 3
  arquivoConfhd <- df.arquivos %>% filter(row_number() == 3) %>% select(arquivo) %>% pull() %>% paste(pastaCaso, ., sep = "/")
  # de acordo com o manual do NEWAVE o arquivo de REE fica na linha 36
  arquivoREE <- df.arquivos %>% filter(row_number() == 36) %>% select(arquivo) %>% pull() %>% paste(pastaCaso, ., sep = "/")

  # pega dados gerais do NEWAVE
  df.dadosGerais <- leituraDadosGerais(arquivoDger)
  # pega dados de configuracao hidro
  df.configuracaoHidro <- leituraConfiguracaoHidro(arquivoConfhd)

  # define inicio e fim de caso dependendo da opcao informada
  if (!anosPre) {
    inicioCaso <- ((df.dadosGerais$anoInicio + df.dadosGerais$anosPre) * 100) + df.dadosGerais$mesInicio
  } else {
    inicioCaso <- df.dadosGerais$anoInicio * 100 + df.dadosGerais$mesInicio
  }

  if (!anosPos) {
    fimCaso <- (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - df.dadosGerais$anosPos - 1) * 100 + 12
  } else {
    fimCaso <- (df.dadosGerais$anoInicio + df.dadosGerais$duracaoEstudo - 1) * 100 + 12
  }

  # define o numero de series utilizadas no estudo
  if (df.dadosGerais$tipoSimulacao == 1) {
    seriesHidro <- df.dadosGerais$seriesSinteticas
  } else if (df.dadosGerais$tipoSimulacao == 2) {
    seriesHidro <- leituraConfiguracaoHidro(arquivoConfhd) %>%
      mutate(seriesHidro = fimHistorico - inicioHistorico) %>%
      summarise(media = mean(seriesHidro), minimo = min(seriesHidro), maximo = max(seriesHidro))
    if (seriesHidro$media != seriesHidro$minimo | seriesHidro$media != seriesHidro$maximo | seriesHidro$minimo != seriesHidro$maximo) {
      stop("Series hidro nao possuem mesmo horizonte cadastrado no arquivo confhd!")
    }
    seriesHidro <- seriesHidro$media
  } else {
    stop("Simulacaoo final apos convergencia PDDE do NEWAVE deve ser com series sinteticas ou historicas!")
  }

  df.dadosGerais$anoMesInicio

  # inicio do processo de gravacao das tabelas BPO_A01_CASOS_ANALISE, BPO_A02_SUBSISTEMAS, BPO_A02_REES e BPO_A19_FATOR_PONTA_OFR
  # abre conexao
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)

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
                                A01_VL_RESERVA_OPERATIVA = reservaOperativa,
                                A01_IN_DEMANDA_LIQUIDA = idDemandaLiquida,
                                A01_NR_SERIES_HIDRO = seriesHidro,
                                A01_NR_MES_INICIO_MDI = ifelse(tipoCaso == 1, anoMesInicioMDI, NA),
                                A01_NR_MES_FIM_MDI = ifelse(tipoCaso == 1, anoMesFimMDI, NA))
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

  df.sistema <- leituraSistema(arquivoSistema)
  names(df.sistema) <- c("A02_NR_SUBSISTEMA","A02_TX_DESCRICAO_SUBSISTEMA","A02_TP_FICTICIO", "A02_VL_CUSTO_DEFICIT")
  df.sistema$A01_TP_CASO <- tipoCaso
  df.sistema$A01_NR_CASO <- numeroCaso
  df.sistema$A01_CD_MODELO <- codModelo

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

  df.ree <- leituraREE(arquivoREE)
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

  # BPO_A19_FATOR_PONTA_OFR
  # limpa a tabela de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }

  arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
  if (!file.exists(arquivoDadosOFR)) {
    stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel n\u00E3o encontrado!"))
  }
  df.fatorPontaOFR <- read_xlsx(arquivoDadosOFR, sheet = "FatorPonta")
  df.fatorPontaOFR$A01_TP_CASO <- tipoCaso
  df.fatorPontaOFR$A01_NR_CASO <- numeroCaso
  df.fatorPontaOFR$A01_CD_MODELO <- codModelo

  # salva BPO_A19_FATOR_PONTA_OFR
  dbWriteTable(conexao, "BPO_A19_FATOR_PONTA_OFR", df.fatorPontaOFR, append = T)

  # fecha conexao
  dbDisconnect(conexao)

  return("Sucesso!")
  }

# # converte planilha de pequenas em arquivo csv para usar no Julia
# leituraPlanilhaRenovaveis(pastaCaso, tipoCaso, anoMesInicioMDI, anoMesFimMDI)
