#' Grava dados de armazenamento e geracao das usinas hidraulicas no banco de dados do balanco de potencia
#'
#' Grava dados de armazenamento, vertimento turbinavel e geracao das usinas hidraulicas originalmente nos 
#' arquivos earmfpXXX.out, verturbxxx.* e ghtotxxx.out na tabela BPO_A06_SAIDA_HIDRO_NEWAVE 
#' do banco de dados do balanco de potencia. Usa funcoes do pacote (\code{leitorrmpe}).
#'
#' @param pasta localizacao dos arquivos do NEWAVE
#' @param pastaSaidas localizacao dos arquivos de saida do modulo NWLISTOP
#' @param conexao conexao com o banco de dados (classe SQLiteConnection)
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A06_SAIDA_HIDRO_NEWAVE
#'
#' @examples
#' \dontrun{
#' gravaSaidasNewaveBDBP("C:/PDE2027_Caso080", "C:/PDE2027_Caso080/nwlistop", conexao, 1, 80, 1)}
#'
#' @export
gravaSaidasNewaveBDBP <- function(pasta, pastaSaidas, conexao, tipoCaso, numeroCaso, codModelo) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
  }
  if (missing(pastaSaidas)) {
    stop("favor indicar a pasta com os arquivos de sa\u00EDda do NEWAVE")
  }
  if (missing(conexao)) {
    stop("favor indicar a conex\u00E3o com o banco de dados")
  }
  if (missing(tipoCaso)) {
    stop("favor indicar tipo do caso")
  }
  if (missing(numeroCaso)) {
    stop("favor indicar o n\u00FAmero do caso")
  }
  if (missing(codModelo)) {
    stop("favor indicar o c\u00F3digo do modelo")
  }
  
  # limpa BPO_A06_SAIDA_HIDRO_NEWAVE de dados iguais de execucoes anteriores
  dbExecute(conexao, paste0("DELETE FROM BPO_A06_SAIDA_HIDRO_NEWAVE
                             WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo))
  
  df.ree = dbGetQuery(conexao, paste0("SELECT A02_NR_REE, A02_TP_CALC_POTENCIA 
                                         FROM BPO_A02_REES 
                                         WHERE A01_TP_CASO = ", tipoCaso, 
                                       " AND A01_NR_CASO = ", numeroCaso, 
                                       " AND A01_CD_MODELO = ", codModelo, 
                                       " ORDER BY A02_NR_REE"))
  
  # Carrega arquivos
  df.energiaArmazenadaFinal <- leituraEnergiaArmazenadaFinalPercentual(pastaSaidas)
  df.geracaoHidroTotal <- leituraGeracaoHidroTotal(pastaSaidas)
  df.vertimentoTurbinavel <- leituraVertimentoFioDaguaTurbinavel(pastaSaidas)

  # calculo de potencia tipo 2: nao modula - considera a geracao hidraulica na ponta resultante da simulacao
  df.dadosUsinasCalculoTipo2 <- full_join(df.energiaArmazenadaFinal, 
                                           filter(df.geracaoHidroTotal, patamar == 1), 
                                           by = c("codREE", "serie", "anoMes")) %>% 
    full_join(df.vertimentoTurbinavel, by = c("codREE", "serie", "anoMes")) %>%
    inner_join(df.ree, by = c("codREE" = "A02_NR_REE")) %>%
    filter(A02_TP_CALC_POTENCIA == 2) %>% 
    inner_join(leituraDadosDuracaoPatamar(pasta), by = c("anoMes", "patamar")) %>%
    # calcula a geracao proporcional ao patamar de carga
    mutate(geracao = geracao / duracaoPatamar, 
           earmfp = ifelse(is.na(earmfp), 0, earmfp / 100)) %>% 
    select(codREE, anoMes, serie, earmfp, geracao, vertimento)
  
  # calculo de potencia tipo 1: modula a geracao hidro pela media (saida modelo de simulacao) para atender x horas de ponta
  # calculo de potencia tipo 3: nao modula - considera a geracao hidraulica media resultante da simulacao
  df.energiaArmazenadaFinal13 <- df.energiaArmazenadaFinal %>% inner_join(df.ree, by = c("codREE" = "A02_NR_REE")) %>%
    filter(A02_TP_CALC_POTENCIA != 2) %>% select(-A02_TP_CALC_POTENCIA)
  
  df.vertimentoTurbinavel13 <- df.vertimentoTurbinavel %>% inner_join(df.ree, by = c("codREE" = "A02_NR_REE")) %>%
    filter(A02_TP_CALC_POTENCIA != 2) %>% select(-A02_TP_CALC_POTENCIA)
  
  df.dadosUsinasCalculoDemaisTipos <- df.geracaoHidroTotal %>% 
    inner_join(df.ree, by = c("codREE" = "A02_NR_REE")) %>%
    filter(A02_TP_CALC_POTENCIA != 2) %>%
    group_by(codREE, serie, anoMes) %>% 
    summarise(geracao = sum(geracao)) %>% ungroup() %>% 
    full_join(df.energiaArmazenadaFinal13, by = c("codREE", "serie", "anoMes")) %>%
    full_join(df.vertimentoTurbinavel13, by = c("codREE", "serie", "anoMes")) %>%
    select(codREE, anoMes, serie, earmfp, geracao, vertimento)
  
  df.dadosUsinas <- rbind(df.dadosUsinasCalculoTipo2, df.dadosUsinasCalculoDemaisTipos) %>% 
    mutate(tipoCaso = tipoCaso, numeroCaso = numeroCaso, codModelo = codModelo, submotorizacao = 0) %>% 
    mutate(earmfp = ifelse(is.na(earmfp), 0, earmfp / 100)) %>% 
    select(A01_TP_CASO = tipoCaso, 
           A01_NR_CASO = numeroCaso, 
           A01_CD_MODELO = codModelo, 
           A02_NR_REE = codREE, 
           A06_NR_MES = anoMes, 
           A06_NR_SERIE = serie, 
           A06_VL_PERC_ARMAZENAMENTO = earmfp, 
           A06_VL_GERACAO_HIDRAULICA = geracao, 
           A06_VL_SUBMOTORIZACAO = submotorizacao,
           A06_VL_VERTIMENTO_TURBINAVEL = vertimento)

  dbWriteTable(conexao, "BPO_A06_SAIDA_HIDRO_NEWAVE", df.dadosUsinas, append = TRUE)
  
  mensagem <- "tabela BPO_A03_DADOS_UHE gravada com sucesso!"
  
  return(mensagem)
}