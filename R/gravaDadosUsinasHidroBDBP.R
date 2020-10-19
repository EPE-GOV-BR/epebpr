#' Grava dados das usinas hidraulicas no banco de dados do balanco de potencia
#'
#' Grava dados das usinas hidraulicas originalmente nos arquivos confhd.\* e hidr.\* na tabela BPO_A03_DADOS_UHE do banco de dados do balanco de potencia
#' Usa funcoes do pacote (\code{leitorrmpe}).
#'
#' @param pasta localizacao dos arquivos do NEWAVE
#' @param conexao conexao com o banco de dados (classe SQLiteConnection)
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A03_DADOS_UHE
#'
#' @examples
#' \dontrun{
#' gravaDadosUsinasHidroBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)}
#'
#' @export
gravaDadosUsinasHidroBDBP <- function(pasta, conexao, tipoCaso, numeroCaso, codModelo) {
  if (missing(pasta)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
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
  
  # limpa BPO_A03_DADOS_UHE de dados iguais de execucoes anteriores
  dbExecute(conexao, paste0("DELETE FROM BPO_A03_DADOS_UHE
                              WHERE A01_TP_CASO = ", tipoCaso, 
                            " AND A01_NR_CASO = ", numeroCaso, 
                            " AND A01_CD_MODELO = ", codModelo))
  
  # Carrega arquivos
  lt.dadosUsinasHidro <- leituraDadosUsinasHidro(pasta) 
  df.dadosUsinasHidroeletricas <- lt.dadosUsinasHidro %>% use_series(df.dadosUsinasHidroeletricas)
  # para o balanco de potencia somente se usa o primeiro polinomio de vazao de nivel de jusante
  df.polinomiosVazaoNivelJusante <- lt.dadosUsinasHidro %>% use_series(df.polinomiosVazaoNivelJusante) %>% filter(polinomio == 1) %>% 
    select(-nomeUsina, -codSubsistema)
  df.configuracaoHidro <- leituraConfiguracaoHidro(pasta) %>% select(codUsina, codREE, idUsinaExistente)
  # une dados em um unico data frame
  df.dadosUsinasHidro <- inner_join(df.dadosUsinasHidroeletricas, df.polinomiosVazaoNivelJusante, by = c("codUsina"))
  df.dadosUsinasHidro <- inner_join(df.dadosUsinasHidro, df.configuracaoHidro, by = c("codUsina"))
  
  # ajusta dados de acordo com a tabela BPO_A03_DADOS_UHE
  df.dadosUsinasHidro <- df.dadosUsinasHidro %>% mutate(tipoCaso = tipoCaso, 
                                                        numeroCaso = numeroCaso, 
                                                        codModelo = codModelo, 
                                                        # passa regulacao para codigo numerico
                                                        regulacao = ifelse(regulacao == "M", 77, ifelse(regulacao == "D", 68, 83))) %>% 
    select(A01_TP_CASO = tipoCaso, 
           A01_NR_CASO = numeroCaso, 
           A01_CD_MODELO = codModelo, 
           A03_CD_USINA = codUsina, 
           A02_NR_REE = codREE, 
           A03_TX_USINA = nomeUsina, 
           A03_TX_STATUS = idUsinaExistente, 
           A03_CD_TIPO = regulacao, 
           A03_NR_PCV_0 = poliCotaVolumeA0, 
           A03_NR_PCV_1 = poliCotaVolumeA1, 
           A03_NR_PCV_2 = poliCotaVolumeA2, 
           A03_NR_PCV_3 = poliCotaVolumeA3, 
           A03_NR_PCV_4 = poliCotaVolumeA4, 
           A03_NR_PVNJ_0 = coeficienteA0, 
           A03_NR_PVNJ_1 = coeficienteA1, 
           A03_NR_PVNJ_2 = coeficienteA2, 
           A03_NR_PVNJ_3 = coeficienteA3, 
           A03_NR_PVNJ_4 = coeficienteA4, 
           A03_TP_PERDA = tipoPerda, 
           A03_VL_PERDA = perda, 
           A03_TP_TURBINA = tipoTurbina,
           A03_VL_PRODUTIBILIDADE = produtibilidade) %>% 
    # remove usinas ficticias
    filter((!str_detect(A03_TX_USINA, "FIC ") & !str_detect(A03_TX_USINA, "FICT")))

  dbWriteTable(conexao, "BPO_A03_DADOS_UHE", df.dadosUsinasHidro, append = T)
  
  mensagem <- "tabela BPO_A03_DADOS_UHE gravada com sucesso!"
  
  return(mensagem)
}