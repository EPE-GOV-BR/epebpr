#' Faz a leitura da tabela com dados dos casos 
#'
#' Faz a leitura da tabela com dados dos casos (BPO_A01_CASOS_ANALISE)
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#'
#' @return \code{df.dadosCaso} data frame com dados dos casos (BPO_A01_CASOS_ANALISE)
#'
#' @export
leituraTabelaDadosCasos <- function(baseSQLite) {
  conexaoSQLite <- dbConnect(RSQLite::SQLite(), baseSQLite)
  sql <- "SELECT A01_TP_CASO as tipoCaso, 
                 A01_NR_CASO as numCaso,
                 A01_CD_MODELO as codModelo
          FROM BPO_A01_CASOS_ANALISE"
  df.dadosCaso <- dbGetQuery(conexaoSQLite, sql)
  # fecha conexao com a base SQLite
  dbDisconnect(conexaoSQLite)
  
  # df auxiliares
  df.tipoCaso <- data.frame(tipoCaso = c(1,2,3), descTipoCaso = c("PDE", "PMO", "GF"))
  df.modelo <- data.frame(codModelo = c(1,2), descModelo = c("NEWAVE", "SUISHI"))
  
  # monta estrutura para input
  df.dadosCaso <- inner_join(df.dadosCaso, df.tipoCaso, by = "tipoCaso")
  df.dadosCaso <- inner_join(df.dadosCaso, df.modelo, by = "codModelo")
  df.dadosCaso <- df.dadosCaso %>% mutate(caso = paste(tipoCaso, numCaso, codModelo, sep = ";"), 
                                          descricao = paste(numCaso, descTipoCaso, descModelo, sep = " - ")) %>% 
    select(caso, descricao)
  return(df.dadosCaso)
}