#' Gravacao dos dados de reserva operativa
#'
#' Faz a gravacao dos dados de de reserva operativa no banco de dados do Balanco de Potencia (BDBP).
#' Os dados sao gravados na tabela BPO_A21_RESERVA do BDBP. Consulta as tabelas BPO_A13_DISPONIBILIDADE_OFR e BPO_A10_DEMANDA.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE e auxliares do BP.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' @param df.energiaOFR data frame com a energia esperada mensal das renovaveis
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A21_RESERVA
#' 
#' @examples
#' \dontrun{
#' gravacaoDadosReservaBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1, df.energiaOFR)
#' }
#'
#' @export
gravacaoDadosReservaBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, df.energiaOFR) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do BP")
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
  
  ## dados de reserva
  arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
  # verifica exitencia do excel
  if (!file.exists(arquivoDadosOFR)) {
    DBI::dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel n\u00E3o encontrado em ", pastaCaso))
  }
  # verifica se o excel possui as abas corretas
  abasExcelDadosOFR <- c("ReservaRenovavel", "Reserva")
  abasExcelDadosOFRLidos <- readxl::excel_sheets(arquivoDadosOFR)
  abasExistentes <- dplyr::setdiff(abasExcelDadosOFR, abasExcelDadosOFRLidos)
  if(length(abasExistentes) != 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " n\u00E3o possui a(s) aba(s) ", paste(abasExistentes, collapse = ", "), 
                " ou h\u00E1 problema com o(s) nome(s) da(s) aba(s)!"))
  }
  
  # reserva em relacao a carga
  df.reserva <- readxl::read_xlsx(arquivoDadosOFR, sheet = "Reserva")
  cabecalho <- c('subsistema', 'janeiro', 'fevereiro', 'mar\u00E7o', 'abril', 'maio', 'junho', 'julho', 
                 'agosto', 'setembro', 'outubro', 'novembro', 'dezembro')
  if(!identical(stringr::str_to_lower(colnames(df.reserva)), cabecalho)) {
    DBI::dbDisconnect(conexao)
    stop(paste0("aba Reserva do arquivo ", arquivoDadosOFR, " n\u00E3o possui o cabe\u00E7alho correto: ", paste(cabecalho, collapse = "| ")))
  }
  # acerta nome das colunas e transforma meses das colunas para variavel mes
  colnames(df.reserva) <- c("subsistema", 1:12) 
  df.reserva <- tidyr::pivot_longer(df.reserva, cols = -subsistema, names_to = "mes", values_to = "reservaDemanda") %>% 
    dplyr::mutate(mes = as.integer(mes))
  
  # demanda
  query <- paste0("SELECT 
                    A02_NR_SUBSISTEMA AS subsistema, 
                    A10_NR_MES AS anoMes,  
                    A10_NR_TIPO_DEMANDA as id, 
                    A10_VL_DEMANDA as demanda 
                  FROM 
                    BPO_A10_DEMANDA 
                  WHERE 
                    A01_TP_CASO = ", tipoCaso, " AND 
                    A01_NR_CASO = ", numeroCaso, " AND 
                    A01_CD_MODELO = ", codModelo)
  df.demanda <- DBI::dbGetQuery(conexao, query)
  df.demanda <- df.demanda %>% dplyr::mutate(mes = anoMes %% 100)
  
  # junta com tabela de demanda para calcular reserva
  df.reserva <- dplyr::inner_join(df.demanda, df.reserva, by = c("subsistema", "mes")) %>% 
    dplyr::mutate(reservaDemanda = reservaDemanda * demanda)
  
  
  # reserva em relacao as renovaveis
  df.reservaRenovavel <- readxl::read_xlsx(arquivoDadosOFR, sheet = "ReservaRenovavel")
  cabecalho <- c('a18_cd_tipo_fonte', 'subsistema', 'janeiro', 'fevereiro', 'mar\u00E7o', 'abril', 'maio', 'junho', 'julho', 
                 'agosto', 'setembro', 'outubro', 'novembro', 'dezembro')
  if(!identical(stringr::str_to_lower(colnames(df.reservaRenovavel)), cabecalho)) {
    DBI::dbDisconnect(conexao)
    stop(paste0("aba ReservaRenovavel do arquivo ", arquivoDadosOFR, " n\u00E3o possui o cabe\u00E7alho correto: ", paste(cabecalho, collapse = "| ")))
  }
  # acerta nome das colunas e transforma meses das colunas para variavel mes
  colnames(df.reservaRenovavel) <- c("tipoFonte", "subsistema", 1:12) 
  df.reservaRenovavel <- tidyr::pivot_longer(df.reservaRenovavel, cols = c(-subsistema, -tipoFonte), names_to = "mes", values_to = "reservaRenovavel") %>% 
    dplyr::mutate(mes = as.integer(mes))
  
  # junta com tabela de energia das renovaveis para calcular reserva
  df.energiaOFR <- df.energiaOFR %>% dplyr::mutate(mes = anoMes %% 100)
  df.reservaRenovavel <- dplyr::inner_join(df.reservaRenovavel, df.energiaOFR, by = c("tipoFonte", "subsistema", "mes")) %>% 
    dplyr::mutate(reservaRenovavel = reservaRenovavel * energia) %>% 
    dplyr::group_by(subsistema, anoMes) %>% 
    dplyr::summarise(reservaRenovavel = sum(reservaRenovavel), .groups = "drop")
  
  # junta reservas para criar formato a ser inserido no BD
  df.reserva <- dplyr::left_join(df.reserva, df.reservaRenovavel, by = c("subsistema", "anoMes")) %>% 
    dplyr::mutate(reservaRenovavel = ifelse(is.na(reservaRenovavel), 
                                            0,
                                            reservaRenovavel)) %>% 
    dplyr::select(A02_NR_SUBSISTEMA = subsistema,
                  A21_NR_MES = anoMes, 
                  A10_NR_TIPO_DEMANDA = id, 
                  A21_VL_RESERVA_CARGA = reservaDemanda, 
                  A21_VL_RESERVA_FONTES = reservaRenovavel) %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo)
  
  # limpa a tabela de uma eventual rodada anterior
  query <- paste0("SELECT COUNT(*) AS TOTAL 
                   FROM 
                    BPO_A21_RESERVA 
                   WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, 
                  " AND A01_CD_MODELO = ", codModelo)
  apagar <- DBI::dbGetQuery(conexao, query) %>% dplyr::pull()
  
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A21_RESERVA 
                     WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, 
                    " AND A01_CD_MODELO = ", codModelo)
    DBI::dbExecute(conexao, query)
  }
  
  # salva BPO_A21_RESERVA
  DBI::dbWriteTable(conexao, "BPO_A21_RESERVA", df.reserva, append = T)
  
  mensagem <- "tabela BPO_A21_RESERVA gravada com sucesso!"
  
  return(mensagem)
}
