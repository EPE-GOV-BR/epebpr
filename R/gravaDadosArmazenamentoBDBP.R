#' Gravacao dos dados de disponilidade das fontes de tecnologia de armazenamento
#'
#' Faz a gravacao dos dados das fontes de tecnologia de armazenamento no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE e auxliares do BP.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' @param anoMesInicioMDI caracter com o ano e mes do inicio da simulacao do MDI.
#' @param anoMesFimMDI caracter com o ano e mes do final da simulacao do MDI.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO
#' 
#' @examples
#' \dontrun{
#' gravacaoDadosArmazenamentoBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1, 201901, 203312)}
#'
#' @export
gravacaoDadosArmazenamentoBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, anoMesInicioMDI, anoMesFimMDI) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do BP")
  }
  if (missing(conexao)) {
    stop("favor indicar a conexão com o banco de dados")
  }
  if (missing(tipoCaso)) {
    stop("favor indicar tipo do caso")
  }
  if (missing(numeroCaso)) {
    stop("favor indicar o número do caso")
  }
  if (missing(codModelo)) {
    stop("favor indicar o código do modelo")
  }
  if (missing(anoMesInicioMDI)) {
    stop("favor indicar a data do inicio do caso simulado")
  }
  if (missing(anoMesFimMDI)) {
    stop("favor indicar a data do fim do caso simulado")
  }
  
  # executa query para apagar da tabela BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  DBI::dbSendQuery(conexao, paste0("DELETE FROM BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO
                                WHERE A01_TP_CASO = ", tipoCaso, 
                                " AND A01_NR_CASO = ", numeroCaso, 
                                " AND A01_CD_MODELO = ", codModelo))
  
  ## dados de outras renovaveis
  arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
  # verifica exitencia do excel
  if (!file.exists(arquivoDadosOFR)) {
    DBI::dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel não encontrado em ", pastaCaso))
  }
  # verifica se o excel possui a aba Armazenamento
  abasExcelDadosOFR <- c("Armazenamento")
  abasExcelDadosOFRLidos <- readxl::excel_sheets(arquivoDadosOFR)
  abasExistentes <- dplyr::setdiff(abasExcelDadosOFR,abasExcelDadosOFRLidos)
  if(length(abasExistentes) != 0) {
    DBI::dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " não possui a(s) aba(s) ", paste(abasExistentes, collapse = ", "), 
                " ou h\u00E1 problema com o(s) nome(s) da(s) aba(s)!"))
  }
  
  df.contribuicaoArmazenamento <- readxl::read_xlsx(arquivoDadosOFR, sheet = "Armazenamento")
  
  arquivoExpansao <- list.files(path = pastaCaso, pattern = "saidaExpansao.txt")
  
  if (length(arquivoExpansao) != 1) {
    DBI::dbDisconnect(conexao)
    stop("Arquivo texto saidaExpansao não encontrado ou multiplos arquivos com nome saidaExpansao em ", pastaCaso)
  }
  
  # le arquivo com as expansoes do MDI - trata ";" no fim para nao ter avisos
  df.expansao <- readr::read_delim(stringi::stri_enc_toutf8(paste(pastaCaso, arquivoExpansao, sep = "/")), 
                                   delim = ";", 
                                   col_names = T, 
                                   local = readr::locale(encoding = "latin1"),
                                   show_col_types = FALSE) %>% 
    dplyr::select(1:(ncol(.) -1)) %>% 
    dplyr::select(df.contribuicaoArmazenamento$NomeFonteMDI) # filtra somente os projetos de armazenamento
  
  # cria vetor com todos os meses no horizonte do MDI e adiciona na df.expansao
  quantidadeMesesHorizonte <- ((anoMesFimMDI %/% 100) * 12 + anoMesFimMDI %% 100) - 
    ((anoMesInicioMDI %/% 100) * 12 + anoMesInicioMDI %% 100)
  
  horizonte <- ((zoo::as.yearmon(as.character(anoMesInicioMDI), "%Y%m")) + seq(0, (quantidadeMesesHorizonte/12), (1/12))) %>% 
    format("%Y%m") %>% 
    as.integer()
  
  # calcula a disponibilidade multiplicando a potencia pelo fator de contribuição
  df.dispArmazenamento <- df.expansao %>% 
    dplyr::mutate("anoMes" = horizonte) %>% 
    dplyr::select("anoMes", tidyr::everything()) %>% 
    dplyr::select(dplyr::where( ~ sum(.) > 0))
  
  # verifica se existe expansão de projetos de armazenamento no caso
  if(ncol(df.dispArmazenamento) > 1){
    df.dispArmazenamento <- df.dispArmazenamento %>% 
      tidyr::pivot_longer(!anoMes, names_to = "NomeFonteMDI", values_to = "Pot") %>% 
      dplyr::inner_join(df.contribuicaoArmazenamento, by = "NomeFonteMDI") %>%
      dplyr::mutate(A32_VL_DISPONIBILIDADE_PONTA = Pot*FatorContribuicao) %>% 
      dplyr::group_by(anoMes, Subsistema) %>% 
      dplyr::reframe(A32_VL_DISPONIBILIDADE_PONTA = sum(A32_VL_DISPONIBILIDADE_PONTA)) %>% 
      dplyr::mutate(A01_TP_CASO = tipoCaso,
                    A01_NR_CASO = numeroCaso,
                    A01_CD_MODELO = codModelo) %>% 
      dplyr::rename(A02_NR_SUBSISTEMA = Subsistema,
                    A32_NR_MES = anoMes)
    
    # executa query para gravar os dados de disponilidade das fontes de tecnologia de armazenamento na tabela BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO do BDBP
    DBI::dbWriteTable(conexao, "BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO", df.dispArmazenamento, append = TRUE)
  }
  
  mensagem <- "tabela BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO gravada com sucesso!"
  
  return(mensagem)
  
}
