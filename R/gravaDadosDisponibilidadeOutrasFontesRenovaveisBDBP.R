#' Gravacao dos dados de disponilidade das outras fontes renovaveis
#'
#' Faz a gravacao dos dados de disponibilidade das outras fontes renovaveis do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP. Alem disso, grava as tabelas de apoio BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' @param anoMesInicioMDI caracter com o ano e mes do inicio da simulacao do MDI.
#' @param anoMesFimMDI caracter com o ano e mes do final da simulacao do MDI.
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao nas tabelas BPO_A13_DISPONIBILIDADE_OFR, 
#' BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR
#' 
#' @examples
#' \dontrun{
#' gravacaoDadosDisponibilidadeOutrasFontesBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1, 201901, 203312)}
#'
#' @export
gravacaoDadosDisponibilidadeOutrasFontesBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, anoMesInicioMDI, anoMesFimMDI) {
  if (missing(pastaCaso)) {
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
  if (missing(anoMesInicioMDI)) {
    stop("favor indicar a data do inicio do caso simulado")
  }
  if (missing(anoMesFimMDI)) {
    stop("favor indicar a data do fim do caso simulado")
  } 
  
  ## leitura da planilha GeraPeq
  planilhaPequenas <- list.files(path = pastaCaso, pattern = "^GeraPeq")
  if (length(planilhaPequenas) != 1) {
    dbDisconnect(conexao)
    stop("Planilha de pequenas n\u00E3o encontrada ou multiplos arquivos com nome GeraPeq em ", pastaCaso)
  }
  # verifica se o excel possui a aba correta
  abasExcelPequenas <- "Principal"
  abasExcelPequenasLidos <- excel_sheets(paste(pastaCaso, planilhaPequenas, sep = "/"))
  abasExistentes <- setdiff(abasExcelPequenas,abasExcelPequenasLidos)
  if(length(abasExistentes != 0)) {
    dbDisconnect(conexao)
    stop(paste0("arquivo ", planilhaPequenas, " n\u00E3o possui a aba de nome Principal!"))
  }
  
  
  # leitura do excel com informacao das usinas contratadas
  df.renovaveis <- read_xlsx(path = paste(pastaCaso, planilhaPequenas, sep = "/"), sheet = "Principal", range = cell_cols("B:S"), 
                              col_types = c("text", "text", "numeric", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  # trata nome de colunas com acentos
  codificacao <- Encoding(colnames(df.renovaveis)) %>% .[. != "unknown"] %>% unique()
  if (length(codificacao) != 1) {
    dbDisconnect(conexao)
    stop("Problema de codifica\u00E7\u00E3o (UTF-8) na Planilha de pequenas. Uma solu\u00E7\u00E3o \u00E9 remover os acentos dos nomes das colunas.")
  }
  colnames(df.renovaveis) <- iconv(colnames(df.renovaveis), from = codificacao, to = "ASCII//TRANSLIT")

  # garante que nenhuma informacao indesejada de fontes indicativas esteja na tabela
  df.renovaveis <- df.renovaveis %>% filter(!str_detect(NOME, "Indicativa")) %>% 
    mutate(`Data entrada` = format(`Data entrada`, "%Y-%m-%d")) %>% 
    # converte os fatores das renovaveis em energia
    mutate_at(vars(matches("FSAZ")), ~(. * `POT (MW)` * `FCMedio (%)`)) %>% select(-`FCMedio (%)`, -NOME) 
  
  
  ## dados de outras renovaveis
  arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
  # verifica exitencia do excel
  if (!file.exists(arquivoDadosOFR)) {
    dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel n\u00E3o encontrado em ", pastaCaso))
  }
  # verifica se o excel possui as abas corretas
  abasExcelDadosOFR <- c("FatorPonta", "RelacaoIndicativas", "SazonalidadeIndicativas", "TipoContribuicaoPonta")
  abasExcelDadosOFRLidos <- excel_sheets(arquivoDadosOFR)
  abasExistentes <- setdiff(abasExcelDadosOFR,abasExcelDadosOFRLidos)
  if(length(abasExistentes != 0)) {
    dbDisconnect(conexao)
    stop(paste0("arquivo ", arquivoDadosOFR, " n\u00E3o possui a(s) aba(s) ", paste(abasExistentes, collapse = ", "), 
                " ou h\u00E1 problema com o(s) nome(s) da(s) aba(s)!"))
  }
  
  # BPO_A18_TIPOS_OFR
  # limpa a tabela de uma eventual rodada anterior
  query <- "SELECT COUNT(*) AS TOTAL FROM BPO_A18_TIPOS_OFR"
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- "DELETE FROM BPO_A18_TIPOS_OFR"
    dbExecute(conexao, query)
  }

  df.tipoContribuicaoPonta <- read_xlsx(arquivoDadosOFR, sheet = "TipoContribuicaoPonta")
  
  # salva BPO_A18_TIPOS_OFR
  dbWriteTable(conexao, "BPO_A18_TIPOS_OFR", df.tipoContribuicaoPonta, append = T)
  
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
  
  df.fatorPontaOFR <- read_xlsx(arquivoDadosOFR, sheet = "FatorPonta") %>% 
    mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo)
  
  # salva BPO_A19_FATOR_PONTA_OFR
  dbWriteTable(conexao, "BPO_A19_FATOR_PONTA_OFR", df.fatorPontaOFR, append = T)
  
  # processa informacoes das indicativas para incluir no arquivo de renovaveis se for PDE
  if (tipoCaso == 1) {
    # tabela com de/para dos nomes das indicativas
    df.relacaoIndicativas <- read_xlsx(path = arquivoDadosOFR, sheet = "RelacaoIndicativas",  
                                        col_types = c("text", "text", "text", "numeric" ))
    
    # tabela com as sazonalidades das indicativas
    df.sazonalidadeIndicativas <- read_xlsx(path = arquivoDadosOFR, sheet = "SazonalidadeIndicativas",  
                                             col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                           "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    
    # trata nome de colunas com acentos
    codificacao <- Encoding(colnames(df.sazonalidadeIndicativas)) %>% .[. != "unknown"] %>% unique()
    if (length(codificacao) != 1) {
      dbDisconnect(conexao)
      stop("Problema de codifica\u00E7\u00E3o (UTF-8) na Planilha ", arquivoDadosOFR,  
           " na aba SazonalidadeIndicativas. Uma solu\u00E7\u00E3o \u00E9 remover os acentos dos nomes das colunas.")
    }
    colnames(df.sazonalidadeIndicativas) <- iconv(colnames(df.sazonalidadeIndicativas), from = codificacao, to = "ASCII//TRANSLIT")
    
    arquivoExpansao <- list.files(path = pastaCaso, pattern = "saidaExpansao.txt")
    if (length(arquivoExpansao) != 1) {
      dbDisconnect(conexao)
      stop("Arquivo texto saidaExpansao n\u00E3o encontrado ou multiplos arquivos com nome saidaExpansao em ", pastaCaso)
    }
    
    # le arquivo com as expansoes do MDI - trata ";" no fim para nao ter avisos
    df.expansao <- read_lines(paste(pastaCaso, arquivoExpansao, sep = "/"), locale = locale(encoding = "latin1")) %>% 
      str_remove(";$") %>% 
      read_delim(guess_max = 10, 
                 delim = ";", 
                 col_types = cols()) %>% 
      select(df.relacaoIndicativas$NomeFonteMDI) # filtra somente as indicativas (as fontes estao em colunas)
    
    # cria vetor com todos os meses no horizonte do MDI e adiciona na df.expansao
    quantidadeMesesHorizonte <- ((anoMesFimMDI %/% 100) * 12 + anoMesFimMDI %% 100) - 
      ((anoMesInicioMDI %/% 100) * 12 + anoMesInicioMDI %% 100)
    horizonte <- ((zoo::as.yearmon(as.character(anoMesInicioMDI), "%Y%m")) + seq(0, (quantidadeMesesHorizonte/12), (1/12))) %>% 
      format("%Y%m") %>% as.integer()
    df.expansao <- df.expansao %>% mutate("Data entrada" = horizonte) %>% select("Data entrada", everything())
    
    # calcula os deltas da expansao (incremento por mes)
    df.expansao <- df.expansao %>% mutate_at(vars(-matches("Data entrada")), ~(. - lag(.))) %>% 
      filter_at(vars(-matches("Data entrada")), any_vars(. != 0)) %>% 
      gather(key = "NomeFonteMDI", value = "POT (MW)", -1) %>% 
      filter(`POT (MW)` != 0)
    df.expansao <- inner_join(df.expansao, df.relacaoIndicativas, by = "NomeFonteMDI") %>% select(-NomeFonteMDI)
    df.expansao <- inner_join(df.expansao, df.sazonalidadeIndicativas, by = c("NomeFonte", "Subsistema")) %>% 
      select(TIPO = TipoFonte, sistema = Subsistema, "Data entrada", `POT (MW)`, `FCMedio (%)`, everything()) %>% 
      # converte os fatores das renovaveis em energia
      mutate_at(vars(matches("FSAZ")), ~(. * `POT (MW)` * `FCMedio (%)`)) %>% select(-`FCMedio (%)`, -NomeFonte) %>% 
      mutate(`Data entrada` = paste(str_sub(`Data entrada`, 1, 4), str_sub(`Data entrada`, 5, 6), "01", sep = "-"))
    
    df.dadosOFR <- rbind(df.renovaveis, df.expansao)
  } else {
    df.dadosOFR <- df.renovaveis
  }
  
  df.dadosOFR <- df.dadosOFR %>% 
    mutate(dataRef = as.integer(substr(as.character(`Data entrada`), 0, 4))*100 + as.integer(substr(as.character(`Data entrada`), 6, 7)), aux = 1) %>% 
    select(-c("Data entrada"))
  
  # define data frame com todos os periodos do horizonte de simulacao, no formato anoMes (AAAAMM)
  df.periodo <- definePeriodo(pastaCaso) %>% select(anoMes) %>% mutate(aux = 1)
  
  # define data frame com os dados de profundidade do patamar de ponta (patamar == 1) das usinas nao simuladas
  df.profundidadeOFR <- leituraDadosPatamarUsinasNaoSimuladas(pastaCaso) %>% filter(patamar == 1)
    
  # mutate(tipofonte = fontes[codBlocoUNS,2]) %>% 
  
  # define data frame com os dados de capacidade instalada das usinas nao simuladas, para cada periodo do horizonte de simulacao
  df.capacidadeOFR <- df.dadosOFR %>% 
    group_by(sistema, TIPO, dataRef) %>% 
    summarise(POT = sum(`POT (MW)`)) %>% 
    mutate(aux = 1) %>% 
    inner_join(df.periodo, by = "aux") %>% 
    select(-aux) %>% 
    mutate(valpot = ifelse((anoMes >= dataRef), "sim", "nao")) %>% 
    filter(valpot != "nao") %>% 
    select(-valpot, -dataRef) %>% 
    group_by(sistema, TIPO, anoMes) %>% 
    summarise(Potencia = sum(POT)) %>% 
    ungroup()
  
  # define data frame com os dados de energia das usinas nao simuladas, para cada periodo do horizonte de simulacao
  df.energiaOFR <- df.dadosOFR %>% 
    select(-aux, -`POT (MW)`) %>% 
    group_by(sistema, TIPO, dataRef) %>% 
    summarise_all(sum) %>% 
    mutate(aux = 1) %>% 
    inner_join(df.periodo, by = "aux") %>% 
    select(-aux) %>% 
    mutate(valpot = ifelse((anoMes >= dataRef),"sim", "nao")) %>% 
    filter(valpot != "nao") %>% 
    select(-valpot, -dataRef) %>% 
    group_by(sistema, TIPO, anoMes) %>% 
    summarise_all(sum) %>% 
    ungroup() %>% 
    pivot_longer(-c(sistema, TIPO, anoMes), names_to = "sazonalidade", values_to = "Energia") %>% 
    filter(str_sub(anoMes, 5, 6) == str_sub(sazonalidade, 5, 6)) %>% 
    select(-sazonalidade)
  
  # define data frame com o calculo final da disponibilidade de ponta das usinas nao simuladas, 
  # para cada periodo do horizonte de simulacao
  # avalia o tipo de contribuicao da ponta atribuido para cada a OFR: 
  # se (A18_TP_CONTRIBUICAO_PONTA == 2), a disponibilidade sera igual a Energia x profundidadeUNS
  # caso contrario, a disponibilidade sera igual a Potencia Instalada x Fator de contribuicao (definido na tabela BPO_A19_FATOR_PONTA_OFR)
  df.disponibilidadeOFR <- inner_join(df.capacidadeOFR, df.energiaOFR, by = c("sistema", "TIPO", "anoMes")) %>% 
    inner_join(df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
    left_join(df.profundidadeOFR, by = c("sistema" = "codSubsistema", "A18_CD_TIPO_FONTE" = "codBlocoUNS", "anoMes" = "anoMes")) %>% 
    # inner_join(tipoOFR, by = c("TIPO" = "fontes")) %>% 
    mutate(mes = as.integer(substr(as.character(anoMes), 5, 6))) %>% 
    left_join(df.fatorPontaOFR, by = c("sistema" = "A02_NR_SUBSISTEMA", "A18_CD_TIPO_FONTE" = "A18_CD_TIPO_FONTE", "mes" = "A19_NR_MES")) %>% 
    # inner_join(fontes,  by = c("TIPO" = "nome_fonte")) %>% 
    mutate(disponibilidade = ifelse(A18_TP_CONTRIBUICAO_PONTA == 2, 
                                    Energia * ifelse(is.na(patamar), 
                                                     1, 
                                                     profundidadeUNS), 
                                    Potencia * A19_VL_FATOR)) %>% 
    mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
    # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
    select(A01_TP_CASO, 
           A01_NR_CASO, 
           A01_CD_MODELO, 
           A02_NR_SUBSISTEMA = sistema, 
           A13_NR_MES = anoMes, 
           A13_CD_TIPO_FONTE = A18_CD_TIPO_FONTE,
           A13_VL_DISPONIBILIDADE_MAXIMA_PONTA = disponibilidade)

  # executa query para apagar da tabela BPO_A13_DISPONIBILIDADE_OFR os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, 
                  " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
  apagar <- dbGetQuery(conexao, query) %>% pull()
  if (apagar > 0) {
    query <- paste0("DELETE FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
    dbExecute(conexao, query)
  }
  
  # executa query para gravar os dados de disponilidade das outras fontes renovaveis na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP
  dbWriteTable(conexao, "BPO_A13_DISPONIBILIDADE_OFR", df.disponibilidadeOFR, append = TRUE)
  
  mensagem <- "tabelas BPO_A13_DISPONIBILIDADE_OFR, BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR gravadas com sucesso!"
  
  return(mensagem)
}