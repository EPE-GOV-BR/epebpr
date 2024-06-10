#' Gravacao dos dados de disponilidade das outras fontes renovaveis
#'
#' Faz a gravacao dos dados de disponibilidade das outras fontes renovaveis do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP. Alem disso, grava as tabelas de apoio BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param tipoDemanda caracter com a definicao do tipo de demanda do caso. [1]=Deterministica [2]=Liquida
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' @param anoMesInicioMDI caracter com o ano e mes do inicio da simulacao do MDI.
#' @param anoMesFimMDI caracter com o ano e mes do final da simulacao do MDI.
#' @param tipoDemanda caracter com a definicao do tipo de demanda do caso. [1]=Deterministica [2]=Liquida
#'
#' @return \code{lista} 
#' \itemize{
#' \item \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao nas tabelas BPO_A13_DISPONIBILIDADE_OFR, 
#' BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR 
#' \item \code{df.capacidadeOFR} data frame com a capacidade instalada das renovaveis
#' }
#' 
#' @examples
#' \dontrun{
#' gravacaoDadosDisponibilidadeOutrasFontesBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1, 201901, 203312)}
#'
#' @export
gravacaoDadosDisponibilidadeOutrasFontesBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, tipoDemanda, codModelo, anoMesInicioMDI, anoMesFimMDI) {
  if (missing(pastaCaso)) {
    stop("favor indicar a pasta com os arquivos do NEWAVE")
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
  if (missing(tipoDemanda)) {
    stop("favor indicar o tipo de demanda")
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

  ## verifica se o caso é do tipo carga liquida ou bruta e faz o processamento de acordo com o caso
  if(tipoDemanda == 1){
  ###### DETERMINISTICO #####  
  
    ## leitura da planilha GeraPeq
    planilhaPequenas <- list.files(path = pastaCaso, pattern = "^GeraPeq")
    if (length(planilhaPequenas) != 1) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de pequenas não encontrada ou multiplos arquivos com nome GeraPeq em ", pastaCaso)
    }
    # verifica se o excel possui a aba correta
    abasExcelPequenas <- "Principal"
    abasExcelPequenasLidos <- readxl::excel_sheets(paste(pastaCaso, planilhaPequenas, sep = "/"))
    abasExistentes <- dplyr::setdiff(abasExcelPequenas,abasExcelPequenasLidos)
    if(length(abasExistentes != 0)) {
      DBI::dbDisconnect(conexao)
      stop(paste0("arquivo ", planilhaPequenas, " não possui a aba de nome Principal!"))
    }
    
    # leitura do excel com informacao das usinas contratadas
    df.renovaveis <- readxl::read_xlsx(path = paste(pastaCaso, planilhaPequenas, sep = "/"), sheet = "Principal", range = cellranger::cell_cols("B:S"), 
                                       col_types = c("text", "text", "numeric", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                     "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    # trata nome de colunas com acentos
    codificacao <- Encoding(colnames(df.renovaveis)) %>% .[. != "unknown"] %>% unique()
    if (length(codificacao) != 1) {
      DBI::dbDisconnect(conexao)
      stop("Problema de codificação (UTF-8) na Planilha de pequenas. Uma solução é remover os acentos dos nomes das colunas.")
    }
    colnames(df.renovaveis) <- iconv(colnames(df.renovaveis), from = codificacao, to = "ASCII//TRANSLIT")
    
    # garante que nenhuma informacao indesejada de fontes indicativas esteja na tabela
    df.renovaveis <- df.renovaveis %>% 
      dplyr::filter(!stringr::str_detect(NOME, "Indicativa")) %>% 
      dplyr::mutate(`Data entrada` = format(`Data entrada`, "%Y-%m-%d")) %>% 
      # converte os fatores das renovaveis em energia
      dplyr::mutate_at(dplyr::vars(dplyr::matches("FSAZ")), ~(. * `POT (MW)` * `FCMedio (%)`)) %>% 
      dplyr::select(-`FCMedio (%)`, -NOME)
    
    # leitura dos REES/subsistemas cadastrados para garantir que nao haja geracao em local inexistente
    reeCadastradas <- DBI::dbReadTable(conexao, "BPO_A02_SUBSISTEMAS") %>% 
      dplyr::pull(A02_NR_SUBSISTEMA)
    
    reeRenovaveis <- df.renovaveis %>% 
      dplyr::pull(sistema) %>% 
      unique()
    
    diferencaREE <- dplyr::setdiff(reeRenovaveis, reeCadastradas) %>% 
      length()
    
    if (diferencaREE != 0) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de pequenas com oferta renov\u00E1vel em subsistema/REE não cadastrado!")
    }
    
    ## dados de outras renovaveis
    arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
    # verifica exitencia do excel
    if (!file.exists(arquivoDadosOFR)) {
      DBI::dbDisconnect(conexao)
      stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel não encontrado em ", pastaCaso))
    }
    # verifica se o excel possui as abas corretas
    abasExcelDadosOFR <- c("FatorPonta", "RelacaoIndicativas", "SazonalidadeIndicativas", "TipoContribuicaoPonta")
    abasExcelDadosOFRLidos <- readxl::excel_sheets(arquivoDadosOFR)
    abasExistentes <- dplyr::setdiff(abasExcelDadosOFR,abasExcelDadosOFRLidos)
    if(length(abasExistentes) != 0) {
      DBI::dbDisconnect(conexao)
      stop(paste0("arquivo ", arquivoDadosOFR, " não possui a(s) aba(s) ", paste(abasExistentes, collapse = ", "), 
                  " ou h\u00E1 problema com o(s) nome(s) da(s) aba(s)!"))
    }
    
    # BPO_A18_TIPOS_OFR
    # limpa a tabela de uma eventual rodada anterior
    query <- "SELECT COUNT(*) AS TOTAL FROM BPO_A18_TIPOS_OFR"
    
    apagar <- DBI::dbGetQuery(conexao, query) %>% 
      dplyr::pull()
    
    if (apagar > 0) {
      query <- "DELETE FROM BPO_A18_TIPOS_OFR"
      DBI::dbExecute(conexao, query)
    }
    
    df.tipoContribuicaoPonta <- readxl::read_xlsx(arquivoDadosOFR, sheet = "TipoContribuicaoPonta")
    
    # salva BPO_A18_TIPOS_OFR
    DBI::dbWriteTable(conexao, "BPO_A18_TIPOS_OFR", df.tipoContribuicaoPonta, append = T)
    
    # BPO_A19_FATOR_PONTA_OFR
    # limpa a tabela de uma eventual rodada anterior
    query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    
    apagar <- DBI::dbGetQuery(conexao, query) %>% 
      dplyr::pull()
    
    if (apagar > 0) {
      query <- paste0("DELETE FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                      " AND A01_CD_MODELO = ", codModelo)
      DBI::dbExecute(conexao, query)
    }
    
    df.fatorPontaOFR <- readxl::read_xlsx(arquivoDadosOFR, sheet = "FatorPonta") %>% 
      dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo)
    
    # salva BPO_A19_FATOR_PONTA_OFR
    DBI::dbWriteTable(conexao, "BPO_A19_FATOR_PONTA_OFR", df.fatorPontaOFR, append = T)
    
    # processa informacoes das indicativas para incluir no arquivo de renovaveis se for PDE
    if (tipoCaso == 1) {
      # tabela com de/para dos nomes das indicativas
      df.relacaoIndicativas <- readxl::read_xlsx(path = arquivoDadosOFR, sheet = "RelacaoIndicativas",  
                                                 col_types = c("text", "text", "text", "numeric" ))
      
      # tabela com as sazonalidades das indicativas
      df.sazonalidadeIndicativas <- readxl::read_xlsx(path = arquivoDadosOFR, sheet = "SazonalidadeIndicativas",  
                                                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
      
      # trata nome de colunas com acentos
      codificacao <- Encoding(colnames(df.sazonalidadeIndicativas)) %>% .[. != "unknown"] %>% unique()
      
      if (length(codificacao) != 1) {
        DBI::dbDisconnect(conexao)
        stop("Problema de codificação (UTF-8) na Planilha ", arquivoDadosOFR,  
             " na aba SazonalidadeIndicativas. Uma solução é remover os acentos dos nomes das colunas.")
      }
      
      colnames(df.sazonalidadeIndicativas) <- iconv(colnames(df.sazonalidadeIndicativas), from = "UTF-8", to = "ASCII//TRANSLIT")
      
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
        dplyr::select(df.relacaoIndicativas$NomeFonteMDI) # filtra somente as indicativas (as fontes estao em colunas)
      
      # cria vetor com todos os meses no horizonte do MDI e adiciona na df.expansao
      quantidadeMesesHorizonte <- ((anoMesFimMDI %/% 100) * 12 + anoMesFimMDI %% 100) - 
        ((anoMesInicioMDI %/% 100) * 12 + anoMesInicioMDI %% 100)
      
      horizonte <- ((zoo::as.yearmon(as.character(anoMesInicioMDI), "%Y%m")) + seq(0, (quantidadeMesesHorizonte/12), (1/12))) %>% 
        format("%Y%m") %>% 
        as.integer()
      
      df.expansao <- df.expansao %>% 
        dplyr::mutate("Data entrada" = horizonte) %>% 
        dplyr::select("Data entrada", tidyr::everything())
      
      # calcula os deltas da expansao (incremento por mes)
      df.expansao <- df.expansao %>% 
        dplyr::mutate_at(dplyr::vars(-dplyr::matches("Data entrada")), ~(. - dplyr::lag(.))) %>% 
        dplyr::filter_at(dplyr::vars(-dplyr::matches("Data entrada")), dplyr::any_vars(. != 0)) %>% 
        tidyr::pivot_longer(!`Data entrada`, names_to = "NomeFonteMDI", values_to = "POT (MW)") %>% 
        dplyr::filter(`POT (MW)` != 0)
      
      df.expansao <- dplyr::inner_join(df.expansao, df.relacaoIndicativas, by = "NomeFonteMDI") %>% 
        dplyr::select(-NomeFonteMDI)
      
      df.expansao <- dplyr::inner_join(df.expansao, df.sazonalidadeIndicativas, by = c("NomeFonte", "Subsistema")) %>% 
        dplyr::select(TIPO = TipoFonte, sistema = Subsistema, "Data entrada", `POT (MW)`, `FCMedio (%)`, tidyr::everything()) %>% 
        # converte os fatores das renovaveis em energia
        dplyr::mutate_at(dplyr::vars(dplyr::matches("FSAZ")), ~(. * `POT (MW)` * `FCMedio (%)`)) %>% dplyr::select(-`FCMedio (%)`, -NomeFonte) %>% 
        dplyr::mutate(`Data entrada` = paste(stringr::str_sub(`Data entrada`, 1, 4), stringr::str_sub(`Data entrada`, 5, 6), "01", sep = "-"))
      
      df.dadosOFR <- rbind(df.renovaveis, df.expansao)
      
    } else {
      
      df.dadosOFR <- df.renovaveis
      
    }
    
    df.dadosOFR <- df.dadosOFR %>% 
      dplyr::mutate(dataRef = as.integer(substr(as.character(`Data entrada`), 0, 4))*100 + as.integer(substr(as.character(`Data entrada`), 6, 7)), aux = 1) %>% 
      dplyr::select(-c("Data entrada"))
    
    # define data frame com todos os periodos do horizonte de simulacao, no formato anoMes (AAAAMM)
    df.periodo <- leitorrmpe::definePeriodo(pastaCaso) %>% 
      dplyr::select(anoMes) %>% 
      dplyr::mutate(aux = 1)
    
    # define data frame com os dados de profundidade do patamar de ponta (patamar == 1) das usinas nao simuladas
    df.profundidadeOFR <- leitorrmpe::leituraDadosPatamarUsinasNaoSimuladas(pastaCaso) %>% 
      dplyr::filter(patamar == 1)
    
    # define data frame com os dados de capacidade instalada das usinas nao simuladas, para cada periodo do horizonte de simulacao
    df.capacidadeOFR <- df.dadosOFR %>% 
      dplyr::group_by(sistema, TIPO, dataRef) %>% 
      dplyr::summarise(POT = sum(`POT (MW)`), .groups = "drop") %>% 
      dplyr::mutate(aux = 1) %>% 
      dplyr::inner_join(df.periodo, by = "aux") %>% 
      dplyr::select(-aux) %>% 
      dplyr::mutate(valpot = ifelse((anoMes >= dataRef), "sim", "nao")) %>% 
      dplyr::filter(valpot != "nao") %>% 
      dplyr::select(-valpot, -dataRef) %>% 
      dplyr::group_by(sistema, TIPO, anoMes) %>% 
      dplyr::summarise(Potencia = sum(POT), .groups = "drop")
    
    # define data frame com os dados de energia das usinas nao simuladas, para cada periodo do horizonte de simulacao
    df.energiaOFR <- df.dadosOFR %>% 
      dplyr::select(-aux, -`POT (MW)`) %>% 
      dplyr::group_by(sistema, TIPO, dataRef) %>% 
      dplyr::summarise(dplyr::across(tidyr::everything(), sum), .groups = "drop") %>% 
      dplyr::mutate(aux = 1) %>% 
      dplyr::inner_join(df.periodo, by = "aux") %>% 
      dplyr::select(-aux) %>% 
      dplyr::mutate(valpot = ifelse((anoMes >= dataRef),"sim", "nao")) %>% 
      dplyr::filter(valpot != "nao") %>% 
      dplyr::select(-valpot, -dataRef) %>% 
      dplyr::group_by(sistema, TIPO, anoMes) %>% 
      dplyr::summarise(dplyr::across(tidyr::everything(), sum), .groups = "drop") %>% 
      tidyr::pivot_longer(-c(sistema, TIPO, anoMes), names_to = "sazonalidade", values_to = "Energia") %>% 
      dplyr::filter(stringr::str_sub(anoMes, 5, 6) == stringr::str_sub(sazonalidade, 5, 6)) %>% 
      dplyr::select(-sazonalidade)
    
    # define data frame com o calculo final da disponibilidade de ponta das usinas nao simuladas, 
    # para cada periodo do horizonte de simulacao
    # avalia o tipo de contribuicao da ponta atribuido para cada a OFR: 
    # se (A18_TP_CONTRIBUICAO_PONTA == 2), a disponibilidade sera igual a Energia x profundidadeUNS
    # caso contrario, a disponibilidade sera igual a Potencia Instalada x Fator de contribuicao (definido na tabela BPO_A19_FATOR_PONTA_OFR)
    df.disponibilidadeOFR <- dplyr::inner_join(df.capacidadeOFR, df.energiaOFR, by = c("sistema", "TIPO", "anoMes")) %>% 
      dplyr::inner_join(df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
      dplyr::left_join(df.profundidadeOFR, by = c("sistema" = "codSubsistema", "A18_CD_TIPO_FONTE" = "codBlocoUNS", "anoMes" = "anoMes")) %>% 
      dplyr::mutate(mes = as.integer(substr(as.character(anoMes), 5, 6))) %>% 
      dplyr::left_join(df.fatorPontaOFR, by = c("sistema" = "A02_NR_SUBSISTEMA", "A18_CD_TIPO_FONTE" = "A18_CD_TIPO_FONTE", "mes" = "A19_NR_MES")) %>% 
      dplyr::mutate(disponibilidade = ifelse(A18_TP_CONTRIBUICAO_PONTA == 2, 
                                             Energia * ifelse(is.na(patamar), 
                                                              1, 
                                                              profundidadeUNS), 
                                             Potencia * A19_VL_FATOR)) %>% 
      dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
      # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
      dplyr::select(A01_TP_CASO, 
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
    
    apagar <- DBI::dbGetQuery(conexao, query) %>% 
      dplyr::pull()
    
    if (apagar > 0) {
      
      query <- paste0("DELETE FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, 
                      " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
      
      DBI::dbExecute(conexao, query)
      
    }
    
    # executa query para gravar os dados de disponilidade das outras fontes renovaveis na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP
    DBI::dbWriteTable(conexao, "BPO_A13_DISPONIBILIDADE_OFR", df.disponibilidadeOFR, append = TRUE)
    
    mensagem <- "tabelas BPO_A13_DISPONIBILIDADE_OFR, BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR gravadas com sucesso!"
    
    # prepara energia de renovaveis para calculos de reserva
    df.energiaOFR <- dplyr::inner_join(df.energiaOFR, df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
      dplyr::select(tipoFonte = A18_CD_TIPO_FONTE, subsistema = sistema, anoMes, energia = Energia)
    
    return(list(mensagem = mensagem, df.energiaOFR = df.energiaOFR))
    
  }else{
  ######## CARGA LIQUIDA ########  
    
    ## leitura da planilha de detalhes da carga liquida maxima
    detalhesCargaLiqMax <- list.files(path = pastaCaso, pattern = "^detalhesCargaLiquida")
    if (length(detalhesCargaLiqMax) != 1) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de detalhes da carga líquida máxima não encontrada ou multiplos arquivos com nome detalhesCargaLiquida em ", pastaCaso)
    }
    
    # leitura do excel com detalhes da carga liquida máxima
    df.detalhesCLiqMax <- readxl::read_xlsx(path = paste(pastaCaso, detalhesCargaLiqMax, sep = "/"))
    
    # leitura dos REES/subsistemas cadastrados para garantir que nao haja geracao em local inexistente
    reeCadastradas <- DBI::dbReadTable(conexao, "BPO_A02_SUBSISTEMAS") %>% 
      dplyr::pull(A02_NR_SUBSISTEMA)
    
    reeDetalhes <- df.detalhesCLiqMax %>% 
      dplyr::pull(codSubsistema) %>% 
      unique()
    
    diferencaREE <- dplyr::setdiff(reeDetalhes, reeCadastradas) %>% length()
    
    if (diferencaREE != 0) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de detalhes com dados em subsistema/REE não cadastrado!")
    }
    
    ## arquivo dadosORF
    arquivoDadosOFR <- paste(pastaCaso, "dadosOFR.xlsx", sep = "/")
    
    # verifica exitencia do excel
    if (!file.exists(arquivoDadosOFR)) {
      DBI::dbDisconnect(conexao)
      stop(paste0("arquivo ", arquivoDadosOFR, " com dados de oferta renov\u00E1vel não encontrado em ", pastaCaso))
    }
    # verifica se o excel possui as abas corretas
    abasExcelDadosOFR <- c("FatorPonta", "RelacaoIndicativas", "SazonalidadeIndicativas", "TipoContribuicaoPonta")
    abasExcelDadosOFRLidos <- readxl::excel_sheets(arquivoDadosOFR)
    abasExistentes <- dplyr::setdiff(abasExcelDadosOFR,abasExcelDadosOFRLidos)
    if(length(abasExistentes) != 0) {
      DBI::dbDisconnect(conexao)
      stop(paste0("arquivo ", arquivoDadosOFR, " não possui a(s) aba(s) ", paste(abasExistentes, collapse = ", "), 
                  " ou h\u00E1 problema com o(s) nome(s) da(s) aba(s)!"))
    }
    
    # BPO_A18_TIPOS_OFR
    # limpa a tabela de uma eventual rodada anterior
    query <- "SELECT COUNT(*) AS TOTAL FROM BPO_A18_TIPOS_OFR"
    apagar <- DBI::dbGetQuery(conexao, query) %>% 
      dplyr::pull()
    if (apagar > 0) {
      query <- "DELETE FROM BPO_A18_TIPOS_OFR"
      DBI::dbExecute(conexao, query)
    }
    
    df.tipoContribuicaoPonta <- readxl::read_xlsx(arquivoDadosOFR, sheet = "TipoContribuicaoPonta")
    
    # salva BPO_A18_TIPOS_OFR
    DBI::dbWriteTable(conexao, "BPO_A18_TIPOS_OFR", df.tipoContribuicaoPonta, append = T)
    
    # BPO_A19_FATOR_PONTA_OFR
    query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                    " AND A01_CD_MODELO = ", codModelo)
    apagar <- DBI::dbGetQuery(conexao, query) %>% dplyr::pull()
    if (apagar > 0) {
      query <- paste0("DELETE FROM BPO_A19_FATOR_PONTA_OFR WHERE A01_TP_CASO = ", tipoCaso, " AND A01_NR_CASO = ", numeroCaso,
                      " AND A01_CD_MODELO = ", codModelo)
      DBI::dbExecute(conexao, query)
    }
    
    df.fatorPontaOFR <- readxl::read_xlsx(arquivoDadosOFR, sheet = "FatorPonta") %>% 
      dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo)
    
    # salva BPO_A19_FATOR_PONTA_OFR
    DBI::dbWriteTable(conexao, "BPO_A19_FATOR_PONTA_OFR", df.fatorPontaOFR, append = T)
    
    ##### CONTRIBUIÇÃO DAS EXISTENTES É PELA PLaNILHA DE DETALHES CARGA LIQUIDA
    
    # define data frame com os dados de energia das usinas nao simuladas, para cada periodo do horizonte de simulacao
    df.energiaOFREx <- df.detalhesCLiqMax %>% 
      dplyr::select(sistema = codSubsistema, 
                    ano, 
                    mes, 
                    EOL = geracaoEol, 
                    UFV = geracaoUfv, 
                    GD_UFV = geracaoGD,
                    GD_Demais = outrasGD,
                    PCH,
                    BIO = PCT) %>% 
      dplyr::mutate(anoMes = ano*100 + mes,
                    MGD = GD_UFV + GD_Demais) %>% 
      dplyr::select(-ano, -mes, -GD_UFV, -GD_Demais) %>% 
      tidyr::pivot_longer(cols = c("EOL", "UFV", "PCH", "BIO", "MGD"), names_to = "TIPO", values_to = "Energia") %>% 
      dplyr::select(sistema, TIPO, tidyr::everything())
    
    # define data frame com a disponibilidade de ponta das usinas nao simuladas, 
    # para cada periodo do horizonte de simulacao
    # para o caso da carga liquida corresponde à contribuição das renovaveis no momento da 
    # carga liquida maxima do SIN para cada periodo
    df.disponibilidadeOFREx <- df.energiaOFREx %>% 
      dplyr::inner_join(df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>%
      dplyr::group_by(sistema, anoMes, A18_CD_TIPO_FONTE) %>% 
      dplyr::reframe(disponibilidade = sum(Energia)) %>% 
      dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
      # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
      dplyr::select(A01_TP_CASO, 
                    A01_NR_CASO, 
                    A01_CD_MODELO, 
                    A02_NR_SUBSISTEMA = sistema, 
                    A13_NR_MES = anoMes, 
                    A13_CD_TIPO_FONTE = A18_CD_TIPO_FONTE,
                    A13_VL_DISPONIBILIDADE_MAXIMA_PONTA = disponibilidade)
    
    df.energiaOFR <- df.energiaOFREx
    df.disponibilidadeOFR <- df.disponibilidadeOFREx
    
    ##### CONTRIBUIÇÃO DAS INDICATIVAS
    # processa informacoes das indicativas para incluir no arquivo de renovaveis se for PDE
    if (tipoCaso == 1) {
      # tabela com de/para dos nomes das indicativas
      df.relacaoIndicativas <- readxl::read_xlsx(path = arquivoDadosOFR, sheet = "RelacaoIndicativas",  
                                                 col_types = c("text", "text", "text", "numeric" ))
      
      # tabela com as sazonalidades das indicativas
      df.sazonalidadeIndicativas <- readxl::read_xlsx(path = arquivoDadosOFR, sheet = "SazonalidadeIndicativas",  
                                                      col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                                                    "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
      
      # trata nome de colunas com acentos
      codificacao <- Encoding(colnames(df.sazonalidadeIndicativas)) %>% .[. != "unknown"] %>% unique()
      
      if (length(codificacao) != 1) {
        DBI::dbDisconnect(conexao)
        stop("Problema de codificação (UTF-8) na Planilha ", arquivoDadosOFR,  
             " na aba SazonalidadeIndicativas. Uma solução é remover os acentos dos nomes das colunas.")
      }
      
      colnames(df.sazonalidadeIndicativas) <- iconv(colnames(df.sazonalidadeIndicativas), from = "UTF-8", to = "ASCII//TRANSLIT")
      
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
        dplyr::select(df.relacaoIndicativas$NomeFonteMDI) # filtra somente as indicativas (as fontes estao em colunas)
      
      # cria vetor com todos os meses no horizonte do MDI e adiciona na df.expansao
      quantidadeMesesHorizonte <- ((anoMesFimMDI %/% 100) * 12 + anoMesFimMDI %% 100) - 
        ((anoMesInicioMDI %/% 100) * 12 + anoMesInicioMDI %% 100)
      
      horizonte <- ((zoo::as.yearmon(as.character(anoMesInicioMDI), "%Y%m")) + seq(0, (quantidadeMesesHorizonte/12), (1/12))) %>% 
        format("%Y%m") %>% 
        as.integer()
      
      df.expansao <- df.expansao %>% 
        dplyr::mutate("Data entrada" = horizonte) %>% 
        dplyr::select("Data entrada", tidyr::everything())
      
      # calcula os deltas da expansao (incremento por mes)
      df.expansao <- df.expansao %>% 
        dplyr::mutate_at(dplyr::vars(-dplyr::matches("Data entrada")), ~(. - dplyr::lag(.))) %>% 
        dplyr::filter_at(dplyr::vars(-dplyr::matches("Data entrada")), dplyr::any_vars(. != 0)) %>% 
        tidyr::pivot_longer(!`Data entrada`, names_to = "NomeFonteMDI", values_to = "POT (MW)") %>% 
        dplyr::filter(`POT (MW)` != 0)
      
      df.expansao <- dplyr::inner_join(df.expansao, df.relacaoIndicativas, by = "NomeFonteMDI") %>% 
        dplyr::select(-NomeFonteMDI)
      
      df.expansao <- dplyr::inner_join(df.expansao, df.sazonalidadeIndicativas, by = c("NomeFonte", "Subsistema")) %>% 
        dplyr::select(TIPO = TipoFonte, sistema = Subsistema, "Data entrada", `POT (MW)`, `FCMedio (%)`, tidyr::everything()) %>% 
        # converte os fatores das renovaveis em energia
        dplyr::mutate_at(dplyr::vars(dplyr::matches("FSAZ")), ~(. * `POT (MW)` * `FCMedio (%)`)) %>% dplyr::select(-`FCMedio (%)`, -NomeFonte) %>% 
        dplyr::mutate(`Data entrada` = paste(stringr::str_sub(`Data entrada`, 1, 4), stringr::str_sub(`Data entrada`, 5, 6), "01", sep = "-"))
      
      df.dadosOFR <- df.expansao %>% 
        dplyr::mutate(dataRef = as.integer(substr(as.character(`Data entrada`), 0, 4))*100 + as.integer(substr(as.character(`Data entrada`), 6, 7)), aux = 1) %>% 
        dplyr::select(-c("Data entrada"))
      
      # define data frame com todos os periodos do horizonte de simulacao, no formato anoMes (AAAAMM)
      df.periodo <- leitorrmpe::definePeriodo(pastaCaso) %>% 
        dplyr::select(anoMes) %>% 
        dplyr::mutate(aux = 1)
      
      # define data frame com os dados de profundidade do patamar de ponta (patamar == 1) das usinas nao simuladas
      df.profundidadeOFR <- leitorrmpe::leituraDadosPatamarUsinasNaoSimuladas(pastaCaso) %>% 
        dplyr::filter(patamar == 1)
      
      # define data frame com os dados de capacidade instalada das usinas nao simuladas, para cada periodo do horizonte de simulacao
      df.capacidadeOFR <- df.dadosOFR %>% 
        dplyr::group_by(sistema, TIPO, dataRef) %>% 
        dplyr::summarise(POT = sum(`POT (MW)`), .groups = "drop") %>% 
        dplyr::mutate(aux = 1) %>% 
        dplyr::inner_join(df.periodo, by = "aux") %>% 
        dplyr::select(-aux) %>% 
        dplyr::mutate(valpot = ifelse((anoMes >= dataRef), "sim", "nao")) %>% 
        dplyr::filter(valpot != "nao") %>% 
        dplyr::select(-valpot, -dataRef) %>% 
        dplyr::group_by(sistema, TIPO, anoMes) %>% 
        dplyr::summarise(Potencia = sum(POT), .groups = "drop")
      
      # define data frame com os dados de energia das usinas nao simuladas, para cada periodo do horizonte de simulacao
      df.energiaOFRInd <- df.dadosOFR %>% 
        dplyr::select(-aux, -`POT (MW)`) %>% 
        dplyr::group_by(sistema, TIPO, dataRef) %>% 
        dplyr::summarise(dplyr::across(tidyr::everything(), sum), .groups = "drop") %>% 
        dplyr::mutate(aux = 1) %>% 
        dplyr::inner_join(df.periodo, by = "aux") %>% 
        dplyr::select(-aux) %>% 
        dplyr::mutate(valpot = ifelse((anoMes >= dataRef),"sim", "nao")) %>% 
        dplyr::filter(valpot != "nao") %>% 
        dplyr::select(-valpot, -dataRef) %>% 
        dplyr::group_by(sistema, TIPO, anoMes) %>% 
        dplyr::summarise(dplyr::across(tidyr::everything(), sum), .groups = "drop") %>% 
        tidyr::pivot_longer(-c(sistema, TIPO, anoMes), names_to = "sazonalidade", values_to = "Energia") %>% 
        dplyr::filter(stringr::str_sub(anoMes, 5, 6) == stringr::str_sub(sazonalidade, 5, 6)) %>% 
        dplyr::select(-sazonalidade)
      
      # verifica se todas as fontes definidas como tipo 1 tem dados na aba FatorPonta
      df.verificaFator <- dplyr::inner_join(df.capacidadeOFR, df.energiaOFRInd, by = c("sistema", "TIPO", "anoMes")) %>% 
        dplyr::inner_join(df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
        dplyr::mutate(mes = as.integer(substr(as.character(anoMes), 5, 6))) %>% 
        dplyr::left_join(df.fatorPontaOFR, by = c("sistema" = "A02_NR_SUBSISTEMA", "A18_CD_TIPO_FONTE" = "A18_CD_TIPO_FONTE", "mes" = "A19_NR_MES")) %>% 
        dplyr::filter(A18_TP_CONTRIBUICAO_PONTA == 1, is.na(A19_VL_FATOR)) %>% 
        dplyr::distinct(A18_CD_TIPO_FONTE, sistema)
      
      if (nrow(df.verificaFator) > 0) {
        DBI::dbDisconnect(conexao)
        stop("Não foram definidos fatores de contribuição de ponta (planilha dadosOFR, aba FatorPonta) para as seguintes fontes:\n", sprintf("A18_CD_TIPO_FONTE %s A02_NR_SUBSISTEMA %s\n", df.verificaFator$A18_CD_TIPO_FONTE, df.verificaFator$sistema))
      }
      
      # define data frame com o calculo final da disponibilidade de ponta das usinas nao simuladas, 
      # para cada periodo do horizonte de simulacao
      # avalia o tipo de contribuicao da ponta atribuido para cada a OFR: 
      # se (A18_TP_CONTRIBUICAO_PONTA == 2), a disponibilidade sera igual a Energia x profundidadeUNS
      # caso contrario, a disponibilidade sera igual a Potencia Instalada x Fator de contribuicao (definido na tabela BPO_A19_FATOR_PONTA_OFR)
      df.disponibilidadeOFRInd <- dplyr::inner_join(df.capacidadeOFR, df.energiaOFRInd, by = c("sistema", "TIPO", "anoMes")) %>% 
        dplyr::inner_join(df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
        dplyr::left_join(df.profundidadeOFR, by = c("sistema" = "codSubsistema", "A18_CD_TIPO_FONTE" = "codBlocoUNS", "anoMes" = "anoMes")) %>% 
        dplyr::mutate(mes = as.integer(substr(as.character(anoMes), 5, 6))) %>% 
        dplyr::left_join(df.fatorPontaOFR, by = c("sistema" = "A02_NR_SUBSISTEMA", "A18_CD_TIPO_FONTE" = "A18_CD_TIPO_FONTE", "mes" = "A19_NR_MES")) %>% 
        dplyr::mutate(disponibilidade = ifelse(A18_TP_CONTRIBUICAO_PONTA == 2, 
                                               Energia * ifelse(is.na(patamar), 
                                                                1, 
                                                                profundidadeUNS), 
                                               Potencia * A19_VL_FATOR)) %>% 
        dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
        # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
        dplyr::select(A01_TP_CASO, 
                      A01_NR_CASO, 
                      A01_CD_MODELO, 
                      A02_NR_SUBSISTEMA = sistema, 
                      A13_NR_MES = anoMes, 
                      A13_CD_TIPO_FONTE = A18_CD_TIPO_FONTE,
                      A13_VL_DISPONIBILIDADE_MAXIMA_PONTA = disponibilidade)
      
      if(nrow(df.energiaOFRInd) > 0){df.energiaOFR <- rbind(df.energiaOFR, df.energiaOFRInd) %>% 
        dplyr::group_by(dplyr::across(c(-Energia))) %>% 
        dplyr::reframe(Energia = sum(Energia))}
      if(nrow(df.disponibilidadeOFRInd) > 0){df.disponibilidadeOFR <- rbind(df.disponibilidadeOFR, df.disponibilidadeOFRInd) %>% 
        dplyr::group_by(dplyr::across(c(-A13_VL_DISPONIBILIDADE_MAXIMA_PONTA))) %>% 
        dplyr::reframe(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A13_VL_DISPONIBILIDADE_MAXIMA_PONTA))}
      
    }
    
    # executa query para apagar da tabela BPO_A13_DISPONIBILIDADE_OFR os dados referentes a um possivel mesmo caso rodado anteriormente, 
    # de forma a evitar duplicacao dos dados
    query <- paste0("SELECT COUNT(*) AS TOTAL FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, 
                    " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
    apagar <- DBI::dbGetQuery(conexao, query) %>% dplyr::pull()
    if (apagar > 0) {
      query <- paste0("DELETE FROM BPO_A13_DISPONIBILIDADE_OFR WHERE A01_TP_CASO = ", tipoCaso, 
                      " AND A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo)
      DBI::dbExecute(conexao, query)
    }
    
    # executa query para gravar os dados de disponilidade das outras fontes renovaveis na tabela BPO_A13_DISPONIBILIDADE_OFR do BDBP
    DBI::dbWriteTable(conexao, "BPO_A13_DISPONIBILIDADE_OFR", df.disponibilidadeOFR, append = TRUE)
    
    mensagem <- "tabelas BPO_A13_DISPONIBILIDADE_OFR, BPO_A18_TIPOS_OFR e BPO_A19_FATOR_PONTA_OFR gravadas com sucesso!"
    
    # prepara energia de renovaveis para calculos de reserva
    df.energiaOFR <- dplyr::inner_join(df.energiaOFR, df.tipoContribuicaoPonta, by = c("TIPO" = "A18_TX_DESCRICAO")) %>% 
      dplyr::select(tipoFonte = A18_CD_TIPO_FONTE, subsistema = sistema, anoMes, energia = Energia)
    
    return(list(mensagem = mensagem, df.energiaOFR = df.energiaOFR))
  }
}
