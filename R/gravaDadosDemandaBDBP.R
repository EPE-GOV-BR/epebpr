#' Gravacao dos dados de demanda de ponta
#'
#' Faz a gravacao dos dados de demanda de ponta do NEWAVE no banco de dados do Balanco de Potencia (BDBP)
#' Os dados sao gravados na tabela BPO_A10_DEMANDA do BDBP.
#'
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE e auxliares do BP.
#' @param conexao caracter com a conexao com o Banco de Dados do Balanco de Potencia.
#' @param tipoCaso caracter com o tipo de caso simulado. [1]=PDE [2]=PMO [3]=GF.
#' @param numeroCaso caracter com o numero do caso, definido pelo usuario.
#' @param codModelo caracter com a definicao do modelo utilizado. [1]=Newave [2]=Suishi.
#' @param tipoDemanda caracter com a definicao do tipo de demanda do caso. [1]=Deterministica [2]=Liquida
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na tabela BPO_A10_DEMANDA 
#'
#' @examples
#' \dontrun{
#' gravacaoDadosDemandaBDBP("C:/PDE2027_Caso080", conexao, 1, 80, 1)
#' }
#'
#' @export
gravacaoDadosDemandaBDBP <- function(pastaCaso, conexao, tipoCaso, numeroCaso, codModelo, tipoDemanda) {
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
  if (missing(tipoDemanda)) {
    stop("favor indicar o tipo de demanda")
  }
  
  # executa query para apagar da tabela BPO_A10_DEMANDA os dados referentes a um possivel mesmo caso rodado anteriormente, 
  # de forma a evitar duplicacao dos dados
  
  DBI::dbExecute(conexao, paste0("DELETE FROM BPO_A10_DEMANDA
                              WHERE A01_TP_CASO = ", tipoCaso, 
                                 " AND A01_NR_CASO = ", numeroCaso, 
                                 " AND A01_CD_MODELO = ", codModelo))
  
  # se for demanda deterministica, faz a leitura com o leitorrmpe
  # se for demanda liquida, faz a leitura do arquivo de detalhes da carga liquida
  if(tipoDemanda == 1){
    # executa as funcoes de leitura do pacote leitorrmpe para o carregamento dos dados da demanda de ponta (patamar = 1)
    # insere as variaveis associadas ao tipoCaso, numeroCaso e codModelo
    # define a demanda para o patamar de ponta (demandaPonta = energiaMercado * profundidadeCarga)
    df.mercado <- leitorrmpe::leituraMercadoEnergia(pastaCaso)
    df.patamar <- dplyr::filter(leitorrmpe::leituraDadosProfundidadePatamarCarga(pastaCaso), patamar == 1)
    
    # verifica se mercado e patamar estao com o mesmo horizonte de meses
    if (length(dplyr::setdiff(unique(df.patamar$anoMes), unique(df.mercado$anoMes))) != 0 & tipoCaso != 2) {
      DBI::dbDisconnect(conexao)
      stop("Horizonte de mercado inferior ao horizonte dos patamares de carga!")
    }
    if (length(dplyr::setdiff(unique(df.mercado$anoMes), unique(df.patamar$anoMes))) != 0) {
      DBI::dbDisconnect(conexao)
      stop("Horizonte dos patamares de carga inferior ao horizonte de mercado!")
    }
    
    # verifica se mercado e patamar estao com os mesmos subsistemas
    subsistemasConjuntos <- length(dplyr::intersect(unique(df.mercado$codSubsistema), unique(df.patamar$codSubsistema)))
    if (length(unique(df.mercado$codSubsistema)) != subsistemasConjuntos | length(unique(df.patamar$codSubsistema)) != subsistemasConjuntos) {
      DBI::dbDisconnect(conexao)
      stop("Patamares de carga e mercado n\u00E3o possuem os mesmos subsistemas!")
    }
    
    # horizonte de simulacao, no formato anoMes (AAAAMM)
    horizonte <- leitorrmpe::definePeriodo(pastaCaso) %>% dplyr::pull(anoMes)
    
    df.Demanda <- dplyr::inner_join(df.mercado, df.patamar, by = c("anoMes", "codSubsistema")) %>% 
      dplyr::mutate(tipoCaso = tipoCaso, 
                    numeroCaso = numeroCaso, 
                    codModelo = codModelo, 
                    tipoDemanda, 
                    demandaPonta = energiaMercado * profundidadeCarga) %>% 
      dplyr::filter(dplyr::between(anoMes, min(horizonte), max(horizonte))) %>% 
      # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
      dplyr::select(A01_TP_CASO = tipoCaso,
                    A01_NR_CASO = numeroCaso,
                    A01_CD_MODELO = codModelo,
                    A02_NR_SUBSISTEMA = codSubsistema,
                    A10_NR_MES = anoMes,
                    A10_NR_TIPO_DEMANDA = tipoDemanda,
                    A10_VL_DEMANDA = demandaPonta)
  }else{
    ## leitura da planilha de detalhes da carga liquida maxima
    detalhesCargaLiqMax <- list.files(path = pastaCaso, pattern = "^detalhesCargaLiquida")
    if (length(detalhesCargaLiqMax) != 1) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de detalhes da carga l\u00EDquida m\u00E1xima n\u00E1o encontrada ou multiplos arquivos com nome detalhesCargaLiquida em ", pastaCaso)
    }
    
    # leitura do excel com detalhes da carga liquida maxima
    df.detalhesCLiqMax <- readxl::read_xlsx(path = paste(pastaCaso, detalhesCargaLiqMax, sep = "/"))
    
    # leitura dos REES/subsistemas cadastrados para garantir que nao haja geracao em local inexistente
    squery <- "SELECT
                A02_NR_SUBSISTEMA
              FROM
                BPO_A02_SUBSISTEMAS
              WHERE
                A02_TP_FICTICIO = 0"
    reeCadastradas <- DBI::dbGetQuery(conexao, squery) %>% 
      dplyr::pull(A02_NR_SUBSISTEMA)
    
    reeDetalhes <- df.detalhesCLiqMax %>% 
      dplyr::pull(codSubsistema) %>% 
      unique()
    
    diferencaREE <- dplyr::setdiff(reeDetalhes, reeCadastradas) %>% length()
    
    if (diferencaREE != 0) {
      DBI::dbDisconnect(conexao)
      stop("Planilha de detalhes com dados em subsistema/REE n\u00E3o cadastrado!")
    }
    
    # verifica quais subsistemas nao estao na planilha de detalhes de carga liquida
    # subsistemas sem demanda
    diferencaREE <- dplyr::setdiff(reeCadastradas, reeDetalhes)
    
    # horizonte de simulacao, no formato anoMes (AAAAMM)
    horizonte <- leitorrmpe::definePeriodo(pastaCaso) %>% dplyr::pull(anoMes)
    
    # cria df com a demanda igual a zero para os subsistemas sem demanda
    df.DemandaZero <- data.frame(A01_TP_CASO = tipoCaso,
                                 A01_NR_CASO = numeroCaso,
                                 A01_CD_MODELO = codModelo,
                                 A02_NR_SUBSISTEMA = rep(diferencaREE, each = length(horizonte)),
                                 A10_NR_MES = rep(horizonte, times = length(diferencaREE)),
                                 A10_NR_TIPO_DEMANDA = tipoDemanda,
                                 A10_VL_DEMANDA = 0)
    
    df.Demanda <- df.detalhesCLiqMax %>% 
      dplyr::mutate(tipoCaso = tipoCaso, 
                    numeroCaso = numeroCaso, 
                    codModelo = codModelo, 
                    tipoDemanda = tipoDemanda,
                    anoMes = ano*100 + mes) %>% 
      dplyr::filter(dplyr::between(anoMes, min(horizonte), max(horizonte))) %>% 
      # renomeia os campos do data frame para compatibilizacao com a tabela do BDBP
      dplyr::select(A01_TP_CASO = tipoCaso,
                    A01_NR_CASO = numeroCaso,
                    A01_CD_MODELO = codModelo,
                    A02_NR_SUBSISTEMA = codSubsistema,
                    A10_NR_MES = anoMes,
                    A10_NR_TIPO_DEMANDA = tipoDemanda,
                    A10_VL_DEMANDA = carga)
    
    dfDemanda <- rbind(df.Demanda, df.DemandaZero)
    
  }
  
  # executa query para gravar os dados da demanda de ponta na tabela BPO_A10_DEMANDA do BDBP
  DBI::dbWriteTable(conexao, "BPO_A10_DEMANDA", df.Demanda, append = TRUE)
  
  mensagem <- "tabela BPO_A10_DEMANDA gravada com sucesso!"
  
  return(mensagem)
}
