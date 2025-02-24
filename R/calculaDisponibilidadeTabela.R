#' Calcula a disponibilidade hidraulica para UHEs que modulam segundo tabela defluencia x potencia disponivel
#'
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE
#' @param pastaCaso caracter com a localizacao dos arquivos NEWAVE e auxliares do BP
#' @param UHE vetor com o codigo das UHE que modulam por tabela
#' @param df.saidasHidro data frame com dados da tabela BPO_A06_SAIDA_HIDRO_NEWAVE
#' @param lt.hidrogramaBM lista contendo os dados do hidrograma de Belo Monte
#' @param df.hidrograma data frame com dados do hidrograma de Belo Monte
#' @param df.tabelaModulacao data frame com dados de defluencia x potencia disponivel
#' @param flagVert booleano que indica se considera ou nao o vertimento para todas as UHE
#'
#' @return \code{df.dadosUHEModulamTabela} data frame com dados de disponibilidade individuais das UHE que modulam conforme tabela
#'
#' @export
#' 
calculaDisponibilidadeTabela <- function(tipoCaso, numeroCaso, codModelo, pastaCaso, UHE, df.saidasHidro, lt.hidrogramaBM, df.hidrograma, df.tabelaModulacao, flagVert){

  # calculo da pordutibilidade das usinas dos REE tipo 4
  df.prodREEModulaTabela <- dplyr::full_join(leitorrmpe::leituraAlteracaoDadosUsinasHidro(pastaCaso)[[1]],
                                             leitorrmpe::leituraDadosUsinasHidro(pastaCaso)[[1]] %>%
                                               tidyr::crossing(anoMes = unique(df.saidasHidro$A06_NR_MES)) %>% 
                                               dplyr::select(codUsina, anoMes, poliCotaVolumeA0, poliCotaVolumeA1, poliCotaVolumeA2, poliCotaVolumeA3, poliCotaVolumeA4, volumeMaximo, volumeReferencia, canalFugaMedio, tipoPerda, perda, tipoTurbina, TEIF, IP, produtibilidade) %>%
                                               dplyr::mutate(volumeReferencia = ifelse(volumeReferencia < volumeMaximo, volumeMaximo, volumeReferencia)) %>%
                                               dplyr::select(-volumeMaximo) %>%  
                                               dplyr::mutate(kturb = ifelse(tipoTurbina == 2, 1.2, 1.5)),
                                             by=c("codUsina" ,"anoMes")) %>% 
    dplyr::left_join(leitorrmpe::leituraConfiguracaoHidro(pastaCaso) %>% dplyr::select(codREE,codUsina), by=c("codUsina")) %>% 
    dplyr::mutate(volumeMaximo = ifelse(is.na(volumeMaximo) | volumeMaximo >  volumeReferencia ,volumeReferencia,volumeMaximo),
                  nivelMontante = ifelse(is.na(nivelMontante),
                                         poliCotaVolumeA0 + volumeMaximo*poliCotaVolumeA1 + volumeMaximo^2*poliCotaVolumeA2 + volumeMaximo^3*poliCotaVolumeA3+ volumeMaximo^4*poliCotaVolumeA4,
                                         nivelMontante),
                  canalFuga = ifelse(is.na(canalFuga), canalFugaMedio, canalFuga),
                  perda = ifelse(tipoPerda==2, perda, (nivelMontante - canalFuga)*perda/100),
                  Hliq = nivelMontante-canalFuga - perda,
                  TEIF = ifelse(is.na(TEIF.x), TEIF.y, TEIF.x),
                  IP = ifelse(is.na(IP.x), IP.y, IP.x)) %>% 
    dplyr::left_join(leitorrmpe::leituraDadosUsinasHidro(pastaCaso)[[3]], by = c("codUsina"), relationship = "many-to-many") %>%
    dplyr::mutate(potConj = numeroMaquinas * ifelse(Hliq >= quedaEfetiva, potenciaUnitaria, potenciaUnitaria*(Hliq/quedaEfetiva)^kturb),
                  produtibilidade = produtibilidade * (nivelMontante - canalFuga-perda)) %>%
    dplyr::filter(codUsina %in% UHE, anoMes %in% unique(df.saidasHidro$A06_NR_MES)) %>%
    dplyr::group_by(codREE, codUsina, anoMes, produtibilidade, TEIF, IP) %>%
    dplyr::summarize(GHmax = sum(potConj)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(GHmax=GHmax * (1 - TEIF/100) * (1 - IP/100)) %>%
    dplyr::select(codREE, codUsina, anoMes, GHmax, produtibilidade) %>%
    dplyr::group_by(codREE, anoMes) %>%
    dplyr::mutate(proporcao = GHmax/sum(GHmax))
  
  df.dadosUHEModulamTabelaUsina <- dplyr::left_join(df.saidasHidro, 
                                                    df.prodREEModulaTabela, 
                                                    by = c("A02_NR_REE" = "codREE", "A06_NR_MES" = "anoMes"),
                                                    relationship = "many-to-many") %>% 
    dplyr::left_join(lt.hidrogramaBM[["usinas"]], by = c("codUsina")) %>% 
    dplyr::mutate(mes = A06_NR_MES%%100) %>% 
    dplyr::left_join(df.hidrograma, by = c("mes", "grupo", "codUsina" = "codUsinaHidrograma")) %>% 
    dplyr::select(-mes) %>% 
    dplyr::mutate(flag = flagVert,
                  ghtot_ree = ifelse(flag,
                                     A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO + A06_VL_VERTIMENTO_TURBINAVEL,
                                     A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO),
                  gh = ghtot_ree * proporcao,
                  flagHidrograma = ifelse(!is.na(grupo) & !is.na(vazao), 1, ifelse(!is.na(grupo) & is.na(vazao), 0, NA))) %>% 
    dplyr::left_join(df.tabelaModulacao %>% dplyr::select(-codUsina), by = c("A02_NR_REE" = "codREE")) %>%
    dplyr::group_by(A02_NR_REE, A06_NR_MES, A06_NR_SERIE, grupo) %>% 
    dplyr::mutate(proporcao_hidrograma = (GHmax  * flagHidrograma)/sum(GHmax  * flagHidrograma),
                  proporcao_impactada = (GHmax  * (1 - flagHidrograma))/sum(GHmax  * (1 - flagHidrograma)),
                  gh_hidrograma = pmin(produtibilidade * vazao, GHmax),
                  gh_impactada = ifelse(flagHidrograma == 0,
                                        pmin(GHmax, pmax(0, sum(gh) - sum(gh_hidrograma, na.rm = T))) * proporcao_impactada,
                                        NA),
                  gh_hidrograma_corrigido = ifelse(flagHidrograma == 1,
                                                   pmin(gh_hidrograma, sum(gh) * proporcao_hidrograma) + pmax(0,(sum(gh) - sum(gh_impactada,na.rm = T) - sum(gh_hidrograma,na.rm = T))) * proporcao_hidrograma,
                                                   NA),
                  gh_corrigido = dplyr::coalesce(gh_hidrograma_corrigido, gh_impactada, gh),
                  vazao = gh_corrigido/produtibilidade) %>% 
    dplyr::ungroup() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pdisph = ifelse(codUsina %in% unique(df.tabelaModulacao$codUsina),
                                  funcao(vazao),
                                  gh_corrigido),
                  A01_CD_MODELO = codModelo,
                  A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso) %>% 
    dplyr::rename(A33_NR_MES = A06_NR_MES,
                  A33_NR_SERIE = A06_NR_SERIE,
                  A03_CD_USINA = codUsina,
                  A33_VL_GERACAO_HIDRO_REE = ghtot_ree,
                  A33_VL_PRODUTIBILIDADE = produtibilidade,
                  A33_VL_PROPORCAO = proporcao,
                  A33_VL_VAZAO = vazao,
                  A33_VL_GERACAO_HIDRO_CORRIGIDA = gh_corrigido,
                  A33_VL_POTENCIA_MAXIMA = GHmax,
                  A33_VL_DISPONIBILIDADE_MAXIMA_PONTA = pdisph) %>% 
    dplyr::select(A01_CD_MODELO, 
                  A01_TP_CASO, 
                  A01_NR_CASO,
                  A02_NR_REE,
                  A33_NR_MES, 
                  A33_NR_SERIE,
                  A03_CD_USINA,
                  A33_VL_GERACAO_HIDRO_REE,
                  A33_VL_PRODUTIBILIDADE,
                  A33_VL_PROPORCAO,
                  A33_VL_VAZAO,
                  A33_VL_GERACAO_HIDRO_CORRIGIDA,
                  A33_VL_POTENCIA_MAXIMA,
                  A33_VL_DISPONIBILIDADE_MAXIMA_PONTA)
  
  df.dadosUHEModulamTabela <- df.dadosUHEModulamTabelaUsina %>% 
    dplyr::group_by(A02_NR_REE, A33_NR_SERIE, A33_NR_MES) %>% #dados por REE
    dplyr::reframe(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A33_VL_DISPONIBILIDADE_MAXIMA_PONTA),
                   A33_VL_POTENCIA_MAXIMA = sum(A33_VL_POTENCIA_MAXIMA)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = A33_VL_DISPONIBILIDADE_MAXIMA_PONTA,
                  A01_CD_MODELO = codModelo,
                  A01_TP_CASO = tipoCaso,
                  A01_NR_CASO = numeroCaso,
                  A09_NR_MES = A33_NR_MES,
                  A09_NR_SERIE = A33_NR_SERIE,
                  A09_VL_GERACAO_HIDRO_MINIMA = 0,
                  A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = 0,
                  A09_VL_POTENCIA_MAXIMA = A33_VL_POTENCIA_MAXIMA) %>% 
    dplyr::select(A01_CD_MODELO, 
                  A01_TP_CASO, 
                  A01_NR_CASO, 
                  A02_NR_REE, 
                  A09_NR_MES,
                  A09_NR_SERIE, 
                  A09_VL_GERACAO_HIDRO_MINIMA, 
                  A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                  A09_VL_DISPONIBILIDADE_MAXIMA_PONTA, 
                  A09_VL_POTENCIA_MAXIMA)
  
  return(df.dadosUHEModulamTabela)
}