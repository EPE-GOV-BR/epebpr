#' Calcula a disponibilidade hidraulica usando a metodologia da EPE
#'
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE
#' @param df.saidasHidro data frame com dados da tabela BPO_A06_SAIDA_HIDRO_NEWAVE
#' @param df.dadosUHE data frame com dados da tabela BPO_A03_DADOS_UHE
#' @param df.dadosVigentes data frame com dados da tabela BPO_A05_DADOS_VIGENTES_UHE
#' @param df.dadosMaquinasUHE data frame com dados da tabela BPO_A04_MAQUINAS_UHE
#' @param df.potMaquinas data frame com dados de potencia das UHE
#' @param df.dadosCaso data frame com dados do caso em execucao
#' @param lt.dadosTucurui lista com dados referentes a UHE Tucurui para calculo do PDisp
#' @param flagVert booleano que indica se considera ou nao o vertimento para todas as UHE
#'
#' @return \code{df.dadosCalculadosUHE} data frame com dados de disponibilidade individuais das UHE
#'
#' @export

calculaDisponibilidadeTipo1 <- function(tipoCaso, numeroCaso, codModelo, df.saidasHidro, df.dadosUHE, df.dadosVigentes, df.dadosMaquinasUHE, df.potMaquinas, df.dadosCaso, lt.dadosTucurui, flagVert){
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosVigentes, 
                                             df.saidasHidro, 
                                             by = c("A02_NR_REE", "A05_NR_MES" = "A06_NR_MES"),
                                             relationship = "many-to-many") %>% 
    dplyr::mutate(A08_VL_VOLUME_OPERATIVO = (A05_VL_VOL_MAX - A05_VL_VOL_MIN) * A06_VL_PERC_ARMAZENAMENTO + A05_VL_VOL_MIN,
                  colunaFlagVert = flagVert,
                  # verifica o flag de vertimento, se verdadeiro soma o vertimento na variavel de GH
                  A06_VL_GERACAO_HIDRAULICA = ifelse(colunaFlagVert,
                                                     A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO + A06_VL_VERTIMENTO_TURBINAVEL,
                                                     A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO)
    ) %>% 
    dplyr::select(-colunaFlagVert)
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosMaquinasUHE, by = "A03_CD_USINA") %>% 
    dplyr::mutate(VL_POT_EXP = round(A05_VL_POTENCIA - POT_TOTAL, 2))
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosUHE, by = "A03_CD_USINA") %>% 
    dplyr::mutate(A01_TP_CASO = tipoCaso, A01_NR_CASO = numeroCaso, A01_CD_MODELO = codModelo) %>% 
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES = A05_NR_MES, A08_NR_SERIE = A06_NR_SERIE, A02_NR_REE, 
                  A08_VL_VOLUME_OPERATIVO, A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4, 
                  A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE, A05_NR_CANAL_FUGA_MEDIO, A05_VL_TEIF, A05_VL_IP, 
                  A05_VL_VAZAO_MINIMA, A06_VL_GERACAO_HIDRAULICA, VL_POT_EXP)
  
  # 2 - COTA OPERATIVA(per,ser) => CALCULADA a partir do VOLUME OPERATIVO(per,ser) e do polinomio cota-volume
  # 3 - ALTURA DE QUEDA LIQUIDA(per,ser) => CALCULADA a partir da COTA OPERATIVA(per,ser), CANAL DE FUGA MEDIO (per) e Perdas
  # 3.1 - Altura de queda liquida = Cota Operativa - Canal de Fuga medio
  # 3.2 - Abate as perdas
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::mutate(A08_VL_COTA_OPERATIVA = A03_NR_PCV_0  + (A03_NR_PCV_1 * A08_VL_VOLUME_OPERATIVO) +
                    (A03_NR_PCV_2 * A08_VL_VOLUME_OPERATIVO^2) +
                    (A03_NR_PCV_3 * A08_VL_VOLUME_OPERATIVO^3) +
                    (A03_NR_PCV_4 * A08_VL_VOLUME_OPERATIVO^4),
                  A08_VL_ALTURA_LIQUIDA = ifelse(A03_TP_PERDA == 1,
                                                 (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO) * (1 - (A03_VL_PERDA / 100)),
                                                 (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO)- A03_VL_PERDA))
  
  
  # limpa campos ja usados
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::select(-A03_NR_PCV_0, -A03_NR_PCV_1, -A03_NR_PCV_2, -A03_NR_PCV_3,
                  -A03_NR_PCV_4, -A03_VL_PERDA, -A05_NR_CANAL_FUGA_MEDIO)
  
  # 3.3 - Ajuste caso encontre algum valor negativo
  df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA[df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA < 0.0001] <- 0.0001
  
  # 4 - POTENCIA MaXIMA(per,ser)
  # 4.1) PARA POTENCIA REFERENTE AOS CONJUNTOS Ja EXISTENTES: ALTURA DE REFERENCIA DO CONJUNTO >= ALTURA DE QUEDA
  df.dadosCalculadosUHEMaquinas <- dplyr::inner_join(df.dadosCalculadosUHE, df.potMaquinas,
                                                     by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA"),
                                                     relationship = "many-to-many")
  
  # Para os conjuntos onde a altura de queda liquida e menor que a altura de referencia do conjunto, a potencia
  # nao e igual a maxima, mas um percentual da maxima, calculado a partir da razao (HLIQ/HREF)^coef da turbina
  df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
    dplyr::mutate(A08_VL_POTENCIA_MAXIMA = ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                  A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                  A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                    (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA))
  
  # 4.3) PARA POTENCIA EM EXPANSaO e ALTURA DE REFERENCIA DO CONJUNTO 1 >= ALTURA DE QUEDA LiQUIDA(per,ser): APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP
  # 4.4) PARA POTENCIA EM EXPANSAO e ALTURA DE REFERENCIA DO CONJUNTO 1 < ALTURA DE QUEDA LiQUIDA(per,ser):
  # APENAS ABATE DO TOTAL EM EXPANSaO O TEIF E O IP E APLICA AINDA
  # UM FATOR DE REDUcaO DA POTENCIA IGUAL A (ALTURA DE QUEDA LiQUIDA(per,ser)/ALTURA DE REFERENCIA DO CONJUNTO 1)^COEFICIENTE DA TURBINA
  df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
    dplyr::mutate(A08_VL_POTENCIA_MAXIMA =
                    (A08_VL_POTENCIA_MAXIMA + ifelse((A04_NR_CONJUNTO == 1 & VL_POT_EXP > 0) ,
                                                     ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                            VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                            VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                              (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA ),
                                                     0)))
  
  df.potenciaMaximaUHE <- df.dadosCalculadosUHEMaquinas %>%
    dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A03_CD_USINA, A08_NR_MES, A08_NR_SERIE) %>%
    dplyr::summarise(A08_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>% dplyr::ungroup()
  
  # remove data frame ja utilizado
  rm(df.dadosCalculadosUHEMaquinas)
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.potenciaMaximaUHE,
                                             by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA", "A08_NR_MES", "A08_NR_SERIE"))
  
  # remove data frame ja utilizado
  rm(df.potenciaMaximaUHE)
  
  ###################################################################################################################################
  # Segundo Passo: Modula a geracao na ponta, respeitando a geracao media (GHTOT(per,res)) durante as horas do mes,
  # o GHMIN(per,ser) fora na hora da ponta e a maxima contribuicao
  # POTENCIA MAXIMA(per,ser) durante o numero de horas em que a ponta ocorre
  ###################################################################################################################################
  
  # GHMIN CALCULADO ORIGINALMENTE
  # Atualiza GHMIN se potencia maxima for inferior ao GHMIN
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::mutate(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL =
                    A08_VL_ALTURA_LIQUIDA * A03_VL_PRODUTIBILIDADE * A05_VL_VAZAO_MINIMA,
                  A08_VL_GERACAO_HIDRO_MINIMA = ifelse(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL > A08_VL_POTENCIA_MAXIMA,
                                                       A08_VL_POTENCIA_MAXIMA,
                                                       A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL))
  
  df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
    dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
    dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA),
                     A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
    dplyr::ungroup()
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                             by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
  
  
  # Calcula GHTOT por usina:
  # Rateia o GHTOT pela potencia maxima, ja abatida do GHMIN*/
  # GHMIN_UHE = GHMIN_UHE X (1 - (GHMIN_SSIS - GHTOT_SSIS)/GHMIN_SSIS)
  # GHMEDIA = GHMIN_UHE + ((GHTOT_SSIS - GHMIN_SSIS) X ((POT_MAX_UHE - GHMIN_UHE)/(POT_MAX_SSIS - GHMIN_SSIS)))
  # Modula o GHTOT, maximizando a geracao na horas de ponta*/
  # POT_MODULADA = ((GHMEDIA X 730.5) - (GHMIN X (730.5 - HORASPONTA))) / HORASPONTA
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::mutate(A08_VL_GERACAO_HIDRO_MINIMA = ifelse((A06_VL_GERACAO_HIDRAULICA < A09_VL_GERACAO_HIDRO_MINIMA_TMP),
                                                       (A08_VL_GERACAO_HIDRO_MINIMA * 
                                                          (1 - (A09_VL_GERACAO_HIDRO_MINIMA_TMP - A06_VL_GERACAO_HIDRAULICA) / A09_VL_GERACAO_HIDRO_MINIMA_TMP)),
                                                       A08_VL_GERACAO_HIDRO_MINIMA))
  
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>% dplyr::select(-A09_VL_GERACAO_HIDRO_MINIMA_TMP)
  
  df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
    dplyr::group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
    dplyr::summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA)) %>% dplyr::ungroup()
  
  df.dadosCalculadosUHE <- dplyr::inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                             by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))
  
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::mutate(A08_VL_GERACAO_HIDRO_MEDIA = (A08_VL_GERACAO_HIDRO_MINIMA + ((A06_VL_GERACAO_HIDRAULICA - A09_VL_GERACAO_HIDRO_MINIMA_TMP) *
                                                                                 ((A08_VL_POTENCIA_MAXIMA - A08_VL_GERACAO_HIDRO_MINIMA) / 
                                                                                    (A09_VL_POTENCIA_MAXIMA - A09_VL_GERACAO_HIDRO_MINIMA_TMP)))),
                  A08_VL_POTENCIA_MAXIMA_MODULADA = (((A08_VL_GERACAO_HIDRO_MEDIA * 730.5) -
                                                        (A08_VL_GERACAO_HIDRO_MINIMA * (730.5 - df.dadosCaso$horasPonta))) / df.dadosCaso$horasPonta))
  
  # Ajusta potencia maxima de acordo com o limite superior
  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse(A08_VL_POTENCIA_MAXIMA_MODULADA > A08_VL_POTENCIA_MAXIMA,
                                                           A08_VL_POTENCIA_MAXIMA,
                                                           A08_VL_POTENCIA_MAXIMA_MODULADA),
                  A08_VL_VAZAO_MAXIMA = 0,
                  A08_VL_VAZAO_MAXIMA_MODULADA = 0,
                  A08_VL_ALTURA_MODULADA = 0)
  
  # Ajusta potencia maxima de tucurui
  df.dadosTucurui <- df.dadosCalculadosUHE %>% 
    dplyr::filter(A03_CD_USINA == lt.dadosTucurui[['cod']]) %>% 
    dplyr::left_join(df.dadosUHE, by = "A03_CD_USINA") %>% 
    dplyr::left_join(df.dadosVigentes, by = c("A03_CD_USINA", "A08_NR_MES" = "A05_NR_MES")) %>% 
    dplyr::mutate(H = A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO - A03_VL_PERDA,
                  A08_VL_POTENCIA_MAXIMA_MODULADA = dplyr::if_else(A08_VL_COTA_OPERATIVA < lt.dadosTucurui[["cotasLimite"]][2],
                                                                   pmin(A08_VL_POTENCIA_MAXIMA_MODULADA,
                                                                       4245*(1-A05_VL_TEIF.x/100)*(1-A05_VL_IP.x/100)*(H/65.5)**1.5),
                                                                   A08_VL_POTENCIA_MAXIMA_MODULADA),
                                                                   dplyr::if_else(A08_VL_COTA_OPERATIVA >= lt.dadosTucurui[["cotasLimite"]][2] & A08_VL_COTA_OPERATIVA < lt.dadosTucurui[["cotasLimite"]][1],
                                                                                  pmin(A08_VL_POTENCIA_MAXIMA_MODULADA,
                                                                                      4245*(1-A05_VL_TEIF.x/100)*(1-A05_VL_IP.x/100)*(H/65.5)**1.5 + 1560*(H/61.7)**1.5),
                                                                                  pmin(A08_VL_POTENCIA_MAXIMA_MODULADA,
                                                                                      4245*(1-A05_VL_TEIF.x/100)*(1-A05_VL_IP.x/100)*(H/65.5)**1.5 + 4680*(1-A05_VL_TEIF.x/100)*(1-A05_VL_IP.x/100)*(H/61.7)**1.5)
                                                                   )
                  )
  
  # substitui os dados de Tucurui no df
  df.dadosCalculadosUHE[df.dadosCalculadosUHE[, "A03_CD_USINA"] == lt.dadosTucurui[['cod']],]$A08_VL_POTENCIA_MAXIMA_MODULADA <- df.dadosTucurui$A08_VL_POTENCIA_MAXIMA_MODULADA

  df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
    dplyr::select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES,
                  A08_NR_SERIE, A02_NR_REE, A08_VL_VOLUME_OPERATIVO,
                  A08_VL_COTA_OPERATIVA, A08_VL_ALTURA_LIQUIDA, A08_VL_VAZAO_MAXIMA,
                  A08_VL_POTENCIA_MAXIMA, A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
                  A08_VL_GERACAO_HIDRO_MINIMA, A08_VL_GERACAO_HIDRO_MEDIA,
                  A08_VL_VAZAO_MAXIMA_MODULADA, A08_VL_ALTURA_MODULADA,
                  A08_VL_POTENCIA_MAXIMA_MODULADA)
  
  return(df.dadosCalculadosUHE)
}