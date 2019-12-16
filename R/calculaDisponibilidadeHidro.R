#' Calcula a disponibilidade hidraulica para o Balanco de Ponta
#'
#' Faz a leitura dos arquivos do NEWAVE e carrega todos os dados necessarios na base SQLite para execucao do Balanco de Ponto.
#' Usa como referencia para a identificacao dos arquivos as posicoes definidas no Manual do Usuario do
#' Modelo Estrategico de geracao hidrotermica a subsistemas equivalentes do Projeto NEWAVE versao 25.0.1 de setembro/2019 - paginas 12 - 15
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de ponta
#' @param pastaCaso localizacao da pasta com os arquivos do NEWAVE do caso a ser analisado no balanco de ponta
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#' @param descricaoCaso vetor de caracteres indicando a descricao do caso
#' @param horasPonta valor inteiro com o numero de horas de ponta
#' @param reservaOperativa valor com a reserva operativa usada no calculo do balanco. Ex. 0.05 (5%)
#' @param idDemandaLiquida identificador de calculo com demanda liquida. 1:Demanda Liquida; 0:Deterministica
#' @param anosPre numero de anos iniciais para fins de estabilizacao no calculo da politica
#' @param anosPos numero de anos finais para fins de estabilizacao no calculo da politica
#' @param sistemasNaoModulamPonta vetor numerico com codigos dos sitemas que nao modulam na ponta
#' @param sistemasNaoModulamMedia vetor numerico com codigos dos sitemas que nao modulam na media
#' @param codTucurui codigo atribuido para a usina de Tucurui ###############################
#' @param cotaLimiteTucurui valor da cota da usina de Tucurui em metros
#' @param geracaoLimiteTucurui valor da geracao limite da usina de Tucurui
#'
#'
#' @return \code{mensagem} vetor de caracteres com a mensagem de sucesso de gravacao na base
#'
#' @export

#### Dados de entrada - arquivo balanco.txt ####
df.dadosBalaco <- read_delim("X:/SGE-Projetos/02-Estudos/05-Plano Decenal de Expansão/2018-2027/Balanço de Ponta/Desenvolvimento/R/balanco.txt",
                             delim = ";", comment = "#", col_types = "cc", locale = locale(encoding = "UTF-8"))
localizacaoBase <- df.dadosBalaco$valor[14] %>% str_squish() %>% enc2native() # garante o encode correto
pastaCaso <- df.dadosBalaco$valor[15] %>% str_squish() %>% enc2native() # garante o encode correto
tipoCaso <- df.dadosBalaco$valor[1] %>% as.numeric()
numeroCaso <- df.dadosBalaco$valor[2] %>% as.numeric()
codModelo <- df.dadosBalaco$valor[3] %>% as.numeric()
codTucurui <- df.dadosBalaco$valor[8] %>% as.numeric()
# gravar balanço resumido e CMO (BPO_A16_BALANCO e BPO_A20_BALANCO_SUBSISTEMA) [T] ou também gravar balanço por gerador (BPO_A17_BALANCO_GERADOR) [F]
balancoResumido <- df.dadosBalaco$valor[20] %>% str_squish() %>%as.logical()
#### FIM dos dados de entrada ####

tic()
# identifica os arquivos do NEWAVE para leitura
df.arquivos <- leituraArquivos(pastaCaso)
# de acordo com o manual do NEWAVE o arquivo de submotorização fica na posição 13
arquivoSubmotorizacao <- df.arquivos %>% filter(row_number() == 13) %>% select(arquivo) %>% pull() %>% paste(pastaCaso, ., sep = "/")

# SQLite
conexaoSQLite <- dbConnect(RSQLite::SQLite(), localizacaoBase)

sql <- paste0("SELECT A01_NR_MES_INICIO as dataInicioCaso,
              A01_NR_MES_FIM as dataFimCaso,
              A01_NR_HORAS_PONTA as horasPonta,
              A01_NR_GERACAO_LIMITE_TUCURUI as gerLimiteTucurui,
              A01_NR_COTA_LIMITE_TUCURUI as cotaLimiteTucurui
              FROM BPO_A01_CASOS_ANALISE
              WHERE
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo, ";")
df.dadosCaso <- dbGetQuery(conexaoSQLite, sql)

# atualização de submotorização
sql <- paste0("SELECT A02_NR_REE, A02_NR_SUBSISTEMA, A02_TX_DESCRICAO_REE
              FROM BPO_A02_REES
              WHERE
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo, ";")
df.subsistemas <- dbGetQuery(conexaoSQLite, sql)

df.submotorizacaoREE <- leituraSubmotorizacaoREE(arquivoSubmotorizacao) %>%
  inner_join(df.subsistemas, by = c("ree" = "A02_TX_DESCRICAO_REE")) %>%
  filter(anoMes >= df.dadosCaso$dataInicioCaso, anoMes <= df.dadosCaso$dataFimCaso)
# cria lista para passar os parâmetros para update
lt.submotorizacaoREE <- list(ree = df.submotorizacaoREE$A02_NR_REE, anoMes = df.submotorizacaoREE$anoMes,
                             submotorizacao = df.submotorizacaoREE$submotorizacao)

sqlUpdate <- paste0("UPDATE BPO_A06_SAIDA_HIDRO_NEWAVE
                    SET A06_VL_SUBMOTORIZACAO = $submotorizacao
                    WHERE
                    A02_NR_REE = $ree AND
                    A06_NR_MES = $anoMes AND
                    A01_TP_CASO = ", tipoCaso, " AND
                    A01_NR_CASO = ", numeroCaso, " AND
                    A01_CD_MODELO = ", codModelo, ";")
dbExecute(conexaoSQLite, "BEGIN TRANSACTION;")
dbExecute(conexaoSQLite, sqlUpdate, param = lt.submotorizacaoREE)
dbExecute(conexaoSQLite, "COMMIT TRANSACTION;")
# fim atualização de submotorização

# dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
# dbExecute(conexaoSQLite, "PRAGMA cache_size = 20000;")
# dbExecute(conexaoSQLite, "PRAGMA page_size = 8192;")
sql <- paste0("SELECT A05.A01_TP_CASO AS A01_TP_CASO,
              A05.A01_NR_CASO AS A01_NR_CASO,
              A05.A01_CD_MODELO AS A01_CD_MODELO,
              A05.A03_CD_USINA AS A03_CD_USINA,
              A05.A05_NR_MES AS A08_NR_MES,
              A06.A06_NR_SERIE AS A08_NR_SERIE,
              A05.A02_NR_REE AS A02_NR_REE,
              (A05.A05_VL_VOL_MAX - A05.A05_VL_VOL_MIN) * A06.A06_VL_PERC_ARMAZENAMENTO + A05.A05_VL_VOL_MIN AS A08_VL_VOLUME_OPERATIVO,
              A03_NR_PCV_0, A03_NR_PCV_1, A03_NR_PCV_2, A03_NR_PCV_3, A03_NR_PCV_4,
              A03_VL_PERDA, A03_TP_PERDA, A03_VL_PRODUTIBILIDADE,
              A05_NR_CANAL_FUGA_MEDIO,
              A05_VL_TEIF, A05_VL_IP,
              A05_VL_VAZAO_MINIMA,
              A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO AS A06_VL_GERACAO_HIDRAULICA,
              ROUND(A05_VL_POTENCIA - POT_TOTAL, 2) AS VL_POT_EXP
              FROM BPO_A03_DADOS_UHE A03,
              BPO_A05_DADOS_VIGENTES_UHE A05,
              BPO_A06_SAIDA_HIDRO_NEWAVE A06,
              (SELECT A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA,
              SUM(A04_NR_MAQUINAS * A04_VL_POTENCIA) AS POT_TOTAL
              FROM BPO_A04_MAQUINAS_UHE
              GROUP BY A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA) A04
              WHERE
              A03.A01_TP_CASO = A05.A01_TP_CASO AND
              A03.A01_NR_CASO = A05.A01_NR_CASO AND
              A03.A01_CD_MODELO = A05.A01_CD_MODELO AND
              A03.A03_CD_USINA = A05.A03_CD_USINA AND
              A05.A01_TP_CASO = A06.A01_TP_CASO AND
              A05.A01_NR_CASO = A06.A01_NR_CASO AND
              A05.A01_CD_MODELO = A06.A01_CD_MODELO AND
              A05.A02_NR_REE = A06.A02_NR_REE AND
              A05.A05_NR_MES = A06.A06_NR_MES AND
              A03.A01_TP_CASO = A04.A01_TP_CASO AND
              A03.A01_NR_CASO = A04.A01_NR_CASO AND
              A03.A01_CD_MODELO = A04.A01_CD_MODELO AND
              A03.A03_CD_USINA = A04.A03_CD_USINA AND
              A03.A03_TX_STATUS <> 'NC' AND
              A05.A05_NR_MES BETWEEN ", df.dadosCaso$dataInicioCaso , " AND ", df.dadosCaso$dataFimCaso, " AND
              A05.A02_NR_REE IN (SELECT A02_NR_REE FROM BPO_A02_REES
              WHERE A02_TP_CALC_POTENCIA = 1 AND
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo,") AND
              A03.A01_TP_CASO = ", tipoCaso, " AND
              A03.A01_NR_CASO = ", numeroCaso, " AND
              A03.A01_CD_MODELO = ", codModelo, ";")
df.dadosCalculadosUHE <- dbGetQuery(conexaoSQLite, sql)
# dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")

# 2 - COTA OPERATIVA(per,ser) => CALCULADA a partir do VOLUME OPERATIVO(per,ser) e do polinomio cota-volume
# 3 - ALTURA DE QUEDA LÍQUIDA(per,ser) => CALCULADA a partir da COTA OPERATIVA(per,ser), CANAL DE FUGA MÉDIO (per) e Perdas
# 3.1 - Altura de queda liquida = Cota Operativa - Canal de Fuga médio*/
# 3.2 - Abate as perdas
df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_COTA_OPERATIVA = A03_NR_PCV_0  + (A03_NR_PCV_1 * A08_VL_VOLUME_OPERATIVO) +
           (A03_NR_PCV_2 * A08_VL_VOLUME_OPERATIVO^2) +
           (A03_NR_PCV_3 * A08_VL_VOLUME_OPERATIVO^3) +
           (A03_NR_PCV_4 * A08_VL_VOLUME_OPERATIVO^4),
         A08_VL_ALTURA_LIQUIDA = ifelse(A03_TP_PERDA == 1,
                                        (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO) * (1 - (A03_VL_PERDA / 100)),
                                        (A08_VL_COTA_OPERATIVA - A05_NR_CANAL_FUGA_MEDIO)- A03_VL_PERDA))


# limpa campos já usados
df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  select(-A03_NR_PCV_0, -A03_NR_PCV_1, -A03_NR_PCV_2, -A03_NR_PCV_3,
         -A03_NR_PCV_4, -A03_VL_PERDA, -A05_NR_CANAL_FUGA_MEDIO)


# 3.3 - Ajuste caso encontre algum valor negativo
df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA[df.dadosCalculadosUHE$A08_VL_ALTURA_LIQUIDA < 0.0001] <- 0.0001

# 4 - POTENCIA MÁXIMA(per,ser)
# 4.1) PARA POTÊNCIA REFERENTE AOS CONJUNTOS JÁ EXISTENTES: ALTURA DE REFERÊNCIA DO CONJUNTO >= ALTURA DE QUEDA
sql <- paste0("SELECT A.A01_TP_CASO,
              A.A01_NR_CASO,
              A.A01_CD_MODELO,
              A.A03_CD_USINA,
              A04_NR_MAQUINAS,
              A04_VL_POTENCIA,
              A04_NR_CONJUNTO,
              A.A04_VL_ALTURA_REFERENCIA,
              CASE B.A03_TP_TURBINA WHEN 1 THEN 1.5 WHEN 3 THEN 1.5 ELSE 1.2 END VL_COEF_TURBINA
              FROM
              BPO_A04_MAQUINAS_UHE A,
              BPO_A03_DADOS_UHE B
              WHERE
              A.A01_CD_MODELO = B.A01_CD_MODELO AND
              A.A01_NR_CASO = B.A01_NR_CASO AND
              A.A01_TP_CASO = B.A01_TP_CASO AND
              A.A03_CD_USINA = B.A03_CD_USINA AND
              A.A01_TP_CASO = ", tipoCaso, " AND
              A.A01_NR_CASO = ", numeroCaso, " AND
              A.A01_CD_MODELO = ", codModelo, ";")
df.potMaquinas<- dbGetQuery(conexaoSQLite, sql)

df.dadosCalculadosUHEMaquinas <- inner_join(df.dadosCalculadosUHE, df.potMaquinas,
                                            by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA"))


# remove data frame df.potMaquinas já utilizado
rm(df.potMaquinas)

# Para os conjuntos onde a altura de queda líquida é menor que a altura de referência do conjunto, a potência
# não é igual à máxima, mas um percentual da máxima, calculado a partir da razão (HLIQ/HREF)^coef da turbina
df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
  mutate(A08_VL_POTENCIA_MAXIMA = ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                         A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                         A04_NR_MAQUINAS * A04_VL_POTENCIA * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                           (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA))

# 4.3) PARA POTENCIA EM EXPANSÃO e ALTURA DE REFERÊNCIA DO CONJUNTO 1 >= ALTURA DE QUEDA LÍQUIDA(per,ser): APENAS ABATE DO TOTAL EM EXPANSÃO O TEIF E O IP
# 4.4) PARA POTÊNCIA EM EXPANSAO e ALTURA DE REFERÊNCIA DO CONJUNTO 1 < ALTURA DE QUEDA LÍQUIDA(per,ser):
# APENAS ABATE DO TOTAL EM EXPANSÃO O TEIF E O IP E APLICA AINDA
# UM FATOR DE REDUÇÃO DA POTÊNCIA IGUAL A (ALTURA DE QUEDA LÍQUIDA(per,ser)/ALTURA DE REFERÊNCIA DO CONJUNTO 1)^COEFICIENTE DA TURBINA
df.dadosCalculadosUHEMaquinas <- df.dadosCalculadosUHEMaquinas %>%
  mutate(A08_VL_POTENCIA_MAXIMA =
           (A08_VL_POTENCIA_MAXIMA + ifelse((A04_NR_CONJUNTO == 1 & VL_POT_EXP > 0) ,
                                            ifelse(A08_VL_ALTURA_LIQUIDA >= A04_VL_ALTURA_REFERENCIA,
                                                   VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP),
                                                   VL_POT_EXP * (1 - A05_VL_TEIF) * (1 - A05_VL_IP) *
                                                     (A08_VL_ALTURA_LIQUIDA/A04_VL_ALTURA_REFERENCIA)^VL_COEF_TURBINA ),
                                            0)))

df.potenciaMaximaUHE <- df.dadosCalculadosUHEMaquinas %>%
  group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A03_CD_USINA, A08_NR_MES, A08_NR_SERIE) %>%
  summarise(A08_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>% ungroup()

# remove data frame já utilizado
rm(df.dadosCalculadosUHEMaquinas)

df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.potenciaMaximaUHE,
                                    by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A03_CD_USINA", "A08_NR_MES", "A08_NR_SERIE"))

# remove data frame já utilizado
rm(df.potenciaMaximaUHE)

###################################################################################################################################
# Segundo Passo: Segundo Passo: Modula a geração na ponta, respeitando a geração média (GHTOT(per,res)) durante as horas do mês,
# o GHMIN(per,ser) fora na hora da ponta e a máxima contribuição
# POTENCIA MÁXIMA(per,ser) durante o numero de horas em que a ponta ocorre
###################################################################################################################################
# GHMIN CALCULADO ORIGINALMENTE

# Atualiza GHMIN se potencia máxima for inferior ao GHMIN
df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL =
           A08_VL_ALTURA_LIQUIDA * A03_VL_PRODUTIBILIDADE * A05_VL_VAZAO_MINIMA,
         A08_VL_GERACAO_HIDRO_MINIMA = ifelse(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL > A08_VL_POTENCIA_MAXIMA,
                                              A08_VL_POTENCIA_MAXIMA,
                                              A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL))


df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
  group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
  summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA),
            A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
  ungroup()

df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                    by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))


# Calcula GHTOT por usina:
# Rateia o GHTOT pela potencia maxima, já abatida do GHMIN*/
# GHMIN_UHE = GHMIN_UHE X (1 - (GHMIN_SSIS - GHTOT_SSIS)/GHMIN_SSIS)


# GHTOT_UHE = GHMIN_UHE + ((GHTOT_SSIS - GHMIN_SSIS) X ((POT_MAX_UHE - GHMIN_UHE)/(POT_MAX_SSIS - GHMIN_SSIS)))
# Modula o GHTOT, maximizando a geração na horas de ponta*/
# POT_MODULADA = ((GHMEDIA X 730.5) - (GHMIN X (730.5 - HORASPONTA))) / HORASPONTA
teste <- df.dadosCalculadosUHE %>% filter(A03_CD_USINA == 154, A08_NR_MES == 201807, A08_NR_SERIE == 85)

df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_GERACAO_HIDRO_MINIMA = ifelse((A06_VL_GERACAO_HIDRAULICA < A09_VL_GERACAO_HIDRO_MINIMA_TMP),
                                              (A08_VL_GERACAO_HIDRO_MINIMA * (1 -
                                                (A09_VL_GERACAO_HIDRO_MINIMA_TMP - A06_VL_GERACAO_HIDRAULICA) / A09_VL_GERACAO_HIDRO_MINIMA_TMP)),
                                              A08_VL_GERACAO_HIDRO_MINIMA))

df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>% select(-A09_VL_GERACAO_HIDRO_MINIMA_TMP)


df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
  group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
  summarise(A09_VL_GERACAO_HIDRO_MINIMA_TMP = sum(A08_VL_GERACAO_HIDRO_MINIMA)) %>% ungroup()

df.dadosCalculadosUHE <- inner_join(df.dadosCalculadosUHE, df.dadosCalculadosSsist,
                                    by = c("A01_CD_MODELO", "A01_NR_CASO", "A01_TP_CASO", "A02_NR_REE", "A08_NR_MES", "A08_NR_SERIE"))


df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_GERACAO_HIDRO_MEDIA = (A08_VL_GERACAO_HIDRO_MINIMA + ((A06_VL_GERACAO_HIDRAULICA - A09_VL_GERACAO_HIDRO_MINIMA_TMP) *
                                                                        ((A08_VL_POTENCIA_MAXIMA - A08_VL_GERACAO_HIDRO_MINIMA) / (A09_VL_POTENCIA_MAXIMA - A09_VL_GERACAO_HIDRO_MINIMA_TMP)))),
         A08_VL_POTENCIA_MAXIMA_MODULADA = (((A08_VL_GERACAO_HIDRO_MEDIA * 730.5) -
                                               (A08_VL_GERACAO_HIDRO_MINIMA * (730.5 - df.dadosCaso$horasPonta))) / df.dadosCaso$horasPonta))



teste2 <- df.dadosCalculadosUHE %>% filter(A03_CD_USINA == 154, A08_NR_MES == 201807, A08_NR_SERIE == 85)

# Ajusta potencia máxima de acordo com o limite superior
df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse(A08_VL_POTENCIA_MAXIMA_MODULADA > A08_VL_POTENCIA_MAXIMA,
                                                  A08_VL_POTENCIA_MAXIMA,
                                                  A08_VL_POTENCIA_MAXIMA_MODULADA),
         A08_VL_VAZAO_MAXIMA = 0,
         A08_VL_VAZAO_MAXIMA_MODULADA = 0,
         A08_VL_ALTURA_MODULADA = 0)

teste3 <- df.dadosCalculadosUHE %>% filter(A03_CD_USINA == 154, A08_NR_MES == 201807, A08_NR_SERIE == 85)

# Ajusta potencia máxima de Tucurui
df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  mutate(A08_VL_POTENCIA_MAXIMA_MODULADA = ifelse((A03_CD_USINA == codTucurui &
                                                     A08_VL_POTENCIA_MAXIMA_MODULADA > df.dadosCaso$gerLimiteTucurui &
                                                     A08_VL_COTA_OPERATIVA < df.dadosCaso$cotaLimiteTucurui),
                                                  df.dadosCaso$gerLimiteTucurui,
                                                  A08_VL_POTENCIA_MAXIMA_MODULADA))


df.dadosCalculadosUHE <- df.dadosCalculadosUHE %>%
  select(A01_TP_CASO, A01_NR_CASO, A01_CD_MODELO, A03_CD_USINA, A08_NR_MES,
         A08_NR_SERIE, A02_NR_REE, A08_VL_VOLUME_OPERATIVO,
         A08_VL_COTA_OPERATIVA, A08_VL_ALTURA_LIQUIDA, A08_VL_VAZAO_MAXIMA,
         A08_VL_POTENCIA_MAXIMA, A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
         A08_VL_GERACAO_HIDRO_MINIMA, A08_VL_GERACAO_HIDRO_MEDIA,
         A08_VL_VAZAO_MAXIMA_MODULADA, A08_VL_ALTURA_MODULADA,
         A08_VL_POTENCIA_MAXIMA_MODULADA)

# limpa base BPO_A08_DADOS_CALCULADOS_UHE
dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
dbExecute(conexaoSQLite, "PRAGMA journal_mode = TRUNCATE;")
sql <- paste0("DELETE FROM BPO_A08_DADOS_CALCULADOS_UHE
              WHERE
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo, ";")
dbExecute(conexaoSQLite, sql)

# grava dados calculados na BPO_A08_DADOS_CALCULADOS_UHE
dbWriteTable(conexaoSQLite, "BPO_A08_DADOS_CALCULADOS_UHE", df.dadosCalculadosUHE, append = T)
dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")


df.dadosCalculadosSsist <- df.dadosCalculadosUHE %>%
  group_by(A01_CD_MODELO, A01_TP_CASO, A01_NR_CASO, A02_NR_REE, A08_NR_MES, A08_NR_SERIE) %>%
  summarise(A09_VL_GERACAO_HIDRO_MINIMA = sum(A08_VL_GERACAO_HIDRO_MINIMA),
            A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = sum(A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),
            A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = sum(A08_VL_POTENCIA_MAXIMA_MODULADA),
            A09_VL_POTENCIA_MAXIMA = sum(A08_VL_POTENCIA_MAXIMA)) %>%
  ungroup()

# corrigindo nome das colunas para ficar igual a tabela BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
colnames(df.dadosCalculadosSsist)[5:6] <- c("A09_NR_MES", "A09_NR_SERIE")
#

sql <- paste0("SELECT
              A01_CD_MODELO,
              A01_TP_CASO,
              A01_NR_CASO,
              A02_NR_REE,
              A06_NR_MES AS A09_NR_MES,
              A06_NR_SERIE AS A09_NR_SERIE,
              0 AS A09_VL_GERACAO_HIDRO_MINIMA,
              0 AS A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL,
              A06_VL_GERACAO_HIDRAULICA + A06_VL_SUBMOTORIZACAO AS A09_VL_DISPONIBILIDADE_MAXIMA_PONTA,
              0 AS A09_VL_POTENCIA_MAXIMA
              FROM BPO_A06_SAIDA_HIDRO_NEWAVE
              WHERE
              A02_NR_REE IN (SELECT A02_NR_REE FROM BPO_A02_REES
              WHERE A02_TP_CALC_POTENCIA IN (2, 3) AND A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND A01_CD_MODELO = ", codModelo, ") AND
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo, ";")
df.dadosSsistNaoModulam <- dbGetQuery(conexaoSQLite, sql)

# concatena as REEs que modulam com as que não modulam para gravar na base
df.dadosCalculadosSsist <- rbind(df.dadosCalculadosSsist,df.dadosSsistNaoModulam)


# Para buscar o subsistema
df.dadosCalculadosSsist <- inner_join(df.dadosCalculadosSsist, df.subsistemas,
                                    by = c("A02_NR_REE"))

# Não precisa mais de REE e Descrição
df.dadosCalculadosSsist <- df.dadosCalculadosSsist %>%
  select(-A02_NR_REE, -A02_TX_DESCRICAO_REE)

# Agrupa os dados calculados por Subsistema, totalizando
df.dadosCalculadosSsistAgrup <- df.dadosCalculadosSsist %>%
  group_by(A01_CD_MODELO,A01_NR_CASO,A01_TP_CASO,A02_NR_SUBSISTEMA,A09_NR_MES,A09_NR_SERIE) %>%
  summarise(A09_VL_GERACAO_HIDRO_MINIMA = round(sum(A09_VL_GERACAO_HIDRO_MINIMA),4),
            A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL = round(sum(A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL),4),
            A09_VL_DISPONIBILIDADE_MAXIMA_PONTA = round(sum(A09_VL_DISPONIBILIDADE_MAXIMA_PONTA),4),
            A09_VL_POTENCIA_MAXIMA = round(sum(A09_VL_POTENCIA_MAXIMA),4)) %>% ungroup()

# remove data frame já utilizado
rm(df.dadosCalculadosSsist)

# limpa BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA de outras execuções para o mesmo caso
dbExecute(conexaoSQLite, "PRAGMA locking_mode = EXCLUSIVE;")
sql <- paste0("DELETE FROM BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
              WHERE
              A01_TP_CASO = ", tipoCaso, " AND
              A01_NR_CASO = ", numeroCaso, " AND
              A01_CD_MODELO = ", codModelo, ";")
dbExecute(conexaoSQLite, sql)

dbWriteTable(conexaoSQLite, "BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA", df.dadosCalculadosSsistAgrup, append = T)
dbExecute(conexaoSQLite, "PRAGMA locking_mode = NORMAL;")

# fecha conexão com a base SQLite
dbDisconnect(conexaoSQLite)

# verifica se a pasta local para os arquivos temporários do balanço existem. Caso não, cria a pasta.
if (!dir.exists("C:/CacheRBalanco")) {
  dir.create("C:/CacheRBalanco")
}

#### Dados de entrada pelo usuario ####
# dados do balanço - valores calibrados
# os valores de CVU da transmissão, hidro e outras renováveis foram calibrados para forçarem o modelo a os considerar no balanço de forma ordenada,
# evitando soluções degeneradas ou irreais mas matematicamente aceitas. Contudo, se os custos das gerações aumentarem, esses valores devem ser recalibrados
# para evitar problemas de escalonamento.
cvuTransmissao <- 2e-6
cvuHidro <- 3e-5
cvuRenovaveis <- 1e-5
cvuOutrasTermicas <- 0.1 # valores de cvu 0
#### FIM dos dados de entrada pelo usuario ####
# chama função de balanço
calculaBalancoParalelo(localizacaoBase, tipoCaso, numeroCaso, codModelo, cvuTransmissao, cvuHidro, cvuRenovaveis, cvuOutrasTermicas,
                       balancoResumido)

# exibe tempo de execução
tempoExecucao <- toc()
tempoExecucao <- round(tempoExecucao$toc - tempoExecucao$tic, 0) %>% as.numeric()
cat(paste0("tempo de execução: ", tempoExecucao %/% 3600, " h. ",
           (tempoExecucao - (tempoExecucao %/% 3600 * 3600)) %/% 60, " min. ", tempoExecucao %% 60, " seg."))
