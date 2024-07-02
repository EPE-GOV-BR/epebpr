--
-- File generated with SQLiteStudio v3.2.1 on qui dez 5 13:40:37 2019
--
-- Text encoding used: System
--
PRAGMA foreign_keys = off;
BEGIN TRANSACTION;

-- Table: BPO_A01_CASOS_ANALISE
DROP TABLE IF EXISTS BPO_A01_CASOS_ANALISE;

CREATE TABLE BPO_A01_CASOS_ANALISE (
    A01_TP_CASO                   INTEGER NOT NULL,
    A01_NR_CASO                   INTEGER NOT NULL,
    A01_CD_MODELO                 INTEGER NOT NULL,
    A01_TX_DESCRICAO              TEXT,
    A01_NR_MES_INICIO             INTEGER,
    A01_NR_MES_FIM                INTEGER,
    A01_NR_HORAS_PONTA            INTEGER,
    A01_NR_COTA_LIMITE_TUCURUI    INTEGER,
    A01_NR_GERACAO_LIMITE_TUCURUI INTEGER,
    -- A01_VL_RESERVA_OPERATIVA      REAL,
    A01_IN_DEMANDA_LIQUIDA        INTEGER,
    A01_NR_SERIES_HIDRO           INTEGER,
    A01_NR_MES_INICIO_MDI         INTEGER,
    A01_NR_MES_FIM_MDI            INTEGER,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO
    )
);


-- Table: BPO_A02_REES
DROP TABLE IF EXISTS BPO_A02_REES;

CREATE TABLE BPO_A02_REES (
    A01_TP_CASO          INTEGER NOT NULL,
    A01_NR_CASO          INTEGER NOT NULL,
    A01_CD_MODELO        INTEGER NOT NULL,
    A02_NR_REE           INTEGER NOT NULL,
    A02_TX_DESCRICAO_REE TEXT,
    A02_TP_CALC_POTENCIA INTEGER,
    A02_NR_SUBSISTEMA    INTEGER,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_REE
    )
);


-- Table: BPO_A02_SUBSISTEMAS
DROP TABLE IF EXISTS BPO_A02_SUBSISTEMAS;

CREATE TABLE BPO_A02_SUBSISTEMAS (
    A01_TP_CASO                 INTEGER NOT NULL,
    A01_NR_CASO                 INTEGER NOT NULL,
    A01_CD_MODELO               INTEGER NOT NULL,
    A02_NR_SUBSISTEMA           INTEGER NOT NULL,
    A02_TX_DESCRICAO_SUBSISTEMA TEXT,
    A02_VL_CUSTO_DEFICIT        REAL,
    A02_TP_FICTICIO             INTEGER,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA
    )
);


-- Table: BPO_A03_DADOS_UHE
DROP TABLE IF EXISTS BPO_A03_DADOS_UHE;

CREATE TABLE BPO_A03_DADOS_UHE (
    A01_TP_CASO            INTEGER NOT NULL,
    A01_NR_CASO            INTEGER NOT NULL,
    A01_CD_MODELO          INTEGER NOT NULL,
    A03_CD_USINA           INTEGER NOT NULL,
    A02_NR_REE             INTEGER,
    A03_TX_USINA           TEXT,
    A03_TX_STATUS          TEXT,
    A03_CD_TIPO            INTEGER,
    A03_NR_PCV_0           REAL,
    A03_NR_PCV_1           REAL,
    A03_NR_PCV_2           REAL,
    A03_NR_PCV_3           REAL,
    A03_NR_PCV_4           REAL,
    A03_NR_PVNJ_0          REAL,
    A03_NR_PVNJ_1          REAL,
    A03_NR_PVNJ_2          REAL,
    A03_NR_PVNJ_3          REAL,
    A03_NR_PVNJ_4          REAL,
    A03_TP_PERDA           INTEGER,
    A03_VL_PERDA           REAL,
    A03_TP_TURBINA         INTEGER,
    A03_VL_PRODUTIBILIDADE REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A03_CD_USINA
    )
);


-- Table: BPO_A04_MAQUINAS_UHE
DROP TABLE IF EXISTS BPO_A04_MAQUINAS_UHE;

CREATE TABLE BPO_A04_MAQUINAS_UHE (
    A01_TP_CASO              INTEGER NOT NULL,
    A01_NR_CASO              INTEGER NOT NULL,
    A01_CD_MODELO            INTEGER NOT NULL,
    A03_CD_USINA             INTEGER NOT NULL,
    A04_NR_CONJUNTO          INTEGER NOT NULL,
    A04_NR_MAQUINAS          INTEGER,
    A04_VL_POTENCIA          INTEGER,
    A04_VL_ALTURA_REFERENCIA INTEGER,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A03_CD_USINA,
        A04_NR_CONJUNTO
    )
);


-- Table: BPO_A05_DADOS_VIGENTES_UHE
DROP TABLE IF EXISTS BPO_A05_DADOS_VIGENTES_UHE;

CREATE TABLE BPO_A05_DADOS_VIGENTES_UHE (
    A01_TP_CASO              INTEGER NOT NULL,
    A01_NR_CASO              INTEGER NOT NULL,
    A01_CD_MODELO            INTEGER NOT NULL,
    A03_CD_USINA             INTEGER NOT NULL,
    A05_NR_MES               INTEGER NOT NULL,
    A02_NR_REE               INTEGER NOT NULL,
    A05_NR_CANAL_FUGA_MEDIO  REAL,
    A05_VL_VOL_MAX           REAL,
    A05_VL_VOL_MIN           REAL,
    A05_VL_VAZAO_MINIMA      REAL,
    A05_NR_CONJUNTOS         INTEGER,
    A05_VL_TEIF              REAL,
    A05_VL_IP                REAL,
    A05_VL_POTENCIA          REAL,
    A05_VL_POTENCIA_EXPANSAO REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A03_CD_USINA,
        A05_NR_MES
    )
);


-- Table: BPO_A06_SAIDA_HIDRO_NEWAVE
DROP TABLE IF EXISTS BPO_A06_SAIDA_HIDRO_NEWAVE;

CREATE TABLE BPO_A06_SAIDA_HIDRO_NEWAVE (
    A01_TP_CASO                  INTEGER NOT NULL,
    A01_NR_CASO                  INTEGER NOT NULL,
    A01_CD_MODELO                INTEGER NOT NULL,
    A02_NR_REE                   INTEGER NOT NULL,
    A06_NR_MES                   INTEGER NOT NULL,
    A06_NR_SERIE                 INTEGER NOT NULL,
    A06_VL_PERC_ARMAZENAMENTO    REAL,
    A06_VL_GERACAO_HIDRAULICA    REAL,
    A06_VL_SUBMOTORIZACAO        REAL,
    A06_VL_VERTIMENTO_TURBINAVEL REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_REE,
        A06_NR_MES,
        A06_NR_SERIE
    )
);


-- Table: BPO_A07_SAIDA_HIDRO_SUISHI
DROP TABLE IF EXISTS BPO_A07_SAIDA_HIDRO_SUISHI;

CREATE TABLE BPO_A07_SAIDA_HIDRO_SUISHI (
    A01_TP_CASO            INTEGER NOT NULL,
    A01_NR_CASO            INTEGER NOT NULL,
    A01_CD_MODELO          INTEGER NOT NULL,
    A07_NR_MES             INTEGER NOT NULL,
    A07_NR_SERIE           INTEGER NOT NULL,
    A02_NR_REE             INTEGER NOT NULL,
    A07_VL_VAZAO_TURBINADA REAL,
    A07_VL_VAZAO_VERTIDA   REAL,
    A07_VL_VOLUME_FINAL    REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A07_NR_MES,
        A07_NR_SERIE
    )
);


-- Table: BPO_A08_DADOS_CALCULADOS_UHE
DROP TABLE IF EXISTS BPO_A08_DADOS_CALCULADOS_UHE;

CREATE TABLE BPO_A08_DADOS_CALCULADOS_UHE (
    A01_TP_CASO                          INTEGER NOT NULL,
    A01_NR_CASO                          INTEGER NOT NULL,
    A01_CD_MODELO                        INTEGER NOT NULL,
    A03_CD_USINA                         INTEGER NOT NULL,
    A08_NR_MES                           INTEGER NOT NULL,
    A08_NR_SERIE                         INTEGER NOT NULL,
    A02_NR_REE                           INTEGER NOT NULL,
    A08_VL_VOLUME_OPERATIVO              REAL,
    A08_VL_COTA_OPERATIVA                REAL,
    A08_VL_ALTURA_LIQUIDA                REAL,
    A08_VL_VAZAO_MAXIMA                  REAL,
    A08_VL_POTENCIA_MAXIMA               REAL,
    A08_VL_GERACAO_HIDRO_MINIMA_ORIGINAL REAL,
    A08_VL_GERACAO_HIDRO_MINIMA          REAL,
    A08_VL_GERACAO_HIDRO_MEDIA           REAL,
    A08_VL_VAZAO_MAXIMA_MODULADA         REAL,
    A08_VL_ALTURA_MODULADA               REAL,
    A08_VL_POTENCIA_MAXIMA_MODULADA      REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A03_CD_USINA,
        A08_NR_MES,
        A08_NR_SERIE
    )
);


-- Table: BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA
DROP TABLE IF EXISTS BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA;

CREATE TABLE BPO_A09_DISPONIBILIDADE_HIDRO_PONTA_SUBSISTEMA (
    A01_TP_CASO                          INTEGER NOT NULL,
    A01_NR_CASO                          INTEGER NOT NULL,
    A01_CD_MODELO                        INTEGER NOT NULL,
    A02_NR_SUBSISTEMA                    INTEGER NOT NULL,
    A09_NR_MES                           INTEGER NOT NULL,
    A09_NR_SERIE                         INTEGER NOT NULL,
    A09_VL_POTENCIA_MAXIMA               REAL,
    A09_VL_GERACAO_HIDRO_MINIMA_ORIGINAL REAL,
    A09_VL_GERACAO_HIDRO_MINIMA          REAL,
    A09_VL_DISPONIBILIDADE_MAXIMA_PONTA  REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A09_NR_MES,
        A09_NR_SERIE
    )
);


-- Table: BPO_A10_DEMANDA
DROP TABLE IF EXISTS BPO_A10_DEMANDA;

CREATE TABLE BPO_A10_DEMANDA (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A02_NR_SUBSISTEMA     INTEGER NOT NULL,
    A10_NR_MES            INTEGER NOT NULL,
    A10_NR_TIPO_DEMANDA INTEGER,
    A10_VL_DEMANDA        REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A10_NR_MES,
        A10_NR_TIPO_DEMANDA
    )
);


-- Table: BPO_A11_INTERCAMBIOS
DROP TABLE IF EXISTS BPO_A11_INTERCAMBIOS;

CREATE TABLE BPO_A11_INTERCAMBIOS (
    A01_TP_CASO               INTEGER NOT NULL,
    A01_NR_CASO               INTEGER NOT NULL,
    A01_CD_MODELO             INTEGER NOT NULL,
    A11_NR_SUBSISTEMA_ORIGEM  INTEGER NOT NULL,
    A11_NR_SUBSISTEMA_DESTINO INTEGER NOT NULL,
    A11_NR_MES                INTEGER NOT NULL,
    A11_VL_LIMITE_INTERCAMBIO REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A11_NR_SUBSISTEMA_ORIGEM,
        A11_NR_SUBSISTEMA_DESTINO,
        A11_NR_MES
    )
);


-- Table: BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO
DROP TABLE IF EXISTS BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO;

CREATE TABLE BPO_A12_LIMITE_AGRUPAMENTOS_INTERCAMBIO (
    A01_TP_CASO               INTEGER NOT NULL,
    A01_NR_CASO               INTEGER NOT NULL,
    A01_CD_MODELO             INTEGER NOT NULL,
    A12_NR_MES                INTEGER NOT NULL,
    A12_CD_AGRUPAMENTO        INTEGER NOT NULL,
    A12_VL_LIMITE_INTERCAMBIO REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A12_NR_MES,
        A12_CD_AGRUPAMENTO
    )
);


-- Table: BPO_A13_DISPONIBILIDADE_OFR
DROP TABLE IF EXISTS BPO_A13_DISPONIBILIDADE_OFR;

CREATE TABLE BPO_A13_DISPONIBILIDADE_OFR (
    A01_TP_CASO                         INTEGER NOT NULL,
    A01_NR_CASO                         INTEGER NOT NULL,
    A01_CD_MODELO                       INTEGER NOT NULL,
    A02_NR_SUBSISTEMA                   INTEGER NOT NULL,
    A13_NR_MES                          INTEGER NOT NULL,
    A13_CD_TIPO_FONTE                   INTEGER NOT NULL,
    A13_VL_DISPONIBILIDADE_MAXIMA_PONTA REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A13_NR_MES,
        A13_CD_TIPO_FONTE
    )
);


-- Table: BPO_A14_DISPONIBILIDADE_UTE
DROP TABLE IF EXISTS BPO_A14_DISPONIBILIDADE_UTE;

CREATE TABLE BPO_A14_DISPONIBILIDADE_UTE (
    A01_TP_CASO                         INTEGER NOT NULL,
    A01_NR_CASO                         INTEGER NOT NULL,
    A01_CD_MODELO                       INTEGER NOT NULL,
    A02_NR_SUBSISTEMA                   INTEGER NOT NULL,
    A14_NR_MES                          INTEGER NOT NULL,
    A14_CD_USINA                        INTEGER NOT NULL,
    A14_VL_POTENCIA                     REAL,
    A14_VL_FATOR_CAPACIDADE             REAL,
    A14_VL_PERC_TEIF                    REAL,
    A14_VL_PERC_IP                      REAL,
    A14_VL_INFLEXIBILIDADE              REAL,
    A14_VL_DISPONIBILIDADE_MAXIMA_PONTA REAL,
    A14_VL_CVU                          REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A14_NR_MES,
        A14_CD_USINA
    )
);


-- Table: BPO_A15_AGRUPAMENTOS_INTERCAMBIO
DROP TABLE IF EXISTS BPO_A15_AGRUPAMENTOS_INTERCAMBIO;

CREATE TABLE BPO_A15_AGRUPAMENTOS_INTERCAMBIO (
    A01_TP_CASO               INTEGER NOT NULL,
    A01_NR_CASO               INTEGER NOT NULL,
    A01_CD_MODELO             INTEGER NOT NULL,
    A12_CD_AGRUPAMENTO        INTEGER NOT NULL,
    A11_NR_SUBSISTEMA_ORIGEM  INTEGER NOT NULL,
    A11_NR_SUBSISTEMA_DESTINO INTEGER NOT NULL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A12_CD_AGRUPAMENTO,
        A11_NR_SUBSISTEMA_ORIGEM,
        A11_NR_SUBSISTEMA_DESTINO
    )
);


-- Table: BPO_A16_BALANCO
DROP TABLE IF EXISTS BPO_A16_BALANCO;

CREATE TABLE BPO_A16_BALANCO (
    A01_TP_CASO                    INTEGER NOT NULL,
    A01_NR_CASO                    INTEGER NOT NULL,
    A01_CD_MODELO                  INTEGER NOT NULL,
    A09_NR_MES                     INTEGER NOT NULL,
    A09_NR_SERIE                   INTEGER NOT NULL,
    A16_TP_GERACAO                 TEXT    NOT NULL,
    A02_NR_SUBSISTEMA              INTEGER NOT NULL,
    A16_VL_GMIN                    REAL,
    A16_VL_DESPACHO                REAL,
    A16_VL_NAO_DESPACHADO          REAL,
    A16_VL_DESPACHO_REDE_ILIMITADA REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A09_NR_MES,
        A09_NR_SERIE,
        A16_TP_GERACAO,
        A02_NR_SUBSISTEMA
    )
);


-- Table: BPO_A17_BALANCO_GERADOR
DROP TABLE IF EXISTS BPO_A17_BALANCO_GERADOR;

CREATE TABLE BPO_A17_BALANCO_GERADOR (
    A01_TP_CASO                    INTEGER NOT NULL,
    A01_NR_CASO                    INTEGER NOT NULL,
    A01_CD_MODELO                  INTEGER NOT NULL,
    A09_NR_MES                     INTEGER NOT NULL,
    A09_NR_SERIE                   INTEGER NOT NULL,
    A16_TP_GERACAO                 TEXT    NOT NULL,
    A17_CD_USINA                   INTEGER NOT NULL,
    A02_NR_SUBSISTEMA              INTEGER NOT NULL,
    A17_VL_GMIN                    REAL,
    A17_VL_DESPACHO                REAL,
    A17_VL_NAO_DESPACHADO          REAL,
    A17_VL_DESPACHO_REDE_ILIMITADA REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A09_NR_MES,
        A09_NR_SERIE,
        A16_TP_GERACAO,
        A17_CD_USINA,
        A02_NR_SUBSISTEMA
    )
);


-- Table: BPO_A18_TIPOS_OFR
DROP TABLE IF EXISTS BPO_A18_TIPOS_OFR;

CREATE TABLE BPO_A18_TIPOS_OFR (
    A18_CD_TIPO_FONTE         INTEGER NOT NULL,
    A18_TX_DESCRICAO          CHAR,
    A18_TP_CONTRIBUICAO_PONTA INTEGER,
    PRIMARY KEY (
        A18_CD_TIPO_FONTE
    )
);


-- Table: BPO_A19_FATOR_PONTA_OFR
DROP TABLE IF EXISTS BPO_A19_FATOR_PONTA_OFR;

CREATE TABLE BPO_A19_FATOR_PONTA_OFR (
    A01_TP_CASO       INTEGER NOT NULL,
    A01_NR_CASO       INTEGER NOT NULL,
    A01_CD_MODELO     INTEGER NOT NULL,
    A02_NR_SUBSISTEMA INTEGER NOT NULL,
    A18_CD_TIPO_FONTE INTEGER NOT NULL,
    A19_NR_MES        INTEGER NOT NULL,
    A19_VL_FATOR      REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A18_CD_TIPO_FONTE,
        A19_NR_MES
    )
);


-- Table: BPO_A20_BALANCO_SUBSISTEMA
DROP TABLE IF EXISTS BPO_A20_BALANCO_SUBSISTEMA;

CREATE TABLE BPO_A20_BALANCO_SUBSISTEMA (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A20_NR_MES            INTEGER NOT NULL,
    A20_NR_SERIE          INTEGER NOT NULL,
    A02_NR_SUBSISTEMA     INTEGER NOT NULL,
    A20_VL_CMO            REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A20_NR_MES,
        A20_NR_SERIE,
        A02_NR_SUBSISTEMA
    )
);

-- Table: BPO_A21_RESERVA
DROP TABLE IF EXISTS BPO_A21_RESERVA;

CREATE TABLE BPO_A21_RESERVA (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A02_NR_SUBSISTEMA     INTEGER NOT NULL,
    A21_NR_MES            INTEGER NOT NULL,
    A10_NR_TIPO_DEMANDA   INTEGER NOT NULL,
    A21_VL_RESERVA_CARGA  REAL,
    A21_VL_RESERVA_FONTES REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A21_NR_MES,
        A10_NR_TIPO_DEMANDA
    )
);

-- Table: BPO_A22_CVAR_MENSAL_SIN
DROP TABLE IF EXISTS BPO_A22_CVAR_MENSAL_SIN;

CREATE TABLE BPO_A22_CVAR_MENSAL_SIN (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A22_NR_MES            INTEGER NOT NULL,
    A22_TX_PERCENT_CVAR   TEXT,
    A22_VL_CVAR           REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A22_NR_MES,
        A22_TX_PERCENT_CVAR
    )
);

-- Table: BPO_A23_CVAR_ANUAL_SIN
DROP TABLE IF EXISTS BPO_A23_CVAR_ANUAL_SIN;

CREATE TABLE BPO_A23_CVAR_ANUAL_SIN (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A23_NR_ANO            INTEGER NOT NULL,
    A23_TX_PERCENT_CVAR   TEXT,
    A23_VL_CVAR           REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A23_NR_ANO,
        A23_TX_PERCENT_CVAR
    )
);

-- Table: BPO_A24_CVAR_MENSAL_SUBS
DROP TABLE IF EXISTS BPO_A24_CVAR_MENSAL_SUBS;

CREATE TABLE BPO_A24_CVAR_MENSAL_SUBS (
    A01_TP_CASO                 INTEGER NOT NULL,
    A01_NR_CASO                 INTEGER NOT NULL,
    A01_CD_MODELO               INTEGER NOT NULL,
    A02_TX_DESCRICAO_SUBSISTEMA TEXT,
    A24_NR_MES                  INTEGER NOT NULL,
    A24_VL_CVAR_5               REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_TX_DESCRICAO_SUBSISTEMA,
        A24_NR_MES
    )
);

-- Table: BPO_A25_VAR_MENSAL_SIN
DROP TABLE IF EXISTS BPO_A25_VAR_MENSAL_SIN;

CREATE TABLE BPO_A25_VAR_MENSAL_SIN (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A25_NR_MES            INTEGER NOT NULL,
    A25_TX_PERCENT_VAR    TEXT,
    A25_VL_VAR            REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A25_NR_MES,
        A25_TX_PERCENT_VAR
    )
);

-- Table: BPO_A26_VAR_ANUAL_SIN
DROP TABLE IF EXISTS BPO_A26_VAR_ANUAL_SIN;

CREATE TABLE BPO_A26_VAR_ANUAL_SIN (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A26_NR_ANO            INTEGER NOT NULL,
    A26_TX_PERCENT_VAR    TEXT,
    A26_VL_VAR            REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A26_NR_ANO,
        A26_TX_PERCENT_VAR
    )
);

-- Table: BPO_A27_LOLP_MENSAL
DROP TABLE IF EXISTS BPO_A27_LOLP_MENSAL;

CREATE TABLE BPO_A27_LOLP_MENSAL (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A27_NR_MES            INTEGER NOT NULL,
    A27_VL_LOLP           REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A27_NR_MES
    )
);

-- Table: BPO_A28_LOLP_ANUAL
DROP TABLE IF EXISTS BPO_A28_LOLP_ANUAL;

CREATE TABLE BPO_A28_LOLP_ANUAL (
    A01_TP_CASO           INTEGER NOT NULL,
    A01_NR_CASO           INTEGER NOT NULL,
    A01_CD_MODELO         INTEGER NOT NULL,
    A28_NR_ANO            INTEGER NOT NULL,
    A28_VL_LOLP           REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A28_NR_ANO
    )
);

-- Table: BPO_A29_REQUISITOS_POTENCIA
DROP TABLE IF EXISTS BPO_A29_REQUISITOS_POTENCIA;

CREATE TABLE BPO_A29_REQUISITOS_POTENCIA (
    A01_TP_CASO             INTEGER NOT NULL,
    A01_NR_CASO             INTEGER NOT NULL,
    A01_CD_MODELO           INTEGER NOT NULL,
    A29_NR_MES              INTEGER NOT NULL,
    A29_VL_VIOLACAO_CRITERIO REAL,
    A29_VL_LIMITE_CRITERIO  REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A29_NR_MES
    )
);

-- Table: BPO_A30_REQUISITOS_POTENCIA_QUAD
DROP TABLE IF EXISTS BPO_A30_REQUISITOS_POTENCIA_QUAD;

CREATE TABLE BPO_A30_REQUISITOS_POTENCIA_QUAD (
    A01_TP_CASO             INTEGER NOT NULL,
    A01_NR_CASO             INTEGER NOT NULL,
    A01_CD_MODELO           INTEGER NOT NULL,
    A30_NR_ANO              INTEGER NOT NULL,
    A30_NR_QUADRIMESTRE     INTEGER NOT NULL,
    A30_VL_REQUISITO        REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A30_NR_ANO,
        A30_NR_QUADRIMESTRE
    )
);

-- Table: BPO_A31_DISPONIBILIDADE_UTE_GNL
DROP TABLE IF EXISTS BPO_A31_DISPONIBILIDADE_UTE_GNL;

CREATE TABLE BPO_A31_DISPONIBILIDADE_UTE_GNL (
    A01_TP_CASO                         INTEGER NOT NULL,
    A01_NR_CASO                         INTEGER NOT NULL,
    A01_CD_MODELO                       INTEGER NOT NULL,
    A02_NR_SUBSISTEMA                   INTEGER NOT NULL,
    A31_NR_MES                          INTEGER NOT NULL,
    A31_NR_SERIE                        INTEGER NOT NULL,
    A31_CD_USINA                        INTEGER NOT NULL,
    A31_VL_POTENCIA                     REAL,
    A31_VL_FATOR_CAPACIDADE             REAL,
    A31_VL_PERC_TEIF                    REAL,
    A31_VL_PERC_IP                      REAL,
    A31_VL_INFLEXIBILIDADE              REAL,
    A31_VL_DISPONIBILIDADE_MAXIMA_PONTA REAL,
    A31_VL_CVU                          REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A31_NR_MES,
        A31_NR_SERIE,
        A31_CD_USINA
    )
);

-- Table: BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO
DROP TABLE IF EXISTS BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO;

CREATE TABLE BPO_A32_DISPONIBILIDADE_ARMAZENAMENTO (
    A01_TP_CASO                         INTEGER NOT NULL,
    A01_NR_CASO                         INTEGER NOT NULL,
    A01_CD_MODELO                       INTEGER NOT NULL,
    A02_NR_SUBSISTEMA                   INTEGER NOT NULL,
    A32_NR_MES                          INTEGER NOT NULL,
    A32_VL_DISPONIBILIDADE_PONTA        REAL,
    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A02_NR_SUBSISTEMA,
        A32_NR_MES
    )
);

-- Table: BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA
DROP TABLE IF EXISTS BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA;

CREATE TABLE BPO_A33_DADOS_CALCULADOS_UHE_REE_TABELA (
    A01_TP_CASO                          INTEGER NOT NULL,
    A01_NR_CASO                          INTEGER NOT NULL,
    A01_CD_MODELO                        INTEGER NOT NULL,
    A02_NR_REE                           INTEGER NOT NULL,
    A03_CD_USINA                         INTEGER NOT NULL,
    A33_NR_MES                           INTEGER NOT NULL,
    A33_NR_SERIE                         INTEGER NOT NULL,
    A33_VL_GERACAO_HIDRO_REE             REAL,
    A33_VL_PRODUTIBILIDADE               REAL,
    A33_VL_PROPORCAO                     REAL,
    A33_VL_VAZAO                         REAL,
    A33_VL_GERACAO_HIDRO_CORRIGIDA       REAL,
    A33_VL_POTENCIA_MAXIMA               REAL,
    A33_VL_DISPONIBILIDADE_MAXIMA_PONTA  REAL,

    PRIMARY KEY (
        A01_TP_CASO,
        A01_NR_CASO,
        A01_CD_MODELO,
        A03_CD_USINA,
        A33_NR_MES,
        A33_NR_SERIE
    )
);

COMMIT TRANSACTION;
PRAGMA foreign_keys = on;
