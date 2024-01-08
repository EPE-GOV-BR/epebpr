#' Exporta os dados de distribuicao dos deficits
#'
#' @param baseSQLite nome e localizacao da base SQLite do balanco de potencia
#' @param tipoCaso valor inteiro. 1:PDE; 2:PMO e 3;Garantia Fisica
#' @param numeroCaso valor inteiro com o numero do caso
#' @param codModelo valor inteiro com o codigo do modelo. 1:NEWAVE; 2:SUISHI
#'
#' @return lt.distDef lista com os dados de distribuicao dos deficits, distribuicao de CVaR e comparacao de distribuicao de deficit com CVaR maximo
#'
#' @export
dadosDistDeficit <- function(baseSQLite, tipoCaso, numeroCaso, codModelo) {
  
  conexao <- dbConnect(RSQLite::SQLite(), baseSQLite)
  # query no banco com join para buscar defict e demanda por serie para calculo da profundidade
  squery <- paste0("SELECT 
                      A02.A02_TX_DESCRICAO_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES,
                      SUM(A16_VL_DESPACHO) AS DEFICIT,
                      A10.DEMANDA 
                    FROM 
                      BPO_A16_BALANCO AS A16,
                      ( SELECT 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO,
                          SUM(A10_VL_DEMANDA) AS DEMANDA
                        FROM BPO_A10_DEMANDA
                        GROUP BY 
                          A02_NR_SUBSISTEMA, 
                          A10_NR_MES,
                          A01_TP_CASO,
                          A01_NR_CASO,
                          A01_CD_MODELO
                      ) AS A10,
                      BPO_A02_SUBSISTEMAS AS A02
                    WHERE 
                      A16.A16_TP_GERACAO = 'DEFICIT_R' AND
                      A16.A01_TP_CASO = ", tipoCaso," AND
                      A16.A01_NR_CASO = ", numeroCaso," AND
                      A16.A01_CD_MODELO = ", codModelo, " AND
                      A10.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA AND
                      A10.A10_NR_MES = A16.A09_NR_MES AND
                      A10.A01_TP_CASO = A16.A01_TP_CASO AND
                      A10.A01_NR_CASO = A16.A01_NR_CASO AND
                      A10.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A01_TP_CASO = A16.A01_TP_CASO AND 
                      A02.A01_NR_CASO = A16.A01_NR_CASO AND 
                      A02.A01_CD_MODELO = A16.A01_CD_MODELO AND
                      A02.A02_NR_SUBSISTEMA = A16.A02_NR_SUBSISTEMA
                    GROUP BY
                      A16.A02_NR_SUBSISTEMA,
                      A16.A09_NR_SERIE,
                      A16.A09_NR_MES;")
  
  tib.resultados <- dbGetQuery(conexao, squery) %>% as_tibble()
  dbDisconnect(conexao)
  
  # filtra subsistemas sem demanda
  filtroSubsistemaSemDemanda <- tib.resultados %>% group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% summarise(demanda = sum(DEMANDA), .groups = "drop") %>% 
    filter(demanda > 0) %>% pull(A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.resultados <- tib.resultados %>% filter(A02_TX_DESCRICAO_SUBSISTEMA %in% filtroSubsistemaSemDemanda)
  
  tib.resultados <- tib.resultados %>% 
    mutate(mes = A09_NR_MES %% 100, ano = A09_NR_MES %/% 100) %>% 
    rename(subsistema = A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.defSIN <- tib.resultados %>% 
    group_by(A09_NR_SERIE, ano, mes) %>% 
    summarise(DEFICIT = sum(DEFICIT)) %>% 
    mutate(subsistema = "SIN")
  
  tib.distDef <- tib.resultados %>% 
    select(subsistema, A09_NR_SERIE, DEFICIT, ano, mes) %>% 
    rbind(tib.defSIN) %>% 
    group_by(subsistema, ano) %>% 
    summarise(quant = list(as_tibble(as.list(quantile(DEFICIT, probs = seq(0, 1, 0.005)))))) %>% 
    unnest(quant) %>% 
    pivot_longer(cols = 3:203, names_to = "P", values_to = "DEFICIT") %>% 
    mutate(P = str_replace(P, "%", ""), 
           P = as.numeric(P)/100,
           P = 1 - P) %>% 
    arrange(subsistema, ano, P)
  
  colunasCvar <- sprintf("cvar%s", seq(1:10))
  
  tib.distCvar <- tib.resultados %>%
    select(subsistema, A09_NR_SERIE, DEFICIT, ano, mes) %>% 
    rbind(tib.defSIN) %>% 
    mutate(!!!setNames(rep(NA, length(colunasCvar)), colunasCvar)) %>% 
    group_by(subsistema, ano, mes) %>%
    summarise(across(starts_with("cvar"), ~ cvar(DEFICIT, as.numeric(str_replace(cur_column(), "cvar", ""))/100)))
  
  tib.distDefxCvarMax <- tib.distCvar %>% 
    pivot_longer(4:13, names_to = "cvar", values_to = "valor") %>% 
    group_by(subsistema, ano, cvar) %>% 
    summarise(valor = max(valor)) %>% 
    right_join(tib.distDef, by = c("subsistema", "ano"), relationship = "many-to-many") %>% 
    pivot_wider(names_from = cvar, values_from = valor) %>% 
    relocate(cvar10, .after = last_col())
  
  lt.distDef <- list(distDef = tib.distDef, distCvar = tib.distCvar, distDefxCvarMax = tib.distDefxCvarMax)
  
  return(lt.distDef)
}
