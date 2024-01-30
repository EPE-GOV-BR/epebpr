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
  
  conexao <- DBI::dbConnect(RSQLite::SQLite(), baseSQLite)
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
                      A16.A16_TP_GERACAO = 'DEFICIT' AND
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
  
  tib.resultados <- DBI::dbGetQuery(conexao, squery) %>% 
    tidyr::as_tibble()
  
  DBI::dbDisconnect(conexao)
  
  # filtra subsistemas sem demanda
  filtroSubsistemaSemDemanda <- tib.resultados %>% 
    dplyr::group_by(A02_TX_DESCRICAO_SUBSISTEMA) %>% 
    dplyr::summarise(demanda = sum(DEMANDA), .groups = "drop") %>% 
    dplyr::filter(demanda > 0) %>% 
    dplyr::pull(A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.resultados <- tib.resultados %>% 
    dplyr::filter(A02_TX_DESCRICAO_SUBSISTEMA %in% filtroSubsistemaSemDemanda)
  
  tib.resultados <- tib.resultados %>% 
    dplyr::mutate(mes = A09_NR_MES %% 100, ano = A09_NR_MES %/% 100) %>% 
    dplyr::rename(subsistema = A02_TX_DESCRICAO_SUBSISTEMA)
  
  tib.defSIN <- tib.resultados %>% 
    dplyr::group_by(A09_NR_SERIE, ano, mes) %>% 
    dplyr::summarise(DEFICIT = sum(DEFICIT)) %>% 
    dplyr::mutate(subsistema = "SIN")
  
  tib.distDef <- tib.resultados %>% 
    dplyr::select(subsistema, A09_NR_SERIE, DEFICIT, ano, mes) %>% 
    rbind(tib.defSIN) %>% 
    dplyr::group_by(subsistema, ano) %>% 
    dplyr::summarise(quant = list(tidyr::as_tibble(as.list(stats::quantile(DEFICIT, probs = seq(0, 1, 0.005)))))) %>% 
    tidyr::unnest(quant) %>% 
    tidyr::pivot_longer(cols = 3:203, names_to = "P", values_to = "DEFICIT") %>% 
    dplyr::mutate(P = stringr::str_replace(P, "%", ""), 
                  P = as.numeric(P)/100,
                  P = 1 - P) %>% 
    dplyr::arrange(subsistema, ano, P)
  
  colunasCvar <- sprintf("cvar%s", seq(1:10))
  
  tib.distCvar <- tib.resultados %>%
    dplyr::select(subsistema, A09_NR_SERIE, DEFICIT, ano, mes) %>% 
    rbind(tib.defSIN) %>% 
    dplyr::mutate(!!!setNames(rep(NA, length(colunasCvar)), colunasCvar)) %>% 
    dplyr::group_by(subsistema, ano, mes) %>%
    dplyr::summarise(dplyr::across(starts_with("cvar"), ~ cvar(DEFICIT, as.numeric(stringr::str_replace(dplyr::cur_column(), "cvar", ""))/100)))
  
  tib.distDefxCvarMax <- tib.distCvar %>% 
    tidyr::pivot_longer(4:13, names_to = "cvar", values_to = "valor") %>% 
    dplyr::group_by(subsistema, ano, cvar) %>% 
    dplyr::summarise(valor = max(valor)) %>% 
    dplyr::right_join(tib.distDef, by = c("subsistema", "ano"), relationship = "many-to-many") %>% 
    tidyr::pivot_wider(names_from = cvar, values_from = valor) %>% 
    dplyr::relocate(cvar10, .after = dplyr::last_col())
  
  lt.distDef <- list(distDef = tib.distDef, distCvar = tib.distCvar, distDefxCvarMax = tib.distDefxCvarMax)
  
  return(lt.distDef)
}
