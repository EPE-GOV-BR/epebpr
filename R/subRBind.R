#' RBind em data frames dentro de listas
#'
#' Funcao para efetuar o rbind nos data frames dentro de listas que sao a saida da funcao balancoPeriodo()
#'
#' @param lista1 lista com data frames
#' @param lista2 lista com data frames. Ambas as listas devem ter a mesma estrutura
#' @return \code{lt.saida} lista onde os data frames de lista1 e lista2 foram agrupados (rbind) seguindo cada estrutura de data frame
#'
#' @examples
#' subRBind(lista1, lista2)
#'
subRBind <- function(lista1, lista2) {
  subItens <- names(lista1)
  if (length(lista1) == 2) {
    df1 <- rbind(lista1[[subItens[1]]], lista2[[subItens[1]]])
    df2 <- rbind(lista1[[subItens[2]]], lista2[[subItens[2]]])
    lt.saida <- list(df1 = df1, df2 = df2)
  } else {
    df1 <- rbind(lista1[[subItens[1]]], lista2[[subItens[1]]])
    df2 <- rbind(lista1[[subItens[2]]], lista2[[subItens[2]]])
    df3 <- rbind(lista1[[subItens[3]]], lista2[[subItens[3]]])
    lt.saida <- list(df1 = df1, df2 = df2, df3 = df3)
  }
  names(lt.saida) <- subItens
  return(lt.saida)
}
