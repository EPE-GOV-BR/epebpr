#' Calcula conditional-value-at-risk (CVaR)
#'
#' Cria a funcao que calcula o valor de risco condicional (CVaR)
#'
#' @param x vetor com valores a se calcular o CVaR
#' @param cvar valor de cvar aplicado. Ex. 0.01, 0.02, 0.05
#'
#' @return valor de cvar aplicado no vetor x
#'
#' @export
cvar <- function(x, cvar) {
  mean(head(sort(x, decreasing = T), round((length(x) * cvar), 0)))
}