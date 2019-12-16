#' Calcula Value at Risk (VaR)
#'
#' Cria a funcao que calcula o valor de risco (VaR)
#'
#' @param x vetor com valores a se calcular o VaR
#' @param var valor de var aplicado. Ex. 0.01, 0.02, 0.05
#'
#' @return valor de var aplicado no vetor x
#'
#' @export
var <- function(x, var) {
  min(head(sort(x, decreasing = T), round((length(x) * var), 0)))
}