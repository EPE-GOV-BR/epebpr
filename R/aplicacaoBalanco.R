#' Aplicacao web do Balanco de Potencia
#'
#' Executa a aplicacao web do Balanco de Potencia
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function() {
  runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
}
