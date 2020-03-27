#' Aplicacao web do Balanco de Ponta
#'
#' Executa a aplicacao web do Balanco de Ponta
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function() {
  runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
}
