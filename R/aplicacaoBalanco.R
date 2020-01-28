#' Aplicacao web do Balanco de Ponta
#'
#' Executa a aplicacao web do Balanco de Ponta
#'
#' @import shiny
#' @import shinythemes
#' @importFrom base64enc dataURI
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function() {
  runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
}
