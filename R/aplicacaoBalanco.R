#' Aplicacao web do Balanco de Potencia
#'
#' Executa a aplicacao web do Balanco de Potencia
#'
#' @examples
#' \dontrun{
#' aplicacaoBalanco()
#' }
#' 
#' @import shiny shinythemes shinybusy
#' 
#' @export
aplicacaoBalanco <- function() {
  runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
}
