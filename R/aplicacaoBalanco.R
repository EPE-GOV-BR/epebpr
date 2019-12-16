#' Aplicacao web do Balanco de Ponta
#'
#' Executa a aplicacao web do Balanco de Ponta
#'
#' @import shiny
#' @import shinythemes
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function() {
  runApp(shinyApp(ui = uiBalanco, server = serverBalanco), launch.browser = T)
}
