#' Aplicacao web do Balanco de Potencia
#'
#' Executa a aplicacao web do Balanco de Potencia
#' 
#' @param execucao define se vai exibir o bp padrao ou versao de analise da EPE
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function(execucao = "") {
  runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
}
