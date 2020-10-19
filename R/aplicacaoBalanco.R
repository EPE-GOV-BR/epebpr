#' Aplicacao web do Balanco de Potencia
#'
#' Executa a aplicacao web do Balanco de Potencia
#' 
#' @param execucao define se vai exibir a aba de graficos padrao ou a aba de graficos de analise da EPE
#'
#' @examples
#' aplicacaoBalanco()
#'
#' @export
aplicacaoBalanco <- function(execucao = "") {
  if (execucao == "epe") {
    runApp(shinyAppDir(system.file("appBalancoEPE", package = "epebpr")), launch.browser = T)
  } else {
    runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
  }
}
