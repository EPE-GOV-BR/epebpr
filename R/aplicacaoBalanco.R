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
  if (execucao == "analisesepe") {
    runApp(shinyAppDir(system.file("appBalancoEPE", package = "epebpr")), launch.browser = T)
  } else {
    runApp(shinyAppDir(system.file("appBalanco", package = "epebpr")), launch.browser = T)
  }
}
