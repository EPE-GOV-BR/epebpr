#' Cria banco de dados do balanco de potencia
#'
#' Executa script SQL e cria banco de dados SQLite com estrutura dos dados do balanco de potencia
#'
#' @param caminho localizacao do banco de dados
#' @param nomeBD nome do banco de dados
#' @return mensagem de sucesso
#'
#' @examples
#' \dontrun{
#' criaBDBalanco("C:/PDE2027_Caso080/BD", "bd_balanco_pde")
#' }
#' 
#' @export
criaBDBalanco <- function(caminho, nomeBD) {
  if (!dir.exists(caminho)) {
    mensagem <- "Pasta definida para gravar banco de dados n\u00E3o existe!"
  } else {
    caminhoSQL <- system.file("SQL", package = "epebpr")
    # monta query que vai criar estrutura do banco de dados
    query <- readr::read_file(paste(caminhoSQL, "criaBDBalanco.sql", sep = "/"), )
    query <- stringr::str_remove_all(query, "--.*\\\r")
    query <- stringr::str_remove_all(query, "--.*\\\n")
    query <- stringr::str_remove_all(query, "\\\r")
    query <- stringr::str_remove_all(query, "\\\n")
    query <- stringr::str_split(query, ";")
    query <- unlist(query)
    query <- query[query != ""]
    conexao <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(caminho, "/", nomeBD, ".sqlite3"))
    # executa multiplas execucoes no banco
    resultado <- sapply(query, DBI::dbExecute, conn = conexao, USE.NAMES = F)
    DBI::dbDisconnect(conexao)
    resultado <- sum(resultado)
    if (resultado == 0) {
      mensagem <- "Base de dados criada com sucesso!"
    } else {
      mensagem <- "Problemas para criar base de dados. Verifique se o pacote epebpr foi instalado corretamente!"
    }
  }
}
