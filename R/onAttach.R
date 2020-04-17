.onAttach <- function(libname, pkgname) {
  
  dependencias <- c('readr', 'readxl', 'writexl', 'dplyr', 'stringr', 'tidyr', 'clpAPI', 'DBI', 'RSQLite',
                    'shiny', 'shinythemes', 'shinybusy', 'tictoc', 'ggplot2', 'zoo', 'scales', 'showtext',
                    'jsonlite', 'parallel', 'foreach', 'doParallel', 'numbers', 'hash', 'leitorrcepel')
  
  for (andaDependencias in 1:length(dependencias)) {
    if (!(dependencias[andaDependencias] %in% rownames(installed.packages()))) {
      packageStartupMessage(paste0("Biblioteca ", dependencias[andaDependencias], " n\u00E3o encontrada. Favor instalar."))
    }
  }
  packageStartupMessage("\nPara executar digite: aplicacaoBalanco()")
}
