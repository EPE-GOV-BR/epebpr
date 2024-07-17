#' Servidor Shiny do Banlanco
#'
#' Cria servidor do Balanco de Potencia
#'
#' @param input entradas da camada ui
#' @param output saidas da camada ui
#' @param session objeto contendo dados e funcionalidades da da sessao
#' @export
serverBalanco <- function(input, output, session) {
  
  ####### ABA BALANCO #######
  output$textoPasta <- renderText("Escolha a pasta do caso:")
  pastaCaso <- ""
  output$textoPastaSaidas <- renderText("Escolha a pasta com as sa\u00EDdas do caso (nwlistop):")
  pastaSaidas <- ""
  output$textoBaseSQLite <- renderText("Escolha a base SQLite ou crie nova:")
  baseSQLite <- ""
  pastaBD <- ""
  
  # dados do balanco
  codTucurui <- 275 # codigo da usina Tucurui
  cotaLimiteTucurui <- 62 # cota limite de Tucurui em metros
  potenciaLimiteTucurui <- 4000 # potencia limite de Tucurui em MW
  # os valores de CVU da transmissao, hidro e outras renovaveis foram calibrados para forcarem o modelo a os considerar no balanco de forma ordenada,
  # evitando solucoes degeneradas ou irreais, mas matematicamente aceitas. Contudo, se os custos das geracoes aumentarem, 
  # esses valores devem ser recalibrados para evitar problemas de escalonamento.
  cvuTransmissao <- 2e-6
  cvuHidro <- 3e-5
  cvuRenovaveis <- 1e-5
  cvuOutrasTermicas <- 0.1 # valores de cvu 0
  
  ## eventos da janela popup ##
  # monitora botao para criar base sqlite da barra lateral e abre janela para interacao com usuario (1)
  observeEvent(input$btnCriaBaseSQLite, {
    showModal(
      modalDialog(
        title = HTML("Criar nova base de dados para o Balan\u00E7o de Pot\u00EAncia"),
        tags$div(style="display:inline-block;", HTML("Localiza\u00E7\u00E3o da pasta:")),
        span(strong(textOutput(outputId = "pastaBDModal"), style = "color:red; display:inline-block")),
        actionButton(inputId = "btnProcurarPastaBD",
                     label = NULL,
                     icon = icon("search")),
        textInput(inputId = "nomeBD",
                  label = HTML("Nome da base de dados"),
                  value = NULL #,
                  # placeholder = "bd_balanco_pde"
        ),
        footer = tagList(
          modalButton(label = "     ",
                      icon = icon("times")),
          actionButton(inputId = "btnCriaBaseSQLiteModal",
                       label = NULL,
                       icon = icon("check"))
        )
      )
    )
    output$pastaBDModal <- renderText(pastaBD)
  })
  
  # monitora botao de pesquisa de pasta dentro da janela de criar pasta (2)
  observeEvent(input$btnProcurarPastaBD, {
    if (Sys.info()["sysname"] == "Windows") {
      pastaBD <<- choose.dir(caption = "Escolha a pasta do banco de dados")
    } else {
      pastaBD <<- file.choose() %>% dirname()
    }
    output$pastaBDModal <- renderText(pastaBD)
  })
  
  # monitora botao de criar base sqlite da janela (3)
  observeEvent(input$btnCriaBaseSQLiteModal, {
    if (pastaBD == "") {
      output$pastaBDModal <- renderText("Favor selecionar pasta para base de dados!")
    } else if (input$nomeBD == "") {
      updateTextInput(session, "nomeBD", placeholder = "Favor selecionar um nome!")
    } else{
      mensagemModal <- criaBDBalanco(pastaBD, input$nomeBD)
      baseSQLite <<- paste0(pastaBD, "\\", input$nomeBD, ".sqlite3")
      removeModal()
      showModal(
        modalDialog(
          title = "Base de dados",
          mensagemModal,
          footer = tags$div(style="text-align:center", modalButton("Ok"))
        )
      )
      output$baseSQLite <- renderText(baseSQLite)
      output$textoBaseSQLite <- renderText("Base selecionada:")
    }
  })
  ## fim dos eventos da janela popup ##
  
  # monitora botao de selecao de base existente
  observeEvent(input$btnBaseSQLite, {
    if (Sys.info()["sysname"] == "Windows") {
      baseSQLite <<- choose.files(caption = "Escolha a base SQLite",
                                  filters = matrix(c("SQLite", "*.db;*.sqlite;*.sqlite3"), ncol = 2))
    } else {
      baseSQLite <<- file.choose()
    }
    output$baseSQLite <- renderText(baseSQLite)
    output$textoBaseSQLite <- renderText("Base selecionada:")
  })
  
  # monitora o botao de selecao da pasta do caso
  observeEvent(input$btnPasta, {
    if (Sys.info()["sysname"] == "Windows") {
      pastaCaso <<- choose.dir(caption = "Escolha a pasta do caso")
    } else {
      pastaCaso <<- file.choose() %>% dirname()
    }
    output$pasta <- renderText(pastaCaso)
    output$textoPasta <- renderText("Pasta do caso:")
  })
  
  # monitora o botao de selecao da pasta de saidas do caso (nwlistop)
  observeEvent(input$btnPastaSaidas, {
    if (Sys.info()["sysname"] == "Windows") {
      pastaSaidas <<- choose.dir(caption = "Escolha a pasta com as sa\u00EDdas do caso (nwlistop)")
    } else {
      pastaSaidas <<- file.choose() %>% dirname()
    }
    output$pastaSaidas <- renderText(pastaSaidas)
    output$textoPastaSaidas <- renderText("Pasta com as sa\u00EDdas do caso (nwlistop):")
  })
  
  # monitora o botao para encerrar o app
  observeEvent(input$btnSair, {
    stopApp()
  })
  
  # monitora botao do calculo do balanco
  textoSelecao <- eventReactive(input$btnBalanco, {
    
    sistemasNaoModulamPonta <- strsplit(input$sistemasNaoModulamPonta, ",") %>% unlist() %>% as.numeric()
    sistemasNaoModulamMedia <- strsplit(input$sistemasNaoModulamMedia, ",") %>% unlist() %>% as.numeric()
    sistemasModulamTabela <- strsplit(input$sistemasModulamTabela, ",") %>% unlist() %>% as.numeric()
    validaModulacao <- length(dplyr::intersect(sistemasNaoModulamPonta, sistemasNaoModulamMedia)) +
      length(dplyr::intersect(sistemasNaoModulamPonta, sistemasModulamTabela)) +
      length(dplyr::intersect(sistemasNaoModulamMedia, sistemasModulamTabela))
    
    shiny::validate(
      need(input$numeroCaso, "Caso sem n\u00FAmero"),
      need(input$horasPonta, "Defina o n\u00FAmero de horas de ponta"),
      need(input$descricao, "Caso sem descri\u00E7\u00E3o"),
      need(pastaCaso != "", "Defina a pasta de caso"),
      need(pastaSaidas != "", "Defina a pasta com as sa\u00EDdas do caso (nwlistop)"),
      need(baseSQLite != "", "Defina a base de dados SQLite"),
      need(input$distribuicaoDeficit, "Defina valor de limite do rateio do d\u00E9ficit (0-100%)"),
      need(validaModulacao == 0, "Apenas um tipo de modula\u00E7\u00E3o de GH pode ser definido para cada REE")
    )
    
    tictoc::tic()
    
    # mensagem de tipo de simulacao
    # verifica se o usuario escolheu efetuar a leitura de dados, caso seja uma rodada com opcao epe
    if (as.logical(input$leituraDados)) {
      # pega dados gerais do NEWAVE
      df.dadosGerais <- leitorrmpe::leituraDadosGerais(pastaCaso)
      if (df.dadosGerais$tipoSimulacao == 1) {
        mensagemLeitura <- "Lendo dados de simula\u00E7\u00E3o com s\u00E9ries sint\u00E9ticas e gravando no banco de dados..."
      } else if (df.dadosGerais$tipoSimulacao == 2){
        mensagemLeitura <- "Lendo dados de simula\u00E7\u00E3o com s\u00E9ries hist\u00F3ricas e gravando no banco de dados..."
      } else {
        return("Outro tipo de simula\u00E7\u00E3o. <font color=red>Verifique o arquivo dger!</font>")
      }
    } else {
      mensagemLeitura <- "Processando a Leitura de Dados..."
    }
    
    withProgress(message = mensagemLeitura, value = 0, {
      # bloco de dados de entrada
      # verifica se o usuario escolheu efetuar a leitura de dados
      if (as.logical(input$leituraDados)) {
        mensagemBancoDados <- carregaDadosSQLite(baseSQLite,
                                                 pastaCaso,
                                                 pastaSaidas,
                                                 as.integer(input$tipoCaso),
                                                 as.integer(input$numeroCaso),
                                                 as.integer(input$codModelo),
                                                 input$descricao,
                                                 as.integer(input$horasPonta),
                                                 as.integer(input$idDemanda),
                                                 sistemasNaoModulamPonta,
                                                 sistemasNaoModulamMedia,
                                                 sistemasModulamTabela,
                                                 codTucurui,
                                                 cotaLimiteTucurui,
                                                 potenciaLimiteTucurui,
                                                 TRUE)
      } else {
        mensagemBancoDados <- ""
      }
      
      # bloco de calculo da disponibilidade hidro
      # verifica se o usuario escolheu efetuar o calculo da disponibilidade hidro
      if (as.logical(input$disponibilidadeHidro)) {
        setProgress(message = "Calculando disponibilidade hidr\u00E1ulica...")
        mensagemDisponibilidade <- calculaDisponibilidadeHidro(baseSQLite,
                                                               pastaCaso,
                                                               pastaSaidas,
                                                               as.integer(input$tipoCaso),
                                                               as.integer(input$numeroCaso),
                                                               as.integer(input$codModelo),
                                                               codTucurui,
                                                               input$flagVert,
                                                               input$flagUHE,
                                                               TRUE)
      } else {
        mensagemDisponibilidade <- ""
      }
      
      # bloco de calculo de balanco
      # verifica se o usuario escolheu efetuar o calculo do BP
      if (as.logical(input$execucaoBP)) {
        setProgress(message = "Calculando balan\u00E7o de pot\u00EAncia...")
        mensagem <- calculaBalancoParalelo(baseSQLite,
                                           as.integer(input$tipoCaso),
                                           as.integer(input$numeroCaso),
                                           as.integer(input$codModelo),
                                           cvuTransmissao,
                                           cvuHidro,
                                           cvuRenovaveis,
                                           cvuOutrasTermicas,
                                           as.logical(input$balancoResumido),
                                           as.double(input$distribuicaoDeficit)/100,
                                           TRUE)
        
        # se o balanco foi calculado, gera saidas na BD e em excel
        df.dadosGerais <- leitorrmpe::leituraDadosGerais(pastaCaso)
        mensagemSaidas <- gravacaoSaidasAnalises(baseSQLite, as.integer(input$tipoCaso), as.integer(input$numeroCaso), as.integer(input$codModelo), df.dadosGerais)
        
      } else {
        mensagem <- ""
      }
    })
    # exibe tempo de execucao
    tempoExecucao <- tictoc::toc(quiet = T)
    tempoExecucao <- round(tempoExecucao$toc - tempoExecucao$tic, 0) %>% as.numeric()
    tempoExecucao <- paste0("Executado em: ", tempoExecucao %/% 3600, " h. ",
                            (tempoExecucao - (tempoExecucao %/% 3600 * 3600)) %/% 60, " min. ", tempoExecucao %% 60, " seg.")
    
    # salva para controle as opcoes de execucao escolhidas
    arqOpt <- paste0(pastaCaso, "/optExec_", stringr::str_remove(basename(baseSQLite), "\\.sqlite3"), ".txt")
    # momento do inicio da execucao
    cat(paste0(# versao do pacote utilizada
               "Vers\u00E3o do pacote epebpr: ", packageVersion("epebpr"), "\n",
               # usuario
               "Usu\u00E1rio: ", Sys.getenv("USERNAME"), "\n",
               # pastas e arquivos selecionados
               "Diret\u00F3rio NEWAVE: ", pastaCaso, "\n",
               "Diret\u00F3rio NWLISTOP: ", pastaSaidas, "\n",
               "Base de Dados: ", baseSQLite, "\n",
               # opcoes de execucao
               "Tipo de Caso: ", ifelse(input$tipoCaso == 1, "PDE", ifelse(input$tipoCaso == 2, "PMO", ifelse(input$tipoCaso == 3, "GF", NA))), "\n",
               "Modelo: ", ifelse(input$codModelo == 1, "NEWAVE", ifelse(input$codModelo == 2, "SUISHI", NA)), "\n",
               "Demanda: ", ifelse(input$idDemanda == 1, "Determin\u00EDstica", ifelse(input$idDemanda == 2, "L\u00EDquida", NA)), "\n",
               "N\u00B0 do caso: ", input$numeroCaso, "\n",
               "Horas de Ponta: ", input$horasPonta, "\n",
               "Distribui\u00E7\u00E3o D\u00E9ficit [%]: ", input$distribuicaoDeficit, "\n",
               "Descri\u00E7\u00E3o: ", input$descricao, "\n",
               "REEs n\u00E3o modulam GHPonta: ", input$sistemasNaoModulamPonta, "\n",
               "REEs n\u00E3o modulam GHM\u00E9dia: ", input$sistemasNaoModulamMedia, "\n",
               "REEs modulam por tabela: ", input$sistemasModulamTabela, "\n",
               "Balan\u00E7o Resumido: ", ifelse(input$balancoResumido, "Sim", "N\u00E3o"), "\n",
               "Dados: ", ifelse(input$leituraDados, "Sim", "N\u00E3o"), "\n",
               "Disp. Hidro: ", ifelse(input$disponibilidadeHidro, "Sim", "N\u00E3o"), "\n",
               "Balan\u00E7o de Ponta: ", ifelse(input$execucaoBP, "Sim", "N\u00E3o"), "\n",
               "Vertimento para todas UHE: ", ifelse(input$flagVert, "Sim", "N\u00E3o"), "\n",
               "T\u00E9rmino da execu\u00E7\u00E3o: ", lubridate::now(), "\n",
               tempoExecucao
    ), file = arqOpt)
    
    return({paste(mensagemBancoDados, mensagemDisponibilidade, mensagem, tempoExecucao, sep = "<br>")})
  })
  output$selecao <- renderText({
    textoSelecao()
  })
  
  
  ####### ABA GRAFICOS #######
  output$textoBaseSQLiteGrafico <- renderText("Escolha a base SQLite")
  baseSQLiteGrafico <- ""
  
  # monitora botao de selecao de base existente
  observeEvent(input$btnBaseSQLiteGrafico, 
               withLogErrors({
                 if (Sys.info()["sysname"] == "Windows") {
                   baseSQLiteGrafico <<- choose.files(caption = "Escolha a base SQLite",
                                                      filters = matrix(c("SQLite", "*.db;*.sqlite;*.sqlite3"), ncol = 2))
                 } else {
                   baseSQLiteGrafico <<- file.choose()
                 }
                 if(!identical(baseSQLiteGrafico, character(0))){
                   output$baseSQLiteGrafico <- renderText(baseSQLiteGrafico)
                   output$textoBaseSQLiteGrafico <- renderText("Base selecionada:")
                   df.casosInputGrafico <- leituraTabelaDadosCasos(baseSQLiteGrafico)
                   # monta lista para input
                   lt.casosInputGrafico <- as.list(df.casosInputGrafico$caso)
                   names(lt.casosInputGrafico) <- df.casosInputGrafico$descricao
                   lt.casosInputGrafico <- append(list("Selecione um Caso" = -1), lt.casosInputGrafico)
                   updateSelectInput(session, 
                                     inputId = "casoGrafico", 
                                     choices = lt.casosInputGrafico,
                                     selected = -1)
                 }
               })
  )
  # monitora botao para exibir graficos
  grafico <- eventReactive(input$btnGrafico, 
                           {
                             shiny::validate(
                               need(input$anoInicioGrafico, HTML("Favor determinar o in\u00EDcio do horizonte para o gr\u00E1fico!")),
                               need(input$anoFimGrafico, HTML("Favor determinar o fim do horizonte para o gr\u00E1fico!")),
                               need(input$casoGrafico != -1, HTML("Favor selecionar um caso para o gr\u00E1fico!"))
                             )
                             show_modal_spinner()
                             chaveGrafico <- c(input$casoGrafico %>% str_split(";") %>% unlist() %>% as.numeric())
                             # CvaR
                             if(as.numeric(input$tipoGrafico) %in% c(1, 2, 3)){
                               grafico <- graficoCVAR(baseSQLiteGrafico, 
                                                      chaveGrafico[1], 
                                                      chaveGrafico[2], 
                                                      chaveGrafico[3], 
                                                      as.numeric(input$anoInicioGrafico), 
                                                      as.numeric(input$anoFimGrafico),
                                                      as.numeric(input$tipoGrafico))
                               
                               # Risco 
                             } else if (as.numeric(input$tipoGrafico) == 4) {
                               grafico <- graficoRiscoDeficit(baseSQLiteGrafico, 
                                                              chaveGrafico[1], 
                                                              chaveGrafico[2], 
                                                              chaveGrafico[3], 
                                                              as.numeric(input$anoInicioGrafico), 
                                                              as.numeric(input$anoFimGrafico))
                               
                               # VaR
                             } else if (as.numeric(input$tipoGrafico) %in% c(5, 6, 7)){
                               grafico <- graficoVAR(baseSQLiteGrafico, 
                                                     chaveGrafico[1], 
                                                     chaveGrafico[2], 
                                                     chaveGrafico[3], 
                                                     as.numeric(input$anoInicioGrafico), 
                                                     as.numeric(input$anoFimGrafico),
                                                     as.numeric(input$tipoGrafico))
                               
                               # LOLP    
                             } else if (as.numeric(input$tipoGrafico) == 8){
                               grafico <- graficoRiscoDeficitAnual(baseSQLiteGrafico, 
                                                                   chaveGrafico[1], 
                                                                   chaveGrafico[2], 
                                                                   chaveGrafico[3], 
                                                                   as.numeric(input$anoInicioGrafico), 
                                                                   as.numeric(input$anoFimGrafico))
                               
                               # CVaR Mensal Subsistema
                             } else if(as.numeric(input$tipoGrafico) == 9){
                               grafico <- graficoCVARSubsistema(baseSQLiteGrafico, 
                                                                chaveGrafico[1], 
                                                                chaveGrafico[2], 
                                                                chaveGrafico[3], 
                                                                as.numeric(input$anoInicioGrafico), 
                                                                as.numeric(input$anoFimGrafico))
                               
                               # Graficos com criterios de GF    
                             } else if(as.numeric(input$tipoGrafico) %in% c(10, 11, 12)){
                               grafico <- graficosGF(baseSQLiteGrafico, 
                                                     chaveGrafico[1], 
                                                     chaveGrafico[2], 
                                                     chaveGrafico[3], 
                                                     as.numeric(input$tipoGrafico))
                               # Requisitos de potencia  
                             } else if(as.numeric(input$tipoGrafico) == 13){
                               grafico <- graficoRequisitosPot(baseSQLiteGrafico, 
                                                               chaveGrafico[1], 
                                                               chaveGrafico[2], 
                                                               chaveGrafico[3], 
                                                               as.numeric(input$anoInicioGrafico), 
                                                               as.numeric(input$anoFimGrafico))
                               # Requisitos de potencia quadrimestral
                             } else if(as.numeric(input$tipoGrafico) == 14){
                               grafico <- graficoRequisitosPotQuad(baseSQLiteGrafico, 
                                                                   chaveGrafico[1], 
                                                                   chaveGrafico[2], 
                                                                   chaveGrafico[3], 
                                                                   as.numeric(input$anoInicioGrafico), 
                                                                   as.numeric(input$anoFimGrafico))
                             }
                             
                             remove_modal_spinner()
                             return(grafico)
                           })
  
  # output$graficosCVar <- renderPlot(grafico(), height = 600, width = 1000)
  output$graficoBalanco <- plotly::renderPlotly(grafico())
  
}
