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
  output$textoPastaSaidas <- renderText("Escolha a pasta com as saídas do caso (nwlistop):")
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
        title = HTML("Criar nova base de dados para o Balanço de Potência"),
        tags$div(style="display:inline-block;", HTML("Localização da pasta:")),
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
    pastaBD <<- choose.dir(caption = "Escolha a pasta do banco de dados") # importante <<- para passar valor para variavel global
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
    baseSQLite <<- choose.files(caption = "Escolha a base SQLite",
                                filters = matrix(c("SQLite", "*.db;*.sqlite;*.sqlite3"), ncol = 2))
    output$baseSQLite <- renderText(baseSQLite)
    output$textoBaseSQLite <- renderText("Base selecionada:")
  })
  
  # monitora o botao de selecao da pasta do caso
  observeEvent(input$btnPasta, {
    pastaCaso <<- choose.dir(caption = "Escolha a pasta do caso")
    output$pasta <- renderText(pastaCaso)
    output$textoPasta <- renderText("Pasta do caso:")
  })
  
  # monitora o botao de selecao da pasta de saidas do caso (nwlistop)
  observeEvent(input$btnPastaSaidas, {
    pastaSaidas <<- choose.dir(caption = "Escolha a pasta com as saídas do caso (nwlistop)")
    output$pastaSaidas <- renderText(pastaSaidas)
    output$textoPastaSaidas <- renderText("Pasta com as saídas do caso (nwlistop):")
  })
  
  # monitora o botao para encerrar o app
  observeEvent(input$btnSair, {
    stopApp()
  })
  
  # monitora botao do calculo do balanco
  textoSelecao <- eventReactive(input$btnBalanco, {
    
    sistemasNaoModulamPonta <- strsplit(input$sistemasNaoModulamPonta, ",") %>% unlist() %>% as.numeric()
    sistemasNaoModulamMedia <- strsplit(input$sistemasNaoModulamMedia, ",") %>% unlist() %>% as.numeric()
    validaModulacao <- length(intersect(sistemasNaoModulamPonta, sistemasNaoModulamMedia))
    
    shiny::validate(
      need(input$numeroCaso, "Caso sem n\u00FAmero"),
      need(input$horasPonta, "Defina o n\u00FAmero de horas de ponta"),
      need(input$descricao, "Caso sem descrição"),
      need(pastaCaso != "", "Defina a pasta de caso"),
      need(pastaSaidas != "", "Defina a pasta com as saídas do caso (nwlistop)"),
      need(baseSQLite != "", "Defina a base de dados SQLite"),
      need(input$distribuicaoDeficit, "Defina valor de limite do rateio do déficit (0-100%)"),
      need(validaModulacao == 0, "Sistemas que não modulam na ponta devem ser diferentes dos que não modulam na média!")
      # need(input$sistemasNaoModulamPonta, "Defina os sistemas que não modulam na ponta"),
      # need(input$sistemasNaoModulamMedia, "Defina os sistemas que não modulam na média")
    )
    
    tic()
    
    # mensagem de tipo de simulacao
    # verifica se o usuario escolheu efetuar a leitura de dados, caso seja uma rodada com opcao epe
    if (as.logical(input$leituraDados)) {
      # pega dados gerais do NEWAVE
      df.dadosGerais <- leituraDadosGerais(pastaCaso)
      if (df.dadosGerais$tipoSimulacao == 1) {
        mensagemLeitura <- "Lendo dados de simulação com séries sintéticas e gravando no banco de dados..."
      } else if (df.dadosGerais$tipoSimulacao == 2){
        mensagemLeitura <- "Lendo dados de simulação com séries históricas e gravando no banco de dados..."
      } else {
        return("Outro tipo de simulação. <font color=red>Verifique o arquivo dger!</font>")
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
                                                 # as.numeric(input$reservaOperativa)/100,
                                                 as.integer(input$idDemanda),
                                                 sistemasNaoModulamPonta,
                                                 sistemasNaoModulamMedia,
                                                 codTucurui,
                                                 cotaLimiteTucurui,
                                                 potenciaLimiteTucurui)
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
                                                               codTucurui)
      } else {
        mensagemDisponibilidade <- ""
      }
      
      # bloco de calculo de balanco
      # verifica se o usuario escolheu efetuar o calculo do BP
      if (as.logical(input$execucaoBP)) {
        setProgress(message = "Calculando balanço de potência...")
        mensagem <- calculaBalancoParalelo(baseSQLite,
                                           as.integer(input$tipoCaso),
                                           as.integer(input$numeroCaso),
                                           as.integer(input$codModelo),
                                           cvuTransmissao,
                                           cvuHidro,
                                           cvuRenovaveis,
                                           cvuOutrasTermicas,
                                           as.logical(input$balancoResumido),
                                           as.double(input$distribuicaoDeficit)/100)
        
        # se o balanço foi calculado, gera saidas na BD e em excel
        df.dadosGerais <- leituraDadosGerais(pastaCaso)
        mensagemSaidas <- gravacaoSaidasAnalises(baseSQLite, as.integer(input$tipoCaso), as.integer(input$numeroCaso), as.integer(input$codModelo), df.dadosGerais)
        
      } else {
        mensagem <- ""
      }
    })
    # exibe tempo de execucao
    tempoExecucao <- toc(quiet = T)
    tempoExecucao <- round(tempoExecucao$toc - tempoExecucao$tic, 0) %>% as.numeric()
    tempoExecucao <- paste0("Executado em: ", tempoExecucao %/% 3600, " h. ",
                            (tempoExecucao - (tempoExecucao %/% 3600 * 3600)) %/% 60, " min. ", tempoExecucao %% 60, " seg.")

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
                 # importante <<- para passar valor para variavel global
                 baseSQLiteGrafico <<- choose.files(caption = "Escolha a base SQLite",
                                                    filters = matrix(c("SQLite", "*.db;*.sqlite;*.sqlite3"), ncol = 2))
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
                                    need(input$anoInicioGrafico, HTML("Favor determinar o início do horizonte para o gráfico!")),
                                    need(input$anoFimGrafico, HTML("Favor determinar o fim do horizonte para o gráfico!")),
                                    need(input$casoGrafico != -1, HTML("Favor selecionar um caso para o gráfico!"))
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
  output$graficoBalanco <- renderPlotly(grafico())

}
