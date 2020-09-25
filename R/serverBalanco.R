#' Servidor Shiny do Banlanco
#'
#' Cria servidor do Balanco de Potencia
#'
#' @param input entradas da camada ui
#' @param output saidas da camada ui
#' @param session objeto contendo dados e funcionalidades da da sessao
#'
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
        title = HTML("Criar nova base de dados para o Balan&ccedil;o de Pot\u00EAncia"),
        tags$div(style="display:inline-block;", HTML("Localiza&ccedil;&atilde;o da pasta:")),
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
    baseSQLite <<- choose.files(caption = "Escolha a base SQLite")
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
    pastaSaidas <<- choose.dir(caption = "Escolha a pasta com as sa\u00EDdas do caso (nwlistop)")
    output$pastaSaidas <- renderText(pastaSaidas)
    output$textoPastaSaidas <- renderText("Pasta com as sa\u00EDdas do caso (nwlistop):")
  })
  
  # monitora o botao para encerrar o app
  observeEvent(input$btnSair, {
    stopApp()
  })

  # monitora botao do calculo do balanco
  textoSelecao <- eventReactive(input$btnBalanco, {
    shiny::validate(
      need(input$numeroCaso, "Caso sem n\u00FAmero"),
      need((as.integer(input$anoMesInicioMDI) %/% 100) >= 2018, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesInicioMDI) %/% 100) <= 2050, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesInicioMDI) %% 100) <= 12, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesInicioMDI) %% 100) != 0, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesFimMDI) %/% 100) >= 2018, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesFimMDI) %/% 100) <= 2050, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesFimMDI) %% 100) <= 12, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((as.integer(input$anoMesFimMDI) %% 100) != 0, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need(input$horasPonta, "Defina o n\u00FAmero de horas de ponta"),
      need(input$descricao, "Caso sem descri\u00E7\u00E3o"),
      need(pastaCaso != "", "Defina a pasta de caso"),
      need(pastaSaidas != "", "Defina a pasta com as sa\u00EDdas do caso (nwlistop)"),
      need(baseSQLite != "", "Defina a base de dados SQLite"),
      need(input$reservaOperativa, "Defina valor de reserva operativa (0-100%)")#,
      # need(input$sistemasNaoModulamPonta, "Defina os sistemas que n\u00E3o modulam na ponta"),
      # need(input$sistemasNaoModulamMedia, "Defina os sistemas que n\u00E3o modulam na m\u00E9dia")
    )

    tic()
    sistemasNaoModulamPonta <- strsplit(input$sistemasNaoModulamPonta, ",") %>% unlist() %>% as.numeric()
    sistemasNaoModulamMedia <- strsplit(input$sistemasNaoModulamMedia, ",") %>% unlist() %>% as.numeric()
    
    # mensagem de tipo de simulacao
    # pega dados gerais do NEWAVE
    df.dadosGerais <- leituraDadosGerais(pastaCaso)
    if (df.dadosGerais$tipoSimulacao == 1) {
      mensagemLeitura <- "Lendo dados de simula\u00E7\u00E3o com s\u00E9ries sint\u00E9ticas e gravando no banco de dados..."
    } else if (df.dadosGerais$tipoSimulacao == 2){
      mensagemLeitura <- "Lendo dados de simula\u00E7\u00E3o com s\u00E9ries hist\u00F3ricas e gravando no banco de dados..."
    } else {
      return("Outro tipo de simula\u00E7\u00E3o. <font color=red>Verifique o arquivo dger!</font>")
    }
    withProgress(message = mensagemLeitura, value = 0, {
      # bloco de dados de entrada
      mensagemBancoDados <- carregaDadosSQLite(baseSQLite,
                                               pastaCaso,
                                               pastaSaidas,
                                               as.integer(input$tipoCaso),
                                               as.integer(input$numeroCaso),
                                               as.integer(input$codModelo),
                                               input$descricao,
                                               as.integer(input$horasPonta),
                                               as.numeric(input$reservaOperativa)/100,
                                               as.integer(input$idDemanda),
                                               sistemasNaoModulamPonta,
                                               sistemasNaoModulamMedia,
                                               codTucurui,
                                               cotaLimiteTucurui,
                                               potenciaLimiteTucurui,
                                               as.integer(input$anoMesInicioMDI),
                                               as.integer(input$anoMesFimMDI))

      # bloco de calculo da disponibilidade hidro
      setProgress(message = "Calculando disponibilidade hidr\u00E1ulica...")
      incProgress(1/3)
      mensagemDisponibilidade <- calculaDisponibilidadeHidro(baseSQLite,
                                                             pastaCaso,
                                                             as.integer(input$tipoCaso),
                                                             as.integer(input$numeroCaso),
                                                             as.integer(input$codModelo),
                                                             codTucurui)
      
      # bloco de calculo de balanco
      setProgress(message = "Calculando balan\u00E7o de pot\u00EAncia...")
      incProgress(1/3)
      mensagem <- calculaBalancoParalelo(baseSQLite,
                                         as.integer(input$tipoCaso),
                                         as.integer(input$numeroCaso),
                                         as.integer(input$codModelo),
                                         cvuTransmissao,
                                         cvuHidro,
                                         cvuRenovaveis,
                                         cvuOutrasTermicas,
                                         as.logical(input$balancoResumido))
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
                 baseSQLiteGrafico <<- choose.files(caption = "Escolha a base SQLite") # importante <<- para passar valor para variavel global
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
                 
                 # output$tabelaDadosCasos <- renderDT(
                 #   datatable(data = leituraTabelaDadosCasos(baseSQLiteGrafico),
                 #             options = list(pageLength = 12,
                 #                            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')),
                 #             rownames = FALSE,
                 #             selection = "none"))
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
                                  show_spinner()
                                  chaveGrafico <- c(input$casoGrafico %>% str_split(";") %>% unlist() %>% as.numeric())
                                  if (as.numeric(input$tipoGrafico) == 4) {
                                    withLogErrors({ 
                                      grafico <- graficoRiscoDeficit(baseSQLiteGrafico, 
                                                                     chaveGrafico[1], 
                                                                     chaveGrafico[2], 
                                                                     chaveGrafico[3], 
                                                                     as.numeric(input$anoInicioGrafico), 
                                                                     as.numeric(input$anoFimGrafico))
                                      
                                    })  
                                  } else if(as.numeric(input$tipoGrafico) %in% c(1,2,3)){
                                    withLogErrors({
                                      grafico <- graficoCVAR(baseSQLiteGrafico, 
                                                             chaveGrafico[1], 
                                                             chaveGrafico[2], 
                                                             chaveGrafico[3], 
                                                             as.numeric(input$anoInicioGrafico), 
                                                             as.numeric(input$anoFimGrafico),
                                                             as.numeric(input$tipoGrafico))
                                      
                                    })
                                  } else {
                                    grafico <- graficoVAR(baseSQLiteGrafico, 
                                                          chaveGrafico[1], 
                                                          chaveGrafico[2], 
                                                          chaveGrafico[3], 
                                                          as.numeric(input$anoInicioGrafico), 
                                                          as.numeric(input$anoFimGrafico),
                                                          as.numeric(input$tipoGrafico))
                                  }
                                  hide_spinner()
                                  return(grafico)
                                })
  
  output$graficosCVar <- renderPlot(grafico(), height = 700)
  
  # monitora botao de download
  output$btnDownload <- downloadHandler(
    filename = function() {
      chaveGrafico <- c(input$casoGrafico %>% str_split(";") %>% unlist() %>% as.numeric())
      if (as.numeric(input$tipoGrafico) == 4) {
        paste0("Risco de Deficit - Caso ", chaveGrafico[2], ".xlsx")
      } else if(as.numeric(input$tipoGrafico) %in% c(1,2,3)) {
        paste0("Profundidade de Deficit - CVaR - ", ifelse(as.numeric(input$tipoGrafico) == 3, "Ano", "Mes")," - Caso ", chaveGrafico[2], ".xlsx")
      } else {
        paste0("Profundidade de Deficit - VaR - ", ifelse(as.numeric(input$tipoGrafico) == 7, "Ano", "Mes")," - Caso ", chaveGrafico[2], ".xlsx")
      }
    },
    content = function(arquivoExcel) {
      chaveGrafico <- c(input$casoGrafico %>% str_split(";") %>% unlist() %>% as.numeric())
      if (as.numeric(input$tipoGrafico) == 4) {
        write_xlsx(dadosGraficoRiscoDeficit(baseSQLiteGrafico, 
                                            chaveGrafico[1], 
                                            chaveGrafico[2], 
                                            chaveGrafico[3], 
                                            as.numeric(input$anoInicioGrafico), 
                                            as.numeric(input$anoFimGrafico)),
                   arquivoExcel)
      } else if(as.numeric(input$tipoGrafico) %in% c(1,2,3)) {
        write_xlsx(dadosGraficoCVAR(baseSQLiteGrafico, 
                                    chaveGrafico[1], 
                                    chaveGrafico[2], 
                                    chaveGrafico[3], 
                                    as.numeric(input$anoInicioGrafico), 
                                    as.numeric(input$anoFimGrafico),
                                    as.numeric(input$tipoGrafico)),
                   arquivoExcel)
      } else {
        write_xlsx(dadosGraficoVAR(baseSQLiteGrafico, 
                                   chaveGrafico[1], 
                                   chaveGrafico[2], 
                                   chaveGrafico[3], 
                                   as.numeric(input$anoInicioGrafico), 
                                   as.numeric(input$anoFimGrafico),
                                   as.numeric(input$tipoGrafico)),
                   arquivoExcel)
      }
    }
  )
}