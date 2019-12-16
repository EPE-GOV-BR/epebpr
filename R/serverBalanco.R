#' Servidor Shiny do Banlanco
#'
#' Cria servidor do Balanco de Ponta
#'
#' @param input entradas da camada ui
#' @param output saidas da camada ui
#' @param session objeto contendo dados e funcionalidades da da sessao
#'
#' @import shinybusy
#'
serverBalanco <- function(input, output, session) {

  output$textoPasta <- renderText("Escolha a pasta do caso:")
  pastaCaso <- ""
  output$textoBaseSQLite <- renderText("Escolha a base SQLite ou crie nova:")
  baseSQLite <- ""
  pastaBD <- ""

  # monitora botao para criar base sqlite da barra lateral e abre janela para interecao com usuario (1)
  observeEvent(input$btnCriaBaseSQLite, {
    showModal(
      modalDialog(
        title = HTML("Criar nova base de dados para o Balan&ccedil;o de Ponta"),
        div(style="display:inline-block;", HTML("Localiza&ccedil;&atilde;o da pasta:")),
        span(strong(textOutput(outputId = "pastaBDModal"), style = "color:red; display:inline-block")),
        actionButton(inputId = "btnProcurarPastaBD",
                     label = "Pesquisar"),
        textInput(inputId = "nomeBD",
                  label = HTML("Nome da base de dados"),
                  value = NULL #,
                  # placeholder = "bd_balanco_pde"
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(inputId = "btnCriaBaseSQLiteModal",
                       label = "Criar Base")
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
          footer = div(style="text-align:center", modalButton("Ok"))
        )
      )
      output$baseSQLite <- renderText(baseSQLite)
      output$textoBaseSQLite <- renderText("Base selecionada:")
    }
  })

  # monitora botao de selecao de base existente
  observeEvent(input$btnBaseSQLite, {
    baseSQLite <<- choose.files(caption = "Escolha a base SQLite") # importante <<- para passar valor para variavel global
    output$baseSQLite <- renderText(baseSQLite)
    output$textoBaseSQLite <- renderText("Base selecionada:")
  })

  # monitora o botao de selecao da pasta do caso
  observeEvent(input$btnPasta, {
    pastaCaso <<- choose.dir(caption = "Escolha a pasta do caso") # importante <<- para passar valor para variavel global
    output$pasta <- renderText(pastaCaso)
    output$textoPasta <- renderText("Pasta do caso:")
  })

  # monitora botao do calculo do balanco
  textoSelecao <- eventReactive(input$btnBalanco, {
    validate(
      need(input$numeroCaso, HTML("Caso sem n\u00FAmero")),
      # need((input$inicioCaso %/% 100) >= 2018, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$inicioCaso %/% 100) <= 2050, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$inicioCaso %% 100) <= 12, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$inicioCaso %% 100) != 0, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$fimCaso %/% 100) >= 2018, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$fimCaso %/% 100) <= 2050, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$fimCaso %% 100) <= 12, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      # need((input$fimCaso %% 100) != 0, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need(input$horasPonta, "Defina o n\u00FAmero de horas de ponta"),
      need(input$descricao, "Caso sem descri\u00E7\u00E3o"),
      need(pastaCaso != "", "Defina a pasta de caso"),
      need(baseSQLite != "", "Defina a base de dados SQLite"),
      need(input$reservaOperativa, "Defina valor de reserva operativa (0-100%)"),
      need(input$codTucurui, "Defina o c\u00F3digo de Tucuru\u00ED"),
      need(input$cotaLimiteTucurui, "Defina cota limite de Tucuru\u00ED"),
      need(input$geracaoLimiteTucurui, "Defina a gera\u00E7\u00E3o limite de Tucuru\u00ED")
      # need(input$sistemasNaoModulamPonta, "Defina os sistemas que n\u00E3o modulam na ponta"),
      # need(input$sistemasNaoModulamMedia, "Defina os sistemas que n\u00E3o modulam na m\u00E9dia")
    )
    show_spinner()
    sistemasNaoModulamPonta <- strsplit(input$sistemasNaoModulamPonta, ",") %>% unlist() %>% as.numeric()
    sistemasNaoModulamMedia <- strsplit(input$sistemasNaoModulamMedia, ",") %>% unlist() %>% as.numeric()
    mensagem <- withLogErrors({carregaDadosSQLite(baseSQLite,
                                                  pastaCaso,
                                                  as.integer(input$tipoCaso),
                                                  as.integer(input$numeroCaso),
                                                  as.integer(input$codModelo),
                                                  input$descricao,
                                                  as.integer(input$horasPonta),
                                                  as.numeric(input$reservaOperativa)/100,
                                                  as.integer(input$idDemanda),
                                                  as.logical(input$anosPre),
                                                  as.logical(input$anosPos),
                                                  sistemasNaoModulamPonta,
                                                  sistemasNaoModulamMedia,
                                                  as.integer(input$codTucurui),
                                                  as.numeric(input$cotaLimiteTucurui),
                                                  as.numeric(input$geracaoLimiteTucurui))})
    hide_spinner()
    return(mensagem)
    # paste(input$tipoCaso, input$codModelo, input$numeroCaso, input$descricao, pastaCaso, input$inicioCaso, input$fimCaso, baseSQLite)
  })
  output$selecao <- renderText({
    textoSelecao()
  })

}
