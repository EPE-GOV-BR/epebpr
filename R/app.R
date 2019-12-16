# UI
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  tags$head(tags$style(".navbar {margin: 0px 0px 5px 0px; padding: 0px}")), # altera os espacamentos da barra de navegacao
  tags$head(tags$style(".container-fluid {margin: 0px 0px 0px 2px; padding: 0px}")), # altera as margens da pagina
  tags$head(tags$style("#logoepe{position: fixed; left: 10px; top: 5px;}")), # posiciona o logo da epe na barra de forma correta
  tags$head(tags$style(".well{padding: 10px; margin-bottom: 5px}")), # altera espacamentos da barra lateral


  navbarPage(
    title = div(style = "width:70px", div(id = "logoepe", img(src = 'http://www.epe.gov.br/PublishingImages/Logos/logo-epe-azul.png', height = '40px'))),
    windowTitle = HTML("Balan&ccedil;o"),
    tabPanel(HTML("Balan&ccedil;o de Ponta"),
             sidebarLayout(
               sidebarPanel(#width = 5,
                 # Selecao do tipo de caso
                 # div para colocar inputs na mesma linha
                 div(style="display:inline-block; width:140px",
                     selectInput(inputId = "tipoCaso",
                                 label = "Tipo de Caso:",
                                 choices = c("PDE" = 1,
                                             "PMO" = 2,
                                             "Garantia Fisica" = 3),
                                 selected = 1)),
                 # Espaco entre inputs
                 div(style="display:inline-block; width:7px"),

                 # Selecao do modelo
                 div(style="display:inline-block; width:100px",
                     selectInput(inputId = "codModelo",
                                 label = "Modelo:",
                                 choices = c("NEWAVE" = 1,
                                             "SUISHI" = 2),
                                 selected = 1)),

                 # Espaco entre inputs
                 div(style="display:inline-block; width:7px"),

                 # Selecao do modelo
                 div(style="display:inline-block; width:140px",
                     selectInput(inputId = "idDemanda",
                                 label = "Demanda:",
                                 choices = c("L\u00EDquida" = 1,
                                             "Determin\u00EDstica" = 0),
                                 selected = 0)),


                 # Entrada numerica para o numero do caso
                 div(style="display:inline-block; width:80px",
                     numericInput(inputId = "numeroCaso",
                                  label = HTML("Caso:"),
                                  min = 1,
                                  value = NULL)),

                 # Espaco entre inputs
                 div(style="display:inline-block; width:7px"),

                 # Entrada para horas de ponta
                 div(style="display:inline-block; width:120px",
                     numericInput(inputId = "horasPonta",
                                  label = HTML("Horas de ponta:"),
                                  min = 1,
                                  value = 10)),

                 # Espaco entre inputs
                 div(style="display:inline-block; width:7px"),

                 # Entrada para reserva operativa
                 div(style="display:inline-block; width:160px",
                     numericInput(inputId = "reservaOperativa",
                                  label = HTML("Reserva Operativa [%]:"),
                                  min = 0,
                                  value = 5)),

                 # Entrada de texto para a descricao do caso
                 textInput(inputId = "descricao",
                           label = HTML("Descri&ccedil;&atilde;o do Caso:"),
                           value = NULL),

                 # Entrada para inicio da serie
                 div(style="display:inline-block; width:155px",
                     numericInput(inputId = "inicioCaso",
                                  label = HTML("In&iacute;cio Caso (aaaamm)"),
                                  min = 201801,
                                  max = 205012,
                                  value = NULL)),

                 # Espaco entre inputs
                 div(style="display:inline-block; width:7px"),

                 # Entrada para fim da serie
                 div(style="display:inline-block; width:155px",
                     numericInput(inputId = "fimCaso",
                                  label = HTML("Fim Caso (aaaamm)"),
                                  min = 201801,
                                  max = 205012,
                                  value = NULL)),

                 br(),
                 tags$b(HTML("Tucuru&iacute;")),
                 br(),
                 wellPanel(style = "padding: 5px 5px 0px 10px;", # style top right bottom left
                           # Entrada para codigo de Tucurui
                           div(style="display:inline-block; width:80px",
                               numericInput(inputId = "codTucurui",
                                            label = HTML("C&oacute;digo:"),
                                            min = 1,
                                            value = 275)),

                           # Espaco entre inputs
                           div(style="display:inline-block; width:7px"),

                           # Entrada para cota limite de Tucurui
                           div(style="display:inline-block; width:120px",
                               numericInput(inputId = "cotaTucurui",
                                            label = HTML("Cota Limite [m]:"),
                                            min = 1,
                                            value = 62)),
                           # Espaco entre inputs
                           div(style="display:inline-block; width:7px"),

                           # Entrada para geracao limite de Tucurui
                           div(style="display:inline-block; width:150px",
                               numericInput(inputId = "geracaoTucurui",
                                            label = HTML("Gera&ccedil;&atilde;o Limite [???]:"),
                                            min = 1,
                                            value = 4000))
                 ),

                 # Localiza base SQLite
                 textOutput(outputId = "textoBaseSQLite"),
                 span(strong(textOutput(outputId = "baseSQLite"), style = c("color:red"))),
                 div(style = "height:3px"),
                 div(actionButton(inputId = "btnBaseSQLite",
                                  label = "Pesquisar"),
                     actionButton(inputId = "btnCriaBaseSQLite",
                                  label = "Criar Base")),

                 # Localiza a pasta do caso
                 div(style = "height:8px"),
                 textOutput(outputId = "textoPasta"),
                 span(strong(textOutput(outputId = "pasta"), style = c("color:red"))),
                 div(style = "height:3px"),
                 actionButton(inputId = "btnPasta",
                              label = "Pesquisar"),

                 # Action button
                 div(style = "height:8px"),
                 HTML("Calcula Balan&ccedil;o de Ponta"),
                 div(style = "height:3px"),
                 actionButton(inputId = "btnBalanco",
                              label = "Calcular")

               ),

               # Output:
               mainPanel(
                 verbatimTextOutput(outputId = "selecao"),
                 tableOutput(outputId = "datatable")
               )
             )
    ),
    tabPanel(HTML("Gr&aacute;ficos"))
  )
)

# Define server function required
server <- function(input, output, session) {

  output$textoPasta <- renderText("Escolha a pasta do caso:")
  pastaCaso <- ""
  output$textoBaseSQLite <- renderText("Escolha a base SQLite ou crie nova:")
  baseCaso <- ""
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
      baseCaso <<- input$nomeBD
      removeModal()
      showModal(
        modalDialog(
          title = "Base de dados",
          mensagemModal,
          footer = div(style="text-align:center", modalButton("Ok"))
        )
      )
      output$baseSQLite <- renderText(paste0(pastaBD, "\\", baseCaso, ".sqlite3"))
      output$textoBaseSQLite <- renderText("Base selecionada:")
    }
  })

  # monitora botao de selecao de base existente
  observeEvent(input$btnBaseSQLite, {
    baseCaso <<- choose.files(caption = "Escolha a base SQLite") # importante <<- para passar valor para variavel global
    output$baseSQLite <- renderText(baseCaso)
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
      need((input$inicioCaso %/% 100) >= 2018, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((input$inicioCaso %/% 100) <= 2050, "Ano da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((input$inicioCaso %% 100) <= 12, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((input$inicioCaso %% 100) != 0, "M\u00EAs da data de inicio do caso incorreto ou fora de formato (aaaamm)"),
      need((input$fimCaso %/% 100) >= 2018, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((input$fimCaso %/% 100) <= 2050, "Ano da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((input$fimCaso %% 100) <= 12, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need((input$fimCaso %% 100) != 0, "M\u00EAs da data de fim do caso incorreto ou fora de formato (aaaamm)"),
      need(input$horasPonta, "Sem horas de ponta"),
      need(input$descricao, "Caso sem descri\u00E7\u00E3o"),
      need(pastaCaso != "", "Sem pasta de caso"),
      need(baseCaso != "", "Sem base de dados SQLite"),
      need(input$codTucurui, "Sem codigo de Tucurui"),
      need(input$cotaTucurui, "Sem cota limite de Tucurui"),
      need(input$geracaoTucurui, "Sem geracao limite de Tucurui")
    )
    paste(input$tipoCaso, input$codModelo, input$numeroCaso, input$descricao, pastaCaso, input$inicioCaso, input$fimCaso, baseCaso)
  })
  output$selecao <- renderText({
    textoSelecao()
  })

}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
