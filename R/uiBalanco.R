#' Interface do usuario (ui) Shiny do Banlanco de Ponta
#'
#' Cria estrutura de interface de usuario (ui) do Balanco de Ponta
#'
#' @export
uiBalanco <- fluidPage(
  theme = shinytheme("spacelab"),
  tags$head(tags$style(".shiny-notification {height: 80px; width: 400px; position: fixed; top: 40% ;left: 40%}"), # notificacao de progresso
            tags$style(".navbar {margin: 0px 0px 5px 0px; padding: 0px}"), # altera os espacamentos da barra de navegacao
            tags$style(".navbar-brand {padding: 5px 20px 5px 20px}"), # altera os espacamentos do titulo da barra de navegacao (logo)
            tags$style(".container-fluid {margin: 0px 0px 0px 2px; padding: 0px}"), # altera as margens da pagina
            tags$style(".well {padding: 10px; margin-bottom: 5px}"), # altera espacamentos da barra lateral
            tags$style(".form-group, .selectize-control {margin-bottom: 5px;}"), # altera margem para selectInput
            # altera cor das barras de nevegacao
            tags$style(HTML(".navbar-default .navbar-brand:hover, .navbar-default .navbar-brand:focus {color: #000000;}")), 
            tags$style(HTML(".navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {color: #000000;}")),
            tags$style(HTML(".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {
                            color: #000000; font-weight: bold;}"))
  ),

  # spinning
  use_busy_spinner(spin = "fading-circle", color="#274580", margins = c(300, 500), height = "80px", width = "80px"),

  navbarPage(
    title = tagList(img(src = 'http://www.epe.gov.br/PublishingImages/Logos/logo-epe-azul.png', height = '40px'),
                    # link para ajuda
                    a(href = "html/index.html", target = "_blank", 
                      img(src = "imagens/logo-wiki.png", height = "36px", 
                        style = 'position: absolute; top: 7px; right: 60px;'), title = "Documenta\u00E7\u00E3o e Ajuda"),
                    tags$div(style = 'position: absolute; top: 8px; right: 10px;', 
                        tags$button(id = "btnSair", class="btn action-button", style = "background-color: transparent; padding: 0px",
                                    img(src = "imagens/close.png", onmouseover = "this.src = 'imagens/close-red.png'", onmouseout = "this.src = 'imagens/close.png'",
                                        height = "34px", title = "Encerrar Balan\u00E7o")))),
    windowTitle = HTML("Balan&ccedil;o"),
    
    # Painel principal do balanco
    tabPanel(HTML("Balan&ccedil;o de Ponta"),
             sidebarLayout(
               sidebarPanel(#width = 5,
                 # Selecao do tipo de caso
                 # div para colocar inputs na mesma linha
                 tags$div(style="display:inline-block; width:140px",
                     selectInput(inputId = "tipoCaso",
                                 label = "Tipo de Caso:",
                                 choices = c("PDE" = 1,
                                             "PMO" = 2,
                                             "Garantia Fisica" = 3),
                                 selected = 1)),
                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),

                 # Selecao do modelo
                 tags$div(style="display:inline-block; width:100px",
                     selectInput(inputId = "codModelo",
                                 label = "Modelo:",
                                 choices = c("NEWAVE" = 1,
                                             "SUISHI" = 2),
                                 selected = 1)),

                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),

                 # Selecao do modelo
                 tags$div(style="display:inline-block; width:140px",
                     selectInput(inputId = "idDemanda",
                                 label = "Demanda:",
                                 choices = c("L\u00EDquida" = 1,
                                             "Determin\u00EDstica" = 0),
                                 selected = 0)),
                 br(),


                 # Entrada numerica para o numero do caso
                 tags$div(style="display:inline-block; width:80px",
                     numericInput(inputId = "numeroCaso",
                                  label = HTML("Caso:"),
                                  min = 1,
                                  value = NULL)),

                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),

                 # Entrada para horas de ponta
                 tags$div(style="display:inline-block; width:120px",
                     numericInput(inputId = "horasPonta",
                                  label = HTML("Horas de Ponta:"),
                                  min = 1,
                                  value = 10)),

                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),

                 # Entrada para reserva operativa
                 tags$div(style="display:inline-block; width:160px",
                     numericInput(inputId = "reservaOperativa",
                                  label = HTML("Reserva Operativa [%]:"),
                                  min = 0,
                                  value = 5)),
                 br(),

                 # Entrada de texto para a descricao do caso
                 textInput(inputId = "descricao",
                           label = HTML("Descri&ccedil;&atilde;o do Caso:"),
                           value = NULL),

                 # Entrada para inicio da serie
                 tags$div(style="display:inline-block; width:100px",
                     numericInput(inputId = "anoMesInicioMDI",
                                  label = HTML("In&iacute;cio MDI:"),
                                  min = 201801,
                                  max = 205012,
                                  value = 201901)),

                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),

                 # Entrada para fim da serie
                 tags$div(style="display:inline-block; width:100px",
                     numericInput(inputId = "anoMesFimMDI",
                                  label = HTML("Fim MDI:"),
                                  min = 201801,
                                  max = 205012,
                                  value = 203312)),
                 
                 # # Espaco entre inputs
                 # tags$div(style="display:inline-block; width:7px"),
                 #
                 # # Entrada para quantidade de series hidro
                 # tags$div(style="display:inline-block; width:100px",
                 #     numericInput(inputId = "seriesHidro",
                 #                  label = HTML("S&eacute;ries Hidro:"),
                 #                  min = 1,
                 #                  value = NULL)),

                 # Check box dos anos de estabilizacao
                 # tags$b(HTML("Considera anos de estabiliza&ccedil&atilde;o:")),
                 # br(),
                 # tags$div(style="display:inline-block",
                 #     checkboxInput(inputId = "anosPre",
                 #                   value = F,
                 #                   label = HTML("Pr&eacute;"))),
                 # 
                 # tags$div(style = "display:inline-block; width:10px"),
                 # 
                 # tags$div(style="display:inline-block",
                 #     checkboxInput(inputId = "anosPos",
                 #                   value = F,
                 #                   label = HTML("P&oacute;s"))),
                 # br(),

                 wellPanel(style = "padding: 5px;",
                           tags$b(HTML("REEs N&atilde;o Modulam")),
                           br(),
                           # Entrada para sistemas que nao modulam na ponta
                           tags$div(style="display:inline-block; width:150px",
                               textInput(inputId = "sistemasNaoModulamPonta",
                                         label = HTML("GHPonta:"),
                                         value = "2",
                                         placeholder = "sist1, sist2, etc.")),
                           
                           # Espaco entre inputs
                           tags$div(style="display:inline-block; width:7px"),
                           
                           # Entrada para sistemas que nao modulam na media
                           tags$div(style="display:inline-block; width:150px",
                               textInput(inputId = "sistemasNaoModulamMedia",
                                         label = HTML("GHM&eacute;dia:"),
                                         value = "6, 8, 13",
                                         placeholder = "sist1, sist2, etc."))),

                 # Localiza base SQLite
                 textOutput(outputId = "textoBaseSQLite"),
                 span(strong(textOutput(outputId = "baseSQLite"), style = c("color:red"))),
                 tags$div(style = "height:3px"),
                 tags$div(actionButton(inputId = "btnBaseSQLite",
                                  label = NULL,
                                  icon = icon("search"),
                                  width = 77),
                     actionButton(inputId = "btnCriaBaseSQLite",
                                  label = NULL,
                                  icon = icon("database"),
                                  width = 77)),

                 # Localiza a pasta do caso
                 tags$div(style = "height:8px"),
                 textOutput(outputId = "textoPasta"),
                 span(strong(textOutput(outputId = "pasta"), style = c("color:red"))),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnPasta",
                              label = NULL,
                              icon = icon("search"),
                              width = 77),
                 
                 # Localiza a pasta de dados de saida do caso (nwlistop)
                 tags$div(style = "height:8px"),
                 textOutput(outputId = "textoPastaSaidas"),
                 span(strong(textOutput(outputId = "pastaSaidas"), style = c("color:red"))),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnPastaSaidas",
                              label = NULL,
                              icon = icon("search"),
                              width = 77),

                 # Action button
                 tags$div(style = "height:8px"),
                 HTML("Calcula Balan&ccedil;o de Ponta"),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnBalanco",
                              label = NULL,
                              icon = icon("calculator"),
                              width = 77),
                 tags$div(style = "display:inline-block; width:10px"),
                     tags$div(style="display:inline-block;",
                         checkboxInput(inputId = "balancoResumido",
                               value = T,
                               label = HTML("Balan&ccedil;o resumido")))
               ),

               # Output:
               mainPanel(
                 htmlOutput(outputId = "avisos"),
                 htmlOutput(outputId = "selecao")
               )
             )
    ),
    # Painel de graficos
    tabPanel(HTML("Gr&aacute;ficos"),
             sidebarLayout(
               sidebarPanel(# width = 3,
                 # Localiza base SQLite
                 textOutput(outputId = "textoBaseSQLiteGrafico"),
                 span(strong(textOutput(outputId = "baseSQLiteGrafico"), style = c("color:red"))),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnBaseSQLiteGrafico",
                              label = NULL,
                              icon = icon("search"),
                              width = 70),
                 
                 # Exibe/esconde input com resultado da base
                 conditionalPanel(condition = "input.casoGrafico != -2",
                                  tags$div(style = "height:8px"),
                                  # Input dos casos na base
                                  selectInput(inputId = "casoGrafico",
                                              label = "Caso:",
                                              choices = c(escondido = -2),
                                              selected = -2),
                                  
                                  # Horizonte do grafico
                                  HTML("Defina o horizonte de exibi&ccedil;&atilde;o do gr&aacute;fico"),
                                  br(),
                                  # Espaco
                                  tags$div(style = "height:3px"),
                                  # Entrada para inicio da serie do grafico
                                  HTML("In&iacute;cio:"),
                                  tags$div(style="display:inline-block; width:100px",
                                      numericInput(inputId = "anoInicioGrafico", 
                                                   label = NULL,
                                                   min = 2018,
                                                   max = 2050,
                                                   value = 2020)),
                                  
                                  # Espaco entre inputs
                                  tags$div(style="display:inline-block; width:7px"),
                                  
                                  # Entrada para fim da serie do grafico
                                  HTML("Fim:"),
                                  tags$div(style="display:inline-block; width:100px",
                                      numericInput(inputId = "anoFimGrafico", 
                                                   label = NULL,
                                                   min = 2018,
                                                   max = 2060,
                                                   value = 2029)),
                                  br(),
                                  # Input dos casos na base
                                  selectInput(inputId = "tipoGrafico",
                                              label = HTML("Tipo do Gr&aacute;fico de CVaR:"),
                                              choices = c("CVaR Mensal Patamar" = 1, "CVaR Mensal" = 2, "CVaR Anual" = 3, "Risco" = 4),
                                              selected = -2),
                                  
                                  # Botao para exibir grafico
                                  actionButton(inputId = "btnGrafico",
                                               label = NULL,
                                               icon = icon("chart-area"),
                                               width = 70),
                                  # Botao de download
                                  tags$div(style = "display:inline-block;",
                                      # Exibe/esconde botao se nao tiver caso selecionado
                                      conditionalPanel(condition = "input.casoGrafico != -1",
                                                       downloadButton(outputId = "btnDownload", 
                                                                      label = NULL, 
                                                                      style = "width: 70px;")
                                      )
                                  )
                 )
               ),
               
               # Output:
               mainPanel(
                 plotOutput(outputId = "graficosCVar")
               )
             )
    )
  )
)