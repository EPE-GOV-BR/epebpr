#' Interface do usuario (ui) Shiny do Balanco de Potencia
#'
#' Cria estrutura de interface de usuario (ui) do Balanco de Potencia
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
            tags$style(HTML(".navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default 
                             .navbar-nav>.active>a:focus {color: #000000; font-weight: bold;}"))
  ),
  
  # spinner de processamento
  use_busy_spinner(spin = "fading-circle", color="#274580", margins = c(300, 500), height = "80px", width = "80px"),
  
  navbarPage(
    title = tagList(img(src = 'http://www.epe.gov.br/PublishingImages/Logos/logo-epe-azul.png', height = '40px'),
                    # link para ajuda
                    tags$a(href = "html/index.html", target = "_blank", 
                           img(src = "imagens/logo-wiki.png", height = "36px", 
                               style = 'position: absolute; top: 7px; right: 60px;'), title = "Documenta\u00E7\u00E3o e Ajuda"),
                    tags$div(style = 'position: absolute; top: 8px; right: 10px;', 
                             tags$button(id = "btnSair", class="btn action-button", style = "background-color: transparent; padding: 0px",
                                         img(src = "imagens/close.png", onmouseover = "this.src = 'imagens/close-red.png'", 
                                             onmouseout = "this.src = 'imagens/close.png'",
                                             height = "34px", title = "Encerrar Balan\u00E9o")))),
    windowTitle = HTML("Balan\u00E7o"),
    
    # Painel principal do balanco
    tabPanel(HTML("Balan\u00E7o de Pot\u00EAncia"),
             sidebarLayout(
               sidebarPanel(width = 3,
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
                                                  "SUISHI" = 2
                                      ),
                                      selected = 1)),
                 
                 # Espaco entre inputs
                 tags$div(style="display:inline-block; width:7px"),
                 
                 # Selecao do modelo
                 tags$div(style="display:inline-block; width:140px",
                          selectInput(inputId = "idDemanda",
                                      label = "Demanda:",
                                      choices = c("L\u00EDquida" = 2,
                                                  "Determin\u00EDstica" = 1),
                                      selected = 2)),
                 tags$br(),
                 
                 
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
                 
                 # Entrada para percentual de rateio do deficit
                 tags$div(style="display:inline-block; width:170px",
                          numericInput(inputId = "distribuicaoDeficit",
                                       label = HTML("Distribui\u00E7\u00E3o D\u00E9ficit [%]:"),
                                       min = 0,
                                       max = 100,
                                       value = 100)),
                 
                 tags$br(),
                 
                 # Entrada de texto para a descricao do caso
                 textInput(inputId = "descricao",
                           label = HTML("Descri\u00E7\u00E3o do Caso:"),
                           value = NULL),
                 
                 wellPanel(style = "padding: 5px;",
                           tags$b(HTML("REEs N\u00E3o Modulam")),
                           tags$br(),
                           # Entrada para sistemas que nao modulam na ponta
                           tags$div(style="display:inline-block; width:150px",
                                    textInput(inputId = "sistemasNaoModulamPonta",
                                              label = HTML("GHPonta:"),
                                              value = NULL,
                                              placeholder = "sist1, sist2, etc.")),
                           
                           # Espaco entre inputs
                           tags$div(style="display:inline-block; width:7px"),
                           
                           # Entrada para sistemas que nao modulam na media
                           tags$div(style="display:inline-block; width:150px",
                                    textInput(inputId = "sistemasNaoModulamMedia",
                                              label = HTML("GHM\u00E9dia:"),
                                              value = "6, 13",
                                              placeholder = "sist1, sist2, etc."))),
                 
                 wellPanel(style = "padding: 5px;",
                           tags$b(HTML("REEs Modulam Conforme Tabela")),
                           tags$br(),
                           # Entrada para sistemas que nao modulam na ponta
                           tags$div(style="display:inline-block; width:150px",
                                    textInput(inputId = "sistemasModulamTabela",
                                              label = HTML("GHPonta:"),
                                              value = "5, 8",
                                              placeholder = "sist1, sist2, etc."))),
                 
                 # Localiza base SQLite
                 tags$span(strong(textOutput(outputId = "textoBaseSQLite"))),
                 tags$span(strong(textOutput(outputId = "baseSQLite"), style = c("color:red; font-size:10px"))),
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
                 tags$span(strong(textOutput(outputId = "textoPasta"))),
                 tags$span(strong(textOutput(outputId = "pasta"), style = c("color:red; font-size:10px"))),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnPasta",
                              label = NULL,
                              icon = icon("search"),
                              width = 77),
                 
                 # Localiza a pasta de dados de saida do caso (nwlistop)
                 tags$div(style = "height:8px"),
                 tags$span(strong(textOutput(outputId = "textoPastaSaidas"))),
                 tags$span(strong(textOutput(outputId = "pastaSaidas"), style = c("color:red; font-size:10px"))),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnPastaSaidas",
                              label = NULL,
                              icon = icon("search"),
                              width = 77),
                 tags$br(),
                 tags$br(),
                 tags$b(HTML("Etapas de execu\u00E7\u00E3o:")),
                 tags$br(),
                 tags$div(style = "display:inline-block; width:5px"),
                 tags$div(style="display:inline-block;",
                          checkboxInput(inputId = "leituraDados",
                                        value = T,
                                        label = HTML("1. Dados"))),
                 tags$div(style = "display:inline-block; width:5px"),
                 tags$div(style="display:inline-block;",
                          checkboxInput(inputId = "disponibilidadeHidro",
                                        value = T,
                                        label = HTML("2. Disp. Hidro"))),
                 tags$div(style = "display:inline-block; width:5px"),
                 tags$div(style="display:inline-block;",
                          checkboxInput(inputId = "execucaoBP",
                                        value = T,
                                        label = HTML("3. BP"))),
                 tags$br(),
                 conditionalPanel(condition = "input.execucaoBP == 1 || input.disponibilidadeHidro == 1",
                                  tags$b(HTML("Op\u00E7\u00F5es adicionais:"))
                 ),
                 conditionalPanel(condition = "input.execucaoBP == 1",
                                  tags$div(style = "display:inline-block; width:5px"),
                                  tags$div(style="display:inline-block;",
                                           checkboxInput(inputId = "balancoResumido",
                                                         value = T,
                                                         label = HTML("BP Resumido")))
                 ),
                 conditionalPanel(condition = "input.disponibilidadeHidro == 1",
                                  tags$div(style = "display:inline-block; width:5px"),
                                  tags$div(style="display:inline-block;",
                                           checkboxInput(inputId = "flagVert",
                                                         value = F,
                                                         label = HTML("Considerar Vertimento para todas UHE")),
                                           checkboxInput(inputId = "flagUHE",
                                                         value = F,
                                                         label = HTML("Gravar dados individuais das UHE")))
                 ),

                 # Action button
                 tags$div(style = "height:8px"),
                 tags$div(style = "height:3px"),
                 actionButton(inputId = "btnBalanco",
                              label = "Executar",
                              icon = icon("play"),
                              width = 200)
                 
               ),
               
               # Output:
               mainPanel(
                 htmlOutput(outputId = "selecao")
               )
             )
    ),
    # Painel de graficos
    tabPanel(HTML("Gr\u00E1ficos"),
             sidebarLayout(
               sidebarPanel(width = 3,
                            # Localiza base SQLite
                            textOutput(outputId = "textoBaseSQLiteGrafico"),
                            tags$span(strong(textOutput(outputId = "baseSQLiteGrafico"), style = c("color:red"))),
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
                                             HTML("Defina o horizonte de exibi\u00E7\u00E3o do gr\u00E1fico"),
                                             tags$br(),
                                             # Espaco
                                             tags$div(style = "height:3px"),
                                             # Entrada para inicio da serie do grafico
                                             HTML("In\u00EDcio:"),
                                             tags$div(style="display:inline-block; width:100px",
                                                      numericInput(inputId = "anoInicioGrafico", 
                                                                   label = NULL,
                                                                   min = 2018,
                                                                   max = 2050,
                                                                   value = 2023)),
                                             
                                             # Espaco entre inputs
                                             tags$div(style="display:inline-block; width:7px"),
                                             
                                             # Entrada para fim da serie do grafico
                                             HTML("Fim:"),
                                             tags$div(style="display:inline-block; width:100px",
                                                      numericInput(inputId = "anoFimGrafico", 
                                                                   label = NULL,
                                                                   min = 2018,
                                                                   max = 2060,
                                                                   value = 2033)),
                                             tags$br(),
                                             # Input dos casos na base
                                             selectInput(inputId = "tipoGrafico",
                                                         label = HTML("Tipo de Gr\u00E1fico:"),
                                                         choices = c("CVaR Mensal" = 1,
                                                                     "CVaR Mensal Subsistema" = 9,
                                                                     "CVaR Anual" = 3, 
                                                                     "Risco (LOLP) Mensal" = 4,
                                                                     "Risco (LOLP) Anual" = 8,
                                                                     "VaR Mensal" = 6,
                                                                     "VaR Anual" = 7,
                                                                     "Requisitos de Pot\u00EAncia" = 13,
                                                                     "Requisitos de Pot\u00EAncia - Quadrimestral" = 14,
                                                                     "CVaR Mensal GF" = 10,
                                                                     "VaR Mensal GF" = 11,
                                                                     "VaR Anual GF" = 12),
                                                         selected = -2),
                                             
                                             # Botao para exibir grafico
                                             actionButton(inputId = "btnGrafico",
                                                          label = NULL,
                                                          icon = icon("chart-area"),
                                                          width = 70)
                            )
               ),
               
               # Output:
               mainPanel(
                 # plotOutput(outputId = "graficosCVar")
                 plotly::plotlyOutput(outputId = "graficoBalanco", width = 1100, height = 600)
               )
             )
    )
  )
)
