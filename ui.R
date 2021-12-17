

header <- dashboardHeader(title = "Projeto de Estatística")

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Métricas", tabName = "m", icon = icon("chart-line")),
        menuItem('Comparando Ações', tabName = 'comp', icon = icon('chart-bar'))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = 'm',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock', 'Ação', stock_list, multiple=FALSE),
                        uiOutput("timedate"),
                        actionButton('go', 'Submeter')
                        )
                ),
                fluidRow(
                    box(title = "Informações sobre a ação", width = 12, solidHeader = TRUE,
                        DTOutput('info')
                    )
                ),
                fluidRow(
                    box(title = "Série de Preços", width = 12, solidHeader = TRUE,
                        plotOutput('sh')
                    )
                ),
                fluidRow(
                  box(title = "Histograma de Preços", width = 12, solidHeader = TRUE,
                      plotOutput('hh')
                  )
                ),
                fluidRow(
                  box(title = "Boxplot de Preços", width = 12, solidHeader = TRUE,
                      plotOutput('bh')
                  )
                ),
        ),
        tabItem(tabName = 'comp',
                fluidRow(
                    box(title = 'Selecione suas opções', width=12, solidHeader = TRUE, status='warning',
                        selectInput('stock_comp', 'Ação', stock_list, multiple=TRUE),
                        uiOutput("timedate_comp"),
                        actionButton('go_comp', 'Submeter')
                    )
                ),
                fluidRow(
                  box(title = "Informações sobre as ações", width = 12, solidHeader = TRUE,
                      DTOutput('info2')
                  )
                ),
                fluidRow(
                  box(title = "Série de Preços", width = 12, solidHeader = TRUE,
                      plotOutput('sh_comp')
                  )
                ),
                fluidRow(
                  box(title = "Média de Preços", width = 12, solidHeader = TRUE,
                      plotOutput('mh_comp')
                  )
                ),
                fluidRow(
                  box(title = "Scatterplot", width = 12, solidHeader = TRUE,
                      plotOutput('sph_comp')
                  )
                ),
        )
    )
)

ui <- dashboardPage(
    skin = 'blue',
    header, sidebar, body)
