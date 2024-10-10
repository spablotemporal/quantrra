header <- dashboardHeader(title = 'quantrra')
header$children[[2]]$children <-  tags$a(href='https://github.com/spablotemporal/quantrra',
                                           tags$img(src='icon.png',height='50',width='50'))
# Sidebar ---------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Model", icon = icon("shield-virus"),
           menuSubItem(text = 'Model', tabName = "tab_quant"),
           menuSubItem(text = 'Stratified Model', tabName = 'tab_strat'),
           menuSubItem(text = 'Sensitivity Analysis', tabName = 'tab_sa'),
           menuSubItem(text = "Qualtative", tabName = "tab_qual")
           # menuSubItem(text = 'Distribution Fitting', tabName = 'DFTab')
           ),
  
  menuItem("Documentation", tabName = "t2", icon = icon("book")),
  menuItem('Examples', tabName = "t3", icon = icon('atlas')),
  hr()
))
# Body ----------
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    ## Model tab -----
    tabItem(tabName = "tab_quant",
            h1("quantrra: Quantitative risk assesment"),
            tags$i('This application is still under development, documentation is still in progress, for any questions please contact the developer: '), tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu. '),
            # hr(),
            br(),
            # tags$em('Due to higher traffic than expected, we are experiencing some problems with the server. You can also download the R package and run the app locally using the QuantRRA::runQuantRRA() function, for more information visit: '),
            # tags$a('Project repository', href = 'https://github.com/spablotemporal/quantrra'),
            'The following application was developed for the implementation of rapid risk assesment. A model tree file can be uploaded or specified in the app, and the risk is estimated using a stochastic probabilistic model.',
            br(),
            'Example model files can be found in the library of examples tab in this application',
            hr(),
            fluidRow(column(width = 12,
                            ### Instructions ----------------
                            box(
                              title = "Instructions", width = 12, collapsible = T,
                              doc$ra_instructions,
                              ### Upload file -----------
                              # column(
                              # width = 12,
                              fileInput(
                                "upload", 
                                label = "Load a model file:",
                                buttonLabel = "Search",
                                accept = c(".zip", ".xlsx"),
                                placeholder = "No file selected"
                              ),
                              # actionButton(inputId = "refresh", label = "Refresh"),
                              # ),
                              actionButton(
                                inputId = "reset",
                                label = "Reset Model"
                              ),
                              # shinyWidgets::actionBttn(
                              #   inputId = "reset",
                              #   label = "Reset Model"
                              # ),
                              downloadButton("dl", "Export model")
                              # downloadButton('downloadData', 'Download Table'),
                            ),
                            ### Model details ------------
                            tabBox(
                              width = 12, title = tags$b("Model details"),
                              side = "right", selected = "Model Tree", 
                              ### Parameters table ------------
                              tabPanel(
                                "Parameters table",
                                #### dropdown opts ------------
                                dropdownButton(
                                  tags$h3("Add parameter"), size = "sm",
                                  selectInput(
                                    inputId = "par_id",
                                    label = "Id",
                                    choices = ra_model$model %>% filter(type %in% c("In", "in")) %>% pull(id)
                                  ),
                                  textInput(
                                    inputId = "par_choice",
                                    label = "Choice label"
                                  ),
                                  textInput(
                                    inputId = "par_val",
                                    label = "Value"
                                  ),
                                  shinyWidgets::actionBttn(
                                    inputId = "par_add",
                                    label = "Add",
                                    color = "success"
                                  ),
                                  
                                  circle = TRUE, status = "success",
                                  icon = icon("plus"), width = "150px",
                                  
                                  tooltip = tooltipOptions(title = "Click to add choices")
                                ) %>% column(width = 6),
                                actionBttn(
                                  size = "sm",
                                  inputId = "par_remove",
                                  label = "Remove row",
                                  style = "material-circle", 
                                  color = "danger",
                                  icon = icon("minus")
                                ) %>% column(width = 6),
                                #### tbl out ----------------
                                DTOutput("parameters")
                              ),
                              ### Model table ------------
                              tabPanel(
                                "Model table",
                                #### dropdown options ------------
                                dropdownButton(
                                  tags$h3("Add node"), size = "sm",
                                  textInput(
                                    inputId = "newid",
                                    label = "Id"
                                  ),
                                  textInput(
                                    inputId = "newLab",
                                    label = "Label"
                                  ),
                                  selectInput(
                                    inputId = "newType",
                                    label = "Type",
                                    choices = c("In", "Out")
                                  ),
                                  conditionalPanel(
                                    condition = "input.newType == 'In'",
                                    textInput(
                                      inputId = "newDist",
                                      label = "Distribution"
                                    )
                                  ),
                                  conditionalPanel(
                                    condition = "input.newType == 'Out'",
                                    textInput(
                                      inputId = "newFormula",
                                      label = "Formula"
                                    )
                                  ),
                                  shinyWidgets::actionBttn(
                                    inputId = "newAdd",
                                    label = "Add node",
                                    color = "success"
                                  ),
                                  
                                  circle = TRUE, status = "success",
                                  icon = icon("plus"), width = "150px",
                                  
                                  tooltip = tooltipOptions(title = "Click to add nodes")
                                ) %>% column(width = 6),
                                actionBttn(
                                  size = "sm",
                                  inputId = "node_remove",
                                  label = "Remove row",
                                  style = "material-circle", 
                                  color = "danger",
                                  icon = icon("minus")
                                ) %>% column(width = 6),
                                #### table out -----------------
                                DTOutput("nodes")
                              ),
                              
                              tabPanel(
                                "Model Tree",
                                ### dropdown for graph options ----------
                                dropdownButton(
                                  tags$h3("Options"), size = "sm",
                                  radioGroupButtons(
                                    inputId = "gdir",
                                    label = "Direction: ",
                                    choices = c("Left-Right" = "LR", "Right-Left" = "RL", "Up-down" = "UD", "Down-Up" = "DU"),
                                    selected = "UD",
                                    direction = "vertical"
                                  ),
                                  sliderInput(
                                    inputId = "gnchar",
                                    label = "Characters before the breaks",
                                    value = 15, min = 5, max = 50
                                  ),
                                  circle = TRUE, status = "success",
                                  icon = icon("gear"), width = "150px",
                                  tooltip = tooltipOptions(title = "Click to see options")
                                ),
                                visNetworkOutput("ModelTree", height = "400px")
                              ),
                              hr()
                            ),
                            ### Parametros -------------
                            box(title = "Parameters", width = 12, 
                                uiOutput("par_inputs"),
                                numericInput("Nsim", "Number of simulations",
                                             min = 1, value = 1000, width = '50%'),
                                actionButton(inputId = 'Run', label = "Run Model")
                                # br(), DTOutput("testTbl") # Results
                            ),
                            ### Risk estimation ---------
                            box(title = 'Risk estimation', width = 12,
                                'For every output defined, the model will estimate a distribution and the median is showed by the vertical line in each plot',
                                numericInput('Nsim', 'Number of simulations',
                                             min = 1, value = 5000, width = '60%'),
                                # actionButton(inputId = 'Run', label = 'Run model'),
                                # DTOutput("MTbl")
                                selectInput(inputId = "outputs", label = "Output", choices = NULL),
                                # plotlyOutput('P4')
                                plotlyOutput("scorePlot")
                                )
            ))
    ),
    ## Sensitivity analysis tab -------
    tabItem(tabName = 'tab_sa',
            h2('Sensitivity Analysis'),
            'To run the sensitivity analysis, make sure to run the model first on the main tab',
            hr(),
            fluidRow(box(title = 'Sensitivity Analysis',
                         uiOutput(outputId = 'Outcomes'),
                         uiOutput(outputId = 'DepVar'),
                         actionButton(inputId = 'RunSA', label = 'Run Sensitivity Analysis')
                         , width = 12),
                     valueBoxOutput(outputId = 'VarExp', width = 12),
                     box(
                       plotlyOutput(outputId = 'VI', height  = '50%'),
                       visNetworkOutput(outputId = 'RT'), width = 12)
            )
    ),
    ## Qualitative tab------------
    tabItem(tabName = "tab_qual",
            box(width = 12, title = tags$b("Cargar datos del modelo"), 
                solidHeader = TRUE, collapsible = T,
                fluidRow(
                  column(width = 4,
                         fileInput(inputId = "model", label = "Cargar archivo:",
                                   buttonLabel = "Buscar...",
                                   placeholder = "Ningún archivo seleccionado")),
                  column(
                    width = 8,
                    tags$h5(tags$b("Instrucciones:"))
                    # Instrucciones
                  )
                )
            ),
            tabBox(width = 12, title = tags$b("Detalles del modelo"), 
                   side = "right", selected = "Árbol de eventos", 
                   tabPanel("Árbol de eventos",
                            fluidRow(
                              column(width = 12,
                                     dropdownButton(tags$h4(tags$b("Configuración:"), align = "center"),
                                                    radioButtons(inputId = "Dirtree", label = "Dirección del árbol de escenarios", 
                                                                 choices = list( "Vertical" = "UD", "Horizontal" = "LR")),
                                                    sliderInput(inputId = "Septree", label = "Separación",
                                                                min = 0, max = 300,value = 100, ticks = FALSE),
                                                    circle = TRUE, status = "danger", 
                                                    icon = icon("gear"), width = "300px",
                                                    tooltip = tooltipOptions(title = "Click para ver opciones !")
                                     ),
                                     visNetworkOutput("treeplot")))),
                   tabPanel("Modelo", DTOutput("table_model"))
            ),
            fluidRow(
              column(width = 12,
                     box(width = 12,title = tags$b("Evaluación de riesgo de los nodos"), status = "primary", collapsible = T,
                         uiOutput("inputs"),
                         actionBttn(inputId = "submit",label = "Analizar",style = "material-flat", color = "primary")))
            ),
            conditionalPanel("input.submit > 0", 
                             fluidRow(
                               column(width = 6,
                                      tabBox(width = 12, title = tags$b("Evaluación de riesgo de los nodos"),  side = "right", selected = "Gráfico de evaluación",
                                             tabPanel("Tabla de evaluación",  DTOutput("result_risk")),
                                             tabPanel("Gráfico de evaluación",  plotOutput("barResults"))
                                      )
                               ),
                               column(width = 6,
                                      tabBox(width = 12, title = tags$b("Resultados de evaluación de riesgo"),  side = "right", selected = "Resultado",
                                             tabPanel("Tabla de resultados", DTOutput("results_table")),
                                             tabPanel("Resultado", plotlyOutput("results_gauge"))
                                      )
                               )
                             )
            )
    ),
    ## Fitting tab -------
    tabItem(tabName = 'DFTab',
            h2('Distribution fitting'),
            'Comming soon ...'),
    
    tabItem(tabName = 'tab_strat',
            h2('Stratified Model'),
            'In this section you can use the model file and a dataset where each row represents a starata of the population with corresponding parameters, and run the model for each of those strata',
            br(),
            'Some examples of stratified models can include: local risk estimation, risk estimation for different age groups, among others...',
            hr(),
            # Model table input
            # Outputs
            # ## barplot with ranking of risk
            fluidRow(
              tabBox(width = 12,
                     tabPanel(title = 'Data',
                              # fileInput("uploadData", "Upload Data", accept = '.csv'),
                              DTOutput('InData')),
                     # tabPanel(title = 'Spatial features', 
                     #          # fileInput("uploadSp", "Upload Shapefile"),
                              fileInput(inputId = "filemap",
                                        label = "Upload map. Choose shapefile",
                                        multiple = TRUE,
                                        accept = c(".gpkg"))
                     #          )
                     ),
              actionButton(inputId = 'RunStratified', label = 'Run stratified model'),
              uiOutput(outputId = 'Outcomes_s'),
              hr(),
              tabBox(width = 12,
                     tabPanel(title = 'Ranking', plotlyOutput(outputId = 'Ranking_p')),
                     tabPanel(title = 'Map', plotOutput(outputId = 'Map_p'))
              )
            )
            ### Select which variable (risk or uncertainty a.k.a variance?)
            ## Option to add a shapefile and plot it
            ),
    # Documentation tab -------
    Documentation,
    # Examples tab ----------
    Examples
  ))

# UI --------
dashboardPage(header = header, 
              sidebar = sidebar, 
              body = body
              )  %>% 
  shinyUI