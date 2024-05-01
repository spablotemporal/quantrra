header <- dashboardHeader(title = 'quantrra')
header$children[[2]]$children <-  tags$a(href='https://github.com/spablotemporal/quantrra',
                                           tags$img(src='icon.png',height='50',width='50'))
# Sidebar ---------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Model", icon = icon("shield-virus"),
           menuSubItem(text = 'Model', tabName = "ModelTab"),
           menuSubItem(text = 'Stratified Model', tabName = 'StratifiedTab'),
           menuSubItem(text = 'Sensitivity Analysis', tabName = 'SATab')
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
    tabItem(tabName = "ModelTab",
            h1("quantrra: Quantitative risk assesment"),
            tags$i('This application is still under development, documentation is still in progress, for any questions please contact the developer: '), tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu. '),
            # hr(),
            br(),
            # tags$em('Due to higher traffic than expected, we are experiencing some problems with the server. You can also download the R package and run the app locally using the QuantRRA::runQuantRRA() function, for more information visit: '),
            # tags$a('Project repository', href = 'https://github.com/jpablo91/QuantRRA'),
            'The following application was developed for the implementation of rapid risk assesment. A model tree file can be uploaded or specified in the app, and the risk is estimated using a stochastic probabilistic model.',
            br(),
            'Example model files can be found in the library of examples tab in this application',
            hr(),
            fluidRow(column(width = 12,
                            ### Model table ---------
                            box(title = 'Model table', width = 12, collapsible = T,
                                'To start, you need to specify the model. Models can be constructed directly from the app using the network tools in the following section, or can be uploaded from a model file previosly created.',
                                fileInput("upload", "Upload a model file", accept = c(".zip", ".xlsx")),
                                # add dropdown button ----------
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
                                ),
                                # Add the model table ------------
                                DTOutput("nodes"),
                                'If you want to save your model to continue working on it later or to share it, you can download the file here:',
                                br(),
                                # downloadButton('downloadData', 'Download Table'),
                                downloadButton("dl", "Download"),
                                actionButton(inputId = 'reset', label = 'Clear Table', icon = icon("exclamation-triangle"))),
                            ### Model Tree -------------
                            box(title = 'Model Tree', width = 12, 
                                'To start building a model, click on the edit button and start adding the nodes and edges. You can edit the node attributes directly in the table above, and you can delete nodes from the tree below. ',
                                'Depending on the type of node, the user has to specify different parameters that can be updated later from the table above. ',
                                br(),
                                'There are two type of nodes:',
                                tags$li('Inputs: These nodes represent a distribution specified by the user. To specify a distribution, write the name of the distribution and its parameters, i.e. Pert(0.1, 0.01, 0.2). Current distributions supported include Normal, Binomial, Poisson and Pert'),
                                tags$li('Outputs: These nodes will be calculated by the model based on the equation specified by the user.'),
                                br(),
                                visNetworkOutput("ModelTree", height = "400px")),
                            ### Risk estimation ---------
                            box(title = 'Risk estimation', width = 12,
                                'For every output defined, the model will estimate a distribution and the median is showed by the vertical line in each plot',
                                numericInput('Nsim', 'Number of simulations',
                                             min = 1, value = 5000, width = '60%'),
                                actionButton(inputId = 'Run', label = 'Run model'),
                                # DTOutput("MTbl")
                                plotlyOutput('P4')
                                )
            ))
    ),
    ## Sensitivity analysis tab -------
    tabItem(tabName = 'SATab',
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
    
    ## Fitting tab -------
    tabItem(tabName = 'DFTab',
            h2('Distribution fitting'),
            'Comming soon ...'),
    
    tabItem(tabName = 'StratifiedTab',
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
                              DTOutput('InData'))
                     # tabPanel(title = 'Spatial features', 
                     #          # fileInput("uploadSp", "Upload Shapefile"),
                     #          fileInput(inputId = "filemap",
                     #                    label = "Upload map. Choose shapefile",
                     #                    multiple = TRUE,
                     #                    accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj'))
                     #          )
                     ),
              actionButton(inputId = 'RunStratified', label = 'Run stratified model'),
              uiOutput(outputId = 'Outcomes_s'),
              hr(),
              tabBox(width = 12,
                     tabPanel(title = 'Ranking', plotlyOutput(outputId = 'Ranking_p'))
                     # tabPanel(title = 'Map', plotOutput(outputId = 'Map_p'))
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