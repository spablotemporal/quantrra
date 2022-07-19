
header <- dashboardHeader(title = 'QuantRRA')

# Sidebar ---------
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Model", icon = icon("shield-virus"),
           menuSubItem(text = 'Model', tabName = "firstTab"),
           menuSubItem(text = 'Sensitivity Analysis', tabName = 'SATab'),
           menuSubItem(text = 'Distribution Fitting', tabName = 'DFTab')),
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
    tabItem(tabName = "firstTab",
            h2("Quantitative rapid risk assesment"),
            'This application is still under development, documentation will be shortly added, for any questions please contact the developers', tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu'),
            br(),
            'The following application was developed for the implementation of rapid risk assesment. A model tree file can be uploaded or specified in the app, and the risk is estimated using a stochastic probabilistic model.',
            br(),
            'An example model file can be downloaded', tags$a('HERE', href = 'https://github.com/jpablo91/RA/blob/master/Data/M_Tbl/M.zip'),
            hr(),
            fluidRow(column(width = 12,
                            box(title = 'Model table', width = 12, collapsible = T,
                                'To start, you need to specify the model. Models can be constructed directly from the app using the network tools in the following section, or can be uploaded from a model file previosly created.',
                                fileInput("upload", "Upload a model file"),
                                DTOutput("nodes"),
                                'If you want to save your model to continue working on it later or to share it, you can download the file here:',
                                br(),
                                downloadButton('downloadData', 'Download Table'),
                                actionButton(inputId = 'reset', label = 'Clear Table', icon = icon("exclamation-triangle"))),
                            box(title = 'Model Tree', width = 12, 
                                'To start building a model, click on the edit button and start adding the nodes and edges. You can edit the node attributes directly in the table above, and you can delete nodes from the tree below. ',
                                'Depending on the type of node, the user has to specify different parameters that can be updated later from the table above. ',
                                br(),
                                'There are two type of nodes:',
                                tags$li('Inputs: These nodes represent a distribution specified by the user. To specify a distribution, write the name of the distribution and its parameters, i.e. Pert(0.1, 0.01, 0.2). Current distributions supported include Normal, Binomial, Poisson and Pert'),
                                tags$li('Outputs: These nodes will be calculated by the model based on the equation specified by the user.'),
                                br(),
                                tags$em('NOTE: current version crashes when field is left as undefined, make sure you input any characters for all variables when adding a new node.'),
                                visNetworkOutput("ModelTree", height = "400px")),
                            box(title = 'Risk estimation', width = 12,
                                'For every output defined, the model will estimate a distribution and the median is showed by the vertical line in each plot',
                                numericInput('Nsim', 'Number of simulations',
                                             min = 1, value = 5000, width = '60%'),
                                actionButton(inputId = 'Run', label = 'Run model'),
                                # DTOutput("MTbl"),
                                plotlyOutput('P4'))
            ))
    ),
    ## Sensitivity analysis tab -------
    tabItem(tabName = 'SATab',
            h2('Sensitivity Analysis'),
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
    
    # Documentation tab -------
    tabItem(tabName = 't2',
            'This application was developed with the intention to provide a framework for a transparent, accesible and intuitive methodology for rapid risk assesment. ',
            'We provide some examples for different models that can be adapted to specific scenarios. In this documentation we explain how to load a pre-existing model file, modify it, and how to creat a new mode from scratch. ',
            br(),
            'The main tab consist on 3 sections: Model table, Model tree and results',
            tags$h2('Model table'),
            'This table contains the information on how the nodes interact with each other to calculate the risk of the event. The model table can be edited inside this shiny app, you can modify the nodes names, distribution or formulas. The columns included in the table are:',
            tags$li('id. A unique identifyier for the node. Try to use short names without spaces or special characters for this variables, and do not repeat the same id for different nodes.'),
            tags$li('label. The name of the node. THis label can contain spaces or special characters. The purpose of this variable is to be a human friendly readable identifyier for the nodes'),
            tags$li('type. There are two types of nodes, In and Out. In nodes are user defined inputs that use a probability distribution to describe the probability of the event happening. 
                  Out nodes are calculated by the model there can be multiple outputs and the outputs can be calculated using other outputs. '),
            tags$li('level. The hierarchy level of the node, the hierarchy order of the nodes is very important to tell the model the order of how the output nodes will be calculated. For example, if an output node O1 is dependent of another output node O2, the dependent node O2 needs to be a lower hierarchy than O1 in order to be calculated'),
            tags$li('distribution. The distributions of the input nodes, current distributions supported includes: normal, binomial, poisson and pert. Only defined for In nodes'),
            tags$li('formula. Formula used to calculate the node. Needs to be defined only for Out nodes'),
            br(),
            tags$h3('Distributions supported'),
            'To specify the distribution you must write the name of the distribution followed by the parameters. Currently there are 4 probability distributions supported.',
            tags$h4('Normal'),
            'Normal distribution has two parameters: mean and standard deviation. For example, if we want to specify a normal distribution with mean = 20 and standard deviation = 1.5, we would write ',
            tags$em('Normal(20, 1.5) '), 'in the column for the given node. For more information of the normal distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Normal_distribution'),
            tags$h4('Pert'),
            'The Pert distribution has three parameters: min,mode, and max. For example, if we want to specify a pert distribution with a mode of 20, a min of 10 and a max of 35, we would write ',
            tags$em('Pert(10, 20, 35) '), 'in the column for the given node. For more information of the pert distribution visit: ', tags$a('https://en.wikipedia.org/wiki/PERT_distribution'),
            tags$h4('Binomial'),
            'The Binomial distribution has two parameters: number of trials (n) and probability of success (p). For example, if we want to specify a binomial distribution with a n of 8, a p of 0.2, we would write ',
            tags$em('Binomial(8, 0.2) '), 'in the column for the given node. For more information of the binomial distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Binomial_distribution'),
            tags$h4('Poisson'),
            'The Poisson distribution has one parameters: lambda, which represents both the mean and standard deviation. For example, if we want to specify a Poisson distribution with a lambda of 10, we would write ',
            tags$em('Poisson(10) '), 'in the column for the given node. For more information of the poisson distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Poisson_distribution'),
            tags$h2('Model tree'),
            'This section provide a graphical representation of the current model. You can edit the model and add new nodes in this section. To add a new node click on the edit button,
            this will change the interface to edit mode with two options: add Node and Add Edge. To exit the edit mode, click the x on the top right corner.',
            tags$h2('Loading a model file'),
            'When the app is initialized, a basic model file is already pre-loaded, you cans tart editing the provided model, or you can use another model from the examples. You can find some pre-existing model files in the ', 
            tags$em('Examples '), 'tab of this app. For more information of the example model files, you can go to the reference in the link provided',
            br(),
            tags$img(src = 'G1.GIF', align = 'center'),
            br(),
            'Once you have downloaded, you can go back to the main tab and load the .zip file. ',
            br(),
            tags$img(src = 'G2.GIF', align = 'center'),
            br(),
            'For questions, contact: ', tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu')),
    
    # Examples tab ----------
    tabItem(tabName = 't3',
            'Examples',
            br(),
            tags$h2('OIRSA'),
            'This model was developed by the Organismo Internacional Regional de Sanidad Agropecuaria (OIRSA) to estimate the probability of introduction of african swine fever into the countries from the OIRSA region', 
            br(),
            tags$a('Reference', href = 'https://www.oirsa.org/contenido/2020/AR_PPA_Edición%20revisada%2001_07_20.pdf'),
            hr(),
            downloadButton("downloadOIRSA", "Download"),
            # tags$a('Download model', href ='https://www.oirsa.org/contenido/2020/AR_PPA_Edición%20revisada%2001_07_20.pdf'),
            br(),
            # tags$h2('OIE'),
            # 'Description', 
            # br(),
            # tags$a('Reference', href = 'https://rr-africa.oie.int/wp-content/uploads/2018/03/handbook_on_import_risk_analysis_-_oie_-_vol__i.pdf'),
            # downloadButton("downloadM2", "Download")
    )
  ))

# UI --------
dashboardPage(header = header, 
              sidebar = sidebar, 
              body = body
              )  %>% 
  shinyUI