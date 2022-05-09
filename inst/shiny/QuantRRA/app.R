#' Function to Call the shinny app
#' 
#' @export

# QuantRRA: QUANTITATIVE RAPID RISK ASSESMENT 

# TO DO: ------------------
# - [x] Modify all the model files for the new specification of Pert distribution (mode, min, max) -> (min, mode, max)
# - [ ] Implement additional distributions in the documentation. Triangle, inverse gamma, uniform, etc 
# - [ ] Tab for the sensitivity analysis
# - [ ] Tab for the distribution fitting

# Setup ----------
# libraries used 
library(shiny); library(shinydashboard)
library(rlang)
library(dplyr)
library(plotly)
library(visNetwork)
library(DT)
library(QuantRRA)

init_nodes <- read.csv(unz('www/M_1.zip', 'nodes.csv'))
init_edges <- read.csv(unz('www/M_1.zip', 'edges.csv'))


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
ui <- dashboardPage(header = header, 
                    sidebar = sidebar, 
                    body = body
)

# Server --------
server <- function(input, output){
  # Create empty reactive values
  Graph <- reactiveValues(
    nodes = init_nodes,
    edges = init_edges
  )
  
  proxy = dataTableProxy('nodes')
  
  # read from zip
  observeEvent(input$upload, {
    Graph$nodes <- read.csv(unz(input$upload$datapath, 'nodes.csv'))
    Graph$edges <- read.csv(unz(input$upload$datapath, 'edges.csv'))
  })
  
  # Make the edits to the data
  observeEvent(input$nodes_cell_edit,{
    Graph$nodes <- editData(Graph$nodes, input$nodes_cell_edit, 'nodes')
  })
  
  ## Run the model -------
  Df <- eventReactive(input$Run, {
    # Graph$N
    RRA(M = Graph$nodes, input$Nsim)
  })
  
  ## Run sensitivity Analysis -------
  
  observeEvent(input$RunSA, {
    showModal(modalDialog("Running sensitivyt analysis...", footer=NULL, easyClose = T))
  })
  
  SA <- eventReactive(input$RunSA, {
    f <- paste(input$DepVars, collapse = '+')
    f <- paste0(input$Outcomes, '~', f)
    
    RFCART(data = Df(), f = eval(parse(text = f)), tree = 'interactive')
  })
  
  observeEvent(SA(), {
    removeModal()
  })
  
  ## Clear table -------
  observeEvent(input$reset,{
    Graph$nodes = data.frame(id = "N1",
                             label = "Node 1",
                             type = 'In',
                             level = 0,
                             distribution = 'Pert(0.1, 0.01, 0.5)',
                             formula = 'x',
                             shape = 'box',
                             color = 'lightgrey',
                             stringsAsFactors = F)
    
    Graph$edges = data.frame(id = "Edge",
                             from = "N1", 
                             to = "N2",
                             stringsAsFactors = F)
  })
  
  
  ## Outputs --------
  ### Nodes ---------
  # Render the table showing all the nodes in the graph.
  output$nodes <- renderDT({
    Graph$nodes %>% 
      select(c('id', 'label', 'type', 'level', 'distribution', 'formula')) %>%
      DT::datatable(data = .,
                    # rownames = F,
                    editable = T)
  })
  
  ### Node table -----------
  output$MTbl <- renderDT({
    Df() %>%
      data.frame() %>%
      DT::datatable(data = .,
                    rownames = F)
  })
  
  # Render the graph.
  output$ModelTree <- renderVisNetwork({
    visNetwork(Graph$nodes, Graph$edges) %>%
      visHierarchicalLayout(direction = "LR") %>%
      visOptions(manipulation = list(enabled = T,
                                     editNodeCols = c('id', 'label', 'type', 'level', 'distribution', 'formula'),
                                     addNodeCols = c('id', 'label', 'type', 'level', 'distribution', 'formula')))
    
  })
  
  # If the user edits the graph, this shows up in
  # `input$[name_of_the_graph_output]_graphChange`.  This is a list whose
  # members depend on whether the user added a node or an edge.  The "cmd"
  # element tells us what the user did.
  observeEvent(input$ModelTree_graphChange, {
    # If the user added a node, add it to the data frame of nodes.
    if(input$ModelTree_graphChange$cmd == "addNode") {
      temp = bind_rows(
        Graph$nodes,
        data.frame(id = input$ModelTree_graphChange$id,
                   label = input$ModelTree_graphChange$label,
                   type = input$ModelTree_graphChange$type,
                   level = as.numeric(input$ModelTree_graphChange$level),
                   distribution = input$ModelTree_graphChange$distribution,
                   formula = input$ModelTree_graphChange$formula,
                   shape = 'box',
                   stringsAsFactors = F) %>% 
          mutate(color = ifelse(type == 'In', "#8FFF91", '#FF918F'),
                 formula = ifelse(type == 'In', NA, formula),
                 distribution = ifelse(type == 'Out', NA, distribution))
      )
      Graph$nodes = temp %>% 
        arrange(level)
    }
    # If the user added an edge, add it to the data frame of edges.
    else if(input$ModelTree_graphChange$cmd == "addEdge") {
      temp = bind_rows(
        Graph$edges,
        data.frame(id = input$ModelTree_graphChange$id,
                   from = input$ModelTree_graphChange$from,
                   to = input$ModelTree_graphChange$to,
                   stringsAsFactors = F)
      )
      Graph$edges = temp
    }
    # If the user edited a node, update that record.
    else if(input$ModelTree_graphChange$cmd == "editNode") {
      temp = Graph$nodes
      temp$label[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$label
      temp$type[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$type
      temp$level[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$level
      temp$distribution[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$distribution
      temp$formula[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$formula
      Graph$nodes = temp %>% 
        arrange(level)
    }
    # If the user edited an edge, update that record.
    else if(input$ModelTree_graphChange$cmd == "editEdge") {
      temp = Graph$edges
      temp$from[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$from
      temp$to[temp$id == input$ModelTree_graphChange$id] = input$ModelTree_graphChange$to
      Graph$edges = temp
    }
    # If the user deleted something, remove those records.
    else if(input$ModelTree_graphChange$cmd == "deleteElements") {
      for(node.id in input$ModelTree_graphChange$nodes) {
        temp = Graph$nodes
        temp = temp[temp$id != node.id,]
        Graph$nodes = temp %>% 
          arrange(level)
      }
      for(edge.id in input$ModelTree_graphChange$edges) {
        temp = Graph$edges
        temp = temp[temp$id != edge.id,]
        Graph$edges = temp
      }
    }
  })
  
  ### Sensitivity analysis outputs --------
  output$VarExp <- renderValueBox({
    v <- round(SA()$VarianceExp, 4)
    valueBox(value = v, subtitle = "Variance explained", icon = icon("truck"), color = "red")
  })
  
  output$VI <- renderPlotly({
    ggplotly(SA()$RelImport)
  })
  
  output$RT <- renderVisNetwork({
    SA()$RT
  })
  
  ## Downloads -------
  
  # Download the tree
  output$downloadData <- downloadHandler(
    filename = function() {'Model.zip'},
    content = function(file) {
      tmpdir <- tempdir()
      setwd(tempdir())
      print(tempdir())
      
      fs <- c('nodes.csv', 'edges.csv')
      write.csv(data.frame(Graph$nodes), file = 'nodes.csv', row.names = FALSE)
      write.csv(data.frame(Graph$edges), file = 'edges.csv', row.names = FALSE)
      print(fs)
      
      zip(zipfile=file, files=fs)
      if(file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
      
    },
    contentType = "application/zip"
  )
  # Example files
  # ------------------------
  output$downloadOIRSA <- downloadHandler(
    filename <- function() {
      paste("OIRSA_PPA", "zip", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/OIRSA_PPA.zip", file)
    },
    contentType = "application/zip"
  )
  
  # ~~~~~~~~~~~~~ Output plots  ~~~~~~~~~~~~
  # P4
  output$P4 <- renderPlotly({
    # Filter only outputs
    o <- Graph$nodes %>% 
      filter(type == 'Out')
    
    if(nrow(o) > 1){
      PL <- lapply(1:nrow(o), function(x){
        # x <- 1
        p <- Df() %>% 
          ggplot() +
          geom_histogram(aes_string(o$id[x])) +
          geom_vline(data = data.frame(m = round(quantile(Df()[,o$id[x]], 0.5), 4)), aes(xintercept = m), lty = 1, lwd = 1, col = 'grey20') +
          labs(title = paste0(o$id[x], ': ', o$label[x])) +
          theme_minimal()
        
        ggplotly(p)
      })
      
      subplot(PL, nrows = 2)
    }else{
      p <- Df() %>% 
        ggplot() +
        geom_histogram(aes_string(o$id)) +
        geom_vline(data = data.frame(m = round(quantile(Df()[,o$id], 0.5), 4)), aes(xintercept = m), lty = 1, lwd = 1, col = 'grey20') +
        labs(title = paste0(o$id, ': ', o$label)) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  ## Outcomes
  output$Outcomes <- renderUI({
    opts <- colnames(Df())
    selectInput('Outcomes', 'Outcomes', opts, selected = opts[length(opts)])
  })
  ## Dependent vars
  output$DepVar <- renderUI({
    opts <- colnames(Df())
    selectInput('DepVars', 'Dependent Variables', opts, multiple = T, selected = opts[1:(length(opts) - 1)])
  })
}


shinyApp(ui, server)