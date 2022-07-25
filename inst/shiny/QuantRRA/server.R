# Server --------
function(input, output){
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
  
  ## Read Data for strat -------
  Strat <- reactive({
    input$uploadData$datapath %>% 
      read.csv()
  })
  
  SpStrat <- reactive({
    input$uploadSp$datapath %>% 
      st_read(., quiet = T)
  })
  
  ## Run strat model -----
  DFs <- eventReactive(input$RunStratified, {
    QuantRRA::RRA_s(M = Graph$nodes, Tbl = Strat(), nsim = input$Nsim)
  })
  
  output$Outcomes_s <- renderUI({
    opts <- Graph$nodes %>% 
      filter(type == 'Out') %>% 
      pull(id)
    
    selectInput(inputId = 'Outcomes', label = 'Outcomes', opts, selected = opts[length(opts)])
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
  
  ### Strat outputs -----------
  output$InData <- renderDT({
    Strat() %>% 
      DT::datatable(data = ., options = list(pageLength = 5))
  })
  
  # Ranking plot
  output$Ranking_p <- renderPlotly({
    # DFs() %>%
    #   plot_ly(x = ~IDs,
    #           y = ~O4_m) %>%
    #   layout(xaxis = list(categoryorder = "total descending"))
    RankingPlot(d = DFs(), var = 'O4')
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
      write.csv(data.frame(Graph$edges[-1,]), file = 'edges.csv', row.names = FALSE)
      print(fs)
      
      zip(zipfile=file, files=fs)
      if(file.exists(paste0(file, ".zip"))) {file.rename(paste0(file, ".zip"), file)}
      
    },
    contentType = "application/zip"
  )
  # Example files ------------------------
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
    selectInput(inputId = 'Outcomes', label = 'Outcomes', opts, selected = opts[length(opts)])
  })
  ## Dependent vars
  output$DepVar <- renderUI({
    opts <- colnames(Df())
    selectInput('DepVars', 'Dependent Variables', opts, multiple = T, selected = opts[1:(length(opts) - 1)])
  })
} %>% shinyServer() 