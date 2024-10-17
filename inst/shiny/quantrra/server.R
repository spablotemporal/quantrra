# Server --------
function(input, output, session){
  # Create empty reactive values -----------
  rv <- reactiveValues(
    model = init_nodes, # Nodes
    # edges = ra_plot_tree(init_nodes, edgetbl = T), # Edges
    stratified = NULL,
    shp = NULL,
    par = ra_model$par,
    res = NULL # Empty table for results
  )
  
  proxy = dataTableProxy('modelTbl')
  
  ## Read file -----------
  observeEvent(input$upload, {
    im <- ra_import(input$upload$datapath)
    rv$model <- im$model %>% 
      mutate(distribution = as.character(distribution))
    rv$stratified <- im$stratified
    rv$par <- im$par
    
    ### Update opts ---------
    updateSelectInput(session = session, inputId = "par_id", choices = rv$model %>% filter(type %in% c("In", "in")) %>% pull(id))
  })
  
  # Make the edits to the table -------------
  observeEvent(input$nodes_cell_edit,{
    rv$model <- editData(rv$model, input$nodes_cell_edit, 'modelTbl') %>% 
      mutate(
        type = tolower(type),
        color = ifelse(type == 'in', "#A0F0A0", '#F0A0A0'),
      )
  })
  
  ## Add a row ------------
  observeEvent(input$newAdd,{
    nn <- data.frame(
      id = input$newid,
      label = input$newLab,
      type = input$newType,
      # level = max(rv$model$level),
      level = 0,
      distribution = ifelse((input$newType %in% c("In", "in", "IN")), input$newDist, NA),
      formula = ifelse(input$newType == "Out", input$newFormula, NA),
      shape = 'box',
      stringsAsFactors = F
    ) %>% 
      mutate(
        type = tolower(type),
        color = ifelse(type == 'in', "#A0F0A0", '#F0A0A0'),
      )
    
    rv$model <- rbind(rv$model, nn) %>% 
      arrange(type, level)
    
    ### Update opts ---------
    updateTextInput(inputId = "newid", value = "")
    updateTextInput(inputId = "newLab", value = "")
    updateSelectInput(inputId = "newType", selected = "In")
    updateTextInput(inputId = "newDist", value = "")
    updateTextInput(inputId = "newFormula", value = "")
    
    sendSweetAlert(
      session = session,
      title = "Node added",
      # text = "All in order",
      type = "success"
    )
    updateSelectInput(session = session, inputId = "par_id", choices = rv$model %>% filter(type %in% c("In", "in")) %>% pull(id))
  })
  
  ### remove node ----------------
  observeEvent(input$node_remove,{
    
    if (!is.null(input$nodes_rows_selected)) {
      rv$model <- rv$model[-as.numeric(input$nodes_rows_selected),]
    }
    
    showNotification("Selected node removed", type = "warning")
    updateSelectInput(session = session, inputId = "par_id", choices = rv$model %>% filter(type %in% c("In", "in")) %>% pull(id))
  })
  
  ### Edit par tbl ----------
  observeEvent(input$parameters_cell_edit,{
    rv$par <- editData(rv$par, input$parameters_cell_edit, 'par_tbl')
  })
  
  ### Add a par ------------
  observeEvent(input$par_add,{
    np <- data.frame(
      id = input$par_id,
      choice = input$par_choice,
      value = input$par_val,
      stringsAsFactors = F
    )
    
    rv$par <- bind_rows(rv$par, np)
    
    updateTextInput(inputId = "par_choice", value = "")
    updateTextInput(inputId = "par_val", value = "")
    
    sendSweetAlert(
      session = session,
      title = "Choice added",
      # text = "All in order",
      type = "success"
    )
  })
  
  ### remove par ----------------
  observeEvent(input$par_remove,{
    
    if (!is.null(input$parameters_rows_selected)) {
      rv$par <- rv$par[-as.numeric(input$parameters_rows_selected),]
    }
    
    showNotification("Selected choice removed", type = "warning")
    
  })
  
  # ## Update inputs
  # observe(rv$model, {
  #   updateSelectInput(session = session, inputId = "par_id", choices = rv$model %>% filter(type %in% c("In", "in")) %>% pull(id))
  # })
  
  ## Run RA model -------
  observeEvent(input$Run,{
    showModal(modalDialog("Runing model...", footer = NULL))
    # Make sure the parameter table is not empty:
    if(length(rv$par) > 0){
      # Pull the inputs only
      nodes <- rv$model %>%
        filter(type %in% c("In", "in", "IN"))
      
      # TODO: MAKE SURE ALL INPUTS HAVE OPTIONS, ELSE SEND A ERROR MESSAGE FOR THE USER
      
      # Merge the input nodes with the selected choices
      input_values <- lapply(names(input)[names(input) %in% nodes$id], function(id) {
        list(inputId = id, value = input[[id]])
      })
      
      input_values <- do.call(rbind, lapply(input_values, function(x) {
        data.frame(id = x$inputId, distribution = x$value, stringsAsFactors = FALSE)
      }))
      
      tryCatch({ 
        m <- left_join(select(rv$model, -distribution), input_values)
        rv$res <- ra_run(m = m, input$Nsim) %>% 
          select(-sim)
        
        showNotification("Parameter table present, parameters are sourced from the parameter table", type = "message")
      }, error = function(e) {
        # Manejo del error
        showNotification("Error in model definition, check your inputs", type = "error")
        NULL  # Devolver NULL para evitar que se renderice un objeto vacío
      })
    }else{
      tryCatch({ 
        rv$res <- ra_run(m = rv$model, input$Nsim) %>% 
          select(-sim)
        showNotification("No Parameter table found, parameters are sourced from the model table", type = "message")
      }, error = function(e) {
        # Manejo del error
        showNotification("Error in model definition, check your inputs", type = "error")
        NULL  # Devolver NULL para evitar que se renderice un objeto vacío
      })
    }
    
    
    removeModal()
    
    
    # Update the outputs for the plots
    o <- rv$model %>% arrange(-level) %>% pull(id)
    updateSelectInput(session = session, inputId = "outputs", choices = o)
  })
  
  ## Read shapefile ---------
  observeEvent(input$filemap,{
    rv$shp <- read_sf(input$filemap$datapath)
  })
  
  ## Run strat model -----
  observeEvent(input$RunStratified, {
    showModal(modalDialog("Runing the model...", footer = NULL))
  })
  DFs <- eventReactive(input$RunStratified, {
    quantrra::ra_run_strat(m = rv$model, tbl = rv$stratified, nsim = input$Nsim)
  })
  observeEvent(DFs(),{
    removeModal()
  })
  
  output$Outcomes_s <- renderUI({
    opts <- rv$model %>% 
      filter(type == 'Out') %>% 
      pull(id)
    
    selectInput(inputId = 'Outcomes', label = 'Outcomes', opts, selected = opts[length(opts)])
  })
  
  ## Run sensitivity Analysis -------
  observeEvent(input$RunSA, {
    showModal(modalDialog("Running sensitivity analysis...", footer=NULL, easyClose = T))
  })
  
  SA <- eventReactive(input$RunSA, {
    f <- paste(input$DepVars, collapse = '+')
    f <- paste0(input$Outcomes, '~', f)
    quantrra::ra_gsa(data = rv$res, f = eval(parse(text = f)), tree = 'interactive')
  })
  
  observeEvent(SA(), {
    removeModal()
  })
  
  ## Clear table -------
  observeEvent(input$reset,{
    rv$model = data.frame(
      id = c("i1", "o1"),
      label = c("Input 1", "Output 1"),
      type = c("In", "Out"),
      level = c(0, 1),
      distribution = c("Pert(0.1, 0.15, 0.5)", NA),
      formula = c(NA, "i1"),
      shape = 'box',
      color = c("#A0F0A0", "#F0A0A0"),
      stringsAsFactors = F
    )
    rv$par = data.frame(
      id = "i1",
      choice = "choice 1", 
      value = "1"
    )
    
    ### Update opts ---------
    updateSelectInput(session = session, inputId = "par_id", choices = rv$model %>% filter(type %in% c("In", "in")) %>% pull(id))
    
    showNotification("New model has been initiated", type = "warning")
  })
  
  # download excel ---------------
  output$dxl <- downloadHandler(
    filename = function() {
      paste("model-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      # List of data frames
      data_list <- list(
        model = rv$model,
        par = rv$par
      )
      
      # Write data frames to an Excel file
      writexl::write_xlsx(data_list, path = file)
    }
  )
  
  # Outputs --------
  ### Nodes ---------
  # Render the table showing all the nodes in the graph.
  output$nodes <- renderDT({
    rv$model %>% 
      select(c('id', 'label', 'type', 'level', 'distribution', 'formula')) %>%
      arrange(type, level) %>% 
      DT::datatable(
        data = .,
        # rownames = F,
        editable = T, options = list(dom = 'tp')
      )
  })
  
  ### Parameters ---------
  # Render the table showing all the nodes in the graph.
  output$parameters <- renderDT({
    rv$par %>% 
      DT::datatable(data = .,
                    # rownames = F,
                    editable = T, options = list(dom = 'tp'))
  })
  
  ### Output table -----------
  output$MTbl <- renderDT({
    rv$res %>%
      data.frame() %>%
      DT::datatable(data = .,
                    rownames = F)
  })
  
  ### Render the graph.------------
  output$ModelTree <- renderVisNetwork({
    n <- rv$model %>% 
      mutate(title = paste0('ID: ', id, 
                            "<br>Name: ", label))
    
    ra_plot_tree(n, static = F, direction = input$gdir)
  })
  
  ## Render parameters opts ------------
  output$par_inputs <- renderUI({
    req(rv$par)
    # FIlter inputs only
    nodes <- rv$model %>% 
      filter(type %in% c("In", "in"))
    # Get the parameter table
    par <- rv$par
    
    inputs <- lapply(1:nrow(nodes), function(i) {
      
      options <- subset(par, id == nodes$id[i])
      choices <- setNames(options$value, options$choice)
      uiElement <- radioButtons(inputId = nodes$id[i], 
                                label = paste0(nodes$id[i], ". ", nodes$label_input[i]),
                                choices = choices)
      
      column(3, uiElement)
      # uiElement
      
    })
    
    fluidRow(inputs)
  })
  
  #### Risk gauge ----------
  output$riskGauge <- renderPlotly({
    if(!is.null(reactiveModel$results)){
      m <- reactiveModel$results %>%
        pull(pf) %>% quantile()
      
      fig <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = m[3],
        title = list(text = "The risk of introduction \n is 1 in every"),
        type = "indicator",
        mode = "gauge+number",
        # This following part is to add a threshold and the difference to it
        # mode = "gauge+number+delta",
        # delta = list(reference = 380),
        gauge = list(
          axis =list(range = list(NULL, m[4]*1.5)),
          bar = list(color = "#204060"),
          steps = list(
            list(range = c(0, 100e3), color = "#FFD0D0"),
            list(range = c(100e3, 300e3), color = "#FFFFD0"),
            list(range = c(300e3, m[4]*2), color = "#D0FFD0"),
            list(range = c(m[2], m[4]), thicness=0.5, color = "#00000000", line = list(color="#50206080", width=4))
          )
        )
      )
      fig <- fig %>%
        layout(margin = list(l=20,r=30))
      
      fig
      
    }
    
  })
  
  ### Gauge plot ----------
  output$scorePlot <- renderPlotly({
    req(rv$res)
    rv$res[,input$outputs] %>%
      ra_plotScore(brks = c(0, 3, 7, 20)) 
  })
  
  ### Histograms -----------
  output$P4 <- renderPlotly({
    if(!is.null(rv$res)){
      # Filter only outputs
      o <- rv$model %>% 
        filter(type == 'Out')
      
      if(nrow(o) > 1){
        PL <- lapply(1:nrow(o), function(x){
          # x <- 1
          p <- rv$res %>% 
            ggplot() +
            geom_histogram(aes_string(o$id[x]), fill = 'red4') +
            geom_vline(data = data.frame(m = round(quantile(rv$res[,o$id[x]], 0.5), 4)), aes(xintercept = m), lty = 1, lwd = 1, col = 'grey20') +
            labs(title = paste0(o$id[x], ': ', o$label[x])) +
            theme_minimal()
          
          ggplotly(p)
        })
        
        subplot(PL, nrows = 2)
      }else{
        p <- rv$res %>% 
          ggplot() +
          geom_histogram(aes_string(o$id), fill = 'red4') +
          geom_vline(data = data.frame(m = round(quantile(rv$res[,o$id], 0.5), 4)), aes(xintercept = m), lty = 1, lwd = 1, col = '#904444') +
          labs(title = paste0(o$id, ': ', o$label)) +
          theme_minimal()
        
        ggplotly(p)
      }
    }
    
  })
  
  
  ### Strat outputs -----------
  output$InData <- renderDT({
    rv$stratified %>% 
      DT::datatable(data = ., options = list(pageLength = 5))
  })
  
  # Ranking plot
  output$Ranking_p <- renderPlotly({
    ra_plot_ranking(x = DFs(), var = 'Pf')
  })
  
  # Map
  output$Map_p <- renderPlot({
    # require(map()){
      rv$shp %>%  # This is our spatial shape file
        # left_join(DFs(), by = c('Entidad' = 'IDs')) %>%  # we use left join to join our model results
        ggplot() + # We call ggplot
      geom_sf() + # We add a layer representing the polygons colored by the variable E
        # geom_sf(aes(fill = O4_m)) + # We add a layer representing the polygons colored by the variable E
        # scale_fill_gradient(low = 'black', high = 'red') + # set the color scale
        theme_void() # Theme of the plot
    # }
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
  
  ### Qualitative model -----------
  output$treeplot_model <- renderVisNetwork({
    
    nodes <- NodesAdd() %>% 
      rename(old_label = label) %>%
      mutate(color = recode(type, Out = "#EE5C42", In = "#66CD00"),
             shape = "box",
             label = paste0(id, '\n', old_label)) %>%
      select(-old_label) 
    
    edges <- EdgesAdd() %>%
      select(from, to)
    
    visNetwork(nodes = nodes, edges = edges) %>% 
      visEdges(arrows = "to", color = "black") %>% 
      visPhysics(enabled = FALSE, solver = "hierarchicalRepulsion") %>%
      visHierarchicalLayout(levelSeparation = input$Septree, direction = input$Dirtree, nodeSpacing = 200) %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  ## Downloads -------
  # download excel ---------------
  # TODO Add messages when one of the files is missing
  output$dl <- downloadHandler(
    # if(is.null(rv$par)){
    #   # showNotification("No parameter table found", type = "warning")
    # }
    filename = function() {
      paste("model-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      # List of data frames
      data_list <- list(
        model = rv$model,
        par = rv$par
      )
      
      # Write data frames to an Excel file
      writexl::write_xlsx(data_list, path = file)
    }
  )
  
  
  # Example files ------------------------
  ## OIRSA ----------
  output$downloadOIRSA <- downloadHandler(
    filename <- function() {
      paste("OIRSA", "zip", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/OIRSA.zip", file)
    },
    contentType = "application/zip"
  )
  
  ## ASF Products -----------
  output$downloadASFP <- downloadHandler(
    filename <- function() {
      paste("asf_products", "zip", sep=".")
    },
    
    content <- function(file) {
      file.copy("www/asf_products.zip", file)
    },
    contentType = "application/zip"
  )
  
  ## Outcomes
  output$Outcomes <- renderUI({
    opts <- colnames(rv$res)
    selectInput(inputId = 'Outcomes', label = 'Outcomes', opts, selected = opts[length(opts)])
  })
  ## Dependent vars
  output$DepVar <- renderUI({
    opts <- colnames(rv$res)
    selectInput('DepVars', 'Dependent Variables', opts, multiple = T, selected = opts[1:(length(opts) - 1)])
  })
} %>% shinyServer() 