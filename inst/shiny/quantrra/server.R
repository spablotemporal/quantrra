# Server --------
function(input, output, session){
  # Create empty reactive values -----------
  rv <- reactiveValues(
    model = init_nodes, # Nodes
    # edges = ra_plot_tree(init_nodes, edgetbl = T), # Edges
    stratified = NULL,
    par = ra_model$par,
    res = NULL, # Empty table for results
    qual_nodes = data.frame(
      id = character(),
      label = character(),
      type = character(),
      level = character(),
      distribution = character(),
      formula =  character(),
      input =  character(),
      label_input =  character(),
      stringsAsFactors = FALSE
    )
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
  
  # # read from file -----------
  # observeEvent(input$upload, {
  #   im <- ra_import(input$upload$datapath)
  #   rv$model <- im$model
  #   rv$stratified <- im$stratified
  # })
  
  ## Make the edits to the table -------------
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
      level = max(rv$model$level),
      distribution = ifelse(input$newType == "In", input$newDist, NA),
      formula = ifelse(input$newType == "Out", input$newFormula, NA),
      shape = 'box',
      stringsAsFactors = F
    ) %>% 
      mutate(
        type = tolower(type),
        color = ifelse(type == 'in', "#A0F0A0", '#F0A0A0'),
      )
    
    rv$model <- rbind(rv$model, nn)
    
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
  })
  
  ## Run the model -------
  observeEvent(input$Run,{
    showModal(modalDialog("Runing the model...", footer = NULL))
    rv$res <- ra_run(m = rv$model, input$Nsim)
    
    removeModal()
  })
  
  # in server()
  map <- reactive({
    # shpdf is a data.frame with the name, size, type and datapath
    # of the uploaded files
    shpdf <- input$filemap
    
    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    
    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.
    
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"
    ))
    map
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
      id = c("I1", "O1"),
      label = c("Input 1", "Output 1"),
      type = c("In", "Out"),
      level = c(0, 1),
      distribution = c("Pert(0.1, 0.15, 0.5)", NA),
      formula = c(NA, "I1"),
      shape = 'box',
      color = c("#A0F0A0", "#F0A0A0"),
      stringsAsFactors = F
    )
    
    # rv$edges = ra_plot_tree(rv$model, edgetbl = T)
  })
  
  ## Outputs --------
  ### Nodes ---------
  # Render the table showing all the nodes in the graph.
  # output$nodes <- renderDT({
  #   rv$model %>% 
  #     select(c('id', 'label', 'type', 'level', 'distribution', 'formula')) %>%
  #     DT::datatable(data = .,
  #                   # rownames = F,
  #                   editable = T)
  # })
  
  ### Nodes ---------
  # Render the table showing all the nodes in the graph.
  output$nodes <- renderDT({
    rv$model %>% 
      select(c('id', 'label', 'type', 'level', 'distribution', 'formula')) %>%
      arrange(level) %>% 
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
  
  ### P4 -----------
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
    map() %>%  # This is our spatial shape file
      st_as_sf() %>% 
      left_join(DFs(), by = c('Entidad' = 'IDs')) %>%  # we use left join to join our model results
      ggplot() + # We call ggplot
      geom_sf(aes(fill = O4_m)) + # We add a layer representing the polygons colored by the variable E
      scale_fill_gradient(low = 'black', high = 'red') + # set the color scale
      theme_void() # Theme of the plot
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
  ### In xlsx
  
  output$dl <- downloadHandler(
    filename = function() { "model.xlsx"},
    content = function(file) {write_xlsx(list(model = rv$model), path = file)}
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