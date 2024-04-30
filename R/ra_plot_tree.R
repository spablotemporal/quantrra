#' Plot a model tree from a quantrra table
#' @description
#' A traditional representation of the model table is plotted using the library DiagrammeR.
#' This function expects as an input a list object with named elements as nodes and edges, corresponding to the tables specifying the nodes of the tree and connections between them
#' Shape and Color for the nodes are obtained directly from variables in the nodes table. If missing, default colors and shape are used
#' 
#' @param m Model table with columns id, label, type, distribution and formula see examples.
#' @param fontColor Color of the text displayed in the figure
#' @param shape shape of the nodes, options are the same from the DiagrammeR library: ellipse, oval, diamond, egg, plaintext, point, square, triangle
#' @param static if TRUE, the figure will be created with DiagrammeR for a static visualization. When FALSE, visNetwork is used for a dynamic visualization
#' @param edgetbl if TRUE, return a table of edges for visualization purposes
#' @return a grViz/htmlwidget figure representing the risk assessment tree
#' @examples
#' # Use one of the examples from the library
#' m <- quantrra::OIRSA
#' ra_plot_tree(m)
#' 
#' @export

ra_plot_tree <- function(m, fontColor = 'black', shape = 'rectangle', static = T, edit = F, edgetbl = F){
  # if a list provided, extract the first element (model table)
  if(class(m) == "list"){
    # if provided a list, make sure it has the correct names for elements
    if(!"model" %in% names(m)){
      stop("When providing a list, make sure to include the elements correctly named")
    }else{
      m <- m$model
    }
  }
  # Make sure a valid variables area provided
  cn <- c("id", "label", "type", "distribution", "formula")
  if (any(!cn %in% colnames(m))){
    stop("Provide valid variable names. Model file must include variables that specify the id, label, type, distribution and formula see examples")
  }
  # If no color defined, add green for inputs, and red for outputs
  if(is.null(m$color)){
    m <- m %>% 
      mutate(
        type = tolower(type),
        type = recode(type, input = "in", output = "out")
      ) %>% 
      mutate(color = recode(type, out = '#FFF0F0', `in` = '#F0FFF0'))
  }
  
  if(is.null(m$shape)){
    m$shape <- shape
  }
  # Define nodes
  vs <- paste0(
    m$id, 
    "[label = '", m$id, '\n', m$label, "' , fillcolor = '", m$color, "']"
  ) %>% paste(., collapse = '\n ')
  # Define edges
  ## v0.1.1 -------------------
  # After version 0.1.1, the edge table is generated from the nodes, no need to specify edge table
  # First we get a list of the nodes involved in each out calculation
  es <- m %>% 
    mutate(type = tolower(type)) %>% 
    filter(type %in% c("out")) %>% 
    select(id, formula) %>% 
    pull(formula, id) %>% 
    lapply(., function(x){
      strsplit(x, "[^[:alnum:]]+") %>% 
        unlist()
    })
  
  es <- lapply(1:length(es), function(n){
    data.frame(
      from = unlist(es[n]),
      to = names(es)[n]
    )
  }) %>% 
    do.call(rbind,.) %>% 
    distinct() %>% 
    filter(from %in% m$id) # make sure that use only the ids in nodes table
    
  if(edgetbl){
    return(es)
  }else{
    # Make the figure -----------
    ## Static figure ------------
    if(static){
      es <- paste0(es$from, ' -> ', es$to, ';\n ') %>% paste(., collapse = '\n ')
      # TODO: add warning if no shape matches the options
      # if(!shape %in% c("box", "diamond", "plaintext", "Mrecord", "point", "house", "invtriangle", "doublecircle")){
      #   warning("Specified shape not part of the options, options include: 'box', 'diamond', 'plaintext', 'Mrecord', 'point', 'house', 'invtriangle', 'doublecircle'")
      # }
      DiagrammeR::grViz(diagram = paste0(
        "digraph flowchart {
    node [
      fontname = arial,", 
        "shape =", shape, 
        ",style = filled, 
      fontcolor =", fontColor,
        "]\n ",
        vs,
        es,
        "}",
        sep = '\n'
      ))
    }else{
      # TODO: add warning if no shape matches the options
      visNetwork::visNetwork(m, es) %>% 
        visNetwork::visHierarchicalLayout(direction = "LR") %>% 
        visNetwork::visOptions(manipulation = list(enabled = edit,
                                                   editNodeCols = c('id', 'label', 'type', 'level', 'distribution', 'formula'),
                                                   addNodeCols = c('id', 'label', 'type', 'level', 'distribution', 'formula')))
    }
  }
  
}