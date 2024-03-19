#' Plot a model tree from a quantrra table
#' @description
#' A traditional representation of the model table is plotted using the library DiagrammeR.
#' This function expects as an input a list object with named elements as nodes and edges, corresponding to the tables specifying the nodes of the tree and connections between them
#' Shape and Color for the nodes are obtained directly from variables in the nodes table. If missing, default colors and shape are used
#' 
#' @param M Model list with nodes and edges, see example quantrra::OIRSA
#' @param fontColor Color of the text displayed in the figure
#' @param shape shape of the nodes, options are the same from the DiagrammeR library: ellipse, oval, diamond, egg, plaintext, point, square, triangle
#' @return a grViz/htmlwidget figure representing the risk assessment tree
#' @examples
#' # Use one of the examples from the library
#' m <- quantrra::OIRSA
#' ra_plot_tree(m)
#' 
#' @export

ra_plot_tree <- function(M, fontColor = 'black', shape = 'rectangle'){
  # Make sure a valid variables area provided
  cn <- c("id", "label", "type", "distribution", "formula")
  if (any(!cn %in% colnames(M$nodes))){
    stop("Provide valid variable names. Model file must include variables that specify the id, lable, type, distribution and formula see examples")
  }
  # If no color defined, add green for inputs, and red for outputs
  if(is.null(M$nodes$color)){
    M$nodes <- M$nodes %>% 
      mutate(
        type = tolower(type),
        type = recode(type, input = "in", output = "out")
      ) %>% 
      mutate(color = recode(type, out = '#FFF0F0', `in` = '#F0FFF0'))
  }
  
  if(is.null(M$nodes$shape)){
    M$nodes$shape <- 'rectangle'
  }
  # Define nodes
  vs <- paste0(
    M$nodes$id, 
    "[label = '", M$nodes$id, '\n', M$nodes$label, "' , fillcolor = '", M$nodes$color, "']"
  ) %>% paste(., collapse = '\n ')
  # Define edges
  es <- paste0(M$edges$from, ' -> ', M$edges$to, ';\n ') %>% paste(., collapse = '\n ')
  
  # Make the figure
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
}