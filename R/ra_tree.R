#' Function to plot a model tree from a quantrra table
#' 
#' @param M Model list with nodes and edges, see example quantrra::ASFm
#' @param fontColor Color of the text displayed in the figure
#' @param shape shape of the nodes, options are the same from the DiagrammeR library: ellipse, oval, diamond, egg, plaintext, point, square, triangle
#' @export

ra_tree <- function(M, fontColor = 'black', shape = 'rectangle'){
  # If no color defined, add green for inputs, and red for outputs
  if(is.null(M$nodes$color)){
    M$nodes <- M$nodes %>% 
      mutate(color = recode(type, Out = '#FFF0F0', out = '#FFF0F0', `in` = '#F0FFF0', In = '#F0FFF0'))
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