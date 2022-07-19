#' Function to plot the model tree
#' 
#' @param M Model list with nodes and edges, see example QuantRRA::ASFm
#' @export

ModelTree <- function(M){
  vs <- paste0(M$nodes$id, "[label = '", M$nodes$id, '\n', M$nodes$label, "' , color = '", M$nodes$color, "']") %>% paste(., collapse = '\n ')
  es <- paste0(M$edges$from, ' -> ', M$edges$to, ';\n ') %>% paste(., collapse = '\n ')
  
  DiagrammeR::grViz(diagram = paste0(
    "digraph flowchart {
    node [fontname = arial, shape = rectangle]\n ",
    vs,
    es,
    "}",
    sep = '\n'
  ))
}