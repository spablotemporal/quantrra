#' Function to plot the model tree
#' 
#' @param M Model list with nodes and edges, see example QuantRRA::ASFm
#' @export

ModelTree <- function(M){
  vs <- paste0(M$nodes$id, "[label = '", Tbl$id, '\n', M$nodes$label, "' , color = '", init_nodes$color, "']") %>% paste(., collapse = '\n ')
  es <- paste0(M$edges$from, ' -> ', M$edges$to, ';\n ') %>% paste(., collapse = '\n ')
  
  grViz(diagram = paste0(
    "digraph flowchart {
    node [fontname = arial, shape = rectangle]\n ",
    vs,
    es,
    "}",
    sep = '\n'
  ))
}