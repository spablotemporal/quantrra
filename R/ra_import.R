#' Function to calculate the risk
#' 
#' @param p Path to the model file
#' @export


ra_import <- function(p){
  M <- list(nodes = read.csv(unz(p, 'nodes.csv')),
               edges = read.csv(unz(p, 'edges.csv'))[-1,]
               )
  return(M)
}