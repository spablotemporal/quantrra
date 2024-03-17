#' Export a model file to a zip file for external use
#' 
#' @description
#' This function conveniently exports a set of nodes and edges into a zip file for external use.
#' The zip file exported can be used in the shiny interactive interface of quantrra or imported back into an R session.
#' 
#' @param n table of nodes
#' @param e table of edges
#' @param dir directory path for the file
#' @param name name of the output file (not necessary to ad extension .zip)
#' @export

ra_export <- function(n, e, dir = 'model', name = 'model'){
  dir.create(dir) # Create the directory
  write.csv(n, file = paste(dir, 'nodes.csv', sep = '/'), row.names = F) # export nodes
  write.csv(e, file = paste(dir, 'edges.csv', sep = '/'), row.names = F) # export edges
  zip(zipfile = name, files = dir) # create zip file
  unlink(dir, recursive = T) # delete directory used for zip file
}