#' Function to export a model file
#' 
#' @param n table of nodes
#' @param e table of edges
#' @param dir directory path for the file
#' @param name name of the output file (not necessary to ad extension .zip)
#' @export

exportModel <- function(n, e, dir = 'model', name = 'model'){
  dir.create(dir) # Create the directory
  write.csv(n, file = paste(dir, 'nodes.csv', sep = '/'), row.names = F) # export nodes
  write.csv(e, file = paste(dir, 'edges.csv', sep = '/'), row.names = F) # export edges
  zip(zipfile = name, files = dir) # create zip file
  unlink(dir, recursive = T) # delete directory used for zip file
}