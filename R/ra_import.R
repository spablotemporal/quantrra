#' Import a model file from zip or xlsx
#' @description
#' A short description...
#' 
#' @param p Path to the model file
#' @export


ra_import <- function(p){
  # identify file type
  ## split path string
  ft <- sub(pattern = ".+\\.", replacement = "", x = p)
  # depending on the file type, read it with the appropiate method
  m <- switch (ft,
    # read the zip file           
    zip = "This is a zip file",
    # read the xlsx file
    # xlsx = "This is a xlsx file",
    xlsx = xlsx::getSheets(p),
    # if none of above, return error message
    "Unsupported file type"
  )
  
  # M <- list(
  #   nodes = read.csv(unz(p, 'nodes.csv')),
  #   edges = read.csv(unz(p, 'edges.csv'))[-1,]
  # )
  return(m)
}