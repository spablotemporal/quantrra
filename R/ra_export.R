#' Export a model file to a file for external use
#' 
#' @description
#' This function conveniently exports a set of nodes and edges into a xlsx or zip file for external use.
#' The file exported can be used for editing outside R, for the shiny interactive interface of quantrra or imported back into an R session.
#' 
#' @param n Model table
#' @param e optional table of edges that defines the connections between the nodes. Used for visualization purposes
#' @param dir directory path for the file
#' @param name name of the output file (not necessary to add extension)
#' @param format the format used to export the model file, can be: "zip" or "xlsx"
#' @return Function will export a file to the specified location on the operating system.
#' @examples
#' m <- quantrra::ASFm # use one of the included example model files
#' 
#' # This will export only the nodes (model table)
#' ra_export(n = m$nodes, format = "zip")
#' 
#' # We can also add an edge table, used for visualization purposes
#' ra_export(n = m$nodes, e = m$edges, format = "xlsx")
#' 
#' @export

ra_export <- function(n, e = NULL, dir = 'model', name = 'model', format = "zip"){
  if(format == "zip"){
    # Export to Zip format ---------
    dir.create(dir) # Create the directory
    write.csv(n, file = paste(dir, 'nodes.csv', sep = '/'), row.names = F) # export nodes
    if(!is.null(e)){
      write.csv(e, file = paste(dir, 'edges.csv', sep = '/'), row.names = F) # export edges
    } 
    zip(zipfile = name, files = dir) # create zip file
    unlink(dir, recursive = T) # delete directory used for zip file
  }else if(format == "xlsx"){
    # Export to xlsx format -----------
    ## Export nodes
    xlsx::write.xlsx(
      x = n, 
      sheetName = "nodes", 
      file = paste0(name, ".xlsx"), 
      row.names = F
    )
    if(!is.null(e)){
      # Export edges
      xlsx::write.xlsx(
        x = e, 
        sheetName = "edges", 
        file = paste0(name, ".xlsx"), 
        append = T,
        row.names = F
      )
    }
    
  }
}