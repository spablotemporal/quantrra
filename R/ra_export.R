#' Export a model file to a file for external use
#' 
#' @description
#' This function conveniently exports a set of nodes and edges into a xlsx or zip file for external use.
#' The file exported can be used for editing outside R, for the shiny interactive interface of quantrra or imported back into an R session.
#' 
#' @param m Model table, can be a single data frame or a list
#' @param dir directory path for the file
#' @param name name of the output file (not necessary to add extension)
#' @param format the format used to export the model file, can be: "zip" or "xlsx"
#' @return Function will export a file to the specified location on the operating system.
#' @examples
#' m <- quantrra::ASFm # use one of the included example model files
#' 
#' # This will export only the nodes (model table)
#' ra_export(m = m$nodes, format = "zip")
#' 
#' # We can also add an edge table, used for visualization purposes
#' ra_export(m = m, format = "xlsx")
#' 
#' @export

ra_export <- function(m, dir = 'model', file = 'model', format = "xlsx"){
  if(format == "zip"){
    # Export to Zip format ---------
    ## identify if list or df
    to <- class(m)
    dir.create(dir) # Create the directory
    switch(
      to,
      # When object type is list:
      list = {lapply(names(m), function(x){
        write.csv(x = m[[x]], file = paste0(dir, "/", x, ".csv"), row.names = F)
      })},
      # When is not (data.frame, data table, etc...)
      write.csv(x = m, file = paste0(dir, "/", file, ".csv"), row.names = F)
    )
    zip(zipfile = file, files = dir) # create zip file
    unlink(dir, recursive = T) # delete directory used for zip file
  }else if(format == "xlsx"){
    # Export to xlsx format -----------
    openxlsx::write.xlsx(x = m, file = paste0(file, ".xlsx"))
  }
}