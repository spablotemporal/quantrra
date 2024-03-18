#' Import a model file from zip or xlsx
#' @description
#' This function imports a previously created model file. This can be either a zip or xlsx file.
#' 
#' @param p Path to the model file created in the format from ra_export, which can be either a zip file or a xlsx
#' @export

ra_import <- function(p){
  # identify file type
  ## split path string
  ft <- sub(pattern = ".+\\.", replacement = "", x = p)
  # depending on the file type, read it with the appropiate method
  m <- switch (ft,
    # read the zip file           
    zip = {
      # Extract files from the zip archive
      fs <- unzip(p, exdir = tempdir())
      
      # read the files into a list
      m <- lapply(fs, function(x){
        read.csv(x)
      }) %>% 
        `names<-`(gsub(".csv$", "", basename(fs)))
    },
    # read the xlsx file
    xlsx = {
      openxlsx::getSheetNames(p) %>% 
        lapply(., function(x) openxlsx::read.xlsx(p, sheet = x)) %>% 
        `names<-`(openxlsx::getSheetNames(p))
    },
    # if none of above, return error message
    "Unsupported file type"
  )
  return(m)
}