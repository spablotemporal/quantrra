#' Function to open the quantrra interactive platform
#'  @description
#'  This function runs the quantrra dashboard shiny application. Which serves as a GUI for Risk Assessment and model building 
#' @examples
#' ## DO NOT RUN
#' run_quantrra()
#' 
#' @export
run_quantrra <- function() {
  appDir <- system.file("shiny", "quantrra", package = "quantrra")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `quantrra` or contact jpgo@ucdavis.edu for support.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}