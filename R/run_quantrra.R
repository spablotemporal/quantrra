#' Function to open the quantrra interactive platform
#' @export
run_quantrra <- function() {
  appDir <- system.file("shiny", "quantRRA", package = "quantRRA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `quantrra` or contact jpgo@ucdavis.edu for support.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}