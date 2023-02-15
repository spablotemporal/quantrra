#' Function to open the QuantRRA interactive platform
#' @export
runQuantRRA <- function() {
  appDir <- system.file("shiny", "QuantRRA", package = "QuantRRA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `QuantRRA` or contact jpgo@ucdavis.edu for support.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}