#' @export
runQuantRRA <- function() {
  appDir <- system.file("shiny", "QuantRRA", package = "QuantRRA")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}