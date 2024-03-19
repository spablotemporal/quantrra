#' Plot the distribution using a gauge plot
#' 
#' @description
#' This function uses plotly to generate a gauge plot using a vector of numbers. Gauge plots provide a visual representation of a numeric magnitude 
#' 
#' @param x vector of numbers 
#' @param main Main title for the figure
#' @param brks breaks for the risk thresholds
#' @return A plotly gauge figure 
#' @examples
#' # use one of the example models
#' m <- quantrra::OIRSA
#' 
#' ra_run(m = m$nodes, nsim = 100) %>% # run the model
#'   pull(P) %>% # extract the final probability
#'   ra_plot_score() # plot the score as a gauge plot
#' @export


ra_plot_score <- function(x, main = "", brks = NULL){
  m <- x %>% quantile()
  
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = m[3],
    title = list(text = main),
    type = "indicator",
    mode = "gauge+number",
    # This following part is to add a threshold and the difference to it
    # mode = "gauge+number+delta",
    # delta = list(reference = 380),
    gauge = list(
      axis =list(range = list(NULL, m*2)),
      bar = list(color = "#205040"),
      steps = list(
        list(range = c(brks[1], brks[2]), color = "#FFD0D0"),
        list(range = c(brks[2], brks[3]), color = "#FFFFD0"),
        list(range = c(brks[3], m*2), color = "#D0FFD0"),
        list(range = c(m[2], m[4]), thicness=0.5, color = "#00000000", line = list(color="#505020", width=4))
      )
    )
  ) 
  fig <- fig %>%
    layout(margin = list(l=20,r=30))
  
  fig 
}