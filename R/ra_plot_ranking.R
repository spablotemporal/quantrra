#' Function to plot the ranking
#' 
#' @param M Model outputs
#' @param var variable to plot
#' @param id Name of IDs
#' @param col color of the plot
#' @export
#' 
ra_plot_ranking <- function(d, var, id = 'IDs', col = 'red4'){
  # Ranking plot
  x <- pull(d, 'IDs')
  y <- pull(d, paste0(var, '_m'))
  lb <- pull(d, paste0(var, '_q05'))
  ub <- pull(d, paste0(var, '_q95'))
  # lb <- rabRR$
  plotly::plot_ly(x = x,
          y = y, type = 'bar', color = I(col), 
          error_y = ~list(symmetric = F,
                          arrayminus = lb,
                          array = ub,
                          color = '#000000')
  ) %>% 
    plotly::layout(xaxis = list(categoryorder = "total descending"))
}