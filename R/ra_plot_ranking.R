#' Barplot used to rank the estimate and 95 percentile of the output
#' 
#' @description
#' A barplot arranged in descending order is plotted for the estimates and 95% percentiles obtained from a stratified analysis.
#' 
#' @param x output from a stratified analysis obtained by the function ra_run_strat
#' @param var variable to plot
#' @param id Name given to the unique identifier of the stratified var. defaul is id = "IDs"
#' @param col color of the plot
#' @param format whether the figure is static (using ggplot) or interactive (using plotly)
#' @param xlab label used for the x axis
#' @param ylab label used for the y axis
#' @export
#' 
ra_plot_ranking <- function(x, var, id = "ids", col = "red4", format = "static", xlab = "", ylab = ""){
  # Ranking plot
  px <- pull(x, "ids")
  py <- pull(x, paste0(var, "_m"))
  lb <- pull(x, paste0(var, "_q05"))
  ub <- pull(x, paste0(var, "_q95"))
  
  p <- switch (
    format,
    # Interactive plot -------------
    interactive = plotly::plot_ly(
      type = "bar",
      x = px, y = py,
      color = I(col),
      error_y = ~list(
        symmetric = F,
        arrayminus = lb,
        array = ub - py, color = "#000000"
      )
    ) %>% 
      plotly::layout(xaxis = list(categoryorder = "total descending")),
    static = data.frame(px, py, lb, ub) %>% 
      ggplot() +
      geom_bar(
        aes(x = reorder(px, -py), y = py), 
        fill = col,
        stat = "identity"
      ) +
      geom_errorbar(
        aes(x=px, ymin=lb, ymax=ub), 
        width=0.4, colour="black", alpha=0.9
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, size = 6)
      ) +
      # lims(y = c(0, max(ub))) +
      labs(x = xlab, y = ylab)
  )
  
  return(p)
}