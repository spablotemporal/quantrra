#' Plot a distribution with descriptive statistics
#' 
#' @param x Vector of the sampled values
#' @param col color used for the fill
#' @param main Main text used as a title for the plot
#' @export

# Distribution plots (with min, max, mean, etc)
ra_plot_dist <- function(x, col = "red4", main = ""){
  tb <- data.frame(min = min(x), 
                   max = max(x), 
                   mean = mean(x), 
                   StdDev = sd(x)
  ) %>%
    t() %>% data.frame() %>% 
    format(., digits = 3, scientific = F) %>%
    ggtexttable(cols = "Value", theme = ttheme('light'))
  
  p <- x %>% 
    data.frame() %>% 
    ggplot() +
    geom_histogram(aes(x = .), fill = col, col = 'white', lwd = 0.1) +
    labs(title = main) +
    theme_minimal()
  
  ggarrange(p, tb, widths = c(3, 1)) 
}