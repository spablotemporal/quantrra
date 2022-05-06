#' Function to sample from different distributions 
#' 
#' @param x Vector of the distribution
#' @export

# Distribution plots (with min, max, mean, etc)
plotDist <- function(x, col = 'red4', main = ''){
  tb <- data.frame(min = min(x), 
                   max = max(x), 
                   mean = mean(x), 
                   StdDev = sd(x)
                   # N = length(x)
  ) %>%
    t() %>% data.frame() %>% 
    format(., digits = 3, scientific = F) %>%
    ggtexttable(cols = 'Value', theme = ttheme('light'))
  
  p <- x %>% 
    data.frame() %>% 
    ggplot() +
    geom_histogram(aes(x = .), fill = col, col = 'white', lwd = 0.1) +
    labs(title = main) +
    theme_minimal()
  
  ggarrange(p, tb, widths = c(3, 1)) 
}