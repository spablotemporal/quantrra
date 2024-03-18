#' (experimental) fit a set of observations to a specific distribution 
#' 
#' @description
#' Experimental: This function fits a vector of values to a specified distribution. 
#' The distribution is fitted based on an optimization of the residual sum of squares (rss).
#' Currently, the function supports only a few distributions, including: NORMAL, POISSON, PERT, TRIANGLE, and UNIFORM
#' 
#' 
#' @param x Data to fit
#' @param dist Distribution specification
#' @export


ra_fit <- function(x, dist, full = F, bw = 0.2, digits = 2){
  # Function to get the mode (from https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
  getmode <- function(v) {
    v <- x
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  dist <- toupper(dist)
  
  rss <- function(pars, x, distribution){
    # pars = c(4, 1); data = d; distribution = x
    dist <- quantrra::ra_sample(distribution, n = 1, T)[-3] # identify distribution
    dist$parameters <- pars # Set new parameters
    out <- quantrra::ra_sample(dist, n = length(x)) # Sample with new params
    
    sum((sort(x) - sort(out))^2) # Estimate rss
  }
  
    if(dist %in% c('NORMAL')){ # For Normal distribution
      Opt <- optim(par = c(mean(x), sd(x)), fn = rss, x = x, distribution = dist, lower = 0)
    }else if(dist %in% c('POISSON')){ # For poisson
      Opt <- optim(par = c(mean(x)), fn = rss, x = x, distribution = dist, lower = 0)
    }else if(dist %in% c('PERT')){ # For pert
      Opt <- optim(par = c(min(x), getmode(x), max(x)), fn = rss, x = x, distribution = dist, lower = 0)
    }else if(dist %in% c('TRIANGLE')){ # For triangle
      Opt <- optim(par = c(min(x), getmode(x), max(x)), fn = rss, x = x, distribution = dist, lower = 0)
    }else if(dist %in% c('UNIFORM')){ # For uniform
      Opt <- optim(par = c(min(x), max(x)), fn = rss, x = x, distribution = dist, lower = 0)
    }else if(dist %in% c('INVERSE-GAMMA', 'INV-GAMMA', 'INVERSE GAMMA')){ # For inverse gamma
      Opt <- optim(par = c(1, 1), fn = rss, x = x, distribution = dist, lower = 0)
    }
    Opt[['distribution']] <- dist
    Opt <- list(distribution = paste0(Opt$distribution, '(', paste0(round(Opt$par, digits), collapse = ', '), ')'), 
                value = round(Opt$value, digits)) %>% 
      do.call(cbind, .)
    Opt[,2] <- ifelse(Opt[,2] == 0, NA, Opt[,2])
    return(Opt)
  
  # Code to return a plot
  # if(full){
  #   y <- sampleDist(Opt[,1], n = 1000)
  #   tb <- data.frame(min = min(x),
  #                    max = max(x),
  #                    mean = mean(x),
  #                    StdDev = sd(x),
  #                    OLS = Opt[,2]
  #                    # N = length(x)
  #   ) %>%
  #     t() %>% data.frame() %>%
  #     format(., digits = 3, scientific = F) %>%
  #     ggtexttable(cols = 'Value', theme = ttheme('light'))
  #   
  #   p <- x %>%
  #     data.frame() %>%
  #     ggplot() +
  #     geom_histogram(aes(x = .), fill = 'red4', col = 'white', lwd = 0.1) +
  #     geom_density(aes(x = ., y = ..density.. * (nrow(.) * bw))) +
  #     labs(title = Opt[,1], y = '') +
  #     theme_minimal()
  #   
  #   # return(p)
  #   
  #   ggarrange(p, tb, widths = c(3, 1))
  # }else{
  #   return(Opt)
  # }
}