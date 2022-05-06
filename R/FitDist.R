#' Function to Fit a distribution
#' 
#' @param x Data to fit
#' @param dist Distribution specification
#' @export


FitDist <- function(x, dist, full = F){
  # Function to get the mode (from https://www.tutorialspoint.com/r/r_mean_median_mode.htm)
  getmode <- function(v) {
    v <- x
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  dist <- toupper(dist)
  rss <- function(pars, x, distribution){
    # pars = c(4, 1); data = d; distribution = x
    dist <- SampleDist(distribution, n = 1, T)[-3] # identify distribution
    dist$parameters <- pars # Set new parameters
    out <- SampleDist(dist, n = length(x)) # Sample with new params
    
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
  Opt <- list(distribution = paste0(Opt$distribution, '(', paste0(round(Opt$par, 4), collapse = ', '), ')'), 
              value = round(Opt$value, 4)) %>% 
    do.call(cbind, .)
  Opt[,2] <- ifelse(Opt[,2] == 0, NA, Opt[,2])
  
  if(full){
    plotDist(x)
  }else{
    return(Opt)
  }
}