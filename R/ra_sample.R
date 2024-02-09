#' Function to sample from different distributions 
#' 
#' @param x Distribution specification, must specify the name of the distribution and parameters, e.g. Normal (10, 0.4) .Current distributions supported include: uniform, binomial, triangle, poisson, normal and pert.
#' @param n Number of observations to be sampled from the distribution
#' @param full TRUE/FALSE. If TRUE a list with the samples, and distribution specified will be returned, Only a vector of the observations will be returned otherwise.
#' @export

ra_sample <- function(x, n, full = F){
  if(rlang::is_string(x)){ # If input is a string, convert to list
    dist <- list()
    # identify distribution (first element of the list)
    dist[['distribution']] <- sub(pattern = '\\(.*', replacement = '', x = x) %>% toupper()
    # Extract parameters
    dist[['parameters']] <- sub(".*(?:\\((.*)\\)).*|.*", "\\1",x) %>% 
      stringr::str_split(string = ., pattern = ',') %>% unlist() %>% as.numeric() 
  }else{ # else, just toupper distribution name
    dist <- x
    dist$distribution <- toupper(dist$distribution)
  }
  # Sample from distribution
  if(dist$distribution == 'PERT'){
    xi <- mc2d::rpert(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3])
  }else if(dist$distribution %in% c('BINOM', 'BINOMIAL')){
    xi <- rbinom(n = n, size = dist$parameters[1], prob = dist$parameters[2])
  }else if(dist$distribution %in% c('TRIANGLE', 'TRI', 'TRIANG')){
    xi <- mc2d::rtriang(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3])
  }else if(dist$distribution %in% c('POISSON', 'POI')){
    xi <- rpois(n = n, lambda = dist$parameters[1])
  }else if(dist$distribution %in% c('NORMAL', 'GAUSS', 'GAUSSIAN')){
    xi <- rnorm(n = n, mean = dist$parameters[1], sd = dist$parameters[2])
  }else if(dist$distribution %in% c('UNIFORM', 'UNIF', 'U')){
    xi <- runif(n = n, min = dist$parameters[1], max = dist$parameters[2])
  }else if(dist$distribution %in% c('INVERSE-GAMMA', 'INV-GAMMA', 'INVERSE GAMMA')){
    xi <- invgamma::rinvgamma(n = n, shape = dist$parameters[1], rate =  dist$parameters[2])
  }else if(dist$distribution %in% c('BETA')){
    xi <- rbeta(n = n, shape1 = dist$parameters[1], shape2 = dist$parameters[2])
  }else if(dist$distribution %in% c('MULT', 'MULTINOMIAL')){
    xi <- one_of(n = n, x = dist$parameters)
  }else if(dist$distribution %in% c('FIXED')){
    xi <- rep(x = dist$parameters[1], n)
  }else if(dist$distribution %in% c('LOG-NORMAL', 'LOGN', 'LOG NORMAL', 'LNORM', 'LNORMAL')){
    xi <- mc2d::rlnormb(n = n, mean = dist$parameters[1], sd = dist$parameters[2])
  }else{
    print('Distribution not recognized/supported. Distributions supported include: Pert, Normal, Binomial, Triangle, Poisson, Uniform, and Inverse-gamma. If youre interested in support for particular distributions, please contact jpgo@ucdavis.edu')
  }
  if(full == T){
    dist[['x']] <- xi
    return(dist)
  } else{
    return(xi) 
  }
}