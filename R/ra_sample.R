#' Sample from different distributions 
#' 
#' @description
#' This function samples random values from probability distributions. 
#' The function, conveniently, wraps other functions to provide multiple distributions under a single function.
#' Some of the distributions currently supported includes: normal, poisson, triangular, uniform, gamma, inverse gamma, log normal, pert.
#' 
#' @param x Distribution specification, must specify the name of the distribution and parameters, e.g. Normal (10, 0.4) .Current distributions supported include: uniform, binomial, triangle, poisson, normal and pert. If no distribution specified. the value will be assumed to be fixed.
#' @param n Number of observations to be sampled from the distribution
#' @param full TRUE/FALSE. If TRUE a list with the samples, and distribution specified will be returned, Only a vector of the observations will be returned otherwise.
#' @return A numeric vector containing values sampled from the distribution specified.
#' @export
#' @examples
#' ra_sample(x = 'pert(0.01,  0.03, 0.64)', n = 10)

ra_sample <- function(x, n, full = F){
  if(rlang::is_string(x)){ # If input is a string, convert to list
    dist <- list()
    # identify distribution (first element of the list)
    dist[['distribution']] <- sub(pattern = '\\(.*', replacement = '', x = x) %>% toupper()
    # Extract parameters
    dist[['parameters']] <- sub(".*(?:\\((.*)\\)).*|.*", "\\1",x) %>% 
      stringr::str_split(string = ., pattern = ',') %>% unlist() %>% as.numeric() 
  }else{ # else, assume is a fixed value
    dist <- list()
    dist[["distribution"]] <- "FIXED"
    dist[["parameters"]] <- x
  }
  # Sample from distribution
  xi <- switch (dist$distribution,
                PERT = {mc2d::rpert(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3])},
                BINOM = ,BINOMIAL = {rbinom(n = n, size = dist$parameters[1], prob = dist$parameters[2])},
                TRI =, TRIANGLE =, TRIANG = { mc2d::rtriang(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3])},
                POI =, POISSON = {rpois(n = n, lambda = dist$parameters[1])},
                NORMAL =, N =, GAUSS =, GAUSSIAN = {rnorm(n = n, mean = dist$parameters[1], sd = dist$parameters[2])},
                UNIFORM =, UNIF =, U = {runif(n = n, min = dist$parameters[1], max = dist$parameters[2])},
                GAMMA = rgamma(n = n, shape = dist$parameters[1], scale = dist$parameters[2]),
                `INVERSE-GAMMA` =, `INV-GAMMA` =, `INVERSE GAMMA` = {invgamma::rinvgamma(n = n, shape = dist$parameters[1], rate =  dist$parameters[2])},
                BETA = {rbeta(n = n, shape1 = dist$parameters[1], shape2 = dist$parameters[2])},
                MULT =, MULTINOMIAL = ra_one_of(n = n, x = dist$parameters), 
                FIXED = {rep(x = dist$parameters[1], n)},
                `LOG_NORMAL` =, LOGN =, LNORM =, LNORMAL = {mc2d::rlnormb(n = n, mean = dist$parameters[1], sd = dist$parameters[2])},
                "Error: Distribution not recognized/supported. Distributions supported include: Pert, Normal, Binomial, Triangle, Poisson, Uniform, and Inverse-gamma. If youre interested in support for particular distributions, please contact jpgo@ucdavis.edu"
  )
  if(full == T){
    dist[['x']] <- xi
    return(dist)
  } else{
    return(xi) 
  }
}
