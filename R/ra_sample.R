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

# TODO: Add an option for a 4th value on the pert distribution for the shape of dist

ra_sample <- function(x, n, full = F){
  dist <- list()
  # identify distribution (first element of the list)
  dist[['distribution']] <- sub(pattern = '\\(.*', replacement = '', x = x) %>% tolower()
  # Extract parameters
  dist[['parameters']] <- sub(".*(?:\\((.*)\\)).*|.*", "\\1",x) %>% 
    stringr::str_split(string = ., pattern = ',') %>% unlist() %>% as.numeric()
  # If no distribution is specified, interpret as a fixed value
  if(!is.na(suppressWarnings(as.numeric(dist$distribution)))){ 
    dist <- list()
    dist[["distribution"]] <- "fixed"
    dist[["parameters"]] <- as.numeric(x)
  }
  # Sample from distribution
  xi <- switch (dist$distribution,
                pert = {mc2d::rpert(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3], shape = ifelse(is.na(dist$parameters[4]), 4, dist$parameters[4]))},
                binom = ,binomial = {rbinom(n = n, size = dist$parameters[1], prob = dist$parameters[2])},
                tri =, triangle =, triang = { mc2d::rtriang(n = n, min = dist$parameters[1], mode = dist$parameters[2], max = dist$parameters[3])},
                poi =, poisson = {rpois(n = n, lambda = dist$parameters[1])},
                normal =, n =, gauss =, gaussian = {rnorm(n = n, mean = dist$parameters[1], sd = dist$parameters[2])},
                uniform =, unif =, u = {runif(n = n, min = dist$parameters[1], max = dist$parameters[2])},
                gamma = rgamma(n = n, shape = dist$parameters[1], scale = dist$parameters[2]),
                `inverse-gamma` =, `inv-gamma` =, `invgamma` =, `inverse gamma` = {invgamma::rinvgamma(n = n, shape = dist$parameters[1], rate =  dist$parameters[2])},
                beta = {rbeta(n = n, shape1 = dist$parameters[1], shape2 = dist$parameters[2])},
                mult =, multinomial = ra_one_of(n = n, x = dist$parameters), 
                fixed = {rep(x = dist$parameters[1], n)},
                `log normal` =, logn =, lnorm =, lnormal = {mc2d::rlnormb(n = n, mean = dist$parameters[1], sd = dist$parameters[2])},
                stop("Distribution not recognized/supported. Distributions supported include: Pert, Normal, Binomial, Triangle, Poisson, Uniform, and Inverse-gamma. If youre interested in support for particular distributions, please contact jpgo@ucdavis.edu")
  )
  if(full == T){
    dist[['x']] <- xi
    return(dist)
  } else{
    return(xi) 
  }
}
