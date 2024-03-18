#' Sample from a multinomial (discrete) distribution
#' 
#' @description
#' This function samples a value from a discrete distribution.
#' 
#' @param x vector of categories
#' @param p vector of probabilities 
#' @param n Number of samples
#' @return A vector of values sampled form the distribution specified
#' @examples
#' ra_one_of(x = c("one", "two", "3"), p = c(0.2, 0.3, 0.5), n = 5)
#' 
#' @export
#' 

ra_one_of <- function(x, p = NULL, n = 1){
  if(is.null(p)){
    sample(x, size = n, replace = T)
  }else{
    sample(x, size = n, prob = p, replace = T)
  }
}