#' Function to Sample from a multinomial distribution
#' 
#' @param x vector of categories
#' @param p vector of probabilities 
#' @param n Number of samples
#' @export
#' 

one_of <- function(x, p = NA, n = 1){
  if(is.na(p)){
    sample(x, size = n, replace = T)
  }else{
    sample(x, size = n, prob = p, replace = T)
  }
}