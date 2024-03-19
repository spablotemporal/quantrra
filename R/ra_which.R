#' (experimental) Fit data to multiple distributions to see which one represents it best
#' 
#' @description
#' This function fits a vector of numbers to multiple distributions and provides test statistics on the fit.
#' 
#' @param x Data to fit
#' @param dist Distribution specification
#' @export

ra_which <- function(x, distributions){
  y <- lapply(dists, function(d){
    ra_fit(x, d)
  }) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    mutate(value = as.numeric(value)) %>% 
    arrange(value)
  return(y)
}