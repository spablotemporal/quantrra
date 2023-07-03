#' Function to sample from different distributions 
#' 
#' @param x Data to fit
#' @param dist Distribution specification
#' @export

whichDist <- function(x, distributions){
  y <- lapply(dists, function(d){
    FitDist(x, d)
  }) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    mutate(value = as.numeric(value)) %>% 
    arrange(value)
  return(y)
}