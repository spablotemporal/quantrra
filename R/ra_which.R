#' Function to sample from different distributions 
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