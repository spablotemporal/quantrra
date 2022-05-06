#' Function to calculate the risk
#' 
#' @param M Model file
#' @param nsim Number of simulations
#' @export


RRA <- function(M, nsim){
  # Filter only inputs
  df <- M %>% 
    filter(type == 'In')
  # Sample inputs from distribution
  df <- lapply(1:nrow(df), function(x){
    x <- SampleDist(x = df[x,]$distribution, n = nsim)
  }) %>% 
    `names<-`(df$id) %>%  # assign names
    do.call(cbind, .) %>%  # add all columns
    data.frame() # format as data frame
  # Filter only outputs
  o <- M %>% 
    filter(type == 'Out')
  # calculate outputs
  for (x in 1:nrow(o)) {
    df <-  df %>% 
      mutate(!!o$id[x] := !!parse_quo(o$formula[x], env = caller_env()))
  }
  return(df)
}