#' Function to run the stochastic probabilistic model using a QuantRRA table
#' 
#' @param M Model file
#' @param nsim Number of simulations
#' @export


ra_run <- function(M, nsim){
  # Filter only inputs
  df <- M %>% 
    filter(type == 'In')
  if(nrow(df) == 0){ # Check if at least one input was provided
    print('ERROR: At least one input must be provided')
  }else{
    # Sample inputs from distribution
    df <- lapply(1:nrow(df), function(x){
      x <- ra_sample(x = df[x,]$distribution, n = nsim)
    }) %>% 
      `names<-`(df$id) %>%  # assign names
      do.call(cbind, .) %>%  # add all columns
      data.frame() # format as data frame
    # Filter only outputs
    o <- M %>% 
      filter(type == 'Out')
    if(nrow(o) > 0){ # If at least 1 output provided calculate
      # calculate outputs
      for (x in 1:nrow(o)) {
        df <-  df %>% 
          rowwise() %>% # Make sure operations are made per row
          mutate(!!o$id[x] := !!parse_quo(o$formula[x], env = caller_env()))
      }
    }
  }
  return(data.frame(df))
}
