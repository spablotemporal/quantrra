#' Function to calculate the stratified risk
#' 
#' @param M Model file
#' @param Tbl Table with parameters per strata
#' @param nsim Number of simulations
#' @export

# Outputs Table of mean, median and CIs
RRA_s <- function(M, Tbl, nsim){
  # M = init_nodes; Tbl = st; nsim = 100
  # Reformat the strata data:
  Tbl_t <- Tbl[,-1] %>% 
    t() %>%
    data.frame() %>% 
    `colnames<-`(Tbl[,1]) %>% 
    mutate(id = rownames(.))
  
  # Join data
  Ms <- M %>% 
    left_join(Tbl_t, by = 'id')
  
  # Get strata names
  strata <- Tbl[,1]
  # get outputs names
  o <- Ms %>% 
    filter(type == 'Out') %>% 
    pull(id)
  
  Out <- lapply(strata, function(x){
    Ms %>% 
      mutate(distribution = eval(parse(text = x))) %>% 
      RRA(M = ., nsim = nsim) %>% 
      select(o)  %>%
      mutate(IDs = x)
  }) %>% 
    do.call(rbind,.) %>% 
    group_by(IDs) %>% 
    summarise_at(.vars = o, .funs = c(m = ~median(., na.rm = T), q05 = ~quantile(., 0.5), q95 = ~quantile(., 0.95))) 
  
  return(Out)
}