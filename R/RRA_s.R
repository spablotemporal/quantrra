#' Function to calculate the stratified risk
#' 
#' @param M Model file
#' @param Tbl Table with parameters per strata
#' @param nsim Number of simulations
#' @export

# Outputs Table of mean, median and CIs
rra_s <- function(M, Tbl, nsim, full = F, simplify = T){
  # M = M; Tbl = ct_s; nsim = 100
  Tbl <- data.frame(Tbl) # Make sure its a dataframe (not tibble or other format)
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
  
  if(full){
    varsOut <- c("In", "Out")
  }else{
    varsOut <- c("Out") 
  }

  # get outputs names
  o <- Ms %>% 
    filter(type %in% varsOut) %>% 
    pull(id)
  
  Out <- lapply(strata, function(x){
    # x = strata[1]
    Ms %>% 
      mutate(distribution = eval(parse(text = x))) %>% 
      rra(M = ., nsim = nsim) %>% 
      select(o)  %>%
      mutate(IDs = x)
  })
  if(simplify){
    Out <- Out %>% 
      do.call(rbind,.) %>% 
      group_by(IDs) %>% 
      summarise_at(.vars = o, .funs = c(m = ~mean(., na.rm = T), q05 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))) 
  }
  
  
  return(Out)
}