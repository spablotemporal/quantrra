#' Run a stratified model.
#' @description
#' The stratified model uses a different set of parameters for each starta with the intention to account for population heterogeneity. 
#' A strata represents different ways of grouping the target population. I.e. estimate the risk for different age groups, regions, or other groups to account for the population heterogeneity.
#' 
#' @param m Model file
#' @param tbl Table with parameters per strata
#' @param nsim Number of simulations
#' @return data frame with mean, and 95% percentiles for each of the output nodes
#' @examples
#' # use one of the example models
#' m <- quantrra::asf_products
#' ra_run_strat(m = m$nodes, tbl = m$stratified, nsim = 10e3)
#' 
#' @export

ra_run_strat <- function(m, tbl, nsim, full = F, simplify = T){
  # m = m; Tbl = ct_s; nsim = 100
  
  tbl <- data.frame(tbl) # Make sure its a dataframe (not tibble or other format)
  # Reformat the strata data:
  tbl_t <- tbl[,-1] %>% 
    t() %>%
    data.frame() %>% 
    `colnames<-`(tbl[,1]) %>% 
    mutate(id = rownames(.))
  
  # Join data
  ms <- m %>% 
    left_join(tbl_t, by = 'id')
  
  # Get strata names
  strata <- tbl[,1]
  
  if(full){
    varsOut <- c("In", "Out")
  }else{
    varsOut <- c("Out") 
  }

  # get outputs names
  o <- ms %>% 
    filter(type %in% varsOut) %>% 
    pull(id)
  
  Out <- lapply(strata, function(x){
    # x = strata[1]
    ms %>% 
      mutate(distribution = eval(parse(text = x))) %>% 
      ra_run(m = ., nsim = nsim) %>% 
      select(o)  %>%
      mutate(ids = x)
  })
  
  if(simplify){
    Out <- Out %>% 
      do.call(rbind,.) %>% 
      group_by(ids) %>% 
      summarise_at(.vars = o, .funs = c(m = ~mean(., na.rm = T), q05 = ~quantile(., 0.05), q95 = ~quantile(., 0.95))) 
  }
  
  
  return(Out)
}