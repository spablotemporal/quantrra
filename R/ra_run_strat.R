#' Run a stratified model.
#' @description
#' The stratified model uses a different set of parameters for each starta with the intention to account for population heterogeneity. 
#' A strata represents different ways of grouping the target population. I.e. estimate the risk for different age groups, regions, or other groups to account for the population heterogeneity.
#' 
#' @param m Model file. If model file provided as a list, must contain named elements for the 'model' and 'stratified' tables. else a data frame containing a mode table can be provided, if this is the case, the argument tbl must include a data.frame that represents the stratified table
#' @param tbl Table with parameters per strata, if model provided as named list, this is not required
#' @param nsim Number of simulations
#' @param simplify Whether or not the output will be simplified. When simplify = T, the function will return a data.frame with the median and 95 quantiles of the results. When simplify = F, the output will return a list of data.frames corresponding to each of the strata in the model
#' @return data frame with mean, and 95% percentiles for each of the output nodes
#' @examples
#' # use one of the example models
#' m <- quantrra::asf_products
#' ra_run_strat(m = m, nsim = 10e2)
#' 
#' @export

ra_run_strat <- function(m, tbl = NULL, nsim, full = F, simplify = T){
  # m = m; Tbl = ct_s; nsim = 100
  if(!is.null(tbl)){
    tbl <- data.frame(tbl) # Make sure its a dataframe (not tibble or other format) 
  }else{
    if(!("stratified" %in% names(m))){
      stop("Make sure to provide the model file in the correct format. Must be a list that includes the model table and the stratified table with specified names")
    }else{
      tbl <- m$stratified
      m <- m$model
    }
  }
  # Reformat the strata data:
  tbl_t <- tbl[,-1] %>% 
    t() %>%
    data.frame() %>% 
    `colnames<-`(tbl[,1]) %>% 
    mutate(id = rownames(.))
  
  # Join data
  ms <- m %>% 
    left_join(tbl_t, by = "id") %>% 
    mutate(type = tolower(type))
  
  # Get strata names
  strata <- tbl[,1]
  
  if(full){
    varsOut <- c("in", "input", "out", "output")
  }else{
    varsOut <- c("out", "output") 
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