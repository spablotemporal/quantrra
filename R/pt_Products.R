#' Function to estimate the probability of importing a case via other products
#' 
#' @param pars List of parameters for the model
#' @export
######## Other products Import ##############
pt_Products <- function(pars){
  with(pars,{
    # estimate vaccination probability
    piv <- ifelse(Vacc == 1, (1 - pv*pe), 1)
    # Get the introduction probability
    return(pt = p0*piv*pse*pip*pspp*pst)
  })
}