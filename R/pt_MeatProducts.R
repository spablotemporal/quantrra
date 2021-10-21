#' Function to estimate the probability of importing a case via meat products
#' 
#' @param pars List of parameters for the model
#' @export
 
pt_MeatProducts <- function(pars){
  with(pars,{
    # estimate vaccination probability
    piv <- ifelse(Vacc == 1, (1 - pv*pe), 1)
    # Get the introduction probability
    return(c(pt = p0*piv*(1 - pds)*pse*pip*psmp*pst, piv = piv))
  })
}