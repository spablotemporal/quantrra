#' Run the stochastic model using a quantrra model table
#' @description
#' Run the stochastic model using a quantrra model table. 
#' In the model table in quantrra, each row represents a node in the scenario tree, with it's associated id, label, level of hierarchy, distribution and formula.
#' Each node requires an id which is the short name used in the formulas, and a a label which is meant to provide a more detailed description of the node.
#' The hierarchy level of the nodes will specify the order in which the nodes will be executed in the calculation of the risk; multiple nodes can be in the same hierarchy level.
#' There are two types of nodes in quantrra: inputs and outputs.
#' The input nodes are nodes that require the user specification of a probability distribution and the parameters associated.
#' Output nodes are nodes that require the user to specify a formula or R function that will be executed using other nodes of lower hierarchy.
#' A example model table is provided inside the package and can be accessed using `quantrra::OIRSA`.
#' 
#' @param m Model file
#' @param nsim Number of simulations
#' @return A data frame with the values for each of the inputs and outputs
#' @examples
#' # define a model table using one of the provided examples
#' m <- quantrra::OIRSA
#' 
#' ra_run(M = m$nodes, nsim = 10) # Run the model 
#' 
#' @export

ra_run <- function(m, nsim){
  ## Make sure a valid variables area provided
  cn <- c("id", "label", "type", "distribution", "formula")
  if (any(!cn %in% colnames(m))){
    stop("Provide valid variable names. Model file must include variables that specify the id, lable, type, distribution and formula see examples")
  }
  # Standardize the notation used for the type of node (i.e. convert input, Input, to just in) ####
  m <- m %>% 
    mutate(
      type = tolower(type),
      type = recode(type, input = "in", output = "out")
    )
  # Filter only inputs
  df <- m %>% 
    filter(type == 'in')
  # Check if at least one input was provided
  if(nrow(df) == 0){ 
    stop('At least one input variable must be provided, make sure the table includes a variable named type with either in or out')
  }
  # Sample inputs from distribution
  df <- lapply(1:nrow(df), function(x){
    x <- ra_sample(x = df[x,]$distribution, n = nsim)
    }) %>% 
    `names<-`(df$id) %>%  # assign names
    do.call(cbind, .) %>%  # add all columns
    data.frame() # format as data frame
  # Filter only outputs
  o <- M %>% 
    filter(type == 'out')
  
  # Check if at least one input was provided
  if(nrow(o) == 0){ 
    stop('At least one input variable must be provided, make sure the table includes a variable named type with either in or out')
  }
  # calculate outputs
  for (x in 1:nrow(o)) {
    df <-  df %>% 
      rowwise() %>% # Make sure operations are made per row
      mutate(!!o$id[x] := !!parse_quo(o$formula[x], env = caller_env()))
  }
  
  return(data.frame(df))
}
