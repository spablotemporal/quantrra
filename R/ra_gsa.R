#' Function to perform a  global sensitivity analysis using random forest.
#' 
#' @description
#' This function is based on the publication: Global sensitivity analysis for complex ecological models: a case study of riparian cottonwood population dynamics by Harper et. al 2011.
#' This method combines Random Forest and CART to rank the most influential parameters in the main outcome and provide a graphic representation of the interaction between the parameters in the outcom.
#' 
#' @param data data set for the analysis
#' @param f formula
#' @param mainMain title for the plots
#' @param seed seed used for replication purposes
#' @param palette Color palette for the static tree
#' @param tree Type of tree. options include: 'none' no tree, 'interactive' interactive visualization with visNetwork, and 'static' static plot with rpart.plot
#' @return A list with the following elements: $VarianceExp The variance explained by the GSA, $RelImport a plot ranking the variables by their relative importance
#' @examples
#' set.seed(1)
#' 
#' # use one of the example models
#' m <- quantrra::OIRSA
#' 
#' # run the model
#' output <- ra_run(m = m$nodes, nsim = 1000)
#' 
#' # Run the GSA on the model output
#' ra_gsa(
#'   data = output,
#'   f = P ~ P1 + P2 + P3 + R1 + R2
#' )
#' 
#' @export
 
ra_gsa <- function(data, f, main = '', seed = 1, palette  ='-RdYlGn', tree = 'none'){
  # data = Mo; f = f; main = ''; seed = 1; palette  ='-RdYlGn'
  PL <- list()
  set.seed(seed)
  # Split the data train/test
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
  train <- data[ind==1,]
  test <- data[ind==2,]
  # Random Forest model fit
  rf <- randomForest::randomForest(f, data=train, proximity=TRUE) 
  PL[['VarianceExp']] <- median(rf$rsq)*100
  
  # Variable importance
  PL[['RelImport']] <- randomForest::importance(rf) %>%
    data.frame() %>% 
    mutate(variable = rownames(.), RelImpt = IncNodePurity/sum(IncNodePurity)) %>%
    ggplot(aes(x = reorder(variable, RelImpt), y = RelImpt)) +
    geom_col() +
    coord_flip() +
    labs(x = '', y = 'Relative Importance', title = main) +
    theme_minimal() +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))
  
  if(tree == 'interactive'){
    fit <- rpart::rpart(f, data=data, method = 'anova')
    PL[['RT']] <- visNetwork::visTree(fit) 
  }else if(tree == 'static'){
    fit <- rpart::rpart(f, data=data, method = 'anova')
    PL[['RT']] <- rpart.plot::rpart.plot(fit, type = 4, box.palette = palette, branch.lty = 2) 
  }
  
  return(PL)
}