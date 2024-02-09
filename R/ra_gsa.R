#' Function to do the global sensitivity analysis using random forest.
#' 
#' @param data data set for the analysis
#' @param f formula
#' @param mainMain title for the plots
#' @param seed seed used for replication purposes
#' @param palette Color palette for the static tree
#' @param tree Type of tree. options include: 'none' no tree, 'interactive' interactive visualization with visNetwork, and 'static' static plot with rpart.plot
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