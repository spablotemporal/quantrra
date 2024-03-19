#' Truncate a distribution
#' @description
#' Truncates a vector of numeric variables to a specific range. Different methods of truncating the distribution are provided.
#' 'ignore' will ignore the values outside the provided range.
#' 'sub' will substitute the values outside the provided range with the min and max.
#' 'resample' will bootstrap resample the values outside the distribution from the input values
#' 
#' 
#' @param x vector of values to truncate
#' @param range range of values to truncate at
#' @param method method for the truncation. default is 'ignore', which will ignore all the values out of the range. 'sub' will substitute the values out of the range for the min and max values. 'resample' will resample the values of out range from the values inside the range. 
#' @return A vector of numeric values with the truncated values
#' @examples
#' # sample from a pert distribution
#' x <- ra_sample('pert(60, 80, 120)', n = 1000)
#' 
#' # method, ignore: ignore values out of bounds
#' ra_truncate(x = x, range = c(10, 90)) %>% 
#'  ra_plot_dist(main = 'Ignore')
#'  
#' # method substitute: substitute values out of bounds for min max provided
#' ra_truncate(x = x, range = c(10, 90), method = 'sub') %>%
#'   ra_plot_dist(main = 'Substitution')
#' # Method resample: Resample values out of bounds for values inside bounds
#' ra_truncate(x = x, range = c(10, 90), method = 'resample') %>% 
#'   ra_plot_dist(main = 'Resample')
#'  
#' @export


ra_truncate <- function(x, range, method = 'ignore'){
  if(method == 'ignore'){
    # Ignore values out of range
    x <- x[x >= range[1] & x <= range[2]]
  }else if(method == 'sub'){
    # Substitute values out of range for min and max
    x[x <= range[1]] <- range[1]
    x[x >= range[2]] <- range[2]
  }else if(method == 'resample'){
    # Resample the values out of range from range
    y <- x[x >= range[1] & x <= range[2]]
    s <- sample(1:length(y))
    x[x <= range[1]] <- y[s]
    s <- sample(1:length(y))
    x[x >= range[2]] <- y[s]
  }
  return(x)
}