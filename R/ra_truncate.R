#' Function to truncate a distribution
#' 
#' @param x vector of values to truncate
#' @param range range of values to truncate at
#' @param method method for the truncation. default is 'ignore', which will ignore all the values out of the range. 'sub' will substitute the values out of the range for the min and max values. 'resample' will resample the values of out range from the values inside the range. 
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