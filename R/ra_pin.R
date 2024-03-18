#' Estimate the probability of an animal not being detected with a diagnostic test
#' 
#' @param p0 estimated prevalence
#' @param Se Test sensitivity
#' @param Sp Test specificity
#' @export
#### pin ####
ra_pin <- function(p0, Se, Sp){
  (p0 * (1 - Se)) / (p0 * (1 - Se) + (1 - p0) * Sp)
}
