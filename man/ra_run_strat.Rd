% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ra_run_strat.R
\name{ra_run_strat}
\alias{ra_run_strat}
\title{Run a stratified model.}
\usage{
ra_run_strat(m, tbl = NULL, nsim, full = F, simplify = T)
}
\arguments{
\item{m}{Model file. If model file provided as a list, must contain named elements for the 'model' and 'stratified' tables. else a data frame containing a mode table can be provided, if this is the case, the argument tbl must include a data.frame that represents the stratified table}

\item{tbl}{Table with parameters per strata, if model provided as named list, this is not required}

\item{nsim}{Number of simulations}

\item{simplify}{Whether or not the output will be simplified. When simplify = T, the function will return a data.frame with the median and 95 quantiles of the results. When simplify = F, the output will return a list of data.frames corresponding to each of the strata in the model}
}
\value{
data frame with mean, and 95% percentiles for each of the output nodes
}
\description{
The stratified model uses a different set of parameters for each starta with the intention to account for population heterogeneity. 
A strata represents different ways of grouping the target population. I.e. estimate the risk for different age groups, regions, or other groups to account for the population heterogeneity.
}
\examples{
# use one of the example models
m <- quantrra::asf_products
ra_run_strat(m = m$nodes, tbl = m$stratified, nsim = 10e3)

}
