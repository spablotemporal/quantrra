#' Function to Call the shinny app
#' 
#' @export

# QuantRRA: QUANTITATIVE RAPID RISK ASSESMENT 
# TO DO: ------------------
# - [x] Modify all the model files for the new specification of Pert distribution (mode, min, max) -> (min, mode, max)
# - [ ] Implement additional distributions in the documentation. Triangle, inverse gamma, uniform, etc 
# - [x] Tab for the sensitivity analysis
# - [ ] Tab for the distribution fitting
# - [ ] Tab for stratified models
#   - [ ] Bar plot for the risk
#   - [ ] Bubble plot
#   - [ ] Map (option to add shp file)

# Setup ----------
# libraries used 
library(shiny); library(shinydashboard)
library(rlang)
library(dplyr)
library(plotly)
library(visNetwork)
library(DT)
library(QuantRRA)
# library(rgdal)

source('Documentation.R')
init_nodes <- read.csv(unz('www/M_1.zip', 'nodes.csv'))
init_edges <- read.csv(unz('www/M_1.zip', 'edges.csv'))

# functions ----------
# Function for undefined fields
undf <- function(x, y = NA) ifelse(is.null(x), y, x)