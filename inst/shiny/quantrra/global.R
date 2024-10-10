#' Function to Call the shinny app
#' 
#' @export

# QuantRRA: QUANTITATIVE RAPID RISK ASSESMENT 
# TO DO: ------------------
# - [ ] Add a modal box when running the model 
# - [ ] Implement additional distributions in the documentation. Triangle, inverse gamma, uniform, etc 
# - [ ] Tab for the distribution fitting
# - [ ] Bubble plot at the stratified models tab
# Completed:
# - [x] Modify all the model files for the new specification of Pert distribution (mode, min, max) -> (min, mode, max)
# - [x] Tab for the sensitivity analysis
# - [X] Tab for stratified models
#   - [X] Bar plot for the risk
#   - [X] Map (option to add shp file)

# Setup ----------
# libraries used 
library(shiny); library(shinydashboard)
library(rlang)
library(dplyr)
library(plotly)
library(visNetwork)
library(DT)
library(quantrra)
library(sf)
library(shinyWidgets)
library(writexl)
# library(rgdal)

DFs <- NULL

source('Documentation.R')
source("www/fun.R")

# init_nodes <- read.csv(unz('www/asf_products.zip', 'nodes.csv'))
init_nodes <- quantrra::asf_products$model
ra_model <- quantrra::asf_products
# init_edges <- read.csv(unz('www/M_1.zip', 'edges.csv'))

# functions ----------
# Function for undefined fields
undf <- function(x, y = NA) ifelse(is.null(x), y, x)