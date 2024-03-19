#' Risk of ASF introduction via live products
#'
#' Model adapted from the publication by OIRSA 2020 to evaluate the risk of introduction of ASF in the OIRSA region (Mexico and Central America). 
#'
#' @format List with two data frames
#' A list with the nodes and edges to use by the quantrra library
#' \describe{
#'   \item{nodes}{This table contains the nodes form the risk assessment scenario tree and its asociated parameters. The table is used for runing the model with the function quantrra::ra_run()}
#'   \item{edges}{This table contains the information of the connections between the nodes of the risk assessment tree. The table is used for visualization purposes with the function quantrra::ra_plot_tree()}
#'   ...
#' }
#' @source {OIRSA. 2020. Análisis de riesgo sobre la probabilidad de ingreso, establecimiento y diseminación de la PPA en la porcicultura de los países de la región del OIRSA. https://www.oirsa.org/contenido/2020/AR_PPA_Edición\%20revisada\%2001_07_20.pdf}
"OIRSA"

#' Risk of ASF introduction via live products
#'
#' Model adapted from the publication by Hernandez-Ibata el. al 2017. 
#'
#' @format List with two data frames
#' A list with the nodes and edges to use by the quantrra library
#' \describe{
#'   \item{nodes}{This table contains the nodes form the risk assessment scenario tree and its asociated parameters. The table is used for runing the model with the function quantrra::ra_run()}
#'   \item{edges}{This table contains the information of the connections between the nodes of the risk assessment tree. The table is used for visualization purposes with the function quantrra::ra_plot_tree()}
#'   ...
#' }
#' @source {Herrera-Ibatá, Diana María, Beatriz Martínez-López, Darla Quijada, Kenneth Burton, and Lina Mur. 2017. “Quantitative Approach for the Risk Assessment of African Swine Fever and Classical Swine Fever Introduction into the United States Through Legal Imports of Pigs and Swine Products.” Edited by Shuo Su. PLOS ONE 12 (8): e0182850.} \doi{https://doi.org/10.1371/journal.pone.0182850}
"ASFm"

#' Risk of ASF introduction via sub products
#'
#' Model used for the publication: “Quantitative Risk Assessment of African Swine Fever Introduction into Spain by Legal Import of Swine Products.” This model evaluates the risk of introduction of African Swine Fever into Spain via legal product imports.
#'
#' @format List with three data frames
#' A list with the nodes, edges and stratified values to use by the quantrra library to perform risk assessment in R
#' \describe{
#'   \item{nodes}{This table contains the nodes form the risk assessment scenario tree and its asociated parameters. The table is used for runing the model with the function quantrra::ra_run()}
#'   \item{edges}{This table contains the information of the connections between the nodes of the risk assessment tree. The table is used for visualization purposes with the function quantrra::ra_plot_tree()}
#'   \item{stratified}{This table contains the parameters used to run a stratified analysis with the function ra_run_stratified(). Each row represents one strata and the columns represent the parameters from the model for that strata.}
#'   ...
#' }
#' @source {Muñoz-Pérez, Carolina, Beatriz Martínez-López, José Pablo Gómez-Vázquez, Cecilia Aguilar-Vega, Jaime Bosch, Satoshi Ito, Marta Martínez-Avilés, and José Manuel Sánchez-Vizcaíno. 2023. “Quantitative Risk Assessment of African Swine Fever Introduction into Spain by Legal Import of Swine Products.” Research in Veterinary Science, August, 104990.} \doi{https://doi.org/10.1016/j.rvsc.2023.104990}
"asf_products"

