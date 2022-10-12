# {QuantRRA} <img src="img/Icon.png" align="right" alt="" width="120" />

`{QuantRRA}` is a package for rapid risk assessment developed for the statistical software R.

# Installing QuantRRA

You can install `QuantRRA` development version from [GitHub](https://github.com/jpablo91/QuantRRA) using the following code in your R console (make sure you have R > 3.5):

```r
# make sure you have the package devtools installed
devtools::install_github("jpablo91/QuantRRA")
```

# Getting started

QuantRRA was developed for the implementation of stochastic probabilistic risk assessment models as an open access alternative to software such as [@Risk](https://www.palisade.com/risk/). QuantRRA provides a set of functions to be used in R, and also offers a more user friendly interface trough shiny that can be accessed using:

```r
QuantRRA::runQuantRRA()
```

<img src="img/RunQuantRRA.gif" alt="" width="450" />

## Main usage

The main usage for QuantRRA is to simulate stochastic events using the function `RRA()`, which requires two arguments:  
  
  - **M**, the model file
  - **n** number of simulations.  
  
A model file is just a data.frame with rows that represent inputs and outputs of the model; and the columns include ID, label, whether the event is input or output, a distribution (for inputs), and a formula (for outputs).  

<img src="img/ModelFile.png" alt="" width="500" />

The model file then can be used to simulate events 

```r
# Save the model file to an object
M <- QuantRRA::OIRSA_M
# Run the model 5000 times
Mo <- RRA(M = M, nsim = 5000)
# Visualize the results:
plotDist(Mo$P)
```

<img src="img/eventsOut.png" alt="" width="500" />

# Other features

QuantRRA also provides functions to evaluate the model and creating stratified models to estimate regional or strata-specific risk estimation of events.

<img src="img/Strata.png" align="right" alt="" width="450" />


<img src="img/SA.png" align="left" alt="" width="450" />


see the package [example](https://cadms-ucd.github.io/Teaching/224_Lab7.html) for more information.




