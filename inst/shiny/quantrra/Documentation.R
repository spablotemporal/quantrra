# This file contains all the UI elements for the documentation including instructions, methods, among others
doc <- list(
  # Contact info --------------
  contact = list(
    "For questions and comments contact:", 
    br(), tags$ul(
      tags$li("Developer: Jose Pablo Gómez-Vázquez DVM, MPVM, PhD ", tags$a('jpgo@ucdavis.edu')),
      tags$li("PI of the project: Beatriz Martinez Lopez, DVM, MPVM, PhD:", tags$a("beamartinezlopez@ucdavis.edu")),
    )
  ),
  # Acknowledgements-----------
  ack = list(
    tags$li("Acknowledgement. “This material is based upon work supported by the U.S. Department of Homeland Security through the Cross-Border Threat Screening and Supply Chain Defense under Grant Award Number 18STCBT00001."),
    tags$li("Disclaimer. “The views and conclusions contained in this document are those of the authors and should not be interpreted as necessarily representing the official policies, either expressed or implied, of the U.S. Department of Homeland Security.”")
  ),
  # Home -----------
  home = list(
    tags$h2(tags$b("User-friendly online dashboard for the assessment of the risk of African swine fever virus introduction, exposure and potential spread into the United States"), align = "center"),
    tags$h4('This dashboard presents the models developed to evaluate the risk of introduction and potential spread of African swine virus in the United states.', align = "center"),
    br(), h4("Use the left sidebar to jump between the models and the documentation at any time.", align = "center"),
    br(), p("You can open and close the sidebar by using the button located at the upper header ", tags$img(src = "img/sidebar_bttn.png", width = "30px", height = "30px"), align = "center"),
    br(), "This dashboard was developed with the intention to explore the application of agent based models and risk assesment to evaluate the targeted surveillance and emergency response agaisnt African Swine Fever in the United States. "
  ),
  # ABM documentation --------------------
  abm = list(
    h2('Disease spread model'),
    hr(),
    box(
      width = 12,
      column(
        width = 6,
        "To evaluate the potential disease spread of an ASF introduction into the United States, we developed a agent-based stochastic simulation model that recreates the transmission dynamics on both a local and national scale. 
  To achieve this, we incorporated a hexagonal grid of 10 km radius as the basic unit of analysis and aggregated demographic characteristics of the population at risk within each cell. 
  This approach allows for the incorporation of population heterogeneity and the effects of local interventions, such as culling and restriction of movements, within a 10 km radius.",
        br(), "To model the transmission dynamics of the disease, we used a set of parameters that were sampled from defined probability distributions for each simulation run. This approach enables the estimation of the expected behavior of the disease spread, and allows for the analysis of various scenarios.\
The model operates in three stages per time step, including: (I) updating the disease status for all agents, (II) local disease spread, and (III) long-distance spread. 
  The progression of the disease through these stages is determined by a set of rules and functions that are based on biologically and epidemiologically-informed assumptions."
      ), 
      column(
        width = 6, 
        img(src = 'img/ModelFramework.png', width = '90%', align = 'center')
      )
    ), 
    
    br(),
    br(),
    hr(),
    actionBttn(
      inputId = "GoSpread", label = "Go to the Disease spread Model",
      color = "success", size = "lg"
    )
  ),
  # Disease intro ------------------
  intro = list(
    h4("Disease introduction"),
    "This section integrates the tools developed to build and run a disease introduction model. The disease introduction model is based on a stochastic risk assessment modelling approach, where the user defines a set of events with associated probabilities.", br(),
    "The Disease introduction section consist on two subsections:",
    tags$ol(
      tags$li("Model building and evaluation"),
      tags$li("Library of models")
    ), br(),
    "More in depth documentation and training materials can be found in the ", tags$a(href = "http://github.com/spablotemporal/quantrra", "Project repository"), " and the ", tags$a(href = "https://www.spablo-temporal.network/ra-workshop/", "workshop homepage"), br()
  ),
  # ra instructions -----------
  ra_instructions = list(
    "This module integrates stochastic risk modeling to evaluate the likelihood of an introduction of African Swine Fever into the country. An example model is already loaded into the platform. More examples can be found in the model library", br(),
    "The basic structure of a model is a .xlsx file with different tables arranged into sheets. Each sheet represent a component of the model:",
    tags$li("model (required): Describes the events associated with the probability estimation "),
    tags$li("par (optional): Describes different parametrization options of the model"),
    tags$li("stratified (optional): Describes different parameterizations to run a stratified model."),
    "You can see the model library to explore different implementations in the development of risk assessment models with this tool."
  ),
  # quantrra -------------
  quantrra = list(
    tabItem(
      tabName = 't2',
      h1('Documentation'),
      'This application was developed with the intention to provide a framework for a transparent, accesible and intuitive methodology for risk assesment. ',
      'We provide some examples for different models that can be adapted to specific scenarios. In this documentation we explain how to load a pre-existing model file, modify it, and how to creat a new mode from scratch. ',
      br(),
      'The main tab consist on 3 sections: Model table, Model tree and results',
      tags$h2('Model table'),
      'This table contains the information on how the nodes interact with each other to calculate the risk of the event. The model table can be edited inside this shiny app, you can modify the nodes names, distribution or formulas. The columns included in the table are:',
      tags$li('id. A unique identifyier for the node. Try to use short names without spaces or special characters for this variables, and do not repeat the same id for different nodes.'),
      tags$li('label. The name of the node. THis label can contain spaces or special characters. The purpose of this variable is to be a human friendly readable identifyier for the nodes'),
      tags$li('type. There are two types of nodes, In and Out. In nodes are user defined inputs that use a probability distribution to describe the probability of the event happening. 
  Out nodes are calculated by the model there can be multiple outputs and the outputs can be calculated using other outputs. '),
      tags$li('level. The hierarchy level of the node, the hierarchy order of the nodes is very important to tell the model the order of how the output nodes will be calculated. For example, if an output node O1 is dependent of another output node O2, the dependent node O2 needs to be a lower hierarchy than O1 in order to be calculated'),
      tags$li('distribution. The distributions of the input nodes, current distributions supported includes: normal, binomial, poisson and pert. Only defined for In nodes'),
      tags$li('formula. Formula used to calculate the node. Needs to be defined only for Out nodes'),
      br(),
      tags$h3('Distributions supported'),
      'To specify the distribution you must write the name of the distribution followed by the parameters. Currently there are 4 probability distributions supported.',
      tags$h4('Normal'),
      'Normal distribution has two parameters: mean and standard deviation. For example, if we want to specify a normal distribution with mean = 20 and standard deviation = 1.5, we would write ',
      tags$em('Normal(20, 1.5) '), 'in the column for the given node. For more information of the normal distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Normal_distribution'),
      tags$h4('Pert'),
      'The Pert distribution has three parameters: min,mode, and max. For example, if we want to specify a pert distribution with a mode of 20, a min of 10 and a max of 35, we would write ',
      tags$em('Pert(10, 20, 35) '), 'in the column for the given node. For more information of the pert distribution visit: ', tags$a('https://en.wikipedia.org/wiki/PERT_distribution'),
      tags$h4('Binomial'),
      'The Binomial distribution has two parameters: number of trials (n) and probability of success (p). For example, if we want to specify a binomial distribution with a n of 8, a p of 0.2, we would write ',
      tags$em('Binomial(8, 0.2) '), 'in the column for the given node. For more information of the binomial distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Binomial_distribution'),
      tags$h4('Poisson'),
      'The Poisson distribution has one parameters: lambda, which represents both the mean and standard deviation. For example, if we want to specify a Poisson distribution with a lambda of 10, we would write ',
      tags$em('Poisson(10) '), 'in the column for the given node. For more information of the poisson distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Poisson_distribution'),
      tags$h2('Model tree'),
      'This section provide a graphical representation of the current model. You can edit the model and add new nodes in this section. To add a new node click on the edit button,
            this will change the interface to edit mode with two options: add Node and Add Edge. To exit the edit mode, click the x on the top right corner.',
      tags$h2('Loading a model file'),
      'When the app is initialized, a basic model file is already pre-loaded, you cans tart editing the provided model, or you can use another model from the examples. You can find some pre-existing model files in the ', 
      tags$em('Examples '), 'tab of this app. For more information of the example model files, you can go to the reference in the link provided',
      br(),
      # tags$img(src = 'G1.GIF', align = 'center'),
      br(),
      'Once you have downloaded, you can go back to the main tab and load the .zip file. ',
      br(),
      # tags$img(src = 'G2.GIF', align = 'center'),
      br(),
      'For questions, contact: ', tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu'))
  )
)

# Documentation ----------------
Documentation <- tabItem(
  tabName = 't2',
  h1('Documentation'),
  'This application was developed with the intention to provide a framework for a transparent, accesible and intuitive methodology for risk assesment. ',
  'We provide some examples for different models that can be adapted to specific scenarios. In this documentation we explain how to load a pre-existing model file, modify it, and how to creat a new mode from scratch. ',
  br(),
  'The main tab consist on 3 sections: Model table, Model tree and results',
  tags$h2('Model table'),
  'This table contains the information on how the nodes interact with each other to calculate the risk of the event. The model table can be edited inside this shiny app, you can modify the nodes names, distribution or formulas. The columns included in the table are:',
  tags$li('id. A unique identifyier for the node. Try to use short names without spaces or special characters for this variables, and do not repeat the same id for different nodes.'),
  tags$li('label. The name of the node. THis label can contain spaces or special characters. The purpose of this variable is to be a human friendly readable identifyier for the nodes'),
  tags$li('type. There are two types of nodes, In and Out. In nodes are user defined inputs that use a probability distribution to describe the probability of the event happening. 
  Out nodes are calculated by the model there can be multiple outputs and the outputs can be calculated using other outputs. '),
  tags$li('level. The hierarchy level of the node, the hierarchy order of the nodes is very important to tell the model the order of how the output nodes will be calculated. For example, if an output node O1 is dependent of another output node O2, the dependent node O2 needs to be a lower hierarchy than O1 in order to be calculated'),
  tags$li('distribution. The distributions of the input nodes, current distributions supported includes: normal, binomial, poisson and pert. Only defined for In nodes'),
  tags$li('formula. Formula used to calculate the node. Needs to be defined only for Out nodes'),
  br(),
  tags$h3('Distributions supported'),
  'To specify the distribution you must write the name of the distribution followed by the parameters. Currently there are 4 probability distributions supported.',
  tags$h4('Normal'),
  'Normal distribution has two parameters: mean and standard deviation. For example, if we want to specify a normal distribution with mean = 20 and standard deviation = 1.5, we would write ',
          tags$em('Normal(20, 1.5) '), 'in the column for the given node. For more information of the normal distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Normal_distribution'),
          tags$h4('Pert'),
          'The Pert distribution has three parameters: min,mode, and max. For example, if we want to specify a pert distribution with a mode of 20, a min of 10 and a max of 35, we would write ',
          tags$em('Pert(10, 20, 35) '), 'in the column for the given node. For more information of the pert distribution visit: ', tags$a('https://en.wikipedia.org/wiki/PERT_distribution'),
          tags$h4('Binomial'),
          'The Binomial distribution has two parameters: number of trials (n) and probability of success (p). For example, if we want to specify a binomial distribution with a n of 8, a p of 0.2, we would write ',
          tags$em('Binomial(8, 0.2) '), 'in the column for the given node. For more information of the binomial distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Binomial_distribution'),
          tags$h4('Poisson'),
          'The Poisson distribution has one parameters: lambda, which represents both the mean and standard deviation. For example, if we want to specify a Poisson distribution with a lambda of 10, we would write ',
          tags$em('Poisson(10) '), 'in the column for the given node. For more information of the poisson distribution visit: ', tags$a('https://en.wikipedia.org/wiki/Poisson_distribution'),
          tags$h2('Model tree'),
          'This section provide a graphical representation of the current model. You can edit the model and add new nodes in this section. To add a new node click on the edit button,
            this will change the interface to edit mode with two options: add Node and Add Edge. To exit the edit mode, click the x on the top right corner.',
          tags$h2('Loading a model file'),
          'When the app is initialized, a basic model file is already pre-loaded, you cans tart editing the provided model, or you can use another model from the examples. You can find some pre-existing model files in the ', 
          tags$em('Examples '), 'tab of this app. For more information of the example model files, you can go to the reference in the link provided',
          br(),
          # tags$img(src = 'G1.GIF', align = 'center'),
          br(),
          'Once you have downloaded, you can go back to the main tab and load the .zip file. ',
          br(),
          # tags$img(src = 'G2.GIF', align = 'center'),
          br(),
          'For questions, contact: ', tags$a("Jose Pablo Gomez", href = 'mailto:jpgo@ucdavis.edu'))


# ------------ | Examples | ----------------
Examples <- tabItem(
  tabName = 't3',
  tags$h1('Examples'),
  'In this tab you will find a library of example model files, the idea is that as more models are available this library will get populated with a diversity of model files that can be used and adapted for other diseases/regions',
  br(),
  tags$h2('PPA OIRSA'),
  'This model was developed by the Organismo Internacional Regional de Sanidad Agropecuaria (OIRSA) to estimate the probability of introduction of african swine fever into the countries from the OIRSA region', 
  br(),
  tags$a('Reference', href = 'https://www.oirsa.org/contenido/2020/AR_PPA_Edición%20revisada%2001_07_20.pdf'),
  hr(),
  downloadButton("downloadOIRSA", "Download"),
  br(),
  tags$h2('ASF Legal imports'),
  'Model used for the publication: “Quantitative Risk Assessment of African Swine Fever Introduction into Spain by Legal Import of Swine Products.” This model evaluates the risk of introduction of African Swine Fever into Spain via legal product imports.', 
  br(),
  tags$a('Reference', href = 'https://doi.org/10.1016/j.rvsc.2023.104990'),
  hr(),
  downloadButton("downloadASFP", "Download"),
  # br(),
  # tags$h2('Pleuromutilins in Denmark'),
  # 'This model was developed by Alban et. al. to evaluate the Risk to Public Health due to Use of Antimicrobials in Pigs', 
  # br(),
  # tags$a('Reference', href = 'https://www.oirsa.org/contenido/2020/AR_PPA_Edición%20revisada%2001_07_20.pdf'),
  # hr(),
  # downloadButton("downloadLis", "Download"),
  # tags$a('Download model', href ='https://www.oirsa.org/contenido/2020/AR_PPA_Edición%20revisada%2001_07_20.pdf'),
  br(),
  # tags$h2('OIE'),
  # 'Description', 
  # br(),
  # tags$a('Reference', href = 'https://rr-africa.oie.int/wp-content/uploads/2018/03/handbook_on_import_risk_analysis_-_oie_-_vol__i.pdf'),
  # downloadButton("downloadM2", "Download")
  )
