# Author: Etienne CAMENEN
# Date: 2018
# Institute: ICM - Institut du Cerveau et de la Moelle epiniere (Paris, FRANCE),
# Institut Francais de Bioinformatique (IFB), Centre national de la recherche scientifique (CNRS)
# Contact: iconics@icm-institute.org
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
# EDAM topic: omics, medecine, mathematics
#
# Abstract: A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA)
# with all default settings predefined. Produce four figures to help clinicians to identify fingerprint:
# the samples and the variables projected on the two first component of the multi-block analysis, the histograms
# of the most explicative variables and the explained variance for each blocks.

library(shiny)


ui <- fluidPage(

  titlePanel("RGCCA - graphical interface"),


  sidebarLayout(

    sidebarPanel(

      useShinyjs(),

      # Data loading
      fileInput(inputId = "blocks",
                label = h5("Choose blocks : "),
                multiple = TRUE
      ),
      fileInput(inputId = "connection",
                label = h5("Choose a connection design : ")
      ),
      fileInput(inputId = "response",
                label = h5("Choose a response : ")
      ),

      # File parsing
      checkboxInput("adv_pars",
                    "Advanced parsing",
                    value = FALSE),
      conditionalPanel(
        condition = "input.adv_pars == true",
        checkboxInput(inputId = "header",
                      label = "Consider first row as header",
                      value = TRUE),
        radioButtons(inputId = "sep",
                     label = "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tabulation = "\t"),
                     selected = "\t")),

      # Analysis parameters
      checkboxInput("adv_ana",
                    "Advanced analysis",
                    value = FALSE),
      conditionalPanel(
        condition = "input.adv_ana == true",
        uiOutput("nb_comp_custom"),
        checkboxInput(inputId = "superblock",
                      label = "Use a superblock",
                      value = TRUE),
        checkboxInput(inputId = "scale",
                      label = "Scale the blocks",
                      value = TRUE),
        checkboxInput(inputId = "bias",
                      label = "Biaised estimator of the var/cov",
                      value = TRUE),
        radioButtons(inputId = "scheme",
                     label = "Scheme function",
                     choices = c(Horst = "horst",
                                 Centroid = "centroid",
                                 Factorial = "factorial"),
                     selected = "factorial"),
        radioButtons("init",
                     label = "Mode of initialization",
                     choices = c(SVD = "svd",
                                 Random = "random"),
                     selected = "svd")),

      # Graphical parameters
      checkboxInput("adv_graph",
                    "Advanced graphics",
                    value = FALSE),
      conditionalPanel(
        condition = "input.adv_graph == true",
        uiOutput("id_block_custom"),
        uiOutput("axis1_custom"),
        uiOutput("axis2_custom"),
        sliderInput(inputId = "nb_mark",
                    label = h5("Number of potential biomarkers: "),
                    min = 10, max = 500, value = 100)),

      actionButton(inputId = "save_all",
                   label = "Save all")

    ),

    mainPanel(

      tabsetPanel(
        type = "tabs",
        id = "navbar",
        tabPanel("Samples plot",
                 plotOutput("samplesPlot"),
                 actionButton("samples_save","Save")),
        tabPanel("Corcircle plot",
                 plotOutput("corcirclePlot"),
                 actionButton("corcircle_save","Save")),
        tabPanel("Fingerprint plot",
                 plotOutput("fingerprintPlot"),
                 actionButton("fingerprint_save","Save")),
        tabPanel("AVE plot",
                 plotOutput("AVEPlot"),
                 actionButton("ave_save","Save"))
      )

    )
  )
)
