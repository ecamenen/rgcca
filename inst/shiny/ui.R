# Author: Etienne CAMENEN
# Date: 2018
# Contact: arthur.tenenhaus@l2s.centralesupelec.fr
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
#
# Abstract: A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA)
# with all default settings predefined. Produce four figures to help clinicians to identify fingerprint:
# the samples and the variables projected on the two first component of the multi-block analysis, the histograms
# of the most explicative variables and the explained variance for each blocks.

rm(list=ls())
# Libraries loading
librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx", "shiny", "shinyjs", "plotly", "visNetwork", "igraph", "ggrepel", "parallel")
for (l in librairies) {
  if (!(l %in% installed.packages()[, "Package"]))
    install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}

ui <- fluidPage(

  titlePanel("R/SGCCA - Shiny graphical interface"),

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
        checkboxInput(inputId = "tau_opt",
                      label = "Use an optimal tau",
                      value = TRUE),
        conditionalPanel(
          condition = "input.tau_opt == false",
          sliderInput(inputId = "tau",
                      label = h5("Tau: "),
                      min = 0, max = 1, value = .5, step = .1)),
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
        sliderInput(inputId = "boot",
                    label = h5("Number of boostrap : "),
                    min = 5, max = 100, value = 10, step = 5),
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
        uiOutput("blocks_names_custom"),
        uiOutput("axis1_custom"),
        uiOutput("axis2_custom"),
        uiOutput("nb_mark_custom")),

      actionButton(inputId = "save_all",
                   label = "Save all")

    ),

    mainPanel(

      tabsetPanel(
        type = "tabs",
        id = "navbar",
        tabPanel("Samples",
                 plotOutput("samplesPlot"),
                 actionButton("samples_save","Save")),
        tabPanel("Corcircle",
                 plotOutput("corcirclePlot"),
                 actionButton("corcircle_save","Save")),
        tabPanel("Fingerprint",
                 plotOutput("fingerprintPlot"),
                 actionButton("fingerprint_save","Save")),
        tabPanel("AVE",
                 plotOutput("AVEPlot"),
                 actionButton("ave_save","Save")),
        tabPanel("Bootstrap",
                 plotOutput("bootstrapPlot"),
                 actionButton("bootstrap_save","Save")),
        tabPanel("Connection",
                 plotOutput("connectionPlot"),
                 actionButton("connection_save","Save"))
      )

    )
  )
)
