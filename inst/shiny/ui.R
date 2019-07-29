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
options(shiny.maxRequestSize = 30*1024^2)

setInfo <- function(., text){
  shinyInput_label_embed(
    icon("question") %>%
      bs_embed_tooltip(title = text)
  )
}

# Global variables
one_block <<- c(`Principal Component Analysis` = "PCA")
two_blocks <<- c(`Canonical Correlation Analysis` = 'CCA', `Interbattery Factor Analysis` = "IFA", `Partial Least Squares Regression` = 'PLS',  `Redundancy analysis` = 'RA')
multiple_blocks  <<- c(`Regularized Generalized CCA (RGCCA)` = 'RGCCA', `Sparse Generalized CCA (SGCCA)` = 'SGCCA', `SUM of CORrelations method` = 'SUMCOR', `Sum of SQuared CORrelations method` = 'SSQCOR',
                      `Sum of ABSolute value CORrelations method` = 'SABSCOR',`SUM of COVariances method` = 'SUMCOV',`Sum of SQuared COVariances method` = 'SSQCOV',
                      `Sum of ABSolute value COVariances method` = 'SABSCOV', `MAXBET` = 'MAXBET', `MAXBETB` = 'MAXBET-B')
multiple_blocks_super  <<- c(`Generalized CCA (GCCA)` = 'GCCA', `Hierarchical PCA` = 'HPCA', `Multiple Factor Analysis` = 'MFA')
analyse_methods  <<- list(one_block, two_blocks, multiple_blocks, multiple_blocks_super)
reac_var  <<- reactiveVal()
id_block_y <<- id_block <<- id_block_resp <<- analysis <<- boot <<- analysis_type <<- NULL
clickSep <<- FALSE
if_text <<- TRUE
comp_x <<- 1
nb_comp <<- comp_y <<- 2
nb_mark <<- 100

# Load functions
source("../../R/parsing.R")
source("../../R/plot.R")
source("../../R/select.type.R")
source("../../R/network.R")

# maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar,
# rcon-pca, ridge-gca, , ssqcov-1, ssqcov-2, , sum-pca, sumcov-1, sumcov-2

loadLibraries(c("RGCCA", "ggplot2", "scales", "plotly", "visNetwork", "devtools", "igraph", "shiny", "shinyjs", "bsplus"))

# ("bsplus")
# if(l == "bsplus")
#   devtools::install_github("ijlyttle/bsplus", upgrade = "never")

ui <- fluidPage(

  bs_modal(
    id = "modal_superblock",
    title = "Help on superblock",
    body =  "If ticked, a superblock is introduced. This superblock is defined as a concatenation of all the other blocks.
     The space spanned by global components is viewed as a compromise space that integrated all the modalities
     and facilitates the visualization of the results and their interpretation.
     If unchecked, a connection file could be used. Otherwise, all blocks are assumed to be connected.",
    size = "medium"
  ),

  bs_modal(
    id = "modal_scheme",
    title = "Help on scheme functions",
    body="Link (i.e. scheme) function for covariance maximization is calculated with: the identity function (horst scheme),
    the absolute values (centroid scheme), the squared values (factorial scheme). Only, the horst scheme penalizes structural
    negative correlation. The factorial scheme discriminates more strongly the blocks than the centroid one.",
    size = "medium"
  ),

  titlePanel("R/SGCCA - The Shiny graphical interface"),
  tags$div(
    tags$strong("Authors: "),
    tags$p("Etienne CAMENEN, Ivan MOSZER, Arthur TENENHAUS (", tags$a(href="arthur.tenenhaus@l2s.centralesupelec.fr","arthur.tenenhaus@l2s.centralesupelec.fr"),")")
  ),
  tags$a(href="https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/blob/release/3.0/inst/shiny/tutorialShiny.md", "Go to the tutorial"),
  useShinyjs(),
  sidebarLayout(

    sidebarPanel(
      tabsetPanel(id = "tabset",
                  tabPanel("Data",

                           # Data loading
                           fileInput(inputId = "blocks",
                                     label = "Blocks",
                                     multiple = TRUE)
                           %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(title = "One or multiple CSV files containing a matrix with : (i) quantitative values only (decimal should be separated by '.'), (ii) the samples in lines (should be labelled in the 1rst column) and (iii) variables in columns (should have a header)",
                                                  placement = "bottom")
                             ),

                           radioButtons(inputId = "sep",
                                        label = "Column separator",
                                        choices = c(Comma = ",",
                                                    Semicolon = ";",
                                                    Tabulation = "\t"),
                                        selected = "\t")
                           %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(title = "Character used to separate the column in the dataset")
                             ),

                           checkboxInput(inputId = "header",
                                         label = "Consider first row as header",
                                         value = TRUE)
                  ),


                  # Analysis parameters

                  tabPanel("RGCCA",
                           uiOutput("analysis_type_custom"),
                           uiOutput("nb_comp_custom"),

                           checkboxInput(inputId = "scale",
                                         label = "Scale the blocks",
                                         value = TRUE)
                           %>%
                           shinyInput_label_embed(
                             icon("question") %>%
                               bs_embed_tooltip(title = "A data centering step is always performed. If ticked, each block is normalised and divided by the square root of its number of variables.")
                           ),

                           radioButtons("init",
                                        label = "Mode of initialization",
                                        choices = c(SVD = "svd",
                                                    Random = "random"),
                                        selected = "svd"),

                           checkboxInput(inputId = "superblock",
                                         label = "Use a superblock",
                                         value = T)
                           %>%
                             shinyInput_label_embed(
                               shiny_iconlink(name = "question-circle") %>%
                                 bs_attach_modal(id_modal = "modal_superblock")
                           ),

                           checkboxInput(inputId = "supervised",
                                         label = "Supervised analysis",
                                         value = F),

                           conditionalPanel(
                             condition = "input.supervised || input.analysis_type == 'RA'",
                             uiOutput("blocks_names_response")
                           ),

                           uiOutput("connection_custom"),

                           checkboxInput(inputId = "tau_opt",
                                         label = "Use an optimal tau",
                                         value = TRUE)
                           %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_embed_tooltip(title = "A tau near 0 maximize the the correlation whereas a tau near 1 maximize the covariance")
                             ),
                           uiOutput("tau_custom"),

                           radioButtons(inputId = "scheme",
                                        label = "Scheme function",
                                        choices = c(Horst = "horst",
                                                    Centroid = "centroid",
                                                    Factorial = "factorial"),
                                        selected = "factorial")
                           %>%
                             shinyInput_label_embed(
                               icon("question") %>%
                                 bs_attach_modal(id_modal = "modal_scheme")
                               ),

                           sliderInput(inputId = "boot",
                                       label = "Number of boostraps",
                                       min = 5, max = 100, value = 10, step = 5),
                           actionButton(inputId = "run_boot",
                                        label = "Run bootstrap"),
                           actionButton(inputId = "run_analysis",
                                        label = "Run Analysis")
                  ),

                  # Graphical parameters

                  tabPanel("Graphic",
                           checkboxInput(inputId = "text",
                                         label = "Display names",
                                         value = TRUE),
                           uiOutput("blocks_names_custom_x"),
                           uiOutput("blocks_names_custom_y"),
                           uiOutput("comp_x_custom"),
                           uiOutput("comp_y_custom"),
                           uiOutput("nb_mark_custom"),
                           uiOutput("response_custom"),

                           actionButton(inputId = "save_all",
                                        label = "Save all")
                  )

      )),

    mainPanel(

      tabsetPanel(
        type = "tabs",
        id = "navbar",
        tabPanel("Connection",
                 visNetworkOutput("connectionPlot"),
                 actionButton("connection_save","Save")),
        tabPanel("AVE",
                 plotOutput("AVEPlot"),
                 actionButton("ave_save","Save")),
        tabPanel("Samples",
                 plotlyOutput("samplesPlot", height = 700),
                 actionButton("samples_save","Save")),
        tabPanel("Corcircle",
                 plotlyOutput("corcirclePlot"),
                 actionButton("corcircle_save","Save")),
        tabPanel("Fingerprint",
                 plotlyOutput("fingerprintPlot", height = 700),
                 actionButton("fingerprint_save","Save")),
        tabPanel("Bootstrap",
                 plotlyOutput("bootstrapPlot", height = 700),
                 actionButton("bootstrap_save","Save"))
      )

    )
  ),

  use_bs_tooltip()
)
