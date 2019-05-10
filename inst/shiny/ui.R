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

ui <- fluidPage(

  titlePanel("R/SGCCA - The Shiny graphical interface"),
  useShinyjs(),
  sidebarLayout(

    sidebarPanel(
      tabsetPanel(id = "tabset",
      tabPanel("Data",

      # Data loading
      fileInput(inputId = "blocks",
                label = h5("Choose blocks"),
                multiple = TRUE
      ),
      fileInput(inputId = "response",
                label = h5("Choose a response")
      ),

      # File parsing

        condition = "input.adv_pars == true",
        checkboxInput(inputId = "header",
                      label = "Consider first row as header",
                      value = TRUE),
        radioButtons(inputId = "sep",
                     label = "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tabulation = "\t"),
                     selected = "\t")
      ),
      tabPanel("RGCCA",
      # Analysis parameters
        uiOutput("analysis_type_custom"),
        uiOutput("nb_comp_custom"),
        checkboxInput(inputId = "scale",
                      label = "Scale the blocks",
                      value = TRUE),
        radioButtons("init",
                   label = "Mode of initialization",
                   choices = c(SVD = "svd",
                               Random = "random"),
                   selected = "svd"),

        fileInput(inputId = "connection",
                  label = h5("Choose a connection design")
        ),
        checkboxInput(inputId = "superblock",
                    label = "Use a superblock",
                    value = TRUE),
        checkboxInput("supervised",
                       "Supervised analysis",
                       value = FALSE),
        conditionalPanel(
           condition = "input.supervised || input.analysis_type == 'RA'",
           uiOutput("blocks_names_response")
         ),
        checkboxInput(inputId = "tau_opt",
                       label = "Use an optimal tau",
                       value = TRUE),
        uiOutput("tau_custom"),
        radioButtons(inputId = "scheme",
                     label = "Scheme function",
                     choices = c(Horst = "horst",
                                 Centroid = "centroid",
                                 Factorial = "factorial"),
                     selected = "factorial"),
        sliderInput(inputId = "boot",
                    label = h5("Number of boostraps"),
                    min = 5, max = 100, value = 10, step = 5),
        actionButton(inputId = "run_boot",
                   label = "Run bootstrap"),
        actionButton(inputId = "run_analysis",
                   label = "Run Analysis")
      ),

      # Graphical parameters

      tabPanel("Graphic",
        checkboxInput(inputId = "text",
                      label = "Print names",
                      value = TRUE),
        uiOutput("blocks_names_custom_x"),
        uiOutput("blocks_names_custom_y"),
        uiOutput("axis1_custom"),
        uiOutput("axis2_custom"),
        uiOutput("nb_mark_custom"),


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
                 plotlyOutput("samplesPlot"),
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
  )
)
