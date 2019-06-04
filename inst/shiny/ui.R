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

setInfo <- function(., text){
  shinyInput_label_embed(
    icon("question") %>%
      bs_embed_tooltip(title = text)
  )
}

ui <- fluidPage(

  bs_modal(
    id = "modal_superblock",
    title = "Help on superblock",
    body =  "If activated, add a supplementary block (the 'superblock') corresponding to a concatenation of all the blocks.
    This block shynthesize the other blocks all together into a common space to better interpret the results.
    If disabled, a connection file could be used. Otherwise, all blocks are connected together.",
    size = "medium"
  ),

  titlePanel("R/SGCCA - The Shiny graphical interface"),
  tags$div(
    tags$strong("Authors: "),
    tags$p("Arthur TENENHAUS (", tags$a(href="arthur.tenenhaus@l2s.centralesupelec.fr","arthur.tenenhaus@l2s.centralesupelec.fr"), "), Vincent GUILLEMOT, Etienne CAMENEN")
    ),
  tags$a(href="https://github.com/BrainAndSpineInstitute/rgcca_Rpackage/blob/master/inst/shiny/tutorialShiny.md", "Go to the tutorial"),
  useShinyjs(),
  sidebarLayout(

    sidebarPanel(
      tabsetPanel(id = "tabset",
      tabPanel("Data",

      # Data loading
      fileInput(inputId = "blocks",
                label = "Blocks",
                multiple = TRUE
      ) %>%
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
                     selected = "\t") %>%
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
                      value = TRUE) %>%
        shinyInput_label_embed(
          icon("question") %>%
            bs_embed_tooltip(title = "A zero means translation is always performed. If activated, each block are standardized to unit variances and then divide them by the square root of its number of variables.")
        ),

        radioButtons("init",
                   label = "Mode of initialization",
                   choices = c(SVD = "svd",
                               Random = "random"),
                   selected = "svd"),

        checkboxInput(inputId = "tau_opt",
                       label = "Use an optimal tau",
                       value = TRUE)  %>%
          shinyInput_label_embed(
            icon("question") %>%
              bs_embed_tooltip(title = "A tau near 0 maximize the covariance in each blocks whereas a tau near 1 maximize the correlation.")
          ),
        uiOutput("tau_custom"),

        checkboxInput(inputId = "superblock",
                      label = "Use a superblock",
                      value = T) %>%
          shinyInput_label_embed(
            shiny_iconlink(name = "question-circle") %>%
              bs_attach_modal(id_modal = "modal_superblock")
        ),

        uiOutput("connection_custom"),

        checkboxInput(inputId = "supervised",
                      label = "Supervised analysis",
                      value = F),

        conditionalPanel(
          condition = "input.supervised || input.analysis_type == 'RA'",
          uiOutput("blocks_names_response")
        ),

        radioButtons(inputId = "scheme",
                     label = "Scheme function",
                     choices = c(Horst = "horst",
                                 Centroid = "centroid",
                                 Factorial = "factorial"),
                     selected = "factorial") %>%
          shinyInput_label_embed(
            icon("question") %>%
              bs_embed_tooltip(title = "The maximization of the sum of covariances between block components is calculated with : the identity function (horst scheme),
                               the absolute values (centroid scheme), the squared values (factorial scheme).")
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
                      label = "Print names",
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
