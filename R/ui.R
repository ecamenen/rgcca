library(shiny)

ui <- fluidPage(

  titlePanel("RGCCA Shiny"),


  sidebarLayout(

    sidebarPanel(
      fileInput(inputId="blocks",
                label=h5("Choose a blocks file: "),
                multiple = TRUE
      ),
      fileInput(inputId="connection",
                label=h5("Choose a connection file: ")
      ),
      fileInput(inputId="response",
                label=h5("Choose a response file: ")
      ),
      checkboxInput("header",
                    "Consider first row as header",
                    value = TRUE),
      checkboxInput("superblock",
                    "Use a superblock",
                    value = TRUE),
      checkboxInput("scale",
                    "Scale the blocks",
                    value = TRUE),
      radioButtons("scheme",
                   "Scheme function",
                   choices = c(Horst = "horst",
                               Centroid = "centroid",
                               Factorial = "factorial"),
                   selected = "factorial"),
      radioButtons("sep",
                   "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tabulation = "\t"),
                   selected = "\t"),
      sliderInput("id_block",
                  h5("Number of the block: "),
                  min = 1, max = 4, value = 4),
      sliderInput("axis1",
                 h5("PCA axis 1: "),
                 min = 1, max = 4, value = 1),
      sliderInput("axis2",
                 h5("PCA axis 2: "),
                 min = 2, max = 4, value = 2),
      sliderInput("nb_mark",
                  h5("Number of top potential biomarkers: "),
                  min = 10, max = 500, value = 100),
      actionButton("save_all",
                   "Save all")

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
