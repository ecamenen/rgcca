# Define server logic required to draw a histogram ----
server <- function(input, output) {
  source("parsing.R")
  source("plot.R")

  librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx")
  for (l in librairies) {
    if (!(l %in% installed.packages()[, "Package"]))
      install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
    library(l, character.only = TRUE)
  }

  setVariables = reactive({
    assign("blocks",setBlocks (TRUE, "../data/agriculture.tsv,../data/industry.tsv,../data/politic.tsv", "agric,ind,polit"),
           .GlobalEnv)
    assign("response",setResponse (blocks, "../data/response.tsv"),
           .GlobalEnv)
           assign("connection", setConnection (blocks, "../data/connection.tsv"),
           .GlobalEnv)
    assign("NB_COMP",max(c(input$axis1, input$axis2)),
                  .GlobalEnv)
                  assign("ncomp", rep(NB_COMP, length(blocks)),
           .GlobalEnv)
    assign("scheme","factorial",
                         .GlobalEnv)
    assign("sgcca.res",sgcca(A = blocks,
                      C = connection,
                      scheme = scheme,
                      ncomp = ncomp,
                      scale = TRUE,
                      verbose = FALSE),
           .GlobalEnv)
    names(sgcca.res$a) = names(blocks)
  })

  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$samplePlot <- renderPlot({
    setVariables()
    plotSamplesSpace(sgcca.res, response, input$axis1, input$axis2)

  })

}
