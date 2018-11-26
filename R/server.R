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
    assign("blocks",
           setBlocks (input$superblock, "../data/agriculture.tsv,../data/industry.tsv,../data/politic.tsv", "agric,ind,polit", input$sep, input$header),
           .GlobalEnv)
    assign("response",
           setResponse (blocks, input$response$datapath, input$sep, input$header),
           .GlobalEnv)
    assign("connection",
           setConnection (blocks, input$connection$datapath, input$sep),
           .GlobalEnv)
    assign("ncomp",
           rep(max(c(input$axis1, input$axis2)), length(blocks)),
           .GlobalEnv)
    assign("scheme",
           "factorial",
            .GlobalEnv)
    assign("sgcca.res",
           sgcca(A = blocks,
                      C = connection,
                      scheme = input$scheme,
                      ncomp = ncomp,
                      scale = input$scale,
                      verbose = FALSE),
           .GlobalEnv)
  })


  output$samplesPlot <- renderPlot({
    setVariables()
    plotSamplesSpace(sgcca.res, response, input$axis1, input$axis2, input$blocks)
  })

  output$corcirclePlot <- renderPlot({
    setVariables()
    names(sgcca.res$a) = names(blocks)
    plotVariablesSpace(sgcca.res, blocks, input$axis1, input$axis2, input$superblock, input$blocks)
  })

  output$fingerprintPlot <- renderPlot({
    setVariables()
    names(sgcca.res$a) = names(blocks)
    plotFingerprint(sgcca.res, input$axis1, input$superblock, input$nb_mark, input$blocks)
  })

  output$AVEPlot <- renderPlot({
    setVariables()
    plotAVE(sgcca.res, input$axis1)
  })

}
