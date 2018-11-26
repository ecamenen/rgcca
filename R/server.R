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

    paths = paste(input$blocks$datapath, collapse = ',')

    assign("blocks",
           setBlocks (input$superblock, paths, "agric,ind,polit", input$sep, input$header),
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

  setFuncs = reactive({
    setVariables()
    names(sgcca.res$a) = names(blocks)
    assign("samples",
           function() plotSamplesSpace(sgcca.res, response, input$axis1, input$axis2, input$id_block),
           .GlobalEnv)
    assign("corcircle",
           function() plotVariablesSpace(sgcca.res, blocks, input$axis1, input$axis2, input$superblock, input$id_block),
           .GlobalEnv)
    assign("fingerprint",
           function() plotFingerprint(sgcca.res, input$axis1, input$superblock, input$nb_mark, input$id_block),
           .GlobalEnv)
    assign("ave",
           function() plotAVE(sgcca.res, input$axis1),
           .GlobalEnv)
  })

  observeEvent(input$save_all, {
    if(!is.null(input$blocks$datapath)){
      setFuncs()
      savePlot("samples_plot.pdf", samples())
      savePlot("corcircle.pdf", corcircle())
      savePlot("fingerprint.pdf", fingerprint())
      savePlot("AVE.pdf", ave())
    }
  })

  output$samplesPlot <- renderPlot({
    if(!is.null(input$blocks$datapath)){
      setFuncs()
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      samples()
    }
  })

  output$corcirclePlot <- renderPlot({
    if(!is.null(input$blocks$datapath)){
      setFuncs()
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      corcircle()
    }
  })

  output$fingerprintPlot <- renderPlot({
    if(!is.null(input$blocks$datapath)){
      setFuncs()
      observeEvent(input$fingerprint_save, savePlot("fingerprint.pdf", fingerprint()))
      fingerprint()
    }
  })

  output$AVEPlot <- renderPlot({
    if(!is.null(input$blocks$datapath)){
      setFuncs()
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

}
