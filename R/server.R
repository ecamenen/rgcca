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

rm(list=ls())

server <- function(input, output) {
  source("parsing.R")
  source("plot.R")

  # Libraries loading
  librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx")
  for (l in librairies) {
    if (!(l %in% installed.packages()[, "Package"]))
      install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
    library(l, character.only = TRUE)
  }

  # assign("blocks", NULL, .GlobalEnv)


  output$id_block_custom = renderUI({
    names = paste(input$blocks$name, collapse = ',')
    n = length(names)
    sliderInput(inputId = "id_block",
                label = h5("Block selected: "),
                min = 1, max = n, value = n)
  })

  ################################################ Set variables ################################################

  getDynamicVariables <- reactive({

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme,
                 input$scale, input$bias, input$init, input$axis1, input$axis2, input$id_block, input$response,
                input$connection)
  })

  setInfile <- reactive({
    # Load the blocks

    paths = paste(input$blocks$datapath, collapse = ',')
    names = paste(input$blocks$name, collapse = ',')

    assign("blocks", setBlocks (superblock = input$superblock,
                      file = paths,
                      names = names,
                      sep = input$sep,
                      header = input$header),
          .GlobalEnv)
  })

  setData <- reactive({
    # Load the blocks, the response and the connection matrix

    setInfile()
    assign("response", setResponse (blocks = blocks,
                        file = input$response$datapath,
                        sep = input$sep,
                        header = input$header),
          .GlobalEnv)
    assign("connection", setConnection (blocks = blocks,
                          file = input$connection$datapath,
                          sep = input$sep),
          .GlobalEnv)
  })

  setAnalysis <- reactive({
    # Load the analysis

    ncomp = rep(max(c(input$axis1, input$axis2)), length(blocks))
    sgcca.res = sgcca(A = blocks,
                 C = connection,
                 scheme = input$scheme,
                 ncomp = ncomp,
                 scale = input$scale,
                 bias = input$bias,
                 init = input$init,
                 verbose = FALSE)
    names(sgcca.res$a)  = names(blocks)

    assign("sgcca.res", sgcca.res, .GlobalEnv)
  })

  samples <- function() plotSamplesSpace(rgcca = sgcca.res,
                                         resp = response,
                                         comp_x = input$axis1,
                                         comp_y = input$axis2,
                                         i_block = input$id_block)

  corcircle <- function() plotVariablesSpace(rgcca = sgcca.res,
                                             blocks = blocks,
                                             comp_x = input$axis1,
                                             comp_y = input$axis2,
                                             superblock = input$superblock,
                                             i_block = input$id_block)

  fingerprint <- function() plotFingerprint(rgcca = sgcca.res,
                                            comp = input$axis1,
                                            superblock = input$superblock,
                                            n_mark = input$nb_mark,
                                            i_block = input$id_block)

  ave <- function() plotAVE(rgcca = sgcca.res,
                            comp = input$axis1)


  setFuncs <- reactive({
    # Set plotting functions

    samples()
    corcircle()
    fingerprint()
    ave()
  })


  ################################################ Observe events ################################################


  observeEvent(c(input$sep, input$header, input$blocks, input$superblock), {
    # Observe the changes for parsing functionnalities (column separator,
    # the header, the path for the blocks and the presence of a superblock)

    if(!is.null(input$blocks$datapath)){
      setData()
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(input$response, {
    # Observe if a response is fixed
    if(!is.null(input$blocks$datapath)){
      assign("response", setResponse (blocks = blocks,
                          file = input$response$datapath,
                          sep = input$sep,
                          header = input$header),
             .GlobalEnv)
      setFuncs()
    }
  })

  observeEvent(input$connection, {
    # Observe if a connection is fixed
    if(!is.null(input$blocks$datapath)){
      assign("connection", setConnection (blocks = blocks,
                            file = input$connection$datapath,
                            sep = input$sep),
            .GlobalEnv)
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(c(input$scheme, input$scale, input$bias, input$init, input$axis1, input$axis2), {
    # Observe if analysis parameters are changed
    if(!is.null(input$blocks$datapath)){
      getDynamicVariables()
      setAnalysis()
      setFuncs()
      print(sgcca.res$AVE$AVE_X[[1]])
    }
  })

  observeEvent(c(input$id_block, input$nb_mark), {
    # Observe if graphical parameters are changed
    if(!is.null(input$blocks$datapath)){
      setFuncs()
    }
  })

#TODO : Duplicates rows are not allowed

  observeEvent(input$save_all, {
    if(!is.null(input$blocks$datapath)){
      savePlot("samples_plot.pdf", samples())
      savePlot("corcircle.pdf", corcircle())
      savePlot("fingerprint.pdf", fingerprint())
      savePlot("AVE.pdf", ave())
    }
  })

  ################################################ Outputs ################################################


  output$samplesPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks$datapath)){
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      samples()
    }
  })

  output$corcirclePlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks$datapath)){
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      corcircle()
    }
  })

  output$fingerprintPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks$datapath)){
      observeEvent(input$fingerprint_save, savePlot("fingerprint.pdf", fingerprint()))
      fingerprint()
    }
  })

  output$AVEPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks$datapath)){
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

}
