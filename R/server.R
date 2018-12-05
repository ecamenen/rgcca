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

server <- function(input, output) {
  source("parsing.R")
  source("plot.R")

  # Libraries loading
  librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx", "shinyjs")
  for (l in librairies) {
    if (!(l %in% installed.packages()[, "Package"]))
      install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
    library(l, character.only = TRUE)
  }

  assign("i_block", reactiveVal(), .GlobalEnv)
  assign("n_comp", reactiveVal(), .GlobalEnv)
  assign("click", FALSE, .GlobalEnv)

  #TODO: remove blocks, superblock from observeEvent
  output$blocks_names_custom <- renderUI({
    if(!is.null(input$blocks)){
      refesh = input$superblock
      blocks = getInfile()
      n <- round(length(blocks))
    }else{
      n <- 1
    }

    selectInput(inputId = "names_block",
                label = h5("Block selected: "),
                choices = getNames(), selected = n)
  })


  output$nb_comp_custom <- renderUI({
    n_comp(getMinComp())
    assign("nb_comp", 2, .GlobalEnv)

    sliderInput(inputId = "nb_comp",
                label = h5("Number of Component: "),
                min = 2, max = getDefaultComp(), value = 2, step = 1)

    # TODO: pas plusieurs sliderInput, découper en modules
  })

  output$axis1_custom <- renderUI({
    sliderInput(inputId = "axis1",
                label = h5("Component X-axis: "),
                min = 1, max = getNbComp(), value = 1, step = 1)
  })

  output$axis2_custom <- renderUI({
    sliderInput(inputId = "axis2",
                label = h5("Component Y-axis: "),
                min = 1, max = getNbComp(), value = 2, step = 1)
  })

  output$nb_mark_custom <- renderUI({
    sliderInput(inputId = "nb_mark",
                label = h5("Number of potential biomarkers: "),
                min = 10, max = getMaxCol(), value = getDefaultCol(), step = 1)
  })



  ################################################ Set variables ################################################

  getMinComp = function(){
    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( min(unlist(lapply(blocks, NCOL))) )
    }else{
      return(2)
    }
  }

  getNames = function(){
    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( as.list(sapply(names(blocks), function(i) as.integer(which(names(blocks) == i)), USE.NAMES = TRUE)) )
    }else{
      return(list(" " = 0))
    }
  }


  getMaxCol = function(){
    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( max(unlist(lapply(blocks, NCOL))) )
    }else{
      return(100)
    }
  }

  getDefaultComp = function(){
    if (getMinComp() < 5)
      return (getMinComp())
    else
      return (5)
  }

  getDefaultCol = function(){
  if (getMaxCol() < 100)
    return (getMaxCol())
  else
    return (100)
  }


  getNbComp = function(){
    refresh <- input$nb_comp
    return(nb_comp)
  }

  getDynamicVariables <- reactive({

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme,
                 input$scale, input$bias, input$init, input$axis1, input$axis2, input$response, input$tau, input$tau_opt,
                input$connection, input$nb_comp, input$adv_pars, input$adv_ana, input$adv_graph, input$names_block)
  })

  getInfile <- eventReactive(c(input$blocks, input$superblock, input$sep), {
    # Load the blocks
    paths = paste(input$blocks$datapath, collapse = ',')
    names = paste(input$blocks$name, collapse = ',')
    tryCatch({
      assign("blocks", setBlocks (superblock = input$superblock,
                      file = paths,
                      names = names,
                      sep = input$sep,
                      header = TRUE),
          .GlobalEnv)
    }, error = function(e) {
      if(click)
        message(e$message)
      else
        message(e$message)
    })
    assign("click", FALSE, .GlobalEnv)
    return(blocks)
  })

  onclick("sep", function(e) assign("click", TRUE, .GlobalEnv))

  setData <- reactive({
    # Load the blocks, the response and the connection matrix
    refresh = c(input$superblock, input$blocks)
    assign("response", setResponse (blocks = blocks,
                        file = NULL,
                        sep = input$sep,
                        header = input$header),
          .GlobalEnv)
    assign("connection", setConnection (blocks = blocks,
                          file = NULL,
                          sep = input$sep),
          .GlobalEnv)
  })

  setAnalysis <- eventReactive(c(nb_comp, input$nb_comp, input$scheme, input$scale, input$bias, input$init,
                                 input$connection, input$superblock, input$blocks, input$tau, input$tau_opt), {
    # Load the analysis
    refresh = c(input$superblock, input$blocks)

    if (input$tau_opt)
      tau = input$tau_opt
    else
      tau = input$tau

    ncomp = rep(nb_comp, length(blocks))
    rgcca.res = rgcca(A = blocks,
                 C = connection,
                 tau = rep(tau, length(blocks)),
                 scheme = input$scheme,
                 ncomp = ncomp,
                 scale = input$scale,
                 bias = input$bias,
                 init = input$init,
                 verbose = FALSE)

    names(rgcca.res$a)  = names(blocks)
    assign("rgcca.res", rgcca.res, .GlobalEnv)
  })

  samples <- function() plotSamplesSpace(rgcca = rgcca.res,
                                         resp = response,
                                         comp_x = input$axis1,
                                         comp_y = input$axis2,
                                         i_block = id_block)

  corcircle <- function() plotVariablesSpace(rgcca = rgcca.res,
                                             blocks = blocks,
                                             comp_x = input$axis1,
                                             comp_y = input$axis2,
                                             superblock = input$superblock,
                                             i_block = id_block)

  fingerprint <- function() plotFingerprint(rgcca = rgcca.res,
                                            comp = input$axis1,
                                            superblock = input$superblock,
                                            n_mark = input$nb_mark,
                                            i_block = id_block)
  ave <- function() plotAVE(rgcca = rgcca.res,
                            comp = input$axis1)


  setFuncs <- reactive({
    # Set plotting functions
    samples()
    corcircle()
    fingerprint()
    ave()
  })


  ################################################ Observe events ################################################

  observeEvent(c(input$superblock, input$blocks, input$sep), {
    # Observe the changes for parsing functionnalities (column separator,
    # the header, the path for the blocks and the presence of a superblock)

    if(!is.null(input$blocks)){

      if(!input$superblock && as.integer(input$names_block) > round(length(blocks)) ){
        i_block(as.integer(input$names_block))
        assign("id_block", i_block() - 1, .GlobalEnv)
      }else{
        assign("id_block", length(getInfile()), .GlobalEnv)
      }

      setData()
      assign("nb_comp", 2, .GlobalEnv)
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(input$response, {
    # Observe if a response is fixed
    if(!is.null(input$blocks)){
      assign("response", setResponse (blocks = blocks,
                          file = input$response$datapath,
                          sep = input$sep,
                          header = input$header),
             .GlobalEnv)
    }
  })

  observeEvent(input$connection, {
    # Observe if a connection is fixed
    if(!is.null(input$blocks)){
      connection = setConnection (blocks = blocks,
                                          file = input$connection$datapath,
                                          sep = input$sep)
      assign("connection", connection,
            .GlobalEnv)
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(c(input$nb_comp, input$scheme, input$scale, input$bias, input$init, input$tau, input$tau_opt), {
    # Observe if analysis parameters are changed
    if(!is.null(input$blocks)){
      assign("nb_comp", input$nb_comp, .GlobalEnv)
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(c(input$names_block, input$nb_mark, input$axis1, input$axis2), {
    # Observe if graphical parameters are changed
    if(!is.null(input$blocks)){
      i_block(as.integer(input$names_block))
      assign("id_block", i_block(), .GlobalEnv)
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
    if(!is.null(input$blocks)){
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      samples()
    }
  })

  output$corcirclePlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks)){
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      corcircle()
    }
  })

  output$fingerprintPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks)){
      observeEvent(input$fingerprint_save, savePlot("fingerprint.pdf", fingerprint()))
      fingerprint()
    }
  })

  output$AVEPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(input$blocks)){
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

}
