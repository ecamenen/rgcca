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

server <- function(input, output) {
  source("../../R/parsing.R")
  source("../../R/plot.R")
  source("../../R/select.type.R")
  source("../../R/network.R")

  # Assign reactive variables
  assign("i_block", reactiveVal(), .GlobalEnv)
  assign("id_block", NULL, .GlobalEnv)
  assign("id_block_y", NULL, .GlobalEnv)
  assign("n_comp", reactiveVal(), .GlobalEnv)
  assign("clickSep", FALSE, .GlobalEnv)

  # maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar,
  # rcon-pca, ridge-gca, , ssqcov-1, ssqcov-2, , sum-pca, sumcov-1, sumcov-2

  #TODO: remove blocks, superblock from observeEvent
  output$blocks_names_custom_x <- renderUI({
    refresh <- input$superblock
    setNamesInput()
  })

  setNamesInput = function(){
    refesh = input$superblock
    selectInput(inputId = "names_block_x",
                label = h5("Block for X-axis : "),
                choices = getNames(),
                selected = setBlockNames())
  }

  output$blocks_names_custom_y <- renderUI({

    # Refresh the function when superblock option is changed
    refesh = input$superblock

    selectInput(inputId = "names_block_y",
                label = h5("Block for Y-axis : "),
                choices = getNames(),
                selected = setBlockNames())
  })

  # Define the names of the blocks and set by default on the last block
   setBlockNames = function(){

    if(!is.null(input$blocks)){
      # Set selected value on the last block
      return(round(length(blocks)))
    }else{
      # If any dataset is selected
      return(1)
    }
  }

  output$nb_comp_custom <- renderUI({
    # Set dynamicly the maximum number of component that should be used in the analysis

    # Get the number minimum of columns among the whole blocks
    n_comp(getMinComp())
    # Dynamically assign this number of component
    assign("nb_comp", 2, .GlobalEnv)

    sliderInput(inputId = "nb_comp",
                label = h5("Number of Component: "),
                min = 2, max = getDefaultComp(), value = 2, step = 1)

    # TODO: pas plusieurs sliderInput, découper en modules
  })

  output$axis1_custom <- renderUI({
    refresh <- input$nb_comp
    sliderInput(inputId = "axis1",
                label = h5("Component for X-axis: "),
                min = 1, max = input$nb_comp, value = 1, step = 1)
  })

  output$axis2_custom <- renderUI({
    refresh <- input$nb_comp
    sliderInput(inputId = "axis2",
                label = h5("Component for Y-axis: "),
                min = 1, max = input$nb_comp, value = 2, step = 1)
  })

  output$nb_mark_custom <- renderUI({
    sliderInput(inputId = "nb_mark",
                label = h5("Number of potential biomarkers: "),
                min = 10, max = getMaxCol(), value = getDefaultCol(), step = 1)
  })

  ################################################ Set variables ################################################

  getMinComp = function(){
    # Get the maximum number of component allowed in an analysis based on the minimum
    # number of column among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( min(unlist(lapply(blocks, NCOL))) )
    }else
      return(2)

  }

  getNames = function(){
    # Get the names of the blocks

    if(!is.null(input$blocks)){
      # Creates a list of nb_blocks dimension, each one containing a id from 1 to nb_blocks and having the same names as the blocks
      return( as.list(sapply(names(blocks), function(i) as.integer(which(names(blocks) == i)), USE.NAMES = TRUE)) )
    }else
      return(list(" " = 0))

  }

  getMaxCol = function(){
    # Get the maximum number of columns among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( max(unlist(lapply(blocks, NCOL))) )
    }else
      return(100)

  }

  getDefaultComp = function(){
    # Set the maximum of component to the minimum
    # number of column among the blocks but not higher than 5

    if (getMinComp() < 5)
      return (getMinComp())
    else
      return (5)
  }

  getDefaultCol = function(){
    # Set the maximum of biomarkers to the maximum
    # number of column among the blocks but not lower than 100

    if (getMaxCol() < 100)
      return (getMaxCol())
    else
      return (100)
  }

  getDynamicVariables = function(){
    # Refresh all the plots when any input is changed

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme, input$nb_mark,
                input$scale, input$init, input$axis1, input$axis2, input$response, input$tau, input$tau_opt, input$analysis_type,
                input$connection, input$nb_comp, input$names_block_x, input$names_block_y, input$boot, input$text )
  }


  setParRGCCA <- function(){

    # Tau is set to optimal by default
    if (input$tau_opt)
      tau = "optimal"
    else
      # otherwise the tau value fixed by the user is used
      tau = input$tau

    blocks = blocks_without_superb

    pars = select.type(A = blocks, C = NULL, tau = rep(tau, length(blocks)),
                       ncomp = rep(nb_comp, length(blocks)), scheme = input$scheme,
                       superblock = input$superblock, type  = input$analysis_type)

    assign("connection", pars$connection, .GlobalEnv)
    assign("tau", pars$tau, .GlobalEnv)
    assign("ncomp", pars$ncomp, .GlobalEnv)
    assign("scheme", pars$scheme, .GlobalEnv)
    assign("superblock", pars$superblock, .GlobalEnv)

    return(pars$blocks)
  }

  setData <- function() {
    # Load the blocks, the response and the connection matrix

    # This function activated only when a new dataset is loaded, set the reponse
    # and connection of the previous dataset to NULL
    assign("response",
           setResponse (blocks = blocks,
                        file = NULL,
                        sep = input$sep,
                        header = input$header),
          .GlobalEnv)

    if(is.null(connection)){
      assign("connection",
             setConnection (blocks = blocks,
                            superblock = superblock,
                            file = NULL,
                            sep = input$sep),
            .GlobalEnv)
    }
  }

  setRGCCA <- function() {
    # Load the analysis

    rgcca.res = rgcca.analyze(blocks,
                 connection = connection,
                 tau = tau,
                 ncomp = ncomp,
                 scheme = scheme,
                 scale = FALSE,
                 init = input$init,
                 bias = TRUE,
                 type = input$analysis_type)

    assign("rgcca.res", rgcca.res, .GlobalEnv)

    assign("nodes", getNodes(blocks, rgcca = rgcca.res), .GlobalEnv)
    assign("edges", getEdges(connection, blocks), .GlobalEnv)
    getBoot()
  }

  getBoot <- function()
    assign("boot",
           bootstrap(blocks, input$boot, connection, tau, ncomp, input$scheme, input$scale, input$init, TRUE),
           .GlobalEnv)

  samples <- function() plotSamplesSpace(rgcca = rgcca.res,
                                         resp = response,
                                         comp_x = input$axis1,
                                         comp_y = input$axis2,
                                         i_block = id_block,
                                         text = input$text,
                                         i_block_y = id_block_y)

  corcircle <- function() plotVariablesSpace(rgcca = rgcca.res,
                                             blocks = blocks,
                                             comp_x = input$axis1,
                                             comp_y = input$axis2,
                                             superblock = superblock,
                                             i_block = id_block,
                                             text = input$text)

  fingerprint <- function() plotFingerprint(rgcca = rgcca.res,
                                            comp = input$axis1,
                                            superblock = superblock,
                                            n_mark = input$nb_mark,
                                            i_block = id_block)

  ave <- function() plotAVE(rgcca = rgcca.res,
                            comp = input$axis1)

  conNet <- function() plotNetwork(nodes, edges, blocks)
  conNet2 <- function() plotNetwork2(nodes, edges, blocks)

  plotBoot <- function() plotBootstrap(boot,
                                       input$axis1,
                                       input$nb_mark,
                                       id_block)

  blocksExists = function(){
    # Test if the blocks are loaded and contain any errors

    if(!is.null(input$blocks))
      if(!is.null(getInfile()))
        return(TRUE)
    return(FALSE)
  }

  setIdBlock = function(){

    if(!superblock && as.integer(input$names_block_x) > round(length(blocks)) ){
      i_block(as.integer(input$names_block_x))
      assign("id_block", i_block() - 1, .GlobalEnv)
      assign("id_block_y", i_block() - 1, .GlobalEnv)
    }else{
      # By default, when a new dataset is loaded, the selected block is the last
      assign("id_block", length(blocks), .GlobalEnv)
      assign("id_block_y", length(blocks), .GlobalEnv)
    }

  }

  setAnalysis = function(){
    assign("blocks", setParRGCCA(), .GlobalEnv)

    if(is.null(connection))
      assign("connection", setConnection (blocks = blocks,
                                          superblock = input$superblock,
                                          file = input$connection$datapath,
                                          sep = input$sep),
             .GlobalEnv)

    setRGCCA()
    setIdBlock()
  }

  ################################################ Observe events ################################################

  onclick("sep", function(e) assign("clickSep", TRUE, .GlobalEnv))

  getInfile <- eventReactive(c(input$blocks, input$sep), {
    # Return the list of blocks

    # Load the blocks
    paths = paste(input$blocks$datapath, collapse = ',')
    names = paste(input$blocks$name, collapse = ',')

    tryCatch({
      assign("blocks_unscaled",
             setBlocks (file = paths,
                        names = names,
                        sep = input$sep,
                        header = TRUE),
             .GlobalEnv)

      assign("blocks_without_superb",
             scaling(blocks_unscaled, input$scale, TRUE),
             .GlobalEnv)

      blocks = setParRGCCA()
      assign("blocks", blocks, .GlobalEnv)

      setData()
      # By default, the number of component is set to 2
      assign("nb_comp", 2, .GlobalEnv)
      setRGCCA()
      setIdBlock()

    }, error = function(e) {

      assign("blocks", NULL, .GlobalEnv)
      if(clickSep)
        message(e$message)

    })

    assign("clickSep", FALSE, .GlobalEnv)
    return(blocks)
  })

  observeEvent(c(input$scale), {
    if(blocksExists()){
      assign("blocks_without_superb",
             scaling(blocks_unscaled, input$scale, TRUE),
             .GlobalEnv)
      setAnalysis()
    }
  })

  observeEvent(input$superblock, {
    if(blocksExists()){
      setAnalysis()
      setNamesInput()
    }
  })

  observeEvent(c(input$response, input$header), {
    # Observe if a response is fixed

    if(blocksExists()){
      tryCatch({
        assign("response", setResponse (blocks = blocks,
                            file = input$response$datapath,
                            sep = input$sep,
                            header = input$header),
               .GlobalEnv)
      }, error = function(e) {
        #TODO: catch and english error also
        if(e$message == "la ligne 1 n'avait pas 2 éléments"){
          message ("The first line does not have a row name")
        }
        else
          message(e$message)
      })
    }

  })

  observeEvent(input$connection, {
    # Observe if a connection is fixed

    if(blocksExists()){
      tryCatch({

        if(is.null(connection))
          assign("connection", setConnection (blocks = blocks,
                                              superblock = input$superblock,
                                              file = input$connection$datapath,
                                              sep = input$sep),
                .GlobalEnv)
        setRGCCA()

      }, error = function(e) {
        message(e$message)
      })
    }

  })

  observeEvent(c(input$nb_comp, input$scheme, input$init, input$tau, input$tau_opt, input$analysis_type), {
    # Observe if analysis parameters are changed

    if(blocksExists()){
      assign("nb_comp", input$nb_comp, .GlobalEnv)
      setAnalysis()
    }
  })

  observeEvent(input$boot, {
    if(blocksExists())
      getBoot()
  })

  observeEvent(input$names_block_x, {
    # Observe if graphical parameters are changed

    if(blocksExists()){
      i_block(as.integer(input$names_block_x))
      assign("id_block", i_block(), .GlobalEnv)
    }

  })

  observeEvent(input$names_block_y, {
    # Observe if graphical parameters are changed

    if(blocksExists()){
      i_block(as.integer(input$names_block_y))
      assign("id_block_y", i_block(), .GlobalEnv)
    }

  })

  observeEvent(input$save_all, {
    if(blocksExists()){
      savePlot("samples_plot.pdf", samples())
      savePlot("corcircle.pdf", corcircle())
      savePlot("fingerprint.pdf", fingerprint())
      savePlot("AVE.pdf", ave())
    }
  })

  ################################################ Outputs ################################################

  output$samplesPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      samples()
    }
  })

  output$corcirclePlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      corcircle()
    }
  })

  output$fingerprintPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$fingerprint_save, savePlot("fingerprint.pdf", fingerprint()))
      fingerprint()
    }
  })

  output$AVEPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

  output$connectionPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$connection_save, savePlot("connection.pdf", conNet()))
      conNet()
    }
  })

  output$bootstrapPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$bootstrap_save, savePlot("bootstrap.pdf", plotBoot()))
      plotBoot()
    }
  })

}
