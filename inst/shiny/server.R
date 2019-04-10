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

options(shiny.maxRequestSize = 30*1024^2)
server <- function(input, output) {
  source("../../R/parsing.R")
  source("../../R/plot.R")
  source("../../R/select.type.R")
  source("../../R/network.R")

  # Assign reactive variables
  i_block  <<- reactiveVal()
  id_block <<- NULL
  id_block_y <<- NULL
  id_block_resp <<- NULL
  n_comp <<- reactiveVal()
  clickSep <<- FALSE

  # maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar,
  # rcon-pca, ridge-gca, , ssqcov-1, ssqcov-2, , sum-pca, sumcov-1, sumcov-2

  ################################################ User Interface ################################################

  output$blocks_names_custom_x <- renderUI({
    setNamesInput("x")
  })

  setNamesInput = function(x){
    refesh = c(input$superblock, input$supervized)
    selectInput(inputId = paste0("names_block_", x),
                label = h5( paste0("Blocks for ", x ,"-axis : ")),
                choices = getNames(),
                selected = setBlockNames())
  }

  output$blocks_names_custom_y <- renderUI({
    setNamesInput("y")
  })

  output$response <- renderUI({
    setNamesInput("response")
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

    if (getMaxCol() < 50)
      return (getMaxCol())
    else
      return (50)
  }

  getDynamicVariables = function(){
    # Refresh all the plots when any input is changed

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme, input$nb_mark,
                input$scale, input$init, input$axis1, input$axis2, input$response, input$tau, input$tau_opt, input$analysis_type,
                input$connection, input$nb_comp, input$names_block_x, input$names_block_y, input$boot, input$text,
                input$names_block_response, input$supervized )
  }

  setParRGCCA <- function(){

    blocks = blocks_without_superb
    ncomp = rep(nb_comp, length(blocks))

    # Tau is set to optimal by default
    if (input$tau_opt)
      tau = "optimal"
    else
      # otherwise the tau value fixed by the user is used
      tau = rep(input$tau, length(blocks))

    if(length(blocks) == 1){
      showWarn(warning("Only one block is selected. By default, a PCA is performed."))
      assign("analysis_type", "pca", .GlobalEnv)
    }else
      assign("analysis_type", input$analysis_type, .GlobalEnv)

    if(!input$supervized)
      response = NULL
    else
      response = input$supervized

    pars = showWarn(checkSuperblock(list(response = response, superblock = input$superblock)))

    if(input$supervized){
      pars = setPosPar(list(tau = tau, ncomp = ncomp, superblock = pars$superblock), blocks, id_block_resp)
      blocks = pars$blocks; tau = pars$tau; ncomp = pars$ncomp
    }

    pars = showWarn(select.type(A = blocks, C = NULL, tau = tau,
                       ncomp = ncomp, scheme = input$scheme,
                       superblock = pars$superblock, type  = analysis_type))

    if(length(pars) == 1)
      return(NULL)

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

    assign("connection", setConnectionShiny(), .GlobalEnv)

  }

  showWarn = function(f, duration = 10){

    ids <- character(0)

    try(withCallingHandlers({
        res <- f
      }, warning = function(w) {

        id <- showNotification(w$message, type = "warning", duration = duration)
        ids <<- c(ids, id)

      }, error = function(e) {

        id <- showNotification(e$message, type = "error", duration = duration)
        ids <<- c(ids, id)
        res <<- class(e)[1]
    }), silent = TRUE)

    if(is.null(duration) & length(ids) != 0){
      for (id in ids)
        removeNotification(id)
    }

    return(res)
  }

  setRGCCA <- function() {
    # Load the analysis

    assign("rgcca.res",
           showWarn(
             rgcca.analyze(blocks,
                         connection = connection,
                         tau = tau,
                         ncomp = ncomp,
                         scheme = scheme,
                         scale = FALSE,
                         init = input$init,
                         bias = TRUE,
                         type = analysis_type),
           duration = NULL),
      .GlobalEnv)

    assign("nodes", getNodes(blocks, rgcca = rgcca.res), .GlobalEnv)
    assign("edges", getEdges(connection, blocks), .GlobalEnv)
    #getBoot()
  }

  getBoot <- function()
  assign("boot",
         bootstrap(blocks, input$boot, connection, tau, ncomp, input$scheme, input$scale, input$init, TRUE, analysis_type),
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
                                             superblock = (superblock & tolower(analysis_type) != "pca"),
                                             i_block = id_block,
                                             text = input$text)

  fingerprint <- function() plotFingerprint(rgcca = rgcca.res,
                                            comp = input$axis1,
                                            superblock = (superblock & tolower(analysis_type) != "pca"),
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

    if( !superblock && as.integer(input$names_block_x) > round(length(blocks)) ){
      i_block(as.integer(input$names_block_x))
      assign("id_block", i_block() - 1, .GlobalEnv)
      assign("id_block_y", i_block() - 1, .GlobalEnv)
    }else{
      # By default, when a new dataset is loaded, the selected block is the last
      assign("id_block", length(blocks), .GlobalEnv)
      assign("id_block_y", length(blocks), .GlobalEnv)
    }

  }

  setConnectionShiny = function(){

    file <- input$connection$datapath

    if(length(grep("[sr]gcca", tolower(analysis_type))) == 1){
      try(withCallingHandlers(
       C <- showWarn(setConnection (blocks = blocks,
                    superblock = (is.null(file) & ( superblock  | input$supervized) ),
                    file = file,
                    sep = input$sep))
      ))

      if( identical(C, "104") )
        C <- showWarn(setConnection(blocks = blocks,
                   superblock = ( superblock  | input$supervized ),
                   file = NULL,
                   sep = input$sep))
      return(C)

    }else
      return(connection)
  }

  setAnalysis = function(){

    blocks <- setParRGCCA()

    if(!is.null(blocks)){
      assign("blocks", blocks, .GlobalEnv)
      assign("connection", setConnectionShiny(), .GlobalEnv)
      setRGCCA()
      setIdBlock()
    }

  }

  ################################################ Observe events ################################################

  onclick("sep", function(e) assign("clickSep", TRUE, .GlobalEnv))

  getInfile <- eventReactive(c(input$blocks, input$sep), {
    # Return the list of blocks

    # Load the blocks
    paths = paste(input$blocks$datapath, collapse = ',')
    names = paste(input$blocks$name, collapse = ',')

    if(tolower(input$analysis_type) == "pca" & length(input$blocks$datapath) > 1){
      print("ERROR 1")
    #TODO: notification
    }else if (tolower(input$analysis_type) %in% c("cca", "ra", "ifa", "pls") & ( length(input$blocks$datapath) < 2  |  length(input$blocks$datapath) > 2 ) ){
      print("ERROR 2")
      #TODO: notification
    }

    withCallingHandlers({
      assign("blocks_unscaled",
             showWarn(setBlocks (file = paths,
                        names = names,
                        sep = input$sep,
                        header = TRUE),
                      duration = NULL
                      ),
             .GlobalEnv)

      if(length(blocks_unscaled) == 1)
        return(NULL)

      assign("blocks_without_superb",
             scaling(blocks_unscaled, input$scale, TRUE),
             .GlobalEnv)

      assign("id_block_resp", length(blocks_without_superb), .GlobalEnv)
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

  observeEvent(c(input$superblock, input$supervized), {

    if(blocksExists()){
      setNamesInput("x")
      setNamesInput("response")
      setAnalysis()
    }
  })

  observeEvent(c(input$response, input$header), {
    # Observe if a response is fixed

    if(blocksExists()){
      withCallingHandlers({
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

      C <- showWarn(setConnectionShiny())

      if(is.matrix(C)){
        assign("connection", C, .GlobalEnv)
        setRGCCA()
      }

    }

  })

  observeEvent(c(input$nb_comp, input$scheme, input$init, input$tau, input$tau_opt, input$analysis_type), {
    # Observe if analysis parameters are changed

    if(blocksExists()){
      assign("nb_comp", input$nb_comp, .GlobalEnv)
      setAnalysis()
    }
  })

  observeEvent(input$run_boot, {
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

  observeEvent(input$names_block_response, {
    # Observe if graphical parameters are changed

    if(blocksExists()){
      i_block(as.integer(input$names_block_response))
      assign("id_block_resp", i_block(), .GlobalEnv)
      setAnalysis()
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

  output$samplesPlot <- renderPlotly({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      changeHovertext( dynamicPlot(samples(), ax, "text", TRUE, TRUE), input$text )  %>%
        layout(showlegend = FALSE)
    }
  })

  output$corcirclePlot <- renderPlotly({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      p = changeHovertext( dynamicPlot(corcircle(), ax, "text"), input$text )
      n = length(p$x$data)
      ( style(p, hoverinfo = "none", traces = c(n, n-1)) )
    }
  })

  output$fingerprintPlot <- renderPlotly({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$fingerprint_save, savePlot("fingerprint.pdf", fingerprint()))
      p = changeText ( dynamicPlot(fingerprint(), ax2, "text") )
      n = unlist(lapply(p$x$data, function(x) !is.null(x$orientation)))
      for (i in 1:length(n[n]))
        p$x$data[[i]]$text = round( as.double(sub( "order: .*<br />df\\[, 1\\]: (.*)<.*", "\\1\\", p$x$data[[i]]$text )), 3)
      p
    }
  })

  output$AVEPlot <- renderPlot({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

  output$connectionPlot <- renderVisNetwork({
    getDynamicVariables()
    if(blocksExists()){
      observeEvent(input$connection_save, savePlot("connection.pdf", conNet()))
      conNet2()
    }
  })

  output$bootstrapPlot <- renderPlotly({
    getDynamicVariables()
    if(blocksExists() & input$run_boot){
      observeEvent(input$bootstrap_save, savePlot("bootstrap.pdf", plotBoot()))
      dynamicPlotBoot(plotBoot())
    }
  })

}
