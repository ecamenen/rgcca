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

server <- function(input, output, session) {
  source("R/parsing.R")
  source("R/plot.R")
  source("R/select.type.R")
  source("R/network.R")

  # Assign reactive variables
  reac_var  <<- reactiveVal()
  id_block_y <<- id_block <<- id_block_resp <<- analysis <<- boot <<- analysis_type <<- NULL
  clickSep <<- FALSE
  if_text <<- TRUE
  axis1 <<- 1
  nb_comp <<- axis2 <<- 2
  nb_mark <<- 100

  # maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar,
  # rcon-pca, ridge-gca, , ssqcov-1, ssqcov-2, , sum-pca, sumcov-1, sumcov-2

  ################################################ User Interface ################################################

  output$blocks_names_custom_x <- renderUI({
    setNamesInput("x")
  })

  output$tau_custom <- renderUI({
    if(!is.null(input$analysis_type) && input$analysis_type == "SGCCA"){
      par_name <- "C1"
      cond <- "input.analysis_type == SGCCA"
    }else{
      par_name <- "Tau"
      cond <- "input.tau_opt == false"
    }

    conditionalPanel(
      condition = cond,
      sliderInput(inputId = "tau",
                  label = h5(par_name),
                  min = 0, max = 1, value = 1, step = .1)
   )
  })

  setNamesInput = function(x){
    refesh = c(input$superblock, input$supervised, input$analysis_type)
    selectInput(inputId = paste0("names_block_", x),
                label = h5( paste0("Block for the ", x ,"-axis")),
                choices = getNames(),
                selected = setBlockNames())
  }

  output$blocks_names_custom_y <- renderUI({
    setNamesInput("y")
  })

  output$blocks_names_response<- renderUI({
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
    # Set dynamically the maximum number of component that should be used in the analysis

    # Get the number minimum of columns among the whole blocks
    reac_var(getMinComp())
    # Dynamically assign this number of component
    assign("nb_comp", reac_var(), .GlobalEnv)

    sliderInput(inputId = "nb_comp",
                label = h5("Number of components"),
                min = 2, max = getDefaultComp(), value = 2, step = 1)

    # TODO: pas plusieurs sliderInput, découper en modules
  })

  output$axis1_custom <- renderUI({
    refresh <- input$nb_comp
    sliderInput(inputId = "axis1",
                label = h5("Component for the X-axis"),
                min = 1, max = input$nb_comp, value = 1, step = 1)
  })

  output$axis2_custom <- renderUI({
    refresh <- input$nb_comp
    sliderInput(inputId = "axis2",
                label = h5("Component for the Y-axis"),
                min = 1, max = input$nb_comp, value = 2, step = 1)
  })

  output$nb_mark_custom <- renderUI({
    sliderInput(inputId = "nb_mark",
                label = h5("Number of potential biomarkers"),
                min = 10, max = getMaxCol(), value = getDefaultCol(), step = 1)
  })

  ################################################ UI variables ################################################

  output$analysis_type_custom <- renderUI({
    refresh = c(input$blocks, input$sep)
    selectInput(inputId = "analysis_type",
              h5("Analysis method"),
              selected = analysis_type,
              choices = list(
                `One block` = one_block,
                `Two blocks` = two_blocks,
                `Multiblocks` = multiple_blocks,
                `Multiblocks with a superblock`= multiple_blocks_super
              ))
  })


  getMinComp = function(){
    # Get the maximum number of component allowed in an analysis based on the minimum
    # number of column among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      if(!is.null(blocks)){
        min = min(unlist(lapply(blocks, NCOL)))
        if(min > 5)
          return(5)
        else
          return(min)
      }
    }
    return(2)

  }

  getNames = function(){
    # Get the names of the blocks

    if(!is.null(input$blocks)){
      # Creates a list of nb_blocks dimension, each one containing a id from 1 to nb_blocks and having the same names as the blocks
      return( as.list(sapply(names(blocks), function(i) as.integer(which(names(blocks) == i)), USE.NAMES = TRUE)) )
    }else
      return(list(" " = 1))

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

  showWarn = function(f, duration = 10, show = TRUE){

    ids <- character(0)

    try(withCallingHandlers({
      res <- f
    }, message = function(w) {
      duration <<- NULL
      id <- showNotification(w$message, type = "message", duration = duration)
      ids <<- c(ids, id)
    }, warning = function(w) {

      if(show){
        id <- showNotification(w$message, type = "warning", duration = duration)
        ids <<- c(ids, id)
      }

    }, error = function(e) {
      if(show){
        message(paste("Error:", e$message))
        id <- showNotification(e$message, type = "error", duration = duration)
        ids <<- c(ids, id)
        res <<- class(e)[1]
      }
    }), silent = TRUE)

    if(is.null(duration) & length(ids) != 0){
      for (id in ids)
        removeNotification(id)
    }

    return(res)
  }

  blocksExists = function(){
    # Test if the blocks are loaded and contain any errors

    if(!is.null(input$blocks))
      if(!is.null(getInfile()))
        return(TRUE)
    return(FALSE)
  }

  setAnalysisMenu <- function(){
    refresh = c(input$blocks, input$analysis_type)
    assign("one_block", analyse_methods[[1]], .GlobalEnv)
    assign("two_blocks", analyse_methods[[2]], .GlobalEnv)
    assign("multiple_blocks", analyse_methods[[3]], .GlobalEnv)
    assign("multiple_blocks_super", analyse_methods[[4]], .GlobalEnv)
  }

  setIdBlock = function(){

    if( !superblock && !is.null(input$names_block_x) && as.integer(input$names_block_x) > round(length(blocks)) ){
      reac_var(as.integer(input$names_block_x))
      assign("id_block", reac_var() - 1, .GlobalEnv)
      assign("id_block_y", reac_var() - 1, .GlobalEnv)
    }else{
      # By default, when a new dataset is loaded, the selected block is the last
      assign("id_block", length(blocks), .GlobalEnv)
      assign("id_block_y", length(blocks), .GlobalEnv)
    }

  }

  getDynamicVariables = function(){
    # Refresh all the plots when any input is changed

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme, input$nb_mark,
                input$scale, input$init, input$axis1, input$axis2, input$response, input$tau, input$tau_opt, input$analysis_type,
                input$connection, input$nb_comp, input$names_block_x, input$names_block_y, input$boot, input$text,
                input$names_block_response, input$supervised, input$run_analysis )
  }


  ################################################ Plots  ################################################

  samples <- function() plotSamplesSpace(rgcca = rgcca.res,
                                         resp = response,
                                         comp_x = axis1,
                                         comp_y = axis2,
                                         i_block = id_block,
                                         text = if_text,
                                         i_block_y = id_block_y,
                                         reponse_name = input$response$name)

  corcircle <- function() plotVariablesSpace(rgcca = rgcca.res,
                                             blocks = blocks,
                                             comp_x = axis1,
                                             comp_y = axis2,
                                             superblock = (superblock & tolower(analysis_type) != "pca"),
                                             i_block = id_block,
                                             text = if_text)

  fingerprint <- function() plotFingerprint(rgcca = rgcca.res,
                                            blocks = blocks,
                                            comp = axis1,
                                            superblock = (superblock & tolower(analysis_type) != "pca"),
                                            n_mark = nb_mark,
                                            i_block = id_block)

  ave <- function() plotAVE(rgcca = rgcca.res)

  conNet <- function() plotNetwork2(nodes, edges, blocks)

  plotBoot <- function() plotBootstrap(boot,
                                       axis1,
                                       nb_mark,
                                       id_block)

  ################################################ Analysis ################################################

  setParRGCCA <- function(verbose = TRUE){

    blocks = blocks_without_superb
    ncomp = rep(nb_comp, length(blocks))

    if(is.null(analysis_type) | is.null(input$analysis_type))
      analysis_type <- "RGCCA"
    else
      analysis_type <- input$analysis_type

    # Tau is set to optimal by default
    if (input$tau_opt && analysis_type != "SGCCA")
      tau = "optimal"
    else
      # otherwise the tau value fixed by the user is used
      tau = rep(input$tau, length(blocks))

    setAnalysisMenu()

    if(length(blocks) == 1){
      # if(verbose)
        # showWarn(warning("Only one block is selected. By default, a PCA is performed."))
      analysis_type <- "PCA"
      assign("two_blocks", NULL, .GlobalEnv)
      assign("multiple_blocks", NULL, .GlobalEnv)
      assign("multiple_blocks_super", NULL, .GlobalEnv)
    }else if(length(blocks) == 2){
      assign("one_block", NULL, .GlobalEnv)
      assign("multiple_blocks", NULL, .GlobalEnv)
      assign("multiple_blocks_super", NULL, .GlobalEnv)
      if(!tolower(analysis_type) %in% c("cca", "ra", "ifa", "pls")){
        # showWarn(warning("Only two blocks are selected. By default, a PLS is performed."))
        analysis_type <- "PLS"
      }
    }else if(length(blocks) > 2){
      assign("one_block", NULL, .GlobalEnv)
      assign("two_blocks", NULL, .GlobalEnv)
    }

    getNames()

    if(!input$supervised)
      response = NULL
    else
      response = input$supervised

    pars = showWarn(checkSuperblock(list(response = response, superblock = input$superblock)), show = FALSE)

    if(input$supervised || tolower(analysis_type) == "ra"){
      pars = setPosPar(list(tau = tau, ncomp = ncomp, superblock = pars$superblock), blocks, id_block_resp)
      blocks = pars$blocks; tau = pars$tau; ncomp = pars$ncomp
    }

    pars = showWarn(select.type(A = blocks, C = NULL, tau = tau,
                       ncomp = ncomp, scheme = input$scheme,
                       superblock = pars$superblock, type  = analysis_type, quiet = TRUE))

    c1 = showWarn(checkC1(pars$blocks, pars$tau, analysis_type))

    if(length(pars) == 1 | !is.null(unlist(c1)))
      return(NULL)

    assign("connection", pars$connection, .GlobalEnv)
    assign("tau", pars$tau, .GlobalEnv)
    assign("ncomp", pars$ncomp, .GlobalEnv)
    assign("scheme", pars$scheme, .GlobalEnv)
    assign("superblock", pars$superblock, .GlobalEnv)
    assign("analysis_type", analysis_type, .GlobalEnv)

    return(pars$blocks)
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


  setResponseShiny = function(){

    response <- showWarn(setResponse (blocks = blocks_without_superb,
                        file = input$response$datapath,
                        sep = input$sep,
                        header = input$header))

    if(length(response) > 1)
      assign("response", response, .GlobalEnv)

  }

  setConnectionShiny = function(){

    file <- input$connection$datapath

    if(is.null(connection)){
      try(withCallingHandlers(
        connection <- showWarn(setConnection (blocks = blocks,
                    superblock = (is.null(file) & ( superblock  | input$supervised) ),
                    file = file,
                    sep = input$sep))
      ))

      # Error due to the superblock disabling and the connection have not the same size than the number of blocks
      if( identical(connection, "104") )
        connection <- showWarn(setConnection(blocks = blocks,
                   superblock = ( superblock  | input$supervised ),
                   file = NULL,
                   sep = input$sep))

    }

    if(is.matrix(connection)){
      assign("connection", connection, .GlobalEnv)
      assign("analysis", NULL, .GlobalEnv)
      assign("boot", NULL, .GlobalEnv)
    }

  }

  setAnalysis = function(){

    blocks <- setParRGCCA()

    if(!is.null(blocks)){
      assign("blocks", blocks, .GlobalEnv)
      setConnectionShiny()
      setIdBlock()
    }

  }

################################################ Events ################################################

  setToggle = function(id)
    toggle(condition = (input$analysis_type %in% c("RGCCA", "SGCCA") && length(input$blocks$datapath) > 2), id = id)

  setToggle2 = function(id)
    toggle(condition = (input$analysis_type %in% c("RA", "RGCCA", "SGCCA")), id = id)

  setToggleSaveButton = function(id)
    toggle(condition = !is.null(analysis), id = id)

  #  | (input$analysis_type %in% c("RGCCA", "SGCCA") & !input$superblock )

  observe({

    # Event related to input$analysis_type
    toggle(condition = (input$analysis_type == "RGCCA"), id = "tau_opt")
    setToggle("tau_custom")
    setToggle("scheme")
    setToggle("superblock")
    setToggle("connection")
    setToggle2("blocks_names_response")
    setToggle("supervised")
    hide(selector = "#tabset li a[data-value=Graphic]")
    toggle(condition = (length(input$blocks$datapath) > 1), id = "blocks_names_custom_x")
    toggle(condition = (length(input$blocks$datapath) > 1), id = "blocks_names_custom_y")
    setToggleSaveButton('connection_save')
    for (i in c("bootstrap_save", "fingerprint_save", "corcircle_save", "samples_save", "ave_save", "connection_save"))
      setToggleSaveButton(i)
  })

  observeEvent(c(input$navbar, input$tabset), {

    toggle(condition = ( input$navbar == "Fingerprint"), id = "nb_mark_custom")
    toggle(condition = ( input$navbar != "Fingerprint"), id = "text")
    toggle(condition = ( input$navbar != "Fingerprint"), id = "axis2_custom")
    toggle(condition = ( input$navbar == "Samples"), id = "blocks_names_custom_y")
    toggle(condition = ( !is.null(analysis) && ! input$navbar %in% c("Connection", "AVE")), selector =  "#tabset li a[data-value=Graphic]" )
   })

  observeEvent(input$navbar, {
    if(!is.null(analysis) && input$navbar %in% c("Connection", "AVE"))
      updateTabsetPanel(session, "tabset", selected = "RGCCA")
    else if(!is.null(analysis))
      updateTabsetPanel(session, "tabset", selected = "Graphic")
  })

  observe({
    # Initial events

    hide(selector = "#tabset li a[data-value=RGCCA]")
    hide(selector = "#navbar li a[data-value=Bootstrap]")
    hide(id = "run_boot")
    hide(id = "boot")
    hide(id = "header")
    hide(id = "init")
    hide(id = "response")
    hide(id = "connection")
  })

  onclick("sep", function(e) assign("clickSep", TRUE, .GlobalEnv))

  observeEvent(c(input$blocks, input$sep), {
    if(blocksExists()){
      #assign("analysis", NULL, .GlobalEnv)
      setToggle("connection")
      show(id = "response")
    }
  })

  getInfile <- eventReactive(c(input$blocks, input$sep), {
    # Return the list of blocks

    # Load the blocks
    paths = paste(input$blocks$datapath, collapse = ',')
    names = paste(input$blocks$name, collapse = ',')

    assign("blocks_unscaled",
           showWarn(setBlocks (file = paths,
                      names = names,
                      sep = input$sep,
                      header = TRUE),
                    duration = 2
                    ),
           .GlobalEnv)

    if(!is.list(blocks_unscaled)){
      assign("analysis", NULL, .GlobalEnv)
      return(NULL)
    }else{
      show(selector = "#tabset li a[data-value=RGCCA]")
      setToggle("connection")
    }

    assign("blocks_without_superb",
           scaling(blocks_unscaled, input$scale, TRUE),
           .GlobalEnv)

    # reactualiser l'analyse
    assign("analysis", NULL, .GlobalEnv)
    assign("nb_comp", 2, .GlobalEnv)
    assign("analysis_type", NULL, .GlobalEnv)
    assign("response", NULL, .GlobalEnv)
    assign("connection", NULL, .GlobalEnv)

    assign("id_block_resp", length(blocks_without_superb), .GlobalEnv)
    blocks = setParRGCCA(FALSE)
    assign("blocks", blocks, .GlobalEnv)
    setResponseShiny()
    setConnectionShiny()
    setIdBlock()

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

  observeEvent(c(input$response, input$header), {
    if(blocksExists())
      setResponseShiny()
  })

  observeEvent(input$connection, {
    if(blocksExists())
      setConnectionShiny()
  })

  observeEvent(input$run_analysis, {

    if(!is.null(getInfile()) & is.matrix(connection))
      assign("analysis", setRGCCA(), .GlobalEnv)

    for (i in c("bootstrap_save", "fingerprint_save", "corcircle_save", "samples_save", "ave_save", "connection_save"))
      setToggleSaveButton(i)
  })

  observeEvent(c(input$superblock, input$supervised, input$nb_comp, input$scheme, input$init, input$tau, input$tau_opt, input$analysis_type), {
    # Observe if analysis parameters are changed

    if(blocksExists()){
      setNamesInput("x")
      setNamesInput("response")
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
      reac_var(as.integer(input$names_block_x))
      assign("id_block", reac_var(), .GlobalEnv)
    }
  })

  observeEvent(input$names_block_y, {
    # Observe if graphical parameters are changed

    if(blocksExists()){
      reac_var(as.integer(input$names_block_y))
      assign("id_block_y", reac_var(), .GlobalEnv)
    }

  })

  observeEvent(input$names_block_response, {
    # Observe if graphical parameters are changed

    if(blocksExists()){

      if(input$supervised || input$analysis_type == "RA")
        reac_var(as.integer(input$names_block_response))
      else
        reac_var(as.integer(input$names_block_response) - 1)

      assign("id_block_resp", reac_var(), .GlobalEnv)
      assign("nb_comp", input$nb_comp, .GlobalEnv)
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

  observeEvent(c(input$text, input$axis1, input$axis2, input$nb_mark), {
    if(!is.null(analysis)){
      assign("if_text", input$text, .GlobalEnv)
      assign("axis1", input$axis1, .GlobalEnv)
      assign("axis2", input$axis2, .GlobalEnv)
      if(!is.null(input$nb_mark))
        assign("nb_mark", input$nb_mark, .GlobalEnv)
    }
  })

  ################################################ Outputs ################################################

  output$samplesPlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis)){
      observeEvent(input$samples_save, savePlot("samples_plot.pdf", samples()))
      p = changeHovertext( dynamicPlot(samples(), ax, "text", TRUE, TRUE), if_text )
      if(!unique(isCharacter(na.omit(response))))
        p  = p  %>% layout(showlegend = FALSE)
      p
    }
  })

  output$corcirclePlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis)){
      observeEvent(input$corcircle_save, savePlot("corcircle.pdf", corcircle()))
      p = changeHovertext( dynamicPlot(corcircle(), ax, "text"), if_text )
      n = length(p$x$data)
      ( style(p, hoverinfo = "none", traces = c(n, n-1)) )
    }
  })

  output$fingerprintPlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis)){
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
    if(!is.null(analysis)){
      observeEvent(input$ave_save, savePlot("AVE.pdf", ave()))
      ave()
    }
  })

  output$connectionPlot <- renderVisNetwork({
    getDynamicVariables()
    if(!is.null(analysis)){
      conNet()
    }
  })

  output$bootstrapPlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis) & !is.null(boot)){
      observeEvent(input$bootstrap_save, savePlot("bootstrap.pdf", plotBoot()))
      dynamicPlotBoot(plotBoot())
    }
  })

}
