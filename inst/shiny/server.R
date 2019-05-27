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
  comp_x <<- 1
  nb_comp <<- comp_y <<- 2
  nb_mark <<- 100

  # maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar,
  # rcon-pca, ridge-gca, , ssqcov-1, ssqcov-2, , sum-pca, sumcov-1, sumcov-2

  ################################################ Render UI ################################################

  output$tau_custom <- renderUI({

    refresh <- c(input$superblock)
    isolate ( setAnalysis() )
    setTauUI()
  })

  output$connection_custom <- renderUI({
    setUiConnection()
  })

  output$response_custom <- renderUI({
    setUiResponse()
  })

  output$blocks_names_custom_x <- renderUI({
    setNamesInput("x", bool = input$navbar == "Samples")
  })

  output$blocks_names_custom_y <- renderUI({
    setNamesInput("y")
  })

  output$blocks_names_response<- renderUI({
    setNamesInput("response", "Block used as a response")
  })

  output$nb_comp_custom <- renderUI({
    # Set dynamically the maximum number of component that should be used in the analysis

    # Get the number minimum of columns among the whole blocks
    reac_var(getMinComp())
    # Dynamically assign this number of component
    assign("nb_comp", reac_var(), .GlobalEnv)

    sliderInput(inputId = "nb_comp",
                label = "Number of components",
                min = 2, max = getDefaultComp(), value = 2, step = 1)

    # TODO: pas plusieurs sliderInput, découper en modules
  })

  output$comp_x_custom <- renderUI({
    refresh <- input$nb_comp
    uiComp("x", 1, input$navbar != "Fingerprint")
  })

  output$comp_y_custom <- renderUI({
    refresh <- input$nb_comp
    uiComp("y", 2)
  })

  output$nb_mark_custom <- renderUI({
    sliderInput(inputId = "nb_mark",
                label = "Number of potential biomarkers",
                min = 10, max = getMaxCol(), value = getDefaultCol(), step = 1)
  })

  output$analysis_type_custom <- renderUI({
    refresh = c(input$blocks, input$sep)
    selectInput(inputId = "analysis_type",
                "Analysis method",
                selected = analysis_type,
                choices = list(
                  `One block` = one_block,
                  `Two blocks` = two_blocks,
                  `Multiblocks` = multiple_blocks,
                  `Multiblocks with a superblock`= multiple_blocks_super
                ))
  })

  ################################################ UI function ################################################

  setTauUI <- function(){

    if(!is.null(input$analysis_type) && input$analysis_type == "SGCCA"){
      par_name <- "Sparsity"
      cond <- "input.analysis_type == SGCCA"
    }else{
      par_name <- "Tau"
      cond <- "input.tau_opt == false"
    }

    conditionalPanel(
      condition = cond,
      lapply(1:(length(blocks_without_superb)+ ifelse(input$superblock, 1, 0)), function(i){
        sliderInput(inputId = paste0("tau", i),
                    label = par_name,
                    min = 0, max = 1, value = ifelse(is.null(input$tau1), 1, input[[paste0("tau", i)]]), step = .1)
        })
    )
  }

  setNamesInput = function(x, label = NULL, bool = TRUE){

    refesh = c(input$superblock, input$supervised, input$analysis_type)

    if(is.null(label)){
      label <- "Block"

      if(bool)
        label <- paste0("Block for the ", x ,"-axis")

    }

    selectInput(inputId = paste0("names_block_", x),
                label = label,
                choices = getNames(),
                selected = setBlockNames())
  }

  # Define the names of the blocks and set by default on the last block
   setBlockNames = function(){

    if(!is.null(input$blocks)){

      if(!is.null(id_block))
        return(id_block)
      else
        return(round(length(blocks)))
      # Set selected value on the last block

    }else{
      # If any dataset is selected
      return(1)
    }
  }

  uiComp <- function(x, y, bool = TRUE){

    label <- "Component"

    if(bool)
      label <- paste0("Component for the ", x, "-axis")

    sliderInput(inputId = paste0("comp_", x),
                label = label,
                min = 1, max = input$nb_comp, value = y, step = 1)
  }

  setUiConnection <- function(){
    refresh <- c(input$connection)
    fileInput(inputId = "connection",
              label = "Connection design [OPTIONAL]"
    )  %>%
      shinyInput_label_embed(
        icon("question") %>%
          bs_embed_tooltip(title = "The design matrix is a symmetric matrix of the length of the number of blocks describing the connections between them. Two values are accepted : '1' for a connection between two blocks, or '0' otherwise.")
      )
  }

  setUiResponse <- function(){
    refresh <- c(input$response)
    fileInput(inputId = "response",
              label = "Groups of modalities [OPTIONAL]"
    ) %>% shinyInput_label_embed(
      icon("question") %>%
        bs_embed_tooltip(title = "To color the sample plot. A CSV file containing either : (i) an only column with a qualitative or a quantitative variable; (ii) multiple columns corresponding to a disjunctive table")
    )
  }

  getMinComp = function(){
    # Get the maximum number of component allowed in an analysis based on the minimum
    # number of column among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      if(!is.null(blocks)){
        min = min(sapply(blocks, NCOL))
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
      return( max(sapply(blocks, NCOL)) )
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

  showWarn = function(f, duration = 10, show = TRUE, warn = TRUE){

    ids <- character(0)

    try(withCallingHandlers({
      res <- f
    }, message = function(m) {

      warning(m$message)

        if(show)
        duration <<- NULL

      id <- showNotification(m$message, type = "message", duration = duration)
      ids <<- c(ids, id)

    }, warning = function(w) {
        warning(w$message)
      if(show && warn){
        id <- showNotification(w$message, type = "warning", duration = duration)
        ids <<- c(ids, id)
      }

    }, error = function(e) {
        message(paste("Error:", e$message))
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
                input$scale, input$init, input$comp_x, input$comp_y, input$tau, input$tau_opt, input$analysis_type,
                input$connection, input$nb_comp, input$response, input$names_block_x, input$names_block_y, input$boot, input$text,
                input$names_block_response, input$supervised, input$run_analysis )
  }


  ################################################ Plots  ################################################

  getExtension <- function(f){
    if(!is.null(f)){
      format <- unlist(strsplit(f, '.', fixed="T"))
      return(paste(format[-length(format)], collapse = "."))
    }else
      return(f)
  }

  samples <- function()
    plotSamplesSpace(rgcca = rgcca.res,
                     resp = response,
                     comp_x = comp_x,
                     comp_y = comp_y,
                     i_block = id_block,
                     text = if_text,
                     i_block_y = id_block_y,
                     reponse_name = getExtension(input$response$name))

  corcircle <- function() plotVariablesSpace(rgcca = rgcca.res,
                                             blocks = blocks,
                                             comp_x = comp_x,
                                             comp_y = comp_y,
                                             superblock = (superblock & tolower(analysis_type) != "pca"),
                                             i_block = id_block,
                                             text = if_text)

  fingerprint <- function() plotFingerprint(rgcca = rgcca.res,
                                            blocks = blocks,
                                            comp = comp_x,
                                            superblock = (superblock & tolower(analysis_type) != "pca"),
                                            n_mark = nb_mark,
                                            i_block = id_block)

  ave <- function() plotAVE(rgcca = rgcca.res)

  conNet <- function() plotNetwork2(nodes, edges, blocks)

  plotBoot <- function() plotBootstrap(boot,
                                       comp_x,
                                       nb_mark,
                                       id_block)

  ################################################ Analysis ################################################

  getTau <- function(){
    tau <- integer(0)
    for(i in 1:(length(blocks_without_superb)+ ifelse(input$superblock, 1, 0)))
      tau <- c(tau, input[[paste0("tau", i)]])

    return(tau)
  }

  setParRGCCA <- function(verbose = TRUE){

    blocks = blocks_without_superb
    ncomp = rep(nb_comp, length(blocks))

    if(is.null(analysis_type) | is.null(input$analysis_type))
      analysis_type <- "RGCCA"
    else
      analysis_type <- input$analysis_type

    # Tau is set to optimal by default
    if (input$tau_opt && analysis_type != "SGCCA")
      tau <- "optimal"
    else{
      # otherwise the tau value fixed by the user is used
      tau <- getTau()
    }

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

    if(!is.null(input$supervised) && input$supervised)
      response = input$supervised
    else
      response = NULL

    pars = showWarn(checkSuperblock(list(response = response, superblock = (!is.null(input$supervised) && input$superblock))),
                    show = FALSE)

    if(!is.null(input$supervised) && (input$supervised || tolower(analysis_type) == "ra")){
      pars = setPosPar(list(tau = tau, ncomp = ncomp, superblock = pars$superblock), blocks, id_block_resp)
      blocks = pars$blocks; tau = pars$tau; ncomp = pars$ncomp
    }


    pars = showWarn(select.type(A = blocks, C = NULL, tau = tau,
                                ncomp = ncomp, scheme = input$scheme,
                                superblock = pars$superblock, type  = analysis_type, quiet = TRUE))

    c1 = showWarn(checkC1(pars$blocks, pars$tau, analysis_type))


    if(length(pars) == 1 | !is.null(unlist(c1))){
      assign("analysis", NULL, .GlobalEnv)
      return(NULL)
    }

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

    isolate({
      if(length(grep("[SR]GCCA", analysis_type)) == 1 && !input$tau_opt)
      tau <- getTau()
    })

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

    response <- showWarn(
      setResponse (blocks = blocks_without_superb,
                        file = response_file,
                        sep = input$sep,
                        header = input$header),  warn = FALSE)

    if(length(response) < 1)
      response -> NULL

    return(response)

  }

  setConnectionShiny = function(){

    supervised = (!is.null(input$supervised) && input$supervised)

    if(is.null(connection) | !is.null(connection_file)){
      try(withCallingHandlers(

        connection <- showWarn(setConnection (blocks = blocks,
                    superblock = (is.null(connection_file) && ( superblock  | supervised) ),
                    file = connection_file,
                    sep = input$sep))

      ))

      # Error due to the superblock disabling and the connection have not the same size than the number of blocks
      if( identical(connection, "104") )
        connection <- showWarn(setConnection(blocks = blocks,
                   superblock = ( superblock  | supervised ),
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
  })

  observeEvent(c(input$navbar, input$tabset), {

    toggle(condition = ( input$navbar == "Fingerprint"), id = "nb_mark_custom")
    toggle(condition = ( input$navbar != "Fingerprint"), id = "text")
    toggle(condition = ( input$navbar != "Fingerprint"), id = "comp_y_custom")
    toggle(condition = ( input$navbar == "Samples"), id = "blocks_names_custom_y")
    toggle(condition = ( input$navbar == "Samples"), id = "response")
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
    hide(id = "navbar")
    hide(id = "connection_save")
  })

  onclick("sep", function(e) assign("clickSep", TRUE, .GlobalEnv))

  observeEvent(c(input$blocks, input$sep), {
    # blockExists for having dynamic response to input$blocks
    if(blocksExists()){
      ;
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
    assign("response_file", NULL, .GlobalEnv)
    setUiResponse()

    assign("id_block_resp", length(blocks_without_superb), .GlobalEnv)
    blocks = setParRGCCA(FALSE)
    assign("blocks", blocks, .GlobalEnv)
    assign("connection_file", input$connection$datapath, .GlobalEnv)
    setConnectionShiny()
    setIdBlock()
    hide(id = "navbar")
    updateTabsetPanel(session, "navbar", selected = "Connection")

    return(blocks)
  })

  observeEvent(input$scale, {
    if(blocksExists()){
      assign("blocks_without_superb",
             scaling(blocks_unscaled, input$scale, TRUE),
             .GlobalEnv)
      setAnalysis()
    }
  })

  observeEvent(input$connection, {
    if(blocksExists()){
      assign("connection_file", input$connection$datapath, .GlobalEnv)
      setConnectionShiny()
      setUiConnection()
      showWarn(message("Connection file loaded."), show = FALSE)
      assign("connection_file", NULL, .GlobalEnv)
    }
  })

  observeEvent(input$run_analysis, {

    if(!is.null(getInfile()) & is.matrix(connection)){
      assign("analysis", setRGCCA(), .GlobalEnv)

      show(id = "navbar")

      # for (i in c("bootstrap_save", "fingerprint_save", "corcircle_save", "samples_save", "ave_save"))
      #   setToggleSaveButton(i)
    }
  })

  observeEvent(c(input$superblock, input$supervised, input$nb_comp, input$scheme, input$init, input$tau_opt, input$analysis_type), {
    # Observe if analysis parameters are changed

    if(blocksExists()){
      setNamesInput("x")
      setNamesInput("response")
      assign("nb_comp", input$nb_comp, .GlobalEnv)
      hide(id = "navbar")
      setAnalysis()
      for (i in c("bootstrap_save", "fingerprint_save", "corcircle_save", "samples_save", "ave_save", "connection_save"))
        hide(i)
    }

               if(!input$tau_opt)
                 setTauUI()
  })

  updateSuperblock <- function(id, value)
    updateSelectizeInput(session,
                       inputId = id,
                       choices = value,
                       selected = value,
                       server = TRUE)

  observeEvent(input$supervised, {
    if(input$supervised)
      updateSuperblock("superblock", FALSE)
    else
      updateSuperblock("superblock", TRUE)
  })

  observeEvent(input$superblock, {
    if(input$superblock)
      updateSuperblock("supervised", FALSE)
    else
      updateSuperblock("supervised", TRUE)
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
      msgSave()
    }
  })

  msgSave = function()
    showWarn(message(paste("Save in", getwd())), show = FALSE)

  observeEvent(c(input$text, input$comp_x, input$comp_y, input$nb_mark), {
    if(!is.null(analysis)){
      assign("if_text", input$text, .GlobalEnv)
      assign("comp_x", input$comp_x, .GlobalEnv)
      assign("comp_y", input$comp_y, .GlobalEnv)
      if(!is.null(input$nb_mark))
        assign("nb_mark", input$nb_mark, .GlobalEnv)
    }
  })

  setResponseEvent <- eventReactive(input$response, {

    assign("response_file", input$response$datapath, .GlobalEnv)
    assign("response", setResponseShiny(), .GlobalEnv)
    setUiResponse()
    showWarn(message(paste0(input$response$name, " loaded as a group file.")), show = FALSE)
  })

  ################################################ Outputs ################################################

  output$samplesPlot <- renderPlotly({

    getDynamicVariables()

    if(!is.null(analysis)){

      observeEvent(input$samples_save, {
        savePlot("samples_plot.pdf", samples())
        msgSave()
      })

      assign("response", setResponseShiny(), .GlobalEnv)
      setUiResponse()

      if(!is.null(input$response))
        setResponseEvent()

      p = showWarn(changeHovertext( dynamicPlot(samples(), ax, "text", TRUE, TRUE), if_text ))
      if( length(unique(na.omit(response))) < 2 || (length(unique(na.omit(response))) > 5 && !unique(isCharacter(na.omit(response))) ))
        p  = p  %>% layout(showlegend = FALSE)
      p

    }
  })

  output$corcirclePlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis)){
      observeEvent(input$corcircle_save, {
        savePlot("corcircle.pdf", corcircle())
        msgSave()
        })
      p = changeHovertext( dynamicPlot(corcircle(), ax, "text"), if_text )
      n = length(p$x$data)
      ( style(p, hoverinfo = "none", traces = c(n, n-1)) )
    }
  })

  output$fingerprintPlot <- renderPlotly({
    getDynamicVariables()
    if(!is.null(analysis)){
      observeEvent(input$fingerprint_save, {
        savePlot("fingerprint.pdf", fingerprint())
        msgSave()
        })
      p = changeText ( dynamicPlot(fingerprint(), ax2, "text") )
      n = sapply(p$x$data, function(x) !is.null(x$orientation))
      for (i in 1:length(n[n]))
        p$x$data[[i]]$text = round( as.double(sub( "order: .*<br />df\\[, 1\\]: (.*)<.*", "\\1\\", p$x$data[[i]]$text )), 3)
      p
    }
  })

  output$AVEPlot <- renderPlot({
    getDynamicVariables()
    if(!is.null(analysis)){
      observeEvent(input$ave_save, {
        savePlot("AVE.pdf", ave())
        msgSave()
        })
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
      observeEvent(input$bootstrap_save, {
        savePlot("bootstrap.pdf", plotBoot())
        msgSave()
        })
      dynamicPlotBoot(plotBoot())
    }
  })

}
