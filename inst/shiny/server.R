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
  source("../../R/parsing.R")
  source("../../R/plot.R")

  # Libraries loading
  librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx", "shinyjs")
  for (l in librairies) {
    if (!(l %in% installed.packages()[, "Package"]))
      install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
    library(l, character.only = TRUE)
  }

  # Assign reactive variables
  assign("i_block", reactiveVal(), .GlobalEnv)
  assign("id_block", NULL, .GlobalEnv)
  assign("n_comp", reactiveVal(), .GlobalEnv)
  assign("click", FALSE, .GlobalEnv)

  #TODO: remove blocks, superblock from observeEvent
  output$blocks_names_custom <- renderUI({
    # Define the names of the blocks and set by default on the last block

    if(!is.null(input$blocks)){
      # Refresh the function when superblock option is changed
      refesh = input$superblock
      # Get the blocks dynamically
      blocks = getInfile()
      # Set selected value on the last block
      n <- round(length(blocks))
    }else{
      # If any dataset is selected
      n <- 1
    }

    selectInput(inputId = "names_block",
                label = h5("Block selected: "),
                choices = getNames(), selected = n)
  })


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
    # Get the maximum number of component allowed in an analysis based on the minimum
    # number of column among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( min(unlist(lapply(blocks, NCOL))) )
    }else{
      return(2)
    }
  }

  getNames = function(){
    # Get the names of the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      # Creates a list of nb_blocks dimension, each one containing a id from 1 to nb_blocks and having the same names as the blocks
      return( as.list(sapply(names(blocks), function(i) as.integer(which(names(blocks) == i)), USE.NAMES = TRUE)) )
    }else{
      return(list(" " = 0))
    }
  }


  getMaxCol = function(){
    # Get the maximum number of columns among the blocks

    if(!is.null(input$blocks)){
      blocks = getInfile()
      return( max(unlist(lapply(blocks, NCOL))) )
    }else{
      return(100)
    }
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


  getNbComp = function(){
    refresh <- input$nb_comp
    return(nb_comp)
  }

  getDynamicVariables <- reactive({
    # Refresh all the plots when any input is changed

    refresh = c(input$sep, input$header, input$blocks, input$superblock, input$connection,  input$scheme, input$nb_mark,
                 input$scale, input$bias, input$init, input$axis1, input$axis2, input$response, input$tau, input$tau_opt,
                input$connection, input$nb_comp, input$adv_pars, input$adv_ana, input$adv_graph, input$names_block )
  })

  getInfile <- eventReactive(c(input$blocks, input$superblock, input$sep), {
    # Return the list of blocks

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
      assign("blocks", NULL, .GlobalEnv)
      if(click)
        message(e$message)
      else
        message(paste("COUCOU", e$message))
    })
    assign("click", FALSE, .GlobalEnv)
    return(blocks)
  })

  onclick("sep", function(e) assign("click", TRUE, .GlobalEnv))

  setData <- reactive({
    # Load the blocks, the response and the connection matrix
    refresh = c(input$superblock, input$blocks)
    # This function activated only when a new dataset is loaded, set the reponse
    # and connection of the previous dataset to NULL
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

    # Tau is set to optimal by default
    if (input$tau_opt)
      tau = input$tau_opt
    else
      # otherwise the tau value fixed by the user is used
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

  blocksExists = function(){
    # Test if the blocks are loaded and contain any errors

    if(!is.null(input$blocks))
      if(!is.null(getInfile()))
        return(TRUE)
    return(FALSE)
  }

  ################################################ Observe events ################################################

  observeEvent(c(input$superblock, input$blocks, input$sep), {
    # Observe the changes for parsing functionnalities (column separator,
    # the header, the path for the blocks and the presence of a superblock)

    if(blocksExists()){

      # Update the id_block (the block used for visualization) when superblock option is disabled
      if(!input$superblock && as.integer(input$names_block) > round(length(getInfile())) ){
        i_block(as.integer(input$names_block))
        assign("id_block", i_block() - 1, .GlobalEnv)
      }else{
        # By default, when a new dataset is loaded, the selected block is the last
        assign("id_block", length(getInfile()), .GlobalEnv)
      }

      setData()
      # By default, the number of component is set to 2
      assign("nb_comp", 2, .GlobalEnv)
      setAnalysis()
      setFuncs()
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
      connection = setConnection (blocks = blocks,
                                          file = input$connection$datapath,
                                          sep = input$sep)
      assign("connection", connection,
            .GlobalEnv)
      setAnalysis()
      setFuncs()
      }, error = function(e) {
        message(e$message)
      })
    }
  })

  observeEvent(c(input$nb_comp, input$scheme, input$scale, input$bias, input$init, input$tau, input$tau_opt), {
    # Observe if analysis parameters are changed
    if(blocksExists()){
      assign("nb_comp", input$nb_comp, .GlobalEnv)
      setAnalysis()
      setFuncs()
    }
  })

  observeEvent(c(input$names_block, input$nb_mark, input$axis1, input$axis2, id_block), {
    # Observe if graphical parameters are changed
    if(blocksExists()){
      i_block(as.integer(input$names_block))
      assign("id_block", i_block(), .GlobalEnv)
      setFuncs()
    }
  })

#TODO : Duplicates rows are not allowed

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

}
