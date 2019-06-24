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

rm(list=ls())
graphics.off()

########## Arguments ##########

# Parse the arguments from a command line launch
getArgs = function(){
  option_list = list(
    make_option(c("-d", "--datasets"), type="character", metavar="character", help="List of the paths for each block file separated by comma (without space between)",
                default = "inst/extdata/agriculture.tsv,inst/extdata/industry.tsv,inst/extdata/politic.tsv"),
    make_option(c("-w", "--directory"), type="character", metavar="character", help="Path of the scripts directory (for Galaxy)", default=opt[1]),
    make_option(c("-c", "--connection"), type="character", metavar="character", help="Path of the connection file"),
    make_option(c("--group"), type="character", metavar="character",
                help="Path of the group file (to color samples by group in the associated plot)"),
    make_option(c("-r", "--response"), type="integer", metavar="integer",
                help="Position of the response file in datasets (if not null, activate supervized method)"),
    make_option(c("--names"), type="character", metavar="character", help="List of the names for each block file separated by comma [default: filename]"),
    make_option(c("-H", "--header"), type="logical", action="store_false", help="DO NOT consider the first row as header of columns"),
    make_option(c("--separator"), type="integer", metavar="integer", default=opt[2],
                help="Character used to separate the columns (1: Tabulation, 2: Semicolon, 3: Comma) [default: tabulation]"),
    make_option(c("--type"), type="character", metavar="character", default=opt[3],
                help="Type of analysis to use (by default, 1 for RGCCA) [1: rgcca, 2: pca, 3: cca, 4: gcca, cpca-w, hpca, maxbet-b, maxbet, maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar, rcon-pca, ridge-gca, sabscor, ssqcor, ssqcor, ssqcov-1, ssqcov-2, ssqcov, sum-pca, sumcor, sumcov-1, sumcov-2, sumcov]"),
    make_option(c("--tau"), type="character", metavar="float", default=opt[5],
                help="Tau parameter for RGCCA, a float between 0 (maximize the covariance) and 1 (maximize the correlation between blocks). Could also be a list separated by comma. Ex: 0,1,0.75,1"),
    make_option(c("--scheme"), type="integer", metavar="integer", default=opt[4],
                help="Scheme function g(x) for RGCCA (1: x, 2: x^2, 3: |x|, 4: x^4) [default: x^2]"),
    make_option(c("--scale"),  type="logical", action="store_false",
                help="DO NOT scale the blocks (i.e., standardize each block to zero mean and unit variances and then divide them by the square root of its number of variables)"),
    make_option(c("--superblock"),  type="logical", action="store_false",
                help="DO NOT use a superblock (a concatenation of all the blocks to better interpret the results)"),
    make_option(c("--init"),  type="integer", metavar="integer", default=opt[6],
                help="Initialization mode for RGCCA (1: Singular Value Decompostion , 2: random) [default: SVD]"),
    make_option(c("--bias"),  type="logical", action="store_false",
                help="Unbiased estimator of the variance"),
    make_option(c("--text"),  type="logical", action="store_false", help="Print text when plotting points"),
    make_option(c("--ncomp"),  type="character", metavar="integer", default=opt[7],
                help="Number of components in the analysis for each block (should be greater than 1 and lower than the minimum number of variable among the blocks). Could also be a list separated by comma. Ex: 2,2,3,2."),
    make_option(c("--block"),  type="integer", metavar="integer", default=opt[8],
                help="Number of the block shown in the graphics (0: the superblock or, if not, the last, 1: the fist one, 2: the 2nd, etc.) [default: the last one]"),
    make_option(c("--block_y"),  type="integer", metavar="integer",
                help="Block shown in the Y-axis of the samples plot (0: the superblock or, if not, the last, 1: the fist one, 2: the 2nd, etc.) [default: the last one]"),
    make_option(c("--compx"),  type="integer", metavar="integer", default=opt[9],
                help="Component used in the X-axis for biplots and the only component used for histograms (should not be greater than the --ncomp parameter)"),
    make_option(c("--compy"),  type="integer", metavar="integer", default=opt[10],
                help="Component used in the Y-axis for biplots (should not be greater than the --ncomp parameter)"),
    make_option(c("--nmark"),  type="integer", metavar="integer", default=opt[11],
                help="Number maximum of biomarkers in the fingerprint"),
    make_option(c( "--output1"), type="character", metavar="character", default=opt[12],
                help="Variables space file name [default: %default]"),
    make_option(c( "--output2"), type="character", metavar="character", default=opt[13],
                help="Sample space file name [default: %default]"),
    make_option(c( "--output3"), type="character", metavar="character", default=opt[14],
                help="Best fingerprint file name [default: %default]"),
    make_option(c( "--output4"), type="character", metavar="character", default=opt[15],
                help="AVE plot file name [default: %default]"),
    make_option(c( "--output5"), type="character", metavar="character", default=opt[16],
                help="Correlation with response plot file name [default: %default]"),
    make_option(c( "--output6"), type="character", metavar="character", default=opt[17],
                help="Connection plot file name [default: %default]")
  )
  args = commandArgs(trailingOnly=T)
  return (OptionParser(option_list=option_list))
}

checkFile = function (f){
  # Check the existence of a path
  # f: A character giving the path of a file

  if(!file.exists(f)){
    stop(paste0(f, " file does not exist."), exit_code = 120)
  }
}

checkArg = function(opt){
  # Check the validity of the arguments
  # opt : an optionParser object

  if(is.null(opt$datasets))
    stop(paste0("--datasets is required."), exit_code = 121)

  if (is.null(opt$scheme))
    opt$scheme = "factorial"
  else if ( ! opt$scheme %in% 1:4 ){
    stop(paste0("--scheme must be comprise between 1 and 4 [by default: 2], not be equals to ", opt$scheme, "."), exit_code = 122)
  }else{
    schemes = c("horst", "factorial", "centroid")
    if (opt$scheme == 4)
      opt$scheme = function(x) x^4
    else
      opt$scheme = schemes[opt$scheme]
  }

  if (! opt$separator %in% 1:3 ){
    stop(paste0("--separator must be comprise between 1 and 3 (1: Tabulation, 2: Semicolon, 3: Comma) [by default: 2], not be equals to ", opt$separator, "."), exit_code = 123)
  }else{
    separators = c('\t', ';', ',')
    opt$separator = separators[opt$separator]
  }

  if (! opt$init %in% 1:2 )
    stop(paste0("--init must be 1 or 2 (1: Singular Value Decompostion , 2: random) [by default: 1], not ", opt$init, "."), exit_code = 124)
  else
    opt$init = ifelse(opt$init == 1, "svd", "random")


  FILES = c("connection", "group")
  for (o in FILES)
    if(!is.null(opt[[o]]))
      checkFile(opt[[o]])

  checkInteger("nmark")
  if(opt$nmark < 2)
    stop(paste0("--nmark must be upper than 2, not be equals to ", opt$nmark, "."), exit_code = 135)

  return (opt)
}

checkArgSize <- function(blocks, x, y){
  if(length(x) != length(blocks))
    stop(paste0("--", y, " list must have the same size (", length(x), ") than the the number of blocks (", length(blocks), ")."), exit_code = 130)
  else
    return(TRUE)
}

postCheckArg = function(opt, blocks){
  # Check the validity of the arguments after loading the blocks
  # opt : an optionParser object
  # blocks : a list of matrix

  opt = select.type(blocks, opt)

  if(opt$superblock | opt$type == "pca")
    blocks = c(blocks, list(Reduce(cbind, blocks)))

  out = lapply(1:length(opt$ncomp), function(x){
    checkInteger("ncomp", opt$ncomp[x])
    if ((opt$ncomp[x] < 1) || (opt$ncomp[x] > ncol(blocks[[x]]))){
      stop(paste0("--ncomp must be comprise between 1 and ", ncol(blocks[[x]]) ,", the number of variables of the block (currently equals to ", opt$ncomp[x]  ,")"), exit_code = 126)
    }
  })

  checkArgSize(blocks, opt$ncomp, "ncomp")

  out = sapply(c("compx", "compy"), function (x){
    checkInteger(x)
    if ((opt[[x]] < 1) || (opt[[x]] > opt$ncomp )){
      stop(paste0("--", x, " must be comprise between 1 and ", opt$ncomp ," (the number of component selected)."), exit_code = 128)
    }
  })

  MSG = "--tau must be comprise between 0 and 1 or must correspond to the character 'optimal' for automatic setting."
  if(all(opt$tau!="optimal")){
    tryCatch({

      list_tau = as.list(opt$tau)
      # Check value of each tau
      out = lapply(list_tau, function(x){
        if(((x < 0) || (x > 1)) && x != "optimal")
          stop(MSG, exit_code = 129)
      })

      # If there is only one common tau
      if(length(list_tau) == 1)
        opt$tau = rep(list_tau[[1]], length(blocks))
      else
        if(checkArgSize(blocks, list_tau, "tau"))
          opt$tau = unlist(list_tau)

    }, warning = function(w) {
      stop(MSG, exit_code = 131)
    })
  }else{
    opt$tau = "optimal"
  }

  checkC1(blocks, opt$tau, opt$type)

  if(!is.null(opt$names))
    checkArgSize(blocks, strsplit(gsub(" ", "", opt$names), ",")[[1]], "names")

  for (x in c("block", "block_y", "response")){
    if(!is.null(opt[[x]])){
      if(opt[[x]] > length(blocks))
        stop(paste0("--", x, " must be lower than ", length(blocks), " (the maximum number of blocks)."), exit_code = 133)
      else if(opt$block == 0)
        opt[[x]] = length(blocks)
      else if(opt[[x]] < 0 )
        stop(paste0("--", x, " must be positive."), exit_code = 134)
      checkInteger(x)
    }
  }

  return (opt)
}

checkInteger <- function(x, y = NULL){
  # Test either x is an integer
  # x : a string corresponding to a name in a list "opt"

  if(is.null(y))
    y = opt[[x]]

  # Test if not a character
  tryCatch({
    as.integer(y)
  }, warning = function(w) {
    stop(paste0("--", x, " is a character (", y, ") and must be an integer."))
  })

  # Test if not a float

  if(length(strsplit(as.character(y), ".", fixed=T)[[1]]) > 1)
    stop(paste0("--", x, " is a float (", y, ") and must be an integer."))

}

########## Main ##########

# Libraries loading
librairies = c("RGCCA", "ggplot2", "optparse", "scales", "plotly", "visNetwork", "igraph")
for (l in librairies) {
  if (!(l %in% installed.packages()[, "Package"]))
    install.packages(l, repos = "http://cran.us.r-project.org")
  library(l, character.only = TRUE,
          warn.conflicts = FALSE,
          quiet = TRUE)
}

# Get arguments : R packaging install, need an opt variable with associated arguments
opt = list(directory = ".",
           separator = 1,
           type = "rgcca",
           scheme = 2,
           tau = "optimal",
           init = 1,
           ncomp = 2,
           block = 0,
           compx = 1,
           compy = 2,
           nmark = 100,
           output1 = "samples_plot.pdf",
           output2 = "corcircle.pdf",
           output3 = "fingerprint.pdf",
           output4 = "ave.pdf",
           output5 = "correlation.pdf",
           output6 = "connection.pdf")

tryCatch({
  opt = parse_args(getArgs())
  opt = checkArg(opt)
}, error = function(e) {
  if (length(grep("nextArg", e[[1]])) != 1)
    stop(e[[1]], exit_code = 140)
})

setwd(opt$directory)
source("R/parsing.R")
source("R/select.type.R")
source("R/plot.R")
source("R/network.R")

# Global settings
opt$header = !("header" %in% names(opt))
opt$superblock = !("superblock" %in% names(opt))
opt$bias = !("bias" %in% names(opt))
opt$scale = !("scale" %in% names(opt))
opt$text = !("text" %in% names(opt))

blocks = setBlocks(opt$datasets, opt$names, opt$separator, opt$header)
blocks = scaling(blocks, opt$scale, opt$bias)

if(length(grep(",", opt$ncomp)) == 0)
  opt$ncomp = paste(rep(opt$ncomp, length(blocks)), collapse = ",")

opt = checkSuperblock(opt)
opt = postCheckArg(opt, blocks)

if( ! is.null(opt$response) ){
  opt = setPosPar(opt, blocks, opt$response)
  blocks = opt$blocks
}

blocks = setSuperblock(blocks, opt$superblock, opt$type)

connection = opt$connection
if(!is.matrix(connection))
  connection = setConnection(blocks, (opt$superblock | !is.null(opt$response)), opt$connection, opt$separator)

group = setResponse(blocks, opt$group, opt$separator, opt$header)

rgcca.out = rgcca.analyze(blocks, connection, opt$tau, opt$ncomp, opt$scheme, FALSE, opt$init, opt$bias, opt$type)

########## Plot ##########

# Samples common space
if(opt$ncomp[opt$block] == 1 && is.null(opt$block_y)){
   warning("With a number of component of 1, a second block should be chosen to perform a samples plot")
}else{
  ( samples_plot = plotSamplesSpace(rgcca.out, group, opt$compx, opt$compy, opt$block, opt$text, opt$block_y, getFileName(opt$group)) )
   p = changeHovertext( dynamicPlot(samples_plot, ax, "text", TRUE, TRUE), opt$text )
     if( length(unique(na.omit(group))) < 2 || (length(unique(group)) > 5 && !unique(isCharacter(group)) ))
       p  = p  %>% layout(showlegend = FALSE)
   p
   savePlot(opt$output1, samples_plot)
}

if(opt$ncomp[opt$block] > 1){
  # Variables common space
  ( corcircle = plotVariablesSpace(rgcca.out, blocks, opt$compx, opt$compy, opt$superblock, opt$block, opt$text, n_mark = opt$nmark) )
  p = changeHovertext( dynamicPlot(corcircle, ax, "text"), opt$text)
  n = length(p$x$data)
  ( style(p, hoverinfo = "none", traces = c(n, n-1)) )
  savePlot(opt$output2, corcircle)
}

# Fingerprint plot
fingerprint = plotFingerprint(rgcca.out, blocks, opt$compx, opt$superblock, opt$nmark)
p = changeText ( dynamicPlot(fingerprint, ax2, "text") )
n = unlist(lapply(p$x$data, function(x) !is.null(x$orientation)))
for (i in 1:length(n[n]))
  p$x$data[[i]]$text = round( as.double(sub( "order: .*<br />df\\[, 1\\]: (.*)<.*", "\\1\\", p$x$data[[i]]$text )), 3)
p
# TODO: avoid the scale, zoom in, zoom out, make stop unexpectivly
savePlot(opt$output3, fingerprint)

if( ! is.null(opt$response) ){
  ( correlation = corResponse(rgcca.out, blocks, opt$response, comp = opt$compx, i_block = opt$block) )
  savePlot(opt$output5, correlation)
}

if(opt$type != "pca"){

  # Average Variance Explained
  (ave = plotAVE(rgcca.out))
  savePlot(opt$output4, ave)

  # Creates design scheme
  nodes <- getNodes(blocks, rgcca = rgcca.out)
  edges <- getEdges(connection, blocks)
  conNet <- function() plotNetwork(nodes, edges, blocks)
  plotNetwork2(nodes, edges, blocks)
  savePlot(opt$output6, conNet)

}

saveVars(rgcca.out, blocks, 1, 2)
saveInds(rgcca.out, blocks, 1, 2)
save(rgcca.out, file = "rgcca.result.RData")
