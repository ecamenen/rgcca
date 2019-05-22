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
    make_option(c("-d", "--datasets"), type="character", metavar="character", help="List of the paths for each block file separated by comma (without space between)", default = opt[18]),
    make_option(c("-w", "--directory"), type="character", metavar="character", help="Path of the scripts directory (for Galaxy)", default=opt[1]),
    make_option(c("-c", "--connection"), type="character", metavar="character", help="Path of the connection file"),
    make_option(c("--group"), type="character",
                help="Path of the group file (to color samples by group in the associated plot)"),
    make_option(c("-r", "--response"), type="integer", metavar="integer",
                help="Position of the response file in datasets (if not null, activate supervized method)"),
    make_option(c("--names"), type="character", metavar="character", help="List of the names for each block file separated by comma [default: filename]"),
    make_option(c("-H", "--header"), type="logical", action="store_false", help="DO NOT consider the first row as header of columns"),
    make_option(c("--separator"), type="integer", metavar="integer", default=1,
                help="Character used to separate the columns (1: Tabulation, 2: Semicolon, 3: Comma) [default: tabulation]"),
    make_option(c("--type"), type="character", metavar="character", default=opt[3],
                help="Type of analysis to use (by default, 1 for RGCCA) [1: rgcca, 2: pca, 3: cca, 4: gcca, cpca-w, hpca, maxbet-b, maxbet, maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar, rcon-pca, ridge-gca, sabscor, ssqcor, ssqcor, ssqcov-1, ssqcov-2, ssqcov, sum-pca, sumcor, sumcov-1, sumcov-2, sumcov]"),
    make_option(c("--tau"), type="character", metavar="float", default=opt[5],
                help="Tau parameter for RGCCA, a float between 0 (maximize the covariance) and 1 (maximize the correlation between blocks). Could also be a list separated by comma. Ex: 0,1,0.75,1"),
    make_option(c("--scheme"), type="integer", metavar="integer", default=2,
                help="Scheme function g(x) for RGCCA (1: x, 2: x^2, 3: |x|, 4: x^4) [default: x^2]"),
    make_option(c("--scale"),  type="logical", action="store_false",
                help="DO NOT scale the blocks (i.e., standardize each block to zero mean and unit variances and then divide them by the square root of its number of variables)"),
    make_option(c("--superblock"),  type="logical", action="store_false",
                help="DO NOT use a superblock (a concatenation of all the blocks to better interpret the results)"),
    make_option(c("--init"),  type="integer", metavar="integer", default=1,
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

# Check the validity of the arguments
# opt : an optionParser object
checkArg = function(opt){

  if(is.null(opt$datasets))
    stop(paste0("--datasets is required."), exit_code = 121)

  if (is.null(opt$scheme))
    opt$scheme = "factorial"
  else if ((opt$scheme < 1) || (opt$scheme > 4)){
    stop("--scheme must be comprise between 1 and 4 [by default: 2].", exit_code = 122)
  }else{
    schemes = c("horst", "factorial", "centroid")
    if (opt$scheme == 4)
      opt$scheme = function(x) x^4
    else
      opt$scheme = schemes[opt$scheme]
  }

  if ((opt$separator < 1) || (opt$separator > 3)){
    stop("--separator must be comprise between 1 and 2 (1: Tabulation, 2: Semicolon, 3: Comma) [by default: 2].", exit_code = 123)
  }else{
    separators = c('\t', ';', ',')
    opt$separator = separators[opt$separator]
  }

  if ((opt$init < 1) || (opt$init > 2)){
    stop("--init must be comprise between 1 and 2 (1: Singular Value Decompostion , 2: random) [by default: SVD].", exit_code = 124)
  }else{
    opt$init = ifelse(opt$init == 1, "svd", "random")
  }

  FILES = c("connection", "group")
  for (o in FILES)
    if(!is.null(opt[[o]]))
      checkFile(opt[[o]])

  return (opt)
}

# Check the validity of the arguments after loading the blocks
# opt : an optionParser object
# blocks : a list of matrix
postCheckArg = function(opt, blocks){
  opt = select.type(blocks, opt)

  if(opt$superblock | opt$type == "pca")
    blocks = c(blocks, list(Reduce(cbind, blocks)))

  opt$ncomp = as.list(opt$ncomp)

  out = lapply(1:length(opt$ncomp), function(x){
    if ((opt$ncomp[x] < 1) || (opt$ncomp[x] > ncol(blocks[[x]]))){
      stop(paste0("--ncomp must be comprise between 1 and ", ncol(blocks[[x]]) ,", the number of variables of the block (currently equals to ", opt$ncomp[x]  ,")"), exit_code = 126)
    }
  })

  if(length(opt$ncomp) == 1)
    opt$ncomp = rep(opt$ncomp[[1]], length(blocks))
  else
    if(length(opt$ncomp) != length(blocks))
      stop(paste0("--ncomp list must have the same size (", length(opt$ncomp), ") than the the number of blocks (", length(blocks), ")."), exit_code = 127)
    else
      opt$ncomp = unlist(opt$ncomp)

  out = sapply(c("compx", "compy"), function (x){
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
        if(length(list_tau) != length(blocks))
          stop(paste0("--tau list must have the same size (", length(list_tau), ") than the the number of blocks (", length(blocks), ")."), exit_code = 130)
        else
          opt$tau = unlist(list_tau)

    }, warning = function(w) {
      stop(MSG, exit_code = 131)
    })
  }else{
    opt$tau = "optimal"
  }

  # c1 : A vector of integer giving the spasity parameter for SGCCA (c1)
  # Stop the program if at least one c1 parameter is not in the required interval

  checkC1(blocks, opt$tau, opt$type)


  if(opt$block > length(blocks))
    stop(paste0("--block must be lower than ", length(blocks), " (the maximum number of blocks)."), exit_code = 133)
  else if(opt$block == 0)
    opt$block = length(blocks)

  return (opt)
}

########## Main ##########

# Pre-requisite: for xlsx inputs, java must be installed
# Under linux: sudo apt-get install default-jre default-jdk && sudo R CMD javareconf

#Loading librairies
#suppressPackageStartupMessages(expr)
librairies = c("RGCCA", "ggplot2", "optparse", "scales", "plotly", "visNetwork", "igraph", "ggrepel", "parallel", "xlsx")
for (l in librairies) {
  if (!(l %in% installed.packages()[, "Package"]))
    install.packages(l, repos = "http://cran.us.r-project.org",
                     warn.conflicts = FALSE,
                     quiet = TRUE)
  library(l, character.only = TRUE,
          warn.conflicts = FALSE,
          quiet = TRUE)
}

# Get arguments : R packaging install, need an opt variable with associated arguments
opt = list(directory = ".",
           separator = "\t",
           type = "rgcca",
           scheme = "factorial",
           tau = "0.71, 0.71, 0.71",
           init = "svd",
           ncomp = "2, 2, 2",
           block = 0,
           compx = 1,
           compy = 2,
           nmark = 50,
           output1 = "samples_plot.pdf",
           output2 = "corcircle.pdf",
           output3 = "fingerprint.pdf",
           output4 = "ave.pdf",
           output5 = "correlation.pdf",
           output6 = "connection.pdf",
           datasets = "/home/etienne.camenen/bin/rgccaLauncher/inst/extdata/agriculture.tsv, /home/etienne.camenen/bin/rgccaLauncher/inst/extdata/industry.tsv, /home/etienne.camenen/bin/rgccaLauncher/inst/extdata/politic.tsv")

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

#opt$group = "/home/etienne.camenen/Documents/DATA/Nucleiparks/UPDRS_2.tsv"
#group2 = "/home/etienne.camenen/Documents/DATA/ZEUS/DATA/AMY_Staging_MA_quant.txt"
opt$group = "/home/etienne.camenen/bin/rgccaLauncher/inst/extdata/response.tsv"

group = setResponse(blocks, opt$group, opt$separator, opt$header)

rgcca.out = rgcca.analyze(blocks, connection, opt$tau, opt$ncomp, opt$scheme, FALSE, opt$init, opt$bias, opt$type)

########## Plot ##########

# Samples common space
if(opt$ncomp[opt$block] == 1 && is.null(opt$block_y)){
   warning("With a number of component of 1, a second block should be chosen to perform a samples plot")
}else{
  ( samples_plot = plotSamplesSpace(rgcca.out, group, opt$compx, opt$compy, opt$block, opt$text, opt$block_y, getFileName(opt$group)) )
   p = changeHovertext( dynamicPlot(samples_plot, ax, "text", TRUE, TRUE), opt$text )
     if( length(unique(na.omit(group))) < 2 || (length(unique(na.omit(group))) > 5 && !unique(isCharacter(na.omit(group))) ))
       p  = p  %>% layout(showlegend = FALSE)
   p
   savePlot(opt$output1, samples_plot)
}

if(opt$ncomp[opt$block] > 1){
  # Variables common space
  ( corcircle = plotVariablesSpace(rgcca.out, blocks, opt$compx, opt$compy, opt$superblock, opt$block, opt$text) )
  p = changeHovertext( dynamicPlot(corcircle, ax, "text"), opt$text)
  n = length(p$x$data)
  ( style(p, hoverinfo = "none", traces = c(n, n-1)) )
  savePlot(opt$output2, corcircle)
}

# Fingerprint plot
(  fingerprint = plotFingerprint(rgcca.out, blocks, opt$compx, opt$superblock, 3) )
plotFingerprint(rgcca.out, blocks, opt$compy, opt$superblock, opt$nmark)
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

boot = bootstrap(blocks, 5, connection, opt$tau, opt$ncomp, opt$scheme, opt$scale, opt$init, opt$bias, opt$type)
dynamicPlotBoot(plotBootstrap(boot, opt$compx, opt$nmark, opt$block))
