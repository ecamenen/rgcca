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

##################
#     Arguments
##################

# Parse the arguments from a command line launch
getArgs = function(){
  option_list = list(
    make_option(c("-d", "--datasets"), type="character", metavar="character", help="List of the paths for each block file separated by comma (without space between)", default = opt[18]),
    make_option(c("-w", "--directory"), type="character", metavar="character", help="Path of the scripts directory (for Galaxy)", default=opt[1]),
    make_option(c("-c", "--connection"), type="character", metavar="character", help="Path of the connection file"),
    make_option(c("--group"), type="character", metavar="character",
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
    stop(paste(f, " file does not exist\n", sep=""), call.=FALSE)
  }
}

# Check the validity of the arguments
# opt : an optionParser object
checkArg = function(opt){

  if(is.null(opt$datasets))
    stop(paste("--datasets is required\n", sep=""), call.=FALSE)

  if (is.null(opt$scheme))
    opt$scheme = "factorial"
  else if ((opt$scheme < 1) || (opt$scheme > 4)){
    stop("--scheme must be comprise between 1 and 4 [by default: 2].\n", call.=FALSE)
  }else{
    schemes = c("horst", "factorial", "centroid")
    if (opt$scheme == 4)
      opt$scheme = function(x) x^4
    else
      opt$scheme = schemes[opt$scheme]
  }

  if ((opt$separator < 1) || (opt$separator > 3)){
    stop("--separator must be comprise between 1 and 2 (1: Tabulation, 2: Semicolon, 3: Comma) [by default: 2].\n", call.=FALSE)
  }else{
    separators = c('\t', ';', ',')
    opt$separator = separators[opt$separator]
  }

  if ((opt$init < 1) || (opt$init > 2)){
    stop("--init must be comprise between 1 and 2 (1: Singular Value Decompostion , 2: random) [by default: SVD].\n", call.=FALSE)
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
  opt = select.type(opt, blocks)

  if(opt$superblock | opt$type == "pca")
    blocks = c(blocks, list(Reduce(cbind, blocks)))

  opt$ncomp = as.list(opt$ncomp)

  out = lapply(1:length(opt$ncomp), function(x){
    if ((opt$ncomp[x] < 1) || (opt$ncomp[x] > ncol(blocks[[x]]))){
      stop("--ncomp must be comprise between 1 and ", ncol(blocks[[x]]) ,", the number of variables of the block (currently equals to ", opt$ncomp[x]  ,").\n", call.=FALSE)
    }
  })

  if(length(opt$ncomp) == 1)
    opt$ncomp = rep(opt$ncomp[[1]], length(blocks))
  else
    if(length(opt$ncomp) != length(blocks))
      stop(paste("--ncomp list must have the same size (", length(opt$ncomp), ") than the the number of blocks (", length(blocks), ").\n", sep=""), call.=FALSE)
    else
      opt$ncomp = unlist(opt$ncomp)

  out = sapply(c("compx", "compy"), function (x){
    if ((opt[[x]] < 1) || (opt[[x]] > opt$ncomp )){
      stop(paste("--", x, " must be comprise between 1 and ", opt$ncomp ," (the number of component selected).\n ", sep=""), call.=FALSE)
    }
  })

  MSG = "--tau must be comprise between 0 and 1 or must correspond to the character 'optimal' for automatic setting.\n"
  if(all(opt$tau!="optimal")){
    tryCatch({

      list_tau = as.list(opt$tau)
      # Check value of each tau
      out = lapply(list_tau, function(x){
        if(((x < 0) || (x > 1)) && x != "optimal")
          stop(MSG, call.=FALSE)
      })

      # If there is only one common tau
      if(length(list_tau) == 1)
        opt$tau = rep(list_tau[[1]], length(blocks))
      else
        if(length(list_tau) != length(blocks))
          stop(paste("--tau list must have the same size (", length(list_tau), ") than the the number of blocks (", length(blocks), ").\n", sep=""), call.=FALSE)
        else
          opt$tau = unlist(list_tau)

    }, warning = function(w) {
      stop(MSG, call.=FALSE)
    })
  }else{
    opt$tau = "optimal"
  }

  # c1 : A vector of integer giving the spasity parameter for SGCCA (c1)
  # Stop the program if at least one c1 parameter is not in the required interval

  if(opt$type == "sgcca"){
      #the minimum value avalaible
      min_c1 = lapply(blocks, function(x) 1 / sqrt(ncol(x)))

      # Check c1 varying between 1/sqrt(pj) and 1
      out = mapply(function(x, y){
        if(x < y | x > 1)
          stop(paste("Sparsity parameter is equals to ", x,
                     ". For SGCCA, it must be comprise between 1/sqrt(number_column) (i.e., ",
                     toString(unlist(lapply(min_c1, function(x) ceiling(x * 100) / 100)))
                     , ") and 1.\n", sep=""),
               call. = FALSE)
      }, opt$tau, min_c1)
  }


  if(opt$block > length(blocks))
    stop(paste("--block must be lower than ", length(blocks), " (the maximum number of blocks).\n", sep=""), call.=FALSE)
  else if(opt$block == 0)
    opt$block = length(blocks)

  return (opt)
}

setPosPar = function(opt, blocks, i_resp){

  J = length(blocks)
  opt$blocks = blocks
  opt$block_names = names(blocks)

  par = c("blocks", "block_names", "ncomp")
  if (all(opt$tau != "optimal"))
    par[length(par)+1] = "tau"

  for (i in 1:length(par)){
    temp = opt[[par[i]]][[J]]
    opt[[par[i]]][[J]] = opt[[par[i]]][[i_resp]]
    opt[[par[i]]][[i_resp]] = temp
  }

  names(opt$blocks) = opt$block_names

  return(opt)
}

#' Launch a Shiny application for S/RGCCA
#' @export
runShiny <- function()
  shiny::runApp("inst/shiny")


warnConnection = function(x)
  warning(paste("By using a ", x , ", all blocks are connected to this block in the connection matrix and the connection file is ignored.\n", sep=""),
          call. = FALSE)

##################
#     Main
##################

# Pre-requisite: for xlsx inputs, java must be installed
# Under linux: sudo apt-get install default-jre default-jdk && sudo R CMD javareconf

#Loading librairies
librairies = c("RGCCA", "ggplot2", "optparse", "scales", "plotly", "visNetwork", "igraph", "ggrepel")
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
           type = "sgcca",
           scheme = "factorial",
           tau = "0.3, 1",
           init = "svd",
           ncomp = "2, 2",
           block = 0,
           compx = 1,
           compy = 2,
           nmark = 20,
           output1 = "samples_plot.pdf",
           output2 = "corcircle.pdf",
           output3 = "fingerprint.pdf",
           output4 = "ave.pdf",
           output5 = "correlation.pdf",
           output5 = "connection.pdf",
           datasets = "~/Documents/DATA/Nucleiparks/Nucleiparks_full/lipidomic.tsv, ~/Documents/DATA/Nucleiparks/Nucleiparks_full/clinic_quant.tsv")

tryCatch({
  opt = parse_args(getArgs())
  opt = checkArg(opt)
}, error = function(e) {
  if (length(grep("nextArg", e[[1]])) != 1)
    stop(e[[1]], call.=FALSE)
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
opt$scale = ("scale" %in% names(opt))
opt$text = !("text" %in% names(opt))
VERBOSE = FALSE

if( ! is.null(opt$response) ){
  warnConnection("supervized method with a response")
  if( opt$superblock){
    opt$superblock = FALSE
    if("superblock" %in% names(opt))
      warning("In a supervised mode, the superblock corresponds to the response.\n", call. = FALSE)
  }
}

blocks = setBlocks(opt$superblock, opt$datasets, opt$names, opt$separator, opt$header)

opt = postCheckArg(opt, blocks)

res = blocks[[1]]

if( ! is.null(opt$response) ){
  opt = setPosPar(opt, blocks, opt$response)
  blocks = opt$blocks
}

if( !opt$superblock  && opt$type != "pca"){
  if(!isTRUE(opt$scale))
    blocks = lapply(blocks, function(x) scale2(x, scale = F))
}else{
  if( opt$superblock )
    warnConnection("superblock")

  if(isTRUE(opt$scale))
    blocks  = lapply(blocks, function(x) scale2(x, bias = opt$bias) / nrow(x) )
  else
    blocks = lapply(blocks, function(x) scale2(x, scale = F))

  # TODO: scale par column
  opt$scale = FALSE

  blocks[["Superblock"]] = Reduce(cbind, blocks)
}

connection = opt$connection
if(!is.matrix(connection))
  connection = setConnection(blocks, (opt$superblock | !is.null(opt$response)), opt$connection, opt$separator)

#group = setResponse(blocks, opt$group, opt$separator, opt$header)

#blocks[["Clinic"]] = scale(blocks[["Clinic"]], scale = T)

rgcca.out = rgcca.analyze(blocks, connection, opt$tau, opt$ncomp, opt$scheme, opt$scale, opt$init, opt$bias, opt$type)

##### Nucleiparks #####
group = read.table("~/Documents/DATA/Nucleiparks/group_V2.tsv",
                   header = F,
                   sep = "\t",
                   dec = ".",
                   row.names = 1)

clusters = read.table("~/bin/fingerprint_clustering/clusters.tsv",
                   header = T,
                    sep = "\t",
                   dec = ".",
                   row.names = 1)
clusters = as.data.frame(clusters[, 1], row.names = row.names(clusters))

response = read.table("~/Documents/DATA/Nucleiparks/Nucleiparks_full/clinic.tsv",
                      header = T,
                      sep = "\t",
                      dec = ".",
                      row.names = 1)
response = response[, 6:NCOL(response)]

group2 = as.data.frame(response[, "updrs.score"]); row.names(group2) = row.names(response)

cor = getCor(rgcca.out, blocks)

#cor1 = rev(cor[order(abs(cor[,1])), 1 ]) ; names(cor1) = row.names(cor)[order(cor[,1])]; cor1
#rgcca.out$Y[[1]][order(rgcca.out$Y[[1]][,1]), 1]
#rev(rgcca.out$a[[1]][order(abs(rgcca.out$a[[1]][, 1])), 1])

##########"

ax <- list(linecolor = toRGB("white"), ticks = "")
# Samples common space
# if(opt$ncomp[opt$block] == 1 && is.null(opt$block_y)){
#   warning("With a number of component of 1, a second block should be chosen to perform a samples plot", .call = FALSE)
# }else{
  ( samples_plot = plotSamplesSpace(rgcca.out, group2, opt$compx, opt$compy, opt$block, opt$text, opt$block_y) )
plotSamplesSpace(rgcca.out, group, opt$compx, opt$compy, opt$block, opt$text, opt$block_y)
plotSamplesSpace(rgcca.out, clusters, opt$compx, opt$compy, opt$block, opt$text, opt$block_y)
#   ggplotly(samples_plot) %>%
#     layout(xaxis = ax, yaxis = ax)
#   plotSamplesSpace(rgcca.out, group, opt$compx, opt$compy, 1)
#   savePlot(opt$output1, samples_plot)
# }

#if(opt$ncomp[opt$block] > 1){
  # Variables common space
  ( corcircle = plotVariablesSpace(rgcca.out, blocks, opt$compx, opt$compy, opt$superblock, opt$block, opt$text) )
  #ggplotly(corcircle) %>%
    #layout(xaxis = ax, yaxis = ax)
  # plotVariablesSpace(rgcca.out, blocks, opt$compx, opt$compy, opt$superblock, 3)
  savePlot(opt$output2, corcircle)
#}
# Fingerprint plot
( fingerprint = plotFingerprint(rgcca.out, opt$compx, opt$superblock, 50, opt$block) )
plotFingerprint(rgcca.out, opt$compy, opt$superblock, 50, opt$block)
savePlot(opt$output3, fingerprint)

#if( ! is.null(opt$response) ){
  ( correlation = corResponse(rgcca.out, blocks, response, comp = opt$compx, i_block = 1) )
  corResponse(rgcca.out, blocks, response, comp = opt$compy, i_block = 1)
  savePlot(opt$output5, correlation)
#}

# Average Variance Explained
if(opt$type != "pca"){

  (ave = plotAVE(rgcca.out, opt$compx))
  savePlot(opt$output4, ave)

  nodes <- getNodes(opt, blocks, rgcca.out)
  edges <- getEdges(connection, blocks)
  conNet <- function() plotNetwork(nodes, edges, blocks)
  plotNetwork2(nodes, edges, blocks)
  savePlot(opt$output6, conNet)
}

nrow(blocks[[1]])
#write.table(res, paste0("~/Documents/DATA/Nucleiparks/without_NA/", names(blocks)[1], ".tsv"), row.names= T, col.names = T, sep="\t")


# penalty_matrix = matrix(0, 2, 20)
# penalty_matrix[1, ] = seq(1, sqrt(NCOL(blocks[[1]])), l = 20)
# penalty_matrix[2, ] = rep(sqrt(NCOL(CLI1)), 20)
# perm.out = MultiCCA.permute(xlist, nperms = 50, trace = FALSE,
#                             penalties = penalty_matrix)
