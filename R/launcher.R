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
# with all default settings predefined. Produce two figures to help clinicians to identify fingerprint:
# samples and variables projected on the two first component of the multi-block analysis.


rm(list=ls())

##################
#     Arguments
##################

getArgs = function(){
  option_list = list(
    make_option(c("-d", "--datasets"), type="character", metavar="character", help="Path of the blocks", default = opt[7]),
    make_option(c("-w", "--directory"), type="character", metavar="character", help="Path of the scripts directory (for Galaxy)", default=opt[1]),
    make_option(c("-c", "--connection"), type="character", metavar="character", help="Connection file path"),
    make_option(c("-r", "--response"), type="character", metavar="character", help="Response file path"),
    make_option(c("-n", "--names"), type="character", metavar="character", help="Names of the blocks [default: filename]"),
    make_option(c("-H", "--header"), type="logical", action="store_false", help="DO NOT consider first row as header of columns"),
    make_option(c("-s", "--separator"), type="integer", metavar="integer", default=1,
                help="Type of separator [default: tabulation] (1: Tabulation, 2: Semicolon, 3: Comma"),
    make_option(c("-g", "--scheme"), type="integer", metavar="integer", default=2,
                help="Scheme function g(x) [default: x^2] (1: x, 2: x^2, 3: |x|, 4: x^4"),
    make_option(c( "--output1"), type="character", metavar="character", default=opt[4],
                help="Variables space file name [default: %default]"),
    make_option(c( "--output2"), type="character", metavar="character", default=opt[5],
                help="Sample space file name [default: %default]"),
    make_option(c( "--output3"), type="character", metavar="character", default=opt[6],
                help="Best fingerprint file name [default: %default]")
  )
  args = commandArgs(trailingOnly=T)
  return (OptionParser(option_list=option_list))
}


#Check the validity of the arguments
#Inputs:
# a: arguments (optionParser object)
checkArg = function(a){

  opt = parse_args(a)

  if(is.null(opt$datasets)) stop(paste("--datasets is required\n", sep=""), call.=FALSE)

  if (is.null(opt$scheme)) opt$scheme = "factorial"
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

  FILES = c("connection", "response")
  for (o in FILES)
    if(!is.null(opt[[o]])) checkFile(opt[[o]])

  return (opt)
}

##################
#     Main
##################

# Pre-requisite: for xlsx inputs, java must be installed
# Under linux: sudo apt-get install default-jre default-jdk && sudo R CMD javareconf

#Loading librairies
librairies = c("RGCCA", "ggplot2", "optparse", "scales", "xlsx")
for (l in librairies) {
  if (!(l %in% installed.packages()[, "Package"]))
    install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}


#Get arguments
opt = list(directory = ".", separator = "\t", scheme = "factorial", output1 = "samples_space.pdf", output2 = "variables_space.pdf", output3 = "best_fingerprint.pdf", datasets="data2/Clinique.tsv,data2/Lipidomique.tsv,data2/Transcriptomique.tsv,data2/Imagerie.tsv,data2/Metabolomique.tsv")
args = getArgs()
tryCatch({
  opt = checkArg(args)
}, error = function(e) {
  if (length(grep("nextArg", e[[1]])) != 1)
    stop(e[[1]], call.=FALSE)
})

#Global settings
opt$header = !("header" %in% names(opt))
SCALE = T
VERBOSE = F
#TAU = "optimal"
COMP1 = 1
COMP2 = 2
NB_MARK = 100
SUPERBLOCK = T

setwd(opt$directory)

source("R/parsing.R")
source("R/plot.R")

blocks = setBlocks(SUPERBLOCK, opt$datasets, opt$names, opt$separator, opt$header)
connection = setConnection(blocks, opt$connection, opt$separator)
response = setResponse(blocks, opt$response, opt$separator, opt$header)
NB_COMP = 2
ncomp = rep(NB_COMP, length(blocks))
#sapply(blocks, NCOL)
# TODO: Error in rgcca(blocks, connection_matrix, tau = TAU, scheme = scheme, ncomp = rep(NB_COMP,  :
#                                                                     For each block, choose a number of components smaller than the number of variables!

getColumnSameVal = function(list_m)
  lapply(1:length(list_m), function (x) which( apply(list_m[[x]], 2, sd ) == 0 ))
#getColumnSameVal(blocks)

sgcca.res = sgcca(A = blocks,
              C = connection,
              scheme = opt$scheme,
              ncomp = ncomp,
              scale = SCALE,
              verbose = VERBOSE)

names(sgcca.res$a) = names(blocks)

# Samples common space
( samplesSpace = plotSamplesSpace(sgcca.res, response, COMP1, COMP2) )
plotSamplesSpace(sgcca.res, response, COMP1, COMP2, 1)
savePlot(opt$output1, samplesSpace)

# Variables common space
( variablesSpace = plotVariablesSpace(sgcca.res, blocks, COMP1, COMP2, SUPERBLOCK) )
plotVariablesSpace(sgcca.res, blocks, COMP1, COMP2, SUPERBLOCK, 1)

savePlot(opt$output2, variablesSpace)

# fingerprint plot
( best_fingerprint = plotFingerprint(sgcca.res, COMP1, SUPERBLOCK, NB_MARK) )
plotFingerprint(sgcca.res, COMP1, SUPERBLOCK, NB_MARK, 2)
savePlot(opt$output3, best_fingerprint)

plotAVE(sgcca.res, COMP1)
