# Author: Etienne CAMENEN
# Date: 2018
# Institute: ICM - Institut du Cerveau et de la Moelle épinière (Paris, FRANCE),
# Institut Français de Bioinformatique (IFB), Centre national de la recherche scientifique (CNRS)
# Contact: iconics@icm-institute.org
# Key-words: omics, RGCCA, multi-block
# EDAM operation: analysis, correlation, visualisation
# EDAM topic: omics, medecine, mathematics
#
# Abstract: A user-friendly multi-blocks analysis (Regularized Generalized Canonical Correlation Analysis, RGCCA)
# with all default settings predefined. Produce two figures to help clincians to identify biomarkers: 
# samples and variables projected on the two first component of the multi-block analysis.

rm(list=ls())
#TODO: remove
setwd("C:/Users/etienne.camenen/bin/rgcca_galaxy")

################################
#          File
################################

getFileName = function(fi)
  #get prefix part from a file
  unlist(strsplit(fi, '[.]'))[1]

loadData = function(fi, fo=fi, row.names=NULL, h=F){
  #create a dataset object from a file loading
  #fi: input file name
  #fo: dataset object name
  data = as.matrix(read.table(fi, sep = SEPARATOR, h = h, row.names = row.names))
  assign(fo, data, .GlobalEnv)
  #TODO: catch warning missing \n at the end of the file
}

save = function(p, f)  ggsave(p, f, width=10, height=8)

setBlocks = function(){
  #create a list object of blocks from files loading
  
  #remove white space
  opt$datasets = gsub(" ", "", opt$datasets)
  #split by ,
  blocksName = unlist(strsplit(opt$datasets, ","))
  
  #load each dataset
  blocks = list()
  for (i in 1:length(blocksName)){
    fi = blocksName[i]
    fo = getFileName(fi)
    loadData(fi, fo, 1, T)
    blocks[[fo]] = get(fo)
  }
  blocks[["Superblock"]] = Reduce(cbind, blocks)
  
  return(blocks)
}

setResponse = function(){
  #create a dataset object from a file loading containg the response
  
  if("response" %in% names(opt)){
    opt$response = "Response.tsv"
    loadData(opt$response, "response", 1, F)
    #TODO: check n1  = n2 = ...
    if(isTRUE(DISJONCTIF)) response = factor(apply("Response", 1, which.max))
    return (response)
  }else{
    return ( rep("black", NROW(blocks[[1]])) )
  }
}

################################
#          Graphic
################################

circleFun <- function(center = c(0,0), diameter = 2, npoints = 100){
  #creates x,y coordinates for a circle
  
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

printAxis = function (n)
  #n: number of the axis
  paste("Axis ", n, " (", round(rgcca$AVE$AVE_X[[length(blocks)]][n] * 100 , 1),"%)", sep="")

plotSpace = function (df, title, color, comp1, comp2){
  #plot settings for projection of points in a bi-dimensional space
  
  ggplot(df, aes(df[,1], df[,2], colour = color)) + 
  theme_classic() +
  geom_vline(xintercept = 0, col="grey", linetype="dashed", size=1) + 
  geom_hline(yintercept = 0, col="grey", linetype="dashed", size=1) + 
  labs ( title = title,
         x = printAxis(comp1), 
         y = printAxis(comp2),
         color = "Blocks") +
  geom_text_repel(aes(colour = color, label= rownames(df)), size = 3, force=2) +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(breaks=NULL) +
  theme(
    #panel.border  = element_rect(fill="blue"),
    legend.text = element_text(size = 13),
    legend.title = element_text(face="bold.italic", size=16),
    axis.text = element_blank(),
    axis.title.y = element_text(face="italic", margin = margin(0,20,0,0), size=19),
    axis.title.x = element_text(face="italic", margin = margin(20,0,0,0), size=19),
    plot.title = element_text(size = 25, face = "bold", hjust=0.5, margin = margin(0,0,20,0)))
  #+ stat_ellipse()
  #TODO: if NB_VAR > X
}

plot_biomarkers = function(df, comp){
  ggplot(df, mapping=aes(x=order, y=df[,comp], fill = color)) +
    geom_hline(yintercept = c(-1,1), col="grey", linetype="dashed", size=1) + 
    geom_hline(yintercept = 0, col="grey", size=1) +
    geom_bar(stat = "identity") +
    coord_flip() + 
    scale_x_continuous(breaks=df$order, labels=rownames(df)) +
    labs(title= "Variable weights", subtitle=printAxis(comp), x = "", y = "", fill = "Blocks") +
    theme_classic() +
    theme(legend.text = element_text(size = 8),
          legend.title = element_text(face="bold.italic", size=10),
          axis.text.y = element_text(size = 8, face="italic", labels(rownames(df))),
          axis.text.x = element_text(size = 8, face="italic", color="darkgrey"),
          #axis.line.x = element_line(colour = "grey"),
          axis.line = element_blank(),
          #axis.ticks.x = element_line(colour = "grey"),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 18, face="bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12, face="italic"))
    # scale_fill_manual(values = c("steelblue4", "indianred3")) +
    # scale_colour_manual(values = c("steelblue4", "indianred3"))
}


################################
#          Arguments
################################

#TODO: remove default files
getArgs = function(){
  option_list = list(
    make_option(c("-d", "--datasets"), type="character", metavar="character", default="X_agric.tsv,X_ind.tsv,X_polit.tsv",
                help="Bloc files name"),
    make_option(c("-c", "--connection"), type="character", metavar="character", default="connection_matrix.txt",
                help="Connection file name"),
    make_option(c("-r", "--response"), type="character", metavar="character", default="response.tsv",
                help="Response file name"),
    make_option(c( "-o1", "--output1"), type="character", metavar="character", default="samplesSpace.pdf", 
                help="Variables space file name [default: %default]"),
    make_option(c( "-o2", "--output2"), type="character", metavar="character", default="variablesSpace.pdf", 
                help="Sample space file name [default: %default]"),
    make_option(c( "-o2", "--output3"), type="character", metavar="character", default="best_biomarkers.pdf", 
                help="Best biomarkers file name [default: %default]"),
    make_option(c("-g", "--scheme"), type="integer", metavar="integer", default=2, 
                help="Scheme function g(x) [default: x^2] (1: x, 2: x^2, 3: x^3, 4: |x|")
  )
  args = commandArgs(trailingOnly=T)
  return (OptionParser(option_list=option_list))
  }

#Check the validity of the arguments 
#Inputs:
# a: arguments (optionParser object)
checkArg = function(a){
  opt = parse_args(a)
  
  if (is.null(opt$scheme)) opt$scheme = "factorial"
  else if ((opt$scheme < 1) || (opt$scheme > 4)){
    stop("--scheme must be comprise between 1 and 4 [by default: 3].\n", call.=FALSE)
  }else{
    schemes = c("horst", "factorial", "centroid")
    if (opt$scheme == 4)
      opt$scheme = function(x) x^4
    else 
      opt$scheme = schemes[opt$scheme]
  }
  
  checkFile = function (o){
    # o: one argument from the list of arguments
    if(!file.exists(opt[[o]])){
      stop(paste("--", o, " name does not exist\n", sep=""), call.=FALSE)
    }
  }
  FILES = c("connection", "response", "infile")
  for (o in FILES)
    if(!is.null(opt[[o]])) checkFile(o)
  
  #default settings of connection_matrix matrix
  if(is.null(opt$connection)){
     connection_matrix = matrix(0,length(blocks),length(blocks))
     seq = 1:(length(blocks)-1)
     connection_matrix[length(blocks), seq] <- 1 -> connection_matrix[seq, length(blocks)]
  }else{
    loadData("connection_matrix.txt", "connection_matrix", h=F)
  }
  
  return (opt)
}

################################
#            MAIN
################################

#Loading librairies
librairies = c("RGCCA", "ggplot2", "ggrepel", "optparse")
for (l in librairies){
  if (! (l %in% installed.packages()[,"Package"])) install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}

#Global settings
SCALE = T
SEPARATOR = "\t"
VERBOSE = F
TAU = "optimal"
DISJONCTIF = F
COMP1 = 1
COMP2 = 2

#Get arguments
args = getArgs()
tryCatch({
  opt = checkArg(args)
}, error = function(e) {
  #print_help(args)
  stop(e[[1]], call.=FALSE)
})


blocks = setBlocks()
NB_COMP = sapply(blocks, NCOL)
# TODO: Error in rgcca(blocks, connection_matrix, tau = TAU, scheme = scheme, ncomp = rep(NB_COMP,  : 
#                                                                     For each block, choose a number of components smaller than the number of variables!

rgcca = rgcca(blocks,
              connection_matrix,
              tau = TAU,
              scheme = opt$scheme,
              ncomp = NB_COMP,
              scale = SCALE,
              verbose = VERBOSE)
#ncomp = rep(NB_COMP, length(blocks))
#TODO: catch Error in connection_matrix * h(cov2(Y, bias = bias)) : non-conformable arrays
#message: Number of row/column of connection matrix doesn't match with the number of blocks.


# Samples common space
samples = data.frame(rgcca$Y[[length(blocks)]])
color = setResponse()
samplesSpace = plotSpace(samples, "Samples", color, COMP1, COMP2)
save(samplesSpace, opt$output1)

#attribution of block ID to each corresponding variable
blocks_variables = rep( names(blocks)[-length(blocks)] , sapply(blocks[1:(length(blocks)-1)], NCOL))

# Variables common space
variables =  data.frame( 
 #correlation matrix with superblock for each variables and each component selected
 sapply ( c(COMP1:COMP2), function(x) cor( blocks[["Superblock"]], rgcca$Y[[length(blocks)]][, x] ) ) , 
 blocks_variables,
 row.names = colnames(blocks[["Superblock"]])
)

variablesSpace = plotSpace(variables, "Variables", variables[,3], COMP1, COMP2) + 
  geom_path(aes(x,y), data=circleFun(), col="grey", size=1) + 
  geom_path(aes(x,y), data=circleFun()/2, col="grey", size=1, lty=2)
save(variablesSpace, opt$output2)

# Biomarkers plot
biomarkers = data.frame(rgcca$a[[4]], color=blocks_variables)
biomarkers_ordered = data.frame(biomarkers[order(abs(biomarkers[,1]), decreasing = TRUE),], order = nrow(biomarkers):1)

best_biomarkers = plot_biomarkers(biomarkers_ordered, 1)
best_biomarkers
save(biomarkers, opt$output3)