#TODO: remove
setwd("C:/Users/etienne.camenen/bin/rgcca_galaxy")

################################
#          File
################################

getFileName = function(fi)
  unlist(strsplit(fi, '[.]'))[1]

loadData = function(fi, fo=fi, row.names=NULL, h=F){
  data = as.matrix(read.table(fi, sep = SEPARATOR, h = h, row.names = row.names))
  assign(fo, data, .GlobalEnv)
  #TODO: catch warning missing \n at the end of the file
}


################################
#          Graphic
################################

# Circle 
circleFun <- function(center = c(0,0), diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

printAxis = function (n)
  #n: number of the axis
  paste("Axis ", n, " (", round(rgcca$AVE$AVE_X[[length(A)]][n] * 100 , 1),"%)", sep="")

savePdf = function(f, p){
  pdf(f); p; dev.off()
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
    make_option(c("-r", "--response"), type="character", metavar="character", default="Response.tsv",
                help="Response file name"),
    make_option(c( "-o1", "--output1"), type="character", metavar="character", default="variables.pdf", 
                help="Variables space file name [default: %default]"),
    make_option(c( "-o2", "--output2"), type="character", metavar="character", default="samples.pdf", 
                help="Sample space file name [default: %default]"),
    make_option(c( "-o3", "--output3"), type="character", metavar="character", default="pca.pdf",
                help="PCA file name [default: %default]"),
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
  
  #default settings of C matrix
  if(is.null(opt$connection)){
     D = matrix(0,length(A),length(A))
     seq = 1:(length(A)-1)
     D[length(A), seq] <- 1 -> D[seq, length(A)]
  }else{
    loadData("connection_matrix.txt", "C", h=F)
  }
  
  return (opt)
}

################################
#            MAIN
################################

#Loading librairies
librairies = c("RGCCA", "ggplot2", "optparse")
for (l in librairies){
  if (! (l %in% installed.packages()[,"Package"])) install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}

#Global settings
SCALE = T
SEPARATOR = "\t"
VERBOSE = F
NB_BLOC = 3
TAU = "optimal"
DISJONCTIF = F

#Get arguments
args = getArgs()
tryCatch({
  opt = checkArg(args)
}, error = function(e) {
  #print_help(args)
  stop(e[[1]], call.=FALSE)
})

#remove white space
opt$datasets = gsub(" ", "", opt$datasets)
#split by ,
BLOCKS = unlist(strsplit(opt$datasets, ","))

#load each dataset
A = list()
for (i in 1:length(BLOCKS)){
  fi = BLOCKS[i]
  fo = getFileName(fi)
  loadData(fi, fo, 1, T)
  A[[fo]] = get(fo)
}
A[["Superblock"]] = Reduce(cbind, A)

#Response
if("response" %in% names(opt)){
  opt$response = "Response.tsv"
  loadData(opt$response, "Response", 1, F)
  #TODO: check n1  = n2 = ...
  if(isTRUE(DISJONCTIF)) Response = factor(apply("Response", 1, which.max))
  color = Response
}else{
  color = rep("black", NROW(A[[1]]))
}

#run
NB_COMP = sapply(A, NCOL)
# TODO: Error in rgcca(A, C, tau = TAU, scheme = scheme, ncomp = rep(NB_COMP,  : 
#                                                                     For each block, choose a number of components smaller than the number of variables!

rgcca = rgcca(A,
              C,
              tau = TAU,
              scheme = opt$scheme,
              ncomp = NB_COMP,
              scale = SCALE,
              verbose = VERBOSE)
#ncomp = rep(NB_COMP, length(A)),
#TODO: catch Error in C * h(cov2(Y, bias = bias)) : non-conformable arrays
#message: Number of row/column of connection matrix doesn't match with the number of blocks.

df1 = data.frame(rgcca$Y[[length(A)]])

#Samples common space
p1 <- ggplot( df1, aes(df1[,1], df1[,2])) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  ggtitle("Factor plot") +
  geom_text(aes(colour = color, label= rownames(df1)), vjust=0, nudge_y = 0.03, size = 3) +
  theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())

savePdf(opt$output1, p1)

 
# Variables common space
COMP1 = 1
COMP2 = 2
df2 =  data.frame( 
 sapply ( c(COMP1:COMP2), function(x) cor( A[["Superblock"]], rgcca$Y[[length(A)]][, x] ) ) , 
 BLOCK = rep( sapply ( 1: 3, function(x) rep(paste("Block", x)) ), sapply(A[1:(length(A)-1)], NCOL)) ,
 row.names = colnames(A[["Superblock"]])
)

p2 <- ggplot(df2, aes(df2[,1], df2[,2])) +
 geom_path(aes(x,y), data=circleFun()) + 
 geom_vline(xintercept = 0) + 
 geom_hline(yintercept = 0) + 
 ggtitle("Correlation Circle") + 
 labs ( x = printAxis(COMP1), y = printAxis(COMP2) ) +
 geom_text(aes(colour = BLOCK, label= rownames(df2)), vjust=0, nudge_y = 0.03, size = 3) + 
 theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())

savePdf(opt$output2, p2)