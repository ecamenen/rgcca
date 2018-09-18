#TODO: remove
#setwd("C:/Users/etienne.camenen/bin/galaxy_rgcca")

#Loading librairies
librairies = c("RGCCA")
for (l in librairies){
  if (! (l %in% installed.packages()[,"Package"])) install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}

getArgs = function(){
  option_list = list(
    make_option(c("-d", "--datasets"), type="character", metavar="character",
                help="Bloc files name"),
    make_option(c("-c", "--connection"), type="character", metavar="character",
                help="Connection file name"),
    make_option(c("-r", "--response"), type="character", metavar="character",
                help="Response file name"),
    make_option(c( "--output1"), type="character", default="variables.pdf", 
                metavar="character", help="Variables space file name [default: %default]"),
    make_option(c( "--output2"), type="character", default="samples.pdf", 
                metavar="character", help="Sample space file name [default: %default]"),
    make_option(c( "--output3"), type="character", default="pca.pdf", 
                metavar="character", help="PCA file name [default: %default]"),
    make_option(c("-g", "--scheme"), type="integer", default=, metavar="integer",
                help="Scheme function g(x) [default: x^2] (1: x, 2: x^2, 3: x^3, 4: |x|")
  )}


#Global settings
SCALE = T
SEPARATOR = "\t"
VERBOSE = F
NB_BLOC = 3
NCOMP = 1
TAU = "optimal"

#Check the validity of the arguments 
#Inputs:
# a: arguments (optionParser object)
checkArg = function(a){
  opt = parse_args(a)
  
  if ((opt$scheme < 1) || (opt$scheme > 4)){
    stop("--scheme must be comprise between 1 and 4 [by default: 3].\n", call.=FALSE)
  }else{
    schemes = c("horst", "factorial", "centroid")
    if (opt$scheme == 4)
      scheme = function(x) x^4
    else 
      scheme = schemes[opt$scheme]
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
     C = 1 - diag(1, NB_BLOC+1, NB_BLOC+1)
     #TODO: 1 - diag(length(A))
  }else{
    connection = "connection_matrix.txt"
    C = as.matrix(read.table(connection,
                             sep=SEPARATOR,
                             h=F))
  }
  
  return (opt)
}

#TODO: remove
data(Russett)
X_agric = as.matrix(Russett[,c("gini","farm","rent")])
X_ind = as.matrix(Russett[,c("gnpr","labo")])
X_polit = as.matrix(Russett[ , c("inst", "ecks", "death", "demostab", "dictator")]) 
opt=list()
opt$datasets=list(X_agric, X_ind, X_polit)

#standardization
A = lapply(opt$datasets,
           function(x) scale2(x, bias = TRUE))

#loading data
A = list(A, Superblock = Reduce(cbind,data))

#run
rgcca = rgcca(A,
              C,
              tau = TAU,
              scheme = scheme,
              ncomp = NCOMP,
              scale = SCALE,
              verbose = VERBOSE)
#TODO: catch Error in C * h(cov2(Y, bias = bias)) : non-conformable arrays

#Samples common space
if(isTRUE(disjonctif)) factor(apply(opt$response, 1, which.max))

df1 = data.frame(Response = factor(apply(opt$response, 1, which.max),
                                           labels = c("demostab", "demoinst", "dictator")), 
                 rgcca$Y[[length(A)]] )

comp1 = rgcca_B_factorial$Y[[1]][, 1], 
comp2 = rgcca_B_factorial$Y[[2]][, 1])

p1 <- ggplot(df1, aes(comp1, comp2)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  ggtitle("Factor plot - Common Space") +
  geom_text(aes(colour = Response, label= rownames(df1)), vjust=0, nudge_y = 0.03, size = 3) +
  theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())

p1; pdf(p1)
 
 
 # Variables common space
 
 df2 = data.frame(comp1 = cor(Russett, rgcca.hpca$Y[[4]])[, 1], comp2 = cor(Russett, rgcca.hpca$Y[[4]])[, 2], BLOCK = rep(c("X1", "X2", "X3"), sapply(A[1:3], NCOL)))
 
 # Circle 
 circleFun <- function(center = c(0,0), diameter = 2, npoints = 100){
   r = diameter / 2
   tt <- seq(0,2*pi,length.out = npoints)
   xx <- center[1] + r * cos(tt)
   yy <- center[2] + r * sin(tt)
   return(data.frame(x = xx, y = yy))
 }
 
 circle <- circleFun(c(0,0),2,npoints = 100)
 
 p2 <- ggplot(df2, aes(comp1, comp2), colour = BLOCK) +
   geom_path(aes(x,y), data=circle) + 
   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
   ggtitle("Correlation Circle (Russett data) - Common Space") + 
   geom_text(aes(colour = BLOCK, label= rownames(df2)), vjust=0,nudge_y = 0.03,size = 3) + 
   theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())
 
 save(p2)
 