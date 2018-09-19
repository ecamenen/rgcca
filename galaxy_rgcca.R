#TODO: remove
#setwd("C:/Users/etienne.camenen/bin/rgcca_galaxy")

getFileName = function(fi)
  unlist(strsplit(fi, '[.]'))[1]

loadData = function(fi, fo=fi, row.names=NULL, h=F){
  data = as.matrix(read.table(fi, sep = SEPARATOR, h = h, row.names = row.names))
  assign(fo, data, .GlobalEnv)
  #TODO: catch warning missing \n at the end of the file
}

savePdf = function(f, p){
  pdf(f); p; dev.off()
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
     C = 1 - diag(1, length(A), length(A))
     #TODO: 1 - diag(length(A))
  }else{
    loadData("connection_matrix.txt", "C", h=F)
  }
  
  return (opt)
}

#Loading librairies
librairies = c("RGCCA", "ggplot2")
for (l in librairies){
  if (! (l %in% installed.packages()[,"Package"])) install.packages(l, repos = "http://cran.us.r-project.org", quiet = T)
  library(l, character.only = TRUE)
}

#Global settings
SCALE = T
SEPARATOR = "\t"
VERBOSE = F
NB_BLOC = 3
NB_COMP = 2
TAU = "optimal"
DISJONCTIF = F

#TODO: remove
opt=list()
opt$datasets = "X_agric.tsv, X_ind.tsv,X_polit.tsv"

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
if(SUPERVISED){
  opt$response = "Response.tsv"
  loadData(opt$response, "Response", 1, F)
  #TODO: check n1  = n2 = ...
  if(isTRUE(DISJONCTIF)) Response = factor(apply("Response", 1, which.max))
  color = Response
}else{
  color = rep("black", NROW(A[[1]]))
}

#run
rgcca = rgcca(A,
              C,
              tau = TAU,
              scheme = scheme,
              ncomp = rep(NB_COMP, length(A)),
              scale = SCALE,
              verbose = VERBOSE)
#TODO: catch Error in C * h(cov2(Y, bias = bias)) : non-conformable arrays
#message: Number of row/column of connection matrix doesn't match with the number of blocks.


df1 =  data.frame(rgcca$Y[[length(A)]])

#Samples common space
p1 <- ggplot( df1, aes(comp1, comp2)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  ggtitle("Factor plot") +
  geom_text(aes(colour = color, label= rownames(df1)), vjust=0, nudge_y = 0.03, size = 3) +
  theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())

savePdf(opt$output1, p1) 
 

 
 # Variables common space
 
 df2 = data.frame(comp1 = cor(Russett, rgcca$Y[[4]])[, 1], comp2 = cor(Russett, rgcca$Y[[4]])[, 2], BLOCK = rep(c("X1", "X2", "X3"), sapply(A[1:3], NCOL)))
 
 COMP1 = 1
 COMP2 = 2
 df3 =  data.frame( 
   sapply ( c(COMP1:COMP2), function(x) cor( A[["Superblock"]], rgcca$Y[[length(A)]][, x] ) ) , 
   rep( sapply ( 1: 3, function(x) rep(paste("Block", x)) ), sapply(A[1:(length(A)-1)], NCOL)) ,
   row.names = colnames(A[["Superblock"]])
 )
colnames(df3) = c( paste("Axis", COMP1, sep=""), paste("Axis", COMP2 , sep=""), "BLOCK" )
     
 # Circle 
 circleFun <- function(center = c(0,0), diameter = 2, npoints = 100){
   r = diameter / 2
   tt <- seq(0,2*pi,length.out = npoints)
   xx <- center[1] + r * cos(tt)
   yy <- center[2] + r * sin(tt)
   return(data.frame(x = xx, y = yy))
 }
 
 
 p2 <- ggplot(df3, aes(df3[,1], df3[,2]), colour = BLOCK) +
   geom_path(aes(x,y), data=circleFun()) + 
   geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
   ggtitle("Correlation Circle") + 
   geom_text(aes(colour = BLOCK, label= rownames(df2)), vjust=0,nudge_y = 0.03,size = 3) + 
   theme(legend.position="bottom", legend.box = "horizontal", legend.title = element_blank())

 save(p2)
 