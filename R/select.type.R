#' Translates the type string into the appropriate tau and function
select.type <- function(opt, A = blocks){

  J = length(A)

  scheme = opt$scheme;  tau = opt$tau;  ncomp = opt$ncomp;  C = opt$connection;  superblock = opt$superblock; type = opt$type

  ncomp = unlist(lapply(strsplit(unlist(as.character(ncomp)), ","), as.double)[[1]])
  if(tau != "optimal")
    tau = unlist(lapply(strsplit(unlist(as.character(tau)), ","), as.double)[[1]])

  ### SETTINGS ###

  warnParam = function(param, x)
    warning(paste("Because ", type, " was selected, ", paste(deparse(substitute(param))), " parameter was set to ",
                  toString(x),"\n", sep=""),
            call. = FALSE)

  setTau = function(x){
    warnParam(tau, x)
    return(x)
  }

  setScheme = function(x){
    warnParam(scheme, x)
    return(x)
  }

  setConnection = function(x){
    warnParam(connection, paste(deparse(substitute(x))))
    return(x)
  }

  warnSuper = function(x){
    if(length(x) < (length(A)+1)){
      warning(paste("Because of the use of a superblock, ", paste(deparse(substitute(x))) ,
                    " for the superblock was the one of the first block.\n", sep=""), call. = FALSE)
      return(c(x, x[1]))
    }else{
      return(x)
    }
  }

  setSuperbloc = function(verbose = TRUE){
    if(verbose)
      warning(paste("Because ", type, " was set, a superblock was used.\n",
                    sep=""), call. = FALSE)

    assign("A", list(c(A, Reduce(cbind, A))), envir = parent.frame())
    assign("superblock", TRUE, envir = parent.frame())
    assign("C", NULL, envir = parent.frame())
    assign("ncomp", warnSuper(ncomp), envir = parent.frame())
  }

  ### CHECK TYPES ###

  if (tolower(type) == "rgcca"){
    if(superblock){
      setSuperbloc(FALSE)
      tau <- warnSuper(tau)
    }
  }else{
    superblock <- FALSE
  }

  if (tolower(type) == "pca"){
    if(length(A) != 1)
      stop(paste(length(A), " blocks used in the analysis. Only one block is required for a PCA.\n", sep=""), call.=FALSE)
    scheme   <- setScheme("horst")
    tau      <- setTau(c(1, 1))
    ncomp    <- rep(ncomp[1], 2)
    C        <- setConnection(1-diag(2))
  }

  else if (tolower(type) == "cca"){
    if(length(A) != 2)
      stop(paste(length(A), " blocks used in the analysis. Two blocks are required for a CCA.\n", sep=""), call.=FALSE)
    scheme   <- setScheme("horst")
    tau      <- setTau(c(0, 0))
    C        <- setConnection(1-diag(2))
  }

  else if (tolower(type) == "sumcor"){
    scheme   <- setScheme("horst")
    tau      <- setTau(rep(0, J))
    C        <- setConnection(1-diag(J))
  }

  else if (tolower(type) == "ssqcor"){
    scheme   <- setScheme("factorial")
    tau      <- setTau(rep(0, J))
    C        <- setConnection(1-diag(J))
  }

  else if (tolower(type) == "sabscor"){
    scheme   <- setScheme("centroid")
    tau      <- setTau(rep(0, J))
    C        <- setConnection(1-diag(J))
  }

  else if (tolower(type)%in%c("sumcov", "sumcov-1", "maxbet")){
    scheme   <- setScheme("horst")
    tau      <- setTau(rep(1, J))
    C        <- setConnection(matrix(1, J, J))
  }

  else if (tolower(type)%in%c("sumcov-2", "maxdiff")){
    scheme   <- setScheme("factorial")
    tau      <- setTau(rep(1, J))
    C        <- setConnection(1 - diag(J))
  }

  else if (tolower(type)%in%c("ssqcov", "ssqcov-1", "maxbet-b")){
    scheme   <- setScheme("factorial")
    tau      <- setTau(rep(1, J))
    C        <- setConnection(matrix(1, J, J))
  }

  else if (tolower(type)%in%c("ssqcov-2", "maxdiff-b")){
    scheme   <- setScheme("factorial")
    tau      <- setTau(rep(1, J))
    C        <- setConnection(1 - diag(J))
  }

  else if (tolower(type) == "rcon-pca"){
    tau <- warnSuper(tau)
    setSuperbloc()
  }

  else if (tolower(type)%in%c("maxvar-b", "gcca", "niles", "maxvar")){
    scheme   <- setScheme("factorial")
    tau      <- setTau(rep(0, J+1))
    setSuperbloc()
  }

  else if (tolower(type)%in%c("maxvar-a", "cpca-w", "sum-pca")){
    scheme   <- setScheme("factorial")
    tau      <- setTau(c(rep(1, J), 0))
    setSuperbloc()
  }

  else if (tolower(type) == "ridge-gca"){
    scheme   <- setScheme("factorial")
    tau      <- setTau(c(tau[1:J], 0))
    setSuperbloc()
  }

  else if (tolower(type) == "r-maxvar"){
    scheme   <- setScheme("factorial")
    tau <- warnSuper(tau)
    setSuperbloc()
  }

  else if (tolower(type) == "hpca"){
    scheme   <- function(x) x^4
    tau      <- setTau(c(rep(1, J), 0))
    setSuperbloc()
  }

  else if(tolower(type) != "rgcca"){
    stop("Wrong type of analysis. Please select one among the following list: rgcca, cpca-w, gcca, hpca, maxbet-b, maxbet, maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar, rcon-pca, ridge-gca, sabscor, ssqcor, ssqcor, ssqcov-1, ssqcov-2, ssqcov, sum-pca, sumcor, sumcov-1, sumcov-2, sumcov.\n")
  }

  opt$scheme = scheme;  opt$tau = tau;  opt$ncomp = ncomp;  opt$connection = C;  opt$superblock = superblock

  return(opt)
}
