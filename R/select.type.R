#' Translates the type string into the appropriate tau and function
select.type <- function(opt, A = blocks){

  J = length(A)

  scheme = opt$scheme; C = opt$connection;  superblock = opt$superblock; type = opt$type

  ncomp = unlist(lapply(strsplit(gsub(" ", "", as.character(opt$ncomp)), ","), as.double)[[1]])

  l_tau = as.list(strsplit(gsub(" ", "", as.character(opt$tau)), ",")[[1]])

  tau = lapply(l_tau, function(x){
    tryCatch({
      as.double(x)
      }, warning = function(w){
        "optimal"
      })
  })

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
    if(length(x) < (length(A))){
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
    A = c(A, list(Reduce(cbind, A)))
    assign("A", A, envir = parent.frame())
    assign("superblock", TRUE, envir = parent.frame())
    assign("C", NULL, envir = parent.frame())
    assign("ncomp", warnSuper(ncomp), envir = parent.frame())
  }

  set2Block = function(){
    if(length(A) != 2)
      stop(paste(length(A), " blocks used in the analysis. Two blocks are required for a CCA.\n", sep=""), call.=FALSE)
    assign(scheme, setScheme("horst"), envir = parent.frame())
    assign("C", setConnection(1-diag(2)), envir = parent.frame())
  }

  ### CHECK TYPES ###

  if(length(grep("[sr]gcca", tolower(type))) == 1){
    if(superblock){
      setSuperbloc(FALSE)
      tau <- warnSuper(tau)
    }
  }else{
    superblock <- FALSE
  }

  if(length(grep("pls-?pm", tolower(type))) == 1){
    scheme   <- setScheme("centroid")
    tau      <- setTau(rep(0, J))
  }

  else if (tolower(type) == "pca"){
    if(length(A) != 1)
      stop(paste(length(A), " blocks used in the analysis. Only one block is required for a PCA.\n", sep=""), call.=FALSE)
    scheme   <- setScheme("horst")
    tau      <- setTau(c(1, 1))
    ncomp    <- rep(ncomp[1], 2)
    C        <- setConnection(1-diag(2))
  }

  else if (tolower(type) == "cca"){
    set2Block()
    tau      <- setTau(c(0, 0))
  }

  else if (tolower(type) %in% c("ifa", "pls")){
    set2Block()
    tau      <- setTau(c(1, 1))
  }

  else if (tolower(type) == "ra"){
    set2Block()
    tau      <- setTau(c(1, 0))
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

  else if (tolower(type) == "sabscov"){
    scheme   <- setScheme("centroid")
    tau      <- setTau(rep(1, J))
    C        <- setConnection(matrix(1, J, J))
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
    setSuperbloc()
    tau <- warnSuper(tau)
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
    setSuperbloc()
    tau <- warnSuper(tau)
  }

  else if (tolower(type) == "hpca"){
    scheme   <- function(x) x^4
    tau      <- setTau(c(rep(1, J), 0))
    setSuperbloc()
  }

  else if(length(grep("[sr]gcca", tolower(type))) != 1){
    stop("Wrong type of analysis. Please select one among the following list: rgcca, cpca-w, gcca, hpca, maxbet-b, maxbet, maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar, rcon-pca, ridge-gca, sabscor, ssqcor, ssqcor, ssqcov-1, ssqcov-2, ssqcov, sum-pca, sumcor, sumcov-1, sumcov-2, sumcov., sabscov, plspm\n")
  }

  opt$scheme = scheme;  opt$tau = tau;  opt$ncomp = ncomp;  opt$connection = C;  opt$superblock = superblock

  return(opt)
}

rgcca.analyze = function(blocks, connection = 1 - diag(length(A)), tau = rep(1, length(blocks)),
                         ncomp = rep(2, length(blocks)), scheme = "factorial", scale = TRUE,
                         init = "svd", bias = TRUE, type = "rgcca"){

  WARN = FALSE

  for (i in 1:length(blocks)){
    if( ncol(blocks[[i]]) > 1000 ){
      if( (type == "sgcca" && tau > 0.3) || type != "sgcca" )
        WARN = TRUE
    }
  }

  if (WARN)
    warning("Some blocks are too big. RGCCA could take some times......\n", immediate. = TRUE, call. = FALSE)

  if(type =="sgcca"){
    func = sgcca
    par = "c1"
  }else{
    func = rgcca
    par = "tau"
  }

  func.complete = quote(func(A = blocks,
                             C = connection,
                             scheme = scheme,
                             ncomp = ncomp,
                             scale = scale,
                             verbose = VERBOSE,
                             init = init,
                             bias = bias))
  func.complete[[par]] = tau
  func.res = eval(as.call(func.complete))

  names(func.res$a) = names(blocks)
  return(func.res)
}
