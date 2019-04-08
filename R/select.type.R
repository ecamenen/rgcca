VERBOSE = FALSE

#' Translates the type string into the appropriate tau and function
select.type <- function(A = blocks, opt = NULL, C = 1 - diag(length(A)), tau = rep(1, length(A)),
                        ncomp = rep(1, length(A)), scheme = "centroid", superblock = TRUE, type  = "rgcca"){

  J = length(A)
  warn.msg.super = character(0)

  if(!is.null(opt)){
    scheme = opt$scheme; C = opt$connection;  superblock = opt$superblock; type = opt$type; tau = opt$tau; ncomp = opt$ncomp
    ncomp = unlist(lapply(strsplit(gsub(" ", "", as.character(ncomp)), ","), as.double)[[1]])
    l_tau = as.list(strsplit(gsub(" ", "", as.character(tau)), ",")[[1]])

    tau = unlist(lapply(l_tau, function(x){
      tryCatch({
        as.double(x)
      }, warning = function(w){
        "optimal"
      })
    }))
  }

  ### SETTINGS ###

  warnParam = function(param, x){
    warning(paste("Because ", type, " was selected, ", paste(deparse(substitute(param))), " parameter was set to ",
                  toString(x),"\n", sep=""),
            call. = FALSE, immediate. = TRUE)
  }

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
      warn.msg.super <<- c(warn.msg.super, deparse(substitute(x)))
      return(c(x, x[1]))
    }else
      return(x)
  }

  setSuperbloc = function(verbose = TRUE){
    if(verbose)
      warning(paste("Because ", type, " was set, a superblock was used.\n",
                    sep=""), call. = FALSE, immediate. = TRUE)
    assign("A", c(A, Superblock = list(Reduce(cbind, A))), envir = parent.frame())
    assign("superblock", TRUE, envir = parent.frame())
    assign("C", NULL, envir = parent.frame())
    assign("ncomp", warnSuper(ncomp), envir = parent.frame())
  }

  set2Block = function(){

    if(length(A) != 2)
      stop(paste(length(A), " blocks used in the analysis. Two blocks are required for a CCA.\n", sep=""), call.=FALSE)

    assign("scheme", setScheme("horst"), envir = parent.frame())
    assign("C", setConnection(1-diag(2)), envir = parent.frame())
  }

  ### CHECK TYPES ###

  if(length(grep("[sr]gcca", tolower(type))) == 1){
    if(superblock){
      setSuperbloc(FALSE)
      tau <- warnSuper(tau)
    }else
      superblock <- FALSE
  }else
    superblock <- FALSE

  if(length(grep("pls-?pm", tolower(type))) == 1){
    scheme   <- setScheme("centroid")
    tau      <- setTau(rep(0, J))
    # TODO: superblock allowed in PLS-PM, whos gonna call : Arthur
  }

  else if (tolower(type) == "pca"){

    if(length(A) != 1)
      stop(paste(length(A), " blocks used in the analysis. Only one block is required for a PCA.\n", sep=""), call.=FALSE)

    scheme   <- setScheme("horst")
    tau      <- setTau(c(1, 1))
    setSuperbloc()
  }

  # 2 Blocks cases
  else if (tolower(type) %in% c("cca", "ra", "ifa", "pls")){
    set2Block()

    if (tolower(type) == "cca")
      tau      <- setTau(c(0, 0))

    else if (tolower(type) %in% c("ifa", "pls"))
      tau      <- setTau(c(1, 1))

    else if (tolower(type) == "ra")
      tau      <- setTau(c(1, 0))

  }

  # Design with 1 values everywhere
  else if (tolower(type) %in% c("sumcor", "ssqcor", "sabscor", "sumcov",
                                "sumcov-1", "maxbet", "sabscov")){

    C        <- setConnection(matrix(1, J, J))

    # COR models
    if (tolower(type) %in% c("sumcor", "ssqcor", "sabscor")){

      tau      <- setTau(rep(0, J))

      if (tolower(type) == "sumcor")
        scheme   <- setScheme("horst")

      else if (tolower(type) == "ssqcor")
        scheme   <- setScheme("factorial")

      else if (tolower(type) == "sabscor")
        scheme   <- setScheme("centroid")

    }

    # COV models
    else if (tolower(type) %in% c("sumcov", "sumcov-1", "maxbet",
                                  "ssqcov", "ssqcov-1", "maxbet-b",
                                  "sabscov", "sabscov-1")){

      tau      <- setTau(rep(1, J))

      if (tolower(type) %in% c("sumcov", "sumcov-1", "maxbet"))
        scheme   <- setScheme("horst")

      else if (tolower(type) %in% c("ssqcov", "ssqcov-1", "maxbet-b"))
        scheme   <- setScheme("factorial")

      else if (tolower(type) %in% c("sabscov", "sabscov-1"))
        scheme   <- setScheme("centroid")

    }

   # Design with 1 values everywhere and 0 on the diagonal
  }

  else if (tolower(type) %in% c("sumcov-2", "maxdiff", "ssqcov", "ssqcov-1",
                              "maxbet-b", "ssqcov-2", "maxdiff-b")){

    C        <- setConnection(1 - diag(J))

    if (tolower(type) %in% c("sumcov-2", "maxdiff")){
      scheme   <- setScheme("horst")
      tau      <- setTau(rep(0, J))
    }

    else if (tolower(type) %in% c("ssqcov-2", "maxdiff-b")){
      scheme   <- setScheme("factorial")
      tau      <- setTau(rep(1, J))
    }

  }

  # Models with a superblock
  else if (tolower(type) %in% c("maxvar-b", "gcca", "niles", "maxvar", "hpca",
                              "maxvar-a", "cpca", "cpca-w", "mfa", "sum-pca", "mcoa",
                              "rcon-pca", "ridge-gca", "r-maxvar")){

    setSuperbloc()

    if (tolower(type) %in% c("maxvar-b", "gcca", "niles", "maxvar")){
      scheme   <- setScheme("factorial")
      tau      <- setTau(rep(0, J+1))
    }

    else if (tolower(type) == "hpca"){
      scheme   <- function(x) x^4
      tau      <- setTau(c(rep(1, J), 0))
    }

    else if (tolower(type) %in% c("maxvar-a", "cpca", "cpca-w", "mfa", "sum-pca", "mcoa")){
      scheme   <- setScheme("factorial")
      tau      <- setTau(c(rep(1, J), 0))
    }

    #TODO: verify these three last algo parameters

    else if (tolower(type) == "rcon-pca")
      tau <- warnSuper(tau)

    else if (tolower(type) == "ridge-gca"){
      scheme   <- setScheme("factorial")
      tau      <- setTau(c(tau[1:J], 0))
    }

    else if (tolower(type) == "r-maxvar"){
      scheme   <- setScheme("factorial")
      tau <- warnSuper(tau)
    }

  }

  else if(length(grep("[sr]gcca", tolower(type))) != 1){
    stop("Wrong type of analysis. Please select one among the following list: rgcca, cpca-w, gcca, hpca, maxbet-b, maxbet, maxdiff-b, maxdiff, maxvar-a, maxvar-b, maxvar, niles, r-maxvar, rcon-pca, ridge-gca, sabscor, ssqcor, ssqcor, ssqcov-1, ssqcov-2, ssqcov, sum-pca, sumcor, sumcov-1, sumcov-2, sumcov., sabscov, plspm\n")
  }

  if(length(warn.msg.super) > 0){

    if( length(warn.msg.super) > 1 )
      warn.msg.super = paste(warn.msg.super, collapse = ", ")

    warning(paste("Because of the use of a superblock, ", warn.msg.super,
                  " for the superblock was the one of the first block.\n", sep=""),
            call. = FALSE, immediate. = TRUE)
  }

  opt$blocks = A; opt$scheme = scheme;  opt$tau = tau;  opt$ncomp = ncomp;  opt$connection = C;  opt$superblock = superblock

  return(opt)
}

rgcca.analyze = function(blocks, connection = 1 - diag(length(A)), tau = rep(1, length(blocks)),
                         ncomp = rep(2, length(blocks)), scheme = "factorial", scale = TRUE,
                         init = "svd", bias = TRUE, type = "rgcca", verbose = TRUE){

  WARN = FALSE

  for (i in 1:length(blocks)){
    if( ncol(blocks[[i]]) > 1000 ){
      # if( (type == "sgcca" && tau > 0.3) || type != "sgcca" )
        WARN = TRUE
    }
  }

  if (WARN & verbose)
    warning("RGCCA in progress ...\n", immediate. = TRUE, call. = FALSE)

  if(tolower(type) =="sgcca"){
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

bootstrap_k = function(blocks, connection = 1 - diag(length(blocks)), tau = rep(1, length(blocks)),
                       ncomp = rep(2, length(blocks)), scheme = 'factorial', scale = TRUE,
                       init = "svd", bias = TRUE, type = "rgcca", verbose = FALSE){
  # Shuffle rows
  id_boot = sample(NROW(blocks[[1]]), replace = T)

  # Scale the blocks
  if( !is.null(attr(blocks, "scaled:center"))){
    # If blocks are already scaled

    if(!is.null(attr(blocks, "scaled:scale")))
      boot_blocks  = lapply(blocks, function(x) scale(x[id_boot, ],
                                                      center = attr(blocks, "scaled:center"),
                                                      scale = attr(blocks, "scaled:scale"))
                            / sqrt(ncol(x)) )
    else
      boot_blocks  = lapply(blocks, function(x) scale(x[id_boot, ],
                                                      center = attr(blocks, "scaled:center"),
                                                      scale = F))

  }else{

    if(isTRUE(scale))
      boot_blocks  = lapply(blocks, function(x) scale2(x[id_boot, ], bias = bias) / sqrt(ncol(x)) )
    else
      boot_blocks = lapply(blocks, function(x) scale2(x[id_boot, ], scale = F))
  }

  boot_blocks = removeColumnSdNull(boot_blocks)

  # Get boostraped weights
  w = rgcca.analyze(boot_blocks, connection, tau = tau,  ncomp = ncomp, scheme = scheme,
                scale = FALSE, init = init, bias = bias, type = type, verbose = verbose)$a

  # Add removed variables
  missing_var = lapply(1:length(blocks), function(x) setdiff(colnames(blocks[[x]]), rownames(w[[x]])))
  missing_tab = lapply(1:length(missing_var),
                       function(x) matrix(0,
                                        length(missing_var[[x]]),
                                        ncomp[x],
                                        dimnames = list(missing_var[[x]], 1:ncomp[x]))
                                      )
  # bug mapply with pca
  w = lapply(1:length(w), function(x) rbind(w[[x]], missing_tab[[x]]))
  w = lapply(1:length(w), function(x) w[[x]][ colnames(blocks[[x]]), ])

  return(w)
}

# Examples
# library(RGCCA)
# data("Russett")
# blocks3 = list(agriculture = Russett[, 1:3], industry = Russett[, 4:5], politic = Russett[, 6:11] )
# bootstrap(blocks3)
bootstrap = function(blocks, n_boot = 5, connection = 1 - diag(length(blocks)), tau = rep(1, length(blocks)),
                       ncomp = rep(2, length(blocks)), scheme = 'factorial', scale = TRUE,
                       init = "svd", bias = TRUE, type = "rgcca", nb_cores = NULL){

  if(any(unlist(lapply(blocks, ncol) > 1000)))
    verbose = TRUE

  if(is.null(nb_cores) )
    nb_cores = detectCores() - 1

  w1 = bootstrap_k(blocks, connection, tau, ncomp, scheme, scale, init, bias, type)

  W = parallel::mclapply(1:(n_boot-1), function(x) {

    print(x)

    w = bootstrap_k(blocks, connection, tau, ncomp, scheme, scale, init, bias, type)

    # Test on the sign of the correlation
    for(k in 1:length(blocks)){
      for (j in 1:ncol(w[[k]])){
        if (cor(w1[[k]][, j], w[[k]][, j]) < 0){
          w[[k]][, j] = -1 * w[[k]][, j]
        }
      }
    }

    return(w)

  }, mc.cores = nb_cores)

  return(c(list(w1), W))
}

#' list of list weights (one per bootstrap per blocks)
plotBootstrap = function(W, comp = 1, n_mark = 100, i_block = NULL){

  J = names(W[[1]])

  if ( is.null(i_block) )
    i_block = length(W[[1]])

  if(comp > min(unlist(lapply(W, function(x) lapply(x, function(z) ncol(z))))))
    stop("Selected dimension was not associated to every blocks", call. = FALSE)

  W_select = Reduce(rbind, lapply(W, function(x) x[[i_block]][, comp]) )
  stats = apply(W_select, 2,  function(x) c(mean(x), sd(x)))

  df = data.frame(cbind(stats[1, ],
              stats[1, ] - stats[2, ],
              stats[1, ] + stats[2, ]))

  if (  any(names(W[[1]]) == "Superblock") & i_block == length(J) )
    df$color = as.factor(getBlocsVariables(W[[1]]))

  df = data.frame(getRankedValues(df,  allCol = T), order = nrow(df):1)

  if(nrow(df) > n_mark)
    df = df[1:n_mark, ]

  if (  any(names(W[[1]]) == "Superblock") & i_block == length(J) ){
    color2 = factor(df$color); levels(color2) = colorGroup(color2)
    levels(df$color) = rev(levels(df$color))
    p = ggplot(df,
               aes(order,
                   df[, 1],
                   fill = color))
  }else{
    color2 ="black"
    p = ggplot(df, aes(order,
                        df[, 1],
                        fill = abs( df[, 1])  ) )
  }

  p = plotHistogram(p, df, "Average variable weights", as.character(color2)) +
    geom_errorbar(aes(ymin = X2, ymax = X3), color ="gray40")

  if (  any(names(W[[1]]) == "Superblock") & i_block == length(J) )
    p = p + scale_fill_manual(values = colorGroup(J),
                      limits = J[-length(J)],
                      labels = rev(J[-length(J)]))

    return(p)
}

scaling = function(blocks, scale = TRUE, bias = TRUE){

  if(scale){
    lapply(blocks, function(x)
      scale2(x, bias = bias) / sqrt(ncol(x)) )
  }else{
    lapply(blocks, function(x)
      scale2(x, scale = F))
  }
}
