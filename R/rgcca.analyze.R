#' Performs a r/sgcca
#'
#' Performs a r/sgcca with predefined parameters
#' @inheritParams select_analysis
#' @param scale A boolean scaling the blocks
#' @param init A character among "svd" (Singular Value Decompostion) or "random"
#' for alorithm initialization
#' @param bias A boolean for a biased variance estimator
#' @param type A character giving the type of analysis
#' @param verbose A boolean to display the progress of the analysis
#' @return A RGCCA object
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.analyze(blocks)
#' @export
rgcca.analyze <- function(
    blocks,
    connection = 1 - diag(length(blocks)),
    response = NULL,
    superblock = TRUE,
    tau = rep(1, length(blocks)),
    ncomp = rep(2, length(blocks)),
    type = "rgcca",
    verbose = TRUE,
    scheme = scheme,
    scale = TRUE,
    ...) {

    tau <- elongate_arg(tau, blocks)
    ncomp <- elongate_arg(ncomp, blocks)

    # TODO: elongate_arg avant select_analysis
    opt <- select_analysis(
        blocks = blocks,
        connection = connection,
        tau = tau,
        ncomp = ncomp,
        scheme = scheme,
        superblock = superblock,
        type  = type
    )

    blocks <- scaling(blocks, scale)
    superblock <- check_superblock(response, opt$superblock)
    blocks <- set_superblock(blocks, opt$superblock, type)

    
    opt$blocks <- blocks # TODO
    
    if (!is.null(response)) {
        response <- check_blockx("response", response, blocks)
        par <- c("blocks", "ncomp", "tau")
        for (i in seq(length(par)))
            opt[[par[i]]] <- c(opt[[par[i]]][-response], opt[[par[i]]][response])
    }

    blocks <- opt$blocks

    if (!is.matrix(opt$connection))
        opt$connection <- set_connection(
            blocks,
            (opt$superblock | !is.null(response))
        )
    
    check_connection(opt$connection, blocks)
    opt$tau <- check_tau(opt$tau, blocks)
    opt$tau <- check_spars(blocks, opt$tau, type)
    opt$ncomp <- check_ncomp(opt$ncomp, blocks)

    warn_on <- FALSE

    if (any(sapply(opt$blocks, NCOL) > 1000)) {
            # if( (type <-<- "sgcca" && tau > 0.3) || type !<- "sgcca" )
            warn_on <- TRUE
    }

    if (warn_on & verbose)
        message("RGCCA in progress ...")

    if (tolower(type) == "sgcca") {
        gcca <- sgcca
        par <- "c1"
    } else{
        gcca <- rgcca
        par <- "tau"
    }

    func <- quote(
        gcca(
            A = opt$blocks,
            C = opt$connection,
            ncomp = opt$ncomp,
            verbose = FALSE,
            scheme = opt$scheme,
            scale = FALSE,
            ...
        )
    )
    func[[par]] <- opt$tau

    func_out <- eval(as.call(func))
    names(func_out$a) <- names(opt$blocks)
    func_out$blocks <- opt$blocks
    func_out$superblock <- opt$superblock

    #TODO: superclass = inherit type
    invisible(func_out)
}
