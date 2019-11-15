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
    tau = rep(1, length(blocks)),
    ncomp = rep(2, length(blocks)),
    scheme = "factorial",
    scale = TRUE,
    init = "svd",
    bias = TRUE,
    type = "rgcca",
    verbose = TRUE) {

    warn_on <- FALSE

    for (i in seq(length(blocks))) {
        if (ncol(blocks[[i]]) > 1000) {
            # if( (type <-<- "sgcca" && tau > 0.3) || type !<- "sgcca" )
            warn_on <- TRUE
        }
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
            A = blocks,
            C = connection,
            scheme = scheme,
            ncomp = ncomp,
            scale = scale,
            verbose = FALSE,
            init = init,
            bias = bias
        )
    )
    func[[par]] <- tau

    func_out <- eval(as.call(func))
    names(func_out$a) <- names(blocks)

    return(func_out)
}
