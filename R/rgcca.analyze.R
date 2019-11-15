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
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
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

    WARN <- FALSE

    for (i in seq_len(length(blocks))) {
        if (ncol(blocks[[i]]) > 1000) {
            # if( (type <-<- "sgcca" && tau > 0.3) || type !<- "sgcca" )
            WARN <- TRUE
        }
    }

    if (WARN & verbose)
        message("RGCCA in progress ...")

    if (tolower(type) == "sgcca") {
        func <- sgcca
        par <- "c1"
    } else{
        func <- rgcca
        par <- "tau"
    }

    func.complete <- quote(
        func(
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
    func.complete[[par]] <- tau

    func.res <- eval(as.call(func.complete))
    names(func.res$a) <- names(blocks)

    return(func.res)
}
