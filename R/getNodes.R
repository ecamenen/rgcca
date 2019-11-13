#' Creates the nodes for a design matrix
#' 
#' @inheritParams plotVariablesSpace
#' @inheritParams select_type
#' @return A dataframe with blocks in rows and the number of variables, of rows 
#' and tau or c1 in columns
#' @examples
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' getNodes(blocks, rgcca = rgcca.res)
getNodes <- function(blocks, tau = NULL, rgcca = NULL) {

    if (!is.null(rgcca) & is(rgcca, "sgcca")) {
        par.rgcca <- "c1"
        par.name <- "sparsity"
    } else
        par.rgcca <- par.name <- "tau"

    if (any(tau == "optimal")) {
        if (!is.null(rgcca))
            tau <- unlist(lapply(seq_len(ncol(rgcca[[par.rgcca]])), function(x)
                    Reduce(paste, round(rgcca[[par.rgcca]][, x], 2))))
        else
            tau <- rep(NA, length(blocks))
    }

    if (is.null(tau)) {
        if (is.matrix(rgcca[[par.rgcca]]))
            tau <-  unlist(lapply(seq_len(ncol(rgcca[[par.rgcca]])), function(x)
                    Reduce(paste, round(rgcca[[par.rgcca]][, x], 2))))
        else
            tau <- rgcca[[par.rgcca]]
    }

    nrow <- unlist(lapply(blocks, function(x)
            ifelse(
                is.null(attributes(x)$nrow),
                nrow(blocks[[1]]),
                attributes(x)$nrow
            )))

    values <- list(names(blocks), unlist(lapply(blocks, NCOL)), nrow, tau)
    nodes <- as.data.frame(matrix(unlist(values), length(blocks), length(values)))
    colnames(nodes) <- c("id", "P", "nrow", par.name)

    return(nodes)
}
