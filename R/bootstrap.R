#' Compute bootstrap
#'
#' Computing boostrap of RGCCA
#'
#' @inheritParams rgcca.analyze
#' @inheritParams plot_var_2D
#' @param n_boot A integer for the number of boostrap
#' @param n_cores An integer for the number of cores used in parallelization
#' @return A list of RGCCA bootstrap weights
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' bootstrap(blocks, rgcca.res, 2, FALSE)
#' @export
bootstrap <- function(
    blocks,
    rgcca,
    n_boot = 5,
    scale = TRUE,
    init = "svd",
    bias = TRUE,
    n_cores = parallel::detectCores() - 1) {

    if (n_cores == 0)
        n_cores <- 1

    if (any(unlist(lapply(blocks, ncol) > 1000)))
        verbose <- TRUE

    w1 <- rgcca$a

    cat("Bootstrap in progress...")

    W <- parallel::mclapply(seq(n_boot), function(x) {

        w <- bootstrap_k(
            blocks,
            rgcca,
            scale,
            init,
            bias)

        # Test on the sign of the correlation
        for (k in seq(length(blocks))) {
            for (j in seq(ncol(w[[k]]))) {
                if (cor(w1[[k]][, j], w[[k]][, j]) < 0)
                    w[[k]][, j] <- -1 * w[[k]][, j]
            }
        }

        return(w)

    }, mc.cores = n_cores)

    cat("OK", append = TRUE)

    return(W)
}
