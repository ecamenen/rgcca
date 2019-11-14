#' Compute bootstrap (internal)
#'
#' Internal function for computing boostrap of RGCCA
#'
#' @inheritParams rgcca.analyze
#' @inheritParams plotVariablesSpace
#' @return A list of RGCCA bootstrap weights
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' bootstrap_k(blocks, rgcca.res, FALSE)
#' @export
bootstrap_k <- function(
    blocks,
    rgcca,
    scale = TRUE,
    init = "svd",
    bias = TRUE) {

    # Shuffle rows
    id_boot <- sample(NROW(blocks[[1]]), replace = TRUE)

    if (isTRUE(scale))
        boot_blocks <- lapply(
            blocks, 
            function(x)
                scale(
                    x[id_boot,],
                    center = attr(blocks, "scaled:center"),
                    scale = attr(blocks, "scaled:scale")
                ) / sqrt(ncol(x)))
    else
        boot_blocks <- lapply(blocks, function(x)
            scale2(x[id_boot, ], scale = FALSE))

    boot_blocks <- removeColumnSdNull(boot_blocks)

    if (is(rgcca, "sgcca"))
        tau <- rgcca$c1
    else
        tau <- rgcca$tau

    # Get boostraped weights
    w <- rgcca.analyze(
            boot_blocks,
            rgcca$C,
            tau = tau,
            ncomp = rgcca$ncomp,
            scheme = rgcca$scheme,
            scale = FALSE,
            init = init,
            bias = bias,
            type = class(rgcca),
            verbose = FALSE
        )$a

    # Add removed variables
    missing_var <- lapply(seq_len(length(blocks)), function(x)
    setdiff(colnames(blocks[[x]]), rownames(w[[x]])))
    missing_tab <- lapply(
        seq_len(length(missing_var)),
        function(x)
            matrix(
                0,
                length(missing_var[[x]]),
                rgcca$ncomp[x],
                dimnames = list(missing_var[[x]], seq_len(rgcca$ncomp[x]))
        ))

    # bug mapply with pca
    w <- lapply(seq_len(length(w)), function(x)
    rbind(w[[x]], missing_tab[[x]]))
    w <- lapply(seq_len(length(w)), function(x)
    w[[x]][colnames(blocks[[x]]), ])

    names(w) <- names(blocks)
    return(w)
}
