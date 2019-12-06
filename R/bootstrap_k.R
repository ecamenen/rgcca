#' Compute bootstrap (internal)
#'
#' Internal function for computing boostrap of RGCCA
#'
#' @inheritParams rgcca.analyze
#' @inheritParams plot_var_2D
#' @return A list of RGCCA bootstrap weights
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca_out = rgcca.analyze(blocks)
#' bootstrap_k(rgcca_out, FALSE)
#' @export
bootstrap_k <- function(
    rgcca,
    scale = TRUE) {

    if (is(rgcca, "sgcca"))
        tau <- rgcca$c1
    else
        tau <- rgcca$tau

    blocks.all <- rgcca$blocks
    ncomp <- rgcca$ncomp

    if (rgcca$superblock) {
        rgcca$blocks <- rgcca$blocks[-length(rgcca$blocks)]
        rgcca$C <- NULL
    }

    # Shuffle rows
    id_boot <- sample(NROW(rgcca$blocks[[1]]), replace = TRUE)

    if (isTRUE(scale))
        boot_blocks <- lapply(
            rgcca$blocks, 
            function(x)
                scale(
                    x[id_boot,],
                    center = attr(x, "scaled:center"),
                    scale = attr(x, "scaled:scale")
                ) / sqrt(NCOL(x)))
    else
        boot_blocks <- lapply(rgcca$blocks, function(x)
            scale2(x[id_boot, ], scale = FALSE))

    boot_blocks <- remove_null_sd(boot_blocks)

    # Get boostraped weights
    w <- rgcca.analyze(
        boot_blocks,
        rgcca$C,
        superblock = rgcca$superblock,
        tau = tau,
        ncomp = rgcca$ncomp,
        scheme = rgcca$scheme,
        scale = FALSE,
        type = class(rgcca),
        verbose = FALSE,
        init = rgcca$init,
        bias = rgcca$bias,
        tol = rgcca$tol
    )$a

    # Add removed variables
    missing_var <- lapply(
        seq(length(w)),
        function(x) setdiff(colnames(blocks.all[[x]]), rownames(w[[x]]))
    )

    missing_tab <- lapply(
        seq(length(w)),
        function(x)
            matrix(
                0,
                length(missing_var[[x]]),
                ncomp[x],
                dimnames = list(missing_var[[x]], seq(ncomp[x]))
        ))

    # bug mapply with pca
    w <- lapply(seq(length(missing_tab)), function(x) {
        if (NROW(missing_tab[[x]]) != 0)
            rbind(w[[x]], missing_tab[[x]])
        else
            w[[x]]
        })

    w <- lapply(seq(length(w)), function(x) w[[x]][colnames(blocks.all[[x]]), ])

    names(w) <- names(blocks.all)
    return(w)
}
