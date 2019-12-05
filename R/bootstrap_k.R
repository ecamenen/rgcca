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

    # Shuffle rows
    id_boot <- sample(NROW(rgcca$blocks[[1]]), replace = TRUE)

    if (isTRUE(scale))
        boot_blocks <- lapply(
            rgcca$blocks, 
            function(x)
                scale(
                    x[id_boot,],
                    center = attr(rgcca$blocks, "scaled:center"),
                    scale = attr(rgcca$blocks, "scaled:scale")
                ) / sqrt(NCOL(x)))
    else
        boot_blocks <- lapply(rgcca$blocks, function(x)
            scale2(x[id_boot, ], scale = FALSE))

    boot_blocks <- remove_null_sd(boot_blocks)

    if (is(rgcca, "sgcca"))
        tau <- rgcca$c1
    else
        tau <- rgcca$tau
    
    if (rgcca$superblock) {
        tau <- tau[-length(tau)]
        rgcca$ncomp <- rgcca$ncomp[-length(rgcca$ncomp)]
        rgcca$blocks <- rgcca$blocks[-length(rgcca$blocks)]
        rgcca$C <- rgcca$C[-NROW(rgcca$C), -NCOL(rgcca$C)]
    }

    # Get boostraped weights
    w <- rgcca.analyze(
        boot_blocks,
        rgcca$C,
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
        seq(length(rgcca$blocks)),
        function(x) setdiff(colnames(rgcca$blocks[[x]]), rownames(w[[x]]))
    )
    missing_tab <- lapply(
        seq(length(missing_var)),
        function(x)
            matrix(
                0,
                length(missing_var[[x]]),
                rgcca$ncomp[x],
                dimnames = list(missing_var[[x]], seq(rgcca$ncomp[x]))
        ))

    # bug mapply with pca
    w <- lapply(seq(length(w)), function(x) rbind(w[[x]], missing_tab[[x]]))
    w <- lapply(seq(length(w)), function(x) w[[x]][colnames(rgcca$blocks[[x]]), ])

    names(w) <- names(rgcca$blocks)
    return(w)
}
