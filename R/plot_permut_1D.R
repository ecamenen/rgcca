#' Plot permuation in 2D
#' 
#' Plot permuation in 2D
#' 
#' @param perm A permutation object from a RGCCA analyse
#' @param An string giving the type of the index to look at (among 'crit' for
#'  the RGCCA criterion and 'zstat' for the pseudo Z-score)
#' @examples
#' data("Russett")
#' A = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' perm <- rgcca_permutation(A, nperm = 3, n_cores = 1)
#' plot_permut_2D(perm)
#' perm <- rgcca_permutation(A, p_c1 = TRUE, nperm = 2, n_cores = 1)
#' plot_permut_2D(perm)
#' @export
plot_permut_2D <- function(perm, type = "zstat"){

    match.arg(type, c("crit", "zstat"))

    y <- unlist(perm[type])
    best <- which.max(y)

    plot(
        seq(NROW(perm$penalties)),
        y,
        main = paste0("Permutation scores \n(best value : ", paste(round(perm$penalties[best, ],2), collapse = ","), ")"),
        ylim = c(0, max(y)),
        xlab = "Index of combination",
        ylab = type,
        type = "l",
        axes = F,
        cex.lab = 1.5, font.lab = 3, font.axis = 3, cex.main = 2, cex = 1, lwd = 3
    )
    points(
        best,
        y[best],
        cex = 3,
        col = "red",
        pch = "+"
    )
    abline(
        v = best,
        col = "red"
    )
    # n <- seq(nrow(perm$penalties))
    # axis(1, lwd = 4, las = 2, at = n, labels = sapply(n, function(x) paste(round(perm$penalties[x, ],2), collapse = ",")))
    axis(1, lwd = 4)
    axis(2, lwd = 4)

    if (type == "zstat")
        abline(
            h = c(1.96, 2.58, 3.29),
            lty = 2,
            col = "grey",
            lwd = 4
        )
    else
        for (i in seq(NCOL(perm$permcrit)))
            points(
                seq(NROW(perm$penalties)),
                perm$permcrit[, i],
                col = "grey",
                type = "l"
            )

}

# plot_permut_2D <- function(perm, i_block = 1) {
# 
#     zstat <- cbind(
#         perm$penalties[, i_block], 
#         z = perm$zstat
#     )
# 
#     plot(
#         zstat[, 1],
#         zstat[, 2],
#         type = "o",
#        # mar = c(5.1, 5.1, 5.1, 2.1),
#         xlab = "C1",
#         ylab = "Z-score",
#         pch = "+",
#         main = colnames(perm$penalties)[i_block],
#         axes = F,
#         cex.lab = 1.5, font.lab = 3, font.axis = 3, cex.axis = 0.8, cex.main = 2, cex = 1, lwd = 3
#     )
#     points(
#         zstat[which.max(zstat[, 2]), 1],
#         zstat[which.max(zstat[, 2]), 2],
#         cex = 3,
#         col = "red",
#         pch = "+"
#     )
#     abline(
#         h = c(1.96, 2.58, 3.29),
#         lty = 2,
#         col = "grey",
#         lwd = 4
#     )
#     axis(1, lwd = 4)
#     axis(2, lwd = 4)
# }

