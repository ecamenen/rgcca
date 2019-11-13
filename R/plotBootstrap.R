#' Plot a bootstrap
#'
#' Plot the top variables from a bootstrap
#'
#' @inheritParams plotHistogram
#' @inheritParams plotVariablesSpace
#' @param show.boot A boolean to show the bootstrap mean and sd on the graphic
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' boot = bootstrap(blocks, rgcca.res, 2, FALSE)
#' selected.var = getBootstrap(rgcca.res, boot)
#' plotBootstrap(selected.var, rgcca.res)
#' @export
plotBootstrap <- function(
    df,
    rgcca,
    superblock = TRUE,
    show.boot = TRUE,
    n_mark = 30) {

    color <- intneg <- intpos <- NULL
    J <- names(rgcca$a)

    if (nrow(df) > n_mark)
    df <- df[seq_len(n_mark), ]

    if (superblock) {
        color2 <- factor(df$color)
        levels(color2) <- colorGroup(color2)
        p <- ggplot(df, aes(order, mean, fill = color))
    } else{
        color2 <- "black"
        p <- ggplot(df, aes(order, mean, fill = abs(mean)))
    }

    p <- plotHistogram(p, df, "Variable mean", as.character(color2))

    if (show.boot) {
        p <- p +
            geom_line(aes(x = order, y = mean), inherit.aes = FALSE, lwd = 0.7) +
            geom_point(aes(x = order, y = mean), inherit.aes = FALSE, size = 1.5)

        if (is(rgcca, "rgcca" ))
        p <- p +
        geom_errorbar(aes(ymin = intneg, ymax = intpos))
    }

    if (superblock)
    col <- J
    else
    col <- J[-length(J)]

    if (superblock) {
        matched <- match(rev(unique(df$color)), col)
        p <- orderColorPerBlocs(rgcca$a, p, matched, superblock)
    }

    return(p)
}