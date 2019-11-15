#' Plot a bootstrap in 1D
#'
#' Histogram of the best variables of an RGCCA bootstrap with, on the x-axis,
#' the number of non-zero occurrences (SGCCA) or the bootstrap-ratio
#' (mean/sd; RCCA). The bars are colored according to the average weight of
#' the boostrap  (according to an ascending gradient from red to blue)
#'
#' @param b A matrix of boostrap
#' @param x A character for the column used in the plot
#' @param y A character for the column to color the bars
#' @param n An integer giving the number maximum of top variables
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' boot = bootstrap(blocks, rgcca.res, 2, FALSE)
#' selected.var = getBootstrap(rgcca.res, boot)
#' plotBootstrap1D(selected.var)
#' @export
plotBootstrap1D <- function(
    b, 
    x = "occ", 
    y = "mean", 
    n = 50,    
    cex = 1,
    subtitle_cex = 16 * cex,
    axis_text_cex = 10 * cex) {

    if (!("occ" %in% colnames(b))) {
        title <- "Bootstrap ratio"
        x <- "br"
    }else
        title <- "Occurences selection\nby bootstrap"

    b <- head(b, n)
    p <- ggplot(b,
        aes(x = order,
            y = b[, x],
            fill = b[, y]))

    plotHistogram(
        p,
        b,
        title,
        "black",
        low_col = colorGroup(seq(3))[1],
        mid_col = "white",
        high_col = colorGroup(seq(3))[3],
        cex = cex,
        subtitle_cex = subtitle_cex,
        axis_text_cex = axis_text_cex) +
    labs(fill = "Mean weights")
}
