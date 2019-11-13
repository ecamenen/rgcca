#' Plot a bootstrap in 2D
#'
#' Biplot of the top variables from a SGCCA bootstrap with the number of
#' non-zero occurences in x-axis and the boot-ratio (mean/sd) in y-axis.
#' Negative weights are colored in red and the positive ones are in green.
#'
#' @param b A matrix of boostrap
#' @param x A character for the column to plot in x-axis
#' @param y A character for the column to plot in y-axis
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks, type = "sgcca")
#' boot = bootstrap(blocks, rgcca.res, 2, FALSE)
#' selected.var = getBootstrap(rgcca.res, boot)
#' plotBootstrap2D(selected.var)
#' @export
plotBootstrap2D <- function(b, x = "br", y = "occ"){

    axis <- function(margin){
        element_text(
        face = "italic",
        size = AXIS_TITLE_CEX * 0.75,
        margin = margin
        )
    }

    ggplot(
    b,
    aes(
    x = abs(b[, x]),
    y = b[, y],
    label = row.names(b),
    color = as.factor(mean > 0)
    )
    ) +
        geom_text(
        size = PCH_TEXT_CEX * 0.75
        ) +
        labs(
        y = "Non-zero occurences",
        x = "Bootstrap-ratio",
        title = "Occurences selection\nby bootstrap"
        ) +
        theme_classic()  +
        theme_perso() +
        theme(
        legend.position = "none",
        axis.title.y = axis(margin(0, 20, 0, 0)),
        axis.title.x = axis(margin(20, 0, 0, 0))
        ) +
        scale_color_manual(values = colorGroup(seq(2)))
}