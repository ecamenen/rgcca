#' Plot a bootstrap in 2D
#'
#' Biplot of the top variables from a SGCCA bootstrap with the number of
#' non-zero occurences in x-axis and the boot-ratio (mean/sd) in y-axis.
#' Negative weights are colored in red and the positive ones are in green.
#'
#' @inheritParams plot2D
#' @param b A matrix of boostrap
#' @param x A character for the column to plot in x-axis
#' @param y A character for the column to plot in y-axis
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca_out = rgcca.analyze(blocks, type = "sgcca")
#' boot = bootstrap(rgcca_out, 2, FALSE)
#' selected.var = get_bootstrap(rgcca_out, boot)
#' plot_bootstrap_2D(selected.var)
#' @export
plot_bootstrap_2D <- function(
    b,
    x = "br",
    y = "occ",
    cex = 1,
    cex_sub = 16 * cex,
    cex_point = 3 * cex,
    cex_lab = 19 * cex){

    axis <- function(margin){
        element_text(
        face = "italic",
        size = cex_lab * 0.75,
        margin = margin
        )
    }

    ggplot(b,
        aes(
            x = abs(b[, x]),
            y = b[, y],
            label = row.names(b),
            color = as.factor(mean > 0)
    )) +
    geom_text(
        size = cex_point * 0.75
    ) +
    labs(
        y = "Non-zero occurences",
        x = "Bootstrap-ratio",
        title = "Occurences selection\nby bootstrap"
    ) +
    theme_classic() +
    theme_perso(cex, cex_sub) +
    theme(
        legend.position = "none",
        axis.title.y = axis(margin(0, 20, 0, 0)),
        axis.title.x = axis(margin(20, 0, 0, 0))
    ) +
    scale_color_manual(values = color_group(seq(2)))
}
