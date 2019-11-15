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
#' selected.var = get_bootstrap(rgcca.res, boot)
#' plot_bootstrap_2D(selected.var)
#' @export
plot_bootstrap_2D <- function(
    b,
    x = "br",
    y = "occ",
    cex = 1,
    subtitle_cex = 16 * cex,
    pch_text_cex = 3 * cex,
    axis_title_cex = 19 * cex){

    axis <- function(margin){
        element_text(
        face = "italic",
        size = axis_title_cex * 0.75,
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
        size = pch_text_cex * 0.75
    ) +
    labs(
        y = "Non-zero occurences",
        x = "Bootstrap-ratio",
        title = "Occurences selection\nby bootstrap"
    ) +
    theme_classic() +
    theme_perso(cex, subtitle_cex) +
    theme(
        legend.position = "none",
        axis.title.y = axis(margin(0, 20, 0, 0)),
        axis.title.x = axis(margin(20, 0, 0, 0))
    ) +
    scale_color_manual(values = color_group(seq(2)))
}
