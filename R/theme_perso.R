# Default theme for ggplot
theme_perso <- function(cex = 1, subtitle_cex = 16 * cex) {
    theme(
        legend.text = element_text(size = 13 * cex),
        legend.title = element_text(face = "bold.italic", size = subtitle_cex ),
        plot.title = element_text(
            size = 25 * cex,
            face = "bold",
            hjust = 0.5,
            margin = margin(0, 0, 20, 0)
        )
    )
}
