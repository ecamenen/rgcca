#' Plot of variables space
#'
#' Correlation circle highlighting the contribution of each variables to the
#' construction of the RGCCA components
#' @inheritParams plot_ind
#' @param blocks A list of matrix
#' @param superblock A boolean giving the presence (TRUE) / absence (FALSE)
#' of a superblock
#' @param remove_var A bolean to keep only the 100 variables of each
#' component with the biggest correlation#'
#' @param n_mark An integer giving the number of top variables to select
#' @param collapse A boolean to combine the variables of each blocks as result
#' @examples
#' setMatrix = function(nrow, ncol, iter = 3) lapply(seq(iter),
#'     function(x) matrix(runif(nrow * ncol), nrow, ncol))
#' blocks = setMatrix(10, 5)
#' blocks[[4]] = Reduce(cbind, blocks)
#' for (i in seq(4))
#'     colnames(blocks[[i]]) = paste0( LETTERS[i],
#'     as.character(seq(NCOL(blocks[[i]]))))
#' coord = setMatrix(10, 2, 4)
#' a = setMatrix(5, 2)
#' a[[4]] = matrix(runif(15 * 2), 15, 2)
#' AVE_X = lapply(seq(4), function(x) runif(2))
#' rgcca_out = list(Y = coord, a = a, AVE = list(AVE_X = AVE_X))
#' names(rgcca_out$a) = LETTERS[seq(4)]
#' # Using a superblock
#' plot_var_2D(rgcca_out, 1, 2, TRUE)
#' # Using the first block
#' plot_var_2D(rgcca_out, 1, 2, FALSE, 1)
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca_out = rgcca.analyze(blocks)
#' # Without superblock but with the of all variables to the first block
#' plot_var_2D(rgcca_out, collapse = TRUE)
#' @export
plot_var_2D <- function(
    rgcca,
    compx = 1,
    compy = 2,
    superblock = TRUE,
    i_block = length(blocks),
    text = TRUE,
    remove_var = TRUE,
    n_mark = 100,
    collapse = FALSE,
    no_overlap = FALSE,
    cex = 1,
    subtitle_cex = 16 * cex,
    pch_text_cex = 3 * cex,
    axis_title_cex = 19 * cex) {

    x <- y <- NULL
    
    # PCA case: remove the superblock in legend
    if (identical(rgcca$blocks[[1]], rgcca$blocks[[2]]))
        superblock <- FALSE

    df <- get_ctr2(
        rgcca = rgcca,
        compx = compx,
        compy = compy,
        i_block = i_block,
        type = "cor",
        superblock = superblock,
        n_mark = n_mark,
        collapse = collapse,
        remove_var = remove_var
    )

    circleFun <- function(center = c(0, 0), diameter = 2, npoints = 100) {
        r <- diameter / 2
        tt <- seq(0, 2 * pi, length.out = npoints)
        xx <- center[1] + r * cos(tt)
        yy <- center[2] + r * sin(tt)
        return(data.frame(x = xx, y = yy))
    }

    p <- plot2D(
        rgcca,
        df,
        "Variable",
        df$resp,
        "Blocks",
        compx,
        compy,
        i_block,
        text = text,
        collapse =  collapse,
        no_overlap = no_overlap,
        cex = cex,
        subtitle_cex = subtitle_cex,
        pch_text_cex = pch_text_cex,
        axis_title_cex = axis_title_cex
        ) +
        geom_path(
            aes(x, y),
            data = circleFun(),
            col = "grey",
            size = 1
        ) +
        geom_path(
            aes(x, y),
            data = circleFun() / 2,
            col = "grey",
            size = 1,
            lty = 2
        )
    
    # remove legend if not on superblock
    if (!superblock || i_block != length(rgcca$a))
        p + theme(legend.position = "none")
    else
        p
}
