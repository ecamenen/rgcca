#' Histogram of a fingerprint
#'
#' Histogram of the higher outer weight vectors for a component of a block 
#' (by default, the superblock or the last one) analysed by R/SGCCA
#'
#' @inheritParams plotVariablesSpace
#' @param comp An integer giving the index of the analysis components
#' of a superblock
#' @param type A string giving the criterion to selects biomarkers : either 
#' "cor" for correlation between the component and the block
#' or "weight" for the weight of the RGCCA
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' weights = lapply(seq_len(3), function(x) matrix(runif(7*2), 7, 2))
#' for(i in seq(3))
#' row.names(weights[[i]]) <- paste0(letters[i],
#'      letters[seq_len(nrow(weights[[i]]))])
#' weights[[4]] = Reduce(rbind, weights)
#' rgcca.res = list(a = weights)
#' names(rgcca.res$a) = LETTERS[seq_len(4)]
#' # With the 1rst component of the superblock
#' plotFingerprint(rgcca.res, NULL, 1, TRUE, type = "weigth")
#' # With the 2nd component of the 1rst block by selecting the ten higher weights
#' plotFingerprint(rgcca.res, NULL, 2, FALSE, 10, 1, type = "weigth")
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' plotFingerprint(rgcca.res, blocks, collapse = TRUE)
#' @export
plotFingerprint <- function(
    rgcca,
    blocks = NULL,
    comp = 1,
    superblock = TRUE,
    n_mark = 100,
    i_block = length(rgcca$a),
    type = "cor",
    collapse = FALSE,
    cex = 1,
    subtitle_cex = 16 * cex,
    axis_text_cex = 10 * cex) {

    df <- getVariablesIndexes(
        rgcca = rgcca,
        blocks = blocks,
        comp_x = comp,
        comp_y = comp,
        i_block = i_block,
        type = type,
        superblock = superblock,
        n_mark = n_mark,
        collapse = collapse,
        removeVariable = FALSE
    )
    
    J <- names(rgcca$a)

    title <- ifelse(type == "cor",
            "Variable correlations with",
            "Variable weights on")

    # sort in decreasing order
    df <- data.frame(getRankedValues(df, 1, TRUE), order = nrow(df):1)

    # max threshold for n
    if (nrow(df) >= n_mark)
        df <- df[seq(n_mark), ]

    # if the superblock is selected, color the text of the y-axis according
    # to their belonging to each blocks
    if (superblock & (collapse | (i_block == length(rgcca$a)))) {
        color <- factor(df$resp)
        levels(color) <- colorGroup(color)
        p <- ggplot(df, aes(order, df[, 1], fill = df$resp))
    } else {
        color <- "black"
        p <- ggplot(df, aes(order, df[, 1], fill = abs(df[, 1])))
    }

    p <- plotHistogram(p,
            df,
            title,
            as.character(color),
            cex = cex,
            subtitle_cex = subtitle_cex,
            axis_text_cex = axis_text_cex
        ) +
        labs(subtitle = printAxis(rgcca, comp, i_block))

    # If some blocks have any variables in the top hit, selects the ones
    # corresponding
    if (collapse)
        col <- J
    else
        col <- J[-length(J)]

    matched <- match(rev(unique(df$resp)), col)

    # Force all the block names to appear on the legend
    if (length(color) != 1)
        p <- orderColorPerBlocs(rgcca$a, p, matched, collapse)
    if ( !superblock | (!collapse & i_block != length(rgcca$a)))
            p <- p + theme(legend.position = "none")

    return(p)
}
