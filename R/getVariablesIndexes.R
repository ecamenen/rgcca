#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks, ncomp = c(3, 2, 4))
#' getVariablesIndexes(rgcca.res, blocks, superblock = FALSE)
#' getVariablesIndexes(rgcca.res, blocks, collapse = TRUE)
getVariablesIndexes <- function(
    rgcca,
    blocks,
    comp_x = 1,
    comp_y = 2,
    superblock = TRUE,
    i_block = NULL,
    text = TRUE,
    removeVariable = TRUE,
    type = "cor",
    n_mark = 100,
    collapse = FALSE,
    no_Overlap = FALSE) {

    x <- y <- selectedVar <- NULL

    if (is.null(i_block))
        i_block <- length(blocks)
    
    if (collapse) {
        superblock <- TRUE
        blocks.all <- blocks
        blocks <- rep(list(Reduce(cbind, blocks)), length(blocks))
        names(blocks) <- names(blocks.all)
    }

    df <- getVar(rgcca, blocks, comp_x, comp_y, i_block, type, collapse)

    if (is(rgcca, "sgcca")) {

        if (collapse)
            J <- seq(length(rgcca$a))
        else
            J <- i_block

        selectedVar <- unlist( lapply( J, function(x) varSelected(rgcca, x, comp_x) | varSelected(rgcca, x, comp_y) ) )
        df <- df[ names(which(selectedVar)), ]

    }

    if (n_mark < 2)
        n_mark <- nrow(df)

    if (removeVariable & nrow(df) > 2 * n_mark) {
        selectedVar <- unique(as.vector(unique(
            sapply(c(1, 2), function(x)
                row.names(data.frame(df[order(abs(df[, x]), decreasing = TRUE), ])[seq_len(n_mark), ]))
        )))
        df <- df[selectedVar, ]
    }

    # if superblock is selected, group by blocks
    if (superblock & (collapse | (i_block == length(rgcca$a)))) {

        if (collapse)
            resp <- getBlocsVariables(lapply(blocks.all, t), TRUE)
        else 
            resp <- getBlocsVariables(rgcca$a)
    
            if ( is(rgcca, "sgcca") | collapse)
                resp <- resp[row.names(df)]
            else {
                if (!is.null(selectedVar))
                    resp <- resp[
                        unlist(
                            lapply(
                                seq_len(length(selectedVar)),
                                function(x) which(colnames(blocks[[length(blocks)]]) == selectedVar[x])
                            )
                        )
                    ]
            }
    } else
        resp <- rep(1, NROW(df))

    data.frame(df, resp)
}
