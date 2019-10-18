#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks, ncomp = c(3, 2, 4))
#' getVariablesIndexes(rgcca.res, blocks, superblock = FALSE)
#' rgcca.res = rgcca.analyze(blocks[c(1,3)], ncomp = c(3,4))
#' getVariablesIndexes(rgcca.res, c(blocks[1], blocks[3]), comp_z = 3, i_block = 1, collapse = TRUE)
#' getVariablesIndexes(rgcca.res, c(blocks[1], blocks[3]), 1, 2, 3, "weights", collapse = TRUE, n_mark = 3)
#' getVariablesIndexes(rgcca.res, blocks, collapse = TRUE)
getVariablesIndexes <- function(
    rgcca,
    blocks,
    comp_x = 1,
    comp_y = 2,
    comp_z = NULL,
    i_block = length(blocks),
    type = "cor",
    superblock = TRUE,
    n_mark = 100,
    collapse = FALSE,
    removeVariable = TRUE) {

    x <- y <- selectedVar <- NULL
    
    if (collapse) {
        superblock <- TRUE
        blocks.all <- blocks
        blocks <- rep(list(Reduce(cbind, blocks)), length(blocks))
        names(blocks) <- names(blocks.all)
    }

    df <- getVar(rgcca, blocks, comp_x, comp_y, comp_z, i_block, type, collapse)

    if (is(rgcca, "sgcca")) {

        if (collapse)
            J <- seq(length(rgcca$a))
        else
            J <- i_block

        selectedVar <- unlist(
            lapply(J,
                function(x)
                    apply(
                        sapply(
                            c(comp_x, comp_y, comp_z[comp_z >= rgcca$ncomp[x]]),
                            function(y) rgcca$a[[x]][, y] != 0), 
                        1, 
                        function(z) Reduce("|", z)
                    )
                )
            )
        df <- df[ names(which(selectedVar)), ]

    }

    if (n_mark > nrow(df))
        n_mark <- nrow(df)

    # TODO: function in other place
    if (removeVariable) {
        selectedVar <- unique(as.vector(
            sapply(seq(length(c(comp_x, comp_y, comp_z))), function(x)
                row.names(data.frame(df[order(abs(df[, x]), decreasing = TRUE), ])[seq(n_mark), ]))
        ))
        df <- df[selectedVar, ]
    }

    # group by blocks
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
