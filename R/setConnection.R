#' Create a matrix corresponding to a connection between the blocks
#'
#' @param blocks A list of matrix
#' @param superblock A boolean giving the presence (TRUE) / absence (FALSE) of
#' a superblock
#' @param file A character giving the path of a file used as a response
#' @param sep A character giving the column separator
#' @param rownames An integer corresponding to the column number of the row
#' names (NULL otherwise)
#' @param h A bolean giving the presence or the absence of the header
#' @return A matrix corresponding to the connection between the blocks
#' @examples
#' \dontrun{
#' blocks = lapply(seq_len(4), function(x) matrix(runif(47 * 5), 47, 5))
#' setConnection (blocks, 'data/connection.tsv')
#' }
#' @export
setConnection <- function(
    blocks,
    superblock = FALSE,
    file = NULL,
    sep = "\t",
    h = FALSE,
    rownames = NULL) {

    J <- length(blocks)

    if (superblock) {
        connection <- matrix(0, J, J)
        connection[seq_len(J - 1), J] <- connection[J, seq_len(J - 1)] <- 1

    } else if (is.null(file))
        connection <- 1 - diag(J)
    else {
        isXls <- (length(grep("xlsx?", file)) == 1)

        if (!isXls)
            connection <- loadData(
                f = file,
                sep = sep,
                rownames = rownames,
                h = h
            )
        # else
        # connection = loadExcel(f = file, sheet = 1, rownames = rownames, h = h)
    }

    checkConnection(connection, blocks)

    return(connection)
}
