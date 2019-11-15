#' Get the blocs of each variables
#'
#' Get a vector of block names for each corresponding variable. The last block 
#' is considered as the superblock and ignored.
#'
#' @param df A list of matrix where their names are those of the blocks and the 
#' superblock and their rows are named after their variables
#' @param collapse A boolean to combine the variables of each blocks as result
#' @return A vector of character giving block names for each corresponding 
#' variable.
#' @seealso \code{\link[RGCCA]{rgcca}}, \code{\link[RGCCA]{sgcca}}
#' @examples
#' rgcca.res = list(a = rep(NA, 4))
#' names(rgcca.res$a) = LETTERS[seq_len(4)]
#' getBlocsVariables(rgcca.res)
#' # a, b, c
getBlocsVariables <- function(df, collapse = FALSE) {
    
    if (!collapse)
        bl.names <- names(df)[-length(df)]
    else
        bl.names <- names(df)

    res <- rep(
        bl.names,
        sapply(
            df[seq_len(length(df) - as.integer(!collapse))],
            function(x) nrow(as.matrix(x))
        )
    )
    
    names(res) <- unlist(lapply(df[bl.names], row.names))

    return(res)
}
