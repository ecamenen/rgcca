#' Rank values of a dataframe in decreasing order
#'
#' @param df A dataframe
#' @param comp An integer giving the index of the analysis components
#' @param allCol A boolean to use all the column of the dataframe
#' @return A datafram with ordered values
#' @examples 
#' df = sapply(seq(2), function(x) runif(10))
#' getRankedValues(df)
getRankedValues <- function(df, comp = 1, allCol = TRUE) {
    
    ordered <- order(abs(df[, comp]), decreasing = TRUE)

    if (allCol)
        comp <- seq_len(ncol(df))

    res <- df[ordered, comp]

    if (!allCol)
        names(res) <- row.names(df)[ordered]

    return(res)
}
