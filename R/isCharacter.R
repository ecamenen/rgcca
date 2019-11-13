#' Test for character vector
#'
#' Tests if a dataframe is composed only by qualitative variables
#'
#' @param x A matrix or a vector
#' @return A bolean for the presence (FALSE) or the absence (TRUE) of at least
#' one quantitative variable
#' @examples
#' x = matrix(c(runif(10), LETTERS[seq_len(10)]), 10, 2)
#' isCharacter(x)
#' # FALSE TRUE
#' isCharacter(LETTERS[seq_len(10)])
#' # TRUE
#' @export
isCharacter <- function(x) {
    # is. character() consider a string with '1.2' as a character, not this function.
    # NA are produced by converting a character into an integer as.vector, avoid
    # factors of character in integer without NA

    # NA tolerance :

    if (is.matrix(x)) {
        test <- sapply(seq_len(NCOL(x)), function(i)
        unique(is.na(
        tryCatch(
        as.integer(
        na.omit(as.vector(x[, i])[as.vector(x[, i]) != "NA"])),
        warning = function(w)
        return(NA)
        ))))
    } else
    test <- unique(is.na(
    tryCatch(
    as.integer(na.omit(as.vector(x)[as.vector(x) != "NA"])),
    warning = function(w)
    return(NA)
    )))

    return(test)
}