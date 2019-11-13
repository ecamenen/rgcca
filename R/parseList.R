#' Convert a character in a vector
#'
#' @param s A character separated by comma
#' @return A vector of characters whitout spaces
#' @examples
#' s = '1,2, 3'
#' parseList(s)
#' @export
parseList <- function(s) {
    s <- gsub(" ", "", s)
    # split by comma
    unlist(strsplit(s, ","))
}