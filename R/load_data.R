#' Creates a matrix from loading a file
#'
#' @param f A character giving the file name
#' @param sep A character giving the column separator
#' @param rownames An integer corresponding to the column number of the
#' row names (NULL otherwise)
#' @param h A bolean giving the presence or the absence of the header
#' @return A matrix containing the loaded file
#' @examples
#' \dontrun{
#' load_data('data/agriculture.tsv')
#' }
load_data <- function(f, sep = "\t", rownames = 1, h = TRUE) {

    if (!is.null(rownames) && rownames < 1)
    rownames <- NULL

    func <- function(x = rownames)
        as.matrix(read.table(
            f,
            sep = sep,
            header = h,
            row.names = x,
            na.strings = "NA",
            dec = ","
        ))

    tryCatch({
        func()
    }, error = function(e) {
        if (e$message == "duplicate 'row.names' are not allowed")
        func(NULL)
    })

}
