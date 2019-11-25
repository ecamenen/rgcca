#' Create a matrix corresponding to the response
#'
#' @inheritParams set_blocks
#' @param blocks A list of matrix
#' @return A matrix corresponding to the response
#' @examples
#' \dontrun{
#' blocks = lapply(seq(3), function(x) matrix(runif(47 * 5), 47, 5))
#' set_response (blocks, 'inst/extdata/response3.tsv')
#' }
#' @export
set_response <- function(
    blocks = NULL,
    file = NULL,
    sep = "\t",
    header = TRUE,
    rownames = 1) {

    response <- NULL

    if (!is.null(file)) {

        response <- load_file(
            file,
            sep = sep,
            rownames = rownames,
            header = header,
            response = TRUE
        )
    }
    
    check_response(response, blocks)

}
