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

    if (!is.null(file)) {
        
        response <- load_file(
            file,
            sep = sep,
            rownames = rownames,
            header = header,
            reponse = TRUE
        )

        qualitative <- is.character2(response)

        if (length(qualitative) > 1)
            stop(
            "Please, select a response file with either qualitative data only or quantitative data only.",
            108
            )

        if (!qualitative)
            response <- to_numeric(response)

        if (ncol(response) > 1) {
            disjunctive <- unique(apply(response, 1, sum))

            if (length(disjunctive) &&
                unique(disjunctive %in% c(0, 1)) && disjunctive) {
                response2 <- factor(apply(response, 1, which.max))
                if (header) {
                    levels(response2) <- colnames(response)
                }
                response <- as.matrix(
                    data.frame(
                        as.character(response2),
                        row.names = rownames(response)
                ))

            } else {
                response <- as.matrix(response[, 1])
                warning("There is multiple columns in the response file. By default, only the first one is taken in account.")
            }
        }

        return(response)
    } else {
        return(rep(1, nrow(blocks[[1]])))
    }
}
