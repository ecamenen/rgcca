#' Create a matrix corresponding to the response
#'
#' @param blocks A list of matrix
#' @param file A character giving the path of a file used as a response
#' @param sep A character giving the column separator
#' @param header A bolean giving the presence or the absence of the header
#' @param rownames An integer corresponding to the column number of the row
#' names (NULL otherwise)
#' @return A matrix corresponding to the response
#' @examples
#' \dontrun{
#' blocks = lapply(seq_len(3), function(x) matrix(runif(47 * 5), 47, 5))
#' set_response (blocks, 'data/response3.tsv')
#' }
#' @export
set_response <- function(
    blocks = NULL,
    file = NULL,
    sep = "\t",
    header = TRUE,
    rownames = 1) {


    if (!is.null(file)) {
        isXls <- length(grep("xlsx?", file))

        if (!isXls)
            response <- load_data(file, sep, rownames, header)
            # else response = loadExcel(file, 1, rownames, h = header, num = FALSE)

        qualitative <- unique(is.character2(response))

        if (length(qualitative) > 1)
            stop(
            "Please, select a response file with either qualitative data only or quantitative data only.",
            108
            )

        if (!qualitative)
            response <- to_numeric(response)


        if (NCOL(response) > 1) {
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
        return(rep(1, NROW(blocks[[1]])))
    }
}
