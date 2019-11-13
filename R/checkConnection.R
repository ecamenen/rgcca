#' Check the format of the connection matrix
#'
#' @param c A symmetric matrix containing 1 and 0
#' @param blocks A list of matrix
#' @export
checkConnection <- function(c, blocks) {

    if (!isSymmetric.matrix(unname(c)))
    stop("The connection file must be a symmetric matrix.", exit_code = 103)

    d <- unique(diag(c))
    if (length(d) != 1 || d != 0)
    stop("The diagonal of the connection matrix file must be 0.",
    exit_code = 105)

    x <- unique(c %in% c(0, 1))
    if (length(x) != 1 || x != TRUE)
    stop("The connection file must contains only 0 or 1.", exit_code = 106)

    if (all(c == 0))
    stop("The connection file could not contain only 0.", exit_code = 107)

    n <- length(blocks)
    if (NCOL(c) != n)
    stop(
    paste0(
    "The number of rows/columns of the connection matrix file must be equal to ",
    n,
    " (the number of blocks in the dataset, +1 with a superblock by default)."
    ),
    exit_code = 104
    )

    # TODO: warning if superblock = TRUE

}