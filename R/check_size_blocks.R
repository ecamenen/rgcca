check_size <- function(blocks, x, y = x) {
    
    if (identical(x, y))
        x <- ""

    if (class(y) %in% c("matrix", "data.frame")) {
        dim_y <- ncol(y)
        dim_type <- "number of columns"
    }else{
        dim_y <- length(y)
        dim_type <- "size"
    }

    if (dim_y != length(blocks))
        stop(
            paste0(
                x,
                " must have the same ", dim_type , " (",
                dim_y,
                ") than the number of blocks (",
                length(blocks),
                ")."
            ),
            exit_code = 130
        )
    else
        return(TRUE)
}
