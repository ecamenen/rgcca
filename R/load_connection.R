load_connection <- function(file, separator){
    if (!is.null(file))
        load_file(
            file = file,
            sep = separator,
            rownames = NULL,
            header = FALSE
        )
    else
        NULL
}
