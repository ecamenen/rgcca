check_superblock <- function(opt) {
    if (!is.null(opt$response)) {
        warn_connection("supervized method with a response")
        if (opt$superblock) {
            opt$superblock <- FALSE
            if ("superblock" %in% names(opt))
                warning("In a supervised mode, the superblock corresponds to the response.")
        }
    }
    return(opt)
}
