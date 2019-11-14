changeText <- function(p) {
    for (i in seq_len(length(p$x$data)))
        p$x$data[[i]]$text <- sub(
            "order: .*<br />df\\[, 1\\]: (.*)<.*",
            "\\1\\",
            p$x$data[[i]]$text)
    return(p)
}
