to_numeric <- function(df) {
    matrix(
        sapply(seq(nrow(df) * ncol(df)),
            function(i)
                tryCatch({
                    as.numeric(df[i])
                }, warning = function(e)
                    NA)),
        nrow(df),
        ncol(df),
        dimnames = list(
            row.names(df),
            colnames(df))
    )
}
