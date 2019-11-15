plot_dynamic_histogram <- function(p) {
    
    p <- plot_dynamic(p, option_dyn()$ax2, "text")
    n <- length(p$x$data)
    m <- unlist(lapply(p$x$data, function(x) !is.null(x$orientation)))
    j <- length(m[m])
    
    for (i in seq_len(j)) {
        p$x$data[[i]]$text <- paste(
            round(p$x$data[[i]]$x, 3),
            "+/-",
            round(p$x$data[[n]]$error_x$array[j],3))

        j <- j - 1
    }

    # Remove the onMouseOver for the last trace
    modify_text(p) %>% style(
        error_x = list(
            array = p$x$data[[n]]$error_x$array,
            color = "gray"
        ),
        hoverinfo = "none",
        traces = n
    )
}
