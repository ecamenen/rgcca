order_opt <- function(opt, blocks, i_resp) {

    opt$blocks <- blocks

    par <- c("blocks", "ncomp")
    if (all(opt$tau != "optimal"))
        par[length(par) + 1] <- "tau"

    for (i in seq(length(par)))
        opt[[par[i]]] <- c(opt[[par[i]]][-i_resp], opt[[par[i]]][i_resp])

    return(opt)
}
