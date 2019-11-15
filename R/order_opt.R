order_opt <- function(opt, blocks, i_resp) {

    J <- length(blocks)
    opt$blocks <- blocks
    opt$block_names <- names(blocks)

    par <- c("blocks", "block_names", "ncomp")
    if (all(opt$tau != "optimal"))
        par[length(par) + 1] <- "tau"

    for (i in seq(length(par))) {
        temp <- opt[[par[i]]][[J]]
        opt[[par[i]]][[J]] <- opt[[par[i]]][[i_resp]]
        opt[[par[i]]][[i_resp]] <- temp
    }

    names(opt$blocks) <- opt$block_names

    return(opt)
}
