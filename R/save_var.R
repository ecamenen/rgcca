# Print and save variables analysis attributes
saveVars <- function(
    rgcca,
    blocks,
    comp_x = 1,
    comp_y = 2,
    file = "variables.tsv") {

    indexes <- c("cor", "weight")

    vars <- Reduce(rbind, lapply(seq_len(length(blocks)), function(i)
            data.frame(
                Reduce(cbind,
                        lapply(indexes, function(x)
                            getVar(rgcca, blocks, comp_x, comp_y, i_block = i, type = x))),
                names(blocks)[i]
            )))

    colnames(vars) <- c(as.vector(sapply(indexes, function(x)
            paste0(x, ".", paste0("axis.", c(comp_x, comp_y))))), "block")

    write.table(vars, file, sep = "\t")

    invisible(vars)
}
