# Print and save indidvidual analysis attributes
saveInds <- function(
    rgcca,
    blocks,
    comp_x = 1,
    comp_y = 2,
    file = "individuals.tsv") {
    
    inds <- Reduce(cbind, lapply(
        rgcca$Y,
        function(x) x[, c(comp_x, comp_y)]))
    colnames(inds) <- as.vector(sapply(
        names(blocks),
        function(x) paste0(x, ".axis", c(comp_x, comp_y))))

    write.table(inds, file, sep = "\t")

    invisible(inds)
}
