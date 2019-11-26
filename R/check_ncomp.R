check_ncomp <- function(ncomp, blocks, min = 2) {
    ncomp <- elongate_arg(ncomp, blocks)
    check_size_blocks(blocks, "ncomp", ncomp)
    sapply(
        ncomp,
        function(x)
            check_min_integer("ncomp", x, blocks, min)
    )
}
