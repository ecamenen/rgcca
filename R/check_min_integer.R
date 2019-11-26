check_min_integer <- function(x , y, blocks, min = 1) {
    check_lower_blocks(x, y, blocks)
    check_integer(x, y, min = min)
}
