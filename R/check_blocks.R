# rand_mat <- function(x) matrix(runif(9), 3, 3)
# A = lapply(1:3, rand_mat)
# check_blocks(A)
# names(A) <- LETTERS[1:3]
# check_blocks(A[1])
# check_blocks(A)
# row.names(A[[1]]) <- letters[1:3]
# check_blocks(A)
# for(i in 1:3)
#   row.names(A[[i]]) <- letters[(2*i):(2*i+2)]
# check_blocks(A)
# for(i in 1:3)
#   row.names(A[[i]]) <- letters[1:3]
# A[[1]][2, 3] <- NA
# 
# A[[1]][2, 3] <- runif(1)
check_blocks <- function(blocks) {
    
    msg <- "In blocks arg:"

    if (!is.list(blocks))
        stop(paste(msg, "is not a list."))

    if (length(blocks) < 2)
        stop(paste(msg, "should at least have two elements."))

    if (is.null(names(blocks)))
        stop(paste(msg, "elements of the list should have names."))

    if (any(sapply(blocks, function(x) is.null(row.names(x)))))
        stop(paste(msg, "elements of the list should have rownames."))

    if (length(Reduce(intersect, lapply(blocks, row.names))) == 0)
        stop(paste(msg, "elements of the list should have at least a common rowname."))

   # check_quantitative()

    if(any(is.na(unlist(blocks))))
        stop(paste(msg, "an element contains NA. Please use the impute_mean function.")) 

}
