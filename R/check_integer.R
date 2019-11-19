# check_integer <- function(x, no_vector = TRUE){
# 
#     if (any(is.na(get(x))))
#         stop(paste(x, "should not be NA."))
# 
#     if (!is(get(x), "numeric"))
#         stop(paste(x, "should be numeric."))
#     
#     if (no_vector && !length(get(x)) == 1)
#         stop(paste(x, "should be of length 1."))
# 
#     y <- as.integer(get(x))
#     
#     if (all(y < 0))
#         stop(paste(x, "should be greater than 0."))
# 
#     assign(x, y, 1)
# }

