check_tau <- function(tau, blocks) {
    msg <- "tau must be comprise between 0 and 1 or must correspond to the character 'optimal' for automatic setting"
    # TODO: elongate_arg
    tryCatch({
        # Check value of each tau
        tau <- sapply(tau, function(x) {
            if (x != "optimal") {
                if (x > 1)
                    stop(paste0(msg, " (currently equals to ", x, ")."),
                        exit_code = 129)
                check_integer("tau", x, float = TRUE, min = 0)
            }else
                x
        })

        tau <- elongate_arg(tau, blocks)
        check_size_blocks(blocks, "tau", tau)

        # return(tau)

        # If there is only one common tau
        # if (length(tau) == 1)
        #     tau <- rep(tau[[1]], length(blocks))
    }, warning = function(w)
        stop(msg, exit_code = 131)
    )
}
