check_tau <- function(tau, blocks) {
    msg <- "tau should be comprise between 0 and 1 or should correspond to the character 'optimal' for automatic setting"
    tryCatch({
        # Check value of each tau
        tau <- sapply(
            seq(length(tau)), 
            function(x) {
                if (tau[x] != "optimal") {
                    y <- check_integer("tau", tau[x], float = TRUE, min = 0)
                    if (y > 1)
                        stop(paste0(msg, " (currently equals to ", tau[x], ")."),
                            exit_code = 129)
                }else
                    tau[x]
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
