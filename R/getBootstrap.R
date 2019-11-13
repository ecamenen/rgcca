#' Extract a bootstrap
#'
#' Extract statistical information from a bootstrap
#'
#' @inheritParams bootstrap
#' @inheritParams plotHistogram
#' @inheritParams plotVariablesSpace
#' @param W A list of list weights (one per bootstrap per blocks)
#' @param comp An integer giving the index of the analysis components
#' @return A matrix containing the means, 95% intervals, bootstrap ratio and p-values
#' @examples
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq_len(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' boot = bootstrap(blocks, rgcca.res, 2, FALSE)
#' getBootstrap(rgcca.res, boot)
#' @export
getBootstrap <- function(
    rgcca,
    W,
    comp = 1,
    i_block = NULL,
    collapse = TRUE,
    nb_cores = parallel::detectCores() - 1) {

    if (nb_cores == 0)
    nb_cores <- 1

    if (is.null(i_block))
    i_block <- length(W[[1]])

    if (comp > min(rgcca$ncomp))
    stop("Selected dimension was not associated to every blocks",
    exit_code = 113)

    cat("Binding in progress...")

    mean <- weight <- sd <- occ <- list()

    if (collapse)
    J <- seq(length(rgcca$a))
    else
    J <- i_block

    for (i in J) {

        W_bind <- parallel::mclapply(
        W,
        function(x) x[[i]][, comp],
        mc.cores = nb_cores
        )

        weight[[i]] <- rgcca$a[[i]][, comp]
        W_select <- matrix(unlist(W_bind), nrow = length(W_bind), ncol = length(W_bind[[1]]), byrow = TRUE)
        colnames(W_select) <- names(weight[[i]])
        rm(W_bind); gc()

        n <- seq(ncol(W_select))

        if (is(rgcca, "sgcca")) {

            occ[[i]] <- unlist(parallel::mclapply(n,
            function(x) sum(W_select[,x] != 0) / length(W_select[, x]),
            mc.cores = nb_cores
            ))

        }

        mean[[i]] <- unlist(parallel::mclapply(n,
        function(x) mean(W_select[,x]),
        mc.cores = nb_cores
        ))
        sd[[i]] <- unlist(parallel::mclapply(n,
        function(x) sd(W_select[,x]),
        mc.cores = nb_cores
        ))

        rm(W_select); gc()
    }

    rm(W); gc()

    occ <- unlist(occ)
    mean <- unlist(mean)
    weight <- unlist(weight)
    sd <- unlist(sd)

    cat("OK", append = TRUE)

    p.vals <- pnorm(0, mean = abs(mean), sd = sd)
    tail <- qnorm(1 - .05 / 2)

    df <- data.frame(
    mean = mean,
    rgcca = weight,
    intneg = mean - tail * sd,
    intpos = mean + tail * sd,
    br = abs(mean) / sd,
    p.vals,
    BH = p.adjust(p.vals, method = "BH")
    )

    if (is(rgcca, "sgcca")) {
        index <- 8
        df$occ <- occ
    }else{
        index <- 5
        df$sign <- rep("", nrow(df))

        for (i in seq(nrow(df)))
        if (df$intneg[i]/df$intpos[i] > 0)
        df$sign[i] <- "*"
    }

    if (collapse)
    df$color <- as.factor(getBlocsVariables(rgcca$a, collapse = collapse))

    zero_var <- which(df[, 1] == 0)
    if (length(zero_var) != 0)
    df <- df[-zero_var, ]

    data.frame(getRankedValues(df, index, allCol = TRUE), order = nrow(df):1)
}