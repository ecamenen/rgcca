#' Plot permuation in 3D
#' 
#' Plot permuation in 3D
#' 
#' @inheritParams plot3D
#' @examples
#' data("Russett")
#' A = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' perm <- rgcca_permutation(A, nperm = 2, n_cores = 1)
#' plot_permut_3D(perm)
#' perm <- rgcca_permutation(A, p_c1 = TRUE, nperm = 2, n_cores = 1)
#' plot_permut_3D(perm)
#' c1s <- expand.grid(
#'     lapply(
#'         seq(length(A)),
#'         function(x) seq(1 / sqrt(ncol(A[[x]])), 1, by = 0.1)
#'     )
#' )
#'     perm <- rgcca_permutation(A, p_c1 = c1s), nperm = 2, n_cores = 1)
#' @export
plot_permut_3D <- function(
    perm,
    i_block_x = 1,
    i_block_y = 2,
    i_block_z = 3,
    sign = FALSE) {
        
    zstat <- as.data.frame(
        cbind(perm$penalties[,c(i_block_x, i_block_y, i_block_z)], 
        z = perm$zstat))

    if (sign)
        zstat[, 4] <- as.double(zstat$z > qnorm(1 - 0.05 / 2))
            + as.double(zstat$z > qnorm(1 - 0.01 / 2))
            + as.double(zstat$z > qnorm(1 - 0.001 / 2)) 

    plotly::plot_ly(
        zstat,
        x = ~ zstat[, 1],
        y = ~ zstat[, 2],
        z = ~ zstat[, 3],
        marker = list(
            color = ~ zstat[, 4],
            showscale = TRUE,
            colorbar = list(
                title = 'Z-score'
            ),
            colorscale = list(
                list(0, "rgb(165,0,38)"), list(mean(zstat$z), "rgb(254,224,144)"), list(max(zstat$z), "rgb(49,54,149)")
            ),
            cauto = F,
            cmin = 0,
            cmax = max(zstat$z)
        )
    ) %>% 
    add_markers() %>% 
    layout(
        title = "Z-scores of permutations colored for each block",
            scene = list(
                xaxis = list(title = colnames(zstat)[1]),
                yaxis = list(title = colnames(zstat)[2]),
                zaxis = list(title = colnames(zstat)[3])
            ))

}
