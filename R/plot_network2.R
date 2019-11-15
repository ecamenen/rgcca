#' Plot the connection between blocks (dynamic plot)
#' 
#' @inheritParams select_analysis
#' @inheritParams plot_network
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(visNetwork)
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca.res = rgcca.analyze(blocks)
#' e <- get_edges(rgcca.res$C, blocks)
#' n <- get_nodes(blocks, rgcca = rgcca.res)
#' plot_network2(n, e, blocks)
#' @export
plot_network2 <- function(rgcca, blocks, connection) {

    nodes <- get_nodes(blocks, rgcca)
    edges <- get_edges(connection, blocks)

    par <- ifelse("sparsity" %in% names(nodes), "sparsity", "tau")

    if (all(is.na(nodes[, par])))
        nodes[, par] <- rep("optimal", length(blocks))

    nodes$title <- nodes$id
    nodes$label <- paste(nodes$id,
            "\nP =",
            nodes$P,
            paste0("\n", par, " ="),
            nodes[,par],
            "\nN =",
            nodes$nrow,
            sep = " ")

    edges$width <- edges$weight * 2
    nodes$color.background <- rep("#eee685", length(blocks))

    visNetwork(
        nodes,
        edges,
        main = list(
            text = paste0("Common rows between blocks : ",
                        nrow(blocks[[1]])),
            style = "font-family:sans;font-weight:bold;font-size:28px;text-align:center;"
        )
    ) %>%
        visNodes(
            borderWidth = 2,
            shape = "square",
            shadow = TRUE,
            color = list(
                border = "gray",
                highlight = list(background = "black", border = "darkred")
            )
        ) %>% visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = TRUE,
            color = list(color = "gray", highlight = "darkred")
        )

}
