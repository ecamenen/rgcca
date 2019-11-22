#' Plot the connection between blocks
#' 
#' @inheritParams select_analysis
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(igraph)
#' library(RGCCA)
#' data("Russett")
#' blocks = list(agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11] )
#' rgcca_out = rgcca.analyze(blocks)
#' plot_network(rgcca_out, blocks)
#' @export
plot_network <- function(rgcca, blocks) {
    # Avoid random
    set.seed(1)
    V <- E <- NULL

    nodes <- get_nodes(blocks, rgcca)
    edges <- get_edges(rgcca$C, blocks)

    par <- ifelse("sparsity" %in% names(nodes), "sparsity", "tau")

    net <- graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE)

    if (all(is.na(nodes[, par]))) {
        nodes[, par] <- rep("optimal", length(blocks))
        V(net)$tau <- rep(1, length(blocks))
    }

    V(net)$color <- "khaki2"
    V(net)$label <- paste(
        nodes$id,
        "\nP =",
        nodes$P,
        paste0("\n", par, " ="),
        nodes[,par],
        "\nN =",
        nodes$nrow,
        sep = " ")
    V(net)$shape <- "square"
    E(net)$width <- E(net)$weight * 2

    plot(
        net,
        cex.main = 5,
        edge.color = "gray70",
        edge.lty = 2,
        vertex.frame.color = "gray50",
        vertex.label.color = "black",
        vertex.label.dist = 6,
        vertex.label.degree = 1.5,
        vertex.size = 23,
        main = paste0("Common rows between blocks : ", nrow(blocks[[1]]))
    )

}
