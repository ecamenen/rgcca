getNodes = function(opt, blocks) {

  values <- list( names(blocks), unlist(lapply(blocks, NCOL)), opt$tau, opt$ncomp )
  nodes <- as.data.frame(matrix(unlist(values), length(blocks), length(values)))
  colnames(nodes) = c("id", "P", "tau", "ncomp")

  nodes[, c(2, 3, 4)] <- apply(nodes[, c(2, 3, 4)], 2, as.numeric)

  return(nodes)
}

getEdges = function(connection, blocks) {

  J = NCOL(connection)

  edges = list()

  k = 0
  for (j in 1:J) {
    for (i in 1:J){
      if(i > k && connection[i, j] > 0)
        edges[[ length(edges) + 1 ]] = c( names(blocks)[j], names(blocks)[i], connection[i, j] )
    }
    k = k + 1
  }

  edges <- as.data.frame(t(matrix(unlist(edges), 3, length(edges))))
  colnames(edges) = c("from", "to", "weight")
  edges[, 3] <- as.numeric(edges[, 3])

  return(edges)
}

colorNodes = function(nodes){
  unlist( lapply(as.list(1 - nodes$P/max(nodes$P)), function(x) rgb(colorRamp(c("coral3", "khaki2"))(x)/255) ) )
}

plotNetwotk = function(nodes, edges, blocks){

  net <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  V(net)$color <- colorNodes(nodes)
  V(net)$size <- V(net)$tau * 50
  V(net)$label <- paste(nodes$id, "\nP =", nodes$P, "\ntau =", nodes$tau, "\nncomp =", nodes$ncomp, sep=" ")
  V(net)$label.font <- 3
  V(net)$shape <- "square"
  E(net)$width <- E(net)$weight

  plot(net,
       edge.color = "gray80",
       vertex.frame.color = "gray",
       vertex.label.color = "black",
       vertex.label.dist = "bottom")
}

plotNetwotk2 = function(nodes, edges, blocks){

  nodes$title  <- nodes$id
  nodes$label  <- paste(nodes$id, "\nP =", nodes$P, "\ntau =", nodes$tau, "\nncomp =", nodes$ncomp, sep=" ")
  nodes$size  <-  nodes$tau * 50
  edges$width <- edges$weight * 2
  nodes$color.background <- colorNodes(nodes)

  visnet <- visNetwork(nodes, edges)

  visnet <- visNodes(visnet,
                     borderWidth = 2,
                     shape = "square",
                     shadow = TRUE,
                     color=list(border = "gray",
                                highlight = list(background = "black", border = "darkred"))
  )

  visnet <- visEdges(visnet,
                     smooth = FALSE,
                     shadow = TRUE,
                     dashes = TRUE,
                     color = list(color = "gray", highlight = "darkred")
  )

  # visLegend(visnet, main = "Legend", position = "right", useGroups = FALSE, stepY = 125, addNodes =
  #             data.frame(title = "Tau", label = rev(seq(.25, 1, .25)), shape = "square", size = rev(seq(.25, 1, .25) * 50),
  #                        color.background = "white", color.border = "gray", font.align = "bottom")
  #
  # )
  #
  # visLegend(visnet, main = "Legend", position = "right", useGroups = FALSE, addNodes =
  #             data.frame(title = "Nb. variables", label = c(max(nodes$P), max(nodes$P)/2, 0), shape = "square", size = 25,
  #                        color.background = colorRampPalette(c("coral3", "khaki2"))(3), color.border = "gray", font.align = "bottom")
  # )

  visLegend(visnet, main = "", position = "right", width = .2, useGroups = FALSE, stepY = 135, addNodes =
              data.frame(label = c("Tau", c(1, .5), "", "Nb. variables", c(max(nodes$P), round(max(nodes$P)/2), 0)),
                         size = c(0, c(1, .5) * 50, 0, 0, rep(25, 3)),
                         color.background = c("", rep("gray", 2), "", "", colorRampPalette(c("coral3", "khaki2"))(3)),
                         color.border = "white",
                         font.size = c(30, rep(20, 2), 30, 30, rep(20, 3)),
                         shape = c("text", rep("square", 2), "text", "text", rep("square", 3))
            ))

}

nodes <- getNodes(opt, blocks)
edges <- getEdges(connection, blocks)

library("igraph")
plotNetwotk(nodes, edges, blocks)
library("visNetwork")
plotNetwotk2(nodes, edges, blocks)
