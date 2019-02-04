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

plotNetwotk = function(nodes, edges, blocks){

  net <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

  V(net)$color <-  colorGroup(as.factor(names(blocks)))
  V(net)$size <- V(net)$tau * 50
  V(net)$label.font <- 3
  E(net)$width <- E(net)$weight

  plot(net,
       edge.color = "gray80",
       vertex.frame.color="white",
       vertex.label.color="black")
}

plotNetwotk2 = function(nodes, edges, blocks){

  nodes$title  <- nodes$id
  nodes$label  <- paste(nodes$id, "\nP =", nodes$P, "\ntau =", nodes$tau, "\nncomp =", nodes$ncomp, sep=" ")
  nodes$size  <-  nodes$tau * 50
  edges$width <- edges$weight * 2
  nodes$color.background <- unlist( lapply(as.list(1 - nodes$P/max(nodes$P)), function(x) rgb(colorRamp(c("khaki2", "coral3"))(x)/255) ) )

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

  visnet
}

nodes <- getNodes(opt, blocks)
edges <- getEdges(connection, blocks)

library("igraph")
plotNetwotk(nodes, edges, blocks)
library("visNetwork")
plotNetwotk2(nodes, edges, blocks)

